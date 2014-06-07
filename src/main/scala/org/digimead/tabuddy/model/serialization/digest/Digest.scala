/**
 * TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2014 Alexey Aksenov ezh@ezh.msk.ru
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.digimead.tabuddy.model.serialization.digest

import java.io.{ InputStream, OutputStream }
import java.net.URI
import java.util.concurrent.atomic.AtomicReference
import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.transport.Transport
import org.digimead.tabuddy.model.serialization.yaml.Timestamp
import org.digimead.tabuddy.model.serialization.{ SData, Serialization }
import scala.collection.{ immutable, mutable }
import scala.language.implicitConversions
import scala.ref.SoftReference
import scala.util.control.ControlThrowable

/*
 * Simple layout is
 *   storageURI/digest/${graph.modification}/type - mechanism description
 *   storageURI/digest/${graph.modification}/... - all other data
 *
 * type file contains the description of the mechanism
 * 1st line (mandatory) - Mechanism.Identifier.name
 * 2nd blank separator
 * 3rd line (mandatory) - digest algorithm
 * 4th blank separator
 * 5th line and others ... optional parameters and blank separators
 */
/**
 * Provides digest mechanism for serialization process.
 */
class Digest extends Loggable {
  /** Acquire routines. */
  protected val acquire = new Acquire
  /** Freeze routines. */
  protected val freeze = new Freeze

  /** Approve resource check sum. */
  def approve(resourceURI: URI, sData: SData) =
    log.debug("Approve validation for .../" + sData(SData.Key.storageURI).relativize(resourceURI))
  /** Initialize SData for acquire process. */
  def initAcquire(sData: SData): SData = sData.get(Digest.Key.acquire) match {
    case Some(mandatory) ⇒
      val updated = sData.
        updated(Digest.historyPerURI, Map[URI, Digest.History]()).
        updated(SData.Key.initializeLoader, new acquire.InitializeLoader(sData.get(SData.Key.initializeLoader))).
        updated(SData.Key.initializeSourceSData, new acquire.InitializeSourceSData(sData.get(SData.Key.initializeSourceSData))).
        updated(SData.Key.readFilter, new acquire.ReadFilter(sData.get(SData.Key.readFilter)))
      Digest.perIdentifier.values.foldLeft(updated)((sData, mechanism) ⇒ mechanism.initAcquire(sData))
    case None ⇒
      sData
  }
  /** Initialize SData for freeze process. */
  def initFreeze(sData: SData): SData =
    sData.
      updated(SData.Key.afterFreeze, new freeze.AfterFreeze(sData.get(SData.Key.afterFreeze))).
      updated(SData.Key.initializeFreezeSData, new freeze.InitializeFreezeSData(sData.get(SData.Key.initializeFreezeSData)))
  /** Get digest history for loader. */
  def history(loader: Serialization.Loader): Map[Element.Timestamp, Map[URI, Mechanism.Parameters]] =
    loader.sData.get(Digest.historyPerURI).map { history ⇒
      val transition = mutable.HashMap[Element.Timestamp, Map[URI, Mechanism.Parameters]]()
      history.foreach {
        case (uri, historyPerURI) ⇒
          historyPerURI.foreach {
            case (timestamp, (parameters, context)) ⇒
              transition.get(timestamp) match {
                case Some(mapWithURI) ⇒
                  transition(timestamp) = mapWithURI.updated(uri, parameters)
                case None ⇒
                  transition(timestamp) = Map(uri -> parameters)
              }
          }
      }
      transition.toMap
    } getOrElse immutable.HashMap()
  /** Get digest history for graph. */
  def history(graph: Graph[_ <: Model.Like]): Map[Element.Timestamp, Map[URI, Mechanism.Parameters]] =
    graph.storages match {
      case Nil ⇒
        immutable.HashMap()
      case storages ⇒
        storages.sortBy(_.getScheme == "file").foreach { bootstrapStorageURI ⇒
          try {
            return history(Serialization.acquireLoader(bootstrapStorageURI,
              graph.retrospective.last, SData(Digest.Key.acquire -> false)))
          } catch {
            case ce: ControlThrowable ⇒ throw ce
            case e: Throwable ⇒ log.fatal(e.getMessage(), e)
          }
        }
        immutable.HashMap()
    }
  /** Refuse resource check sum. */
  @throws[SecurityException]("if verification is failed")
  def refuse(resourceURI: URI, sData: SData) =
    throw new SecurityException("Validation failed for " + sData(SData.Key.storageURI).relativize(resourceURI))

  /** Get digest parameter for the specific modification of the storage. */
  protected def getDigestParameters(modified: Element.Timestamp, transport: Transport, sData: SData): Mechanism.Parameters = {
    val storageURI = sData(SData.Key.storageURI)
    log.debug(s"Load digest information at ${modified} from storage ${storageURI}")
    // Read type info.
    val mechanismTypeURI = Digest.digestURI(storageURI, transport, modified, Digest.typeName)
    val lines = try {
      val typeStream = transport.openRead(Serialization.inner.encode(mechanismTypeURI, sData), sData)
      try scala.io.Source.fromInputStream(typeStream).getLines.toList
      catch {
        case e: Throwable ⇒
          log.debug(s"Unable to read ${mechanismTypeURI}: " + e.getMessage())
          Seq()
      } finally try typeStream.close() catch {
        case e: SecurityException ⇒ throw e
        case e: Throwable ⇒ log.error("Unable to load digest information: " + e.getMessage, e)
      }
    } catch {
      case e: Throwable ⇒
        log.debug(s"Unable to read ${mechanismTypeURI}: " + e.getMessage())
        Seq()
    }
    // Create digest parameters.
    if (lines.nonEmpty) {
      val (mechanismIdentifier :: digestAlgorithm :: arguments) = {
        var result = Seq.empty[String]
        var begin = 0
        var end = lines.indexWhere(_.trim.isEmpty())
        while (end >= 0) {
          result = result :+ lines.slice(begin, end).map(_.trim).mkString("\n")
          begin = end + 1
          end = lines.indexWhere(_.trim.isEmpty(), begin)
        }
        (result :+ lines.drop(begin).map(_.trim).mkString("\n")).filterNot(_.isEmpty())
      }
      Digest.perIdentifier.find(_._1.name == mechanismIdentifier) match {
        case Some((identifier, mechanism)) ⇒
          try {
            val parameters = mechanism(digestAlgorithm, arguments: _*)
            log.info(s"Create ${parameters} for storage ${storageURI}")
            parameters
          } catch {
            case e: Throwable ⇒
              log.warn(s"Unable to create parameters for ${identifier}. Arguments: " + (digestAlgorithm +: arguments).mkString(", "), e)
              Digest.NoDigest
          }
        case None ⇒
          log.info(s"Unable to find registered mechanism for ${mechanismIdentifier}.")
          Digest.NoDigest
      }
    } else {
      log.info(s"Digest parameters not found for storage ${storageURI}")
      Digest.NoDigest
    }
  }
  /** Update history for storage. */
  protected def updateHistory(history: Seq[(Element.Timestamp, Mechanism.Parameters)], transport: Transport, sData: SData): SData = {
    val content: Seq[(Element.Timestamp, (Mechanism.Parameters, AtomicReference[SoftReference[AnyRef]]))] = history.map {
      case (modified, parameters) ⇒ modified -> ((parameters, new AtomicReference(new SoftReference[AnyRef](null))))
    }
    sData.updated(Digest.historyPerURI, sData(Digest.historyPerURI).updated(sData(SData.Key.storageURI), Map(content: _*)))
  }

  /**
   * Container for acquire hooks.
   */
  class Acquire {
    /**
     * Hook that initiates digest calculation.
     */
    class ReadFilter(val userFilter: Option[Function4[InputStream, URI, Transport, SData, InputStream]])
      extends Function4[InputStream, URI, Transport, SData, InputStream] {
      /** Apply MessageDigest instance to InputStream. */
      def apply(is: InputStream, uri: URI, transport: Transport, sData: SData): InputStream =
        if (sData.isDefinedAt(Digest.Key.acquire)) {
          (try Option(transport.readTimestamp(uri, sData)) catch { case e: Throwable ⇒ None }) match {
            case Some(modified) ⇒
              sData(Digest.historyPerURI).get(sData(SData.Key.storageURI)) match {
                case Some(history) ⇒
                  history.get(modified) match {
                    case Some((parameters, context)) if parameters.mechanism != null ⇒
                      val stream = parameters.mechanism.readFilter(parameters, context, modified, is, uri, transport, sData)
                      userFilter.map(_(stream, uri, transport, sData)) getOrElse stream
                    case _ ⇒
                      if (sData(Digest.Key.acquire))
                        throw new SecurityException(s"Digest is not available for ${modified} " + uri)
                      userFilter.map(_(is, uri, transport, sData)) getOrElse is
                  }
                case _ ⇒
                  if (sData(Digest.Key.acquire))
                    throw new SecurityException(s"Digest is not available for ${modified} " + uri)
                  userFilter.map(_(is, uri, transport, sData)) getOrElse is
              }
            case None ⇒
              throw new SecurityException(s"Unable to find modification timestamp for digest validation.")
          }
        } else
          is
    }
    /**
     * Hook that propagates Digest.historyPerURI with retrospective data.
     */
    class InitializeLoader(val userInitialize: Option[Function1[Serialization.Loader, Serialization.Loader]])
      extends Function1[Serialization.Loader, Serialization.Loader] {
      /** Propagate digest map with retrospective data. */
      def apply(loader: Serialization.Loader): Serialization.Loader = {
        var adjustedSData = loader.sData
        loader.sources.foreach { source ⇒
          val storageURI = source.storageURI
          val sData = loader.sData.updated(SData.Key.storageURI, storageURI)
          val content = source.graphDescriptor.records.map { record ⇒
            loader.sData(Digest.historyPerURI).get(storageURI).flatMap(_.get(record) map (record -> _)) getOrElse {
              try {
                log.debug(s"Load digest information for ${record}.")
                val parametersForTimestamp = getDigestParameters(record, source.transport, sData)
                record -> (parametersForTimestamp, new AtomicReference(new SoftReference[AnyRef](null)))
              } catch {
                case e: Throwable ⇒
                  log.error("Unable to load digest information: " + e.getMessage(), e)
                  record -> (Digest.NoDigest, new AtomicReference(new SoftReference[AnyRef](null)))
              }
            }
          }
          adjustedSData = adjustedSData.updated(Digest.historyPerURI,
            adjustedSData(Digest.historyPerURI).updated(storageURI, Map(content.toSeq: _*)))
        }
        // Reset cached data for Digest.historyPerURI.
        adjustedSData.get(Digest.historyPerURI).foreach(history ⇒
          history.values.foreach(_.values.map(_._2.set(new SoftReference(null)))))
        val updated = new Serialization.Loader(loader.sources, loader.modified, adjustedSData)
        userInitialize.foldLeft(updated)((loader, hook) ⇒ hook(loader))
      }
    }
    /**
     * Hook that prepare Digest.historyPerURI for particular storage.
     */
    class InitializeSourceSData(val userInitialize: Option[Function3[Element.Timestamp, Transport, SData, SData]])
      extends Function3[Element.Timestamp, Transport, SData, SData] {
      /** Load digest map from storage. */
      def apply(modified: Element.Timestamp, transport: Transport, sData: SData): SData = {
        val updated = sData(Digest.historyPerURI).get(sData(SData.Key.storageURI)) match {
          case None ⇒
            /*
             * Update SData historyPerURI.
             * Add only one digest for the specified 'modified' value.
             * It is allow to load and validate record resources.
             */
            val parametersForTimestamp = getDigestParameters(modified, transport, sData)
            updateHistory(Seq((modified, parametersForTimestamp)), transport, sData)
          case Some(parameters) ⇒
            sData // Skip. Already initialized.
        }
        userInitialize.foldLeft(updated)((sData, hook) ⇒ hook(modified, transport, sData))
      }
    }
  }
  /**
   * Container for freeze hooks.
   */
  class Freeze {
    /**
     * Hook that saves digest results to storage.
     */
    class AfterFreeze(val userAfterFreeze: Option[Function3[Graph[_ <: Model.Like], Transport, SData, _]])
      extends Function3[Graph[_ <: Model.Like], Transport, SData, Unit] {
      /** Save digest results. */
      def apply(graph: Graph[_ <: Model.Like], transport: Transport, sData: SData) {
        sData.get(Digest.Key.freeze).flatMap(_.get(sData(SData.Key.storageURI))).foreach(parameter ⇒
          if (parameter.mechanism != null)
            parameter.mechanism.afterFreeze(parameter, graph, transport, sData))
        userAfterFreeze.foreach(_(graph, transport, sData))
      }
    }
    /**
     * Hook that calculates digest.
     */
    class WriteFilter(val userFilter: Option[Function4[OutputStream, URI, Transport, SData, OutputStream]])
      extends Function4[OutputStream, URI, Transport, SData, OutputStream] {
      /** Apply MessageDigest instance to OutputStream. */
      def apply(os: OutputStream, uri: URI, transport: Transport, sData: SData): OutputStream =
        sData(Digest.Key.freeze).get(sData(SData.Key.storageURI)) match {
          case Some(parameter) if parameter.mechanism != null ⇒
            val stream = parameter.mechanism.writeFilter(parameter, os, uri, transport, sData)
            userFilter.map(_(stream, uri, transport, sData)) getOrElse stream
          case _ ⇒
            userFilter.map(_(os, uri, transport, sData)) getOrElse os
        }
    }
    /**
     * Hook that add Digest.Key.freeze with previous configuration.
     */
    class InitializeFreezeSData(val userInitialize: Option[Function2[Graph[_ <: Model.Like], SData, SData]])
      extends Function2[Graph[_ <: Model.Like], SData, SData] {
      /** Check Digest.Key.freeze and adjust Digest.Key.freeze if needed. */
      def apply(graph: Graph[_ <: Model.Like], sData: SData): SData = {
        val updatedSData = {
          val skipKnownStorages = sData.get(Digest.Key.freeze) match {
            case Some(userParameters) ⇒ userParameters.keys.map(Serialization.inner.addTrailingSlash).toSeq
            case None ⇒ Nil
          }
          val explicitStorages = sData.get(SData.Key.explicitStorages) match {
            case Some(storages: Serialization.Storages) ⇒ storages.seq.map(_.real).flatten
            case None ⇒ Nil
          }
          val previousParameters = for {
            storageURI ← graph.storages.filterNot(skipKnownStorages.contains)
            modified ← graph.retrospective.last
          } yield try {
            Serialization.perScheme.get(storageURI.getScheme()) match {
              case Some(transport) ⇒
                storageURI -> getDigestParameters(modified, transport, sData.updated(SData.Key.storageURI, storageURI))
              case None ⇒
                log.warn(s"Transport for the specified scheme '${storageURI.getScheme()}' not found.")
                storageURI -> Digest.NoDigest
            }
          } catch {
            case e: Throwable ⇒
              log.warn(s"Unable to get digest parameters for ${}: ${e.getMessage}")
              storageURI -> Digest.NoDigest
          }
          val defaultParameters = for {
            storageURI ← explicitStorages.filterNot(s ⇒ skipKnownStorages.contains(s) || previousParameters.exists(_._1 == s))
          } yield storageURI -> Digest.default
          val digestParametersMap = Map((previousParameters ++ defaultParameters).toSeq: _*)
          if (digestParametersMap.nonEmpty) {
            log.debug("Freeze with digest parameters: " + digestParametersMap)
            sData.updated(Digest.Key.freeze, digestParametersMap)
          } else
            sData
        }
        updatedSData.get(Digest.Key.freeze) match {
          case Some(parameters) ⇒
            // Add trailing slash to digestAlgorithm keys
            val updatedDigestAlgorithm = Map(parameters.map {
              case (uri, algorithm) ⇒ Serialization.inner.addTrailingSlash(uri) -> algorithm
            }.toSeq: _*)
            val updated = sData.
              updated(Digest.Key.freeze, updatedDigestAlgorithm).
              updated(SData.Key.initializeFreezeSData, new freeze.InitializeFreezeSData(sData.get(SData.Key.initializeFreezeSData))).
              updated(SData.Key.writeFilter, new freeze.WriteFilter(sData.get(SData.Key.writeFilter)))
            Digest.perIdentifier.values.foldLeft(updated)((sData, mechanism) ⇒ mechanism.initFreeze(sData))
          case None ⇒
            sData
        }
      }
    }
  }
}

object Digest extends Loggable {
  implicit def digest2implementation(d: Digest.type): Digest = d.inner
  type History = Map[Element.Timestamp, (Mechanism.Parameters, AtomicReference[SoftReference[AnyRef]])]
  /**
   * Composite container that merges all available digests per storage URI.
   * For example:
   *   There was modification A with digest Solid(MD5)
   *   There was modification B with digest MySolid(X10)
   *   There was modification C with digest Solid(SHA-512)
   *
   *   then container contains 3 records A,B,C with suitable Mechanism.Parameters and empty context -AtomicReference.
   *
   *   AtomicReference is SoftReference container with a context information of the specific digest.
   *   AtomicReference with SoftReference is using for lazy loading.
   *   There is hash map with sums as an example of such type information.
   */
  val historyPerURI = SData.key[Map[URI, Digest.History]]("digest")

  /** Get default digest algorithm. */
  def default = DI.default
  /** Get digest container name. */
  def containerName = DI.containerName
  /** Get URI of digest data. */
  def digestURI(baseURI: URI, transport: Transport, modified: Element.Timestamp, part: String*) =
    if (part.nonEmpty)
      transport.append(baseURI, (Seq(containerName, Timestamp.dump(modified)) ++ part): _*)
    else
      transport.append(baseURI, (Seq(containerName, Timestamp.dump(modified)) :+ ""): _*)
  /** Get digest implementation. */
  def inner = DI.implementation
  /** Map of all available digest implementations. */
  def perIdentifier = DI.perIdentifier
  /** Get digest type name. */
  def typeName = DI.typeName

  /**
   * A transparent stream that updates the associated verifier using the bits going through the stream.
   */
  class DigestInputStream[T](stream: InputStream, verifier: java.security.MessageDigest,
    onClose: java.security.MessageDigest ⇒ T) extends java.security.DigestInputStream(stream, verifier) {
    override def close() {
      super.close()
      onClose(verifier)
    }
  }
  /**
   * A transparent stream that updates the associated digest using the bits going through the stream.
   */
  class DigestOutputStream[T](stream: OutputStream, digest: java.security.MessageDigest,
    onClose: java.security.MessageDigest ⇒ T) extends java.security.DigestOutputStream(stream, digest) {
    override def close() {
      super.close()
      onClose(digest)
    }
  }
  /**
   * Predefined SData keys.
   */
  object Key {
    /** Acquire parameter. Digest mandatory if true. */
    val acquire = SData.key[Boolean]("digest")
    /** Freeze parameters per storageURI. */
    val freeze = SData.key[Map[URI, Mechanism.Parameters]]("digest")
    /** Apply F(x) to digest content. */
    val readFilter = SData.key[(InputStream, URI, Transport, SData) ⇒ InputStream]("filter")
    /** Apply F(x) to digest content. */
    val writeFilter = SData.key[(OutputStream, URI, Transport, SData) ⇒ OutputStream]("filter")
  }
  /**
   * No mechanism parameter.
   */
  case object NoDigest extends Mechanism.Parameters {
    /** Digest algorithm name. */
    val algorithm = ""
    /** Digest parameters as sequence of strings. */
    val arguments: Seq[String] = Seq.empty
    /** Mechanism instance. */
    val mechanism: Mechanism = null
  }
  /**
   * Dependency injection routines.
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    /** Default digest algorithm. */
    lazy val default = injectOptional[Mechanism.Parameters]("Digest.Default") getOrElse SimpleDigest("SHA-512")
    /** Digest container name. */
    lazy val containerName = injectOptional[String]("Digest.ContainerName") getOrElse "digest"
    /** Digest implementation. */
    lazy val implementation = injectOptional[Digest] getOrElse new Digest
    /**
     * Per identifier digest mechanisms map.
     *
     * Each collected mechanism must be:
     *  1. an instance of Mechanism
     *  2. has name that starts with "Digest.Mechanism."
     */
    lazy val perIdentifier: immutable.HashMap[Mechanism.Identifier, Mechanism] = {
      val mechanisms = bindingModule.bindings.filter {
        case (key, value) ⇒ classOf[Mechanism].isAssignableFrom(key.m.runtimeClass)
      }.map {
        case (key, value) ⇒
          key.name match {
            case Some(name) if name.startsWith("Digest.Mechanism.") ⇒
              log.debug(s"'${name}' loaded.")
              bindingModule.injectOptional(key).asInstanceOf[Option[Mechanism]]
            case _ ⇒
              log.debug(s"'${key.name.getOrElse("Unnamed")}' digest mechanism skipped.")
              None
          }
      }.flatten.toSeq
      assert(mechanisms.distinct.size == mechanisms.size, "digest mechanisms contain duplicated entities in " + mechanisms)
      immutable.HashMap(mechanisms.map(m ⇒ m.identifier -> m): _*)
    }
    /** Digest file name with digest type. */
    lazy val typeName = injectOptional[String]("Digest.TypeName") getOrElse "type"
  }
}
