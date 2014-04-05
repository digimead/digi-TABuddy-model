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
import scala.Option.option2Iterable
import scala.collection.immutable
import scala.language.implicitConversions
import scala.ref.SoftReference

/*
 * Simple layout is
 *   storageURI/digest/${graph.modification}/type - mechanism description
 *   storageURI/digest/${graph.modification}/... - all other data
 *
 * type file contains the description of the mechanism
 * 1st line (mandatory) - Mechanism.Identifier.name
 * 2nd line (mandatory) - digest algorithm
 * 3rd line and others ... optional parameters
 */
/**
 * Provides digest mechanism for serialization process.
 */
class Digest extends Loggable {
  /** Acquire routines. */
  protected val acquire = new Acquire
  /** Freeze routines. */
  protected val freeze = new Freeze
  /** Hex array characters. */
  protected val hexArray = "0123456789ABCDEF".toCharArray()

  /** Convert hex string to byte array. */
  def hexStringToByteArray(s: String): Array[Byte] = {
    val len = s.length()
    val data = new Array[Byte](len / 2)
    for (i ← 0 until len by 2)
      data(i / 2) = ((Character.digit(s.charAt(i), 16) << 4) + Character.digit(s.charAt(i + 1), 16)).toByte
    data
  }
  /** Convert byte array to hex string. */
  def byteArrayToHexString(bytes: Array[Byte]): String = {
    val hexChars = new Array[Char](bytes.length * 2)
    for (j ← 0 until bytes.length) {
      val v = bytes(j) & 0xFF
      hexChars(j * 2) = hexArray(v >>> 4)
      hexChars(j * 2 + 1) = hexArray(v & 0x0F)
    }
    new String(hexChars)
  }

  /** Initialize SData for acquire process. */
  def initAcquire(sData: SData): SData = sData.get(Digest.Key.acquire) match {
    case Some(mandatory) ⇒
      val updated = sData.
        updated(Digest.historyPerURI, immutable.Map[URI, Digest.History]()).
        updated(Digest.modifiedCache, new ThreadLocal[Element.Timestamp]()).
        updated(SData.Key.initializeLoader, new acquire.InitializeLoader(sData.get(SData.Key.initializeLoader))).
        updated(SData.Key.initializeSourceSData, new acquire.InitializeSourceSData(sData.get(SData.Key.initializeSourceSData))).
        updated(SData.Key.decodeFilter, new acquire.DecodeFilter(sData.get(SData.Key.decodeFilter))).
        updated(SData.Key.afterRead, new acquire.AfterRead(sData.get(SData.Key.afterRead)))
      Digest.perIdentifier.values.foldLeft(updated)((sData, mechanism) ⇒ mechanism.initAcquire(sData))
    case None ⇒
      sData
  }
  /** Initialize SData for freeze process. */
  def initFreeze(sData: SData): SData = sData.get(Digest.Key.freeze) match {
    case Some(parameters) ⇒
      // Add trailing slash to digestAlgorithm keys
      val updatedDigestAlgorithm = immutable.Map(parameters.map {
        case (uri, algorithm) ⇒ Serialization.inner.addTrailingSlash(uri) -> algorithm
      }.toSeq: _*)
      val updated = sData.
        updated(Digest.Key.freeze, updatedDigestAlgorithm).
        updated(SData.Key.encodeFilter, new freeze.EncodeFilter(sData.get(SData.Key.encodeFilter))).
        updated(SData.Key.afterWrite, new freeze.AfterWrite(sData.get(SData.Key.afterWrite))).
        updated(SData.Key.afterFreeze, new freeze.AfterFreeze(sData.get(SData.Key.afterFreeze)))
      Digest.perIdentifier.values.foldLeft(updated)((sData, mechanism) ⇒ mechanism.initFreeze(sData))
    case None ⇒
      sData
  }

  /** Get digest parameter for the specific modification of the storage. */
  protected def getDigestParameters(modified: Element.Timestamp, transport: Transport, sData: SData): Mechanism.Parameters = {
    val storageURI = sData(SData.Key.storageURI)
    log.debug(s"Load digest information from ${storageURI}")
    // Read type info.
    val mechanismTypeURI = Digest.digestURI(storageURI, transport, modified, Digest.typeName)
    val lines = try {
      val typeStream = transport.openRead(Serialization.inner.encode(mechanismTypeURI, sData), sData)
      try scala.io.Source.fromInputStream(typeStream).getLines.toList
      catch {
        case e: Throwable ⇒
          log.debug(s"Unable to read ${mechanismTypeURI}: " + e.getMessage())
          Seq()
      } finally try typeStream.close() catch { case e: Throwable ⇒ }
    } catch {
      case e: Throwable ⇒
        log.debug(s"Unable to read ${mechanismTypeURI}: " + e.getMessage())
        Seq()
    }
    // Create digest parameters.
    if (lines.nonEmpty) {
      val (mechanismIdentifier :: digestAlgorithm :: arguments) = lines
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
    sData.updated(Digest.historyPerURI, sData(Digest.historyPerURI).updated(sData(SData.Key.storageURI), immutable.Map(content: _*)))
  }

  /**
   * Container for acquire hooks.
   */
  class Acquire {
    /**
     * Hook that compare calculated digest with original value.
     */
    class AfterRead(val userAfterRead: Option[Function4[URI, Array[Byte], Transport, SData, _]])
      extends Function4[URI, Array[Byte], Transport, SData, Unit] {
      /** Compare calculated digest with original value. */
      def apply(uri: URI, data: Array[Byte], transport: Transport, sData: SData) {
        sData(Digest.historyPerURI).get(sData(SData.Key.storageURI)) match {
          case Some(history) ⇒
            Option(sData(Digest.modifiedCache).get()).foreach { modified ⇒
              history.get(modified).foreach {
                case ((parameters, context)) if parameters.mechanism != null ⇒
                  parameters.mechanism.afterRead(parameters, context, modified, uri, data, transport, sData)
              }
              sData(Digest.modifiedCache).remove()
            }
          case None ⇒
        }
        userAfterRead.foreach(_(uri, data, transport, sData))
      }
    }
    /**
     * Hook that adds DigestInputStream to stream sequence.
     */
    class DecodeFilter(val userFilter: Option[Function4[InputStream, URI, Transport, SData, InputStream]])
      extends Function4[InputStream, URI, Transport, SData, InputStream] {
      /** Apply MessageDigest instance to InputStream. */
      def apply(is: InputStream, uri: URI, transport: Transport, sData: SData): InputStream = {
        (try Option(transport.readTimestamp(uri, sData)) catch { case e: Throwable ⇒ None }) match {
          case Some(modified) ⇒
            sData(Digest.historyPerURI).get(sData(SData.Key.storageURI)) match {
              case Some(history) ⇒
                history.get(modified) match {
                  case Some((parameters, context)) if parameters.mechanism != null ⇒
                    val stream = parameters.mechanism.beforeRead(parameters, context, modified, is, uri, transport, sData)
                    sData(Digest.modifiedCache).set(modified)
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
      }
    }
    /**
     * Hook that propagate Digest.historyPerURI with retrospective data.
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
            adjustedSData(Digest.historyPerURI).updated(storageURI, immutable.Map(content.toSeq: _*)))
        }
        new Serialization.Loader(loader.sources, loader.modified, adjustedSData)
      }
    }
    /**
     * Hook that prepare Digest.historyPerURI for particular storage.
     */
    class InitializeSourceSData(val userInitialize: Option[Function3[Element.Timestamp, Transport, SData, SData]])
      extends Function3[Element.Timestamp, Transport, SData, SData] {
      /** Load digest map from storage. */
      def apply(modified: Element.Timestamp, transport: Transport, sData: SData): SData =
        sData(Digest.historyPerURI).get(sData(SData.Key.storageURI)) match {
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
        sData(Digest.Key.freeze).get(sData(SData.Key.storageURI)).foreach(parameter ⇒
          if (parameter.mechanism != null)
            parameter.mechanism.afterFreeze(parameter, graph, transport, sData))
        userAfterFreeze.foreach(_(graph, transport, sData))
      }
    }
    /**
     * Hook that saves calculated digest to somewhere.
     */
    class AfterWrite(val userAfterWrite: Option[Function4[URI, Array[Byte], Transport, SData, _]])
      extends Function4[URI, Array[Byte], Transport, SData, Unit] {
      /** Handle value calculated by MessageDigest. */
      def apply(uri: URI, data: Array[Byte], transport: Transport, sData: SData) = {
        sData(Digest.Key.freeze).get(sData(SData.Key.storageURI)).foreach(parameter ⇒
          if (parameter.mechanism != null)
            parameter.mechanism.afterWrite(parameter, uri, data, transport, sData))
        userAfterWrite.foreach(_(uri, data, transport, sData))
      }
    }
    /**
     * Hook that adds DigestOutputStream to stream sequence.
     */
    class EncodeFilter(val userFilter: Option[Function4[OutputStream, URI, Transport, SData, OutputStream]])
      extends Function4[OutputStream, URI, Transport, SData, OutputStream] {
      /** Apply MessageDigest instance to OutputStream. */
      def apply(os: OutputStream, uri: URI, transport: Transport, sData: SData): OutputStream =
        sData(Digest.Key.freeze).get(sData(SData.Key.storageURI)) match {
          case Some(parameter) if parameter.mechanism != null ⇒
            val stream = parameter.mechanism.beforeWrite(parameter, os, uri, transport, sData)
            userFilter.map(_(stream, uri, transport, sData)) getOrElse stream
          case _ ⇒
            userFilter.map(_(os, uri, transport, sData)) getOrElse os
        }
    }
  }
}

object Digest extends Loggable {
  implicit def digest2implementation(d: Digest.type): Digest = d.inner
  type History = immutable.Map[Element.Timestamp, (Mechanism.Parameters, AtomicReference[SoftReference[AnyRef]])]
  /**
   * Composite container that merges all available digests per storage URI.
   * For example:
   *   There was modification A with digest Solid(MD5)
   *   There was modification B with digest MySolid(X10)
   *   There was modification C with digest Solid(SHA-512)
   *
   *   then container contains 3 records A,B,C with suitable Digest.Parameters and empty AtomicReference.
   *
   *   AtomicReference is SoftReference container with a context information of the specific digest.
   *   AtomicReference with SoftReference is using for lazy loading.
   *   There is hash map with sums as example of such type information.
   */
  val historyPerURI = SData.key[immutable.Map[URI, Digest.History]]("digest")
  /** Cache for Element.Timestamp value. */
  val modifiedCache = SData.key[ThreadLocal[Element.Timestamp]]("digestModifiedCache")

  /** Get default digest algorithm. */
  def defaultAlgorithm = DI.defaultAlgorithm
  /** Get digest container name. */
  def containerName = DI.containerName
  /** Get digest file extension. */
  def extension = DI.extension
  /** Get digest name. */
  def digestURI(baseURI: URI, transport: Transport, modified: Element.Timestamp, part: String*) =
    transport.append(baseURI, (Seq(containerName, Timestamp.dump(modified)) ++ part): _*)
  /** Get digest implementation. */
  def inner = DI.implementation
  /** Map of all available digest implementations. */
  def perIdentifier = DI.perIdentifier
  /** Get digest type name. */
  def typeName = DI.typeName
  /**
   * Predefined SData keys.
   */
  object Key {
    /** Acquire parameter. Digest mandatory if true. */
    val acquire = SData.key[Boolean]("digest")
    /** Freeze parameters per storageURI. */
    val freeze = SData.key[immutable.Map[URI, Mechanism.Parameters]]("digest")
  }
  /**
   * No mechanism parameter.
   */
  case object NoDigest extends Mechanism.Parameters {
    /** Digest algorithm name. */
    val algorithm = ""
    /** Mechanism instance. */
    val mechanism: Mechanism = null
  }
  /**
   * Dependency injection routines.
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    /** Default digest algorithm. */
    lazy val defaultAlgorithm = injectOptional[String]("Digest.DefaultAlgorithm") getOrElse "SHA-512"
    /** Digest container name. */
    lazy val containerName = injectOptional[String]("Digest.ContainerName") getOrElse "digest"
    /** Digest file extension. */
    lazy val extension = injectOptional[String]("Digest.Extension") getOrElse "digest"
    /** Digest file name with digest type. */
    lazy val typeName = injectOptional[String]("Digest.TypeName") getOrElse "type"
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
            case Some(name) if name.startsWith("Serialization.Digest.") ⇒
              log.debug(s"'${name}' loaded.")
            case _ ⇒
              log.debug(s"'${key.name.getOrElse("Unnamed")}' digest mechanism skipped.")
          }
          bindingModule.injectOptional(key).asInstanceOf[Option[Mechanism]]
      }.flatten.toSeq
      assert(mechanisms.distinct.size == mechanisms.size, "digest mechanisms contain duplicated entities in " + mechanisms)
      immutable.HashMap(mechanisms.map(m ⇒ m.identifier -> m): _*)
    }
  }
}
