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

package org.digimead.tabuddy.model.serialization.signature

import java.io.{ FilterInputStream, FilterOutputStream, InputStream, OutputStream }
import java.net.URI
import java.security.PublicKey
import java.util.concurrent.atomic.AtomicReference
import org.digimead.digi.lib.api.XDependencyInjection
import org.digimead.digi.lib.log.api.XLoggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.digest.Digest
import org.digimead.tabuddy.model.serialization.digest
import org.digimead.tabuddy.model.serialization.transport.Transport
import org.digimead.tabuddy.model.serialization.yaml.Timestamp
import org.digimead.tabuddy.model.serialization.{ SData, Serialization }
import scala.collection.{ immutable, mutable }
import scala.language.implicitConversions
import scala.ref.SoftReference
import scala.util.control.ControlThrowable

class Signature extends XLoggable {
  /** Acquire routines. */
  protected val acquire = new Acquire
  /** Freeze routines. */
  protected val freeze = new Freeze

  /** Approve signature. */
  def approve(resourceURI: URI, sData: SData) =
    log.debug("Approve signature for .../" + sData(SData.Key.storageURI).relativize(resourceURI))
  /** Initialize SData for acquire process. */
  def initAcquire(sData: SData): SData = sData.get(Signature.Key.acquire) match {
    case Some(validator) ⇒
      val updated = sData.
        updated(Digest.Key.readFilter, new acquire.ReadFilter(sData.get(Digest.Key.readFilter))).
        updated(SData.Key.initializeLoader, new acquire.InitializeLoader(sData.get(SData.Key.initializeLoader))).
        updated(SData.Key.initializeSourceSData, new acquire.InitializeSourceSData(sData.get(SData.Key.initializeSourceSData))).
        updated(Signature.historyPerURI, Map[URI, Signature.History]())
      Signature.perIdentifier.values.foldLeft(updated)((sData, mechanism) ⇒ mechanism.initAcquire(sData))
    case None ⇒
      sData
  }
  /** Initialize SData for freeze process. */
  def initFreeze(sData: SData): SData = sData.get(Signature.Key.freeze) match {
    case Some(rawParametersMap) ⇒
      val parametersMap = rawParametersMap.map { case (uri, parameters) ⇒ Serialization.inner.addTrailingSlash(uri) -> parameters }
      val definedStorages = parametersMap.keySet
      val newParameters = sData.get(SData.Key.explicitStorages) match {
        case Some(storages: Serialization.Storages) ⇒
          storages.seq.map(_.real).flatten.
            filterNot(definedStorages.contains).map(storageURI ⇒ storageURI -> Signature.default)
        case None ⇒
          Nil
      }
      val signatureParametersMap = parametersMap ++ Map(newParameters.toSeq: _*)
      val signatureReady = if (signatureParametersMap.nonEmpty) {
        log.debug("Freeze with signature parameters: " + signatureParametersMap)
        sData.updated(Signature.Key.freeze, signatureParametersMap)
      } else
        sData
      val withDigest = signatureReady.get(Digest.Key.freeze) match {
        case Some(key) ⇒ signatureReady
        case None ⇒ signatureReady.updated(Digest.Key.freeze, Map[URI, digest.Mechanism.Parameters]())
      }
      val updated = withDigest.
        updated(Digest.Key.writeFilter, new freeze.WriteFilter(sData.get(Digest.Key.writeFilter))).
        updated(SData.Key.afterFreeze, new freeze.AfterFreeze(sData.get(SData.Key.afterFreeze)))
      Signature.perIdentifier.values.foldLeft(updated)((sData, mechanism) ⇒ mechanism.initFreeze(sData))
    case None ⇒
      sData
  }
  /** Get signature history for loader. */
  def history(loader: Serialization.Loader): Map[Element.Timestamp, Map[URI, Mechanism.Parameters]] =
    loader.sData.get(Signature.historyPerURI).map { history ⇒
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
  /** Get signature history for graph. */
  def history(graph: Graph[_ <: Model.Like]): Map[Element.Timestamp, Map[URI, Mechanism.Parameters]] =
    graph.storages match {
      case Nil ⇒
        immutable.HashMap()
      case storages ⇒
        storages.sortBy(_.getScheme == "file").foreach { bootstrapStorageURI ⇒
          try {
            return history(Serialization.acquireLoader(bootstrapStorageURI,
              graph.retrospective.last, SData(Signature.Key.acquire -> Signature.acceptAll)))
          } catch {
            case ce: ControlThrowable ⇒ throw ce
            case e: Throwable ⇒ log.fatal(e.getMessage(), e)
          }
        }
        immutable.HashMap()
    }
  /** Refuse signature. */
  @throws[SecurityException]("if validation is failed")
  def refuse(resourceURI: URI, sData: SData) =
    throw new SecurityException("Validation failed for " + sData(SData.Key.storageURI).relativize(resourceURI))

  /** Get signature parameter for the specific modification of the storage. */
  protected def getSignatureParameters(modified: Element.Timestamp, transport: Transport, sData: SData): Mechanism.Parameters = try {
    val storageURI = sData(SData.Key.storageURI)
    log.debug(s"Load signature information at ${modified} from storage ${storageURI} ")
    // Read type info.
    val mechanismTypeURI = Signature.signatureURI(storageURI, transport, modified, Signature.typeName)
    val lines = try {
      val is = sData.get(SData.Key.readFilter) match {
        case Some(filter) ⇒
          val is = transport.openRead(Serialization.inner.encode(mechanismTypeURI, sData), sData)
          filter(is, mechanismTypeURI, transport,
            // Prevent validation of digest and signature for 'mechanismTypeURI' file.
            sData - Digest.Key.acquire - Signature.Key.acquire)
        case None ⇒
          transport.openRead(Serialization.inner.encode(mechanismTypeURI, sData), sData)
      }
      try scala.io.Source.fromInputStream(is).getLines.toList
      catch {
        case e: Throwable ⇒
          log.debug(s"Unable to read ${mechanismTypeURI}: " + e.getMessage())
          Seq()
      } finally try is.close() catch {
        case e: SecurityException ⇒ throw e
        case e: Throwable ⇒ log.error("Unable to load signature information: " + e.getMessage, e)
      }
    } catch {
      case e: Throwable ⇒
        log.debug(s"Unable to read ${mechanismTypeURI}: " + e.getMessage())
        Seq()
    }
    // Create signature parameters.
    if (lines.nonEmpty) {
      val (mechanismIdentifier :: keyAlgorithm :: arguments) = {
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
      Signature.perIdentifier.find(_._1.name == mechanismIdentifier) match {
        case Some((identifier, mechanism)) ⇒
          try {
            val parameters = mechanism(keyAlgorithm, arguments: _*)
            log.info(s"Create ${parameters} for storage ${storageURI}")
            parameters
          } catch {
            case e: Throwable ⇒
              log.warn(s"Unable to create parameters for ${identifier}. Arguments: " + (Seq(mechanismIdentifier, keyAlgorithm) ++ arguments).mkString(", "), e)
              Signature.NoSignature
          }
        case None ⇒
          log.info(s"Unable to find registered mechanism for ${mechanismIdentifier}.")
          Signature.NoSignature
      }
    } else {
      log.info(s"Signature parameters not found for storage ${storageURI}")
      Signature.NoSignature
    }
  } catch {
    case e: MatchError ⇒
      log.warn(s"Unable to get signature parameters: broken type descriptor.")
      Signature.NoSignature
    case e: Throwable ⇒
      log.warn(s"Unable to get signature parameters: " + e.getMessage())
      Signature.NoSignature
  }
  /** Update history for storage. */
  protected def updateHistory(history: Seq[(Element.Timestamp, Mechanism.Parameters)], transport: Transport, sData: SData): SData = {
    val content: Seq[(Element.Timestamp, (Mechanism.Parameters, AtomicReference[SoftReference[AnyRef]]))] = history.map {
      case (modified, parameters) ⇒ modified -> ((parameters, new AtomicReference(new SoftReference[AnyRef](null))))
    }
    sData.updated(Signature.historyPerURI, sData(Signature.historyPerURI).updated(sData(SData.Key.storageURI), Map(content: _*)))
  }

  /**
   * Container for acquire hooks.
   */
  class Acquire {
    /**
     * Hook that propagates Signature.historyPerURI with retrospective data.
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
            loader.sData(Signature.historyPerURI).get(storageURI).flatMap(_.get(record) map (record -> _)) getOrElse {
              try {
                log.debug(s"Load signature information for ${record}.")
                val parametersForTimestamp = getSignatureParameters(record, source.transport, sData)
                record -> (parametersForTimestamp, new AtomicReference(new SoftReference[AnyRef](null)))
              } catch {
                case e: Throwable ⇒
                  log.error("Unable to load signature information: " + e.getMessage(), e)
                  record -> (Signature.NoSignature, new AtomicReference(new SoftReference[AnyRef](null)))
              }
            }
          }
          adjustedSData = adjustedSData.updated(Signature.historyPerURI,
            adjustedSData(Signature.historyPerURI).updated(storageURI, Map(content.toSeq: _*)))
        }
        // Reset cached data for Signature.historyPerURI.
        adjustedSData.get(Signature.historyPerURI).foreach(history ⇒
          history.values.foreach(_.values.map(_._2.set(new SoftReference(null)))))
        val updated = new Serialization.Loader(loader.sources, loader.modified, adjustedSData)
        userInitialize.foldLeft(updated)((loader, hook) ⇒ hook(loader))
      }
    }
    /**
     * Hook that prepare Signature.historyPerURI for particular storage.
     */
    class InitializeSourceSData(val userInitialize: Option[Function3[Element.Timestamp, Transport, SData, SData]])
      extends Function3[Element.Timestamp, Transport, SData, SData] {
      /** Load digest map from storage. */
      def apply(modified: Element.Timestamp, transport: Transport, sData: SData): SData = {
        val updated = sData(Signature.historyPerURI).get(sData(SData.Key.storageURI)) match {
          case None ⇒
            /*
             * Update SData historyPerURI.
             * Add only one signature for the specified 'modified' value.
             * It is allow to load and validate record resources.
             */
            val parametersForTimestamp = getSignatureParameters(modified, transport, sData)
            updateHistory(Seq((modified, parametersForTimestamp)), transport, sData)
          case Some(parameters) ⇒
            sData // Skip. Already initialized.
        }
        userInitialize.foldLeft(updated)((sData, hook) ⇒ hook(modified, transport, sData))
      }
    }
    /**
     * Hook that initiates signature calculation.
     */
    class ReadFilter(val userFilter: Option[Function4[InputStream, URI, Transport, SData, InputStream]])
      extends Function4[InputStream, URI, Transport, SData, InputStream] {
      /** Apply signature verification instance to InputStream. */
      def apply(is: InputStream, uri: URI, transport: Transport, sData: SData): InputStream =
        if (sData.isDefinedAt(Signature.Key.acquire)) {
          if (uri.isAbsolute())
            throw new IllegalArgumentException(s"Unable to calculate signature for absolute URI ${uri}")
          sData.get(SData.Key.modified) match {
            case Some(modified) ⇒
              sData(Signature.historyPerURI).get(sData(SData.Key.storageURI)) match {
                case Some(history) ⇒
                  history.get(modified) match {
                    case Some((parameters, context)) if parameters.mechanism != null ⇒
                      val stream = parameters.mechanism.readFilter(parameters, context, modified, is, uri, transport, sData)
                      userFilter.map(_(stream, uri, transport, sData)) getOrElse stream
                    case _ ⇒
                      val storageURI = sData(SData.Key.storageURI)
                      if (sData.get(Digest.Key.acquire) == Some(false)) {
                        // Initialization stage
                        // sData.get(Digest.Key.acquire) == Some(false) is only at acquireGraphLoader
                        userFilter.map(_(is, uri, transport, sData)) getOrElse is
                      } else if (sData(Signature.Key.acquire)(None)) {
                        approve(Digest.digestURI(storageURI, transport, modified).resolve(uri), sData)
                        userFilter.map(_(is, uri, transport, sData)) getOrElse is
                      } else {
                        refuse(Digest.digestURI(storageURI, transport, modified).resolve(uri), sData)
                        is
                      }
                  }
                case _ ⇒
                  sData.get(Signature.Key.acquire).map(_(None))
                  userFilter.map(_(is, uri, transport, sData)) getOrElse is
              }
            case None ⇒
              throw new SecurityException(s"Unable to find modification timestamp for signature validation.")
          }
        } else
          userFilter.map(_(is, uri, transport, sData)) getOrElse is
    }
  }
  /**
   * Container for freeze hooks.
   */
  class Freeze {
    /**
     * Hook that closes signature resources.
     */
    class AfterFreeze(val userAfterFreeze: Option[Function3[Graph[_ <: Model.Like], Transport, SData, _]])
      extends Function3[Graph[_ <: Model.Like], Transport, SData, Unit] {
      /** Save digest results. */
      def apply(graph: Graph[_ <: Model.Like], transport: Transport, sData: SData) {
        sData(Signature.Key.freeze).get(sData(SData.Key.storageURI)).foreach(parameter ⇒
          if (parameter.mechanism != null)
            parameter.mechanism.afterFreeze(parameter, graph, transport, sData))
        userAfterFreeze.foreach(_(graph, transport, sData))
      }
    }
    /**
     * Hook that calculates signature.
     */
    class WriteFilter(val userFilter: Option[Function4[OutputStream, URI, Transport, SData, OutputStream]])
      extends Function4[OutputStream, URI, Transport, SData, OutputStream] {
      /** Apply signature generation instance to OutputStream. */
      def apply(os: OutputStream, uri: URI, transport: Transport, sData: SData): OutputStream = {
        if (uri.isAbsolute())
          throw new IllegalArgumentException(s"Unable to calculate digest for absolute URI ${uri}")
        sData.get(Signature.Key.freeze) match {
          case Some(freezeMap) ⇒
            freezeMap.get(sData(SData.Key.storageURI)) match {
              case Some(parameter) if parameter.mechanism != null ⇒
                val stream = parameter.mechanism.writeFilter(parameter, os, uri, transport, sData)
                userFilter.map(_(stream, uri, transport, sData)) getOrElse stream
              case _ ⇒
                userFilter.map(_(os, uri, transport, sData)) getOrElse os
            }
          case None ⇒
            userFilter.map(_(os, uri, transport, sData)) getOrElse os
        }
      }
    }
  }
}

object Signature extends XLoggable {
  implicit def signature2implementation(s: Signature.type): Signature = s.inner
  type History = Map[Element.Timestamp, (Mechanism.Parameters, AtomicReference[SoftReference[AnyRef]])]
  /** Accept signed and unsigned. */
  lazy val acceptAll = (_: Option[PublicKey]) ⇒ true
  /** Accept any signed. */
  lazy val acceptSigned = (key: Option[PublicKey]) ⇒ key.nonEmpty
  /**
   * Composite container that merges all available signature parameters per storage URI.
   * For example:
   *   There was modification A with signature by Alice RSA 4096
   *   There was modification B with signature by Bob DSA 1024
   *   There was modification C with signature by Dave ECDSA N
   *
   *   then container contains 3 records A,B,C with suitable Mechanism.Parameters and empty context - AtomicReference.
   *
   *   AtomicReference is SoftReference container with a context information of the specific signature.
   *   AtomicReference with SoftReference is using for lazy loading.
   *   There is hash map with signatures as an example of such type information.
   */
  val historyPerURI = SData.key[Map[URI, Signature.History]]("signature")

  /** Get digest container name. */
  def containerName = DI.containerName
  /** Get default signature parameters. */
  def default = DI.default
  /** Get signature implementation. */
  def inner = DI.implementation
  /** Map of all available signature implementations. */
  def perIdentifier = DI.perIdentifier
  /** Get URI of signature data. */
  def signatureURI(baseURI: URI, transport: Transport, modified: Element.Timestamp, part: String*) =
    transport.append(baseURI, (Seq(containerName, Timestamp.dump(modified)) ++ part): _*)
  /** Get digest type name. */
  def typeName = DI.typeName

  /**
   * Predefined SData keys.
   */
  object Key {
    /** Acquire parameter. Validation is blocked by user with SecurityException. */
    val acquire = SData.key[Option[PublicKey] ⇒ Boolean]("signature")
    /** Freeze parameters per storageURI. */
    val freeze = SData.key[Map[URI, Mechanism.Parameters]]("signature")
  }
  /**
   * No mechanism parameter.
   */
  case object NoSignature extends Mechanism.Parameters {
    /** Signature algorithm name. */
    val algorithm: String = ""
    /** Signature parameters as sequence of strings. */
    val arguments: Seq[String] = Seq.empty
    /** Mechanism instance. */
    val mechanism: Mechanism = null

    /** Get public key for the current parameter. */
    def publicKey = ???
  }
  /**
   * A transparent stream that updates the associated verifier using the bits going through the stream.
   */
  class SignatureInputStream[T](val stream: InputStream, val verifier: java.security.Signature,
    onClose: java.security.Signature ⇒ T) extends FilterInputStream(stream) {
    override def close() {
      super.close()
      onClose(verifier)
    }
    override def read(): Int = {
      val result = in.read()
      if (result != -1)
        verifier.update(result.asInstanceOf[Byte])
      result
    }
    override def read(b: Array[Byte], off: Int, len: Int) = {
      val result = in.read(b, off, len)
      if (result > 0)
        verifier.update(b.take(result))
      result
    }
    override def read(b: Array[Byte]): Int = read(b, 0, b.length)
  }
  /**
   * A transparent stream that updates the associated signature using the bits going through the stream.
   */
  class SignatureOutputStream[T](val stream: OutputStream, val signature: java.security.Signature,
    onClose: java.security.Signature ⇒ T) extends FilterOutputStream(stream) {
    override def close() {
      super.close()
      onClose(signature)
    }
    override def write(b: Int) {
      out.write(b)
      signature.update(b.asInstanceOf[Byte])
    }
    override def write(b: Array[Byte], off: Int, len: Int) {
      out.write(b, off, len)
      signature.update(b.slice(off, off + len))
    }
    override def write(b: Array[Byte]) = write(b, 0, b.length)
  }
  /**
   * Dependency injection routines.
   */
  private object DI extends XDependencyInjection.PersistentInjectable {
    /** Default signature algorithm. */
    lazy val default = injectOptional[Mechanism.Parameters]("Signature.Default") getOrElse NoSignature
    /** Signature container name. */
    lazy val containerName = injectOptional[String]("Signature.ContainerName") getOrElse "signature"
    /** Signature implementation. */
    lazy val implementation = injectOptional[Signature] getOrElse new Signature
    /**
     * Per identifier signature mechanisms map.
     *
     * Each collected mechanism must be:
     *  1. an instance of Mechanism
     *  2. has name that starts with "Signature.Mechanism."
     */
    lazy val perIdentifier: immutable.HashMap[Mechanism.Identifier, Mechanism] = {
      val mechanisms = bindingModule.bindings.filter {
        case (key, value) ⇒ classOf[Mechanism].isAssignableFrom(key.m.runtimeClass)
      }.map {
        case (key, value) ⇒
          key.name match {
            case Some(name) if name.startsWith("Signature.Mechanism.") ⇒
              log.debug(s"'${name}' loaded.")
              bindingModule.injectOptional(key).asInstanceOf[Option[Mechanism]]
            case _ ⇒
              log.debug(s"'${key.name.getOrElse("Unnamed")}' signature mechanism skipped.")
              None
          }
      }.flatten.toSeq
      assert(mechanisms.distinct.size == mechanisms.size, "Signature mechanisms contain duplicated entities in " + mechanisms)
      immutable.HashMap(mechanisms.map(m ⇒ m.identifier -> m): _*)
    }
    /** Signature file name with signature type. */
    lazy val typeName = injectOptional[String]("Signature.TypeName") getOrElse "type"
  }
}
