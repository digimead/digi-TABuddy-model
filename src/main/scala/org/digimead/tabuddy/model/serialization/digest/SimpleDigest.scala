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

import java.io.{ BufferedInputStream, BufferedOutputStream, BufferedReader, InputStream, InputStreamReader, OutputStream, PrintStream }
import java.net.URI
import java.security.{ DigestInputStream, DigestOutputStream, MessageDigest }
import java.util.concurrent.atomic.AtomicReference
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.signature.Signature
import org.digimead.tabuddy.model.serialization.transport.Transport
import org.digimead.tabuddy.model.serialization.{ SData, Serialization }
import scala.collection.{ immutable, mutable }
import scala.ref.SoftReference
import scala.reflect.runtime.universe

/*
 * SimpleDigest layout is
 *   storageURI/digest/${graph.modification}/type - process description
 *   storageURI/digest/${graph.modification}/digest - block with digest information that is related to graph.modification
 */
/**
 * Provides digest mechanism for serialization process.
 */
class SimpleDigest extends Mechanism with Loggable {
  /** Identifier of the digest. */
  val identifier = SimpleDigest.Identifier

  /** Get simple mechanism parameters. */
  def apply(algorithmName: String, args: String*): Mechanism.Parameters = {
    if (args.nonEmpty)
      throw new IllegalArgumentException("Unknown parameters: " + args.mkString(", "))
    SimpleDigestParameters(algorithmName)
  }
  /** Just invoked before freeze completion. */
  def afterFreeze(parameters: Mechanism.Parameters, graph: Graph[_ <: Model.Like], transport: Transport, sData: SData) =
    parameters match {
      case SimpleDigestParameters(algorithmName) ⇒
        val storageURI = sData(SData.Key.storageURI)
        log.debug(s"Save digest ${algorithmName} data to ${storageURI}")
        // Write type info.
        val mechanismTypeURI = Digest.digestURI(storageURI, transport, graph.modified, Digest.typeName)
        if (!transport.exists(Serialization.inner.encode(mechanismTypeURI, sData), sData) ||
          sData.get(SData.Key.force) == Some(true)) {
          val os = sData.get(SData.Key.writeFilter) match {
            case Some(filter) ⇒
              val os = transport.openWrite(Serialization.inner.encode(mechanismTypeURI, sData), sData, true)
              filter(new BufferedOutputStream(os), mechanismTypeURI, transport,
                // Prevent calculation of digest and signature for 'mechanismTypeURI' file.
                sData - Digest.Key.freeze - Signature.Key.freeze)
            case None ⇒
              val os = transport.openWrite(Serialization.inner.encode(mechanismTypeURI, sData), sData, true)
              new BufferedOutputStream(os)
          }
          val pos = new PrintStream(os)
          try {
            pos.println(SimpleDigest.Identifier.name)
            pos.println()
            pos.println(algorithmName)
            pos.flush()
          } finally try pos.close() catch {
            case e: SecurityException ⇒ throw e
            case e: Throwable ⇒ log.error("Unable to save digest type info: " + e.getMessage, e)
          }
        }
        // Write payload.
        sData.get(SimpleDigest.printStream).foreach(streamContainer ⇒
          streamContainer.synchronized {
            Option(streamContainer.get).foreach { stream ⇒
              log.debug(s"Close container with digests.")
              stream.flush()
              stream.close()
              streamContainer.set(null)
            }
          })
      case unexpected ⇒
        throw new IllegalArgumentException("Unexpected parameters " + unexpected)
    }
  /** Initialize SData for acquire process. */
  def initAcquire(sData: SData): SData = sData
  /** Initialize SData for freeze process. */
  def initFreeze(sData: SData): SData = sData.get(Digest.Key.freeze) match {
    case Some(parameters) if parameters.exists { case (uri, parameters) ⇒ parameters.isInstanceOf[SimpleDigestParameters] } ⇒
      sData.updated(SimpleDigest.printStream, new AtomicReference[PrintStream]())
    case _ ⇒
      sData
  }
  /** Just invoked after read beginning. */
  def readFilter(parameters: Mechanism.Parameters, context: AtomicReference[SoftReference[AnyRef]],
    modified: Element.Timestamp, is: InputStream, uri: URI, transport: Transport, sData: SData): InputStream =
    parameters match {
      case SimpleDigestParameters(algorithmName) ⇒
        val verifier = MessageDigest.getInstance(algorithmName)
        new Digest.DigestInputStream(is, verifier, verifier ⇒
          try validateDigest(verifier, context, modified, uri, transport, sData)
          catch {
            case e: SecurityException ⇒
              throw e
            case e: Throwable ⇒
              log.error("Error occurred while validating digest: " + e.getMessage(), e)
              Digest.refuse(uri, sData)
          })
      case unexpected ⇒
        throw new IllegalArgumentException("Unexpected parameters " + unexpected)
    }
  /** Just invoked after write beginning. */
  def writeFilter(parameters: Mechanism.Parameters, os: OutputStream, uri: URI, transport: Transport, sData: SData): OutputStream =
    parameters match {
      case SimpleDigestParameters(algorithmName) ⇒
        val digest = MessageDigest.getInstance(algorithmName)
        new Digest.DigestOutputStream(os, digest, digest ⇒
          writeDigest(digest, uri, transport, sData))
      case unexpected ⇒
        throw new IllegalArgumentException("Unexpected parameters " + unexpected)
    }

  /** Check digest data. */
  @throws[SecurityException]("if verification is failed")
  protected def validateDigest(verifier: java.security.MessageDigest, context: AtomicReference[SoftReference[AnyRef]],
    modified: Element.Timestamp, uri: URI, transport: Transport, sData: SData) {
    val storageURI = sData(SData.Key.storageURI)
    val resourceURI = storageURI.relativize(uri)
    val map = getDigestMap(context, modified, transport, sData)
    map.get(resourceURI) match {
      case Some(originalValue) ⇒
        val actualValue = verifier.digest()
        if (java.util.Arrays.equals(originalValue, actualValue))
          Digest.approve(uri, sData)
        else
          Digest.refuse(uri, sData)
      case None ⇒
        throw new IllegalStateException("Unable to find digest for .../" + resourceURI)
    }
  }
  /** Get loaded or load new digest map. */
  protected def getDigestMap(context: AtomicReference[SoftReference[AnyRef]],
    modified: Element.Timestamp, transport: Transport, sData: SData): Map[URI, Array[Byte]] = {
    context.get().get match {
      case Some(map: Map[_, _]) ⇒ return map.asInstanceOf[Map[URI, Array[Byte]]]
      case _ ⇒ context.get().clear
    }
    val storageURI = sData(SData.Key.storageURI)
    log.debug(s"Load digest data from storage ${storageURI}")
    val builder = Map.newBuilder[URI, Array[Byte]]
    val digestDataURI = Digest.digestURI(storageURI, transport, modified, Digest.containerName)
    val is = sData.get(SData.Key.readFilter) match {
      case Some(filter) ⇒
        val is = transport.openRead(Serialization.inner.encode(digestDataURI, sData), sData)
        filter(is, digestDataURI, transport,
          // Prevent validation of digest and signature for 'digestDataURI' file.
          sData - Digest.Key.acquire - Signature.Key.acquire)
      case None ⇒
        transport.openRead(Serialization.inner.encode(digestDataURI, sData), sData)
    }
    val reader = sData.get(Digest.Key.readFilter) match {
      case Some(filter) ⇒
        new BufferedReader(new InputStreamReader(filter(new BufferedInputStream(is),
          Digest.digestURI(storageURI, transport, modified).relativize(digestDataURI),
          transport, sData.updated(SData.Key.modified, modified))))
      case None ⇒
        new BufferedReader(new InputStreamReader(new BufferedInputStream(is)))
    }
    try {
      var line = reader.readLine()
      while (line != null) {
        val hash = line.takeWhile(_ != ' ')
        val uri = new URI(line.drop(hash.length() + 1))
        builder += uri -> Serialization.hexStringToByteArray(hash)
        line = reader.readLine()
      }
    } finally try reader.close() catch {
      case e: SecurityException ⇒ throw e
      case e: Throwable ⇒ log.error("Unable to load digest data: " + e.getMessage, e)
    }
    val map = builder.result
    context.set(new SoftReference(map))
    map
  }
  /** Write digest data to file. */
  protected def writeDigest(digest: java.security.MessageDigest, uri: URI, transport: Transport, sData: SData): Unit = synchronized {
    sData.get(SimpleDigest.printStream).foreach(streamContainer ⇒ streamContainer.synchronized {
      val modified = sData(SData.Key.modified)
      val storageURI = sData(SData.Key.storageURI)
      val printStream = Option(streamContainer.get) getOrElse {
        val digestDataURI = Digest.digestURI(storageURI, transport, modified, Digest.containerName)
        if (!transport.exists(Serialization.inner.encode(digestDataURI, sData), sData) ||
          sData.get(SData.Key.force) == Some(true)) {
          log.debug(s"Open container with digests for ${modified} at ${digestDataURI}")
          val os = sData.get(SData.Key.writeFilter) match {
            case Some(filter) ⇒
              val os = transport.openWrite(Serialization.inner.encode(digestDataURI, sData), sData, true)
              filter(new BufferedOutputStream(os), digestDataURI, transport,
                // Prevent calculation of digest and signature for 'digestDataURI' file.
                sData - Digest.Key.freeze - Signature.Key.freeze)
            case None ⇒
              val os = transport.openWrite(Serialization.inner.encode(digestDataURI, sData), sData, true)
              new BufferedOutputStream(os)
          }
          val printStream = sData.get(Digest.Key.writeFilter) match {
            case Some(filter) ⇒
              new PrintStream(filter(os,
                Digest.digestURI(storageURI, transport, modified).relativize(digestDataURI),
                transport, sData.updated(SData.Key.modified, modified)))
            case None ⇒
              new PrintStream(os)
          }
          streamContainer.set(printStream)
          printStream
        } else {
          log.debug(s"Skip digest data ${modified} for storage ${storageURI}")
          return
        }
      }
      log.debug(s"Append digest for '${uri}'.")
      printStream.println(Serialization.byteArrayToHexString(digest.digest()) + " " + storageURI.relativize(uri))
    })
  }
  /**
   * SimpleDigest parameters.
   */
  case class SimpleDigestParameters(val algorithm: String) extends Mechanism.Parameters {
    /** Digest parameters as sequence of strings. */
    val arguments: Seq[String] = Seq.empty
    /** Mechanism instance. */
    val mechanism = SimpleDigest.this
  }
}

object SimpleDigest {
  /** Simple digest print stream. */
  val printStream = SData.key[AtomicReference[PrintStream]]("simpleDigestPrintStream")

  /** Get SimpleDigest mechanism parameters. */
  def apply(algorithm: String): Mechanism.Parameters = Digest.perIdentifier.get(Identifier) match {
    case Some(digest: SimpleDigest) ⇒ digest(algorithm)
    case _ ⇒ throw new IllegalStateException("SimpleDigest mechanism is not available.")
  }

  /**
   * SimpleDigest mechanism identifier.
   */
  object Identifier extends Mechanism.Identifier { val name = "simple" }
}
