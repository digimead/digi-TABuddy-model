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
  /** Cache for MessageDigest instance. */
  val digestCache = new ThreadLocal[MessageDigest]()

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
        val digestTypeURI = Digest.digestURI(storageURI, transport, graph.modified, Digest.typeName)
        if (!transport.exists(Serialization.inner.encode(digestTypeURI, sData), sData) ||
          sData.get(SData.Key.force) == Some(true)) {
          val os = transport.openWrite(Serialization.inner.encode(digestTypeURI, sData), sData, true)
          val pos = new PrintStream(new BufferedOutputStream(os))
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
        val digestSumURI = Digest.digestURI(storageURI, transport, graph.modified, Digest.containerName)
        if (!transport.exists(Serialization.inner.encode(digestSumURI, sData), sData) ||
          sData.get(SData.Key.force) == Some(true)) {
          val os = transport.openWrite(Serialization.inner.encode(digestSumURI, sData), sData, true)
          val pos = sData.get(Digest.Key.writeFilter) match {
            case Some(filter) ⇒
              new PrintStream(filter(new BufferedOutputStream(os),
                Digest.digestURI(storageURI, transport, graph.modified).relativize(digestSumURI),
                transport, sData.updated(SData.Key.modified, graph.modified)))
            case None ⇒
              new PrintStream(new BufferedOutputStream(os))
          }
          try {
            sData(SimpleDigest.Key.digestMap)(storageURI).foreach {
              case (uri, digest) ⇒
                pos.println(Serialization.byteArrayToHexString(digest) + " " + uri)
            }
            pos.flush()
          } finally try pos.close() catch {
            case e: SecurityException ⇒ throw e
            case e: Throwable ⇒ log.error("Unable to save digest check sums: " + e.getMessage, e)
          }
        }
      case unexpected ⇒
        throw new IllegalArgumentException("Unexpected parameters " + unexpected)
    }
  /** Just invoked before write completion. */
  def afterWrite(parameters: Mechanism.Parameters, uri: URI, data: Array[Byte], transport: Transport, sData: SData) =
    parameters match {
      case SimpleDigestParameters(algorithmName) ⇒ Option(digestCache.get()).foreach { dInstance ⇒
        val storageURI = sData(SData.Key.storageURI)
        sData(SimpleDigest.Key.digestMap)(storageURI) += storageURI.relativize(uri) -> dInstance.digest()
        dInstance.reset()
        digestCache.set(null)
      }
      case unexpected ⇒
        throw new IllegalArgumentException("Unexpected parameters " + unexpected)
    }
  /** Just invoked after read beginning. */
  def readFilter(parameters: Mechanism.Parameters, context: AtomicReference[SoftReference[AnyRef]],
    modified: Element.Timestamp, is: InputStream, uri: URI, transport: Transport, sData: SData): InputStream =
    parameters match {
      case SimpleDigestParameters(algorithmName) ⇒
        val verifier = MessageDigest.getInstance(algorithmName)
        new Digest.DigestInputStream(is, verifier, verifier ⇒
          checkDigest(verifier, context, modified, uri, transport, sData))
      case unexpected ⇒
        throw new IllegalArgumentException("Unexpected parameters " + unexpected)
    }
  /** Just invoked after write beginning. */
  def writeFilter(parameters: Mechanism.Parameters, os: OutputStream, uri: URI, transport: Transport, sData: SData): OutputStream =
    parameters match {
      case SimpleDigestParameters(algorithmName) ⇒
        val dInstance = MessageDigest.getInstance(algorithmName)
        digestCache.set(dInstance)
        new DigestOutputStream(os, dInstance)
      case unexpected ⇒
        throw new IllegalArgumentException("Unexpected parameters " + unexpected)
    }
  /** Initialize SData for acquire process. */
  def initAcquire(sData: SData): SData = sData
  /** Initialize SData for freeze process. */
  def initFreeze(sData: SData): SData = sData.get(Digest.Key.freeze) match {
    case Some(parameters) if parameters.exists { case (uri, parameters) ⇒ parameters.isInstanceOf[SimpleDigestParameters] } ⇒
      // Create digestMap
      val newDigestMap = immutable.Map(sData(Digest.Key.freeze).keys.map(
        uri ⇒ uri -> new mutable.HashMap[URI, Array[Byte]] with mutable.SynchronizedMap[URI, Array[Byte]]).toSeq: _*)
      sData.updated(SimpleDigest.Key.digestMap, newDigestMap)
    case _ ⇒
      sData
  }

  /** Approve resource check sum. */
  protected def approve(resourceURI: URI, sData: SData) =
    log.debug("Approve digest for .../" + sData(SData.Key.storageURI).relativize(resourceURI))
  /** Check digest data. */
  @throws[SecurityException]("if verification is failed")
  protected def checkDigest(verifier: java.security.MessageDigest, context: AtomicReference[SoftReference[AnyRef]],
    modified: Element.Timestamp, uri: URI, transport: Transport, sData: SData) {
    val storageURI = sData(SData.Key.storageURI)
    val resourceURI = storageURI.relativize(uri)
    val map = getDigestMap(context, modified, transport, sData)
    map.get(resourceURI) match {
      case Some(originalValue) ⇒
        val actualValue = verifier.digest()
        if (java.util.Arrays.equals(originalValue, actualValue))
          approve(uri, sData)
        else
          refuse(uri, sData)
      case None ⇒
        throw new IllegalStateException("Unable to find digest for " + uri)
    }
  }
  /** Get loaded or load new digest map. */
  protected def getDigestMap(context: AtomicReference[SoftReference[AnyRef]],
    modified: Element.Timestamp, transport: Transport, sData: SData): immutable.Map[URI, Array[Byte]] = {
    context.get().get match {
      case Some(map: immutable.Map[_, _]) ⇒ return map.asInstanceOf[immutable.Map[URI, Array[Byte]]]
      case _ ⇒ context.get().clear
    }
    val storageURI = sData(SData.Key.storageURI)
    log.debug(s"Load digest data from storage ${storageURI}")
    val builder = immutable.Map.newBuilder[URI, Array[Byte]]
    val digestSumURI = Digest.digestURI(storageURI, transport, modified, Digest.containerName)
    val digestStream = transport.openRead(Serialization.inner.encode(digestSumURI, sData), sData)
    val reader = sData.get(Digest.Key.readFilter) match {
      case Some(filter) ⇒
        new BufferedReader(new InputStreamReader(filter(new BufferedInputStream(digestStream),
          Digest.digestURI(storageURI, transport, modified).relativize(digestSumURI),
          transport, sData.updated(SData.Key.modified, modified))))
      case None ⇒
        new BufferedReader(new InputStreamReader(new BufferedInputStream(digestStream)))
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
  /** Refuse resource check sum. */
  @throws[SecurityException]("if verification is failed")
  protected def refuse(resourceURI: URI, sData: SData) =
    throw new IllegalStateException("Incorrect digest for " + resourceURI)

  /**
   * SimpleDigest parameters.
   */
  case class SimpleDigestParameters(val algorithm: String) extends Mechanism.Parameters {
    val mechanism = SimpleDigest.this
  }
}

object SimpleDigest {
  /** Get SimpleDigest mechanism parameters. */
  def apply(algorithm: String): Mechanism.Parameters = Digest.perIdentifier.get(Identifier) match {
    case Some(digest: SimpleDigest) ⇒ digest(algorithm)
    case _ ⇒ throw new IllegalStateException("SimpleDigest mechanism is not available.")
  }

  /**
   * SimpleDigest mechanism identifier.
   */
  object Identifier extends Mechanism.Identifier { val name = "simple" }
  /**
   * Predefined keys of a generic process.
   */
  object Key {
    /** Digest map with graph hash sums: storageURI -> Map[Relative URI, digest] */
    val digestMap = SData.key[immutable.Map[URI, mutable.Map[URI, Array[Byte]] with mutable.SynchronizedMap[URI, Array[Byte]]]]("digest")
  }
}