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
 * Simple layout is
 *   storageURI/digest/${graph.modification}/type - process description
 *   storageURI/digest/${graph.modification}/digest - block with digest information that is related to graph.modification
 */
/**
 * Provides digest mechanism for serialization process.
 */
class Simple extends Mechanism with Loggable {
  /** Identifier of the digest. */
  val identifier = Simple.Identifier
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
          val digestStream = transport.openWrite(Serialization.inner.encode(digestTypeURI, sData), sData, true)
          val pos = new PrintStream(new BufferedOutputStream(digestStream))
          try {
            pos.println(Simple.Identifier.name)
            pos.println(algorithmName)
            pos.flush()
          } finally try pos.close() catch { case e: Throwable ⇒ }
        }
        // Write payload.
        val digestSumURI = Digest.digestURI(storageURI, transport, graph.modified, Digest.containerName)
        if (!transport.exists(Serialization.inner.encode(digestSumURI, sData), sData) ||
          sData.get(SData.Key.force) == Some(true)) {
          val digestStream = transport.openWrite(Serialization.inner.encode(digestSumURI, sData), sData, true)
          val pos = new PrintStream(new BufferedOutputStream(digestStream))
          try {
            sData(Simple.Key.digestMap)(storageURI).foreach {
              case (uri, digest) ⇒
                pos.println(Digest.byteArrayToHexString(digest) + " " + uri)
            }
            pos.flush()
          } finally try pos.close() catch { case e: Throwable ⇒ }
        }
      case unexpected ⇒
        throw new IllegalArgumentException("Unexpected parameters " + unexpected)
    }
  /** Just invoked before read completion. */
  def afterRead(parameters: Mechanism.Parameters, context: AtomicReference[SoftReference[AnyRef]],
    modified: Element.Timestamp, uri: URI, data: Array[Byte], transport: Transport, sData: SData) =
    Option(digestCache.get()).foreach { dInstance ⇒
      val storageURI = sData(SData.Key.storageURI)
      val resourceURI = storageURI.relativize(uri)
      val map = getDigestMap(parameters, context, modified, transport, sData)
      map.get(resourceURI) match {
        case Some(originalValue) ⇒
          val actualValue = dInstance.digest()
          if (java.util.Arrays.equals(originalValue, actualValue))
            approve(uri, sData)
          else
            refuse(uri, sData)
        case None ⇒
          throw new IllegalStateException("Unable to find digest for " + uri)
      }
      dInstance.reset()
      digestCache.set(null)
    }
  /** Just invoked before write completion. */
  def afterWrite(parameters: Mechanism.Parameters, uri: URI, data: Array[Byte], transport: Transport, sData: SData) =
    parameters match {
      case SimpleDigestParameters(algorithmName) ⇒ Option(digestCache.get()).foreach { dInstance ⇒
        val storageURI = sData(SData.Key.storageURI)
        sData(Simple.Key.digestMap)(storageURI) += storageURI.relativize(uri) -> dInstance.digest()
        dInstance.reset()
        digestCache.set(null)
      }
      case unexpected ⇒
        throw new IllegalArgumentException("Unexpected parameters " + unexpected)
    }
  /** Just invoked after read beginning. */
  def beforeRead(parameters: Mechanism.Parameters, context: AtomicReference[SoftReference[AnyRef]],
    modified: Element.Timestamp, is: InputStream, uri: URI, transport: Transport, sData: SData): InputStream =
    parameters match {
      case SimpleDigestParameters(algorithmName) ⇒
        val dInstance = MessageDigest.getInstance(algorithmName)
        digestCache.set(dInstance)
        new DigestInputStream(is, dInstance)
      case unexpected ⇒
        throw new IllegalArgumentException("Unexpected parameters " + unexpected)
    }
  /** Just invoked after write beginning. */
  def beforeWrite(parameters: Mechanism.Parameters, os: OutputStream, uri: URI, transport: Transport, sData: SData): OutputStream =
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
      sData.updated(Simple.Key.digestMap, newDigestMap)
    case _ ⇒
      sData
  }

  /** Approve resource check sum. */
  protected def approve(resourceURI: URI, sData: SData) =
    log.debug("Approve digest for .../" + sData(SData.Key.storageURI).relativize(resourceURI))
  /** Refuse resource check sum. */
  protected def refuse(resourceURI: URI, sData: SData) =
    throw new IllegalStateException("Incorrect digest for " + resourceURI)
  /** Get loaded or load new digest map. */
  protected def getDigestMap(parameters: Mechanism.Parameters, context: AtomicReference[SoftReference[AnyRef]],
    modified: Element.Timestamp, transport: Transport, sData: SData): immutable.Map[URI, Array[Byte]] = {
    context.get().get match {
      case Some(map: immutable.Map[_, _]) ⇒ return map.asInstanceOf[immutable.Map[URI, Array[Byte]]]
      case _ ⇒ context.get().clear
    }
    val storageURI = sData(SData.Key.storageURI)
    log.debug(s"Load digest data from ${storageURI}")
    val builder = immutable.Map.newBuilder[URI, Array[Byte]]
    val digestSumURI = Digest.digestURI(storageURI, transport, modified, Digest.containerName)
    val digestStream = transport.openRead(Serialization.inner.encode(digestSumURI, sData), sData)
    val reader = new BufferedReader(new InputStreamReader(new BufferedInputStream(digestStream)))
    try {
      var line = reader.readLine()
      while (line != null) {
        val hash = line.takeWhile(_ != ' ')
        val uri = new URI(line.drop(hash.length() + 1))
        builder += uri -> Digest.hexStringToByteArray(hash)
        line = reader.readLine()
      }
    } finally try reader.close() catch { case e: Throwable ⇒ }
    val map = builder.result
    context.set(new SoftReference(map))
    map
  }

  /**
   * Simple digest parameters.
   */
  case class SimpleDigestParameters(val algorithm: String) extends Mechanism.Parameters {
    val mechanism = Simple.this
  }
}

object Simple {
  /** Get simple mechanism parameters. */
  def apply(algorithm: String): Mechanism.Parameters = Digest.perIdentifier.get(Identifier) match {
    case Some(digest: Simple) ⇒ digest(algorithm)
    case _ ⇒ throw new IllegalStateException("Simple digest is not available.")
  }

  /**
   * Simple mechanism identifier.
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
