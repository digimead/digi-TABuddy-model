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

package org.digimead.tabuddy.model.serialization

import java.io.{ BufferedInputStream, BufferedOutputStream, BufferedReader, InputStream, InputStreamReader, OutputStream, PrintStream }
import java.net.URI
import java.security.{ DigestInputStream, DigestOutputStream, MessageDigest }
import java.util.Formatter
import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.transport.Transport
import scala.collection.{ immutable, mutable }
import scala.language.implicitConversions

/**
 * Create/check graph digest.
 */
class GraphDigest extends Loggable {
  /** Cache for MessageDigest instance. */
  val digestCache = new ThreadLocal[MessageDigest]()

  /** Initialize SData for acquire process. */
  def initAcquire(sData: SData): SData = sData.get(SData.Key.digestAlgorithm) match {
    case Some(digestAlgorithm) ⇒
      log.debug("Initialize 'acquire' digest calculation behaviour.")
      // Add trailing slash to digestAlgorithm keys
      digestAlgorithm.keys.toSeq.foreach(uri ⇒
        digestAlgorithm.remove(uri).foreach(digestAlgorithm(uri.map(Serialization.inner.addTrailingSlash)) = _))
      initCommon(sData).
        updated(SData.Key.decodeFilter, new DecodeFilter(sData.get(SData.Key.decodeFilter), digestCache)).
        updated(SData.Key.afterRead, new AfterRead(sData.get(SData.Key.afterRead), digestCache)).
        updated(SData.Key.beforeAcquire, new BeforeAcquire(sData.get(SData.Key.beforeAcquire)))
    case None ⇒
      sData // There is no SData.Key.digestAlgorithm. Skip initialization.
  }
  /** Initialize SData for freeze process. */
  def initFreeze(sData: SData): SData = sData.get(SData.Key.digestAlgorithm) match {
    case Some(digestAlgorithm) ⇒
      log.debug("Initialize 'freeze' digest calculation behaviour.")
      // Add trailing slash to digestAlgorithm keys
      // and default digest algorithm
      digestAlgorithm.keys.toSeq.foreach(uri ⇒
        digestAlgorithm.remove(uri).foreach(algorithm ⇒
          digestAlgorithm(uri.map(Serialization.inner.addTrailingSlash)) = algorithm match {
            case algorithm if algorithm == "?" ⇒ GraphDigest.defaultAlgorithm
            case algorithm ⇒ algorithm
          }))
      initCommon(sData).
        updated(SData.Key.encodeFilter, new EncodeFilter(sData.get(SData.Key.encodeFilter), digestCache)).
        updated(SData.Key.afterWrite, new AfterWrite(sData.get(SData.Key.afterWrite), digestCache)).
        updated(SData.Key.afterFreeze, new AfterFreeze(sData.get(SData.Key.afterFreeze)))
    case None ⇒
      sData // There is no SData.Key.digestAlgorithm. Skip initialization.
  }

  /** Initialize SData. */
  protected def initCommon(sData: SData): SData = sData.get(SData.Key.digestAlgorithm) match {
    case Some(digestAlgorithm) ⇒
      if (digestAlgorithm.get(None) == Some("?"))
        digestAlgorithm(None) = GraphDigest.defaultAlgorithm
      sData.get(SData.Key.digestMap) match {
        case Some(digestMap) ⇒
          // Check digestMap and digestAlgorithm keys
          if (sData(SData.Key.digestMap).keys.toSet != digestAlgorithm.keys.flatten.toSet)
            throw new IllegalArgumentException("SData.Key.digestAlgorithm and SData.Key.digestMap have different keys.")
          // Add trailing slash to digestMap keys
          sData.updated(SData.Key.digestMap,
            immutable.Map(digestMap.map {
              case (key, value) ⇒ Serialization.inner.addTrailingSlash(key) -> value
            }.toSeq: _*))
        case None ⇒
          // Create digestMap
          sData.updated(SData.Key.digestMap, immutable.Map(sData(SData.Key.digestAlgorithm).keys.flatten.map(
            uri ⇒ uri -> new mutable.HashMap[URI, String] with mutable.SynchronizedMap[URI, String]).toSeq: _*))
      }
    case None ⇒
      sData
  }

  /**
   * Hook that saves digest map to storage.
   */
  class AfterFreeze(val userAfterFreeze: Option[Function3[Graph[_ <: Model.Like], Transport, SData, _]])
    extends Function3[Graph[_ <: Model.Like], Transport, SData, Unit] {
    /** Save digest map to storage. */
    def apply(graph: Graph[_ <: Model.Like], transport: Transport, sData: SData) {
      val storageURI = sData(SData.Key.storageURI)
      sData(SData.Key.digestMap).get(storageURI) match {
        case Some(map) if map.nonEmpty ⇒ save(map, graph, transport: Transport, sData: SData)
        case _ ⇒ // skip
      }
      userAfterFreeze.foreach(_(graph, transport, sData))
      sData(SData.Key.digestMap).get(storageURI).map(_.clear())
    }

    /** Save digest map to storage. */
    protected def save(digestMap: mutable.Map[URI, String], graph: Graph[_ <: Model.Like], transport: Transport, sData: SData) {
      val storageURI = sData(SData.Key.storageURI)
      val algorithmName = sData(SData.Key.digestAlgorithm) get Some(sData(SData.Key.storageURI)) getOrElse sData(SData.Key.digestAlgorithm)(None)
      log.debug(s"Save digest ${algorithmName} map for ${storageURI}")
      val digestName = GraphDigest.digestName(graph.modified)
      val digestURI = transport.append(storageURI, GraphDigest.digest, digestName)
      if (!transport.exists(Serialization.inner.encode(digestURI, sData), sData) ||
        sData.get(SData.Key.force) == Some(true)) {
        val digestStream = transport.openWrite(Serialization.inner.encode(digestURI, sData), sData, true)
        val pos = new PrintStream(new BufferedOutputStream(digestStream))
        try {
          pos.println(algorithmName)
          digestMap.foreach { case (uri, digest) ⇒ pos.println(digest + " " + uri) }
          pos.flush()
        } finally try pos.close() catch { case e: Throwable ⇒ }
      }
    }
  }
  /**
   * Hook that compare calculated digest with SData.Key.digestMap
   */
  class AfterRead(val userAfterRead: Option[Function3[URI, Array[Byte], SData, _]], val digestCache: ThreadLocal[MessageDigest])
    extends Function3[URI, Array[Byte], SData, Unit] {
    /** Save value calculated by MessageDigest to SData.Key.digestMap. */
    def apply(uri: URI, data: Array[Byte], sData: SData) {
      Option(digestCache.get()).foreach { dInstance ⇒
        val storageURI = sData(SData.Key.storageURI)
        val formatter = new Formatter()
        dInstance.digest().foreach(b ⇒ formatter.format("%02x", b: java.lang.Byte))
        dInstance.reset()
        digestCache.set(null)
        sData(SData.Key.digestMap)(storageURI).get(storageURI.relativize(uri)) match {
          case Some(digest) ⇒
            if (formatter.toString() != digest)
              throw new IllegalStateException("Incorrect digest for " + uri)
          case None ⇒
            throw new IllegalStateException("Unable to find digest for " + uri)
        }
      }
      userAfterRead.foreach(_(uri, data, sData))
    }
  }
  /**
   * Hook that saves calculated digest to SData.Key.digestMap
   */
  class AfterWrite(val userAfterWrite: Option[Function3[URI, Array[Byte], SData, _]], val digestCache: ThreadLocal[MessageDigest])
    extends Function3[URI, Array[Byte], SData, Unit] {
    /** Save value calculated by MessageDigest to SData.Key.digestMap. */
    def apply(uri: URI, data: Array[Byte], sData: SData) {
      Option(digestCache.get()).foreach { dInstance ⇒
        val storageURI = sData(SData.Key.storageURI)
        val formatter = new Formatter()
        dInstance.digest().foreach(b ⇒ formatter.format("%02x", b: java.lang.Byte))
        sData(SData.Key.digestMap)(storageURI) += storageURI.relativize(uri) -> formatter.toString()
        dInstance.reset()
        digestCache.set(null)
      }
      userAfterWrite.foreach(_(uri, data, sData))
    }
  }
  /**
   * Hook that loads digest map to storage.
   */
  class BeforeAcquire(val userBeforeAcquire: Option[Function3[Graph[_ <: Model.Like], Transport, SData, _]])
    extends Function3[Graph[_ <: Model.Like], Transport, SData, Unit] {
    /** Load digest map from storage. */
    def apply(graph: Graph[_ <: Model.Like], transport: Transport, sData: SData) {
      val storageURI = sData(SData.Key.storageURI)
      val digestAlgorithm = sData(SData.Key.digestAlgorithm)
      (digestAlgorithm.get(Some(storageURI)) orElse digestAlgorithm.get(None)) match {
        case Some(algorithmName) ⇒
          digestAlgorithm(Some(storageURI)) = load(sData(SData.Key.digestMap)(storageURI), graph, transport, sData)
        case None ⇒
      }
      userBeforeAcquire.foreach(_(graph, transport, sData))
    }

    /** Load digest map from storage. */
    protected def load(digestMap: mutable.Map[URI, String], graph: Graph[_ <: Model.Like], transport: Transport, sData: SData): String = {
      val storageURI = sData(SData.Key.storageURI)
      val algorithmName = sData(SData.Key.digestAlgorithm) get Some(sData(SData.Key.storageURI)) getOrElse sData(SData.Key.digestAlgorithm)(None)
      log.debug(s"Load digest ${algorithmName} map from ${storageURI}")
      val digestName = GraphDigest.digestName(graph.modified)
      val digestURI = transport.append(storageURI, GraphDigest.digest, digestName)
      val digestStream = transport.openRead(Serialization.inner.encode(digestURI, sData), sData)
      val reader = new BufferedReader(new InputStreamReader(new BufferedInputStream(digestStream)))
      try {
        val algorithm = reader.readLine()
        reader.readLine()
        var line = reader.readLine()
        while (line != null) {
          val hash = line.takeWhile(_ != ' ')
          val uri = new URI(line.drop(hash.length() + 1))
          digestMap(uri) = hash
          line = reader.readLine()
        }
        algorithm
      } finally try reader.close() catch { case e: Throwable ⇒ }
    }
  }
  /**
   * Hook that adds DigestInputStream to stream sequence.
   */
  class DecodeFilter(val userFilter: Option[Function3[InputStream, URI, SData, InputStream]], val digestCache: ThreadLocal[MessageDigest])
    extends Function3[InputStream, URI, SData, InputStream] {
    /** Apply MessageDigest instance to InputStream. */
    def apply(is: InputStream, uri: URI, sData: SData): InputStream = {
      sData.get(SData.Key.digestAlgorithm) match {
        case Some(digestAlgorithm) ⇒
          digestAlgorithm get Some(sData(SData.Key.storageURI)) orElse digestAlgorithm.get(None) match {
            case Some(algorithmName) if algorithmName != "?" ⇒
              val dInstance = MessageDigest.getInstance(algorithmName)
              digestCache.set(dInstance)
              val stream = new DigestInputStream(is, dInstance)
              userFilter.map(_(stream, uri, sData)) getOrElse stream
            case _ ⇒
              userFilter.map(_(is, uri, sData)) getOrElse is
          }
        case None ⇒
          userFilter.map(_(is, uri, sData)) getOrElse is
      }
    }
  }
  /**
   * Hook that adds DigestOutputStream to stream sequence.
   */
  class EncodeFilter(val userFilter: Option[Function3[OutputStream, URI, SData, OutputStream]], val digestCache: ThreadLocal[MessageDigest])
    extends Function3[OutputStream, URI, SData, OutputStream] {
    /** Apply MessageDigest instance to OutputStream. */
    def apply(os: OutputStream, uri: URI, sData: SData): OutputStream = {
      sData.get(SData.Key.digestAlgorithm) match {
        case Some(digestAlgorithm) ⇒
          digestAlgorithm get Some(sData(SData.Key.storageURI)) orElse digestAlgorithm.get(None) match {
            case Some(algorithmName) ⇒
              val dInstance = MessageDigest.getInstance(algorithmName)
              digestCache.set(dInstance)
              val stream = new DigestOutputStream(os, dInstance)
              userFilter.map(_(stream, uri, sData)) getOrElse stream
            case None ⇒
              userFilter.map(_(os, uri, sData)) getOrElse os
          }
        case None ⇒
          userFilter.map(_(os, uri, sData)) getOrElse os
      }
    }
  }
}

object GraphDigest {
  implicit def digest2implementation(gd: GraphDigest.type): GraphDigest = gd.inner

  /** Get default digest algorithm. */
  def defaultAlgorithm = DI.defaultAlgorithm
  /** Get digest container for data files. */
  def digest = DI.digest
  /** Get digest name. */
  def digestName(modified: Element.Timestamp) = DI.digestName.format(yaml.Timestamp.dump(modified))
  /** GraphDigest implementation. */
  def inner = DI.implementation
  /** Get algorithm priority. */
  def priority = DI.priority

  /**
   * Dependency injection routines
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    /** Default digest algorithm. */
    lazy val defaultAlgorithm = injectOptional[String]("GraphDigest.DefaultAlgorithm") getOrElse "SHA-512"
    /** Digest container for data files. */
    lazy val digest = injectOptional[String]("GraphDigest.Container") getOrElse "digest"
    /** Digest name template. */
    lazy val digestName = injectOptional[String]("GraphDigest.Name") getOrElse "%s.digest"
    /** GraphDigest implementation. */
    lazy val implementation = injectOptional[GraphDigest] getOrElse new GraphDigest
    /** Algorithm priority. */
    // ? - Any unknown.
    lazy val priority = injectOptional[Seq[String]]("GraphDigest.Priority") getOrElse Seq("SHA-512", "SHA-384", "SHA-224", "SHA-256", "?", "MD5", "SHA-1", "SHA1", "SHA", "MD2")
  }
}
