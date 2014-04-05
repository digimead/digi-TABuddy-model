/**
 * TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2012-2014 Alexey Aksenov ezh@ezh.msk.ru
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

package org.digimead.tabuddy.model.serialization.transport

import java.io.{ DataInputStream, DataOutputStream, InputStream, OutputStream }
import java.net.{ URI, URLEncoder }
import java.util.UUID
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.serialization.{ SData, Serialization }

/**
 * Serialization transport that provides implementation for the specific URI scheme.
 */
trait Transport {
  this: Loggable ⇒
  /** Box prefix. */
  val boxPrefix = "box"
  /** Graph directory name. */
  val dataDirectoryName = "data"
  /** Descriptor resource name. */
  val descriptorResourceName = "descriptor"
  /** Element resource name. */
  val elementResourceName = "element"
  /** Node prefix. */
  val nodePrefix = "node"
  /** Name of optional part of element. */
  val optionalResourceName = "optional"
  /** Transport scheme. */
  val scheme: String
  /** Timestamp extension. */
  val timestampExtension = "timestamp"

  /** Append path to URI. */
  def append(uri: URI, part: String*): URI = if (uri.toString().endsWith("/"))
    new URI(uri + part.map(URLEncoder.encode(_, "UTF-8").replace("+", "%20")).mkString("/"))
  else
    new URI(uri + "/" + part.map(URLEncoder.encode(_, "UTF-8").replace("+", "%20")).mkString("/"))
  /** Delete resource. */
  def delete(uri: URI, sData: SData)
  /** Check resource. */
  def exists(uri: URI, sData: SData): Boolean
  /** Get element box URI. */
  def getElementBoxURI(ancestors: Seq[Node[_ <: Element]], elementUniqueId: UUID, elementModified: Element.Timestamp, sData: SData): URI
  /** Get graph URI. */
  def getGraphURI(sData: SData): URI
  /** Get node URI. */
  def getNodeURI(ancestors: Seq[Node[_ <: Element]], nodeId: Symbol, nodeModified: Element.Timestamp, sData: SData): URI
  /** Get sub element URI. */
  def getSubElementURI(ancestors: Seq[Node[_ <: Element]], elementUniqueId: UUID, elementModified: Element.Timestamp, sData: SData, part: String*): URI
  /** Open input stream. */
  def openRead(uri: URI, sData: SData): InputStream
  /** Open output stream. */
  def openWrite(uri: URI, sData: SData, create: Boolean): OutputStream
  /** Read resource. */
  def read(uri: URI, sData: SData): Array[Byte]
  /** Read resource timestamp. */
  def readTimestamp(encodedResourceURI: URI, sData: SData): Element.Timestamp = {
    val decodedResourceURI = Serialization.inner.decode(encodedResourceURI, sData)
    val timestampURI = new URI(decodedResourceURI.getScheme(), decodedResourceURI.getUserInfo(),
      decodedResourceURI.getHost(), decodedResourceURI.getPort(),
      decodedResourceURI.getPath() + "." + timestampExtension, decodedResourceURI.getQuery(),
      decodedResourceURI.getFragment())
    val timestampStream = openRead(Serialization.inner.encode(Serialization.inner.decode(timestampURI, sData), sData), sData)
    val is = new DataInputStream(timestampStream)
    try Element.timestamp(is.readLong(), is.readLong())
    finally try is.close() catch { case e: Throwable ⇒ }
  }
  /** Write resource. */
  def write(uri: URI, content: Array[Byte], sData: SData)
  /** Write resource timestamp. */
  def writeTimestamp(decodedResourceURI: URI, sData: SData) {
    val timestampURI = new URI(decodedResourceURI.getScheme(), decodedResourceURI.getUserInfo(),
      decodedResourceURI.getHost(), decodedResourceURI.getPort(),
      decodedResourceURI.getPath() + "." + timestampExtension, decodedResourceURI.getQuery(),
      decodedResourceURI.getFragment())
    val timestampStream = openWrite(Serialization.inner.encode(timestampURI, sData), sData, true)
    val os = new DataOutputStream(timestampStream)
    try {
      val modified = sData(SData.Key.modified)
      os.writeLong(modified.milliseconds)
      os.writeLong(modified.nanoShift)
      os.flush()
    } finally try os.close() catch { case e: Throwable ⇒ }
  }

}
