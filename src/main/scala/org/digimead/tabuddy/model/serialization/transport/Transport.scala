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

import java.io.{ FilterInputStream, InputStream }
import java.net.{ URI, URLEncoder }
import java.util.UUID
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.{ ElementBox, Node }
import org.digimead.tabuddy.model.serialization.SData
import scala.collection.immutable
import scala.reflect.runtime.universe.TypeTag

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

  /** Get element location with the specific UUID for the specific container. */
  def acquireElementLocation(ancestorsNSelf: Seq[Node[_ <: Element]], elementBox: ElementBox[_ <: Element], sData: SData, part: String*): URI
  /** Load element box descriptor with the specific UUID for the specific container. */
  def acquireElementBox(ancestors: Seq[Node[_ <: Element]], elementUniqueId: UUID, modified: Element.Timestamp, sData: SData): Array[Byte]
  /** Load graph descriptor with the specific origin from the specific URI. */
  def acquireGraph(origin: Symbol, sData: SData): Array[Byte]
  /** Load model node descriptor with the specific id. */
  def acquireModel(id: Symbol, origin: Symbol, modified: Element.Timestamp, sData: SData): Array[Byte]
  /** Load node descriptor with the specific id for the specific parent. */
  def acquireNode(ancestors: Seq[Node[_ <: Element]], id: Symbol, modified: Element.Timestamp, sData: SData): Array[Byte]
  /** Append path to URI. */
  def append(uri: URI, part: String*): URI = if (uri.toString().endsWith("/"))
    new URI(uri + part.map(URLEncoder.encode(_, "UTF-8").replace("+", "%20")).mkString("/"))
  else
    new URI(uri + "/" + part.map(URLEncoder.encode(_, "UTF-8").replace("+", "%20")).mkString("/"))
  /** Delete resource. */
  def delete(uri: URI, sData: SData)
  /** Check resource. */
  def exists(uri: URI, sData: SData): Boolean
  /** Save element box to the specific URI. */
  def freezeElementBox(ancestorsNSelf: Seq[Node[_ <: Element]], elementBox: ElementBox[_ <: Element], elementBoxDescriptorContent: Array[Byte], sData: SData)
  /** Save graph to the specific URI. */
  def freezeGraph(node: Node[_ <: Model.Like], graphDescriptorContent: Array[Byte], sData: SData)
  /** Save node to the specific URI. */
  def freezeNode(ancestorsNSelf: Seq[Node[_ <: Element]], nodeDescriptorContent: Array[Byte], sData: SData)
  /** Open stream. */
  def open(uri: URI, sData: SData): InputStream
  /** Read resource. */
  def read(uri: URI, sData: SData): Array[Byte]
  /** Write resource. */
  def write(uri: URI, content: InputStream, sData: SData)
  /** Write resource. */
  def write(uri: URI, content: Array[Byte], sData: SData)
}
