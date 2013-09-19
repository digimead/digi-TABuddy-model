/**
 * TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2012-2013 Alexey Aksenov ezh@ezh.msk.ru
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

import java.io.InputStream
import java.net.URI
import java.util.UUID

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Value
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.Node

/**
 * Serialization transport that provides implementation for the specific URI scheme
 */
trait Transport {
  this: Loggable ⇒
  /** Element resource name. */
  val elementResourceName = "element"
  /** Descriptor resource name. */
  val descriptorResourceName = "descriptor"
  /** Model directory name. */
  val modelDirectoryName = "model"
  /** Element resource name. */
  val optionalResourceName = "optional"

  /** Get element location with the specific UUID for the specific container. */
  def acquireElementLocation(ancestorsNSelf: Seq[Node[_ <: Element]], elementBox: ElementBox[_ <: Element], storageURI: URI, part: String*): URI
  /** Load element box descriptor with the specific UUID for the specific container. */
  def acquireElementBox(ancestors: Seq[Node[_ <: Element]], elementUniqueId: UUID, modified: Element.Timestamp, storageURI: URI): Array[Byte]
  /** Load graph descriptor with the specific origin from the specific URI. */
  def acquireGraph(origin: Symbol, storageURI: URI): Array[Byte]
  /** Load model node descriptor with the specific id. */
  def acquireModel(id: Symbol, origin: Symbol, modified: Element.Timestamp, storageURI: URI): Array[Byte]
  /** Load node descriptor with the specific id for the specific parent. */
  def acquireNode(ancestors: Seq[Node[_ <: Element]], id: Symbol, modified: Element.Timestamp, storageURI: URI): Array[Byte]
  /** Append path to URI. */
  def append(uri: URI, part: String*): URI = if (uri.toString().endsWith("/"))
    new URI(uri + part.mkString("/"))
  else
    new URI(uri + "/" + part.mkString("/"))
  /** Delete resource. */
  def delete(uri: URI)
  /** Save element box to the specific URI. */
  def freezeElementBox(ancestorsNSelf: Seq[Node[_ <: Element]], elementBox: ElementBox[_ <: Element], storageURI: URI, elementBoxDescriptorContent: Array[Byte])
  /** Save graph to the specific URI. */
  def freezeGraph(node: Node[_ <: Model.Like], storageURI: URI, graphDescriptorContent: Array[Byte])
  /** Save node to the specific URI. */
  def freezeNode(ancestorsNSelf: Seq[Node[_ <: Element]], storageURI: URI, nodeDescriptorContent: Array[Byte])
  /** Save custom value to the specific URI. */
  def freezeValue(value: Value[_ <: AnyRef with java.io.Serializable], element: Element, storageURI: URI, elementContent: Array[Byte])
  /** Open stream. */
  def open(uri: URI): InputStream
  /** Read resource. */
  def read(uri: URI): Array[Byte]
  /** Squeeze model. */
  def squeeze()
  /** Write resource. */
  def write(content: InputStream, uri: URI)
  /** Write resource. */
  def write(content: Array[Byte], uri: URI)
}
