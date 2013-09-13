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

import java.net.URI
import java.util.UUID

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.serialization.Serialization

/**
 * Serialization transport that provides implementation for the specific URI scheme
 */
trait Transport {
  this: Loggable ⇒
  /** Element resource name. */
  val elementResourceName = "element"
  /** Descriptor resource name. */
  val descriptorResourceName = "descriptor.yaml"
  /** Model directory name. */
  val modelDirectoryName = "model"

  /** Load element with the specific UUID for the specific container. */
  def acquireElement[A <: Element](elementBox: ElementBox[A], storageURI: URI)(implicit m: Manifest[A]): A
  /** Load element box descriptor with the specific UUID for the specific container. */
  def acquireElementBox(elementUniqueId: UUID, parentNode: Node.ThreadUnsafe, storageURI: URI): Serialization.Descriptor.Element[_ <: Element]
  /** Load graph descriptor with the specific origin from the specific URI. */
  def acquireGraph(origin: Symbol, storageURI: URI): Serialization.Descriptor.Graph[_ <: Model.Like]
  /** Load model node descriptor with the specific id. */
  def acquireModel(id: Symbol, origin: Symbol, storageURI: URI): Serialization.Descriptor.Node
  /** Load node descriptor with the specific id for the specific parent. */
  def acquireNode(id: Symbol, parentNode: Node.ThreadUnsafe, storageURI: URI): Serialization.Descriptor.Node
  /** Delete resource. */
  def delete(uri: URI)
  /** Save element to the specific URI. */
  def freezeElementBox(elementBox: ElementBox[_ <: Element], storageURI: URI)
  /** Save graph to the specific URI. */
  def freezeGraph(graph: Graph[_ <: Model.Like], storageURI: URI)
  /** Save node to the specific URI. */
  def freezeNode(node: Node.ThreadUnsafe, storageURI: URI, recursive: Boolean = true)
  /** Read resource. */
  def read(uri: URI): Array[Byte]
  /** Squeeze model. */
  def squeeze()
  /** Write resource. */
  def write(content: Array[Byte], uri: URI)
}
