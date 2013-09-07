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

package org.digimead.tabuddy.model.graph

import scala.collection.immutable
import scala.ref.WeakReference

import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.Model

/**
 * Node state is a mutable part of a node.
 */
trait NodeState {
  /** Children nodes. */
  val children: immutable.ListSet[Node]
  /** Graph to which the node belongs. */
  // Option type is meaningless here. Node is always belong graph.
  val graph: Graph[_ <: Model.Like]
  /** Global modification time, based on node elements and children state. */
  val modified: Element.Timestamp
  /** Parent node. */
  // If we have the only strong reference to this node and
  // graph may contains a model node with weak references
  // then all other nodes will be GC'ed and we may work only with our part of tree.
  val parentNodeReference: WeakReference[Node]
  /** Elements with non-root coordinates(List of axes(tags)). */
  val projectionElementBoxes: immutable.HashMap[Coordinate, ElementBox[_ <: Element]]
  /** Element with root coordinate. */
  // Option type is meaningless here. Root element must be defined every time.
  val rootElementBox: ElementBox[_ <: Element]

  /** Copy constructor. */
  def copy(
    children: immutable.ListSet[Node] = this.children,
    graph: Graph[_ <: Model.Like] = this.graph,
    modified: Element.Timestamp = this.modified,
    parentNodeReference: WeakReference[Node] = this.parentNodeReference,
    projectionElementBoxes: immutable.HashMap[Coordinate, ElementBox[_ <: Element]] = this.projectionElementBoxes,
    rootElementBox: ElementBox[_ <: Element] = this.rootElementBox): this.type
}

