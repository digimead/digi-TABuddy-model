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

package org.digimead.tabuddy.model.graph

import scala.collection.immutable
import scala.ref.WeakReference

import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.Model

/**
 * Node state is a mutable part of a node.
 */
trait NodeState[A <: Element] {
  /** NodeState type. */
  type NodeStateType <: NodeState[A]

  /** Flag indicating whether the node is attached to the graph. */
  val attached: Boolean
  /** Children nodes. */
  val children: Seq[Node[_ <: Element]]
  /** Graph to which the node belongs. */
  // Option type is meaningless here. Node is always belong graph.
  val graph: Graph[_ <: Model.Like]
  /** Parent node. */
  // If we have the only strong reference to this node and
  // graph may contains a model node with weak references
  // then all other nodes will be GC'ed and we may work only with our part of tree.
  val parentNodeReference: WeakReference[Node[_ <: Element]]
  /** Elements with non-root coordinates(List of axes(tags)) and root. */
  val projectionBoxes: immutable.HashMap[Coordinate, ElementBox[A]]
  /** Element with root coordinate. */
  // Option type is meaningless here. Root element must be defined every time.
  val rootBox: ElementBox[A]

  /** Copy constructor. */
  def copy(
    attached: Boolean = this.attached,
    children: Seq[Node[_ <: Element]] = this.children,
    graph: Graph[_ <: Model.Like] = this.graph,
    parentNodeReference: WeakReference[Node[_ <: Element]] = this.parentNodeReference,
    projectionBoxes: immutable.HashMap[Coordinate, ElementBox[A]] = this.projectionBoxes): NodeStateType
}

