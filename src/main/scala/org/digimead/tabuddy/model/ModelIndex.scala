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

package org.digimead.tabuddy.model

import java.util.UUID
import org.digimead.digi.lib.log.api.XLoggable
import org.digimead.tabuddy.model.element.{ Coordinate, Element, Reference }
import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.graph.Node

trait ModelIndex {
  this: Model.Like with XLoggable ⇒

  /** Get element for unique id at the specific coordinate. */
  def e(nodeId: UUID, coordinate: Axis[_ <: AnyRef with java.io.Serializable]*): Option[Element] =
    e(Reference(eOrigin, eNode.graph.node.unique, nodeId, Coordinate(coordinate: _*)))
  /** Get element for unique id at the specific coordinate. */
  def e(nodeId: UUID, coordinate: Coordinate): Option[Element] =
    e(Reference(eOrigin, eNode.graph.node.unique, nodeId, coordinate))
  /** Get element for the reference from the current graph. */
  def e(reference: Reference): Option[Element] = {
    if (reference.origin != eNode.graph.origin)
      throw new IllegalArgumentException(s"""Unable to process reference from graph "${reference.origin}" within "${eNode.graph.origin}" one.""")
    e(reference.node).flatMap { _.projectionBoxes.get(reference.coordinate).map(_.e) }
  }
  /** Get node for the unique id from the current graph. */
  def e(nodeId: UUID): Option[Node[_ <: Element]] = eNode.graph.nodes.get(nodeId)
}
