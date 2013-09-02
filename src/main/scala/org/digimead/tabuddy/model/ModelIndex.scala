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

package org.digimead.tabuddy.model

import java.util.UUID
import scala.annotation.tailrec
import scala.collection.mutable
import scala.ref.WeakReference
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Reference
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.graph.Context

trait ModelIndex {
  this: Model with Loggable =>
  //type HashMapPerOrigin = mutable.HashMap[Symbol, WeakReference[Element]] with mutable.SynchronizedMap[Symbol, WeakReference[Element]]
  //type HashMapPerAxis = mutable.HashMap[Coordinate, HashMapPerOrigin] with mutable.SynchronizedMap[Coordinate, HashMapPerOrigin]
  //type HashMapPerId = mutable.HashMap[UUID, HashMapPerAxis] with mutable.SynchronizedMap[UUID, HashMapPerAxis]
  /**
   * Nested HashMap of all elements
   * Id(Symbol) -> Axis(Coordinate) -> Origin(Symbol) = Generic
   */
  //@transient protected lazy val index: HashMapPerId = new mutable.HashMap[UUID, HashMapPerAxis] with mutable.SynchronizedMap[UUID, HashMapPerAxis]

  /** Get element for unique id at the specific coordinate. */
  def e(unique: UUID, coordinate: Axis[_ <: AnyRef with java.io.Serializable]*): Option[Element] =
    e(Reference(eStash.origin, unique, Coordinate(coordinate: _*)))
  /** Get element for unique id at the specific coordinate. */
  def e(unique: UUID, coordinate: Coordinate): Option[Element] =
    e(Reference(eStash.origin, unique, coordinate))
  /** Get element for the reference from the current graph. */
  def e(reference: Reference): Option[Element] = eBox.node.getGraph.flatMap { graph =>
    if (reference.origin != graph.origin)
      throw new IllegalArgumentException(s"""Unable to process reference from graph "${reference.origin}" within "${graph.origin}" one.""")
    e(reference.unique).flatMap { _.getProjection(reference.coordinate).map(_.get) }
  }
  /** Get node for the context from the current graph. */
  def e(context: Context): Option[Node] = eBox.node.getGraph.flatMap { graph =>
    if (context.origin != graph.origin)
      throw new IllegalArgumentException(s"""Unable to process context from graph "${context.origin}" within "${graph.origin}" one.""")
    e(context.unique)
  }
  /** Get node for the unique id from the current graph. */
  def e(unique: UUID): Option[Node] = eBox.node.getGraph.flatMap(_.nodes.get(unique))
}
