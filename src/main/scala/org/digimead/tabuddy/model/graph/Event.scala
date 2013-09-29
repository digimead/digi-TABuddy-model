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

import java.beans.PropertyChangeEvent

import scala.collection.mutable

import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Value

sealed trait Event extends PropertyChangeEvent with mutable.Undoable {
  /** Undo the last operation function container. */
  val undoF: () ⇒ Unit
  /** Undo the last operation. */
  def undo(): Unit = undoF()
}

/**
 * In the moment of the node modification (include/remove/...), all ancestors MUST be valid.
 * Old values may contains NULL for the compatibility with the plain PropertyChangeEvent.
 */
object Event extends mutable.Publisher[Event] {
  import scala.language.existentials

  /** This event is generated when element is changed. */
  case class ElementBoxChange[T <: Element](sourceArg: ElementBox[T], oldValueArg: Element, newValueArg: Element)(val undoF: () ⇒ Unit)
    extends PropertyChangeEvent(sourceArg, sourceArg.node.id.name, oldValueArg, newValueArg) with Event
  /** This event is generated when element box is changed. */
  case class NodeChange[T <: Element](sourceArg: Node[T], oldValueArg: ElementBox[T], newValueArg: ElementBox[T])(val undoF: () ⇒ Unit)
    extends PropertyChangeEvent(sourceArg, sourceArg.id.name, oldValueArg, newValueArg) with Event
  /** This event is generated when new node is added or exists node is removed from graph index. */
  case class GraphChange(sourceArg: Node[_ <: Element], oldValueArg: Node[_ <: Element], newValueArg: Node[_ <: Element])(val undoF: () ⇒ Unit)
    extends PropertyChangeEvent(sourceArg, sourceArg.id.name, oldValueArg, newValueArg) with Event
  /** This event is generated when graph index is cleared. */
  case class GraphReset(sourceArg: Graph[_ <: Model.Like])(val undoF: () ⇒ Unit)
    extends PropertyChangeEvent(sourceArg, sourceArg.origin.name, null, null) with Event
  /** This event is generated when new value is added via element eSet */
  case class ValueInclude[T <: Element](sourceArg: T, newValueArg: Value[_ <: AnyRef with java.io.Serializable])(val undoF: () ⇒ Unit)
    extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, null, newValueArg) with Event
  /** This event is generated when exists value is removed via element eSet/eRemove */
  case class ValueRemove[T <: Element](sourceArg: T, oldValueArg: Value[_ <: AnyRef with java.io.Serializable])(val undoF: () ⇒ Unit)
    extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, oldValueArg, null) with Event
  /** This event is generated when value is replaced via element eSet */
  case class ValueUpdate[T <: Element](sourceArg: T, oldValueArg: Value[_ <: AnyRef with java.io.Serializable],
    newValueArg: Value[_ <: AnyRef with java.io.Serializable])(val undoF: () ⇒ Unit)
    extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, oldValueArg, newValueArg) with Event
}
