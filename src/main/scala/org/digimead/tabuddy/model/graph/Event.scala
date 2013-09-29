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
import java.util.UUID

import scala.collection.mutable

import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Value

sealed trait Event extends PropertyChangeEvent with mutable.Undoable {
  /** Undo the last operation function container. */
  val undoF: () ⇒ Unit
  /** Undo the last operation. */
  def undo(): Unit = undoF()
}

/**
 * In the moment of child modification (include/remove/...), all ancestors MUST be valid.
 */
object Event extends mutable.Publisher[Event] {
  import scala.language.existentials

  /**
   * This event is generated by node when new child added.
   */
  case class ChildInclude[T <: Element](sourceArg: Node[_ <: Element], newValueArg: Node[T])(val undoF: () ⇒ Unit)
    extends PropertyChangeEvent(sourceArg, sourceArg.id.name, null, newValueArg) with Event
  /**
   * This event is generated by node when exists child removed.
   */
  case class ChildRemove[T <: Element](sourceArg: Node[_ <: Element], oldValueArg: Node[T])(val undoF: () ⇒ Unit)
    extends PropertyChangeEvent(sourceArg, sourceArg.id.name, oldValueArg, null) with Event
  /**
   * This event is generated by node when exists child replaced.
   */
  case class ChildReplace[T <: Element](sourceArg: Node[_ <: Element], oldValueArg: Node[T], newValueArg: Node[T])(val undoF: () ⇒ Unit)
    extends PropertyChangeEvent(sourceArg, sourceArg.id.name, oldValueArg, newValueArg) with Event
  /**
   * This event is generated by node when buffer cleared.
   */
  case class ChildrenReset(sourceArg: Node[_ <: Element])(val undoF: () ⇒ Unit)
    extends PropertyChangeEvent(sourceArg, sourceArg.id.name, null, null) with Event
  case class ValueInclude[T <: Element](sourceArg: T, newValueArg: Value[_ <: AnyRef with java.io.Serializable])(val undoF: () ⇒ Unit)
    extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, null, newValueArg) with Event
  case class ValueRemove[T <: Element](sourceArg: T, oldValueArg: Value[_ <: AnyRef with java.io.Serializable])(val undoF: () ⇒ Unit)
    extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, oldValueArg, null) with Event
  case class ValueUpdate[T <: Element](sourceArg: T, oldValueArg: Value[_ <: AnyRef with java.io.Serializable],
    newValueArg: Value[_ <: AnyRef with java.io.Serializable])(val undoF: () ⇒ Unit)
    extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, oldValueArg, newValueArg) with Event
  /**
   * This event is generated when element is updated.
   */
  case class ElementUpdate[T <: Element](sourceArg: T,
    oldValueArg: Option[Element], newValueArg: Element)(val undoF: () ⇒ Unit)
    extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, oldValueArg, newValueArg) with Event
}
