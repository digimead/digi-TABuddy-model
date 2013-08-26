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

package org.digimead.tabuddy.model.predef

import java.util.UUID

import org.digimead.digi.lib.aop.log
import org.digimead.tabuddy.model.Record
import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.element.Context
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.LocationGeneric

class Task[A <: Task.Stash](stashArg: A) extends Note(stashArg) {
  /** create new instance with specific stash */
  override protected def eNewInstance(stash: A): this.type = new Task(stash).asInstanceOf[this.type]
}

/**
 * Task companion object that contains appropriate Stash
 */
object Task {
  type Generic = Task[_ <: Stash]
  val scope = new Scope()

  /**
   * Create a detached element with the standard Task class
   */
  def apply[T](id: Symbol, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]]): Task[Stash] =
    Task.apply(id, rawCoordinate, (f: Task[Stash]) => {})
  /**
   * Create a detached element with the standard Task class
   */
  def apply[T](id: Symbol, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]], f: Task[Stash] => T): Task[Stash] =
    Record.apply(classOf[Task[Stash]], classOf[Task.Stash], None, id, Task.scope, rawCoordinate, f)
  /**
   * Create a detached element with the standard Task class
   */
  def apply[T](id: Symbol, scope: Task.Scope, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]]): Task[Stash] =
    Record.apply(classOf[Task[Stash]], classOf[Task.Stash], None, id, scope, rawCoordinate, (f: Task[Stash]) => {})
  /**
   * Create a detached element with the standard Task class
   */
  def apply[T](id: Symbol, scope: Task.Scope, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]], f: Task[Stash] => T): Task[Stash] =
    Record.apply(classOf[Task[Stash]], classOf[Task.Stash], None, id, scope, rawCoordinate, f)
  /**
   * Get exists or create an attached element with the standard Task class
   */
  def apply[T](container: Element.Generic, id: Symbol, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]]): Task[Stash] =
    Task.apply(container, id, rawCoordinate, (f: Task[Stash]) => {})
  /**
   * Get exists or create an attached element with the standard Task class
   */
  def apply[T](container: Element.Generic, id: Symbol, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]], f: Task[Stash] => T): Task[Stash] =
    Record.apply(classOf[Task[Stash]], classOf[Task.Stash], Some(container), id, Task.scope, rawCoordinate, f)
  /**
   * Get exists or create an attached element with the standard Task class
   */
  def apply[T](container: Element.Generic, id: Symbol, scope: Task.Scope, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]]): Task[Stash] =
    Record.apply(classOf[Task[Stash]], classOf[Task.Stash], Some(container), id, scope, rawCoordinate, (f: Task[Stash]) => {})
  /**
   * Get exists or create an attached element with the standard Task class
   */
  def apply[T](container: Element.Generic, id: Symbol, scope: Task.Scope, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]], f: (Task[Stash]) => T): Task[Stash] =
    Record.apply(classOf[Task[Stash]], classOf[Task.Stash], Some(container), id, scope, rawCoordinate, f)

  /**
   * Part of DSL.Builder for end user
   */
  trait DSL {
    this: org.digimead.tabuddy.model.dsl.DSL[_] =>
    case class TaskLocation(override val id: Symbol,
      override val coordinate: Coordinate = Coordinate.root)
      extends LocationGeneric[Task[Stash], Stash](id, Task.scope, coordinate)
  }
  object DSL {
    trait RichElement {
      this: org.digimead.tabuddy.model.dsl.DSL.RichElement =>
      /**
       * create new or retrieve exists task
       */
      def task[T](id: Symbol, coordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(f: Task[Stash] => T): Task[Stash] =
        apply(DLS_element, id, coordinate, f)
      def toTask() = DLS_element.eAs[Task[Stash], Stash]
    }
  }
  /** The marker object that describes task scope */
  class Scope(override val modificator: Symbol = 'Task) extends Note.Scope(modificator) {
    override def canEqual(other: Any): Boolean = other.isInstanceOf[org.digimead.tabuddy.model.predef.Task.Scope]
  }
  /**
   * Record specific stash realization
   */
  class Stash(override val context: Context, override val coordinate: Coordinate, override val created: Element.Timestamp,
    override val id: Symbol, override val scope: Element.Scope, override val unique: UUID, override val property: org.digimead.tabuddy.model.element.Stash.Data = new org.digimead.tabuddy.model.element.Stash.Data)
    extends Note.Stash(context, coordinate, created, id, scope, unique, property) {
  }
}
