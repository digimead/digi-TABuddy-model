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

import java.io.ObjectInputStream
import java.util.UUID

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.LocationGeneric
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.ElementBox.box2interface

/**
 * Task element.
 */
class Task(val eStash: Task.Stash)(@transient val eBox: ElementBox[Task]) extends Task.Like with Loggable {
  type ElementType = Task
  type StashType = Task.Stash

  /** Built in serialization helper. */
  private def readObject(in: ObjectInputStream) {
    in.defaultReadObject()
    readObjectHelper()
  }
}

object Task extends Loggable {
  val scope = new Scope()

  /** Part of DSL.Builder for end user. */
  trait DSL {
    case class TaskLocation(val id: Symbol, val unique: Option[UUID] = None,
      val coordinate: Coordinate = Coordinate.root)(implicit val elementType: Manifest[Task],
        val stashClass: Class[_ <: Task#StashType]) extends LocationGeneric {
      val scope = Task.scope
      type ElementType = Task
      type RelativeType = Relative[Task]
      def toRelative(element: Task): RelativeType = new Relative(element)
    }
  }
  object DSL {
    trait RichGeneric {
      this: org.digimead.tabuddy.model.dsl.DSL.RichGeneric ⇒
      /** Create a new task or retrieve exists one. */
      def task(id: Symbol, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*): Task =
        withTask(id, rawCoordinate: _*)(x ⇒ x.absolute)
      /**
       * Create a new task or retrieve exists one and apply fTransform to
       *
       * @return task
       */
      def takeTask[A](id: Symbol, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(fTransform: Relative[Task] ⇒ A): Task =
        withTask(id, rawCoordinate: _*)((x) ⇒ { fTransform(x); x.absolute })
      /**
       * Create a new task or retrieve exists one and apply fTransform to.
       *
       * @return fTransform result
       */
      def withTask[A](id: Symbol, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(fTransform: Relative[Task] ⇒ A): A = {
        val coordinate = Coordinate(rawCoordinate: _*)
        withElement[Task, A](id, coordinate, Task.scope, classOf[Task.Stash], (task) ⇒ fTransform(new Relative(task)))
      }
      /** Safe cast element to Task.Like. */
      def asTask = element.eAs[Task.Like]
    }
    trait RichSpecific[A <: Task.Like] {
      this: org.digimead.tabuddy.model.dsl.DSL.RichSpecific[A] ⇒
    }
  }
  /** Base trait for all tasks. */
  trait Like extends Note.Like {
    this: Loggable ⇒
    type ElementType <: Like

    /** Get relative representation. */
    override def eRelative: Task.Relative[ElementType] = new Task.Relative(this.asInstanceOf[ElementType])

    override def canEqual(that: Any): Boolean = that.isInstanceOf[Task.Like]
  }
  /** Relative representation of Task.Like. */
  class Relative[A <: Task.Like](e: A) extends Note.Relative[A](e)
  /** The marker object that describes task scope. */
  class Scope(override val modificator: Symbol = 'Task) extends Note.Scope(modificator) {
    override def canEqual(other: Any): Boolean = other.isInstanceOf[org.digimead.tabuddy.model.predef.Task.Scope]
  }
  /** Task stash. */
  class Stash(val created: Element.Timestamp,
    val modified: Element.Timestamp,
    val property: org.digimead.tabuddy.model.element.Stash.Data,
    val scope: Scope)
    extends Stash.Like {
    /** Stash type. */
    type StashType = Task.Stash
    /** Scope type. */
    type ScopeType = Task.Scope
  }
  object Stash {
    trait Like extends Note.Stash.Like {
      /** Stash type. */
      type Stash <: Task.Like
      /** Scope type. */
      type ScopeType <: Task.Scope
    }
  }
}
