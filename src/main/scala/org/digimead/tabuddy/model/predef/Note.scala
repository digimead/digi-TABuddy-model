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

package org.digimead.tabuddy.model.predef

import java.io.ObjectInputStream
import java.util.UUID
import org.digimead.digi.lib.log.api.XLoggable
import org.digimead.tabuddy.model.Record
import org.digimead.tabuddy.model.element.{ Coordinate, Element, LocationGeneric }
import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.graph.ElementBox

/**
 * Note element.
 */
class Note(val eStash: Note.Stash)(@transient val eBox: ElementBox[Note])
  extends Note.Like with XLoggable {
  type ElementType = Note
  type RelativeType = Note.Relative[ElementType]
  type StashType = Note.Stash

  /** Get relative representation. */
  override def eRelative: RelativeType = new Note.Relative(this)

  /** Built in serialization helper. */
  private def readObject(in: ObjectInputStream) {
    in.defaultReadObject()
    readObjectHelper()
  }
}

object Note extends XLoggable {
  val scope = new Scope()

  /** Part of DSL.Builder for end user. */
  trait DSL {
    case class NoteLocation(val id: Symbol, val coordinate: Coordinate = Coordinate.root,
      val scope: Scope = Note.scope, val unique: Option[UUID] = None)(implicit val elementType: Manifest[Note],
        val stashClass: Class[_ <: Note#StashType]) extends LocationGeneric {
      type ElementType = Note
    }
  }
  object DSL {
    trait RichGeneric {
      this: org.digimead.tabuddy.model.dsl.DSL.RichGeneric ⇒
      /** Create a new note or retrieve exists one. */
      def note(id: Symbol, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*): Note =
        withNote(id, rawCoordinate: _*)(x ⇒ x.absolute)
      /**
       * Create a new note or retrieve exists one and apply fTransform to
       *
       * @return note
       */
      def takeNote[A](id: Symbol, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(fTransform: Relative[Note] ⇒ A): Note =
        withNote(id, rawCoordinate: _*)((x) ⇒ { fTransform(x); x.absolute })
      /**
       * Create a new note or retrieve exists one and apply fTransform to.
       *
       * @return fTransform result
       */
      def withNote[A](id: Symbol, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(fTransform: Relative[Note] ⇒ A): A = {
        val coordinate = Coordinate(rawCoordinate: _*)
        withElement[Note, A](id, coordinate, Note.scope, classOf[Note.Stash], (note) ⇒ fTransform(new Relative(note)))
      }
      /** Safe cast element to Note.Like. */
      def asNote = element.eAs[Note.Like]
    }
    trait RichSpecific[A <: Note.Like] {
      this: org.digimead.tabuddy.model.dsl.DSL.RichSpecific[A] ⇒
    }
  }
  /** Base trait for all records. */
  trait Like extends Record.Like {
    this: XLoggable ⇒
    type ElementType <: Like
    type RelativeType <: Relative[ElementType]

    override def canEqual(that: Any): Boolean = that.isInstanceOf[Note.Like]
  }
  /** Relative representation of Note.Like. */
  class Relative[A <: Like](e: A) extends Record.Relative[A](e)
  /** The marker object that describes note scope. */
  class Scope(override val modificator: Symbol = 'Note) extends Record.Scope(modificator) {
    override def canEqual(other: Any): Boolean = other.isInstanceOf[org.digimead.tabuddy.model.predef.Note.Scope]
  }
  /** Note stash. */
  class Stash(val created: Element.Timestamp,
    val modified: Element.Timestamp,
    val property: org.digimead.tabuddy.model.element.Stash.Data,
    val scope: Scope)
    extends Stash.Like {
    /** Stash type. */
    type StashType = Note.Stash
    /** Scope type. */
    type ScopeType = Note.Scope
  }
  object Stash {
    trait Like extends Record.Stash.Like {
      /** Stash type. */
      type Stash <: Note.Like
      /** Scope type. */
      type ScopeType <: Note.Scope
    }
  }
}
