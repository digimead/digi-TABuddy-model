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
import org.digimead.tabuddy.model.Record
import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.LocationGeneric
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.ElementBox.box2interface

/**
 * Note element.
 */
class Note(val eStash: Note.Stash)(@transient val eBox: ElementBox[Note])
  extends Note.Like with Loggable {
  type ElementType = Note
  type StashType = Note.Stash

  /** Built in serialization helper. */
  private def readObject(in: ObjectInputStream) {
    in.defaultReadObject()
    readObjectHelper()
  }
}

object Note extends Loggable {
  val scope = new Scope()

  /** Part of DSL.Builder for end user. */
  trait DSL {
    case class NoteLocation(val id: Symbol, val unique: Option[UUID] = None,
      val coordinate: Coordinate = Coordinate.root)(implicit val elementType: Manifest[Note],
        val stashClass: Class[_ <: Note#StashType]) extends LocationGeneric[Note] {
      val scope = Note.scope
    }
  }
  object DSL {
    trait RichGeneric {
      this: org.digimead.tabuddy.model.dsl.DSL.RichGeneric ⇒
      /** Create a new note or retrieve exists one. */
      def note(id: Symbol, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*): Note =
        withNote(id, rawCoordinate: _*)(x ⇒ x.immutable)
      /**
       * Create a new note or retrieve exists one and apply fTransform to
       *
       * @return note
       */
      def takeNode[A](id: Symbol, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(fTransform: Mutable[Note] ⇒ A): Note =
        withNote(id, rawCoordinate: _*)((x) ⇒ { fTransform(x); x.immutable })
      /**
       * Create a new note or retrieve exists one and apply fTransform to.
       *
       * @return fTransform result
       */
      def withNote[A](id: Symbol, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(fTransform: Mutable[Note] ⇒ A): A = {
        val coordinate = Coordinate(rawCoordinate: _*)
        // Modify parent node.
        element.eNode.safeWrite { parentNode ⇒
          parentNode.find(_.id == id).map { childNode ⇒
            childNode.safeWrite { node ⇒
              log.debug(s"Get or create note element for exists ${node}.")
              implicit val shashClass = classOf[Note.Stash]
              val note = ElementBox.getOrCreate[Note](coordinate, node, Note.scope, parentNode.rootElementBox.serialization)
              fTransform(new Mutable(note))
            }
          } getOrElse {
            parentNode.createChild(id, UUID.randomUUID()).safeWrite { node ⇒
              log.debug(s"Get or create note element for new ${node}.")
              implicit val shashClass = classOf[Note.Stash]
              val note = ElementBox.getOrCreate[Note](coordinate, node, Note.scope, parentNode.rootElementBox.serialization)
              fTransform(new Mutable(note))
            }
          }
        }
      }
      /** Safe cast element to Note.Like. */
      def toNote() = element.eAs[Note.Like]
    }
    trait RichSpecific[A <: Note.Like] {
      this: org.digimead.tabuddy.model.dsl.DSL.RichSpecific[A] ⇒
    }
  }
  /** Base trait for all records. */
  trait Like extends Record.Like {
    this: Loggable ⇒
    type ElementType <: Like

    /** Get mutable representation. */
    override def eMutable(): Note.Mutable[ElementType] = new Note.Mutable(this.asInstanceOf[ElementType])

    override def canEqual(that: Any): Boolean = that.isInstanceOf[Note.Like]
  }
  /** Mutable representation of Note.Like. */
  class Mutable[A <: Like](e: A) extends Record.Mutable[A](e)
  /** The marker object that describes note scope. */
  class Scope(override val modificator: Symbol = 'Note) extends Record.Scope(modificator) {
    override def canEqual(other: Any): Boolean = other.isInstanceOf[org.digimead.tabuddy.model.predef.Note.Scope]
  }
  /** Note stash. */
  class Stash(val created: Element.Timestamp,
    val modificationTimestamp: Element.Timestamp,
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
