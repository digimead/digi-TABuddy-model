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
class Note(stashArg: Note.Stash)(@transient implicit val eBox: ElementBox[Note])
  extends Note.Like with Loggable {
  type ElementType = Note
  type StashType = Note.Stash

  /** Get current stash. */
  def eStash = stashArg
}

object Note extends Loggable {
  val scope = new Scope()

  /** Part of DSL.Builder for end user. */
  trait DSL {
    this: org.digimead.tabuddy.model.dsl.DSL[_] =>
    case class NoteLocation(override val id: Symbol,
      override val coordinate: Coordinate = Coordinate.root)
      extends LocationGeneric[Note](id, Note.scope, coordinate)
  }
  object DSL {
    trait RichElement {
      this: org.digimead.tabuddy.model.dsl.DSL.RichElement =>
      /** Create new note or retrieve exists one and apply fTransform to. */
      def note[A](id: Symbol, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(fTransform: Note => A): Note = {
        val coordinate = Coordinate(rawCoordinate: _*)
        // Modify parent node.
        element.eBox.node.threadSafe { parentNode =>
          parentNode.children.find { _.id == id } match {
            case Some(childNode) =>
              childNode.threadSafe { node =>
                log.debug(s"Get or create note element for exists ${node}.")
                implicit val shashClass = classOf[Note.Stash]
                val note = ElementBox.getOrCreate[Note](coordinate, node, Note.scope, parentNode.rootElementBox.serialization)
                fTransform(note)
                note
              }
            case None =>
              parentNode.createChild(id, UUID.randomUUID()) { node =>
                log.debug(s"Get or create note element for new ${node}.")
                implicit val shashClass = classOf[Note.Stash]
                val note = ElementBox.getOrCreate[Note](coordinate, node, Note.scope, parentNode.rootElementBox.serialization)
                fTransform(note)
                note
              }
          }
        }
      }
      def toNote() = element.eAs[Note]
    }
  }
  /** Base trait for all records. */
  trait Like extends Record.Like {
    this: Loggable =>
    type ElementType <: Like
  }
  /** The marker object that describes note scope. */
  class Scope(override val modificator: Symbol = 'Note) extends Record.Scope(modificator) {
    override def canEqual(other: Any): Boolean = other.isInstanceOf[org.digimead.tabuddy.model.predef.Note.Scope]
  }
  /** Note stash. */
  class Stash(val coordinate: Coordinate,
    val created: Element.Timestamp,
    val id: Symbol,
    val modified: Element.Timestamp,
    val origin: Symbol,
    val property: org.digimead.tabuddy.model.element.Stash.Data,
    val scope: Scope,
    val unique: UUID)
    extends Stash.Like {
    /** Stash type. */
    type StashType = Stash
    /** Scope type. */
    type ScopeType = Note.Scope
  }
  object Stash {
    trait Like extends Record.Stash.Like {
      /** Stash type. */
      type Stash <: Like
      /** Scope type. */
      type ScopeType <: Note.Scope
    }
  }
}
