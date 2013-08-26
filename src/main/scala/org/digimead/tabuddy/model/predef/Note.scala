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

class Note[A <: Note.Stash](stashArg: A) extends Record(stashArg) {
  /** create new instance with specific stash */
  override protected def eNewInstance(stash: A): this.type = new Note(stash).asInstanceOf[this.type]
}

/**
 * Note companion object that contains appropriate Stash
 */
object Note {
  type Generic = Note[_ <: Stash]
  val scope = new Scope()

  /**
   * Create a detached element with the standard Note class
   */
  def apply[T](id: Symbol, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]]): Note[Stash] =
    Note.apply(id, rawCoordinate, (f: Note[Stash]) => {})
  /**
   * Create a detached element with the standard Note class
   */
  def apply[T](id: Symbol, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]], f: Note[Stash] => T): Note[Stash] =
    Record.apply(classOf[Note[Stash]], classOf[Note.Stash], None, id, Note.scope, rawCoordinate, f)
  /**
   * Create a detached element with the standard Note class
   */
  def apply[T](id: Symbol, scope: Note.Scope, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]]): Note[Stash] =
    Record.apply(classOf[Note[Stash]], classOf[Note.Stash], None, id, scope, rawCoordinate, (f: Note[Stash]) => {})
  /**
   * Create a detached element with the standard Note class
   */
  def apply[T](id: Symbol, scope: Note.Scope, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]], f: Note[Stash] => T): Note[Stash] =
    Record.apply(classOf[Note[Stash]], classOf[Note.Stash], None, id, scope, rawCoordinate, f)
  /**
   * Get exists or create an attached element with the standard Note class
   */
  def apply[T](container: Element.Generic, id: Symbol, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]]): Note[Stash] =
    Note.apply(container, id, rawCoordinate, (f: Note[Stash]) => {})
  /**
   * Get exists or create an attached element with the standard Note class
   */
  def apply[T](container: Element.Generic, id: Symbol, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]], f: Note[Stash] => T): Note[Stash] =
    Record.apply(classOf[Note[Stash]], classOf[Note.Stash], Some(container), id, Note.scope, rawCoordinate, f)
  /**
   * Get exists or create an attached element with the standard Note class
   */
  def apply[T](container: Element.Generic, id: Symbol, scope: Note.Scope, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]]): Note[Stash] =
    Record.apply(classOf[Note[Stash]], classOf[Note.Stash], Some(container), id, scope, rawCoordinate, (f: Note[Stash]) => {})
  /**
   * Get exists or create an attached element with the standard Note class
   */
  def apply[T](container: Element.Generic, id: Symbol, scope: Note.Scope, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]], f: Note[Stash] => T): Note[Stash] =
    Record.apply(classOf[Note[Stash]], classOf[Note.Stash], Some(container), id, scope, rawCoordinate, f)

  /**
   * Part of DSL.Builder for end user
   */
  trait DSL {
    this: org.digimead.tabuddy.model.dsl.DSL[_] =>
    case class NoteLocation(override val id: Symbol,
      override val coordinate: Coordinate = Coordinate.root)
      extends LocationGeneric[Note[Stash], Stash](id, Note.scope, coordinate)
  }
  object DSL {
    trait RichElement {
      this: org.digimead.tabuddy.model.dsl.DSL.RichElement =>
      /**
       * create new or retrieve exists note
       */
      def note[T](id: Symbol, coordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(f: Note[Stash] => T): Note[Stash] =
        apply(DLS_element, id, coordinate, f)
      def toNote() = DLS_element.eAs[Note[Stash], Stash]
    }
  }
  /** The marker object that describes note scope */
  class Scope(override val modificator: Symbol = 'Note) extends Record.Scope(modificator) {
    override def canEqual(other: Any): Boolean = other.isInstanceOf[org.digimead.tabuddy.model.predef.Note.Scope]
  }
  /**
   * Note specific stash realization
   */
  class Stash(override val context: Context, override val coordinate: Coordinate, override val created: Element.Timestamp,
    override val id: Symbol, override val scope: Element.Scope, override val unique: UUID, override val property: org.digimead.tabuddy.model.element.Stash.Data)
    extends Record.Stash(context, coordinate, created, id, scope, unique, property) {
  }
}
