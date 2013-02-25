/**
 * This file is part of the TABuddy project.
 * Copyright (c) 2012-2013 Alexey Aksenov ezh@ezh.msk.ru
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Global License version 3
 * as published by the Free Software Foundation with the addition of the
 * following permission added to Section 15 as permitted in Section 7(a):
 * FOR ANY PART OF THE COVERED WORK IN WHICH THE COPYRIGHT IS OWNED
 * BY Limited Liability Company «MEZHGALAKTICHESKIJ TORGOVYJ ALIANS»,
 * Limited Liability Company «MEZHGALAKTICHESKIJ TORGOVYJ ALIANS» DISCLAIMS
 * THE WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Global License for more details.
 * You should have received a copy of the GNU Affero General Global License
 * along with this program; if not, see http://www.gnu.org/licenses or write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA, 02110-1301 USA, or download the license from the following URL:
 * http://www.gnu.org/licenses/agpl.html
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Global License.
 *
 * In accordance with Section 7(b) of the GNU Affero General Global License,
 * you must retain the producer line in every report, form or document
 * that is created or manipulated using TABuddy.
 *
 * You can be released from the requirements of the license by purchasing
 * a commercial license. Buying such a license is mandatory as soon as you
 * develop commercial activities involving the TABuddy software without
 * disclosing the source code of your own applications.
 * These activities include: offering paid services to customers,
 * serving files in a web or/and network application,
 * shipping TABuddy with a closed source product.
 *
 * For more information, please contact Digimead Team at this
 * address: ezh@ezh.msk.ru
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

  /**
   * Create a detached element with the standard Note class
   */
  def apply[T](id: Symbol, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]]): Note[Stash] =
    Note.apply(id, rawCoordinate, (f) => {})
  /**
   * Create a detached element with the standard Note class
   */
  def apply[T](id: Symbol, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]], f: (Note[Stash]) => T): Note[Stash] =
    Record.apply(classOf[Note[Stash]], classOf[Note.Stash], None, id, rawCoordinate, f)
  /**
   * Get exists or create an attached element with the standard Note class
   */
  def apply[T](container: Element.Generic, id: Symbol, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]]): Note[Stash] =
    Note.apply(container, id, rawCoordinate)
  /**
   * Get exists or create an attached element with the standard Note class
   */
  def apply[T](container: Element.Generic, id: Symbol, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]], f: (Note[Stash]) => T): Note[Stash] =
    Record.apply(classOf[Note[Stash]], classOf[Note.Stash], Some(container), id, rawCoordinate, f)

  /**
   * Part of DSL.Builder for end user
   */
  trait DSL {
    this: org.digimead.tabuddy.model.dsl.DSL[_] =>
    case class NoteLocation(override val id: Symbol,
      override val coordinate: Coordinate = Coordinate.root)
      extends LocationGeneric[Note[Stash], Stash](id, coordinate)
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
  object Scope extends Element.Scope {
    val name = "Note"
  }
  /**
   * Note specific stash realization
   */
  class Stash(override val context: Context, override val coordinate: Coordinate, override val created: Element.Timestamp,
    override val id: Symbol, override val unique: UUID, override val property: org.digimead.tabuddy.model.element.Stash.Data)
    extends Record.Stash(context, coordinate, created, id, unique, property) {
    override lazy val scope: Element.Scope = Note.Scope
  }
}
