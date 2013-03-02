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
