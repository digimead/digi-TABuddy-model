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

package org.digimead.tabuddy.model.element

import java.util.UUID

import scala.collection.mutable

import org.digimead.tabuddy.model.Model

/**
 * Element actual data.
 */
trait Stash extends java.io.Serializable {
  /** Element context. */
  val context: Context
  /** List of axes(tags). */
  val coordinate: Coordinate
  /** Element creation time */
  val created: Element.Timestamp
  /** Element user id. */
  val id: Symbol
  /** Element modification time. */
  @volatile var modified: Element.Timestamp = created
  /** User scope. */
  val scope: Element.Scope
  /** Element system id. */
  val unique: UUID
  /** Element's model */
  var model: Option[Model.Generic] = None
  /** Element properties(values) map: erasure -> Symbol -> Value[T]. */
  val property: Stash.Data
  validateForSelfReference()

  /** Copy constructor */
  def copy(context: Context = this.context,
    coordinate: Coordinate = this.coordinate,
    created: Element.Timestamp = this.created,
    id: Symbol = this.id,
    modified: Element.Timestamp = this.modified,
    scope: Element.Scope = this.scope,
    unique: UUID = this.unique,
    model: Option[Model.Generic] = this.model,
    property: Stash.Data = this.property): this.type
  /** deep copy from -> to with copyF function */
  def copyDeepProperty(from: mutable.HashMap[Symbol, mutable.HashMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]]],
    to: mutable.HashMap[Symbol, mutable.HashMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]]]): Unit =
    copyDeepProperty(from, to, (v) => v.copy(context = v.context.copy(container = Reference(context.container.origin, unique, coordinate))))
  /** deep copy from -> to with copyF function */
  def copyDeepProperty(from: mutable.HashMap[Symbol, mutable.HashMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]]],
    to: mutable.HashMap[Symbol, mutable.HashMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]]],
    copyF: Value[_ <: AnyRef with java.io.Serializable] => Value[_ <: AnyRef with java.io.Serializable]) {
    to.clear
    from.keys.foreach {
      idKey =>
        to(idKey) = new mutable.HashMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]]() with mutable.SynchronizedMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]]
        from(idKey).keys.foreach {
          typeSymbolKey =>
            to(idKey)(typeSymbolKey) = copyF(from(idKey)(typeSymbolKey))
        }
    }
  }

  /** Validates stash on creation for circular reference */
  protected def validateForSelfReference() =
    assert(context.container.unique != unique, "circular reference detected for " + this)

  /** Needed for correct definition of equals for general classes. */
  def canEqual(that: Any): Boolean = that.getClass == that.getClass
  /** Indicates whether some other stash is "equal to" this one. */
  override def equals(that: Any): Boolean =
    (this eq that.asInstanceOf[Object]) || (that match {
      case that: Stash =>
        // 1. can equal
        this.canEqual(that) &&
          // 2. immutable variables are identical
          this.hashCode == that.hashCode &&
          // 3. mutable variables are identical
          this.modified == that.modified
      case _ => false
    })
  /** Returns a hash code value for the object. */
  override def hashCode() = {
    /*
     * Of the remaining four, I'd probably select P(31), as it's the cheapest to calculate on a
     * RISC machine (because 31 is the difference of two powers of two). P(33) is
     * similarly cheap to calculate, but it's performance is marginally worse, and
     * 33 is composite, which makes me a bit nervous.
     */
    val p = 31
    p * (p * (p * (p * (p * (p * (p + getClass.hashCode()) + context.hashCode) + coordinate.hashCode) +
      created.hashCode()) + id.hashCode) + scope.hashCode) + unique.hashCode()
  }
}

object Stash {
  /** Element properties: ID -> typeSymbol, Value */
  class Data extends mutable.HashMap[Symbol, mutable.HashMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]]]() with mutable.SynchronizedMap[Symbol, mutable.HashMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]]]
}
