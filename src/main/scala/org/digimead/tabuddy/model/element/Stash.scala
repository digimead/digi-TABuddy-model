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
