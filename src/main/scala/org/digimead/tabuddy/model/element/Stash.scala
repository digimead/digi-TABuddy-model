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

package org.digimead.tabuddy.model.element

import scala.collection.immutable

import org.digimead.tabuddy.model.graph.Modifiable

/** Generic element stash. */
class Stash(val created: Element.Timestamp,
  val modified: Element.Timestamp,
  val property: Stash.Data,
  val scope: Element.Scope) extends Stash.Like {
  /** Stash type. */
  type StashType = Stash
  /** Scope type */
  type ScopeType = Element.Scope
}

object Stash {
  // The reason for immutability is:
  //   pros:
  //     scalable (for example over network with Akka)
  //     ready for distributed calculation
  //     thread safe
  //   cons:
  //     every modification recreate nested hash structure
  //     greedy
  /** Element properties: valueId -> typeSymbolId, Value */
  type Data = immutable.HashMap[Symbol, immutable.HashMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]]]
  /**
   * Element actual data.
   *
   * (origin, unique, coordinate) is element reference that allow unambiguously identify this data piece.
   */
  trait Like extends Modifiable.Read with Equals with java.io.Serializable {
    /** Stash type. */
    type StashType <: Like
    /** Scope type */
    type ScopeType <: Element.Scope

    /** Element creation time */
    val created: Element.Timestamp
    /** Element modification time. */
    val modified: Element.Timestamp
    /** Element properties(values) map: Erasure -> Symbol -> Value[T]. */
    val property: Stash.Data
    /** User scope. */
    val scope: ScopeType

    /** Copy constructor */
    def copy(created: Element.Timestamp = this.created,
      modified: Element.Timestamp = this.modified,
      property: Stash.Data = this.property,
      scope: Element.Scope = this.scope): StashType = {
      assert(scope == this.scope, "Incorrect scope %s, must be %s".format(scope, this.scope))
      val newStashCtor = this.getClass.getConstructors.find(_.getParameterTypes() match {
        case Array(createdArg, modifiedArg, dataArg, scopeArg) ⇒
          scopeArg.isAssignableFrom(scope.getClass) && createdArg.isAssignableFrom(created.getClass()) &&
            modifiedArg.isAssignableFrom(modified.getClass()) && dataArg.isAssignableFrom(property.getClass())
        case _ ⇒ false
      }) getOrElse {
        throw new NoSuchMethodException(s"Unable to find proper constructor for stash ${this.getClass()}.")
      }
      val data = new Stash.Data
      newStashCtor.newInstance(created, modified, property, scope).asInstanceOf[StashType]
    }

    override def canEqual(that: Any): Boolean = that.isInstanceOf[Stash.Like]
    override def equals(that: Any): Boolean = that match {
      case that: Like ⇒ (that eq this) || ((that canEqual this) && this.## == that.##)
      case _ ⇒ false
    }
    override def hashCode() = lazyHashCode
    protected lazy val lazyHashCode = java.util.Arrays.hashCode(Array[AnyRef](created, modified, property, scope))
  }
}
