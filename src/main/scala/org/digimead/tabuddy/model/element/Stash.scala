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

import scala.collection.immutable

/** Generic element stash. */
class Stash(val coordinate: Coordinate,
  val created: Element.Timestamp,
  val id: Symbol,
  val modified: Element.Timestamp,
  val origin: Symbol,
  val property: Stash.Data,
  val scope: Element.Scope,
  val unique: UUID) extends Stash.Like {
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
  /** Element properties: ID -> typeSymbol, Value */
  type Data = immutable.HashMap[Symbol, immutable.HashMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]]]
  /**
   * Element actual data.
   *
   * (origin, unique, coordinate) is element reference that allow unambiguously identify this data piece.
   */
  trait Like extends Equals {
    /** Stash type. */
    type StashType <: Like
    /** Scope type */
    type ScopeType <: Element.Scope

    /** List of axes(tags). This is copy of ElementBox.coordinate. */
    val coordinate: Coordinate
    /** Element creation time */
    val created: Element.Timestamp
    /** Element verbose id. This is copy of Node.id. */
    val id: Symbol
    /** Element modification time. */
    val modified: Element.Timestamp
    /** Graph owner identifier. This is copy of Graph.origin. */
    val origin: Symbol
    /** Element properties(values) map: Erasure -> Symbol -> Value[T]. */
    val property: Stash.Data
    /** User scope. */
    val scope: ScopeType
    /** Element system id. This is copy of Node.unique. */
    val unique: UUID

    /** Copy constructor */
    def copy(coordinate: Coordinate = this.coordinate,
      created: Element.Timestamp = this.created,
      id: Symbol = this.id,
      modified: Element.Timestamp = this.modified,
      origin: Symbol = this.origin,
      property: Stash.Data = this.property,
      scope: Element.Scope = this.scope,
      unique: UUID = this.unique): StashType = {
      assert(scope == this.scope, "Incorrect scope %s, must be %s".format(scope, this.scope))
      val newStashCtor = this.getClass.getConstructors.find(_.getParameterTypes() match {
        case Array(coordinateArg, createdArg, idArg, modifiedArg, originArg, dataArg, scopeArg, uuidArg) ⇒
          scopeArg.isAssignableFrom(scope.getClass) && coordinateArg.isAssignableFrom(coordinate.getClass()) &&
            createdArg.isAssignableFrom(created.getClass()) && idArg.isAssignableFrom(id.getClass()) &&
            modifiedArg.isAssignableFrom(modified.getClass()) && originArg.isAssignableFrom(origin.getClass()) &&
            dataArg.isAssignableFrom(property.getClass()) && uuidArg.isAssignableFrom(unique.getClass)
        case _ ⇒ false
      }) getOrElse {
        throw new NoSuchMethodException(s"Unable to find proper constructor for stash ${this.getClass()}.")
      }
      val data = new Stash.Data
      newStashCtor.newInstance(coordinate, created, id, modified, origin, property, scope, unique).asInstanceOf[StashType]
    }
    override def canEqual(that: Any): Boolean = that match {
      case _: this.type ⇒ true
      case _ ⇒ false
    }
  }
}
