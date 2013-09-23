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

/**
 * Abstract location description.
 */
trait LocationGeneric {
  type ElementType <: Element
  /** Type of the relative element. */
  type RelativeType <: Element.Relative[ElementType]

  /** Element box coordinate. */
  val coordinate: Coordinate
  /** Element type. */
  val elementType: Manifest[ElementType]
  /** Node id. */
  val id: Symbol
  /** Element scope. */
  val scope: ElementType#StashType#ScopeType

  /** Element stash type. */
  val stashClass: Class[_ <: ElementType#StashType]
  /** Node unique. */
  val unique: Option[UUID]

  /** Get relative element. */
  def toRelative(element: ElementType): RelativeType
}

/**
 * An element reference that points to the relative location.
 */
case class Location(val id: Symbol, val unique: Option[UUID], val scope: Element#StashType#ScopeType,
  val coordinate: Coordinate)(implicit val elementType: Manifest[Element], val stashClass: Class[_ <: Element#StashType])
  extends LocationGeneric {
  type ElementType = Element
  type RelativeType = Element.Relative[ElementType]
  def toRelative(element: ElementType): RelativeType = new Element.Relative(element) {}
}
