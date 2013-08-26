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

/**
 * Abstract location description
 */
abstract class LocationGeneric[A <: Element[B], B <: Stash](val id: Symbol, val scope: Element.Scope,
  val coordinate: Coordinate)(implicit em: Manifest[A], sm: Manifest[B]) {
  lazy val elementManifest = em
  lazy val stashManifest = sm
}

/**
 * Element reference that point to relative location
 */
case class Location[A <: Element[B], B <: Stash](override val id: Symbol, override val scope: Element.Scope,
  override val coordinate: Coordinate = Coordinate.root)(implicit em: Manifest[A], sm: Manifest[B])
  extends LocationGeneric[A, B](id, scope, coordinate)
