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

/** Contain list of an axis values. */
class Coordinate private (
  /** List of an axis values. */
  val coordinate: List[Axis[_ <: AnyRef with java.io.Serializable]]) extends java.io.Serializable {
  /** Check if coordinate at the root point. */
  def isRoot() = coordinate.isEmpty
  /** Visual coordinate representation. */
  override def toString() = if (coordinate.nonEmpty)
    "Axis(%s)".format(coordinate.map(axis => "%s->%s".format(axis.id.name, axis.value.toString)).mkString(", "))
  else
    "ROOT"
  override def equals(that: Any): Boolean = (this eq that.asInstanceOf[Object]) || (that match {
    case that: Coordinate =>
      this.coordinate.sortBy(_.id.name) == that.coordinate.sortBy(_.id.name)
    case _ => false
  })
  override def hashCode() = {
    /*
     * Of the remaining four, I'd probably select P(31), as it's the cheapest to calculate on a
     * RISC machine (because 31 is the difference of two powers of two). P(33) is
     * similarly cheap to calculate, but it's performance is marginally worse, and
     * 33 is composite, which makes me a bit nervous.
     */
    val p = 31
    coordinate.foldLeft(0)((a, b) => a * p + b.hashCode)
  }
}

/**
 * Companion object for coordinate
 * that contains predefined root coordinate and coordinate builders.
 */
object Coordinate {
  /** Predefined point with empty coordinate list. */
  val root = new Coordinate(List())

  /** Create Coordinate with sorted axes list */
  def apply(coordinate: Axis[_ <: AnyRef with java.io.Serializable]*): Coordinate = if (coordinate.nonEmpty) new Coordinate(coordinate.sortBy(_.id.name).toList) else root
  /** Create Coordinate with sorted axes list */
  def apply(coordinate: List[Axis[_ <: AnyRef with java.io.Serializable]]): Coordinate = if (coordinate.nonEmpty) new Coordinate(coordinate.sortBy(_.id.name)) else root
}
