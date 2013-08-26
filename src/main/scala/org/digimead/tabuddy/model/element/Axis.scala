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

import org.digimead.tabuddy.model.dsl.DSLType

import language.implicitConversions

/** Axis/tag value of coordinate. */
case class Axis[T <: AnyRef with java.io.Serializable](
  /** Axis user id. */
  val id: Symbol,
  /** Axis value. */
  val value: T)(implicit val m: Manifest[T]) {
  assert(DSLType.classes(m.runtimeClass), "unknown type " + m.runtimeClass)
}

/**
 * Companion object for an axis
 * that contains implicit conversions between primitive and axis.
 */
object Axis {
  implicit def intToAxis(x: (Symbol, Int)): Axis[java.lang.Integer] = Axis[java.lang.Integer](x._1, Int.box(x._2))
}
