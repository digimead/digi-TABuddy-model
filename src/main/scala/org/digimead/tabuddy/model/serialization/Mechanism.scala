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

package org.digimead.tabuddy.model.serialization

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.ElementBox

/**
 * Interface for serialization of an element content.
 */
trait Mechanism extends Loggable {
  /** Identifier of the serialization mechanism. */
  val identifier: Serialization.Identifier

  /** Load element. */
  def load[A <: Element](elementBox: ElementBox[A], from: Array[Byte])(implicit m: Manifest[A]): A
  /** Save element. */
  def save(element: Element): Array[Byte]
}
