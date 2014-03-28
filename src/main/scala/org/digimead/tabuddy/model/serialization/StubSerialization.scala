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

package org.digimead.tabuddy.model.serialization

import java.net.URI
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.{ ElementBox, Node }
import org.digimead.tabuddy.model.serialization.transport.Transport

class StubSerialization extends Mechanism with Loggable {
  /** Identifier of the serialization mechanism. */
  val identifier = StubSerialization.Identifier

  /**
   * Load element.
   *
   * @param elementBox element to load
   * @param transport serialization transport
   * @param sData serialization data with parameters
   *
   * @return element
   */
  def load[A <: Element](elementBox: ElementBox[A], transport: Transport, sData: SData)(implicit m: Manifest[A]): A = ???
  /**
   * Save element.
   *
   * @param elementBox element to save
   * @param transport serialization transport
   * @param sData serialization data with parameters
   */
  def save[A <: Element](elementBox: ElementBox[A], transport: Transport, sData: SData) = ???
}

object StubSerialization {
  /** StubSerialization identifier. */
  object Identifier extends Serialization.Identifier { val extension = "stub" }
}
