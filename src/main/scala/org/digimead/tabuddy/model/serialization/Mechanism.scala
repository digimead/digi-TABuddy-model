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

import java.net.URI

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.serialization.transport.Transport

/**
 * Interface for serialization of an element content.
 */
trait Mechanism {
  this: Loggable ⇒
  /** Identifier of the serialization mechanism. */
  val identifier: Serialization.Identifier

  /**
   * Load element.
   *
   * @param elementBox box of the loaded element
   * @param storageURI storage URI
   * @param transport serialization transport
   *
   * @return element
   */
  def load[A <: Element](elementBox: ElementBox[A], storageURI: URI, transport: Transport)(implicit m: Manifest[A]): A
  /**
   * Save element.
   *
   * @param ancestorsNSelf sequence of ancestors
   * @param element element to save
   * @param storageURI storage URI
   * @param transport serialization transport
   */
  def save(ancestorsNSelf: Seq[Node[_ <: Element]], element: Element, storageURI: URI, transport: Transport)
}
