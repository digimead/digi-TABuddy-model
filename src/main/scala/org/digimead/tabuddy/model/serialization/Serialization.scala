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
import org.digimead.tabuddy.model.element.Stash

trait Serialization[T] extends Loggable {
  /**
   * Load elements from an Iterable[T] with loadElement().
   * Filter/adjust a loaded element with filter()
   * Return a deserialized element.
   */
  def acquire[A <: Element](loadElement: () => Option[T],
    filter: (Element) => Option[Element] = filterAccept)(implicit ma: Manifest[A]): Option[A]
  /**
   * Get a serialized element.
   * Filter/adjust a child with filter()
   * Save adjusted children to [T] with saveElement().
   */
  def freeze(element: Element, saveElement: (Element, T) => Unit,
    filter: (Element) => Option[Element] = filterAccept)
  /** Serialization filter that accept all elements */
  def filterAccept(element: Element): Option[Element] = Some(element)
  /** Serialization filter that accept all elements */
  def filterDeny(element: Element): Option[Element] = None
}
