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

import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Stash

class Stub extends Serialization[Unit] {
  /**
   * Load elements from Iterable[???] with loadElement().
   * Filter/adjust loaded element with filter()
   * Return deserialized element.
   */
  def acquire[A <: Element[B], B <: Stash](loadElement: () => Option[Unit],
    filter: (Element.Generic) => Option[Element.Generic] = filterAccept)(implicit ma: Manifest[A], mb: Manifest[B]): Option[A] = None
  /**
   * Get serialized element.
   * Filter/adjust children with filter()
   * Save adjusted child to [???] with saveElement().
   */
  def freeze(element: Element.Generic,
    saveElement: (Element.Generic, Unit) => Unit,
    filter: (Element.Generic) => Option[Element.Generic] = filterAccept): Unit = {}
}
