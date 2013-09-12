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

package org.digimead.tabuddy.model.graph

import org.digimead.tabuddy.model.element.Element

/**
 * Base trait for all modifiable objects.
 */
trait Modifiable {
  @volatile protected var modificationTimestamp: Element.Timestamp = Element.timestamp()
  /** Get modification timestamp. */
  def modification: Element.Timestamp
  /** Set modification timestamp. */
  def modification_=(arg: Element.Timestamp)
  /** Update modification timestamp only if argument is greater than current value. */
  def modificationUpdate(arg: Element.Timestamp)
}
