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

import scala.collection.mutable

import org.digimead.digi.lib.log.api.Loggable

trait ElementSet extends mutable.LinkedHashSet[Element.Generic] with Loggable {
  val origin: Element.Generic

  abstract override def add(element: Element.Generic): Boolean = {
    val result = super.add(element)
    if (result) {
      val undoF = () => { super.remove(element); {} }
      Element.Event.publish(Element.Event.ChildInclude(origin, element, origin.eModified)(undoF))
    }
    result
  }
  abstract override def remove(element: Element.Generic): Boolean = {
    val result = super.remove(element)
    if (result) {
      val undoF = () => { super.+=(element); {} }
      Element.Event.publish(Element.Event.ChildRemove(origin, element, origin.eModified)(undoF))
    }
    result
  }
  abstract override def clear(): Unit = {
    val children = this.toSeq
    super.clear
    val undoF = () => { children.foreach(super.add) }
    Element.Event.publish(Element.Event.ChildrenReset(origin, origin.eModified)(undoF))
  }
}
