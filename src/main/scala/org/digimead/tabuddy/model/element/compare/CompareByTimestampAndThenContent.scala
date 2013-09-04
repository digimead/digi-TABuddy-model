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

package org.digimead.tabuddy.model.element.compare

import org.digimead.tabuddy.model.element.Element

object CompareByTimestampAndThenContent extends Compare {
  /** Compares this object with the specified object for order. */
  def compare(e1: Element, e2: Element): Int =
    e1.eModified.milliseconds compare e2.eModified.milliseconds match {
      case 0 ⇒
        e1.eModified.nanoShift compare e2.eModified.nanoShift match {
          case 0 ⇒ 0
          case c ⇒
            // modification time different but
            if (isEqual(e1, e2)) 0 else c
        }
      case c ⇒
        // modification time different but
        if (isEqual(e1, e2)) 0 else c
    }
  /** Compare current elements without children */
  protected def isEqual(e1: Element, e2: Element): Boolean = {
    e1.canEqual(e2) && {
      // 1. immutable element are identical
      e1.## == e2.## || { // OR
        val e1Data = e1.eStash.property
        val e2Data = e2.eStash.property
        val e1DataSize = e1Data.keys.foldLeft(0)((acc, valueId) => acc + e1Data(valueId).size)
        val e2DataSize = e2Data.keys.foldLeft(0)((acc, valueId) => acc + e2Data(valueId).size)
        // 2. can equal
        e1.eStash.canEqual(e2.eStash) &&
          // 3. immutable variables are identical and creation time is the same
          e1.eStash.scope == e2.eStash.scope && e1.eStash.created == e2.eStash.created &&
          // 4. property values are identical
          e1DataSize == e2DataSize && e1Data.keys.forall { valuesId ⇒
            if (e1Data(valuesId).isEmpty) {
              // drop empty valuesType
              !e2Data.contains(valuesId) || e2Data(valuesId).isEmpty
            } else if (!e2Data.contains(valuesId)) {
              false
            } else {
              val e1Values = e1Data(valuesId)
              val e2Values = e2Data(valuesId)
              e1Values == e2Values && {
                // 6. compare values
                e1Values.keys.forall(typeSymbolId ⇒ e1Values(typeSymbolId) == e2Values(typeSymbolId))
              }
            }
          }
      }
    }
  }
}
