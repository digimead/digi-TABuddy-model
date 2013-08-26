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

object CompareByTimestampAndContent extends Compare {
  /** Compares this object with the specified object for order. */
  def compare(e1: Element.Generic, e2: Element.Generic): Int =
    e1.eModified.milliseconds compare e2.eModified.milliseconds match {
      case 0 =>
        e1.eModified.nanoShift compare e2.eModified.nanoShift match {
          case 0 => 0
          case c =>
            // modification time different but
            if (isEqual(e1, e2)) 0 else c
        }
      case c =>
        // modification time different but
        if (isEqual(e1, e2)) 0 else c
    }
  /** Compare current elements without children */
  protected def isEqual(e1: Element.Generic, e2: Element.Generic): Boolean = {
    val e1Data = e1.eStash.property
    val e2Data = e2.eStash.property
    // 1. can equal
    e1.canEqual(e2.getClass, e2.eStash.getClass) &&
      // 2. immutable variables are identical
      e1.hashCode == e2.hashCode && {
        // 3. stash equals
        e1.eStash == e2.eStash || { // OR
          // 3. can equal
          e1.eStash.canEqual(e2.eStash) &&
            // 4. immutable variables are identical
            e1.eStash.hashCode == e2.eStash.hashCode &&
            // 5. mutable variables values are identical
            e1Data.keys.forall { valuesType =>
              if (e1Data(valuesType).isEmpty) {
                // drop empty valuesType
                !e2Data.contains(valuesType) || e2Data(valuesType).isEmpty
              } else if (!e2Data.contains(valuesType)) {
                false
              } else {
                val e1Values = e1Data(valuesType)
                val e2Values = e2Data(valuesType)
                e1Values == e2Values && {
                  // 6. compare values
                  e1Values.keys.forall(propertySymbol => e1Values(propertySymbol) == e2Values(propertySymbol))
                }
              }
            }
        }
      }
  }
}
