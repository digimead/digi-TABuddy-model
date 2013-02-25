/**
 * This file is part of the TABuddy project.
 * Copyright (c) 2012-2013 Alexey Aksenov ezh@ezh.msk.ru
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Global License version 3
 * as published by the Free Software Foundation with the addition of the
 * following permission added to Section 15 as permitted in Section 7(a):
 * FOR ANY PART OF THE COVERED WORK IN WHICH THE COPYRIGHT IS OWNED
 * BY Limited Liability Company «MEZHGALAKTICHESKIJ TORGOVYJ ALIANS»,
 * Limited Liability Company «MEZHGALAKTICHESKIJ TORGOVYJ ALIANS» DISCLAIMS
 * THE WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Global License for more details.
 * You should have received a copy of the GNU Affero General Global License
 * along with this program; if not, see http://www.gnu.org/licenses or write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA, 02110-1301 USA, or download the license from the following URL:
 * http://www.gnu.org/licenses/agpl.html
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Global License.
 *
 * In accordance with Section 7(b) of the GNU Affero General Global License,
 * you must retain the producer line in every report, form or document
 * that is created or manipulated using TABuddy.
 *
 * You can be released from the requirements of the license by purchasing
 * a commercial license. Buying such a license is mandatory as soon as you
 * develop commercial activities involving the TABuddy software without
 * disclosing the source code of your own applications.
 * These activities include: offering paid services to customers,
 * serving files in a web or/and network application,
 * shipping TABuddy with a closed source product.
 *
 * For more information, please contact Digimead Team at this
 * address: ezh@ezh.msk.ru
 */

package org.digimead.tabuddy.model.element.compare

import org.digimead.tabuddy.model.element.Element

object CompareByContent extends Compare {
  /** Compares this object with the specified object for order. */
  def compare(e1: Element.Generic, e2: Element.Generic): Int =
    e1.eModified.milliseconds compare e2.eModified.milliseconds match {
      case 0 =>
        e1.eModified.nanoShift compare e2.eModified.nanoShift
      case c =>
        // modification time different but
        val e1Data = e1.eStash.property
        val e2Data = e2.eStash.property
        // 1. can equal
        val equal = e1.canEqual(e2.getClass, e2.eStash.getClass) &&
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
        if (equal) 0 else c
    }
}
