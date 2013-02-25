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

package org.digimead.tabuddy.model

import java.util.UUID
import scala.annotation.tailrec
import scala.collection.mutable
import scala.ref.WeakReference
import org.digimead.digi.lib.log.logger.RichLogger.rich2slf4j
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.element.Reference

trait ModelIndex {
  this: Model.Interface[_ <: Model.Stash] =>
  type HashMapPerOrigin = mutable.HashMap[Symbol, WeakReference[Element.Generic]] with mutable.SynchronizedMap[Symbol, WeakReference[Element.Generic]]
  type HashMapPerAxis = mutable.HashMap[Coordinate, HashMapPerOrigin] with mutable.SynchronizedMap[Coordinate, HashMapPerOrigin]
  type HashMapPerId = mutable.HashMap[UUID, HashMapPerAxis] with mutable.SynchronizedMap[UUID, HashMapPerAxis]
  /**
   * Nested HashMap of all elements
   * Id(Symbol) -> Axis(Coordinate) -> Origin(Symbol) = Generic
   */
  @transient protected lazy val index: HashMapPerId = new mutable.HashMap[UUID, HashMapPerAxis] with mutable.SynchronizedMap[UUID, HashMapPerAxis]

  /** Get element for unique id at the specific coordinate and default origin */
  def e(unique: UUID, coordinate: Axis[_ <: AnyRef with java.io.Serializable]*): Option[Element.Generic] =
    e(Reference(eStash.id, unique, Coordinate(coordinate: _*)))
  /** Get element for unique id at the specific coordinate and origin */
  def e(origin: Symbol, unique: UUID, coordinate: Axis[_ <: AnyRef with java.io.Serializable]*): Option[Element.Generic] =
    e(Reference(origin, unique, Coordinate(coordinate: _*)))
  /** Get element for unique id at the specific coordinate and origin */
  def e(origin: Symbol, unique: UUID, coordinate: Coordinate): Option[Element.Generic] =
    e(Reference(origin, unique, coordinate))
  /** Get element for unique id at the specific coordinate and default origin */
  def e(unique: UUID, coordinate: Coordinate): Option[Element.Generic] =
    e(Reference(eStash.context.container.origin, unique, coordinate))
  /** Get element for unique id at the specific coordinate and origin */
  def e(reference: Reference): Option[Element.Generic] = {
    for {
      allAreasWithId <- index.get(reference.unique)
      allAreasWithIdPerAxis <- allAreasWithId.get(reference.coordinate)
      element <- allAreasWithIdPerAxis.get(reference.origin)
    } yield element.get
  } getOrElse None
  /** Add/replace an element(s) to index. */
  @tailrec
  protected final def eRegister(elements: Element.Generic*) {
    val children = elements.map { element =>
      log.debug("register %s at elements index".format(element))
      val Reference(origin, unique, coordinate) = element.eReference
      val coordinateField = index get (unique) getOrElse {
        val field = new mutable.HashMap[Coordinate, HashMapPerOrigin] with mutable.SynchronizedMap[Coordinate, HashMapPerOrigin]
        index(unique) = field
        field
      }
      val originField = coordinateField get (coordinate) getOrElse {
        val field = new mutable.HashMap[Symbol, WeakReference[Element.Generic]] with mutable.SynchronizedMap[Symbol, WeakReference[Element.Generic]]
        coordinateField(coordinate) = field
        field
      }
      originField(origin) = new WeakReference[Element.Generic](element)
      element.eChildren
    }
    if (children.isEmpty) return
    eRegister(children.flatten: _*)
  }
  /**
   * Rebuild index.
   */
  def eIndexRebuid() {
    log.debug("rebuild index for " + this.eModel)
    index.clear
    eRegister(eModel)
  }
  /** Remove an element(s) from index. */
  @tailrec
  protected final def eUnregister(elements: Element.Generic*) {
    val children = elements.map { element =>
      log.debug("unregister %s from elements index".format(element))
      val Reference(origin, unique, coordinate) = element.eReference
      index get (unique) foreach { coordinateField =>
        coordinateField get (coordinate) foreach { originField =>
          originField.remove(origin)
          if (originField.isEmpty)
            coordinateField.remove(coordinate)
        }
        if (coordinateField.isEmpty)
          index.remove(unique)
      }
      element.eChildren
    }
    if (children.isEmpty) return
    eUnregister(children.flatten: _*)
  }
}
