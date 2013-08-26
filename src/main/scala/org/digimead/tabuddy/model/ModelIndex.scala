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

package org.digimead.tabuddy.model

import java.util.UUID

import scala.annotation.tailrec
import scala.collection.mutable
import scala.ref.WeakReference

import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
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
