/**
 * This file is part of the TABuddy project.
 * Copyright (c) 2012 Alexey Aksenov ezh@ezh.msk.ru
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

import scala.collection.mutable

/**
 * Element actual data.
 */
trait Stash extends java.io.Serializable {
  /** child elements */
  @volatile var children = List[Element.Generic]()
  /** Element context. */
  val context: Element.Context
  /** List of axes(tags). */
  val coordinate: Element.Coordinate
  /** Element user id. */
  val id: Symbol
  /** User scope. */
  val scope: String
  /** Element system id. */
  val unique: UUID
  /** Element's model */
  var model: Option[Model.Interface]

  /** Element last modification time. */
  @volatile var lastModification: Long = System.currentTimeMillis()
  /** Element properties(values) map: erasure -> Symbol -> Value[T]. */
  val property: Stash.Data

  /** Copy constructor */
  def copy(context: Element.Context = this.context,
    coordinate: Element.Coordinate = this.coordinate,
    id: Symbol = this.id,
    scope: String = this.scope,
    unique: UUID = this.unique,
    model: Option[Model.Interface] = this.model,
    lastModification: Long = this.lastModification,
    property: Stash.Data = this.property): Stash
  def copyDeepProperty(from: mutable.HashMap[String, mutable.HashMap[Symbol, Value[_ <: java.io.Serializable]]],
    to: mutable.HashMap[String, mutable.HashMap[Symbol, Value[_ <: java.io.Serializable]]]) {
    to.clear
    from.keys.foreach {
      typeKey =>
        to(typeKey) = new mutable.HashMap[Symbol, Value[_ <: java.io.Serializable]]() with mutable.SynchronizedMap[Symbol, Value[_ <: java.io.Serializable]]
        from(typeKey).keys.foreach {
          dataKey =>
            to(typeKey)(dataKey) = from(typeKey)(dataKey)
        }
    }
  }
}

object Stash {
  class Data extends mutable.HashMap[String, mutable.HashMap[Symbol, Value[_ <: java.io.Serializable]]]() with mutable.SynchronizedMap[String, mutable.HashMap[Symbol, Value[_ <: java.io.Serializable]]]
}