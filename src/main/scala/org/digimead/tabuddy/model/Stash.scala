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
import java.util.concurrent.ScheduledThreadPoolExecutor
import java.util.concurrent.TimeUnit

/**
 * Element actual data.
 */
trait Stash extends java.io.Serializable {
  /** Element context. */
  val context: Element.Context
  /** List of axes(tags). */
  val coordinate: Element.Coordinate
  /** Element creation time */
  val created: Stash.Timestamp
  /** Element user id. */
  val id: Symbol
  /** Element modification time. */
  @volatile var modified: Stash.Timestamp = created
  /** User scope. */
  val scope: String
  /** Element system id. */
  val unique: UUID
  /** Element's model */
  var model: Option[Model.Generic] = None

  /** Element properties(values) map: erasure -> Symbol -> Value[T]. */
  val property: Stash.Data

  /** Copy constructor */
  def copy(context: Element.Context = this.context,
    coordinate: Element.Coordinate = this.coordinate,
    created: Stash.Timestamp = this.created,
    id: Symbol = this.id,
    modified: Stash.Timestamp = this.modified,
    scope: String = this.scope,
    unique: UUID = this.unique,
    model: Option[Model.Generic] = this.model,
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
  /** Needed for correct definition of equals for general classes. */
  def canEqual(that: Any): Boolean = that.getClass == that.getClass
  /** Indicates whether some other stash is "equal to" this one. */
  override def equals(that: Any): Boolean =
    (this eq that.asInstanceOf[Object]) || (that match {
      case that: Stash =>
        // 1. can equal
        this.canEqual(that) &&
          // 2. immutable variables are identical
          this.hashCode == that.hashCode &&
          // 3. mutable variables are identical
          this.modified == that.modified
      case _ => false
    })
  /** Returns a hash code value for the object. */
  override def hashCode() = List(getClass, context, coordinate, created, id, scope, unique).foldLeft(0)((a, b) => a * 31 + b.hashCode())
}

object Stash {
  /** Base nanoseconds for Timestamp shift */
  private var nanoBase = System.nanoTime()
  /** nanoBase renew scheduled thread pool executor */
  private val nanoBaseRenewExecutor = new ScheduledThreadPoolExecutor(1)
  /** nanoBase renew task */
  private val nanoBaseSchedule = nanoBaseRenewExecutor.schedule(new Runnable {
    def run = Stash.synchronized { nanoBase = System.nanoTime() }
  }, 1, TimeUnit.MINUTES)

  def timestamp() = synchronized { Timestamp(System.currentTimeMillis(), System.nanoTime() - nanoBase) }

  class Data extends mutable.HashMap[String, mutable.HashMap[Symbol, Value[_ <: java.io.Serializable]]]() with mutable.SynchronizedMap[String, mutable.HashMap[Symbol, Value[_ <: java.io.Serializable]]]
  case class Timestamp(val milliseconds: Long, nanoShift: Long)
}
