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

import scala.ref.WeakReference

import org.digimead.digi.lib.log.Loggable
import org.digimead.tabuddy.model.Model.model2implementation

/**
 * Trait that provide general interface for Value implementation
 */
sealed trait Value[T <: java.io.Serializable] extends java.io.Serializable {
  /** Value context information */
  val context: Element.Context

  /** Get value. */
  def get(): T
  /** Set value. */
  def set(x: T): Boolean
  /** Is value writable / set() available. */
  def isWriteable: Boolean
  /** Class equality with context but without values. */
  def ===(that: Any): Boolean
  /** get() equality. */
  override def equals(that: Any): Boolean =
    (this eq that.asInstanceOf[Object]) || (that match {
      case that: Value[_] => this.get == that.get
      case _ => false
    })
  override def hashCode() = context.hashCode
  /** Needed for correct definition of equals for general classes. */
  def canEqual(that: Any): Boolean
}

/**
 * Singleton that contain Static and Dynamic Value implementation.
 */
object Value extends Loggable {
  implicit def byte2value(x: Byte)(implicit weakContainer: WeakReference[_ <: Element.Generic]) = x2staticValue(Byte.box(x))
  implicit def double2value(x: Double)(implicit weakContainer: WeakReference[_ <: Element.Generic]) = x2staticValue(Double.box(x))
  implicit def float2value(x: Float)(implicit weakContainer: WeakReference[_ <: Element.Generic]) = x2staticValue(Float.box(x))
  implicit def int2value(x: Int)(implicit weakContainer: WeakReference[_ <: Element.Generic]) = x2staticValue(Int.box(x))
  implicit def long2value(x: Long)(implicit weakContainer: WeakReference[_ <: Element.Generic]) = x2staticValue(Long.box(x))
  implicit def short2value(x: Short)(implicit weakContainer: WeakReference[_ <: Element.Generic]) = x2staticValue(Short.box(x))
  implicit def bool2value(x: Boolean)(implicit weakContainer: WeakReference[_ <: Element.Generic]) = x2staticValue(Boolean.box(x))
  implicit def serializable2value[T <: java.io.Serializable](x: T)(implicit weakContainer: WeakReference[_ <: Element.Generic], m: Manifest[T]) = x2staticValue(x)
  implicit def value2x[T <: java.io.Serializable](x: Value[T]): T = x.get()

  /**
   * Convert [T] to Value.Static
   */
  def x2staticValue[T <: java.io.Serializable](x: T)(implicit weakContainer: WeakReference[_ <: Element.Generic], m: Manifest[T]): Option[Static[T]] =
    weakContainer.get match {
      case Some(container) =>
        val stack = new Throwable().getStackTrace()
        if (stack.size < 1)
          Some(new Static(x, Element.virtualContext(container)))
        else
          Some(new Static(x, Model.contextForChild(container, Some(stack(1)))))
      case None =>
        log.fatal("container lost")
        None
    }

  /**
   * Dynamic value implementation
   */
  class Dynamic[T <: java.io.Serializable](
    /** Actual value. */
    protected val data: () => T,
    /** Value context information. */
    val context: Element.Context)(implicit m: Manifest[T]) extends Value[T] {

    /** Is value writable / set() available. */
    def isWriteable = false
    /** Get value. */
    def get() = data()
    /** Set value. */
    def set(x: T) = {
      log.fatal("unable to assign data to dynamic value")
      false
    }
    /** Class equality with context but without values. */
    def ===(that: Any): Boolean = (this eq that.asInstanceOf[Object]) || (that match {
      case that: Dynamic[_] if this.data.getClass == that.data.getClass && this.context == that.context => (that canEqual this)
      case _ => false
    })
    override def hashCode() = List(data, context).foldLeft(0)((a, b) => a * 31 + b.hashCode())
    /** Needed for correct definition of equals for general classes. */
    def canEqual(that: Any): Boolean = that.isInstanceOf[Dynamic[_]]
    override def toString() = "Dynamic[%s](%s)".format(m.erasure.getName.split("""\.""").last, get)
  }
  /**
   * Static value implementation
   */
  class Static[T <: java.io.Serializable](
    /** Initial value. */
    protected val initial: T,
    /** Value context information. */
    val context: Element.Context)(implicit m: Manifest[T]) extends Value[T] {
    /** Actual value. */
    @volatile protected var data: T = initial

    /** Is value writable / set() available. */
    def isWriteable = true
    /** Get value. */
    def get() = data
    /** Set value. */
    def set(x: T) = {
      data = x
      true
    }
    /** Class equality with context but without values. */
    def ===(that: Any): Boolean = (this eq that.asInstanceOf[Object]) || (that match {
      case that: Static[_] if this.data.getClass == that.data.getClass && this.context == that.context => (that canEqual this)
      case _ => false
    })
    override def hashCode() = List(initial, data, context).foldLeft(0)((a, b) => a * 31 + b.hashCode())
    /** Needed for correct definition of equals for general classes. */
    def canEqual(that: Any): Boolean = that.isInstanceOf[Static[_]]
    override def toString() = "Static[%s](%s)".format(m.erasure.getName.split("""\.""").last, get)
  }
}
