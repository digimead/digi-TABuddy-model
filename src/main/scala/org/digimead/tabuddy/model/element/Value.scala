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

package org.digimead.tabuddy.model.element

import java.io.Serializable

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.Model.model2implementation
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.dsl.DSLType.dsltype2implementation

import language.implicitConversions

/**
 * Trait that provide general interface for Value implementation
 */
sealed trait Value[T] extends AnyRef with java.io.Serializable {
  /** Value context information */
  val context: Context

  /** Copy constructor */
  def copy(context: Context = this.context): this.type
  /** Get value. */
  def get(): T
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
  implicit def byte2someValue(x: Byte)(implicit container: Element.Generic = null) = Some(static(Byte.box(x)))
  implicit def byte2value(x: Byte)(implicit container: Element.Generic = null) = static(Byte.box(x))
  implicit def double2someValue(x: Double)(implicit container: Element.Generic = null) = Some(static(Double.box(x)))
  implicit def double2value(x: Double)(implicit container: Element.Generic = null) = static(Double.box(x))
  implicit def float2someValue(x: Float)(implicit container: Element.Generic = null) = Some(static(Float.box(x)))
  implicit def float2value(x: Float)(implicit container: Element.Generic = null) = static(Float.box(x))
  implicit def int2someValue(x: Int)(implicit container: Element.Generic = null) = Some(static(Int.box(x)))
  implicit def int2value(x: Int)(implicit container: Element.Generic = null) = static(Int.box(x))
  implicit def long2someValue(x: Long)(implicit container: Element.Generic = null) = Some(static(Long.box(x)))
  implicit def long2value(x: Long)(implicit container: Element.Generic = null) = static(Long.box(x))
  implicit def short2someValue(x: Short)(implicit container: Element.Generic = null) = Some(static(Short.box(x)))
  implicit def short2value(x: Short)(implicit container: Element.Generic = null) = static(Short.box(x))
  implicit def bool2someValue(x: Boolean)(implicit container: Element.Generic = null) = Some(static(Boolean.box(x)))
  implicit def bool2value(x: Boolean)(implicit container: Element.Generic = null) = static(Boolean.box(x))
  implicit def string2someValue(x: String)(implicit container: Element.Generic = null) = Some(static(x))
  implicit def string2value(x: String)(implicit container: Element.Generic = null) = static(x)
  implicit def value2x[T <: AnyRef with java.io.Serializable](x: Value[T]): T = x.get()

  /**
   * Convert [T] to Value.Static
   */
  def static[T <: AnyRef with java.io.Serializable: Manifest](container: Element.Generic, x: T): Static[T] = {
    implicit val e = container
    static(x)
  }
  /**
   * Convert [T] to Value.Static
   */
  def static[T <: AnyRef with java.io.Serializable](x: T)(implicit container: Element.Generic = null, m: Manifest[T]): Static[T] =
    if (container == null)
      new Static(x, Context.empty)
    else {
      val stack = new Throwable().getStackTrace()
      if (stack.size < 1)
        new Static(x, Context.virtual(container))
      else
        new Static(x, Model.contextForChild(container, Some(stack(1))))
    }
  /**
   * Convert () => [T] to Value.Dinamic
   */
  def dinamic[T <: AnyRef with java.io.Serializable: Manifest](container: Element.Generic, x: () => T): Dynamic[T] = {
    implicit val e = container
    dinamic(x)
  }
  /**
   * Convert () => [T] to Value.Dinamic
   */
  def dinamic[T <: AnyRef with java.io.Serializable](x: () => T)(implicit container: Element.Generic = null, m: Manifest[T]): Dynamic[T] =
    if (container == null)
      new Dynamic(x, Context.empty)
    else {
      val stack = new Throwable().getStackTrace()
      if (stack.size < 1)
        new Dynamic(x, Context.virtual(container))
      else
        new Dynamic(x, Model.contextForChild(container, Some(stack(1))))
    }

  /**
   * Dynamic value implementation
   */
  class Dynamic[T <: AnyRef with java.io.Serializable](
    /** Actual value. */
    protected val data: () => T,
    /** Value context information. */
    val context: Context)(implicit m: Manifest[T]) extends Value[T] {
    assert(m.runtimeClass != classOf[java.io.Serializable], "Unable to create a value for generic type java.io.Serializable")

    /** Copy constructor */
    def copy(context: Context = this.context): this.type = (new Dynamic(data, context)).asInstanceOf[this.type]
    /** Get value. */
    def get() = data()
    /** Class equality with context but without values. */
    def ===(that: Any): Boolean = (this eq that.asInstanceOf[Object]) || (that match {
      case that: Dynamic[_] if this.data.getClass == that.data.getClass && this.context == that.context => (that canEqual this)
      case _ => false
    })
    override def hashCode() = {
      /*
       * Of the remaining four, I'd probably select P(31), as it's the cheapest to calculate on a
       * RISC machine (because 31 is the difference of two powers of two). P(33) is
       * similarly cheap to calculate, but it's performance is marginally worse, and
       * 33 is composite, which makes me a bit nervous.
       */
      val p = 31
      p * (p + data.hashCode()) + context.hashCode
    }
    /** Needed for correct definition of equals for general classes. */
    def canEqual(that: Any): Boolean = that.isInstanceOf[Dynamic[_]]
    override def toString() = "Dynamic[%s](%s)".format(m.runtimeClass.getName.split("""\.""").last,
      DSLType.convertToString[T](get()).getOrElse(get()))
  }
  /**
   * Static value implementation
   */
  class Static[T <: AnyRef with java.io.Serializable](
    /** Initial value. */
    protected val data: T,
    /** Value context information. */
    val context: Context)(implicit m: Manifest[T]) extends Value[T] {
    assert(m.runtimeClass != classOf[java.io.Serializable], "Unable to create a value for generic type java.io.Serializable")

    /** Copy constructor */
    def copy(context: Context = this.context): this.type = (new Static(data, context)).asInstanceOf[this.type]
    /** Get value. */
    def get() = data
    /** Class equality with context but without values. */
    def ===(that: Any): Boolean = (this eq that.asInstanceOf[Object]) || (that match {
      case that: Static[_] if this.data.getClass == that.data.getClass && this.context == that.context => (that canEqual this)
      case _ => false
    })
    override def hashCode() = {
      /*
       * Of the remaining four, I'd probably select P(31), as it's the cheapest to calculate on a
       * RISC machine (because 31 is the difference of two powers of two). P(33) is
       * similarly cheap to calculate, but it's performance is marginally worse, and
       * 33 is composite, which makes me a bit nervous.
       */
      val p = 31
      p * (p + data.hashCode()) + context.hashCode
    }
    /** Needed for correct definition of equals for general classes. */
    def canEqual(that: Any): Boolean = that.isInstanceOf[Static[_]]
    override def toString() = "Static[%s](%s)".format(m.runtimeClass.getName.split("""\.""").last,
      DSLType.convertToString[T](get()).getOrElse(get()))
  }
}
