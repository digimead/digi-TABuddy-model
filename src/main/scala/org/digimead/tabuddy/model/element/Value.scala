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

import java.io.Serializable
import java.net.URI
import java.util.UUID

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.dsl.DSLType.dsltype2implementation
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.serialization.Serialization

import scala.language.implicitConversions

/**
 * Trait that provide general interface for Value implementation
 */
sealed trait Value[T] extends AnyRef with java.io.Serializable {
  /** Value context information */
  val context: Value.Context

  /** Commit complex property (if needed) while saving. */
  def commit(element: Element, transport: Serialization.Transport, elementDirectoryURI: URI)
  /** Copy constructor */
  def copy(context: Value.Context = this.context): this.type
  /** Get value. */
  def get(): T
  /** Class equality with context but without values. */
  def ===(that: Any): Boolean
  /** get() equality. */
  override def equals(that: Any): Boolean =
    (this eq that.asInstanceOf[Object]) || (that match {
      case that: Value[_] ⇒ this.get == that.get
      case _ ⇒ false
    })
  override def hashCode() = context.hashCode
  /** Needed for correct definition of equals for general classes. */
  def canEqual(that: Any): Boolean
}

/**
 * Singleton that contain Static and Dynamic Value implementation.
 */
object Value extends Loggable {
  implicit def byte2someValue(x: Byte)(implicit container: Element = null) = Some(static(Byte.box(x)))
  implicit def byte2value(x: Byte)(implicit container: Element = null) = static(Byte.box(x))
  implicit def double2someValue(x: Double)(implicit container: Element = null) = Some(static(Double.box(x)))
  implicit def double2value(x: Double)(implicit container: Element = null) = static(Double.box(x))
  implicit def float2someValue(x: Float)(implicit container: Element = null) = Some(static(Float.box(x)))
  implicit def float2value(x: Float)(implicit container: Element = null) = static(Float.box(x))
  implicit def int2someValue(x: Int)(implicit container: Element = null) = Some(static(Int.box(x)))
  implicit def int2value(x: Int)(implicit container: Element = null) = static(Int.box(x))
  implicit def long2someValue(x: Long)(implicit container: Element = null) = Some(static(Long.box(x)))
  implicit def long2value(x: Long)(implicit container: Element = null) = static(Long.box(x))
  implicit def short2someValue(x: Short)(implicit container: Element = null) = Some(static(Short.box(x)))
  implicit def short2value(x: Short)(implicit container: Element = null) = static(Short.box(x))
  implicit def bool2someValue(x: Boolean)(implicit container: Element = null) = Some(static(Boolean.box(x)))
  implicit def bool2value(x: Boolean)(implicit container: Element = null) = static(Boolean.box(x))
  implicit def string2someValue(x: String)(implicit container: Element = null) = Some(static(x))
  implicit def string2value(x: String)(implicit container: Element = null) = static(x)
  implicit def value2x[T <: AnyRef with java.io.Serializable](x: Value[T]): T = x.get()

  /**
   * Convert [T] to Value.Static
   */
  def static[T <: AnyRef with java.io.Serializable: Manifest](container: Element, x: T): Static[T] = {
    implicit val e = container
    static(x)
  }
  /**
   * Convert [T] to Value.Static
   */
  def static[T <: AnyRef with java.io.Serializable](x: T)(implicit container: Element = null, m: Manifest[T]): Static[T] =
    if (container == null)
      new Static(x, new Value.Context(None, None))
    else {
      val stack = new Throwable().getStackTrace()
      if (stack.size < 1)
        new Static(x, new Value.Context(Some(container.eObjectId), None))
      else
        new Static(x, container.eModel.contextForChild(container, Some(stack(1))))
    }
  /**
   * Convert () => [T] to Value.Dinamic
   */
  def dinamic[T <: AnyRef with java.io.Serializable: Manifest](container: Element, x: () ⇒ T): Dynamic[T] = {
    implicit val e = container
    dinamic(x)
  }
  /**
   * Convert () => [T] to Value.Dinamic
   */
  def dinamic[T <: AnyRef with java.io.Serializable](x: () ⇒ T)(implicit container: Element = null, m: Manifest[T]): Dynamic[T] =
    if (container == null)
      new Dynamic(x, new Value.Context(None, None))
    else {
      val stack = new Throwable().getStackTrace()
      if (stack.size < 1)
        new Dynamic(x, new Value.Context(Some(container.eObjectId), None))
      else
        new Dynamic(x, container.eModel.contextForChild(container, Some(stack(1))))
    }

  /** Value context information. */
  class Context(
    /** Context object. */
    val objectId: Option[UUID],
    /** Context file line. */
    val line: Option[Int]) extends Equals with java.io.Serializable {

    /** Copy constructor. */
    def copy(objectId: Option[UUID] = this.objectId, line: Option[Int] = this.line) = new Context(objectId, line)

    override def canEqual(that: Any) = that.isInstanceOf[Context]
    override def equals(that: Any): Boolean = (this eq that.asInstanceOf[Object]) || (that match {
      case that: Context if this.## == that.## ⇒ that canEqual this
      case _ ⇒ false
    })
    override lazy val hashCode = java.util.Arrays.hashCode(Array[AnyRef](objectId, line))
    override def toString() = "Context[%s:%s]".format(objectId.getOrElse("-"), line.getOrElse("-"))
  }
  object Context {
    /** Create empty context for unbound value. */
    def apply() = new Value.Context(None, None)
    /** Create context for element. */
    def apply(element: Element) = new Value.Context(Some(element.eObjectId), None)
    /** Create context for element box. */
    def apply(box: ElementBox[_ <: Element]) = new Value.Context(Some(box.elementUniqueId), None)
  }
  /**
   * Dynamic value implementation
   */
  class Dynamic[T <: AnyRef with java.io.Serializable](
    /** Actual value. */
    protected val data: () ⇒ T,
    /** Value context information. */
    val context: Context)(implicit m: Manifest[T]) extends Value[T] {
    assert(m.runtimeClass != classOf[java.io.Serializable], "Unable to create a value for generic type java.io.Serializable")

    /** Commit complex property (if needed) while saving. */
    def commit(element: Element, transport: Serialization.Transport, elementDirectoryURI: URI) =
      DSLType.commit[T](this, element, transport, elementDirectoryURI)
    /** Copy constructor */
    def copy(context: Context = this.context): this.type = (new Dynamic(data, context)).asInstanceOf[this.type]
    /** Get value. */
    def get() = data()
    /** Class equality with context but without values. */
    def ===(that: Any): Boolean = (this eq that.asInstanceOf[Object]) || (that match {
      case that: Dynamic[_] if this.data.getClass == that.data.getClass && this.context == that.context ⇒ (that canEqual this)
      case _ ⇒ false
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

    /** Commit complex property (if needed) while saving. */
    def commit(element: Element, transport: Serialization.Transport, elementDirectoryURI: URI) =
      DSLType.commit[T](this, element, transport, elementDirectoryURI)
    /** Copy constructor */
    def copy(context: Context = this.context): this.type = (new Static(data, context)).asInstanceOf[this.type]
    /** Get value. */
    def get() = data
    /** Class equality with context but without values. */
    def ===(that: Any): Boolean = (this eq that.asInstanceOf[Object]) || (that match {
      case that: Static[_] if this.data.getClass == that.data.getClass && this.context == that.context ⇒ (that canEqual this)
      case _ ⇒ false
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
