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

import scala.language.implicitConversions

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.dsl.DSLType.dsltype2implementation
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.serialization.transport.Transport

/**
 * Trait that provide general interface for Value implementation
 */
trait Value[T] extends Equals with java.io.Serializable {
  /** Commit complex property (if needed) while saving. */
  def commit(element: Element, transport: Transport, elementDirectoryURI: URI)
  /** Get value. */
  def get(): T
  /** Value equality. */
  def ===(that: Any): Boolean = that match {
    case that: Value[_] ⇒ this.get == that.get
    case _ ⇒ false
  }
  /** Equality. */
  override def equals(that: Any): Boolean = that match {
    case that: Value[_] ⇒ (this eq that) ||
      (this.canEqual(that) && that.canEqual(this) && this.## == that.##)
    case _ ⇒ false
  }
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
  def static[T <: AnyRef with java.io.Serializable: Manifest](arg: T): Static[T] = new Static[T](arg)
  /**
   * Convert () => [T] to Value.Dinamic
   */
  def dinamic[T <: AnyRef with java.io.Serializable: Manifest](arg: () ⇒ T): Dynamic[T] = new Dynamic[T](arg)
  /**
   * Dynamic value implementation.
   *
   * @param data actual value
   */
  class Dynamic[T <: AnyRef with java.io.Serializable](protected val data: () ⇒ T)(implicit m: Manifest[T]) extends Value[T] {
    assert(m.runtimeClass != classOf[java.io.Serializable], "Unable to create a value for generic type java.io.Serializable")

    /** Commit complex property (if needed) while saving. */
    def commit(element: Element, transport: Transport, elementDirectoryURI: URI) =
      DSLType.commit[T](this, element, transport, elementDirectoryURI)
    /** Get value. */
    def get() = data()

    /** Needed for correct definition of equals for general classes. */
    def canEqual(that: Any): Boolean = that.isInstanceOf[Dynamic[_]]
    override def hashCode() = lazyHashCode
    protected lazy val lazyHashCode = data.##
    override def toString() = "Dynamic[%s](%s)".format(m.runtimeClass.getName.split("""\.""").last,
      DSLType.convertToString[T](get()).getOrElse(get()))
  }
  /**
   * Static value implementation.
   *
   * @param data initial value
   */
  class Static[T <: AnyRef with java.io.Serializable](protected val data: T)(implicit m: Manifest[T]) extends Value[T] {
    assert(m.runtimeClass != classOf[java.io.Serializable], "Unable to create a value for generic type java.io.Serializable")

    /** Commit complex property (if needed) while saving. */
    def commit(element: Element, transport: Transport, elementDirectoryURI: URI) =
      DSLType.commit[T](this, element, transport, elementDirectoryURI)
    /** Get value. */
    def get() = data
    /** Needed for correct definition of equals for general classes. */
    def canEqual(that: Any): Boolean = that.isInstanceOf[Static[_]]
    override def hashCode() = lazyHashCode
    protected lazy val lazyHashCode = data.##
    override def toString() = "Static[%s](%s)".format(m.runtimeClass.getName.split("""\.""").last,
      DSLType.convertToString[T](get()).getOrElse(get()))
  }
}
