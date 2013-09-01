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

package org.digimead.tabuddy.model.graph

import java.net.URI
import java.util.UUID

import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Reference

/**
 * Element/entity context information
 */
class Context(
  /** Container graph origin. */
  val origin: Symbol,
  /** Container node unique. */
  val unique: UUID,
  /** Context file. */
  val file: Option[URI],
  /** Context file line. */
  val line: Option[Int],
  /** Context file digest. */
  val digest: Option[String]) extends Equals {

  /** Copy constructor. */
  def copy(origin: Symbol = this.origin,
    unique: UUID = this.unique,
    file: Option[URI] = this.file,
    line: Option[Int] = this.line,
    digest: Option[String] = this.digest) = new Context(origin, unique, file, line, digest)

  override def canEqual(that: Any) = that.isInstanceOf[Context]
  override def equals(that: Any): Boolean = (this eq that.asInstanceOf[Object]) || (that match {
    case that: Context if this.origin == that.origin && this.unique == that.unique &&
      this.file == that.file && this.line == that.line && this.digest == that.digest => that canEqual this
    case _ => false
  })
  override def hashCode() = Array(origin, unique, file, line, digest).##
  override def toString() = "Context[%s:%s file(%s):%s, line:%s]".format(origin, unique, digest.getOrElse("-"), file.getOrElse("-"), line.getOrElse("-"))
}

/**
 * Companion object for a context information
 */
object Context {
  val emptyUUID = UUID.fromString("00000000-0000-0000-0000-000000000000")
  /** Create context information. */
  def apply(container: Element): Context = apply(container, None, None, None)
  /** Create context information. */
  def apply(container: Element, file: Option[URI], line: Option[Int], digest: Option[String]): Context =
    new Context(container.eStash.origin, container.eUnique, file, line, digest)
  /** Create context information. */
  def apply(reference: Reference): Context = apply(reference, None, None, None)
  /** Create context information. */
  def apply(reference: Reference, file: Option[URI], line: Option[Int], digest: Option[String]): Context =
    new Context(reference.origin, reference.unique, file, line, digest)
  /** Create context information. */
  def apply(origin: Symbol, unique: UUID): Context = apply(origin, unique, None, None, None)
  /** Create context information. */
  def apply(origin: Symbol, unique: UUID, file: Option[URI], line: Option[Int], digest: Option[String]): Context =
    new Context(origin, unique, file, line, digest)
  /** Create context information */
  def apply(): Context = new Context(Symbol(""), emptyUUID, None, None, None)
}
