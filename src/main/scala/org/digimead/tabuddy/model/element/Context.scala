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

import java.io.File
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.Model.model2implementation
import java.util.UUID

/**
 * Element/entity context information
 */
case class Context(
  /** Container reference. */
  container: Reference,
  /** Context file. */
  file: Option[File],
  /** Context file line. */
  line: Option[Int],
  /** Context file digest. */
  digest: Option[String]) {
  override def equals(that: Any): Boolean = (this eq that.asInstanceOf[Object]) || (that match {
    case that: Context if this.container == that.container &&
      this.file == that.file && this.line == that.line && this.digest == that.digest => that canEqual this
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
    p * (p * (p * (p + container.hashCode()) + file.hashCode()) + line.hashCode()) + digest.hashCode()
  }
  override def toString() = "Context(%s file(%s):%s, line:%s)".format(container, digest.getOrElse("-"), file.getOrElse("-"), line.getOrElse("-"))
}

/**
 * Companion object for a context information
 */
object Context {
  val emptyUUID = UUID.fromString("00000000-0000-0000-0000-000000000000")
  /** Provide context information for REPL/new entity. */
  def virtual(container: Element.Generic): Context =
    Context(Reference(Model.origin, container.eUnique, container.eCoordinate), None, None, None)
  /** Provide context information for REPL/new entity. */
  def virtual(id: UUID, coordinate: Coordinate): Context =
    Context(Reference(Model.origin, id, coordinate), None, None, None)
  /** Provide context information for REPL/new entity */
  def empty(): Context =
    Context(Reference(Model.origin, emptyUUID, Coordinate.root), None, None, None)
}
