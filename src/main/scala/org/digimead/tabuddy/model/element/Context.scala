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
  override def hashCode() = List(container, file, line, digest).foldLeft(0)((a, b) => a * 31 + b.hashCode())
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
