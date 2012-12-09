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

import scala.Array.canBuildFrom

import org.digimead.digi.lib.aop.log
import org.digimead.digi.lib.log.Loggable
import org.digimead.digi.lib.log.logger.RichLogger.rich2slf4j
import org.digimead.tabuddy.model.Element.Context
import org.digimead.tabuddy.model.Element.Coordinate
import org.digimead.tabuddy.model.Model.model2implementation
import org.digimead.tabuddy.model.Stash.Data
import org.digimead.tabuddy.model.Value.serializable2value

/**
 * Class that implements Record interface.
 */
class Record[A <: Record.Stash](@volatile var stash: A) extends Record.Interface[A]

/**
 * Record companion object that provide ability
 * to build a new record
 * to get the record with specific coordinate/origin
 * to store the records in index
 */
object Record extends Loggable {
  /**
   * Create an element with standard Record class
   */
  def apply[T](container: Element.Generic, id: Symbol, rawCoordinate: Seq[Element.Axis[_ <: java.io.Serializable]], f: (Record[Stash]) => T): Record[Stash] =
    apply(classOf[Record[Stash]], classOf[Record.Stash], container, id, rawCoordinate, f)
  /**
   * Get exists or create a new record.
   * a bit complicated type ;-)
   * A <: Stash - element stash
   * B <: Interface[A] - we accept only subclass of Record.Interface with particular stash
   * C - user type, that returned by f()
   * D >: B <: B - is exactly as B, not ancestor, not descendant
   */
  @log
  def apply[A <: Stash, B <: Interface[A], C, D >: B <: B](elementClass: Class[B], stashClass: Class[A], container: Element.Generic, id: Symbol,
    rawCoordinate: Seq[Element.Axis[_ <: java.io.Serializable]], f: (D) => C)(implicit a: Manifest[A], d: Manifest[D]): D = {
    val coordinate = Element.Coordinate(rawCoordinate: _*)
    // poor Scala compiler, it is going crazy...
    val storedElement = container.find[D, A](id, coordinate)
    val (result, stored) = storedElement match {
      case Some(record) if record.canEqual(elementClass, stashClass) =>
        // return exists element only if required element class is the same as stored element class
        (Some(record.asInstanceOf[B]), Some(record.asInstanceOf[B]))
      case Some(record) =>
        (None, Some(record.asInstanceOf[B]))
      case _ =>
        (None, None)
    }
    result getOrElse {
      // create new element
      val messagePrefix = if (stored.isEmpty)
        "create new element "
      else
        "replace old element %s with ".format(stored.get.stash.scope)
      val location = (new Throwable).getStackTrace().find(t =>
        t.getFileName() == "(inline)" && t.getMethodName() == "apply")
      val context = Model.contextForChild(container, location)
      val unique = container.children.find(_.id == id).map(_.unique).getOrElse(UUID.randomUUID)
      val stashCtor = stashClass.getConstructor(classOf[Element.Context], classOf[Element.Coordinate],
        classOf[Symbol], classOf[UUID], classOf[Option[_]])
      val stash = stashCtor.newInstance(context, coordinate, id, unique, None)
      val elementCtor = elementClass.getConstructor(a.erasure)
      val element = elementCtor.newInstance(stash)
      log.debug(messagePrefix + element)
      container.stash.model match {
        case Some(model) => model.eAttach(container, element)
        case None =>
          log.debug("add %s to %s".format(element, container))
          container.children = container.children :+ element
      }
      f(element)
      element
    }
  }
  /**
   * General Record interface.
   */
  trait Interface[A <: Record.Stash] extends Element[A, Interface[A]] {
    def description = getOrElseRoot[String]('description).map(_.get) getOrElse ""
    def description_=(value: String) = set('description, value)
    def dump(padding: Int = 2): String = {
      val pad = " " * padding
      val self = if (description.isEmpty)
        "%s: %s".format(stash.scope, stash.id)
      else
        "%s: %s \"%s\"".format(stash.scope, stash.id, description)
      val childrenDump = children.map(_.dump(padding)).mkString("\n").split("\n").map(pad + _).mkString("\n").trim
      if (childrenDump.isEmpty) self else self + "\n" + pad + childrenDump
    }
    def getOrElseRoot[A <: java.io.Serializable](id: Symbol)(implicit m: Manifest[A]): Option[Value[A]] = get[A](id: Symbol) orElse {
      if (stash.coordinate.isRoot)
        // we are already at root but value is absent
        None
      else
        // try to find value at root node
        root.flatMap(root => root.get[A](id))
    }
    /** Get the root element from the current origin if any. */
    def root() = if (stash.coordinate.isRoot) Some(this) else model.e(stash.unique, Element.Coordinate.root).asInstanceOf[Option[Interface[A]]]
    /** Get the root element from the particular origin if any */
    def root(origin: Symbol) = model.e(origin, stash.unique, Element.Coordinate()).asInstanceOf[Option[Interface[A]]]
  }
  /**
   * Part of DSL.Builder for end user
   */
  trait DSL {
    this: DSL.Builder =>
    /**
     * create new or retrieve exists record
     */
    def record[T](id: Symbol, coordinate: Element.Axis[_ <: java.io.Serializable]*)(f: Record[Stash] => T): Record[Stash] =
      Record.apply(DLS_element, id, coordinate, f)
  }
  /**
   * Record specific stash realization
   */
  class Stash(val context: Element.Context, val coordinate: Element.Coordinate,
    val id: Symbol, val unique: UUID, var model: Option[Model.Interface],
    val property: org.digimead.tabuddy.model.Stash.Data)
    extends org.digimead.tabuddy.model.Stash {
    def this(context: Element.Context, coordinate: Element.Coordinate, id: Symbol, unique: UUID, model: Option[Model.Interface]) =
      this(context, coordinate, id, unique, model, new org.digimead.tabuddy.model.Stash.Data)
    val scope: String = "Record"

    /** Copy constructor */
    override def copy(context: Element.Context = this.context,
      coordinate: Element.Coordinate = this.coordinate,
      id: Symbol = this.id,
      scope: String = this.scope,
      unique: UUID = this.unique,
      model: Option[Model.Interface] = this.model,
      lastModification: Long = this.lastModification,
      property: org.digimead.tabuddy.model.Stash.Data = this.property) = {
      assert(scope == this.scope, "incorrect scope %s, must be %s".format(scope, this.scope))
      val newStashCtor = this.getClass().getConstructor(classOf[Element.Context], classOf[Element.Coordinate],
        classOf[Symbol], classOf[UUID], classOf[Option[_]], classOf[org.digimead.tabuddy.model.Stash.Data])
      val newStash = newStashCtor.newInstance(context, coordinate, id, unique, model, property)
      newStash.lastModification = lastModification
      newStash
    }
  }
}
