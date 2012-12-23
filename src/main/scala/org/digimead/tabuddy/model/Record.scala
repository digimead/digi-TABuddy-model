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
class Record[A <: Record.Stash](stashArg: A) extends Record.Interface[A] {
  /** Get current stash */
  def eStash: A = stashArg
  /** Stub for setter of model's data */
  def eStash_=(value: A): Unit = {}
}

/**
 * Record companion object that provide ability
 * to build a new record
 * to get the record with specific coordinate/origin
 * to store the records in index
 */
object Record extends Loggable {
  type Generic = Interface[_ <: Stash]
  
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
    val storedElement = container.eFind[D, A](id, coordinate)
    val (result, stored) = storedElement match {
      case Some(record) if record.canEqual(elementClass, stashClass) =>
        // return exists element only if required element class is the same as stored element class
        (Some(record.asInstanceOf[B]), Some(record.asInstanceOf[B]))
      case Some(record) =>
        (None, Some(record.asInstanceOf[B]))
      case _ =>
        (None, None)
    }
    val element = result getOrElse {
      // create new element
      val messagePrefix = if (stored.isEmpty)
        "create new element "
      else
        "replace old element %s with ".format(stored.get.eStash.scope)
      val location = (new Throwable).getStackTrace().find(t =>
        t.getFileName() == "(inline)" && t.getMethodName() == "apply")
      val context = Model.contextForChild(container, location)
      val unique = container.elementChildren.find(_.eId == id).map(_.eUnique).getOrElse(UUID.randomUUID)
      val stashCtor = stashClass.getConstructor(
        classOf[Element.Context],
        classOf[Element.Coordinate],
        classOf[Stash.Timestamp],
        classOf[Symbol],
        classOf[UUID],
        classOf[org.digimead.tabuddy.model.Stash.Data])
      val stash = stashCtor.newInstance(context, coordinate, Stash.timestamp, id, unique, new org.digimead.tabuddy.model.Stash.Data)
      val elementCtor = elementClass.getConstructor(a.erasure)
      val element = elementCtor.newInstance(stash)
      log.debug(messagePrefix + element)
      container.eStash.model match {
        case Some(model) => model.eAttach(container, element)
        case None =>
          log.debug("add %s to %s".format(element, container))
          container.elementChildren += element
      }
      element
    }
    f(element)
    element
  }
  /**
   * General Record interface.
   */
  trait Interface[StashProjection <: Record.Stash] extends Element[StashProjection] {
    def description = eGetOrElseRoot[String]('description).map(_.get) getOrElse ""
    def description_=(value: String) = eSet('description, value)
    def eDump(padding: Int = 2): String = {
      val pad = " " * padding
      val self = if (description.isEmpty)
        "%s: %s".format(eStash.scope, eStash.id)
      else
        "%s: %s \"%s\"".format(eStash.scope, eStash.id, description)
      val childrenDump = elementChildren.map(_.eDump(padding)).mkString("\n").split("\n").map(pad + _).mkString("\n").trim
      if (childrenDump.isEmpty) self else self + "\n" + pad + childrenDump
    }
    def eGetOrElseRoot[A <: java.io.Serializable](id: Symbol)(implicit m: Manifest[A]): Option[Value[A]] = eGet[A](id: Symbol) orElse {
      if (eStash.coordinate.isRoot)
        // we are already at root but value is absent
        None
      else
        // try to find value at root node
        eRoot.flatMap(root => root.eGet[A](id))
    }
    /** Get the root element from the current origin if any. */
    def eRoot() = (if (eStash.coordinate.isRoot) Some(this) else eModel.e(eStash.unique, Element.Coordinate.root)).asInstanceOf[Option[this.type]]
    /** Get the root element from the particular origin if any */
    def eRoot(origin: Symbol) = eModel.e(origin, eStash.unique, Element.Coordinate()).asInstanceOf[Option[this.type]]
  }
  /**
   * Part of DSL.Builder for end user
   */
  trait DSL {
    this: org.digimead.tabuddy.model.DSL[_] =>
    case class RecordLocation(override val id: Symbol,
      override val coordinate: Element.Coordinate = Element.Coordinate.root)
      extends Element.GenericLocation[Record[Stash], Stash](id, coordinate)
  }
  object DSL {
    trait RichElement {
      this: org.digimead.tabuddy.model.DSL.RichElement =>
      /**
       * Create new or retrieve exists record
       */
      def record[T](id: Symbol, coordinate: Element.Axis[_ <: java.io.Serializable]*)(f: Record[Stash] => T): Record[Stash] =
        Record.apply(DLS_element, id, coordinate, f)
      def toRecord() = DLS_element.eAs[Record[Stash], Stash]
    }
  }
  /**
   * Record specific stash realization
   */
  class Stash(val context: Element.Context, val coordinate: Element.Coordinate, val created: Stash.Timestamp, val id: Symbol,
    val unique: UUID, val property: org.digimead.tabuddy.model.Stash.Data) extends org.digimead.tabuddy.model.Stash {
    val scope: String = "Record"

    /** Copy constructor */
    def copy(context: Element.Context = this.context,
      coordinate: Element.Coordinate = this.coordinate,
      created: Stash.Timestamp = this.created,
      id: Symbol = this.id,
      modified: Stash.Timestamp = this.modified,
      scope: String = this.scope,
      unique: UUID = this.unique,
      model: Option[Model.Generic] = this.model,
      property: org.digimead.tabuddy.model.Stash.Data = this.property) = {
      assert(scope == this.scope, "incorrect scope %s, must be %s".format(scope, this.scope))
      val newStashCtor = this.getClass().getConstructor(
        classOf[Element.Context],
        classOf[Element.Coordinate],
        classOf[Stash.Timestamp],
        classOf[Symbol],
        classOf[UUID],
        classOf[org.digimead.tabuddy.model.Stash.Data])
      val newStash = newStashCtor.newInstance(context, coordinate, created, id, unique, property)
      newStash.model = model
      newStash.modified = modified
      newStash
    }
  }
}
