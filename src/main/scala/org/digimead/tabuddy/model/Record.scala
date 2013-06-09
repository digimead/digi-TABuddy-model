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

package org.digimead.tabuddy.model

import java.util.UUID

import org.digimead.digi.lib.aop.log
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model.model2implementation
import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.element.Context
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Element.Timestamp
import org.digimead.tabuddy.model.element.LocationGeneric
import org.digimead.tabuddy.model.element.Stash.Data
import org.digimead.tabuddy.model.element.Value
import org.digimead.tabuddy.model.element.Value.string2someValue

/**
 * Class that implements Record interface.
 */
class Record[A <: Record.Stash](private var stashArg: A) extends Record.Interface[A] {
  /** Copy constructor */
  override def eCopy(stash: Option[A], children: Option[List[Element.Generic]]): this.type = {
    val result = super.eCopy(stash, children)
    val iterator = result.eChildren.iteratorRecursive()
    iterator.foreach(element => element.eStash.model = None)
    result
  }
  /** Get current stash */
  def eStash: A = stashArg
  /** Stub for setter of model's data */
  def eStash_=(value: A): Unit = {
    val oldValue = stashArg
    stashArg = value
    val undoF = () => {}
    Element.Event.publish(Element.Event.StashReplace(this, oldValue, value, this.eModified)(undoF))
  }

  /** create new instance with specific stash */
  protected def eNewInstance(stash: A): this.type = new Record(stash).asInstanceOf[this.type]
}

/**
 * Record companion object that provide ability
 * to build a new record
 * to get the record with specific coordinate/origin
 * to store the records in index
 */
object Record extends Loggable {
  type Generic = Interface[_ <: Stash]
  val scope = new Scope()

  /**
   * Create a detached element with the standard Record class
   */
  def apply[T](id: Symbol, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]]): Record[Stash] =
    apply(id, rawCoordinate, (f: Record[Stash]) => {})
  /**
   * Create a detached element with the standard Record class
   */
  def apply[T](id: Symbol, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]], f: Record[Stash] => T): Record[Stash] =
    apply(classOf[Record[Stash]], classOf[Record.Stash], None, id, Record.scope, rawCoordinate, f)
  /**
   * Create a detached element with the standard Record class
   */
  def apply[T](id: Symbol, scope: Record.Scope, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]]): Record[Stash] =
    apply(classOf[Record[Stash]], classOf[Record.Stash], None, id, scope, rawCoordinate, (f: Record[Stash]) => {})
  /**
   * Create a detached element with the standard Record class
   */
  def apply[T](id: Symbol, scope: Record.Scope, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]], f: Record[Stash] => T): Record[Stash] =
    apply(classOf[Record[Stash]], classOf[Record.Stash], None, id, scope, rawCoordinate, f)
  /**
   * Get exists or create an attached element with the standard Record class
   */
  def apply[T](container: Element.Generic, id: Symbol, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]]): Record[Stash] =
    apply(container, id, rawCoordinate, (f: Record[Stash]) => {})
  /**
   * Get exists or create an attached element with the standard Record class
   */
  def apply[T](container: Element.Generic, id: Symbol, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]], f: Record[Stash] => T): Record[Stash] =
    apply(classOf[Record[Stash]], classOf[Record.Stash], Some(container), id, Record.scope, rawCoordinate, f)
  /**
   * Get exists or create an attached element with the standard Record class
   */
  def apply[T](container: Element.Generic, id: Symbol, scope: Record.Scope, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]]): Record[Stash] =
    apply(classOf[Record[Stash]], classOf[Record.Stash], Some(container), id, scope, rawCoordinate, (f: Record[Stash]) => {})
  /**
   * Get exists or create an attached element with the standard Record class
   */
  def apply[T](container: Element.Generic, id: Symbol, scope: Record.Scope, rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]], f: Record[Stash] => T): Record[Stash] =
    apply(classOf[Record[Stash]], classOf[Record.Stash], Some(container), id, scope, rawCoordinate, f)
  /**
   * Get exists or create a new record.
   * a bit complicated type ;-)
   * A <: Stash - the element stash
   * B <: Interface[A] - we accept only a subclass of Record.Interface with the particular stash
   * C - a user type, that is returned by f()
   * D >: B <: B - is exactly as B, not ancestor, not descendant
   */
  def apply[A <: Stash, B <: Interface[A], C, D >: B <: B](elementClass: Class[B], stashClass: Class[A], container: Option[Element.Generic], id: Symbol, scope: Element.Scope,
    rawCoordinate: Seq[Axis[_ <: AnyRef with java.io.Serializable]], f: (D) => C)(implicit a: Manifest[A], d: Manifest[D]): D = synchronized {
    val coordinate = Coordinate(rawCoordinate: _*)
    // poor Scala compiler, it is going crazy...
    val storedElement = container.flatMap(_.eFind[D, A](id, coordinate))
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

      val unique = container.flatMap(_.eChildren.find(_.eId == id)).map(_.eUnique).getOrElse(UUID.randomUUID)
      val context = container match {
        case Some(container) => Model.contextForChild(container, location)
        case None => Context.empty()
      }
      val stashCtor = stashClass.getConstructor(
        classOf[Context],
        classOf[Coordinate],
        classOf[Element.Timestamp],
        classOf[Symbol],
        classOf[Element.Scope],
        classOf[UUID],
        classOf[org.digimead.tabuddy.model.element.Stash.Data])
      val stash = stashCtor.newInstance(context, coordinate, Element.timestamp(), id, scope, unique, new org.digimead.tabuddy.model.element.Stash.Data)
      val elementCtor = elementClass.getConstructor(a.runtimeClass)
      val element = elementCtor.newInstance(stash)
      log.debug(messagePrefix + element)
      log.debug("add %s to %s".format(element, container))
      container.foreach(_.eChildren += element)
      element
    }
    f(element)
    element
  }
  /**
   * General Record interface.
   */
  trait Interface[StashProjection <: Record.Stash] extends Element[StashProjection] {
    def name = eGetOrElseRoot[String]('name).map(_.get) getOrElse ""
    def name_=(value: String) = eSet('name, value, "")
    def eDump(brief: Boolean, padding: Int = 2): String = {
      def dumpProperties() = {
        val result = eStash.property.map {
          case (id, sequence) =>
            sequence.map {
              case (typeSymbol, value) =>
                "%s: %s".format(id, value)
            }
        }.flatten
        if (result.nonEmpty) "\n  " + result.toSeq.sorted.mkString("\n  ") else ""
      }
      val pad = " " * padding
      val properties = if (brief) "" else dumpProperties()
      val self = if (name.isEmpty)
        "%s: %s".format(eStash.scope, eStash.id) + properties
      else
        "%s: %s \"%s\"".format(eStash.scope, eStash.id, name) + properties
      val childrenDump = eChildren.map(_.eDump(brief, padding)).mkString("\n").split("\n").map(pad + _).mkString("\n").trim
      if (childrenDump.isEmpty) self else self + "\n" + pad + childrenDump
    }
    def eGetOrElseRoot(id: Symbol, typeSignature: Symbol): Option[Value[_ <: AnyRef with java.io.Serializable]] =
      eGet(id, typeSignature) orElse {
        if (eStash.coordinate.isRoot)
          // we are already at root but value is absent
          None
        else
          // try to find value at root node
          eRoot.flatMap(_.eGet(id, typeSignature))
      }
    /** Get the root element from the current origin if any. */
    def eRoot() = (if (eStash.coordinate.isRoot) Some(this) else eModel.e(eStash.unique, Coordinate.root)).asInstanceOf[Option[this.type]]
    /** Get the root element from the particular origin if any */
    def eRoot(origin: Symbol) = eModel.e(origin, eStash.unique, Coordinate()).asInstanceOf[Option[this.type]]
  }
  /**
   * Part of DSL.Builder for end user
   */
  trait DSL {
    this: org.digimead.tabuddy.model.dsl.DSL[_] =>
    case class RecordLocation(override val id: Symbol,
      override val coordinate: Coordinate = Coordinate.root)
      extends LocationGeneric[Record[Stash], Stash](id, Record.scope, coordinate)
  }
  object DSL {
    trait RichElement {
      this: org.digimead.tabuddy.model.dsl.DSL.RichElement =>
      /**
       * Create new or retrieve exists record
       */
      def record[T](id: Symbol, coordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(f: Record[Stash] => T): Record[Stash] =
        Record.apply(DLS_element, id, coordinate, f)
      def toRecord() = DLS_element.eAs[Record[Stash], Stash]
    }
  }
  /** The marker object that describes record scope */
  class Scope(override val modificator: Symbol = 'Record) extends Element.Scope(modificator) {
    def canEqual(other: Any): Boolean = other.isInstanceOf[org.digimead.tabuddy.model.Record.Scope]
  }
  /**
   * Record specific stash realization
   */
  class Stash(val context: Context, val coordinate: Coordinate, val created: Element.Timestamp, val id: Symbol, val scope: Element.Scope,
    val unique: UUID, val property: org.digimead.tabuddy.model.element.Stash.Data) extends org.digimead.tabuddy.model.element.Stash {

    /** Copy constructor */
    def copy(context: Context = this.context,
      coordinate: Coordinate = this.coordinate,
      created: Element.Timestamp = this.created,
      id: Symbol = this.id,
      modified: Element.Timestamp = this.modified,
      scope: Element.Scope = this.scope,
      unique: UUID = this.unique,
      model: Option[Model.Generic] = this.model,
      property: org.digimead.tabuddy.model.element.Stash.Data = this.property) = {
      assert(scope == this.scope, "incorrect scope %s, must be %s".format(scope, this.scope))
      val newStashCtor = this.getClass().getConstructor(
        classOf[Context],
        classOf[Coordinate],
        classOf[Element.Timestamp],
        classOf[Symbol],
        classOf[Element.Scope],
        classOf[UUID],
        classOf[org.digimead.tabuddy.model.element.Stash.Data])
      val data = new org.digimead.tabuddy.model.element.Stash.Data
      copyDeepProperty(property, data)
      val newStash = newStashCtor.newInstance(context, coordinate, created, id, scope, unique, data)
      newStash.model = model
      newStash.modified = modified
      newStash.asInstanceOf[this.type]
    }
  }
}
