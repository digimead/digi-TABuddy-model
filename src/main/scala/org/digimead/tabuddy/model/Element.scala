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

import java.io.File
import java.util.UUID

import scala.Option.option2Iterable
import scala.annotation.tailrec
import scala.collection.mutable

import org.digimead.digi.lib.log.Loggable
import org.digimead.digi.lib.log.logger.RichLogger.rich2slf4j
import org.digimead.tabuddy.model.Model.model2implementation

/**
 * Base element
 * builded with the curiously recurring generic pattern
 * contains stash with actual data.
 */
trait Element[StashProjection <: Stash] extends Loggable with Ordered[Element.Generic] with java.io.Serializable {
  /** Child elements */
  val elementChildren = new Element.Children
  /** Weak reference for implicit conversion */
  @transient implicit lazy val elementRef = new scala.ref.WeakReference[this.type](this)
  /** Element global modification time, based on children state. */
  @volatile protected var elementModified: Stash.Timestamp = eStash.modified
  log.debug(this + " alive")

  /** Compares this object with the specified object for order. */
  def compare(that: Element.Generic): Int =
    this.elementModified.milliseconds compare that.elementModified.milliseconds match {
      case 0 => this.elementModified.nanoShift compare that.elementModified.nanoShift
      case c => c
    }
  /** As instance of for Element.Generic */
  def eAs[A <: Element[B], B <: Stash]()(implicit ma: Manifest[A], mb: Manifest[B], mt: Manifest[this.type], ms: Manifest[StashProjection]): Option[A] =
    if (ma <:< mt && mb <:< ms) Some(this.asInstanceOf[A]) else None
  /** Get element id */
  def eCoordinate() = eStash.coordinate
  /** Copy constructor */
  def eCopy(stash: Stash = null, children: List[Element.Generic] = null): this.type = {
    val elementStash = Option(stash).getOrElse(this.eStash.copy())
    val elementCtor = this.getClass.getConstructor(this.eStash.getClass())
    val element = elementCtor.newInstance(elementStash).asInstanceOf[this.type]
    val elementChildren = Option(children).getOrElse(this.elementChildren).map(_.eCopy())
    element.elementChildren ++= elementChildren
    element
  }
  /** Dump the element content */
  def eDump(padding: Int): String
  /** Get filtered child elements from subtree */
  def eFilter(filter: (Element.Generic) => Boolean): Seq[Element.Generic] =
    this.eFilter(Seq(this), filter, Seq())
  /**
   * Find child element
   * @param A - element class
   * @param B - stash class
   */
  def eFind[A <: Element[B], B <: Stash](id: Symbol, coordinate: Element.Axis[_ <: java.io.Serializable]*)(implicit a: Manifest[A], b: Manifest[B]): Option[A] = eFind[A, B](id, Element.Coordinate(coordinate: _*))
  /**
   * Find child element
   * @param A - element class
   * @param B - stash class
   */
  def eFind[A <: Element[B], B <: Stash](id: Symbol, coordinate: Element.Coordinate)(implicit a: Manifest[A], b: Manifest[B]): Option[A] = {
    elementChildren.find { element =>
      element.eStash.id == id && element.eStash.coordinate == coordinate
    } match {
      case e @ Some(element) if element.canEqual(a.erasure, b.erasure) => e.asInstanceOf[Option[A]]
      case _ => None
    }
  }
  /** Get a property. */
  def eGet[A <: java.io.Serializable](id: Symbol)(implicit m: Manifest[A]): Option[Value[A]] =
    eStash.property.get(m.erasure.getName()).flatMap(_.get(id)).asInstanceOf[Option[Value[A]]]
  /** Get a property or else get the property from the root element. */
  def eGetOrElseRoot[A <: java.io.Serializable](id: Symbol)(implicit m: Manifest[A]): Option[Value[A]]
  /** Get element id */
  def eId() = eStash.id
  /** Get reference of this element */
  def eModel: Model.Generic = {
    eStash.model getOrElse {
      throw new RuntimeException("Model undefined for detached element " + this)
    }
  }
  /** set new Model for element */
  def eModel_=(value: Model.Generic): Unit = {
    // prevent against null, which may occur at initialization
    eStash.model //.removeElement(this)
    value.eRegister(this)
    eStash.model = Some(value)
  }
  def eReference() = Element.Reference(eStash.context.container.origin, eStash.unique, eStash.coordinate)
  /** Get the root element from the current origin if any. */
  def eRoot(): Option[this.type]
  /** Get the root element from the particular origin if any */
  def eRoot(origin: Symbol): Option[this.type]
  /** Set a new property, return an old property */
  def eSet[A <: java.io.Serializable](id: Symbol, value: Option[Value[A]])(implicit m: Manifest[A]): Option[Value[A]] = {
    eModify()
    value match {
      case Some(value) =>
        eStash.property.get(m.erasure.getName()) match {
          case Some(hash) =>
            val previous = hash.get(id)
            hash(id) = value
            previous.asInstanceOf[Option[Value[A]]]
          case None =>
            val hash = new mutable.HashMap[Symbol, Value[_ <: java.io.Serializable]]() with mutable.SynchronizedMap[Symbol, Value[_ <: java.io.Serializable]]
            eStash.property(m.erasure.getName()) = hash
            hash(id) = value
            None
        }
      case None =>
        eStash.property.get(m.erasure.getName()) match {
          case Some(hash) =>
            val previous = hash.get(id)
            hash.remove(id)
            previous.asInstanceOf[Option[Value[A]]]
          case None =>
            None
        }
    }
  }
  /** Get current stash */
  def eStash: StashProjection
  /** Stub for setter of model's data */
  def eStash_=(value: StashProjection): Unit
  /** Get element id */
  def eUnique() = eStash.unique

  /** Collect all sub elements that conform a user filter */
  @tailrec
  final protected def eFilter(e: Seq[Element.Generic], filter: (Element.Generic) => Boolean,
    acc: Seq[Element.Generic]): Seq[Element.Generic] = {
    val children = e.map(_.elementChildren).flatten.filter(filter)
    if (children.isEmpty) return acc
    this.eFilter(children, filter, acc ++ children)
  }
  /** Set new modification time, push notification */
  protected def eModify() {
    log.___glance("!!!")
    val ts = Stash.timestamp
    elementModified = ts
    eStash.modified = ts
    // notify
  }

  /** Needed for correct definition of equals for general classes. */
  def canEqual(thatElementClass: Class[_], thatStashClass: Class[_]): Boolean =
    this.getClass() == thatElementClass && eStash.getClass() == thatStashClass
  /** Indicates whether some other element is "equal to" this one. */
  override def equals(that: Any): Boolean =
    (this eq that.asInstanceOf[Object]) || (that match {
      case that: Element[_] =>
        // 1. can equal
        this.canEqual(that.getClass, that.eStash.getClass) &&
          // 2. immutable variables are identical
          this.hashCode == that.hashCode &&
          // 3. mutable variables are identical
          this.elementModified == that.elementModified
      case _ => false
    })
  /** Returns a hash code value for the object. */
  override def hashCode() = List(getClass, eStash).foldLeft(0)((a, b) => a * 31 + b.hashCode())
  override def toString() = "%s[%s] at %s".format(eStash.scope, eStash.id.toString, eStash.coordinate.toString)
}

/**
 * Companion object for the base element
 * contains axis and coordinate definition.
 */
object Element extends Loggable {
  type Generic = Element[_ <: Stash]
  /**
   * Elements and it derivative classes default ordering
   */
  implicit def orderingByModification[T <: Element.Generic]: Ordering[T] =
    new Ordering[T] { def compare(x: T, y: T): Int = x.compare(y) }

  /**
   * Check new/exists/modified stash against neighbor
   */
  def check(element: Element.Generic, stash: Stash) {
    stash.model match {
      case Some(model) =>
        model.e(stash.context.container).map(_.elementChildren.filter(_.eId == stash.id)).flatten.foreach {
          nighborWithSameID =>
            val neighborStash = nighborWithSameID.eStash.asInstanceOf[Stash]
            assert(nighborWithSameID.eUnique == stash.unique, "Illegal %s. %s MUST be the same as id %s of neighbor %s.".
              format(element, stash.unique, nighborWithSameID.eUnique, nighborWithSameID))
            assert(neighborStash.coordinate != stash.coordinate, "Illegal %s. There is already neighbor %s exists with same coordinate.".
              format(element, nighborWithSameID))
            assert(nighborWithSameID.canEqual(element.getClass(), stash.getClass()), "Illegal %s. There is already neighbor %s exists and it has different type.".
              format(element, nighborWithSameID))
        }
        assert(model.e(element.eReference).isEmpty, "Illegal %s. The element with reference %s is already attached.".format(element, element.eReference))
      case None =>
        assert(false, "unable to check %s against stash with undefined model".format(element))
    }
  }
  /** Provide context information for REPL/new entity. */
  def virtualContext(container: Generic): Element.Context =
    Element.Context(Reference(Model.eStash.id, container.eUnique, container.eCoordinate), None, None, None)

  /** Axis/tag value of coordinate. */
  case class Axis[T <: java.io.Serializable](
    /** Axis user id. */
    val id: Symbol,
    /** Axis value. */
    val value: T)
  /**
   * Companion object for an axis
   * that contains implicit conversions between primitive and axis.
   */
  object Axis {
    implicit def intToAxis(x: (Symbol, Int)): Axis[java.lang.Integer] = Axis[java.lang.Integer](x._1, Int.box(x._2))
  }
  /**
   * Element children container
   */
  class Children extends mutable.ArrayBuffer[Element.Generic] with history.ObservableElementBuffer[Element.Generic] with mutable.SynchronizedBuffer[Element.Generic]
  /** Contain list of an axis values. */
  class Coordinate private (
    /** List of an axis values. */
    val coordinate: List[Axis[_ <: java.io.Serializable]]) extends java.io.Serializable {
    /** Check if coordinate at the root point. */
    def isRoot() = coordinate.isEmpty
    /** Visual coordinate representation. */
    override def toString() = if (coordinate.nonEmpty)
      "Axis(%s)".format(coordinate.map(axis => "%s->%s".format(axis.id.name, axis.value.toString)).mkString(", "))
    else
      "ROOT"
    override def equals(that: Any): Boolean = (this eq that.asInstanceOf[Object]) || (that match {
      case that: Coordinate =>
        this.coordinate.sortBy(_.id.name) == that.coordinate.sortBy(_.id.name)
      case _ => false
    })
    override def hashCode() = coordinate.foldLeft(0)((a, b) => a * 31 + b.hashCode)
  }
  /**
   * Companion object for coordinate
   * that contains predefined root coordinate and coordinate builders.
   */
  object Coordinate {
    /** Predefined point with empty coordinate list. */
    val root = new Coordinate(List())

    /** Create Coordinate with sorted axes list */
    def apply(coordinate: Axis[_ <: java.io.Serializable]*): Coordinate = if (coordinate.nonEmpty) new Coordinate(coordinate.sortBy(_.id.name).toList) else root
    /** Create Coordinate with sorted axes list */
    def apply(coordinate: List[Axis[_ <: java.io.Serializable]]): Coordinate = if (coordinate.nonEmpty) new Coordinate(coordinate.sortBy(_.id.name)) else root
  }
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
   * Element reference that point to relative location
   */
  case class Location[A <: Element[B], B <: Stash](override val id: Symbol,
    override val coordinate: Element.Coordinate = Element.Coordinate.root)(implicit em: Manifest[A], sm: Manifest[B])
    extends GenericLocation[A, B](id, coordinate)
  abstract class GenericLocation[A <: Element[B], B <: Stash](val id: Symbol,
    val coordinate: Element.Coordinate)(implicit em: Manifest[A], sm: Manifest[B]) {
    lazy val elementManifest = em
    lazy val stashManifest = sm
  }
  /**
   * Element reference that point to particular unique element registered at Element.index
   */
  case class Reference(origin: Symbol, unique: UUID, coordinate: Coordinate)
}
