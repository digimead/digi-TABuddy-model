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

import java.beans.PropertyChangeEvent
import java.util.UUID
import java.util.concurrent.ScheduledThreadPoolExecutor
import java.util.concurrent.ThreadFactory
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf
import scala.util.DynamicVariable

import org.digimead.digi.lib.log.Loggable
import org.digimead.digi.lib.log.logger.RichLogger.rich2slf4j
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.Model.model2implementation
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.dsl.DSLType.dsltype2implementation
import org.digimead.tabuddy.model.element.compare.Compare
import org.digimead.tabuddy.model.element.compare.CompareByModification

import language.implicitConversions

/**
 * Base element
 * builded with the curiously recurring generic pattern
 * contains stash with actual data.
 */
trait Element[StashProjection <: Stash] extends Loggable with Ordered[Element.Generic] with java.io.Serializable {
  /**
   * Child elements
   * NOTE: Circular reference processed by GC
   */
  val eChildren = new Children(this)
  /**
   * Element reference as implicit parameter
   * NOTE: Circular reference processed by GC
   */
  @transient implicit lazy val elementRef: Element.Generic = this
  /** Element global modification time, based on children state. */
  @volatile protected var elementModified: Element.Timestamp = eStash.modified
  log.debug(s"$this with unique ID $eUnique alive")

  /** Compares this object with the specified object for order. */
  def compare(that: Element.Generic): Int =
    Element.comparator.value.compare(this, that)
  /** Build an ancestors sequence */
  def eAncestors(): Seq[Element.Generic] = {
    @tailrec
    def ancestors(current: Element.Generic, acc: Seq[Element.Generic]): Seq[Element.Generic] = {
      val ancestor = Model.e(current.eStash.context.container) match {
        case Some(ancestor) =>
          if (ancestor == Model.inner || ancestor.isInstanceOf[Model.Interface[_]])
            if (ancestor == current)
              return acc // model
            else
              return acc :+ ancestor // regular element
          ancestor
        case None =>
          return acc
      }
      ancestors(ancestor, acc :+ ancestor)
    }
    ancestors(this, Seq())
  }
  /** Build an ancestor references sequence */
  def eAncestorReferences(): Seq[Reference] = {
    @tailrec
    def ancestors(current: Element.Generic, acc: Seq[Reference]): Seq[Reference] = {
      val ancestor = Model.e(current.eStash.context.container) match {
        case Some(ancestor) =>
          if (ancestor == Model.inner || ancestor.isInstanceOf[Model.Interface[_]])
            if (ancestor == current)
              return acc // model
            else
              return acc :+ current.eStash.context.container // regular element
          ancestor
        case None =>
          return acc
      }
      if (acc.size > Element.MAXIMUM_DEEPNESS) {
        log.fatal("element level is too deep: %s, ...".format(acc.take(10).mkString(", ")))
        return acc
      }
      ancestors(ancestor, acc :+ current.eStash.context.container)
    }
    ancestors(this, Seq())
  }
  /**
   * As an optional instance of for Element.Generic
   *
   * for example:
   *   element.eAs[Task[Task.Stash], Task.Stash]
   *   element.eAs[Note[Note.Stash], Note.Stash]
   *   element.eAs[Record[Record.Stash], Record.Stash]
   */
  def eAs[A <: Element[B], B <: Stash]()(implicit ma: Manifest[A], mb: Manifest[B]): Option[A] = {
    val mirror = scala.reflect.runtime.universe.runtimeMirror(getClass.getClassLoader)
    val thisClassSymbol = mirror.classSymbol(getClass)
    val stashClassSymbol = mirror.classSymbol(eStash.getClass)
    // we may get generic with undocumented and unstable API - typeOf[A].asInstanceOf[Types#TypeRefApi].args.head
    if (thisClassSymbol.baseClasses.contains(typeOf[A].typeSymbol) && stashClassSymbol.baseClasses.contains(typeOf[B].typeSymbol))
      Some(this.asInstanceOf[A])
    else
      None
  }
  /** Get element coordinate */
  def eCoordinate() = eStash.coordinate
  /** Copy constructor */
  def eCopy(): this.type = eCopy(None, None)
  /** Copy constructor */
  def eCopy(stash: StashProjection): this.type = eCopy(Some(stash), None)
  /** Copy constructor */
  def eCopy(children: List[Element.Generic]): this.type = eCopy(None, Some(children))
  /** Copy constructor */
  def eCopy(stash: StashProjection, children: List[Element.Generic]): this.type = eCopy(Some(stash), Some(children))
  /** Copy constructor */
  def eCopy(stash: Option[StashProjection], children: Option[List[Element.Generic]]): this.type = {
    val elementStash = stash.getOrElse { this.eStash.copy() }
    val element = eNewInstance(elementStash)
    if (element.eStash.model != Some(element))
      element.eStash.model = None // reset the model for all copies except models
    val eChildren: List[Element.Generic] = children.getOrElse(this.eChildren.map(_.eCopy()).toList)
    element.eChildren ++= eChildren
    element
  }
  /** Dump the element content */
  def eDump(brief: Boolean, padding: Int = 2): String
  /** Get filtered child elements from subtree */
  def eFilter(filter: (Element.Generic) => Boolean): Seq[Element.Generic] =
    this.eFilter(Seq(this), filter, Seq())
  /**
   * Find child element
   * @param A - element class
   * @param B - stash class
   */
  def eFind[A <: Element[B], B <: Stash](id: Symbol, coordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(implicit a: Manifest[A],
    b: Manifest[B]): Option[A] = eFind[A, B](id, Coordinate(coordinate: _*))
  /**
   * Find child element
   * @param A - element class
   * @param B - stash class
   */
  def eFind[A <: Element[B], B <: Stash](id: Symbol, coordinate: Coordinate)(implicit a: Manifest[A], b: Manifest[B]): Option[A] = {
    eChildren.find { element =>
      element.eStash.id == id && element.eStash.coordinate == coordinate
    } match {
      case e @ Some(element) if element.canEqual(a.runtimeClass, b.runtimeClass) => e.asInstanceOf[Option[A]]
      case _ => None
    }
  }
  /** Get a property. */
  def eGet[A <: AnyRef with java.io.Serializable](id: Symbol)(implicit m: Manifest[A]): Option[Value[A]] =
    DSLType.classSymbolMap.get(m.runtimeClass).flatMap(typeSymbol =>
      eGet(id, typeSymbol)).asInstanceOf[Option[Value[A]]]
  /** Get a property. */
  def eGet(id: Symbol, typeSymbol: Symbol): Option[Value[_ <: AnyRef with java.io.Serializable]] =
    eStash.property.get(id).flatMap(_.get(typeSymbol)).asInstanceOf[Option[Value[_ <: AnyRef with java.io.Serializable]]]
  /** Get all property values. */
  def eGetAll(id: Symbol): Seq[Value[_ <: AnyRef with java.io.Serializable]] =
    eStash.property.get(id).map(_.values.toSeq).getOrElse(Seq())
  /** Get a property or else get the property from the root element. */
  def eGetOrElseRoot[A <: AnyRef with java.io.Serializable](id: Symbol)(implicit m: Manifest[A]): Option[Value[A]] =
    DSLType.classSymbolMap.get(m.runtimeClass).flatMap(typeSymbol => eGetOrElseRoot(id, typeSymbol)).asInstanceOf[Option[Value[A]]]
  /** Get a property or else get the property from the root element. */
  def eGetOrElseRoot(id: Symbol, typeSymbol: Symbol): Option[Value[_ <: AnyRef with java.io.Serializable]]
  /** Get element id */
  def eId() = eStash.id
  /** Get Model for this element */
  def eModel(): Model.Generic = eStash.model getOrElse {
    throw new RuntimeException("Model undefined for detached element " + this)
  }
  /** Get modified timestamp */
  def eModified() = elementModified
  /** Get a container */
  def eParent(): Option[Element.Generic] =
    eStash.model.flatMap(_.e(eStash.context.container.origin, eStash.context.container.unique, eStash.context.container.coordinate))
  /** Get reference of this element */
  def eReference() = Reference(eStash.context.container.origin, eStash.unique, eStash.coordinate)
  /** Remove the specific property's value */
  def eRemove[A <: AnyRef with java.io.Serializable](id: Symbol)(implicit m: Manifest[A]): Option[Value[A]] = {
    Element.log.trace(s"remove $id from $eId")
    eSet[A](id, None)
  }
  /** Remove the specific property's value */
  def eRemove(id: Symbol, typeSymbol: Symbol): Option[Value[_ <: AnyRef with java.io.Serializable]] = {
    Element.log.trace(s"remove $id from $eId")
    eSet(id, typeSymbol, None)
  }
  /** Remove all property's values */
  def eRemoveAll(id: Symbol): Seq[Value[_ <: AnyRef with java.io.Serializable]] = {
    Element.log.trace(s"remove all $id from $eId")
    eStash.property.remove(id) match {
      case Some(valueHash) =>
        eModify()
        valueHash.values.toSeq
      case None =>
        Seq()
    }
  }
  /** Get the root element from the current origin if any. */
  def eRoot(): Option[this.type]
  /** Get the root element from the particular origin if any */
  def eRoot(origin: Symbol): Option[this.type]
  /** Get the scope of the element */
  def eScope() = eStash.scope
  /** Set a new property, return an old property */
  def eSet[A <: AnyRef with java.io.Serializable](id: Symbol, value: Option[Value[A]])(implicit m: Manifest[A]): Option[Value[A]] =
    eSet(id, value, null.asInstanceOf[A])
  /** Set a new property, return an old property */
  def eSet[A <: AnyRef with java.io.Serializable](id: Symbol, value: Option[Value[A]], default: A)(implicit m: Manifest[A]): Option[Value[A]] =
    DSLType.classSymbolMap.get(m.runtimeClass).flatMap(typeSymbol =>
      eSet(id, typeSymbol, value, default)).asInstanceOf[Option[Value[A]]]
  /** Set a new property, return an old property */
  def eSet(id: Symbol, typeSymbol: Symbol, value: Option[Value[_ <: AnyRef with java.io.Serializable]]): Option[Value[_ <: AnyRef with java.io.Serializable]] =
    eSet(id, typeSymbol, value, null)
  /** Set a new property, return an old property */
  def eSet(id: Symbol, typeSymbol: Symbol, value: Option[Value[_ <: AnyRef with java.io.Serializable]], default: java.io.Serializable): Option[Value[_ <: AnyRef with java.io.Serializable]] = {
    Element.log.trace(s"set new $id for $eId")
    if (DSLType.symbols(typeSymbol))
      value match {
        case Some(value) if Some(value.get) != Option(default) =>
          eStash.property.get(id) match {
            case Some(valueHash) =>
              eModify()
              val previous = valueHash.get(typeSymbol)
              valueHash(typeSymbol) = value.copy(eStash.context.copy(container = eReference))
              previous match {
                case Some(previous) =>
                  val undoF = () => {}
                  Element.Event.publish(Element.Event.ValueUpdate(this, previous, value, eModified)(undoF))
                case None =>
                  val undoF = () => {}
                  Element.Event.publish(Element.Event.ValueInclude(this, value, eModified)(undoF))
              }
              previous
            case None =>
              eModify()
              val valueHash = new mutable.HashMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]]() with mutable.SynchronizedMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]]
              eStash.property(id) = valueHash
              valueHash(typeSymbol) = value.copy(eStash.context.copy(container = eReference))
              val undoF = () => {}
              Element.Event.publish(Element.Event.ValueInclude(this, value, eModified)(undoF))
              None
          }
        case _ =>
          // None or value == default
          eStash.property.get(id) match {
            case Some(valueHash) =>
              eModify()
              val previous = valueHash.remove(typeSymbol)
              previous.foreach { previous =>
                val undoF = () => {}
                Element.Event.publish(Element.Event.ValueRemove(this, previous, eModified)(undoF))
              }
              previous
            case None =>
              None
          }
      }
    else {
      log.fatal("unable to set new value %s for property %s with unknown type symbol %s".format(value, id, typeSymbol))
      None
    }
  }
  /** Get current stash */
  def eStash: StashProjection
  /**
   * Set current stash
   * It should preserve the modification timestamp
   * Stash already contains 'modified' attribute
   */
  def eStash_=(value: StashProjection): Unit
  /** Get element unique id */
  def eUnique() = eStash.unique

  /** Collect all sub elements that conform a user filter */
  @tailrec
  final protected def eFilter(e: Seq[Element.Generic], filter: (Element.Generic) => Boolean,
    acc: Seq[Element.Generic]): Seq[Element.Generic] = {
    val children = e.map(_.eChildren).flatten.filter(filter)
    if (children.isEmpty) return acc
    this.eFilter(children, filter, acc ++ children)
  }
  /** Set new modification time, push notification */
  protected def eModify() {
    val ts = Element.timestamp()
    elementModified = ts
    eStash.modified = ts
    eParent.foreach(_.eModify)
  }
  /** create new instance with specific stash */
  protected def eNewInstance(stash: StashProjection): this.type

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
  override def toString() = "%s[%s@%s]".format(eStash.scope, eStash.id.name, eStash.coordinate.toString)
}

/**
 * Companion object for the base element
 */
object Element extends Loggable {
  type Generic = Element[_ <: Stash]
  /**
   * Elements and it derivative classes default ordering
   */
  implicit def orderingByModification[T <: Element.Generic]: Ordering[T] =
    new Ordering[T] { def compare(x: T, y: T): Int = x.compare(y) }
  /** Maximum amount of element nested levels */
  val MAXIMUM_DEEPNESS = 10000
  /** Active elements comparator, local thread */
  val comparator = new DynamicVariable[Compare](CompareByModification)
  /** Base nanoseconds for Timestamp shift */
  private var nanoBase = System.nanoTime()
  /** nanoBase renew scheduled thread pool executor */
  private lazy val nanoBaseRenewExecutor = new ScheduledThreadPoolExecutor(1, new ThreadFactory {
    val threadNumber = new AtomicInteger(1)
    val namePrefix = "model-history-pool-thread-"
    def newThread(runnable: Runnable): Thread = {
      val thread = new Thread(runnable, namePrefix + threadNumber.getAndIncrement())
      if (!thread.isDaemon())
        thread.setDaemon(true)
      if (thread.getPriority() != Thread.MIN_PRIORITY)
        thread.setPriority(Thread.MIN_PRIORITY)
      thread
    }
  })
  /** nanoBase renew task */
  private val nanoBaseSchedule = nanoBaseRenewExecutor.schedule(new Runnable {
    def run = Timestamp.synchronized { nanoBase = System.nanoTime() }
  }, 1, TimeUnit.MINUTES)

  /**
   * Check new/exists/modified stash against neighbor
   */
  def check(element: Element.Generic, stash: Stash) {
    stash.model match {
      case Some(model) =>
        model.e(stash.context.container).map(_.eChildren.filter(_.eId == stash.id)).getOrElse(ArrayBuffer[Element.Generic]()).foreach {
          nighborWithSameID =>
            val neighborStash = nighborWithSameID.eStash.asInstanceOf[Stash]
            assert(nighborWithSameID.eUnique == stash.unique, "Illegal new element %s. %s MUST be the same as id %s of neighbor %s.".
              format(element, stash.unique, nighborWithSameID.eUnique, nighborWithSameID))
            assert(neighborStash.coordinate != stash.coordinate, "Illegal new element %s. There is already neighbor %s exists with same coordinate.".
              format(element, nighborWithSameID))
            assert(nighborWithSameID.canEqual(element.getClass(), stash.getClass()), "Illegal new element %s. There is already neighbor %s exists and it has different type.".
              format(element, nighborWithSameID))
        }
        assert(model.e(element.eReference).isEmpty, "Illegal new element %s with reference %s. Such element is already attached.".format(element, element.eReference))
      case None =>
        assert(false, s"unable to check $element against stash with undefined model")
    }
  }
  /** Create new timestamp object */
  def timestamp(ms: Long = System.currentTimeMillis(), ns: Long = System.nanoTime()) = synchronized { Timestamp(ms, ns - nanoBase) }

  sealed trait Event extends mutable.Undoable {
    this: PropertyChangeEvent =>
    /** Ancestors */
    val ancestorRefs: Seq[Reference]
    /** Modification timestamp */
    val modified: Element.Timestamp
    /** Undo the last operation function container. */
    val undoF: () => Unit
    /** Undo the last operation. */
    def undo(): Unit = undoF()
  }
  object Event extends mutable.Publisher[Event] {
    import scala.language.existentials
    /** Provide publish() public access */
    override def publish(event: Event) = try {
      super.publish(event)
    } catch {
      // catch all other subscriber exceptions
      case e: Throwable =>
        log.error(e.getMessage(), e)
    }

    /**
     * The event generated by container ElementBuffer when new child added
     */
    case class ChildInclude[T <: Element.Generic](sourceArg: T, // container with ElementBuffer
      newValueArg: Element.Generic,
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, null, newValueArg) with Event {
      /** Ancestors */
      val ancestorRefs = source.asInstanceOf[T].eAncestorReferences
    }
    /**
     * The event generated by container ElementBuffer when exists child removed
     */
    case class ChildRemove[T <: Element.Generic](sourceArg: T, // container with ElementBuffer
      oldValueArg: Element.Generic,
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, oldValueArg, null) with Event {
      /** Ancestors */
      val ancestorRefs = source.asInstanceOf[T].eAncestorReferences
    }
    /**
     * The event generated by container ElementBuffer when exists child replaced
     */
    case class ChildReplace[T <: Element.Generic](sourceArg: T, // container with ElementBuffer
      oldValueArg: Element.Generic, newValueArg: Element.Generic,
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, oldValueArg, newValueArg) with Event {
      /** Ancestors */
      val ancestorRefs = source.asInstanceOf[T].eAncestorReferences
    }
    /**
     * The event generated by container ElementBuffer when buffer cleared
     */
    case class ChildrenReset[T <: Element.Generic](sourceArg: T, // container with ElementBuffer
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, null, null) with Event {
      /** Ancestors */
      val ancestorRefs = source.asInstanceOf[T].eAncestorReferences
    }
    case class ValueInclude[T <: Element.Generic](sourceArg: T,
      newValueArg: Value[_ <: AnyRef with java.io.Serializable],
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, null, newValueArg) with Event {
      /** Ancestors */
      val ancestorRefs = source.asInstanceOf[T].eAncestorReferences
    }
    case class ValueRemove[T <: Element.Generic](sourceArg: T,
      oldValueArg: Value[_ <: AnyRef with java.io.Serializable],
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, oldValueArg, null) with Event {
      /** Ancestors */
      val ancestorRefs = source.asInstanceOf[T].eAncestorReferences
    }
    case class ValueUpdate[T <: Element.Generic](sourceArg: T,
      oldValueArg: Value[_ <: AnyRef with java.io.Serializable],
      newValueArg: Value[_ <: AnyRef with java.io.Serializable],
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, oldValueArg, newValueArg) with Event {
      /** Ancestors */
      val ancestorRefs = source.asInstanceOf[T].eAncestorReferences
    }
    /**
     * The event generated by container when stash replaced
     */
    case class StashReplace[T <: Element.Generic](sourceArg: T, // container with stash
      oldValueArg: Stash, newValueArg: Stash,
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, oldValueArg, newValueArg) with Event {
      /** Ancestors */
      val ancestorRefs = source.asInstanceOf[T].eAncestorReferences
    }
    /**
     * The event generated by model singleton when model replaced
     */
    case class ModelReplace[A <: Model.Interface[_ <: Model.Stash], B <: Model.Interface[_ <: Model.Stash]](
      oldValueArg: B, // previous model
      newValueArg: A, // current model
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(newValueArg, newValueArg.eId.name, oldValueArg, newValueArg) with Event {
      /** Ancestors */
      val ancestorRefs = source.asInstanceOf[Model.Generic].eAncestorReferences
    }
    case class Mark(val id: UUID = UUID.randomUUID)
      extends PropertyChangeEvent(Model.inner, Model.inner.eId.name, null, null) with Event {
      /** Modification timestamp */
      val modified: Element.Timestamp = Element.timestamp()
      /** Undo the last operation function container. */
      val undoF: () => Unit = () => {}
      /** Ancestors */
      val ancestorRefs = Model.inner.eAncestorReferences

      override def toString() = s"Mark($modified)"
    }
  }
  /** The class that provides a marker for additional specialization of the element */
  abstract class Scope(val modificator: Symbol) extends java.io.Serializable with Equals {
    override def toString() = modificator.name

    def canEqual(other: Any): Boolean
    override def equals(other: Any) = {
      other match {
        case that: org.digimead.tabuddy.model.element.Element.Scope => that.canEqual(this) && modificator == that.modificator
        case _ => false
      }
    }
    override def hashCode() = {
      val prime = 41
      prime + modificator.hashCode
    }
  }
  /** Timestamp class */
  case class Timestamp(val milliseconds: Long, nanoShift: Long) {
    override def toString() = s"Timestamp[$milliseconds:$nanoShift]"
  }
}
