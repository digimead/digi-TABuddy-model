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

import java.util.UUID
import java.util.concurrent.ScheduledThreadPoolExecutor
import java.util.concurrent.ThreadFactory
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.DynamicVariable

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.dsl.DSLType.dsltype2implementation
import org.digimead.tabuddy.model.element.compare.Compare
import org.digimead.tabuddy.model.element.compare.CompareByTimespamp
import org.digimead.tabuddy.model.graph.Context
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.ElementBox.box2interface
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.serialization.Serialization

import language.implicitConversions

/**
 * Base element
 * builded with the curiously recurring generic pattern
 * contains stash with actual data.
 */
trait Element extends Equals {
  this: Loggable ⇒
  /** Element type. */
  type ElementType <: Element
  /** Stash type. */
  type StashType <: Stash.Like
  /**
   * Element are always to know about it's box.
   * Box are always to know about it's element.
   *
   * When (box <-> element) will be unreachable then they will be GC'ed.
   */
  @transient val eBox: ElementBox[ElementType]

  /** Commit complex properties (if needed) while saving. */
  def commitProperties(): Unit =
    eStash.property.foreach { case (typeKey, value) ⇒ value.foreach { case (idKey, value) ⇒ value.commit(this) } }
  /** Compares this object with the specified object for order. */
  def compare(that: Element): Int = Element.comparator.value.compare(this, that)
  /** Build an ancestors sequence. */
  def eAncestors(): Seq[Node] = {
    @tailrec
    def ancestors(current: Node, acc: Seq[Node]): Seq[Node] = {
      if (acc.size > Element.MAXIMUM_DEEPNESS) {
        log.fatal("element level is too deep: %s, ...".format(acc.take(10).mkString(", ")))
        return acc
      }
      current.getParent match {
        case n @ Some(parent) if acc.lastOption != n && parent != current ⇒ ancestors(parent, acc :+ parent)
        case _ ⇒ acc
      }
    }
    ancestors(eNode, Seq())
  }
  /**
   * As an optional instance of for Element
   *
   * for example:
   *   element.eAs[Task.Like]
   *   element.eAs[Note.Like]
   *   element.eAs[Record.Like]
   */
  def eAs[A <: Element]()(implicit m: Manifest[A]): Option[A] =
    if (m.runtimeClass.isAssignableFrom(getClass)) Some(this.asInstanceOf[A]) else None
  /** Get element context. */
  def eContext(): Context = eBox.context
  /** Get element coordinate */
  def eCoordinate() = eStash.coordinate
  /** Copy constructor */
  def eCopy(elementBox: ElementBox[ElementType], stash: ElementType#StashType): ElementType =
    Element[ElementType](elementBox, stash)(Manifest.classType(getClass))
  /** Copy constructor that attach copy to the target node. */
  def eCopy(target: Node, rawCoordinate: Axis.Generic*): ElementType =
    eCopy(target, Coordinate(rawCoordinate: _*))
  /** Copy constructor that attach copy to the target node. */
  /*
   * copy must preserve creation time
   * copy must preserve modification time if scope && properties content are the same
   */
  def eCopy(target: Node, coordinate: Coordinate = eStash.coordinate, serialization: Serialization[_] = eBox.serialization): ElementType = {
    // asInstanceOf[ElementType#StashType] is simplify type between
    // abstract ElementType#StashType#StashType, ElementType#StashType, StashType
    target.threadSafe { target ⇒
      val stash = eStash.copy(
        coordinate = coordinate,
        id = target.id,
        origin = target.graph.origin,
        unique = target.unique).asInstanceOf[ElementType#StashType]
      eCopy(target, stash, serialization)
    }
  }
  /** Copy constructor that attach copy to the target node. */
  /*
   * copy must preserve creation time
   * copy must preserve modification time if scope && properties content are the same
   */
  def eCopy(target: Node, stash: ElementType#StashType, serialization: Serialization[_]): ElementType = {
    // asInstanceOf[ElementType#StashType] is simplify type between
    // abstract ElementType#StashType#StashType, ElementType#StashType, StashType
    target.threadSafe { target ⇒
      ElementBox(Context(stash.origin, target.parentNodeReference.get.
        getOrElse({ throw new IllegalArgumentException(s"Unable to copy ${this} to node ${target} without parent node.") }).unique),
        target, serialization, stash.asInstanceOf[ElementType#StashType])(Manifest.classType(getClass)).get
    }
  }
  /** Dump the element content */
  def eDump(brief: Boolean, padding: Int = 2): String
  /** Find child element. */
  def eFind[A <: Element](p: A ⇒ Boolean)(implicit a: Manifest[A]): Option[A] = eNode.threadSafe {
    _.view.map(_.getElementBoxes).flatten.find { box ⇒ box.elementType == a && p(box.get.asInstanceOf[A]) } match {
      case e @ Some(element) if element.get.getClass.isAssignableFrom(a.runtimeClass) ⇒ Some(element.get.asInstanceOf[A])
      case _ ⇒ None
    }
  }
  /** Get a property. */
  def eGet[A <: AnyRef with java.io.Serializable](id: Symbol)(implicit m: Manifest[A]): Option[Value[A]] =
    DSLType.classSymbolMap.get(m.runtimeClass).flatMap(typeSymbol ⇒
      eGet(id, typeSymbol)).asInstanceOf[Option[Value[A]]]
  /** Get a property. */
  def eGet(id: Symbol, typeSymbol: Symbol): Option[Value[_ <: AnyRef with java.io.Serializable]] =
    eStash.property.get(id).flatMap(_.get(typeSymbol)).asInstanceOf[Option[Value[_ <: AnyRef with java.io.Serializable]]]
  /** Get all property values. */
  def eGetAll(id: Symbol): Seq[Value[_ <: AnyRef with java.io.Serializable]] =
    eStash.property.get(id).map(_.values.toSeq).getOrElse(Seq())
  /** Get a property or else get the property from the root element. */
  def eGetOrElseRoot[A <: AnyRef with java.io.Serializable](id: Symbol)(implicit m: Manifest[A]): Option[Value[A]] =
    DSLType.classSymbolMap.get(m.runtimeClass).flatMap(typeSymbol ⇒ eGetOrElseRoot(id, typeSymbol)).asInstanceOf[Option[Value[A]]]
  /** Get a property or else get the property from the root element. */
  def eGetOrElseRoot(id: Symbol, typeSymbol: Symbol): Option[Value[_ <: AnyRef with java.io.Serializable]]
  /** Get element id */
  def eId() = eStash.id
  /** Get mutable representation. */
  def eMutable(): Element.Mutable[ElementType] = new Element.Mutable(this.asInstanceOf[ElementType]) {}
  /** Get Model for this element. */
  def eModel(): Model.Like = eNode.graph.model
  /** Get modified timestamp. */
  def eModified() = eStash.modified
  /** Get element node. */
  def eNode(): Node = eBox.node
  /** Get a container. */
  def eParent(): Option[Node] = eBox.node.getParent
  /** Get reference of this element */
  def eReference() = Reference(eStash.origin, eStash.unique, eStash.coordinate)
  /** Remove the specific property's value */
  def eRemove[A <: AnyRef with java.io.Serializable](id: Symbol)(implicit m: Manifest[A]): ElementType = {
    Element.log.trace(s"Remove $id from $eId.")
    eSet[A](id, None)
  }
  /** Remove the specific property's value */
  def eRemove(id: Symbol, typeSymbol: Symbol): ElementType = {
    Element.log.trace(s"Remove $id from $eId.")
    eSet(id, typeSymbol, None)
  }
  /** Remove all property's values */
  def eRemoveAll(id: Symbol): ElementType = {
    Element.log.trace(s"Remove all $id from $eId.")
    eCopy(eBox.node, this.eStash.copy(property = new Stash.Data).asInstanceOf[ElementType#StashType], eBox.serialization)
  }
  /** Get the root element from the current origin if any. */
  def eRoot(): Option[Element] = eBox.node.getProjection(Coordinate.root).map(_.get)
  /** Get the scope of the element */
  def eScope() = eStash.scope
  /** Set a new property, return an old property */
  def eSet[A <: AnyRef with java.io.Serializable](id: Symbol, value: Option[Value[A]])(implicit m: Manifest[A]): ElementType =
    eSet(id, value, null.asInstanceOf[A])
  /** Set a new property, return an old property */
  def eSet[A <: AnyRef with java.io.Serializable](id: Symbol, value: Option[Value[A]], default: A)(implicit m: Manifest[A]): ElementType =
    DSLType.classSymbolMap.get(m.runtimeClass) match {
      case Some(typeSymbol) ⇒
        eSet(id, typeSymbol, value, default)
      case None ⇒
        throw new IllegalArgumentException("Unknown type " + m.runtimeClass.getName())
    }
  /** Set a new property, return an old property */
  def eSet(id: Symbol, typeSymbol: Symbol, value: Option[Value[_ <: AnyRef with java.io.Serializable]]): ElementType =
    eSet(id, typeSymbol, value, null)
  /** Set a new property, return an old property */
  def eSet(id: Symbol, typeSymbol: Symbol, value: Option[Value[_ <: AnyRef with java.io.Serializable]], default: java.io.Serializable): ElementType = {
    Element.log.trace(s"Set new $id for $eId.")
    if (DSLType.symbols(typeSymbol))
      value match {
        case Some(value) if Some(value.get) != Option(default) ⇒
          // Set specific value != default
          eStash.property.get(id) match {
            case Some(valueHash) ⇒
              val previousValue = valueHash.get(typeSymbol)
              val newValue = value.copy(context = eBox.context.copy(unique = eUnique))
              previousValue match {
                case Some(previous) ⇒
                  val undoF = () ⇒ {}
                //Element.Event.publish(Element.Event.ValueUpdate(this, previous, value, eModified)(undoF))
                case None ⇒
                  val undoF = () ⇒ {}
                //Element.Event.publish(Element.Event.ValueInclude(this, value, eModified)(undoF))
              }
              eCopy(eBox.node, eStash.copy(modified = Element.timestamp(),
                property = eStash.property.updated(id, valueHash.updated(typeSymbol, newValue))).
                asInstanceOf[ElementType#StashType], eBox.serialization)
            case None ⇒
              val newValue = value.copy(context = eBox.context.copy(unique = eUnique))
              val undoF = () ⇒ {}
              //Element.Event.publish(Element.Event.ValueInclude(this, value, eModified)(undoF))
              None
              eCopy(eBox.node, eStash.copy(modified = Element.timestamp(), property = eStash.property.updated(id,
                immutable.HashMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]](typeSymbol -> newValue))).
                asInstanceOf[ElementType#StashType], eBox.serialization)
          }
        case _ ⇒
          // Set default value or None
          eStash.property.get(id) match {
            case Some(valueHash) ⇒
              val previousValue = valueHash.get(typeSymbol)
              previousValue.foreach { previous ⇒
                val undoF = () ⇒ {}
                //Element.Event.publish(Element.Event.ValueRemove(this, previous, eModified)(undoF))
              }
              eCopy(eBox.node, eStash.copy(modified = Element.timestamp(),
                property = eStash.property.updated(id, (valueHash - typeSymbol))).asInstanceOf[ElementType#StashType], eBox.serialization)
            case None ⇒
              Element.this.asInstanceOf[ElementType]
          }
      }
    else
      throw new IllegalArgumentException(s"""Unable to set new value "${value}" to property "${id}" with unknown type symbol ${typeSymbol}.""")
  }
  /** Get current stash */
  def eStash: StashType
  /** Get element unique id */
  def eUnique(): UUID = eStash.unique

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Element]
  override def equals(that: Any): Boolean = that match {
    case that: Element ⇒ // vs immutable
      (that eq this) || ((that canEqual this) && this.## == that.##)
    case that: Element.Mutable[_] ⇒ // vs mutable
      (that.immutable eq this) || ((that.immutable canEqual this) && this.## == that.immutable.##)
    case _ ⇒ false
  }
  override lazy val hashCode = java.util.Arrays.hashCode(Array[AnyRef](this.eStash))

  override def toString() = "%s[%s@%s]".format(eStash.scope, eStash.id.name, eStash.coordinate.toString)
}

object Element extends Loggable {
  /**
   * Elements and it derivative classes default ordering
   */
  implicit def orderingByModification[T <: Element]: Ordering[T] =
    new Ordering[T] { def compare(x: T, y: T): Int = x.compare(y) }
  /** Maximum amount of element nested levels. */
  val MAXIMUM_DEEPNESS = 10000
  /** Active elements comparator, local thread .*/
  val comparator = new DynamicVariable[Compare](CompareByTimespamp)
  /** Base nanoseconds for Timestamp shift. */
  private var nanoBase = System.nanoTime()
  /** nanoBase renew scheduled thread pool executor. */
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
  /** nanoBase renew task. */
  private val nanoBaseSchedule = nanoBaseRenewExecutor.schedule(new Runnable {
    def run = Timestamp.synchronized { nanoBase = System.nanoTime() }
  }, 1, TimeUnit.MINUTES)

  /** Create a new element. */
  def apply[A <: Element](box: ElementBox[A], stash: A#StashType)(implicit a: Manifest[A]): A = {
    val elementGraph = box.node.graph
    val elementCtor = a.runtimeClass.getConstructors.find(_.getParameterTypes() match {
      case Array(stashArg, boxArg) ⇒
        stashArg.isAssignableFrom(stash.getClass()) && boxArg.isAssignableFrom(box.getClass())
      case _ ⇒ false
    }) getOrElse {
      throw new NoSuchMethodException(s"Unable to find proper constructor for element ${a.runtimeClass}.")
    }
    if (box.coordinate != stash.coordinate)
      throw new IllegalArgumentException(s"Stash is inconsistent. Coordinate: ${box.coordinate} vs ${stash.coordinate}.")
    if (box.node.unique != stash.unique)
      throw new IllegalArgumentException(s"Stash is inconsistent. Unique: ${box.node.unique} vs ${stash.unique}.")
    if (box.node.id != stash.id)
      throw new IllegalArgumentException(s"Stash is inconsistent. Id: ${box.node.id} vs ${stash.id}.")
    if (elementGraph.origin != stash.origin)
      throw new IllegalArgumentException(s"Stash is inconsistent. Origin: ${elementGraph.origin} vs ${stash.origin}")
    elementCtor.newInstance(stash, box).asInstanceOf[A]
  }
  /** Create a new element. */
  def apply[A <: Element](box: ElementBox[A], created: Element.Timestamp, modified: Element.Timestamp,
    property: Stash.Data, scope: A#StashType#ScopeType)(implicit a: Manifest[A], stashClass: Class[_ <: A#StashType]): A = {
    val elementGraph = box.node.graph
    val stashCtor = stashClass.getConstructors.find(_.getParameterTypes() match {
      case Array(coordinateArg, createdArg, idArg, modifiedArg, originArg, dataArg, scopeArg, uuidArg) ⇒
        scopeArg.isAssignableFrom(scope.getClass) && coordinateArg.isAssignableFrom(box.coordinate.getClass()) &&
          createdArg.isAssignableFrom(created.getClass()) && idArg.isAssignableFrom(box.node.id.getClass()) &&
          modifiedArg.isAssignableFrom(modified.getClass()) && originArg.isAssignableFrom(elementGraph.origin.getClass()) &&
          dataArg.isAssignableFrom(property.getClass()) && uuidArg.isAssignableFrom(box.node.unique.getClass)
      case _ ⇒ false
    }) getOrElse {
      throw new NoSuchMethodException(s"Unable to find proper constructor for stash ${stashClass}.")
    }
    val stash = stashCtor.newInstance(box.coordinate, created, box.node.id, modified, elementGraph.origin,
      property, scope, box.node.unique).asInstanceOf[A#StashType]
    apply[A](box, stash)
  }

  /**
   * Check new/exists/modified stash against neighbor
   */
  def check(element: Element, stash: Stash) {
    //stash.model match {
    // case Some(model) =>
    /*model.e(stash.context.container).map(_.eChildren.filter(_.eId == stash.id)).getOrElse(ArrayBuffer[Element2]()).foreach {
          nighborWithSameID =>
            val neighborStash = nighborWithSameID.eStash.asInstanceOf[Stash]
            assert(nighborWithSameID.eUnique == stash.unique, "Illegal new element %s. %s MUST be the same as id %s of neighbor %s.".
              format(element, stash.unique, nighborWithSameID.eUnique, nighborWithSameID))
            assert(neighborStash.coordinate != stash.coordinate, "Illegal new element %s. There is already neighbor %s exists with same coordinate.".
              format(element, nighborWithSameID))
            assert(nighborWithSameID.canEqual(element.getClass(), stash.getClass()), "Illegal new element %s. There is already neighbor %s exists and it has different type.".
              format(element, nighborWithSameID))
        }*/
    //assert(model.e(element.eReference).isEmpty, "Illegal new element %s with reference %s. Such element is already attached.".format(element, element.eReference))
    //case None =>
    //assert(false, s"unable to check $element against stash with undefined model")
    //}
  }
  /** Create new timestamp object */
  def timestamp(ms: Long = System.currentTimeMillis(), ns: Long = System.nanoTime()) = synchronized { Timestamp(ms, ns - nanoBase) }

  /*sealed trait Event extends mutable.Undoable {
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
    case class ChildInclude[T <: Element2](sourceArg: T, // container with ElementBuffer
      newValueArg: Element2,
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, null, newValueArg) with Event {
      /** Ancestors */
      val ancestorRefs = source.asInstanceOf[T].eAncestorReferences
    }
    /**
     * The event generated by container ElementBuffer when exists child removed
     */
    case class ChildRemove[T <: Element2](sourceArg: T, // container with ElementBuffer
      oldValueArg: Element2,
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, oldValueArg, null) with Event {
      /** Ancestors */
      val ancestorRefs = source.asInstanceOf[T].eAncestorReferences
    }
    /**
     * The event generated by container ElementBuffer when exists child replaced
     */
    case class ChildReplace[T <: Element2](sourceArg: T, // container with ElementBuffer
      oldValueArg: Element2, newValueArg: Element2,
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, oldValueArg, newValueArg) with Event {
      /** Ancestors */
      val ancestorRefs = source.asInstanceOf[T].eAncestorReferences
    }
    /**
     * The event generated by container ElementBuffer when buffer cleared
     */
    case class ChildrenReset[T <: Element2](sourceArg: T, // container with ElementBuffer
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, null, null) with Event {
      /** Ancestors */
      val ancestorRefs = source.asInstanceOf[T].eAncestorReferences
    }
    case class ValueInclude[T <: Element2](sourceArg: T,
      newValueArg: Value[_ <: AnyRef with java.io.Serializable],
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, null, newValueArg) with Event {
      /** Ancestors */
      val ancestorRefs = source.asInstanceOf[T].eAncestorReferences
    }
    case class ValueRemove[T <: Element2](sourceArg: T,
      oldValueArg: Value[_ <: AnyRef with java.io.Serializable],
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, oldValueArg, null) with Event {
      /** Ancestors */
      val ancestorRefs = source.asInstanceOf[T].eAncestorReferences
    }
    case class ValueUpdate[T <: Element2](sourceArg: T,
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
    case class StashReplace[T <: Element2](sourceArg: T, // container with stash
      oldValueArg: Stash, newValueArg: Stash,
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(sourceArg, sourceArg.eId.name, oldValueArg, newValueArg) with Event {
      /** Ancestors */
      val ancestorRefs = source.asInstanceOf[T].eAncestorReferences
    }
    /**
     * The event generated by model singleton when model replaced
     */
    /*case class ModelReplace[A <: Model.Interface[_ <: Model.Stash], B <: Model.Interface[_ <: Model.Stash]](
      oldValueArg: B, // previous model
      newValueArg: A, // current model
      val modified: Element.Timestamp)(val undoF: () => Unit)
      extends PropertyChangeEvent(newValueArg, null, oldValueArg, newValueArg) with Event {
//      extends PropertyChangeEvent(newValueArg, newValueArg.eId.name, oldValueArg, newValueArg) with Event {
      /** Ancestors */
      val ancestorRefs = null//source.asInstanceOf[Model.Generic].eAncestorReferences
    }*/
    //    case class Mark(val id: UUID = UUID.randomUUID)
    //      extends PropertyChangeEvent(Model.inner, null, null, null) with Event {
    //      extends PropertyChangeEvent(Model.inner, Model.inner.eId.name, null, null) with Event {
    //      /** Modification timestamp */
    //      val modified: Element.Timestamp = Element.timestamp()
    //     /** Undo the last operation function container. */
    //    val undoF: () => Unit = () => {}
    //    /** Ancestors */
    //    val ancestorRefs = null//Model.inner.eAncestorReferences

    //    override def toString() = s"Mark($modified)"
    //  }
  }*/
  /** Emulate mutable behavior for immutable element. */
  abstract class Mutable[A <: Element](@volatile protected var element: A) extends Equals {
    def immutable() = element

    /** Copy constructor */
    def copy(stash: A#StashType): A = {
      val e = element
      element = e.eCopy(e.eBox.asInstanceOf[ElementBox[e.ElementType]], stash.asInstanceOf[e.ElementType#StashType]).asInstanceOf[A]
      element
    }

    override def canEqual(that: Any) = that match {
      case thatMutable: Mutable[_] ⇒ element.canEqual(thatMutable.element)
      case _ ⇒ element.canEqual(that)
    }
    override def equals(that: Any): Boolean = that match {
      case thatMutable: Mutable[_] ⇒ element.equals(thatMutable.element)
      case _ ⇒ element.equals(that)
    }
    override def hashCode = element.##
  }
  object Mutable {
    implicit def mutable2immutable[A <: Element](m: Mutable[A]): A = m.element
  }
  /** The class that provides a marker for additional specialization of the element */
  abstract class Scope(val modificator: Symbol) extends java.io.Serializable with Equals {
    override def toString() = modificator.name

    def canEqual(other: Any): Boolean
    override def equals(other: Any) = other match {
      case that: Element.Scope ⇒ that.canEqual(this) && modificator == that.modificator
      case _ ⇒ false
    }
    override def hashCode() = modificator.##
  }
  /** Timestamp class */
  case class Timestamp(val milliseconds: Long, nanoShift: Long) extends Ordered[Timestamp] {
    /**
     * Result of comparing `this` with operand `that`.
     *
     * Returns `x` where:
     *
     *   - `-1 < 0` when `this < that`
     *
     *   - `0 == 0` when `this == that`
     *
     *   - `1 > 0` when  `this > that`
     *
     */
    def compare(that: Timestamp): Int =
      this.milliseconds compare that.milliseconds match {
        case 0 ⇒ this.nanoShift compare that.nanoShift
        case c ⇒ c
      }

    override def toString() = s"Timestamp[$milliseconds:$nanoShift]"
  }
}
