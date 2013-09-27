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

import scala.collection.immutable
import scala.util.DynamicVariable

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.dsl.DSLType.dsltype2implementation
import org.digimead.tabuddy.model.element.compare.Compare
import org.digimead.tabuddy.model.element.compare.CompareByTimespamp
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.ElementBox.box2interface
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Modifiable
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.serialization.Serialization

import scala.language.implicitConversions

/**
 * Base element
 * builded with the curiously recurring generic pattern
 * contains stash with actual data.
 */
trait Element extends Modifiable.Read with Equals with java.io.Serializable {
  this: Loggable ⇒
  /** Element type. */
  type ElementType <: Element
  /** Relative type. */
  type RelativeType <: Element.Relative[ElementType]
  /** Stash type. */
  type StashType <: Stash.Like
  /**
   * Element are always to know about it's box.
   * Box are always to know about it's element.
   *
   * When (box <-> element) will be unreachable then they will be GC'ed.
   */
  @transient val eBox: ElementBox[ElementType]

  /** Compares this object with the specified object for order. */
  def compare(that: Element): Int = Element.comparator.value.compare(this, that)
  /** Build an ancestors sequence. */
  def eAncestors: Seq[Node[_ <: Element]] = eNode.safeRead(_.ancestors)
  /**
   * As an optional instance of for Element
   *
   * for example:
   *   element.eAs[Task.Like]
   *   element.eAs[Note.Like]
   *   element.eAs[Record.Like]
   */
  def eAs[A <: Element: Manifest]: Option[A] =
    if (implicitly[Manifest[A]].runtimeClass.isAssignableFrom(getClass)) Some(this.asInstanceOf[A]) else None
  /** Get list of axes(tags). */
  def eCoordinate = eBox.coordinate
  /** Copy constructor. */
  def eCopy(elementBox: ElementBox[ElementType], stash: ElementType#StashType): ElementType = {
    val element = Element[ElementType](elementBox, stash)(Manifest.classType(getClass))
    elementBox.e = Some(element)
    element
  }
  /** Copy constructor that attach copy to the target node. */
  def eCopy(target: Node[ElementType], rawCoordinate: Axis.Generic*): ElementType =
    eCopy(target, Coordinate(rawCoordinate: _*))
  /** Copy constructor that attach copy to the target node. */
  /*
   * copy must preserve creation time
   * copy must preserve modification time if scope && properties content are the same
   */
  def eCopy(target: Node[ElementType], coordinate: Coordinate = eBox.coordinate, serialization: Serialization.Identifier = eBox.serialization): ElementType = {
    // asInstanceOf[ElementType#StashType] is simplify type between
    // abstract ElementType#StashType#StashType, ElementType#StashType, StashType
    target.safeWrite { target ⇒
      val stash = eStash.copy().asInstanceOf[ElementType#StashType]
      eCopy(target, coordinate, stash, serialization)
    }
  }
  /** Copy constructor that attach copy to the target node. */
  /*
   * copy must preserve creation time
   * copy must preserve modification time if scope && properties content are the same
   */
  def eCopy(target: Node[ElementType], coordinate: Coordinate, stash: ElementType#StashType, serialization: Serialization.Identifier): ElementType = {
    if (target == eNode && coordinate == eCoordinate && serialization == eBox.serialization)
      eCopy(eBox, stash)
    else
      target.safeWrite { target ⇒
        // asInstanceOf[ElementType#StashType] is simplify type between
        // abstract ElementType#StashType#StashType, ElementType#StashType, StashType
        if (target.rootBox == null && coordinate != Coordinate.root) {
          val root = ElementBox(Coordinate.root, stash.created, target, stash.modified, stash.scope, serialization)(Manifest.classType(getClass), stash.getClass)
          val projection = ElementBox(coordinate, target, serialization, stash.asInstanceOf[ElementType#StashType])(Manifest.classType(getClass))
          target.updateState(target.state.copy(projectionBoxes = target.state.projectionBoxes +
            (coordinate -> projection) + (Coordinate.root -> root)), stash.modified)
          projection
        } else {
          val box = ElementBox(coordinate, target, serialization, stash.asInstanceOf[ElementType#StashType])(Manifest.classType(getClass))
          target.updateBox(coordinate, box, stash.modified)
          box
        }
      }.e
  }
  /** Dump the element content. */
  def eDump(brief: Boolean, padding: Int = 2): String
  /** Find child element. */
  def eFind[A <: Element](p: A ⇒ Boolean)(implicit a: Manifest[A]): Option[A] = eNode.safeRead {
    _.view.map(_.projectionBoxes.values.toSeq: Seq[ElementBox[_ <: Element]]).flatten.
      find { box ⇒ box.node.elementType.runtimeClass.isAssignableFrom(a.runtimeClass) && p(box.e.asInstanceOf[A]) }.map(_.e.asInstanceOf[A])
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
  /** Get node/element graph. */
  def eGraph: Graph[_ <: Model.Like] = eBox.node.graph
  /** Get node/element verbose id. */
  def eId: Symbol = eBox.node.id
  /** Get Model for this element. */
  def eModel: Model.Like = eNode.graph.model
  /** Get element node. */
  def eNode: Node[ElementType] = eBox.node
  /** Get identifier which uniquely identify this element. */
  def eUniqueId: UUID = eBox.elementUniqueId
  /** Get graph origin identifier. */
  def eOrigin: Symbol = eBox.node.graph.origin
  /** Get a container. */
  def eParent: Option[Node[_ <: Element]] = eNode.parent
  /** Get reference of this element */
  def eReference = Reference(eOrigin, eNode.graph.node.unique, eNode.unique, eCoordinate)
  /** Get relative representation. */
  def eRelative: RelativeType
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
    eCopy(eNode, eCoordinate, this.eStash.copy(property = new Stash.Data).asInstanceOf[ElementType#StashType], eBox.serialization)
  }
  /** Get the root element from the current origin if any. */
  def eRoot: Element = eNode.rootBox.e
  /** Get the scope of the element */
  def eScope = eStash.scope
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
              val newValue = value
              previousValue match {
                case Some(previous) ⇒
                  val undoF = () ⇒ {}
                //Element.Event.publish(Element.Event.ValueUpdate(this, previous, value, eModified)(undoF))
                case None ⇒
                  val undoF = () ⇒ {}
                //Element.Event.publish(Element.Event.ValueInclude(this, value, eModified)(undoF))
              }
              eCopy(eBox, eStash.copy(modified = Element.timestamp(),
                property = eStash.property.updated(id, valueHash.updated(typeSymbol, newValue))).
                asInstanceOf[ElementType#StashType])
            case None ⇒
              val newValue = value
              val undoF = () ⇒ {}
              //Element.Event.publish(Element.Event.ValueInclude(this, value, eModified)(undoF))
              None
              eCopy(eBox, eStash.copy(modified = Element.timestamp(), property = eStash.property.updated(id,
                immutable.HashMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]](typeSymbol -> newValue))).
                asInstanceOf[ElementType#StashType])
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
              eCopy(eBox, eStash.copy(modified = Element.timestamp(),
                property = eStash.property.updated(id, (valueHash - typeSymbol))).asInstanceOf[ElementType#StashType])
            case None ⇒
              Element.this.asInstanceOf[ElementType]
          }
      }
    else
      throw new IllegalArgumentException(s"""Unable to set new value "${value}" to property "${id}" with unknown type symbol ${typeSymbol}.""")
  }
  /** Get current stash */
  def eStash: StashType
  /** Get modification timestamp. */
  def modified = eStash.modified

  /** Built in serialization helper. */
  protected def readObjectHelper() = {
    val eBoxField = getClass.getDeclaredField("eBox")
    if (!eBoxField.isAccessible())
      eBoxField.setAccessible(true)
    eBoxField.set(this, Serialization.stash.get)
    if (eBox == null)
      throw new IllegalStateException(s"Unable to adjust ${getClass.getName}")
  }
  override def canEqual(that: Any): Boolean = that.isInstanceOf[Element]
  override def equals(that: Any): Boolean = that match {
    case that: Element ⇒ // vs absolute
      (that eq this) ||
        ((that canEqual this) && this.## == that.##)
    case that: Element.Relative[_] ⇒ // vs relative
      (that.absolute eq this) ||
        ((that.absolute canEqual this) && this.## == that.absolute.##)
    case _ ⇒ false
  }
  override def hashCode() = lazyHashCode
  protected lazy val lazyHashCode = java.util.Arrays.hashCode(Array[AnyRef](this.eStash, eBox.elementUniqueId))
  override def toString() = "%s[%s@%s]".format(eStash.scope, eId.name, eCoordinate.toString)
}

object Element extends Loggable {
  implicit def relative2absolute[A <: Element](m: A#RelativeType): A = m.absolute.asInstanceOf[A]
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
    elementCtor.newInstance(stash, box).asInstanceOf[A]
  }
  /** Create a new element. */
  def apply[A <: Element](box: ElementBox[A], created: Element.Timestamp, modified: Element.Timestamp,
    property: Stash.Data, scope: A#StashType#ScopeType)(implicit a: Manifest[A], stashClass: Class[_ <: A#StashType]): A = {
    val elementGraph = box.node.graph
    val stashCtor = stashClass.getConstructors.find(_.getParameterTypes() match {
      case Array(createdArg, modifiedArg, dataArg, scopeArg) ⇒
        scopeArg.isAssignableFrom(scope.getClass) && createdArg.isAssignableFrom(created.getClass()) &&
          modifiedArg.isAssignableFrom(modified.getClass()) && dataArg.isAssignableFrom(property.getClass())
      case _ ⇒ false
    }) getOrElse {
      throw new NoSuchMethodException(s"Unable to find proper constructor for stash ${stashClass}.")
    }
    val stash = stashCtor.newInstance(created, modified, property, scope).asInstanceOf[A#StashType]
    apply[A](box, stash)
  }
  /** Create new timestamp object */
  def timestamp(ms: Long = System.currentTimeMillis(), ns: java.lang.Long = null) =
    if (ns == null) Timestamp(ms, System.nanoTime() - nanoBase) else Timestamp(ms, ns)

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
  abstract class Relative[A <: Element] private (protected val node: Node[A], protected val coordinate: Coordinate) extends Equals {
    def this(element: A) = this(element.eNode.asInstanceOf[Node[A]], element.eBox.coordinate)

    /** Get absolute element representation. */
    def absolute: A = if (coordinate.isRoot)
      node.safeRead(_.rootBox).asInstanceOf[ElementBox[A]].e
    else
      node.safeRead(_.projectionBoxes(coordinate)).asInstanceOf[ElementBox[A]].e
    /** Get list of axes(tags). */
    def eCoordinate = coordinate
    /** Get element node. */
    def eNode: Node[A] = node

    /** Copy constructor */
    def copy(stash: A#StashType): A = {
      val e = absolute
      e.eCopy(e.eNode, e.eBox.coordinate, stash.asInstanceOf[e.ElementType#StashType], e.eBox.serialization).asInstanceOf[A]
    }

    override def canEqual(that: Any) = that match {
      case that: Relative[_] ⇒ that.absolute.canEqual(this.absolute)
      case that: Element ⇒ that.canEqual(this.absolute)
      case _ ⇒ false
    }
    override def equals(that: Any): Boolean = that match {
      case that: Relative[_] if that canEqual this ⇒ this.absolute.equals(that.absolute)
      case that: Element if that canEqual this.absolute ⇒ this.absolute.equals(that)
      case _ ⇒ false
    }
    override def hashCode() = absolute.##
    override def toString() = s"R{${absolute}}"
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
