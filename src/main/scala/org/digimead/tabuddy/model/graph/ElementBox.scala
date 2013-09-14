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
import java.util.concurrent.atomic.AtomicReference

import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Reference
import org.digimead.tabuddy.model.serialization.Serialization

import language.implicitConversions

/**
 * ElementBox class holds element instance.
 * It is suitable for lazy loading or for SoftReference containers.
 *
 * @param coordinate element coordinate
 * @param elementUniqueId identifier which is uniquely identify the specific element
 * @param initial initial element location or value, which is erased after save
 * @param node container of the element box
 * @param serialization type of the serialization mechanism
 * @param unmodified the modification timestamp of the unmodified element
 * @param elementType manifest with an element type
 */
/*
 * please, note => on 'initial' field
 * this is allow to pass AtomicReference as forward reference:
 *
 * val ref = new AtomicReference[MyElement](null)
 * ElementBox(..., ref, ...)
 * ref.set(element)
 *
 * ref.get or any other accessor will trigger an initialization of lazy 'initial' value
 */
/*
 * ElementBox contains strong reference to Node.
 * It's looks like: Node (<- ElementBox <-> Element)
 * (<- ElementBox <-> Element) is CG'ed part.
 */
abstract class ElementBox[A <: Element](val coordinate: Coordinate, val elementUniqueId: UUID, initial: ⇒ Either[URI, A],
  val node: Node[A], val serialization: Serialization.Identifier, val unmodified: Element.Timestamp) extends Equals {
  def this(coordinate: Coordinate, elementUniqueId: UUID, initial: Either[URI, AtomicReference[A]], node: Node[A],
    serialization: Serialization.Identifier, unmodified: Element.Timestamp) =
    this(coordinate, elementUniqueId, initial match {
      case initial @ Left(baseURIForLazyLoading) ⇒ Left(baseURIForLazyLoading)
      case Right(explicitElement) ⇒ Right(explicitElement.get)
    }, node, serialization, unmodified)

  /** Get reference of this element. */
  def eReference(): Reference = Reference(node.graph.origin, node.unique, coordinate)
  /** Current box context */
  def context(): ElementBox.Context
  /** Copy current element box. */
  def copy(coordinate: Coordinate = this.coordinate,
    node: Node[A] = this.node,
    serialization: Serialization.Identifier = this.serialization): ElementBox[A] = {
    val element = get()
    val elementElementForwardReference = new AtomicReference[A](null.asInstanceOf[A])
    val elementBox = ElementBox[A](coordinate, UUID.randomUUID(), Right(elementElementForwardReference),
      element.modification)(elementType = node.elementType, node = node, serialization = serialization)
    val elementCopy = element.eCopy(elementBox.asInstanceOf[ElementBox[element.ElementType]],
      element.eStash.asInstanceOf[element.ElementType#StashType]).asInstanceOf[A]
    elementElementForwardReference.set(elementCopy)
    elementBox.get
    elementBox
  }
  /** Get element. */
  def get(): A
  /** Get modified element. Returns only if value was modified. */
  def getModified(): Option[A]
  /** (Re)Load element. */
  def load(): A
  /** Save element. */
  def save(): Array[Byte] = Serialization.perIdentifier.get(serialization) match {
    case Some(serialization) ⇒ serialization.save(get)
    case None ⇒ throw new IllegalStateException(s"Serialization for ${serialization} not found.")
  }
  /** Set element. */
  def set(arg: A)

  override def canEqual(that: Any): Boolean = that.isInstanceOf[ElementBox[_]]
  override def equals(that: Any): Boolean = that match {
    case that: ElementBox[_] ⇒ (that eq this) || ((that canEqual this) && this.## == that.##)
    case _ ⇒ false
  }
  override def hashCode() = lazyHashCode
  protected lazy val lazyHashCode = java.util.Arrays.hashCode(Array[AnyRef](this.coordinate, this.elementUniqueId,
    this.unmodified, this.node, this.serialization))
  override def toString = s"graph.Box[${context}:${coordinate}/${node.elementType};${node.id}]"
}

object ElementBox {
  implicit def box2interface(g: ElementBox.type): Interface = DI.implementation

  /** Element box context information */
  class Context(
    /** Context file. */
    val file: Option[URI],
    /** Context file digest. */
    val digest: Option[String]) extends Equals {

    /** Copy constructor. */
    def copy(file: Option[URI] = this.file, digest: Option[String] = this.digest) = new Context(file, digest)

    override def canEqual(that: Any) = that.isInstanceOf[Context]
    override def equals(that: Any): Boolean = (this eq that.asInstanceOf[Object]) || (that match {
      case that: Context if this.## == that.## ⇒ that canEqual this
      case _ ⇒ false
    })
    override def hashCode() = lazyHashCode
    protected lazy val lazyHashCode = java.util.Arrays.hashCode(Array[AnyRef](file, digest))
    override def toString() = "Context[%s:%s]".format(digest.getOrElse("-"), file.getOrElse("-"))
  }
  /**
   *  Element box companion interface
   */
  trait Interface {
    /**
     * Create element box with specific parameters.
     *
     * @param coordinate element coordinate
     * @param elementUniqueId identifier which is uniquely identify the specific element
     * @param initial initial element value, which is erased after save
     * @param node container of the element box
     * @param serialization type of the serialization mechanism
     * @param unmodified the modification timestamp of unmodified element
     * @param elementType manifest with element type
     */
    def apply[A <: Element](coordinate: Coordinate, elementUniqueId: UUID, initial: Either[URI, AtomicReference[A]],
      unmodified: Element.Timestamp)(implicit elementType: Manifest[A], node: Node[A], serialization: Serialization.Identifier): ElementBox[A] = {
      val boxClass = DI.elementBoxClass
      val newBoxCtor = boxClass.getConstructor(
        classOf[Coordinate],
        classOf[UUID],
        classOf[Either[_, _]],
        classOf[Node[_]],
        classOf[Serialization.Identifier],
        classOf[Element.Timestamp],
        classOf[Manifest[_]])
      newBoxCtor.newInstance(coordinate, elementUniqueId, initial, node, serialization, unmodified, elementType).asInstanceOf[ElementBox[A]]
    }
    /** Create element box except element and assign it to node. */
    def apply[A <: Element](coordinate: Coordinate, elementUniqueId: UUID, node: Node.ThreadUnsafe[A], base: URI,
      serialization: Serialization.Identifier, unmodified: Element.Timestamp)(implicit m: Manifest[A]): ElementBox[A] = {
      apply[A](coordinate, elementUniqueId, Left(base), unmodified)(m, node, serialization)
    }
    /** Create element, element box with specific parameters. */
    def apply[A <: Element](coordinate: Coordinate, created: Element.Timestamp, elementNode: Node.ThreadUnsafe[A],
      modified: Element.Timestamp, scope: A#StashType#ScopeType, serialization: Serialization.Identifier)(implicit m: Manifest[A],
        stashClass: Class[_ <: A#StashType]): ElementBox[A] = {
      // create root or projection element box
      val elementElementForwardReference = new AtomicReference[A](null.asInstanceOf[A])
      val elementBox = apply[A](coordinate, UUID.randomUUID(), Right(elementElementForwardReference), modified)(m, elementNode, serialization)
      // create element.
      val element = Element.apply[A](elementBox, created, modified, new org.digimead.tabuddy.model.element.Stash.Data, scope)
      elementElementForwardReference.set(element)
      if (elementBox.get == null)
        throw new IllegalStateException(s"${element} element is absent.")
      elementBox
    }
    /** Create element, element box with specific stash. */
    def apply[A <: Element](coordinate: Coordinate, elementNode: Node.ThreadUnsafe[A],
      serialization: Serialization.Identifier, stash: A#StashType)(implicit m: Manifest[A]): ElementBox[A] = {
      // create root or projection element box
      val elementElementForwardReference = new AtomicReference[A](null.asInstanceOf[A])
      val elementBox = apply[A](coordinate, UUID.randomUUID(), Right(elementElementForwardReference), stash.modificationTimestamp)(m, elementNode, serialization)
      // create element.
      val element = Element.apply[A](elementBox, stash)
      elementElementForwardReference.set(element)
      if (elementBox.get == null)
        throw new IllegalStateException(s"${element} element is absent.")
      elementBox
    }
    /** Get or create new element box at specific coordinates. */
    def getOrCreate[A <: Element](coordinate: Coordinate, node: Node.ThreadUnsafe[A], scope: A#StashType#ScopeType,
      serialization: Serialization.Identifier)(implicit m: Manifest[A], stashClass: Class[_ <: A#StashType]): A = {
      val requiredBox = if (coordinate == Coordinate.root)
        Option(node.rootElementBox)
      else
        node.projectionElementBoxes.get(coordinate)
      requiredBox.map(box ⇒ box.get.eAs[A].
        getOrElse { throw new IllegalAccessException(s"Found ${box.get}, but it type isn't ${m.runtimeClass}.") }).
        getOrElse {
          val timestamp = Element.timestamp()
          val elementBox = for {
            parent ← node.parentNodeReference.get
          } yield {
            // create root element before projection one
            if (node.rootElementBox == null && coordinate != Coordinate.root) {
              val root = ElementBox[A](Coordinate.root, timestamp, node, timestamp, scope, serialization)
              val projection = ElementBox[A](coordinate, timestamp, node, timestamp, scope, serialization)
              node.updateState(node.state.copy(projectionElementBoxes = node.state.projectionElementBoxes + (coordinate -> projection),
                rootElementBox = root), timestamp)
              projection
            } else {
              val box = ElementBox[A](coordinate, timestamp, node, timestamp, scope, serialization)
              node.updateElementBox(coordinate, box, timestamp)
              box
            }
          }
          elementBox match {
            case Some(box) ⇒ box.get
            case None ⇒ throw new IllegalStateException("Unable to create element box.")
          }
        }
    }
  }
  /**
   * Hard element reference that always contains element instance.
   */
  class Hard[A <: Element](coordinate: Coordinate, elementUniqueId: UUID, initial: ⇒ Either[URI, A], node: Node[A],
    serialization: Serialization.Identifier, unmodified: Element.Timestamp)(implicit elementType: Manifest[A])
    extends ElementBox[A](coordinate, elementUniqueId, initial, node, serialization, unmodified) {
    def this(coordinate: Coordinate, elementUniqueId: UUID, initial: Either[URI, AtomicReference[A]], node: Node[A],
      serialization: Serialization.Identifier, unmodified: Element.Timestamp)(implicit elementType: Manifest[A]) =
      this(coordinate, elementUniqueId, initial match {
        case initial @ Left(baseURIForLazyLoading) ⇒ Left(baseURIForLazyLoading)
        case Right(explicitElement) ⇒ Right(explicitElement.get)
      }, node, serialization, unmodified)(elementType)
    /** Element object. */
    @volatile protected var cached: Option[A] = None

    /** Current box context. */
    def context(): ElementBox.Context = new ElementBox.Context(None, None)
    /** Get modification timestamp of unmodified element. */
    /*
     * Hard element box is always contains the initial data.
     * The simplest way is just to overwrite it. Returns 0 timestamp.
     */
    def get(): A = cached getOrElse load()
    def getModified(): Option[A] = initial.right.toOption
    def load(): A = synchronized {
      initial match {
        case Right(element) ⇒
          // Element is explicitly passed to this box
          cached = Some(element)
          element
        case Left(baseURIForLazyLoading) ⇒
          // Base URI that is passed instead of element.
          val element = Serialization.perScheme.get(baseURIForLazyLoading.getScheme()) match {
            case Some(transport) ⇒
              transport.acquireElement(this, baseURIForLazyLoading)
            case None ⇒
              throw new IllegalArgumentException(s"Transport for the specified scheme '${baseURIForLazyLoading.getScheme()}' not found.")
          }
          cached = Some(element)
          element
      }
    }
    def set(arg: A) = throw new UnsupportedOperationException

    override def toString = s"graph.Box$$Hard[${coordinate}/${elementType};${node.id}]"
  }
  /**
   * Soft element reference that contains element only while there is enough memory (RAM) available.
   * And reload it if required.
   */
  // Implement if needed with SoftReference
  /**
   * Dependency injection routines.
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    /** The local origin that is alias of a user or a system or an anything other */
    lazy val elementBoxClass = injectOptional[Class[_ <: ElementBox[_]]] getOrElse classOf[Hard[_]]
    lazy val implementation = injectOptional[Interface] getOrElse new AnyRef with Interface {}
  }
}
