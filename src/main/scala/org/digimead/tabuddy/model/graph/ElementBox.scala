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

import java.util.UUID
import java.util.concurrent.atomic.AtomicReference

import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Reference
import org.digimead.tabuddy.model.serialization.Serialization
import org.digimead.tabuddy.model.Model

import language.implicitConversions

/**
 * ElementBox class holds element instance.
 * It is suitable for lazy loading or for SoftReference containers.
 *
 * @param elementUniqueId element unique ID
 * @param elementContext element context
 * @param initial initial element value, which is erased after save
 * @param elementType manifest with element type
 * @param node reference to container
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
abstract class ElementBox[A <: Element](val context: Context, val coordinate: Coordinate,
  initial: => Option[A])(implicit val elementType: Manifest[A], val node: Node, val serialization: Serialization[_]) {
  def this(context: Context, coordinate: Coordinate, initial: AtomicReference[A])(implicit elementType: Manifest[A],
    node: Node, serialization: Serialization[_]) = this(context, coordinate, Option(initial.get))(elementType, node, serialization)
  /** Get reference of this element. */
  def eReference(): Option[Reference] = for {
    graph <- node.getGraph
  } yield Reference(graph.origin, node.unique, coordinate)
  /** Get element. */
  def get(): A
  /** (Re)Load element. */
  def load(): A
  /** Save element. */
  def save()
  /** Set element. */
  def set(arg: A)

  override def toString = s"graph.Box[${context}:${coordinate}/${elementType}]"
}

object ElementBox {
  implicit def box2interface(g: ElementBox.type): Interface = DI.implementation

  trait Interface {
    /** Create element box with specific parameters. */
    def apply[A <: Element](context: Context, coordinate: Coordinate, initial: AtomicReference[A])(implicit elementType: Manifest[A],
      node: Node, serialization: Serialization[_]): ElementBox[A] = {
      val boxClass = DI.elementBoxClass
      val newBoxCtor = boxClass.getConstructor(
        classOf[Context],
        classOf[Coordinate],
        classOf[AtomicReference[_]],
        classOf[Manifest[_]],
        classOf[Node],
        classOf[Serialization[_]])
      newBoxCtor.newInstance(context, coordinate, initial, elementType, node, serialization).asInstanceOf[ElementBox[A]]
    }
    /** Create element, element box with specific parameters and assign it to node. */
    def apply[A <: Element](context: Context, coordinate: Coordinate, created: Element.Timestamp, elementNode: Node.ThreadUnsafe,
      modified: Element.Timestamp, scope: A#StashType#ScopeType, serialization: Serialization[_])(implicit m: Manifest[A],
        stashClass: Class[_ <: A#StashType]): ElementBox[A] = {
      /*
       * Validate
       */
      Option(elementNode.rootElementBox).foreach { targetRootElement =>
        // addition subtype(A) to supertype(that) is legal
        if (!m.runtimeClass.isAssignableFrom(targetRootElement.elementType.runtimeClass))
          throw new IllegalArgumentException(s"Unable to mix ${m} with ${targetRootElement.elementType}.")
      }
      for {
        graphOrigin <- elementNode.graph.map(_.origin)
        parentUnique <- elementNode.parentNode.map(_.unique)
      } {
        if (context.origin != graphOrigin)
          throw new IllegalArgumentException(s"Unable to create element box. Context origin: ${context.origin}. Node origin ${graphOrigin}")
        if (context.unique != parentUnique)
          throw new IllegalArgumentException(s"Unable to create element box. Context unique: ${context.unique}. Parent node unique ${parentUnique}")
      }
      /*
       * Create
       */
      // create root element before projection one
      if (elementNode.rootElementBox == null && coordinate != Coordinate.root)
        apply(context, Coordinate.root, created, elementNode, modified, scope, serialization)
      // create root or projection element
      val elementElementForwardReference = new AtomicReference[A](null.asInstanceOf[A])
      val elementBox = apply[A](context, coordinate, elementElementForwardReference)(m, elementNode, serialization)
      if (coordinate == Coordinate.root)
        elementNode.rootElementBox = elementBox
      else
        elementNode.projectionElementBoxes(coordinate) = elementBox
      // 3rd. Create element.
      val element = Element.apply[A](elementBox, created, modified, new org.digimead.tabuddy.model.element.Stash.Data, scope)
      elementElementForwardReference.set(element)
      elementBox.get
      if (elementBox.get == null)
        throw new IllegalStateException(s"${element} element is absent.")
      elementBox
    }
    /** Create element, element box with specific stash and assign it to node. */
    def apply[A <: Element](context: Context, elementNode: Node.ThreadUnsafe, serialization: Serialization[_], stash: A#StashType)(implicit m: Manifest[A]): ElementBox[A] = {
      /*
       * Validate
       */
      Option(elementNode.getRootElementBox).foreach { targetRootElement =>
        // addition subtype(A) to supertype(that) is legal
        if (!m.runtimeClass.isAssignableFrom(targetRootElement.elementType.runtimeClass))
          throw new IllegalArgumentException(s"Unable to mix ${m} with ${targetRootElement.elementType}.")
      }
      for {
        graphOrigin <- elementNode.graph.map(_.origin)
        parentUnique <- elementNode.parentNode.map(_.unique)
      } {
        if (context.origin != graphOrigin)
          throw new IllegalArgumentException(s"Unable to create element box. Context origin: ${context.origin}. Node origin ${graphOrigin}.")
        if (context.unique != parentUnique)
          throw new IllegalArgumentException(s"Unable to create element box. Context unique: ${context.unique}. Parent node unique ${parentUnique}.")
      }
      if (context.origin != stash.origin)
        throw new IllegalArgumentException(s"Unable to create element box. Stash origin: ${stash.origin}. Node origin ${context.origin}.")
      if (elementNode.unique != stash.unique)
        throw new IllegalArgumentException(s"Unable to create element box. Stash unique: ${stash.unique}. Node unique ${elementNode.unique}.")
      if (elementNode.id != stash.id)
        throw new IllegalArgumentException(s"Unable to create element box. Stash id: ${stash.id}. Node id ${elementNode.id}.")
      /*
       * Create
       */
      // create root element before projection one
      if (elementNode.getRootElementBox == null && stash.coordinate != Coordinate.root) {
        implicit val stashClass = stash.getClass
        apply(context, Coordinate.root, stash.created, elementNode, stash.modified, stash.scope, serialization)
      }
      // create root or projection element
      val elementElementForwardReference = new AtomicReference[A](null.asInstanceOf[A])
      val elementBox = apply[A](context, stash.coordinate, elementElementForwardReference)(m, elementNode, serialization)
      if (stash.coordinate == Coordinate.root)
        elementNode.rootElementBox = elementBox
      else
        elementNode.projectionElementBoxes(stash.coordinate) = elementBox
      // 3rd. Create element.
      val element = Element.apply[A](elementBox, stash)
      elementElementForwardReference.set(element)
      elementBox.get
      if (elementBox.get == null)
        throw new IllegalStateException(s"${element} element is absent.")
      elementBox
    }
    /** Get or create new element box at specific coordinates. */
    def getOrCreate[A <: Element](coordinate: Coordinate, node: Node.ThreadUnsafe, scope: A#StashType#ScopeType,
      serialization: Serialization[_])(implicit m: Manifest[A], stashClass: Class[_ <: A#StashType]): A = {
      val requiredBox = if (coordinate == Coordinate.root)
        Option(node.rootElementBox)
      else
        node.projectionElementBoxes.get(coordinate)
      requiredBox.map(box => box.get.eAs[A].
        getOrElse { throw new IllegalAccessException(s"Found ${box.get}, but it type isn't ${m.runtimeClass}.") }).
        getOrElse {
          val timestamp = Element.timestamp()
          val elementBox = for {
            parent <- node.parentNode
            graph <- node.graph
          } yield ElementBox[A](Context(graph.origin, parent.unique), coordinate, timestamp, node, timestamp, scope, serialization)
          elementBox.getOrElse { throw new IllegalStateException("Unable to create element box.") }.get
        }
    }
  }
  /**
   * Hard element reference that always contain element instance.
   */
  class Hard[A <: Element](context: Context, coordinate: Coordinate,
    initial: => A)(implicit elementType: Manifest[A], node: Node, serialization: Serialization[_])
    extends ElementBox[A](context, coordinate, Option(initial)) {
    def this(context: Context, coordinate: Coordinate, initial: AtomicReference[A])(implicit elementType: Manifest[A],
      node: Node, serialization: Serialization[_]) = this(context, coordinate, initial.get)(elementType, node, serialization)
    def get(): A = initial
    def load(): A = initial
    def save() {}
    def set(arg: A) = throw new UnsupportedOperationException

    override def toString = s"graph.Box$$Hard[${context}://${coordinate}/${elementType}]"
  }
  /**
   * Dependency injection routines.
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    /** The local origin that is alias of a user or a system or an anything other */
    lazy val elementBoxClass = injectOptional[Class[_ <: ElementBox[_]]] getOrElse classOf[Hard[_]]
    lazy val implementation = injectOptional[Interface] getOrElse new AnyRef with Interface {}
  }
}
