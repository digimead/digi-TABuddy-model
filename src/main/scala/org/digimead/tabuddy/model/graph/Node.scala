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
import java.util.concurrent.locks.ReentrantReadWriteLock

import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable
import scala.ref.WeakReference

import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element

import scala.language.implicitConversions

/**
 * Node is a brick of graph.
 */
trait Node[A <: Element] extends Modifiable.Write with Equals {
  /** Node element type. */
  val elementType: Manifest[A]
  /** Element verbose id. */
  val id: Symbol
  /** Node read/write lock. */
  protected val rwl = new ReentrantReadWriteLock()
  /** Node state information. Implementation MUST be volatile. */
  protected var internalState: NodeState[A]
  /** Node modification time. Implementation MUST be volatile. */
  protected var modificationTimestamp: Element.Timestamp
  /** Element system id. */
  val unique: UUID

  /** Copy this node and attach it to target. */
  def copy[B <: Element](target: Node.ThreadUnsafe[B], recursive: Boolean): Node[A] = safeWrite { currentNode ⇒
    implicit val m = elementType
    val copyNode = target.createChild[A](id, unique).safeWrite { copyNode ⇒
      val rootElementBox = internalState.rootElementBox.copy(node = copyNode)
      val projectionElementBoxes: Seq[(Coordinate, ElementBox[A])] = internalState.projectionElementBoxes.map {
        case (coordinate, box) ⇒ coordinate -> box.copy(node = copyNode)
      }.toSeq
      copyNode.updateState(rootElementBox = rootElementBox, projectionElementBoxes = immutable.HashMap(projectionElementBoxes: _*))
      copyNode
    }
    if (recursive)
      currentNode.iterator.foreach { _.copy(copyNode, recursive) }
    copyNode
  }
  /**
   * Lock this node and all child nodes.
   * Synchronization lock spread from the leaves and finish at the current node.
   */
  def freezeRead[B](f: Node.ThreadUnsafe[A] ⇒ B): B = {
    rwl.readLock().lock()
    try { internalState.children.foldLeft(f)((nestedFn, child) ⇒ child.freezeRead { child ⇒ (node) ⇒ nestedFn(node) })(this.asInstanceOf[Node.ThreadUnsafe[A]]) }
    finally { rwl.readLock().unlock() }
  }
  /**
   * Lock this node and all child nodes.
   * Synchronization lock spread from the leaves and finish at the current node.
   */
  def freezeWrite[B](f: Node.ThreadUnsafe[A] ⇒ B): B = {
    rwl.writeLock().lock()
    try { internalState.children.foldLeft(f)((nestedFn, child) ⇒ child.freezeRead { child ⇒ (node) ⇒ nestedFn(node) })(this.asInstanceOf[Node.ThreadUnsafe[A]]) }
    finally { rwl.writeLock().unlock() }
  }
  /** Get node element boxes. */
  def getElementBoxes(): Seq[ElementBox[A]] = {
    rwl.readLock().lock()
    try { Seq(internalState.rootElementBox) ++ internalState.projectionElementBoxes.values }
    finally { rwl.readLock().unlock() }
  }
  /** Get graph. */
  def graph(): Graph[_ <: Model.Like] = {
    rwl.readLock().lock()
    try { internalState.graph }
    finally { rwl.readLock().unlock() }
  }
  /** Get parent node. */
  def getParent(): Option[Node[_ <: Element]] = {
    rwl.readLock().lock()
    try { internalState.parentNodeReference.get }
    finally { rwl.readLock().unlock() }
  }
  /** Get element box at the specific coordinate. */
  def getProjection(coordinate: Coordinate): Option[ElementBox[A]] = {
    rwl.readLock().lock()
    try { if (coordinate == Coordinate.root) Option(internalState.rootElementBox) else internalState.projectionElementBoxes.get(coordinate) }
    finally { rwl.readLock().unlock() }
  }
  /** Get projection's element boxes. */
  def getProjections(): immutable.Map[Coordinate, ElementBox[A]] = {
    rwl.readLock().lock()
    try { immutable.Map((internalState.projectionElementBoxes.toSeq :+ (Coordinate.root, internalState.rootElementBox)): _*) }
    finally { rwl.readLock().unlock() }
  }
  /** Get root element box. */
  def getRootElementBox() = {
    rwl.readLock().lock()
    try { internalState.rootElementBox }
    finally { rwl.readLock().unlock() }
  }
  /** Get modification timestamp. */
  def modification: Element.Timestamp = modificationTimestamp
  /** Set modification timestamp. */
  def modification_=(arg: Element.Timestamp) = {
    modificationTimestamp = arg
    internalState.parentNodeReference.get.foreach(_.modificationUpdate(arg))
  }
  /** Update modification timestamp only if argument is greater than current value. */
  def modificationUpdate(arg: Element.Timestamp) = {
    if (modificationTimestamp < arg) modificationTimestamp = arg else modificationTimestamp = Element.timestamp()
    internalState.parentNodeReference.get.foreach(_.modificationUpdate(arg))
  }
  /**
   * Explicitly convert this trait to ThreadUnsafe.
   * Conversion prevents the consumer to access unguarded content.
   */
  def safeRead[B](f: Node.ThreadUnsafe[A] ⇒ B): B = {
    rwl.readLock().lock()
    try { f(this.asInstanceOf[Node.ThreadUnsafe[A]]) }
    finally { rwl.readLock().unlock() }
  }
  /**
   * Explicitly convert this trait to ThreadUnsafe.
   * Conversion prevents the consumer to access unguarded content.
   */
  def safeWrite[B](f: Node.ThreadUnsafe[A] ⇒ B): B = {
    rwl.writeLock().lock()
    try { f(this.asInstanceOf[Node.ThreadUnsafe[A]]) }
    finally { rwl.writeLock().unlock() }
  }
  /** Get internal state, */
  def state = internalState

  override def toString = s"graph.Node[${unique}(${id})]"
}

object Node extends Loggable {
  implicit def node2interface(g: Node.type): Interface = DI.implementation

  /** Simple implementation of the mutable part of a node. */
  class State[A <: Element](val children: Seq[Node[_ <: Element]], val graph: Graph[_ <: Model.Like], val parentNodeReference: WeakReference[Node[_ <: Element]],
    val projectionElementBoxes: immutable.HashMap[Coordinate, ElementBox[A]], val rootElementBox: ElementBox[A]) extends NodeState[A] {
    type NodeStateType = State[A]

    /** Copy constructor. */
    def copy(
      children: Seq[Node[_ <: Element]] = this.children,
      graph: Graph[_ <: Model.Like] = this.graph,
      parentNodeReference: WeakReference[Node[_ <: Element]] = this.parentNodeReference,
      projectionElementBoxes: immutable.HashMap[Coordinate, ElementBox[A]] = this.projectionElementBoxes,
      rootElementBox: ElementBox[A] = this.rootElementBox): NodeStateType =
      new State(children, graph, parentNodeReference, projectionElementBoxes, rootElementBox)
  }
  /**
   * Node companion object realization
   */
  trait Interface {
    /** Create common element node. */
    def apply[A <: Element: Manifest](id: Symbol, unique: UUID, state: NodeState[A], modificationTimestamp: Element.Timestamp): Node.ThreadUnsafe[A] =
      new ThreadUnsafe[A](id, unique, state, modificationTimestamp)
    /** Create model node. */
    def model[A <: Element: Manifest](id: Symbol, unique: UUID, modificationTimestamp: Element.Timestamp): Node.ThreadUnsafe[A] with ModelNodeInitializer =
      new ThreadUnsafe[A](id, unique, null, modificationTimestamp) with ModelNodeInitializer
    /** Dump the node structure. */
    def dump(node: Node[_ <: Element], brief: Boolean, padding: Int = 2): String = node.safeRead { node ⇒
      val pad = " " * padding
      def dumpBoxes() = {
        val result = Option(node.rootElementBox).map(box ⇒
          if (Coordinate.root != box.coordinate)
            Seq(s"Inconsistent coordinates node:${Coordinate.root} vs box:${box.coordinate} for ${box}!", box.toString)
          else
            Seq(box.toString)) ++
          node.projectionElementBoxes.map {
            case (coordinate, box) ⇒
              if (coordinate != box.coordinate)
                Seq(s"Inconsistent coordinates node:${coordinate} vs box:${box.coordinate} for ${box}!", box.toString)
              else
                Seq(box.toString)
          }
        if (result.nonEmpty) "\n " + pad + result.flatten.toSeq.mkString("\n " + pad) else ""
      }
      val boxes = if (brief) "" else dumpBoxes()
      val self = node + boxes + "\n"
      val childrenDump = node.children.map(Node.dump(_, brief, padding)).map(pad + _).mkString("\n\n").split("\n").map(pad + _).mkString("\n")
      (if (childrenDump.isEmpty) self else self + "\n" + childrenDump).trim
    }
  }
  /** Allow to initialize model node. */
  trait ModelNodeInitializer {
    this: Node[_ <: Element] ⇒
    /** Initialize model node. */
    def initializeModelNode[A <: Model.Like](graph: Graph[A], modification: Element.Timestamp) = {
      rwl.writeLock().lock()
      try {
        assert(internalState == null)
        internalState = new State(children = Seq(),
          graph = graph,
          parentNodeReference = WeakReference(null),
          projectionElementBoxes = immutable.HashMap(),
          rootElementBox = null)
        graph.nodes += this.unique -> this
      } finally { rwl.writeLock().unlock() }
    }
  }
  /**
   * Thread unsafe node representation.
   */
  class ThreadUnsafe[A <: Element](val id: Symbol, val unique: UUID, @volatile protected var internalState: NodeState[A],
    @volatile protected var modificationTimestamp: Element.Timestamp)(implicit val elementType: Manifest[A])
    extends Node[A] with mutable.Set[Node[_ <: Element]] with mutable.SetLike[Node[_ <: Element], ThreadUnsafe[_ <: Element]] {

    /** Adds a single element to the set. */
    def +=(elem: Node[_ <: Element]): this.type = { add(elem); this }
    /** Removes a single element to the set. */
    def -=(elem: Node[_ <: Element]): this.type = { remove(elem); this }
    /** Adds a single element to the set. */
    override def add(elem: Node[_ <: Element]): Boolean = {
      if (!internalState.children.contains(elem)) {
        /* process children */
        elem.safeWrite { node ⇒
          if (node.parentNodeReference.get != Some(ThreadUnsafe.this)) {
            // Update parent reference
            node.updateState(parentNodeReference = WeakReference(ThreadUnsafe.this))
            if (node.graph != graph) {
              // Update graph
              node.iteratorRecursive().foreach(_.safeWrite { child ⇒
                val rootElementBox = child.rootElementBox
                val projectionElementBoxes = child.projectionElementBoxes
                child.updateState(graph = graph,
                  parentNodeReference = WeakReference(ThreadUnsafe.this),
                  projectionElementBoxes = immutable.HashMap(projectionElementBoxes.
                    map { case (coordinate, box) ⇒ coordinate -> box.copy(node = child) }.toSeq: _*),
                  rootElementBox = rootElementBox.copy(Coordinate.root, child, rootElementBox.serialization))
              })
            }
          }
          /* add node */
          updateState(children = internalState.children :+ elem)
          graph.nodes += elem.unique -> elem
          node.iteratorRecursive().foreach { subChildNode ⇒ graph.nodes += subChildNode.unique -> subChildNode }
        }
        modification = Element.timestamp()
        /* notify */
        //val undoF = () => { super.remove(node); {} }
        //Element.Event.publish(Element.Event.ChildInclude(parentNode.get, node.get, parentNode.get.eModified)(undoF))
        true
      } else
        false
    }
    /** Build an ancestors sequence. */
    def ancestors(): Seq[Node[_ <: Element]] = {
      @tailrec
      def ancestors(current: Node[_ <: Element], acc: Seq[Node[_ <: Element]]): Seq[Node[_ <: Element]] = {
        if (acc.size > Element.MAXIMUM_DEEPNESS) {
          log.fatal("Node level is too deep: %s, ...".format(acc.take(10).mkString(", ")))
          return acc
        }
        current.getParent match {
          case n @ Some(parent) if acc.lastOption != n && parent != current ⇒ ancestors(parent, acc :+ parent)
          case _ ⇒ acc
        }
      }
      ancestors(this, Seq())
    }
    /** Clears the Node's contents. After this operation, there are no children. */
    override def clear() = {
      iteratorRecursive().foreach { node ⇒ graph.nodes -= node.unique }
      updateState(children = Seq())
      /* notify */
      //val undoF = () ⇒ { state.children.foreach(super.add) }
      //Element.Event.publish(Element.Event.ChildrenReset(parentNode.get, parentNode.get.eModified)(undoF))
    }
    /** Clone this node. */
    override def clone(): Node.ThreadUnsafe[A] = new Node.ThreadUnsafe[A](id, unique, internalState, this.modificationTimestamp)(elementType)
    /** Tests whether this set contains a given node as a children. */
    def contains(elem: Node[_ <: Element]): Boolean = internalState.children.contains(elem)
    /** Create new children node. */
    def createChild[B <: Element: Manifest](id: Symbol, unique: UUID): Node[B] = {
      internalState.children.foreach { child ⇒
        if (child.id == id)
          throw new IllegalArgumentException(s"Node with the same identifier '${id}' is already exists.")
        if (child.unique == unique)
          throw new IllegalArgumentException(s"Node with the same identifier '${unique}' is already exists.")
      }
      val newNode = Node[B](id, unique, new State(children = Seq(),
        graph = graph,
        parentNodeReference = WeakReference(this),
        projectionElementBoxes = immutable.HashMap(),
        rootElementBox = null), Element.timestamp())
      add(newNode)
      newNode
    }
    /**
     * The empty set of the same type as this set
     * @return  an empty set of type `Node`.
     */
    override def empty = new ThreadUnsafe(id, unique, internalState.copy(children = Seq()), Element.timestamp())
    /** Applies a function `f` to all children of this Node. */
    override def foreach[U](f: Node[_ <: Element] ⇒ U) = internalState.children.foreach(f)
    /** Creates a new iterator over all children contained in this node. */
    def iterator: Iterator[Node[_ <: Element]] = internalState.children.iterator
    /**
     * Creates a new iterator over all elements contained in this iterable object and it's children.
     * @param transformNodeChildren - function that provide sorting/filtering capability
     * @return the new iterator
     */
    def iteratorRecursive(transformNodeChildren: (Node[_ <: Element], Seq[Node[_ <: Element]]) ⇒ Iterator[Node[_ <: Element]] = (parent, children) ⇒ children.toIterator): Iterator[Node[_ <: Element]] = {
      new Iterator[Node[_ <: Element]] {
        val iterator: Iterator[Node[_ <: Element]] = buildIterator(transformNodeChildren(ThreadUnsafe.this, ThreadUnsafe.this.internalState.children))
        def hasNext: Boolean = iterator.hasNext
        def next(): Node[_ <: Element] = iterator.next
        private def buildIterator(iterator: Iterator[Node[_ <: Element]]): Iterator[Node[_ <: Element]] = {
          for (child ← iterator) yield child.safeRead { node ⇒ Iterator(child) ++ buildIterator(transformNodeChildren(node, node.internalState.children)) }
        }.foldLeft(Iterator[Node[_ <: Element]]())(_ ++ _)
      }
    }
    /** Removes a single element to the set. */
    override def remove(elem: Node[_ <: Element]): Boolean = if (internalState.children.contains(elem)) {
      /* remove node */
      elem.safeWrite { node ⇒
        updateState(children = internalState.children.filterNot(_.eq(elem)))
        graph.nodes -= elem.unique
        node.iteratorRecursive().foreach { subChildNode ⇒ graph.nodes -= subChildNode.unique }
      }
      /* notify */
      //val undoF = () => { super.add(node); {} }
      //Element.Event.publish(Element.Event.ChildRemove(parentNode.get, node.get, parentNode.get.eModified)(undoF))
      true
    } else
      false
    /** The number of children. */
    override def size: Int = internalState.children.size
    /** Update state of the current node. */
    def updateState(state: NodeState[A], modification: Element.Timestamp): Node.ThreadUnsafe[A] = {
      internalState = state
      if (modification != null)
        this.modification = modification
      this
    }
    /** Update state of the current node. */
    def updateState(children: Seq[Node[_ <: Element]] = this.internalState.children,
      graph: Graph[_ <: Model.Like] = this.internalState.graph,
      modification: Element.Timestamp = Element.timestamp(),
      parentNodeReference: WeakReference[Node[_ <: Element]] = this.internalState.parentNodeReference,
      projectionElementBoxes: immutable.HashMap[Coordinate, ElementBox[A]] = this.internalState.projectionElementBoxes,
      rootElementBox: ElementBox[A] = this.internalState.rootElementBox): Node.ThreadUnsafe[A] = {
      internalState = new State[A](children, graph, parentNodeReference, projectionElementBoxes, rootElementBox)
      if (modification != null)
        this.modification = modification
      this
    }
    /** Update element box at the specific coordinates. */
    def updateElementBox(coordinate: Coordinate, box: ElementBox[A], modification: Element.Timestamp = Element.timestamp()): Node.ThreadUnsafe[A] = {
      if (coordinate == Coordinate.root)
        internalState = internalState.copy(rootElementBox = box)
      else
        internalState = internalState.copy(projectionElementBoxes = internalState.projectionElementBoxes + (coordinate -> box))
      if (modification != null)
        this.modification = modification
      this
    }

    /*
     * I don't want separate nodes by type.
     * IMHO common user will be confused if there will be elements with different types and the same id.
     */
    override def canEqual(that: Any): Boolean = that.isInstanceOf[Node[_]]
    override def equals(other: Any) = other match {
      case that: Node[_] ⇒ that.canEqual(this) && this.## == that.##
      case _ ⇒ false
    }
    override def hashCode() = lazyHashCode
    protected lazy val lazyHashCode = java.util.Arrays.hashCode(Array[AnyRef](this.id, this.unique))

    override def toString = s"graph.Node[${unique};${id}]"
  }
  object ThreadUnsafe {
    implicit def threadUnsafe2data[A <: Element](tu: ThreadUnsafe[A]): NodeState[A] = tu.internalState
  }
  /**
   * Dependency injection routines.
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    lazy val implementation = injectOptional[Interface] getOrElse new AnyRef with Interface {}
  }
}
