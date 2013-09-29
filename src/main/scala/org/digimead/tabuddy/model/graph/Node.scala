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
import org.digimead.tabuddy.model.graph.ElementBox.box2interface

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
  /** Node modified time. Implementation MUST be volatile. */
  protected var modifiedTimestamp: Element.Timestamp
  /** Element system id. */
  val unique: UUID

  /** Build an ancestors sequence (head:this -> last:top parent). */
  def ancestors: Seq[Node[_ <: Element]] = {
    @tailrec
    def ancestors(current: Node[_ <: Element], acc: Seq[Node[_ <: Element]]): Seq[Node[_ <: Element]] = {
      if (acc.size > Element.MAXIMUM_DEEPNESS) {
        Node.log.fatal("Node level is too deep: %s, ...".format(acc.take(10).mkString(", ")))
        return acc
      }
      current.parent match {
        case n @ Some(parent) if acc.lastOption != n && parent != current ⇒ ancestors(parent, acc :+ parent)
        case _ ⇒ acc
      }
    }
    rwl.readLock().lock()
    try { ancestors(this, Seq()) }
    finally { rwl.readLock().unlock() }
  }
  /** Copy this node and attach it to target. */
  def copy[B <: Element](target: Node.ThreadUnsafe[B],
    recursive: Boolean = true,
    modified: Element.Timestamp = this.modified): Node[A] = safeRead { currentNode ⇒
    implicit val m = elementType
    val copyNode: Node.ThreadUnsafe[A] = Node[A](id, unique, new Node.State(children = Seq(),
      graph = target.graph,
      parentNodeReference = WeakReference(target.asInstanceOf[Node[_ <: Element]]),
      projectionBoxes = immutable.HashMap()), modified)
    val projectionBoxes: Seq[(Coordinate, ElementBox[A])] = currentNode.internalState.projectionBoxes.map {
      case (coordinate, box) ⇒ coordinate -> box.copy(node = copyNode)
    }.toSeq
    assert(copyNode.modified == modified)
    val children = if (recursive)
      currentNode.internalState.children.map { _.copy(copyNode, recursive) }.toSeq
    else
      currentNode.internalState.children
    copyNode.updateState(
      children = children,
      modified = null, // modification is already assigned
      projectionBoxes = immutable.HashMap(projectionBoxes: _*))
    currentNode.registerWithAncestors(copyNode)
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
  /** Get graph. */
  def graph: Graph[_ <: Model.Like] = {
    rwl.readLock().lock()
    try { internalState.graph }
    finally { rwl.readLock().unlock() }
  }
  /** Get modified timestamp. */
  def modified: Element.Timestamp = modifiedTimestamp
  /** Set modified timestamp. */
  def modified_=(arg: Element.Timestamp) = {
    modifiedTimestamp = arg
    internalState.parentNodeReference.get.foreach(_.updateModification(arg))
  }
  /** Get parent node. */
  def parent: Option[Node[_ <: Element]] = {
    rwl.readLock().lock()
    try { internalState.parentNodeReference.get }
    finally { rwl.readLock().unlock() }
  }
  /** Get or create projection. */
  def projection(coordinate: Coordinate): ElementBox[A] = {
    rwl.readLock().lock()
    (try { internalState.projectionBoxes.get(coordinate) } finally { rwl.readLock().unlock() }) getOrElse {
      rwl.writeLock().lock()
      // double check
      internalState.projectionBoxes.get(coordinate) getOrElse {
        try {
          ElementBox.getOrCreate[A](coordinate, this.asInstanceOf[Node.ThreadUnsafe[A]],
            internalState.rootBox.e.eScope, rootBox.serialization)(elementType, internalState.rootBox.e.eStash.getClass).eBox.asInstanceOf[ElementBox[A]]
        } finally { rwl.writeLock().unlock() }
      }
    }
  }
  /** Get projection's element boxes. */
  def projectionBoxes: immutable.HashMap[Coordinate, ElementBox[A]] = {
    rwl.readLock().lock()
    try { internalState.projectionBoxes }
    finally { rwl.readLock().unlock() }
  }
  /** Get root element box. */
  def rootBox = {
    rwl.readLock().lock()
    try { internalState.rootBox }
    finally { rwl.readLock().unlock() }
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
  def state = {
    rwl.readLock().lock()
    try { internalState }
    finally { rwl.readLock().unlock() }
  }
  /** Update modified timestamp only if argument is greater than current value. */
  def updateModification(arg: Element.Timestamp) = {
    if (modifiedTimestamp < arg) modifiedTimestamp = arg else modifiedTimestamp = Element.timestamp()
    internalState.parentNodeReference.get.foreach(_.updateModification(arg))
  }

  override def toString = s"graph.Node[${unique}(${id})]"
}

object Node extends Loggable {
  implicit def node2interface(g: Node.type): Interface = DI.implementation

  /** Simple implementation of the mutable part of a node. */
  class State[A <: Element](val children: Seq[Node[_ <: Element]], val graph: Graph[_ <: Model.Like], val parentNodeReference: WeakReference[Node[_ <: Element]],
    val projectionBoxes: immutable.HashMap[Coordinate, ElementBox[A]]) extends NodeState[A] {
    lazy val rootBox: ElementBox[A] = projectionBoxes.get(Coordinate.root).getOrElse(null)
    type NodeStateType = State[A]

    /** Copy constructor. */
    def copy(
      children: Seq[Node[_ <: Element]] = this.children,
      graph: Graph[_ <: Model.Like] = this.graph,
      parentNodeReference: WeakReference[Node[_ <: Element]] = this.parentNodeReference,
      projectionElementBoxes: immutable.HashMap[Coordinate, ElementBox[A]] = this.projectionBoxes): NodeStateType =
      new State(children, graph, parentNodeReference, projectionElementBoxes)
  }
  /**
   * Node companion object realization
   */
  trait Interface {
    /** Create common element node. */
    def apply[A <: Element: Manifest](id: Symbol, unique: UUID, state: NodeState[A], modifiedTimestamp: Element.Timestamp): Node.ThreadUnsafe[A] =
      new ThreadUnsafe[A](id, unique, state, modifiedTimestamp)
    /** Create model node. */
    def model[A <: Element: Manifest](id: Symbol, unique: UUID, modifiedTimestamp: Element.Timestamp): Node.ThreadUnsafe[A] with ModelNodeInitializer =
      new ThreadUnsafe[A](id, unique, null, modifiedTimestamp) with ModelNodeInitializer
    /** Dump the node structure. */
    def dump(node: Node[_ <: Element], brief: Boolean, padding: Int = 2): String = node.safeRead { node ⇒
      val pad = " " * padding
      def dumpBoxes() = {
        val result = Option(node.rootBox).map(box ⇒
          if (Coordinate.root != box.coordinate)
            Seq(s"Inconsistent coordinates node:${Coordinate.root} vs box:${box.coordinate} for ${box}!", box.toString)
          else
            Seq(box.toString)) ++
          node.projectionBoxes.filterNot(_._1.isRoot).map {
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
    def initializeModelNode[A <: Model.Like](graph: Graph[A], modified: Element.Timestamp) = {
      rwl.writeLock().lock()
      try {
        assert(internalState == null)
        internalState = new State(children = Seq(),
          graph = graph,
          parentNodeReference = WeakReference(null),
          projectionBoxes = immutable.HashMap())
        graph.nodes += this.unique -> this
        modifiedTimestamp = modified
      } finally { rwl.writeLock().unlock() }
    }
  }
  /**
   * Thread unsafe node representation.
   */
  class ThreadUnsafe[A <: Element](val id: Symbol, val unique: UUID, @volatile protected[Node] var internalState: NodeState[A],
    @volatile protected var modifiedTimestamp: Element.Timestamp)(implicit val elementType: Manifest[A])
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
              node.iteratorRecursive.foreach(_.safeWrite { child ⇒
                val projectionBoxes = child.projectionBoxes
                child.updateState(graph = graph,
                  parentNodeReference = WeakReference(ThreadUnsafe.this),
                  projectionBoxes = immutable.HashMap(projectionBoxes.
                    map { case (coordinate, box) ⇒ coordinate -> box.copy(node = child) }.toSeq: _*))
              })
            }
          }
          /* add node */
          updateState(children = internalState.children :+ elem)
          registerWithAncestors(node)
          node.iteratorRecursive.foreach { subChildNode ⇒ graph.nodes += subChildNode.unique -> subChildNode }
        }
        modified = Element.timestamp()
        true
      } else
        false
    }
    /** Clears the Node's contents. After this operation, there are no children. */
    override def clear() = {
      iteratorRecursive.foreach { node ⇒ graph.nodes -= node.unique }
      updateState(children = Seq())
      /* notify */
      //val undoF = () ⇒ {}
      //internalState.graph.publish(Event.ChildrenReset(this)(undoF))
    }
    /** Clone this node. */
    override def clone(): Node.ThreadUnsafe[A] = new Node.ThreadUnsafe[A](id, unique, internalState, this.modifiedTimestamp)(elementType)
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
        projectionBoxes = immutable.HashMap()), Element.timestamp())
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
     * Iteration order is parent1, child1OfParent1, childN, parent2, child1OfParent2, childN, parent3 ...
     * @return the new iterator
     */
    def iteratorRecursive: Iterator[Node[_ <: Element]] = new Iterator[Node[_ <: Element]] {
      private var stack = List[Iterator[Node[_ <: Element]]](internalState.children.iterator)
      def hasNext: Boolean = {
        while (!stack.head.hasNext && stack.size > 1)
          stack = stack.tail
        stack.head.hasNext
      }
      def next(): Node[_ <: Element] = if (hasNext) {
        val nextChild = stack.head.next
        stack = nextChild.internalState.children.iterator +: stack
        nextChild
      } else Iterator.empty.next()
    }
    /** Removes a single element to the set. */
    override def remove(elem: Node[_ <: Element]): Boolean = if (internalState.children.contains(elem)) {
      /* remove node */
      elem.safeWrite { node ⇒
        updateState(children = internalState.children.filterNot(_.eq(elem)))
        graph.nodes -= elem.unique
        node.iteratorRecursive.foreach { subChildNode ⇒ graph.nodes -= subChildNode.unique }
      }
      /* notify */
      //val undoF = () ⇒ {}
      //internalState.graph.publish(Event.ChildRemove(this, elem)(undoF))
      true
    } else
      false
    /** The number of children. */
    override def size: Int = internalState.children.size
    /** Update state of the current node. */
    def updateState(state: NodeState[A], modified: Element.Timestamp): Node.ThreadUnsafe[A] = {
      internalState = state
      if (modified != null)
        this.modified = modified
      this
    }
    /** Update state of the current node. */
    def updateState(children: Seq[Node[_ <: Element]] = this.internalState.children,
      graph: Graph[_ <: Model.Like] = this.internalState.graph,
      modified: Element.Timestamp = Element.timestamp(),
      parentNodeReference: WeakReference[Node[_ <: Element]] = this.internalState.parentNodeReference,
      projectionBoxes: immutable.HashMap[Coordinate, ElementBox[A]] = this.internalState.projectionBoxes): Node.ThreadUnsafe[A] = {
      internalState = new State[A](children, graph, parentNodeReference, projectionBoxes)
      if (modified != null)
        this.modified = modified
      this
    }
    /** Update element box at the specific coordinates. */
    def updateBox(coordinate: Coordinate, box: ElementBox[A], modified: Element.Timestamp = Element.timestamp()): Node.ThreadUnsafe[A] = {
      internalState = internalState.copy(projectionBoxes = internalState.projectionBoxes + (coordinate -> box))
      if (modified != null)
        this.modified = modified
      this
    }

    /** Register node and ancestors at graph. */
    protected[Node] def registerWithAncestors(node: Node.ThreadUnsafe[_ <: Element]) {
      if (!node.internalState.graph.nodes.contains(node.unique)) {
        node.internalState.parentNodeReference.get.foreach(_.safeRead(registerWithAncestors))
        node.internalState.graph.nodes += node.unique -> node.asInstanceOf[Node[_ <: Element]]
      }
    }

    /*
     * I don't want separate nodes by type.
     * IMHO common user will be confused if there will be elements with different types and the same id.
     */
    override def canEqual(that: Any): Boolean = that.isInstanceOf[Node[_]]
    override def equals(other: Any) = other match {
      case that: Node[_] ⇒ (that eq this) ||
        (that.canEqual(this) && this.## == that.## && this.modified == that.modified && this.elementType == that.elementType)
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
