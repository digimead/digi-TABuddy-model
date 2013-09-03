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

import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.annotation.migration
import scala.collection.immutable
import scala.collection.mutable
import scala.ref.WeakReference

import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element

import scala.language.implicitConversions

/**
 * Node is a brick of graph.
 */
trait Node {
  /** Children nodes. */
  protected val children: NodeChildren
  /** Element verbose id. */
  val id: Symbol
  /** Graph to which the node belongs. */
  protected var graphReference: Option[Graph[_ <: Model.Like]] = None
  /** Global modification time, based on node elements and children state. */
  @volatile var modified: Element.Timestamp = Element.timestamp()
  /** Parent node. */
  // If we have the only strong reference to this node and
  // graph may contains a model node with weak references
  // then all other nodes will be GC'ed and we may work only with our part of tree.
  protected var parentNodeReference = WeakReference[Node](null)
  /** Elements with non-root coordinates(List of axes(tags)). */
  protected val projectionElementBoxes: mutable.HashMap[Coordinate, ElementBox[_ <: Element]]
  /** Element with root coordinate. */
  // Option is meaningless here. Root element must be defined every time.
  protected var rootElementBox: ElementBox[_ <: Element]

  /** Element system id. */
  val unique: UUID

  /** Copy this node and attach it to target. */
  def copy(target: Node.ThreadUnsafe, recursive: Boolean): Node = threadSafe { currentNode ⇒
    val targetGraph = target.graph.getOrElse { throw new IllegalArgumentException(s"${target} graph is unreachable.") }
    val copyNode = target.createChild(id, unique) { copyNode ⇒
      copyNode.rootElementBox = rootElementBox.copy(node = copyNode,
        context = Context(targetGraph.origin, target.unique))
      projectionElementBoxes.foreach {
        case (coordinate, box) ⇒
          copyNode.projectionElementBoxes(coordinate) = box.copy(node = copyNode,
            context = Context(targetGraph.origin, target.unique))
      }
      copyNode
    }
    if (recursive)
      currentNode.iterator.foreach { _.copy(copyNode, recursive) }
    copyNode
  }
  /** Create new children node. */
  def createChild[A](id: Symbol, unique: UUID)(f: Node.ThreadUnsafe ⇒ A): A = synchronized {
    if (children.exists(child ⇒ child.id == id || child.unique == unique))
      throw new IllegalArgumentException("Node with the same identifier is already exists.")
    val newNode = Node(id, unique)
    val result = newNode.threadSafe { node ⇒
      children += newNode
      f(node)
    }
    result
  }
  /**
   * Lock this node and all child nodes.
   * Synchronization lock spread from the leaves and finish at the current node.
   */
  def freeze[A](f: Node.ThreadUnsafe ⇒ A): A =
    synchronized { children.foldLeft(f)((nestedFn, child) ⇒ child.freeze { child ⇒ (node) ⇒ nestedFn(node) })(this.asInstanceOf[Node.ThreadUnsafe]) }
  /** Get children nodes recursively. */
  def getChildrenRecursive(): Iterator[Node] = synchronized { children.iteratorRecursive() }
  /** Get children recursive */
  /** Get node element boxes. */
  def getElementBoxes(): Seq[ElementBox[_ <: Element]] = synchronized { Option(rootElementBox).toSeq ++ projectionElementBoxes.values }
  /** Get graph. */
  def getGraph(): Option[Graph[_ <: Model.Like]] = synchronized { graphReference }
  /** Get modified timestamp. */
  def getModified(): Element.Timestamp = modified
  /** Get parent node. */
  def getParent(): Option[Node] = synchronized { parentNodeReference.get }
  /** Get element box at the specific coordinate. */
  def getProjection(key: Coordinate): Option[ElementBox[_ <: Element]] = synchronized {
    if (key == Coordinate.root)
      Option(rootElementBox)
    else
      projectionElementBoxes.get(key)
  }
  /** Get projection's element boxes. */
  def getProjections(): immutable.Map[Coordinate, ElementBox[_ <: Element]] =
    synchronized { immutable.Map(projectionElementBoxes.updated(Coordinate.root, rootElementBox).toSeq: _*) }
  /** Get root element box. */
  def getRootElementBox() = synchronized { rootElementBox }
  /** Touch modification time of the current node. */
  def modify(ts: Element.Timestamp = Element.timestamp()): Unit = synchronized {
    if (modified < ts) {
      modified = ts
      parentNodeReference.get.foreach(_.modify(ts))
    }
  }
  /**
   * Explicitly convert this trait to ThreadUnsafe.
   * Conversion prevents the consumer to access unguarded content.
   */
  def threadSafe[A](f: Node.ThreadUnsafe ⇒ A): A = synchronized { f(this.asInstanceOf[Node.ThreadUnsafe]) }

  override def toString = s"graph.Node[${unique}(${id})]#${modified}"
}

object Node {
  implicit def node2interface(g: Node.type): Interface = DI.implementation

  trait Interface {
    /** Create common element node. */
    def apply[A <: Element](id: Symbol, unique: UUID): Node = new ThreadUnsafe(id, unique)
    /** Create model node. */
    def model[A <: Element](id: Symbol, unique: UUID): Node with ModelNodeInitializer = new ThreadUnsafe(id, unique) with ModelNodeInitializer
    /** Dump the node structure. */
    def dump(node: Node, brief: Boolean, padding: Int = 2): String = node.threadSafe { node ⇒
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

  /**
   * Thread unsafe node representation.
   */
  class ThreadUnsafe(val id: Symbol, val unique: UUID)
    extends Node with mutable.Set[Node] with mutable.SetLike[Node, ThreadUnsafe] {
    /** Children nodes. */
    val children = new NodeChildren(new WeakReference(this))
    /** Elements with non-root coordinates. */
    val projectionElementBoxes = mutable.HashMap[Coordinate, ElementBox[_ <: Element]]()
    /** Element with root coordinate. */
    var rootElementBox: ElementBox[_ <: Element] = null

    /** Adds a single element to the set. */
    def +=(elem: Node): this.type = { add(elem); this }
    /** Removes a single element to the set. */
    def -=(elem: Node): this.type = { remove(elem); this }
    /** Adds a single element to the set. */
    override def add(elem: Node): Boolean = children.add(elem)
    /** Clears the Node's contents. After this operation, there are no children. */
    override def clear() = children.clear()
    /** Tests whether this set contains a given node as a children. */
    def contains(elem: Node): Boolean = children.contains(elem)
    /**
     * The empty set of the same type as this set
     * @return  an empty set of type `Node`.
     */
    override def empty = new ThreadUnsafe(id, unique)
    /** Applies a function `f` to all children of this Node. */
    override def foreach[U](f: Node ⇒ U) = children.foreach(f)
    /** Creates a new iterator over all children contained in this node. */
    def iterator: Iterator[Node] = children.iterator
    /** Creates a new iterator over all children contained in this branch. */
    def iteratorRecursive: Iterator[Node] = children.iteratorRecursive()
    /** Get graph to which the node belongs. */
    def graph: Option[Graph[_ <: Model.Like]] = graphReference
    /** Get parent. */
    def parentNode: Option[Node] = parentNodeReference.get
    /** Set parent reference. */
    def parentNode_=(arg: Option[Node]): Unit = arg match {
      case Some(parent) ⇒
        // Remove from previous graph.
        graphReference.foreach { graph ⇒
          graph.nodes -= this.unique
        }
        parentNodeReference = WeakReference(parent)
        graphReference = parent.getGraph
        // Add to new graph.
        graphReference.foreach { graph ⇒
          graph.nodes += this.unique -> this
        }
        // Update graph links for all child nodes.
        children.foreach { _.threadSafe { _.parentNode = Some(ThreadUnsafe.this) } }
      case None ⇒
        // Remove from previous graph.
        graphReference.foreach { graph ⇒
          graph.nodes -= this.unique
          graphReference = None
          // Update graph links for all child nodes.
          children.foreach { _.threadSafe { _.parentNode = Some(ThreadUnsafe.this) } }
        }
        parentNodeReference = WeakReference(null)
    }
    /** Removes a single element to the set. */
    override def remove(elem: Node): Boolean = children.remove(elem)
    /** The number of children. */
    override def size: Int = children.size
    /**
     * A version of this collection with all
     * of the operations implemented sequentially (i.e. in a single-threaded manner).
     *
     * This method returns a reference to this collection. In parallel collections,
     * it is redefined to return a sequential implementation of this collection. In
     * both cases, it has O(1) complexity.
     *
     * @return a sequential view of the collection.
     */
    override def seq = children.seq

    override def toString = s"graph.Node[${unique}(${id})]#${modified}"
  }
  /** Allow to initialize model node. */
  trait ModelNodeInitializer {
    this: Node ⇒
    /** Initialize model node. */
    def initializeModelNode[A <: Model.Like](graph: Graph[A]) = synchronized {
      assert(graphReference.isEmpty)
      graphReference = Some(graph)
      parentNodeReference = WeakReference(this)
      graph.nodes += this.unique -> this
      // Update graph links for all child nodes.
      children.foreach { _.threadSafe { _.parentNode = Some(ModelNodeInitializer.this) } }
    }
  }
  /**
   * Dependency injection routines.
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    lazy val implementation = injectOptional[Interface] getOrElse new AnyRef with Interface {}
  }
}
