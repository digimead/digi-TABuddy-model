/**
 * TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2012-2014 Alexey Aksenov ezh@ezh.msk.ru
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
import java.util.concurrent.ConcurrentHashMap
import org.digimead.digi.lib.api.XDependencyInjection
import org.digimead.digi.lib.log.api.XLoggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.{ Coordinate, Element }
import org.digimead.tabuddy.model.serialization.Serialization
import scala.collection.{ immutable, mutable }
import scala.collection.convert.Wrappers.JMapWrapperLike
import scala.language.implicitConversions

/**
 * Graph is a container for nodes.
 *
 * @param node root element of the graph
 * @param origin graph owner identifier
 */
class Graph[A <: Model.Like](val created: Element.Timestamp, val node: Node[A],
  val origin: Symbol)(implicit val modelType: Manifest[A]) extends Modifiable.Read with ConsumerData with mutable.Publisher[Event] with Equals {
  /** HashMap for index of graph nodes.   */
  // Please, keep it consistent.
  val nodes = NodeMap(new ConcurrentHashMap[UUID, Node[_ <: Element]])
  /** List of stored graphs. */
  @volatile var retrospective = Graph.Retrospective.empty(origin)
  /** Check if such node is already exists. */
  @volatile var strict = true

  /** Copy graph. */
  def copy(created: Element.Timestamp = created,
    id: Symbol = node.id,
    modified: Element.Timestamp = node.modified,
    origin: Symbol = this.origin,
    unique: UUID = node.unique)(graphEarlyAccess: Graph[A] ⇒ Unit): Graph[A] = node.freezeRead { sourceModelNode ⇒
    /*
     * Create graph and model node
     */
    val targetModelNode = Node.model[A](id, unique, modified)
    val graph = new Graph[A](created, targetModelNode, origin)
    graph.retrospective = this.retrospective
    graphEarlyAccess(graph)
    targetModelNode.safeWrite { targetNode ⇒
      targetModelNode.initializeModelNode(graph, modified)
      val projectionBoxes: Seq[(Coordinate, ElementBox[A])] = sourceModelNode.projectionBoxes.map {
        case (coordinate, box) ⇒ coordinate -> box.copy(node = targetNode)
      }.toSeq
      if (graph.modelType != graph.node.elementType)
        throw new IllegalArgumentException(s"Unexpected model type ${graph.modelType} vs ${graph.node.elementType}")
      /*
       * Copy model children
       */
      targetNode.updateState(
        children = sourceModelNode.children.map(_.copy(attach = false, target = targetNode, recursive = true)),
        modified = null, // modification is already assigned
        projectionBoxes = immutable.HashMap(projectionBoxes: _*))
    }
    graph
  }
  /** Get graph model. */
  def model: A = node.rootBox.e
  /** Get modification timestamp. */
  def modified: Element.Timestamp = node.modified
  /** Provide publish() public access */
  override def publish(event: Event) = try {
    super.publish(event)
  } catch {
    // catch all other subscriber exceptions
    case e: Throwable ⇒
      Graph.log.error(e.getMessage(), e)
      throw e
  }
  /** Get actual storages. */
  def storages: Seq[URI] = if (retrospective.history.isEmpty) Seq() else retrospective.getStorages(retrospective.history.keys.max)
  /** Visit graph elements. */
  def visit[A](visitor: Element.Visitor[A], onlyModified: Boolean = true,
    multithread: Boolean = true)(implicit param: Element.Visitor.Param = Element.Visitor.defaultParam): Iterator[A] = {
    val lazyIterator = node.freezeRead {
      if (multithread) {
        _.iteratorRecursive.grouped(param.multithreadGroupSize).flatMap { nodes ⇒
          nodes.par.flatMap(_.projectionBoxes.values.flatMap(box ⇒
            if (onlyModified) box.getModified.flatMap(_.eOnVisit(visitor)) else box.e.eOnVisit(visitor)))
        }
      } else {
        _.iteratorRecursive.flatMap(_.projectionBoxes.values.flatMap(box ⇒
          if (onlyModified) box.getModified.flatMap(_.eOnVisit(visitor)) else box.e.eOnVisit(visitor)))
      }
    }
    if (param.lazyVizit) lazyIterator else lazyIterator.toVector.toIterator
  }

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Graph[_]]
  override def equals(other: Any) = other match {
    case that: Graph[_] ⇒ (that eq this) ||
      (that.canEqual(this) && this.## == that.## && this.modified == that.modified)
    case _ ⇒ false
  }
  override def hashCode() = lazyHashCode
  protected lazy val lazyHashCode = java.util.Arrays.hashCode(Array[AnyRef](this.created, this.node, this.origin, this.modelType))
  override def toString() = s"Graph[${model.eId.name}@${origin.name}]#${modified}"

  case class NodeMap[A, B <: Node[_ <: Element]](underlying: ConcurrentHashMap[A, B])
    extends mutable.AbstractMap[A, B] with JMapWrapperLike[A, B, NodeMap[A, B]] {
    override def empty = NodeMap(new ConcurrentHashMap[A, B])
    /** Adds a single element to the map. */
    override def +=(kv: (A, B)): this.type = { put(kv._1, kv._2); this }
    /** Removes a key from this map. */
    override def -=(key: A): this.type = { remove(key); this }
    /** Adds a new key/value pair to this map and optionally returns previously bound value. */
    override def put(key: A, value: B): Option[B] = {
      if (isEmpty) {
        // 1st node MUST be always a model
        val undoF = () ⇒ {}
        val result = super.put(key, value)
        Graph.this.publish(Event.GraphChange(value, null, value)(undoF))
        result
      } else {
        // Nth node MUST always have parent
        val undoF = () ⇒ {}
        val result = super.put(key, value)
        if (strict)
          result.foreach { previous ⇒
            // restore
            super.put(key, previous)
            throw new IllegalStateException(s"Such node ${key} is already exists.")
          }
        Graph.this.publish(Event.GraphChange(value.parent.get, result.getOrElse(null.asInstanceOf[B]), value)(undoF))
        result
      }
    }
    /** Removes a key from this map, returning the value associated previously */
    override def remove(key: A): Option[B] = super.remove(key).map { node ⇒
      if (isEmpty) {
        // last node MUST be always a model
        val undoF = () ⇒ {}
        Graph.this.publish(Event.GraphChange(node, node, null)(undoF))
      } else {
        // Nth node MUST always have parent
        val undoF = () ⇒ {}
        Graph.this.publish(Event.GraphChange(node.parent.get, node, null)(undoF))
      }
      node
    }
    /** Adds a new key/value pair to this map. */
    override def update(key: A, value: B): Unit = put(key, value)
    /**
     * Removes all nodes from the map. After this operation has completed,
     *  the map will be empty.
     */
    override def clear() = {
      val undoF = () ⇒ {}
      Graph.this.publish(Event.GraphReset(Graph.this)(undoF))
      super.clear()
    }
  }
}

object Graph extends XLoggable {
  implicit def graph2interface(g: Graph.type): Interface = DI.implementation

  trait Interface {
    def apply[A <: Model.Like: Manifest](node: Node[A], origin: Symbol): Graph[A] = new Graph(Element.timestamp(), node, origin)
    /** Create a new graph. */
    def apply[A <: Model.Like: Manifest](origin: Symbol, scope: A#StashType#ScopeType, serialization: Serialization.Identifier,
      unique: UUID)(graphEarlyAccess: Graph[A] ⇒ Unit)(implicit stashClass: Class[_ <: A#StashType]): Graph[A] =
      apply[A](origin, origin, scope, serialization, unique)(graphEarlyAccess)
    /** Create a new graph. */
    def apply[A <: Model.Like](id: Symbol, origin: Symbol, scope: A#StashType#ScopeType, serialization: Serialization.Identifier, unique: UUID,
      timestamp: Element.Timestamp = Element.timestamp())(graphEarlyAccess: Graph[A] ⇒ Unit)(implicit m: Manifest[A], stashClass: Class[_ <: A#StashType]): Graph[A] = {
      val modelNode = Node.model[A](id, unique, timestamp)
      val modelGraph = new Graph[A](timestamp, modelNode, origin)
      graphEarlyAccess(modelGraph)
      modelNode.safeWrite { node ⇒
        modelNode.initializeModelNode(modelGraph, timestamp)
        val modelBox = ElementBox[A](Coordinate.root, timestamp, node, timestamp, scope, serialization)
        node.updateBox(Coordinate.root, modelBox, timestamp)
        if (modelGraph.modelType != modelGraph.node.elementType)
          throw new IllegalArgumentException(s"Unexpected model type ${modelGraph.modelType} vs ${modelGraph.node.elementType}")
        modelGraph
      }
    }
    /** Dump the graph structure. */
    def dump(graph: Graph[_ <: Model.Like], brief: Boolean, padding: Int = 2): String = synchronized {
      val pad = " " * padding
      val self = "graph origin:%s, model id:%s, model unique:%s".format(graph.origin, graph.node.id, graph.node.unique)
      val childrenDump = Node.dump(graph.node, brief, padding)
      if (childrenDump.isEmpty) self else self + "\n" + pad + childrenDump
    }
  }

  /**
   * Container with graph evolution.
   */
  case class Retrospective(val history: Map[Element.Timestamp, Retrospective.Indexes], val origins: Seq[Symbol], val storages: Seq[URI]) {
    /** Get last modification. */
    def last: Option[Element.Timestamp] = if (history.isEmpty) None else Some(history.keys.max)
    /** Get head modification. */
    def head: Option[Element.Timestamp] = if (history.isEmpty) None else Some(history.keys.min)
    /** Get origin. */
    def getOrigin(ts: Element.Timestamp): Symbol = history.get(ts) match {
      case Some(Retrospective.Indexes(originIndex, storageIndexes)) ⇒ origins(originIndex)
      case None ⇒ throw new NoSuchElementException("Timestamp not found: " + ts)
    }
    /** Get storages. */
    def getStorages(ts: Element.Timestamp): Seq[URI] = history.get(ts) match {
      case Some(Retrospective.Indexes(originIndex, storageIndexes)) ⇒ storageIndexes.map(storages)
      case None ⇒ throw new NoSuchElementException("Timestamp not found: " + ts)
    }
  }
  object Retrospective {
    /** Get empty retrospective. */
    def empty(origin: Symbol) = Graph.Retrospective(immutable.HashMap(), Seq(origin), Seq.empty)

    /**
     * History value.
     */
    case class Indexes(val originIndex: Int, val storageIndexes: Seq[Int])
  }
  /**
   * Dependency injection routines.
   */
  private object DI extends XDependencyInjection.PersistentInjectable {
    lazy val implementation = injectOptional[Interface] getOrElse new AnyRef with Interface {}
  }
}
