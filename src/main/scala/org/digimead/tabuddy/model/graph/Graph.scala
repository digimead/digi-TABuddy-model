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

import scala.ref.WeakReference
import scala.collection.immutable
import scala.collection.mutable

import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Stash
import org.digimead.tabuddy.model.serialization.Serialization

import language.implicitConversions

/**
 * Graph is a container for nodes.
 *
 * @param node root element of the graph
 * @param origin graph owner identifier
 */
class Graph[A <: Model.Like](val node: Node, val origin: Symbol)(implicit val modelType: Manifest[A]) {
  /** Index of all graph nodes. */
  val nodes = new mutable.HashMap[UUID, Node] with mutable.SynchronizedMap[UUID, Node]
  /** Path to graph storage. */
  @volatile var location: Option[URI] = None

  /** Copy graph. */
  def copy(origin: Symbol, id: Symbol = node.id, unique: UUID = node.unique): Graph[A] = node.freeze { sourceModelNode ⇒
    val timestamp = Element.timestamp()
    /*
     * Create graph and model node
     */
    val targetModelNode = Node.model(id, unique)
    val graph = Graph[A](targetModelNode, origin)
    targetModelNode.threadSafe { targetNode ⇒
      targetModelNode.initializeModelNode(graph)
      val rootElementBox = sourceModelNode.rootElementBox.copy(node = targetNode, context = Context(graph.origin, targetNode.unique))
      val projectionElementBoxes: Seq[(Coordinate, ElementBox[_ <: Element])] = sourceModelNode.projectionElementBoxes.map {
        case (coordinate, box) ⇒
          coordinate -> box.copy(node = targetNode, context = Context(graph.origin, targetNode.unique))
      }.toSeq
      targetNode.updateState(rootElementBox = rootElementBox, projectionElementBoxes = immutable.HashMap(projectionElementBoxes: _*))
      if (graph.modelType != graph.node.getRootElementBox.elementType)
        throw new IllegalArgumentException(s"Unexpected model type ${graph.modelType} vs ${graph.node.getRootElementBox.elementType}")
      /*
       * Copy model children
       */
      sourceModelNode.children.foreach(_.copy(targetNode, true))
    }
    graph
  }
  /** Get graph model. */
  def model(): A = node.getRootElementBox.get.asInstanceOf[A]
}

object Graph {
  implicit def graph2interface(g: Graph.type): Interface = DI.implementation

  trait Interface {
    def apply[A <: Model.Like: Manifest](node: Node, origin: Symbol): Graph[A] = new Graph(node, origin)
    /** Create a new graph. */
    def apply[A <: Model.Like: Manifest](origin: Symbol, scope: A#StashType#ScopeType, serialization: Serialization[_],
      unique: UUID)(implicit stashClass: Class[_ <: A#StashType]): Graph[A] =
      apply[A](origin, origin, scope, serialization, unique)
    /** Create a new graph. */
    def apply[A <: Model.Like](id: Symbol, origin: Symbol, scope: A#StashType#ScopeType, serialization: Serialization[_],
      unique: UUID)(implicit m: Manifest[A], stashClass: Class[_ <: A#StashType]): Graph[A] = {
      val timestamp = Element.timestamp()
      val modelNode = Node.model(id, unique)
      val modelGraph = Graph[A](modelNode, origin)
      modelNode.threadSafe { node ⇒
        modelNode.initializeModelNode(modelGraph)
        val modelBox = ElementBox[A](Context(origin, modelNode.unique), Coordinate.root, timestamp, node, timestamp, scope, serialization)
        if (modelGraph.modelType != modelGraph.node.getRootElementBox.elementType)
          throw new IllegalArgumentException(s"Unexpected model type ${modelGraph.modelType} vs ${modelGraph.node.getRootElementBox.elementType}")
        modelGraph
      }
    }
    /** Dump the graph structure. */
    def dump(graph: Graph[_], brief: Boolean, padding: Int = 2): String = synchronized {
      val pad = " " * padding
      val self = "graph origin:%s, model id:%s, model unique:%s".format(graph.origin, graph.node.id, graph.node.unique)
      val childrenDump = Node.dump(graph.node, brief, padding)
      if (childrenDump.isEmpty) self else self + "\n" + pad + childrenDump
    }
  }
  /**
   * Dependency injection routines.
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    lazy val implementation = injectOptional[Interface] getOrElse new AnyRef with Interface {}
  }
}
