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

import scala.collection.immutable
import scala.collection.mutable

import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.ElementBox.box2interface
import org.digimead.tabuddy.model.graph.Node.node2interface
import org.digimead.tabuddy.model.serialization.Serialization

import scala.language.implicitConversions

/**
 * Graph is a container for nodes.
 *
 * @param node root element of the graph
 * @param origin graph owner identifier
 */
class Graph[A <: Model.Like](val created: Element.Timestamp, val node: Node[A],
  val origin: Symbol)(implicit val modelType: Manifest[A]) extends Modifiable.Read with Equals {
  /** Index of all graph nodes. */
  val nodes = new mutable.HashMap[UUID, Node[_ <: Element]] with mutable.SynchronizedMap[UUID, Node[_ <: Element]]
  /** Path to graph storages. */
  @volatile var storages: Seq[URI] = Seq()
  /** List of timestamp to stored graphs. */
  @volatile var stored = Seq[Element.Timestamp]()

  /** Copy graph. */
  def copy(created: Element.Timestamp = created, id: Symbol = node.id, modified: Element.Timestamp = node.modified, origin: Symbol = this.origin, unique: UUID = node.unique): Graph[A] = node.freezeRead { sourceModelNode ⇒
    /*
     * Create graph and model node
     */
    val targetModelNode = Node.model[A](id, unique, modified)
    val graph = new Graph[A](created, targetModelNode, origin)
    graph.storages = this.storages
    graph.stored = this.stored
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
        children = sourceModelNode.children.map(_.copy(targetNode, true)),
        modified = null, // modification is already assigned
        projectionBoxes = immutable.HashMap(projectionBoxes: _*))
      graph.nodes ++= targetNode.children.map(n ⇒ n.unique -> n)
    }
    graph
  }
  /** Get graph model. */
  def model: A = node.rootBox.e
  /** Get modification timestamp. */
  def modified: Element.Timestamp = node.modified

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Graph[_]]
  override def equals(other: Any) = other match {
    case that: Graph[_] ⇒ (that eq this) ||
      (that.canEqual(this) && this.## == that.## && this.modified == that.modified)
    case _ ⇒ false
  }
  override def hashCode() = lazyHashCode
  protected lazy val lazyHashCode = java.util.Arrays.hashCode(Array[AnyRef](this.created, this.node, this.origin, this.modelType))
  override def toString() = s"Graph[${origin}]#${modified}"
}

object Graph {
  implicit def graph2interface(g: Graph.type): Interface = DI.implementation

  trait Interface {
    def apply[A <: Model.Like: Manifest](node: Node[A], origin: Symbol): Graph[A] = new Graph(Element.timestamp(), node, origin)
    /** Create a new graph. */
    def apply[A <: Model.Like: Manifest](origin: Symbol, scope: A#StashType#ScopeType, serialization: Serialization.Identifier,
      unique: UUID)(implicit stashClass: Class[_ <: A#StashType]): Graph[A] =
      apply[A](origin, origin, scope, serialization, unique)
    /** Create a new graph. */
    def apply[A <: Model.Like](id: Symbol, origin: Symbol, scope: A#StashType#ScopeType, serialization: Serialization.Identifier,
      unique: UUID, timestamp: Element.Timestamp = Element.timestamp())(implicit m: Manifest[A], stashClass: Class[_ <: A#StashType]): Graph[A] = {
      val modelNode = Node.model[A](id, unique, timestamp)
      val modelGraph = new Graph[A](timestamp, modelNode, origin)
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
   * Dependency injection routines.
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    lazy val implementation = injectOptional[Interface] getOrElse new AnyRef with Interface {}
  }
}
