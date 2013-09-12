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

package org.digimead.tabuddy.model.serialization

import java.net.URI
import java.util.LinkedHashMap
import java.util.UUID
import java.util.concurrent.locks.ReentrantReadWriteLock

import scala.Option.option2Iterable
import scala.collection.immutable
import scala.collection.mutable
import scala.ref.WeakReference

import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.ElementBox.box2interface
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.graph.Node.node2interface
import org.digimead.tabuddy.model.serialization.transport.Transport
import org.yaml.snakeyaml.DumperOptions
import org.yaml.snakeyaml.Yaml

import scala.language.implicitConversions

/** Common serialization implementation. */
class Serialization extends Serialization.Interface with Loggable {
  /** Load graph with the specific origin. */
  def acquireGraph(origin: Symbol, bootstrapStorageURI: URI): Graph[_ <: Model.Like] = {
    log.debug(s"Acquire graph ${origin}.")
    // Bootstrap graph from here. After that we may check another locations with more up to date elements.
    val storages = Serialization.perScheme.get(bootstrapStorageURI.getScheme()) match {
      case Some(transport) ⇒
        val descriptor = transport.acquireGraph(origin, bootstrapStorageURI)
        if (descriptor.origin == null)
          throw new IllegalStateException("Origin value not found in graph descriptor file.")
        if (descriptor.origin.name != origin.name)
          throw new IllegalStateException(s"Incorrect saved origin value ${descriptor.origin.name} vs required ${origin.name}.")
        if (!descriptor.storages.contains(bootstrapStorageURI))
          descriptor.copy(storages = bootstrapStorageURI +: descriptor.storages).storages
        else
          descriptor.storages
      case None ⇒
        throw new IllegalArgumentException(s"Unable to load graph from URI with unknown scheme ${bootstrapStorageURI.getScheme}.")
    }
    if (storages.isEmpty)
      throw new IllegalArgumentException("Unable to aquire graph without any defined storages.")
    val graphDescriptors: Seq[Option[(Serialization.Descriptor.Graph[_ <: Model.Like], URI, Transport)]] = storages.map {
      case storageURI if storageURI.isAbsolute() ⇒
        Serialization.perScheme.get(storageURI.getScheme()) match {
          case Some(transport) ⇒
            val descriptor = transport.acquireGraph(origin, storageURI)
            if (descriptor.origin == null)
              throw new IllegalStateException("Origin value not found in graph descriptor file.")
            if (descriptor.origin.name != origin.name)
              throw new IllegalStateException(s"Incorrect saved origin value ${descriptor.origin.name} vs required ${origin.name}.")
            val graphDescriptor = if (!descriptor.storages.contains(bootstrapStorageURI))
              descriptor.copy(storages = bootstrapStorageURI +: descriptor.storages)
            else
              descriptor
            Some(graphDescriptor, storageURI, transport)
          case None ⇒
            log.error(s"Unable to acquire graph ${origin} from URI with unknown scheme ${storageURI.getScheme}.")
            None
        }
      case storageURI ⇒
        log.fatal(s"Unable to process relative storage URI as base: ${storageURI}.")
        None
    }
    val mostUpToDate = graphDescriptors.flatten.maxBy(_._1.modificationTimestamp)
    // TODO Synchronize obsolete graphs
    acquireGraph(mostUpToDate._1, mostUpToDate._2, mostUpToDate._3)
  }
  /** Load node with the specific id for the specific parent. */
  def acquireNode(id: Symbol, parentNode: Node.ThreadUnsafe): Option[Node] = try {
    log.debug(s"Acquire node ${id}.")
    if (parentNode.graph.storages.isEmpty)
      throw new IllegalArgumentException("Unable to aquire element box without any defined storages.")
    val nodeDescriptors: Seq[Option[(Serialization.Descriptor.Node, URI, Transport)]] =
      parentNode.graph.storages.map {
        case storageURI if storageURI.isAbsolute() ⇒
          Serialization.perScheme.get(storageURI.getScheme()) match {
            case Some(transport) ⇒
              val descriptor = transport.acquireNode(id, parentNode, storageURI)
              if (descriptor.elements.isEmpty)
                throw new IllegalStateException("There are no elements in the node.")
              if (descriptor.id == null)
                throw new IllegalStateException("Id value not found in node descriptor file.")
              if (descriptor.unique == null)
                throw new IllegalStateException("Unique value not found in model node descriptor file.")
              if (descriptor.id.name != id.name)
                throw new IllegalStateException(s"Incorrect saved node id value ${descriptor.id.name} vs required ${id.name}.")
              Some(descriptor, storageURI, transport)
            case None ⇒
              log.error(s"Unable to acquire node ${id} from URI with unknown scheme ${storageURI.getScheme}.")
              None
          }
        case storageURI ⇒
          log.fatal(s"Unable to process relative storage URI as base: ${storageURI}.")
          None
      }
    val mostUpToDate = nodeDescriptors.flatten.maxBy(_._1.modificationTimestamp)
    // TODO Synchronize obsolete nodes
    Some(acquireNode(mostUpToDate._1, parentNode, mostUpToDate._2, mostUpToDate._3))
  } catch {
    case e: Throwable ⇒
      log.error(s"Unable to load node ${id} : " + e.getMessage(), e)
      if (skipBrokenNodes)
        None
      else
        throw e // rethrow
  }
  /** Save graph. */
  def freezeGraph(graph: Graph[_ <: Model.Like], context: Serialization.Context = new Serialization.Context) {
    log.debug(s"Freeze ${graph}.")
    if (graph.storages.isEmpty) {
      log.debug("Unable to freeze graph without any defined storages.")
      return
    }
    graph.node.freezeRead { modelNode ⇒
      graph.storages.foreach {
        case storageURI if storageURI.isAbsolute() ⇒
          Serialization.perScheme.get(storageURI.getScheme()) match {
            case Some(transport) ⇒ transport.freezeGraph(graph, storageURI)
            case None ⇒ log.error(s"Unable to save graph to URI with unknown scheme ${storageURI.getScheme}.")
          }
        case storageURI ⇒
          log.fatal(s"Unable to process relative storage URI as base: ${storageURI}.")
      }
    }
  }
  /** Save node. */
  def freezeNode(node: Node, context: Serialization.Context, recursive: Boolean = true) {
    log.debug(s"Freeze ${node}.")
    if (node.graph.storages.isEmpty) {
      log.debug("Unable to freeze node without any defined storages.")
      return
    }
    node.freezeRead { node ⇒
      node.graph.storages.foreach {
        case storageURI if storageURI.isAbsolute() ⇒
          Serialization.perScheme.get(storageURI.getScheme()) match {
            case Some(transport) ⇒ transport.freezeNode(node, storageURI)
            case None ⇒ log.error(s"Unable to save node to URI with unknown scheme ${storageURI.getScheme}.")
          }
        case storageURI ⇒
          log.fatal(s"Unable to process relative storage URI as base: ${storageURI}.")
      }
    }
  }

  /** Internal method that loads graph from the graph descriptor. */
  protected def acquireGraph(graphDescriptor: Serialization.Descriptor.Graph[_ <: Model.Like],
    storageURI: URI, transport: Transport): Graph[_ <: Model.Like] = {
    val nodeDescriptor = transport.acquireModel(graphDescriptor.modelId, graphDescriptor.origin, storageURI)
    if (nodeDescriptor.elements.isEmpty)
      throw new IllegalStateException("There are no elements in the model node.")
    if (nodeDescriptor.id == null)
      throw new IllegalStateException("Id value not found in model node descriptor file.")
    if (nodeDescriptor.unique == null)
      throw new IllegalStateException("Unique value not found in model node descriptor file.")
    if (nodeDescriptor.id.name != graphDescriptor.modelId.name)
      throw new IllegalStateException(s"Incorrect saved model id value ${nodeDescriptor.id.name} vs required ${graphDescriptor.modelId.name}.")
    /*
       * Create graph and model node
       */
    log.debug(s"Acquire node ${nodeDescriptor.id} from ${storageURI}.")
    val targetModelNode = Node.model(nodeDescriptor.id, nodeDescriptor.unique)
    val graph = new Graph(graphDescriptor.created, targetModelNode, graphDescriptor.origin)(Manifest.classType(graphDescriptor.modelType))
    graph.storages = graphDescriptor.storages
    targetModelNode.safeWrite { targetNode ⇒
      targetModelNode.initializeModelNode(graph, nodeDescriptor.modificationTimestamp)
      /* Get element boxes */
      val elementBoxes = nodeDescriptor.elements.map { elementUniqueId ⇒
        val descriptor = transport.acquireElementBox(elementUniqueId, targetNode, storageURI)
        ElementBox[Element](descriptor.coordinate, descriptor.elementUniqueId, targetNode, storageURI,
          descriptor.serializationIdentifier, descriptor.modificationTimestamp)(Manifest.classType(descriptor.clazz))
      }
      val (rootElementsPart, projectionElementsPart) = elementBoxes.partition(_.coordinate == Coordinate.root)
      if (rootElementsPart.isEmpty)
        throw new IllegalStateException("Root element not found.")
      if (rootElementsPart.size > 1)
        throw new IllegalStateException("There are few root elements.")
      val rootElementBox = rootElementsPart.head
      val projectionElementBoxes: Seq[(Coordinate, ElementBox[_ <: Element])] = projectionElementsPart.map(e ⇒ e.coordinate -> e)
      /* Get children */
      val children = nodeDescriptor.children.flatMap(Serialization.acquireNode(_, targetNode))
      targetNode.graph.nodes ++= children.map(node ⇒ (node.unique, node))
      targetNode.updateState(
        children = children,
        rootElementBox = rootElementBox,
        projectionElementBoxes = immutable.HashMap(projectionElementBoxes: _*),
        modificationTimestamp = nodeDescriptor.modificationTimestamp)
      if (graph.modelType != graph.node.getRootElementBox.elementType)
        throw new IllegalArgumentException(s"Unexpected model type ${graph.modelType} vs ${graph.node.getRootElementBox.elementType}")

    }
    graph
  }
  /** Internal method that loads node from the node descriptor. */
  protected def acquireNode(nodeDescriptor: Serialization.Descriptor.Node, parentNode: Node, storageURI: URI, transport: Transport): Node = {
    if (nodeDescriptor.elements.isEmpty)
      throw new IllegalStateException("There are no elements in the model node.")
    if (nodeDescriptor.id == null)
      throw new IllegalStateException("Id value not found in model node descriptor file.")
    if (nodeDescriptor.unique == null)
      throw new IllegalStateException("Unique value not found in model node descriptor file.")
    val targetNode = Node.model(nodeDescriptor.id, nodeDescriptor.unique)
    targetNode.safeWrite { targetNode ⇒
      targetNode.updateState(children = Seq(),
        graph = parentNode.graph,
        modificationTimestamp = nodeDescriptor.modificationTimestamp,
        parentNodeReference = WeakReference(parentNode),
        projectionElementBoxes = immutable.HashMap(),
        rootElementBox = null)
      /* Get element boxes */
      val elementBoxes = nodeDescriptor.elements.map { elementUniqueId ⇒
        val descriptor = transport.acquireElementBox(elementUniqueId, targetNode, storageURI)
        ElementBox[Element](descriptor.coordinate, descriptor.elementUniqueId, targetNode, storageURI,
          descriptor.serializationIdentifier, descriptor.modificationTimestamp)(Manifest.classType(descriptor.clazz))
      }
      val (rootElementsPart, projectionElementsPart) = elementBoxes.partition(_.coordinate == Coordinate.root)
      if (rootElementsPart.isEmpty)
        throw new IllegalStateException("Root element not found.")
      if (rootElementsPart.size > 1)
        throw new IllegalStateException("There are few root elements.")
      val rootElementBox = rootElementsPart.head
      val projectionElementBoxes: Seq[(Coordinate, ElementBox[_ <: Element])] = projectionElementsPart.map(e ⇒ e.coordinate -> e)
      /* Get children */
      val children = nodeDescriptor.children.flatMap(Serialization.acquireNode(_, targetNode))
      targetNode.updateState(
        children = children,
        rootElementBox = rootElementBox,
        projectionElementBoxes = immutable.HashMap(projectionElementBoxes: _*),
        modificationTimestamp = nodeDescriptor.modificationTimestamp)
      targetNode
    }
  }
}

object Serialization {
  implicit def interface2implementation(m: Serialization.type): Interface = m.inner
  /** Serialization stash. */
  val stash = new ThreadLocal[AnyRef]()

  /** Serialization implementation. */
  def inner = DI.implementation
  /** Consumer defined map of per identifier serialization. */
  def perIdentifier = DI.perIdentifier
  /** Consumer defined map of per scheme transport. */
  def perScheme = DI.perScheme

  class Context {
    /** Maximum size of the nodeModificationTimestampMemo map. */
    protected val nodeModificationTimestampMemoMaxSize = 1000
    /** Contain records with modificationTimestamp of nodes. */
    protected val nodeModificationTimestampMemo = new mutable.HashMap[URI, MaxSizeHashMap[UUID, Element.Timestamp]]()
    /** nodeModificationTimestampMemo read/write lock. */
    protected val rwl = new ReentrantReadWriteLock()

    /** Get modification timestamp for the node at the storageURI. */
    def getModificationTimestampMemo(storageURI: URI, nodeUnique: UUID): Option[Element.Timestamp] = {
      rwl.readLock().lock()
      try { nodeModificationTimestampMemo.get(storageURI).flatMap(linkedHashMap ⇒ Option(linkedHashMap.get(nodeUnique))) }
      finally { rwl.readLock().unlock() }
    }
    /** Set modification timestamp for the node at the storageURI. */
    def setModificationTimestampMemo(storageURI: URI, nodeUnique: UUID, modificationTimestamp: Element.Timestamp) {
      rwl.writeLock().lock()
      try {
        nodeModificationTimestampMemo.get(storageURI) match {
          case Some(linkedHashMap) ⇒
            linkedHashMap.put(nodeUnique, modificationTimestamp)
          case None ⇒
            val linkedHashMap = new MaxSizeHashMap[UUID, Element.Timestamp](nodeModificationTimestampMemoMaxSize)
            nodeModificationTimestampMemo(storageURI) = linkedHashMap
            linkedHashMap.put(nodeUnique, modificationTimestamp)
        }
      } finally { rwl.writeLock().unlock() }
    }

    class MaxSizeHashMap[K, V](maxSize: Int) extends LinkedHashMap[K, V] {
      override def removeEldestEntry(eldest: java.util.Map.Entry[K, V]): Boolean = size() > maxSize
    }
  }
  /**
   * Descriptor of the serialized object.
   */
  object Descriptor {
    import org.digimead.tabuddy.model.element.{ Element ⇒ TAElement }
    case class Element[A <: TAElement](val clazz: Class[A], val coordinate: Coordinate, elementUniqueId: UUID,
      val modificationTimestamp: TAElement.Timestamp, val serializationIdentifier: Serialization.Identifier)
    case class Node(val children: Seq[Symbol], val elements: Seq[UUID], val id: Symbol,
      val modificationTimestamp: TAElement.Timestamp, val unique: UUID)
    case class Graph[A <: Model.Like](val created: TAElement.Timestamp, val modelId: Symbol, val modelType: Class[A],
      val modificationTimestamp: TAElement.Timestamp, val origin: Symbol, val storages: Seq[URI])
  }
  /**
   * Serialization interface.
   */
  trait Interface {
    this: Loggable ⇒
    /** YAML de/serializer. */
    lazy val yaml = {
      val options = new DumperOptions()
      options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK)
      new Yaml(options)
    }
    /** Skip broken nodes on load. */
    val skipBrokenNodes = false

    /** Load graph with the specific origin. */
    def acquireGraph(origin: Symbol, bootstrapStorageURI: URI): Graph[_ <: Model.Like]
    /** Load node with the specific id for the specific parent. */
    def acquireNode(id: Symbol, parentNode: Node.ThreadUnsafe): Option[Node]
    /** Save graph. */
    def freezeGraph(graph: Graph[_ <: Model.Like], context: Serialization.Context = new Serialization.Context)
    /** Save node. */
    def freezeNode(node: Node, context: Serialization.Context, recursive: Boolean = true)
  }
  /**
   * Serialization identifier that is associated with serialization mechanism.
   */
  trait Identifier extends Equals with java.io.Serializable {
    val extension: String

    override def canEqual(that: Any) = that.isInstanceOf[Identifier]
    override def equals(that: Any): Boolean = that match {
      case that: Identifier ⇒ that.canEqual(this) && that.extension.equals(this.extension)
      case _ ⇒ false
    }
    override def hashCode = extension.##

    override def toString = s"Serialization.Identifier(${extension})"
  }
  /**
   * Dependency injection routines
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    /** Implementation of the serialization. */
    lazy val implementation: Interface = injectOptional[Interface] getOrElse new Serialization
    /** Per identifier serialization. */
    lazy val perIdentifier: immutable.HashMap[Identifier, Mechanism] = injectOptional[immutable.HashMap[Identifier, Mechanism]] getOrElse
      immutable.HashMap(BuiltinSerialization.Identifier -> new BuiltinSerialization,
        YAMLSerialization.Identifier -> new YAMLSerialization)
    /** Per scheme transports. */
    lazy val perScheme: immutable.HashMap[String, Transport] = injectOptional[immutable.HashMap[String, Transport]] getOrElse
      immutable.HashMap("file" -> new transport.Local())
  }
}
