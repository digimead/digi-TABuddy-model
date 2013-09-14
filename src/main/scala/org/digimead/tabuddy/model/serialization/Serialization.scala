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
import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.collection.mutable
import scala.ref.WeakReference

import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.ElementBox.box2interface
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.graph.Node.node2interface
import org.digimead.tabuddy.model.graph.NodeState
import org.digimead.tabuddy.model.serialization.transport.Transport
import org.digimead.tabuddy.model.serialization.yaml.YAML.yaml2implementation

import scala.language.implicitConversions

/** Common serialization implementation. */
class Serialization extends Serialization.Interface with Loggable {
  /** Load graph with the specific origin and timestamp. */
  def acquireGraph(origin: Symbol, bootstrapStorageURI: URI, fTransform: Serialization.AcquireTransformation,
    modificationTimestamp: Option[Element.Timestamp]): Graph[_ <: Model.Like] = {
    log.debug(s"Acquire graph ${origin}.")
    // Bootstrap graph from here. After that we may check another locations with more up to date elements.
    val storages = Serialization.perScheme.get(bootstrapStorageURI.getScheme()) match {
      case Some(transport) ⇒
        val descriptor = graphDescriptorFromYaml(transport.acquireGraph(origin, bootstrapStorageURI))
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
            val descriptor = graphDescriptorFromYaml(transport.acquireGraph(origin, storageURI))
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
    acquireGraph(mostUpToDate._1, modificationTimestamp.getOrElse(mostUpToDate._1.stored.last), mostUpToDate._2, mostUpToDate._3, fTransform)
  }
  /** Save graph. */
  def freezeGraph(graph: Graph[_ <: Model.Like], fTransform: Serialization.FreezeTransformation) {
    log.debug(s"Freeze ${graph}.")
    if (graph.storages.isEmpty) {
      log.debug("Unable to freeze graph without any defined storages.")
      return
    }
    if (graph.stored.contains(graph.modification))
      log.warn("This graph is already stored.")
    else
      graph.stored = graph.stored :+ graph.modification
    graph.node.freezeRead { modelNode ⇒
      graph.storages.foreach {
        case storageURI if storageURI.isAbsolute() ⇒
          Serialization.perScheme.get(storageURI.getScheme()) match {
            case Some(transport) ⇒
              transport.freezeGraph(graph, storageURI, graphDescriptorToYAML(graph))
              freezeNode(graph.node, storageURI, transport, fTransform)
            case None ⇒
              log.error(s"Unable to save graph to URI with unknown scheme ${storageURI.getScheme}.")
          }
        case storageURI ⇒
          log.fatal(s"Unable to process relative storage URI as base: ${storageURI}.")
      }
    }
  }

  /** Internal method that loads graph from the graph descriptor. */
  protected def acquireGraph(graphDescriptor: Serialization.Descriptor.Graph[_ <: Model.Like], modificationTimestamp: Element.Timestamp,
    storageURI: URI, transport: Transport, fTransform: Serialization.AcquireTransformation): Graph[_ <: Model.Like] = {
    // Load model saved at modificationTimestamp
    val nodeDescriptor = fTransform(None, nodeDescriptorFromYaml(transport.acquireModel(graphDescriptor.modelId,
      graphDescriptor.origin, modificationTimestamp, storageURI)))
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
    val modelTypeManifest = Manifest.classType[Model.Like](graphDescriptor.modelType)
    val targetModelNode = Node.model[Model.Like](nodeDescriptor.id, nodeDescriptor.unique)(modelTypeManifest)
    val graph = new Graph[Model.Like](graphDescriptor.created, targetModelNode, graphDescriptor.origin)(modelTypeManifest)
    graph.storages = graphDescriptor.storages
    targetModelNode.safeWrite { targetNode ⇒
      targetModelNode.initializeModelNode(graph, nodeDescriptor.modificationTimestamp)
      /* Get element boxes */
      val elementBoxes = nodeDescriptor.elements.map {
        case (elementUniqueId, elementModificationTimestamp) ⇒
          val descriptor = elementDescriptorFromYaml(transport.acquireElementBox(elementUniqueId, elementModificationTimestamp, targetNode, storageURI))
          ElementBox[Model.Like](descriptor.coordinate, descriptor.elementUniqueId, targetNode, storageURI,
            descriptor.serializationIdentifier, descriptor.modificationTimestamp)(Manifest.classType(descriptor.clazz))
      }
      val (rootElementsPart, projectionElementsPart) = elementBoxes.partition(_.coordinate == Coordinate.root)
      if (rootElementsPart.isEmpty)
        throw new IllegalStateException("Root element not found.")
      if (rootElementsPart.size > 1)
        throw new IllegalStateException("There are few root elements.")
      val rootElementBox = rootElementsPart.head
      val projectionElementBoxes: Seq[(Coordinate, ElementBox[Model.Like])] = projectionElementsPart.map(e ⇒ e.coordinate -> e)
      /* Get children */
      val children = nodeDescriptor.children.flatMap {
        case (childId, childModificationTimestamp) ⇒
          acquireNode(childId, childModificationTimestamp, targetNode, fTransform)
      }
      targetNode.updateState(children = children,
        modificationTimestamp = nodeDescriptor.modificationTimestamp,
        projectionElementBoxes = immutable.HashMap(projectionElementBoxes: _*),
        rootElementBox = rootElementBox)
      targetNode.graph.nodes ++= targetNode.children.map(node ⇒ (node.unique, node))
      if (graph.modelType != graph.node.elementType)
        throw new IllegalArgumentException(s"Unexpected model type ${graph.modelType} vs ${graph.node.elementType}")
    }
    graph
  }
  /** Internal method that loads node with the specific id for the specific parent. */
  protected def acquireNode(id: Symbol, modificationTimestamp: Element.Timestamp,
    parentNode: Node.ThreadUnsafe[_ <: Element], fTransform: Serialization.AcquireTransformation): Option[Node[_ <: Element]] = try {
    log.debug(s"Acquire node ${id}.")
    if (parentNode.graph.storages.isEmpty)
      throw new IllegalArgumentException("Unable to aquire element box without any defined storages.")
    val nodeDescriptors: Seq[Option[(Serialization.Descriptor.Node[_ <: Element], URI, Transport)]] =
      parentNode.graph.storages.map {
        case storageURI if storageURI.isAbsolute() ⇒
          Serialization.perScheme.get(storageURI.getScheme()) match {
            case Some(transport) ⇒
              val descriptor = nodeDescriptorFromYaml(transport.acquireNode(id, modificationTimestamp, parentNode, storageURI))
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
    Some(acquireNode(mostUpToDate._1, parentNode, mostUpToDate._2, mostUpToDate._3, fTransform))
  } catch {
    case e: Throwable ⇒
      log.error(s"Unable to load node ${id} : " + e.getMessage(), e)
      if (skipBrokenNodes)
        None
      else
        throw e // rethrow
  }
  /** Internal method that loads node from the specific storageURI. */
  protected def acquireNode(nodeDescriptor: Serialization.Descriptor.Node[_ <: Element], parentNode: Node[_ <: Element],
    storageURI: URI, transport: Transport, fTransform: Serialization.AcquireTransformation): Node[_ <: Element] = {
    if (nodeDescriptor.elements.isEmpty)
      throw new IllegalStateException("There are no elements in the model node.")
    if (nodeDescriptor.id == null)
      throw new IllegalStateException("Id value not found in model node descriptor file.")
    if (nodeDescriptor.unique == null)
      throw new IllegalStateException("Unique value not found in model node descriptor file.")
    val targetNodeInitialState = new Node.State[Element](children = Seq(),
      graph = parentNode.graph,
      parentNodeReference = WeakReference(parentNode),
      projectionElementBoxes = immutable.HashMap(),
      rootElementBox = null)
    val targetNode = Node[Element](nodeDescriptor.id, nodeDescriptor.unique, targetNodeInitialState)
    targetNode.safeWrite { targetNode ⇒
      /* Get element boxes */
      val elementBoxes = nodeDescriptor.elements.map {
        case (elementUniqueId, elementModificationTimestamp) ⇒
          log.debug(s"Acquire element box ${elementUniqueId}.")
          val descriptor = elementDescriptorFromYaml(transport.acquireElementBox(elementUniqueId, elementModificationTimestamp, targetNode, storageURI))
          ElementBox[Element](descriptor.coordinate, descriptor.elementUniqueId, targetNode, storageURI,
            descriptor.serializationIdentifier, descriptor.modificationTimestamp)(Manifest.classType(descriptor.clazz))
      }
      val (rootElementsPart, projectionElementsPart) = elementBoxes.partition(_.coordinate == Coordinate.root)
      if (rootElementsPart.isEmpty)
        throw new IllegalStateException("Root element not found.")
      if (rootElementsPart.size > 1)
        throw new IllegalStateException("There are few root elements.")
      val rootElementBox = rootElementsPart.head
      val projectionElementBoxes: Seq[(Coordinate, ElementBox[Element])] = projectionElementsPart.map(e ⇒ e.coordinate -> e)
      /* Get children */
      val children = nodeDescriptor.children.flatMap {
        case (childId, childModificationTimestamp) ⇒
          acquireNode(childId, childModificationTimestamp, targetNode, fTransform)
      }
      targetNode.graph.nodes ++= children.map(node ⇒ (node.unique, node))
      targetNode.updateState(
        children = children,
        rootElementBox = rootElementBox,
        projectionElementBoxes = immutable.HashMap(projectionElementBoxes: _*),
        modificationTimestamp = nodeDescriptor.modificationTimestamp)
      targetNode
    }
  }
  /** Create element descriptor from YAML. */
  protected def elementDescriptorFromYaml(descriptor: Array[Byte]): Serialization.Descriptor.Element[_ <: Element] = {
    var axes: Seq[Axis[_ <: AnyRef with Serializable]] = Seq()
    var clazz: Class[_ <: Element] = null
    var element_unique_id: UUID = null
    var modified_milli = 0L
    var modified_nano = 0L
    var serialization: Serialization.Identifier = null
    yaml.YAML.load(new String(descriptor, io.Codec.UTF8.charSet)).
      asInstanceOf[java.util.LinkedHashMap[String, AnyRef]].asScala.foreach {
        case ("axes", value: java.util.HashMap[_, _]) ⇒
          axes = value.asInstanceOf[java.util.HashMap[String, String]].asScala.
            map { case (id, axis) ⇒ Axis(Symbol(id), null) }.toSeq
        case ("class", value: String) ⇒ clazz = getClass.getClassLoader().loadClass(value).asInstanceOf[Class[_ <: Element]]
        case ("element_unique_id", value: String) ⇒ element_unique_id = UUID.fromString(value)
        case ("modified_milli", value: java.lang.Integer) ⇒ modified_milli = value.toLong
        case ("modified_milli", value: java.lang.Long) ⇒ modified_milli = value
        case ("modified_nano", value: java.lang.Integer) ⇒ modified_nano = value.toLong
        case ("modified_nano", value: java.lang.Long) ⇒ modified_nano = value
        case ("serialization_identifier", value: String) ⇒
          serialization = Serialization.perIdentifier.keys.find(_.extension == value).
            getOrElse { throw new IllegalStateException(s"Unable to find serialization mechanism for '${value}'.") }
        case (key, value) ⇒
          log.warn(s"Unknown element descriptor entry: ${key} -> ${value}.")
      }
    Serialization.Descriptor.Element(clazz, Coordinate(axes: _*), element_unique_id,
      Element.timestamp(modified_milli, modified_nano), serialization)
  }
  /** Create YAML element descriptor. */
  protected def elementDescriptorToYAML(elementBox: ElementBox[_ <: Element]): Array[Byte] = {
    val axisMap = new java.util.HashMap[String, AnyRef]()
    elementBox.coordinate.coordinate.foreach { axis ⇒
      axisMap.put(axis.id.name, axis.value.toString())
    }
    val modified_milli = elementBox.get.modification.milliseconds
    val modified_nano = elementBox.get.modification.nanoShift
    val descriptorMap = new java.util.HashMap[String, AnyRef]()
    descriptorMap.put("axes", axisMap)
    descriptorMap.put("class", elementBox.node.elementType.runtimeClass.getName)
    descriptorMap.put("element_unique_id", elementBox.elementUniqueId.toString)
    descriptorMap.put("modified_milli", modified_milli: java.lang.Long)
    descriptorMap.put("modified_nano", modified_nano: java.lang.Long)
    descriptorMap.put("serialization_identifier", elementBox.serialization.extension)
    yaml.YAML.dump(descriptorMap).getBytes(io.Codec.UTF8.charSet)
  }
  /** Internal method that saves the element content. */
  protected def freezeElementBox(elementBox: ElementBox[_ <: Element], storageURI: URI, transport: Transport) {
    log.debug(s"Freeze ${elementBox}.")
    transport.freezeElementBox(elementBox, storageURI, elementDescriptorToYAML(elementBox))
    // save element
    elementBox.getModified match {
      case Some(modified) ⇒
        if (modified ne elementBox.get)
          throw new IllegalStateException("Element and modified element are different.")
        transport.freezeElement(modified, storageURI, elementBox.save)
        elementBox.get.eStash.property.foreach {
          case (valueId, perTypeMap) ⇒ perTypeMap.foreach { case (typeSymbolId, value) ⇒ value.commit(modified, transport, storageURI) }
        }
      case None ⇒
        log.debug("Skip unmodified element.")
    }
  }
  /** Internal method that saves the node descriptor. */
  protected def freezeNode(node: Node[_ <: Element], storageURI: URI, transport: Transport, fTransform: Serialization.FreezeTransformation, recursive: Boolean = true) {
    log.debug(s"Freeze ${node}.")
    node.freezeRead { node ⇒
      val (id: Symbol, unique: UUID, modification: Element.Timestamp, state: NodeState[_]) =
        fTransform(node.id, node.unique, node.modification, node.state: NodeState[_ <: Element])
      transport.freezeNode(node, storageURI, nodeDescriptorToYAML(node.elementType, id, modification, state, unique))
      freezeElementBox(state.rootElementBox, storageURI, transport)
      state.projectionElementBoxes.foreach { case (coordinate, box) ⇒ freezeElementBox(box, storageURI, transport) }
      state.children.foreach(freezeNode(_, storageURI, transport, fTransform))
    }
  }
  /** Create element descriptor from YAML. */
  protected def graphDescriptorFromYaml(descriptor: Array[Byte]): Serialization.Descriptor.Graph[_ <: Model.Like] = {
    var created: Element.Timestamp = null
    var model_id: Symbol = null
    var model_type: Class[_ <: Model.Like] = null
    var modified: Element.Timestamp = null
    var origin: Symbol = null
    var storages: Seq[URI] = Seq()
    var stored = Seq[Element.Timestamp]()
    yaml.YAML.load(new String(descriptor, io.Codec.UTF8.charSet)).
      asInstanceOf[java.util.LinkedHashMap[String, AnyRef]].asScala.foreach {
        case ("created", value: Element.Timestamp) ⇒ created = value
        case ("model_id", value: String) ⇒ model_id = Symbol(value)
        case ("model_type", value: String) ⇒ model_type = getClass.getClassLoader().loadClass(value).asInstanceOf[Class[_ <: Model.Like]]
        case ("modified", value: Element.Timestamp) ⇒ modified = value
        case ("origin", value: String) ⇒ origin = Symbol(value)
        case ("storages", value: java.util.ArrayList[_]) ⇒
          storages = value.asInstanceOf[java.util.ArrayList[String]].asScala.map(new URI(_))
        case ("stored", value: java.util.ArrayList[_]) ⇒
          stored = value.asInstanceOf[java.util.ArrayList[Element.Timestamp]].asScala.sorted
        case (key, value) ⇒
          log.warn(s"Unknown graph descriptor entry: ${key} -> ${value}.")
      }
    Serialization.Descriptor.Graph(created, model_id, model_type, modified, origin, storages, stored)
  }
  /** Create YAML graph descriptor. */
  protected def graphDescriptorToYAML(graph: Graph[_ <: Model.Like]): Array[Byte] = {
    val storages = graph.storages.map(_.toString).asJava
    val descriptorMap = new java.util.HashMap[String, AnyRef]()
    descriptorMap.put("created", graph.created)
    descriptorMap.put("model_id", graph.node.id.name)
    descriptorMap.put("model_type", graph.node.elementType.runtimeClass.getName())
    descriptorMap.put("modified", graph.modification)
    descriptorMap.put("origin", graph.origin.name)
    descriptorMap.put("storages", storages)
    descriptorMap.put("stored", graph.stored.asJava)
    yaml.YAML.dump(descriptorMap).getBytes(io.Codec.UTF8.charSet)
  }
  /** Create node descriptor from YAML content. */
  protected def nodeDescriptorFromYaml(descriptor: Array[Byte]): Serialization.Descriptor.Node[_ <: Element] = {
    var children = Seq[(Symbol, Element.Timestamp)]()
    var clazz: Class[_ <: Element] = null
    var elements = Seq[(UUID, Element.Timestamp)]()
    var id: Symbol = null
    var unique: UUID = null
    var modified: Element.Timestamp = null
    yaml.YAML.load(new String(descriptor, io.Codec.UTF8.charSet)).
      asInstanceOf[java.util.LinkedHashMap[String, AnyRef]].asScala.foreach {
        case ("children", value: java.util.ArrayList[_]) ⇒
          children = for (n ← value.asInstanceOf[java.util.ArrayList[java.util.ArrayList[_]]].asScala)
            yield (Symbol(n.get(0).asInstanceOf[String]), n.get(1).asInstanceOf[Element.Timestamp])
        //value.asInstanceOf[java.util.ArrayList[String]].asScala.map(Symbol(_))
        case ("class", value: String) ⇒ clazz = getClass.getClassLoader().loadClass(value).asInstanceOf[Class[_ <: Element]]
        case ("elements", value: java.util.ArrayList[_]) ⇒
          elements = for (e ← value.asInstanceOf[java.util.ArrayList[java.util.ArrayList[_]]].asScala)
            yield (UUID.fromString(e.get(0).asInstanceOf[String]), e.get(1).asInstanceOf[Element.Timestamp])
        case ("id", value: String) ⇒ id = Symbol(value)
        case ("modified", value: Element.Timestamp) ⇒ modified = value
        case ("unique", value: String) ⇒ unique = UUID.fromString(value)
        case (key, value) ⇒
          log.warn(s"Unknown node descriptor entry: ${key} -> ${value}.")
      }
    Serialization.Descriptor.Node(children, clazz, elements, id, modified, unique)
  }
  /** Create YAML node descriptor. */
  protected def nodeDescriptorToYAML(elementType: Manifest[_ <: Element], id: Symbol, modification: Element.Timestamp, state: NodeState[_ <: Element], unique: UUID): Array[Byte] = {
    val elements = (Seq[ElementBox[_ <: Element]](state.rootElementBox) ++ state.projectionElementBoxes.values).map(box ⇒
      Array(box.elementUniqueId.toString, box.getModified.map(_.modification).getOrElse(box.unmodified))).asJava
    val children = state.children.map(child ⇒ Array(child.id.name, child.modification)).asJava
    val descriptorMap = new java.util.HashMap[String, AnyRef]()
    descriptorMap.put("children", children)
    descriptorMap.put("class", elementType.runtimeClass.getName)
    descriptorMap.put("elements", elements)
    descriptorMap.put("id", id.name)
    descriptorMap.put("modified", modification)
    descriptorMap.put("unique", unique.toString())
    yaml.YAML.dump(descriptorMap).getBytes(io.Codec.UTF8.charSet)
  }
}

object Serialization {
  /** Transformation that is applied to acquiring nodes. */
  type AcquireTransformation = Function2[Option[Node[_ <: Element]], Descriptor.Node[_ <: Element], Descriptor.Node[_ <: Element]]
  /** Transformation that is applied to freezing nodes. */
  /* Returns NodeState[Element] because serialization is simplify structures and types. */
  type FreezeTransformation = Function4[Symbol, UUID, Element.Timestamp, NodeState[_ <: Element], (Symbol, UUID, Element.Timestamp, NodeState[Element])]
  /** Serialization stash. */
  val stash = new ThreadLocal[AnyRef]()

  /** Load graph with the specific origin. */
  def acquire(origin: Symbol, bootstrapStorageURI: URI, fTransform: AcquireTransformation = defaultAcquireTransformation,
    modificationTimestamp: Option[Element.Timestamp] = None): Graph[_ <: Model.Like] =
    inner.acquireGraph(origin, bootstrapStorageURI, fTransform, modificationTimestamp)
  /** Acquire transformation that keeps arguments unmodified. */
  def defaultAcquireTransformation(parentNode: Option[Node[_ <: Element]], nodeDescriptor: Descriptor.Node[_ <: Element]) = nodeDescriptor
  /** Freeze transformation that keeps arguments unmodified. */
  def defaultFreezeTransformation(nodeId: Symbol, nodeUnique: UUID, nodeModificationTimestamp: Element.Timestamp,
    nodeState: NodeState[_ <: Element]): (Symbol, UUID, Element.Timestamp, NodeState[Element]) =
    (nodeId, nodeUnique, nodeModificationTimestamp, nodeState.asInstanceOf[NodeState[Element]])
  /** Save graph. */
  def freeze(graph: Graph[_ <: Model.Like], fTransform: FreezeTransformation = defaultFreezeTransformation) =
    inner.freezeGraph(graph, fTransform)
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
    case class Node[A <: TAElement](val children: Seq[(Symbol, TAElement.Timestamp)], val clazz: Class[A],
      val elements: Seq[(UUID, TAElement.Timestamp)], val id: Symbol, val modificationTimestamp: TAElement.Timestamp, val unique: UUID)
    case class Graph[A <: Model.Like](val created: TAElement.Timestamp, val modelId: Symbol, val modelType: Class[A],
      val modificationTimestamp: TAElement.Timestamp, val origin: Symbol, val storages: Seq[URI], val stored: Seq[TAElement.Timestamp])
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
   * Serialization interface.
   */
  trait Interface {
    this: Loggable ⇒
    /** Skip broken nodes on load. */
    val skipBrokenNodes = false

    /** Load graph with the specific origin. */
    def acquireGraph(origin: Symbol, bootstrapStorageURI: URI, fTransform: AcquireTransformation, modificationTimestamp: Option[Element.Timestamp]): Graph[_ <: Model.Like]
    /** Save graph. */
    def freezeGraph(graph: Graph[_ <: Model.Like], fTransform: FreezeTransformation)
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
