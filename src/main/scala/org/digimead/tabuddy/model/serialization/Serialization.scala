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
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.ElementBox.box2interface
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.graph.Node.node2interface
import org.digimead.tabuddy.model.graph.NodeState
import org.digimead.tabuddy.model.serialization.transport.Transport
import org.digimead.tabuddy.model.serialization.yaml.YAML
import org.digimead.tabuddy.model.serialization.yaml.YAML.yaml2implementation
import org.yaml.snakeyaml.nodes.SequenceNode
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.nodes.{ Node ⇒ YAMLNode }

import scala.language.implicitConversions

/** Common serialization implementation. */
class Serialization extends Serialization.Interface with Loggable {
  /** Load graph with the specific origin and timestamp. */
  def acquireGraph(origin: Symbol, bootstrapStorageURI: URI, fTransform: Serialization.AcquireTransformation,
    modified: Option[Element.Timestamp]): Graph[_ <: Model.Like] = {
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
    val mostUpToDate = graphDescriptors.flatten.maxBy(_._1.modified)
    // TODO Synchronize obsolete graphs
    acquireGraph(mostUpToDate._1, modified.getOrElse(mostUpToDate._1.stored.last), mostUpToDate._2, mostUpToDate._3, fTransform)
  }
  /** Save graph. */
  def freezeGraph(graph: Graph[_ <: Model.Like], fTransform: Serialization.FreezeTransformation): Element.Timestamp = {
    log.debug(s"Freeze ${graph}.")
    if (graph.storages.isEmpty)
      throw new IllegalStateException("Unable to freeze graph without any defined storages.")
    val storedTimestamps = graph.node.freezeRead { modelNode ⇒
      graph.storages.map {
        case storageURI if storageURI.isAbsolute() ⇒
          Serialization.perScheme.get(storageURI.getScheme()) match {
            case Some(transport) ⇒
              graph.node.freezeRead { model ⇒
                val modelˈ = fTransform(model.asInstanceOf[Node.ThreadUnsafe[Element]]).asInstanceOf[Node.ThreadUnsafe[Model.Like]]
                if (!graph.stored.contains(modelˈ.modified))
                  graph.stored = (graph.stored :+ modelˈ.modified).sorted
                transport.freezeGraph(modelˈ, storageURI, graphDescriptorToYAML(modelˈ))
                freezeNode(modelˈ, storageURI, transport, fTransform, Seq(modelˈ))
                Some(modelˈ.modified)
              }
            case None ⇒
              log.error(s"Unable to save graph to URI with unknown scheme ${storageURI.getScheme}.")
              None
          }
        case storageURI ⇒
          log.fatal(s"Unable to process relative storage URI as base: ${storageURI}.")
          None
      }
    }.flatten
    if (storedTimestamps.isEmpty)
      throw new IllegalStateException("Unable to freeze graph.")
    storedTimestamps.sorted.last
  }

  /** Internal method that loads graph from the graph descriptor. */
  protected def acquireGraph(graphDescriptor: Serialization.Descriptor.Graph[_ <: Model.Like], modified: Element.Timestamp,
    storageURI: URI, transport: Transport, fTransform: Serialization.AcquireTransformation): Graph[_ <: Model.Like] = {
    // Load a model with respect for the specific modification
    val nodeDescriptor = nodeDescriptorFromYaml(transport.acquireModel(graphDescriptor.modelId,
      graphDescriptor.origin, modified, storageURI))
    val nodeDescriptorˈ = fTransform(None, nodeDescriptor.asInstanceOf[Serialization.Descriptor.Node[Element]])
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
    val targetModelNode = Node.model[Model.Like](nodeDescriptorˈ.id, nodeDescriptorˈ.unique, nodeDescriptorˈ.modified)(modelTypeManifest)
    val graph = new Graph[Model.Like](graphDescriptor.created, targetModelNode, graphDescriptor.origin)(modelTypeManifest)
    graph.storages = graphDescriptor.storages
    targetModelNode.safeWrite { targetNode ⇒
      targetModelNode.initializeModelNode(graph, nodeDescriptorˈ.modified)
      /* Get element boxes */
      val elementBoxes = nodeDescriptorˈ.elements.map {
        case (elementUniqueId, elementModificationTimestamp) ⇒
          val descriptor = elementDescriptorFromYaml(transport.acquireElementBox(Seq(targetNode), elementUniqueId, elementModificationTimestamp, storageURI))
          ElementBox[Model.Like](descriptor.coordinate, descriptor.elementUniqueId, targetNode, storageURI,
            descriptor.serializationIdentifier, descriptor.modified)(Manifest.classType(descriptor.clazz))
      }
      val (rootElementsPart, projectionElementsPart) = elementBoxes.partition(_.coordinate == Coordinate.root)
      if (rootElementsPart.isEmpty)
        throw new IllegalStateException("Root element not found.")
      if (rootElementsPart.size > 1)
        throw new IllegalStateException("There are few root elements.")
      val rootElementBox = rootElementsPart.head
      val projectionElementBoxes: Seq[(Coordinate, ElementBox[Model.Like])] = projectionElementsPart.map(e ⇒ e.coordinate -> e)
      /* Get children */
      val children = nodeDescriptorˈ.children.flatMap {
        case (childId, childModificationTimestamp) ⇒
          acquireNode(childId, childModificationTimestamp, fTransform, Seq(targetNode))
      }
      targetNode.updateState(children = children,
        modified = null, // modification is already assigned
        projectionElementBoxes = immutable.HashMap(projectionElementBoxes: _*),
        rootElementBox = rootElementBox)
      targetNode.graph.nodes ++= targetNode.children.map(node ⇒ (node.unique, node))
      if (graph.modelType != graph.node.elementType)
        throw new IllegalArgumentException(s"Unexpected model type ${graph.modelType} vs ${graph.node.elementType}")
    }
    graph
  }
  /** Internal method that loads node with the specific id for the specific parent. */
  protected def acquireNode(id: Symbol, modified: Element.Timestamp, fTransform: Serialization.AcquireTransformation,
    ancestors: Seq[Node.ThreadUnsafe[_ <: Element]]): Option[Node[_ <: Element]] = try {
    log.debug(s"Acquire node ${id}.")
    if (ancestors.head.graph.storages.isEmpty)
      throw new IllegalArgumentException("Unable to aquire element box without any defined storages.")
    val nodeDescriptors: Seq[Option[(Serialization.Descriptor.Node[_ <: Element], URI, Transport)]] =
      ancestors.head.graph.storages.map {
        case storageURI if storageURI.isAbsolute() ⇒
          Serialization.perScheme.get(storageURI.getScheme()) match {
            case Some(transport) ⇒
              val descriptor = nodeDescriptorFromYaml(transport.acquireNode(ancestors, id, modified, storageURI))
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
    val mostUpToDate = nodeDescriptors.flatten.maxBy(_._1.modified)
    // TODO Synchronize obsolete nodes
    Some(acquireNode(mostUpToDate._1, mostUpToDate._2, mostUpToDate._3, fTransform, ancestors))
  } catch {
    case e: Throwable ⇒
      log.error(s"Unable to load node ${id} : " + e.getMessage(), e)
      if (skipBrokenNodes)
        None
      else
        throw e // rethrow
  }
  /** Internal method that loads node from the specific storageURI. */
  protected def acquireNode(nodeDescriptor: Serialization.Descriptor.Node[_ <: Element], storageURI: URI, transport: Transport,
    fTransform: Serialization.AcquireTransformation, ancestors: Seq[Node.ThreadUnsafe[_ <: Element]]): Node[_ <: Element] = {
    if (nodeDescriptor.elements.isEmpty)
      throw new IllegalStateException("There are no elements in the model node.")
    if (nodeDescriptor.id == null)
      throw new IllegalStateException("Id value not found in model node descriptor file.")
    if (nodeDescriptor.unique == null)
      throw new IllegalStateException("Unique value not found in model node descriptor file.")
    val targetNodeInitialState = new Node.State[Element](children = Seq(),
      graph = ancestors.head.graph,
      parentNodeReference = WeakReference(ancestors.last),
      projectionElementBoxes = immutable.HashMap(),
      rootElementBox = null)
    val targetNode = Node[Element](nodeDescriptor.id, nodeDescriptor.unique, targetNodeInitialState, nodeDescriptor.modified)
    targetNode.safeWrite { targetNode ⇒
      /* Get element boxes */
      val elementBoxes = nodeDescriptor.elements.map {
        case (elementUniqueId, elementModificationTimestamp) ⇒
          log.debug(s"Acquire element box ${elementUniqueId}.")
          val descriptor = elementDescriptorFromYaml(transport.acquireElementBox((ancestors :+ targetNode), elementUniqueId, elementModificationTimestamp, storageURI))
          ElementBox[Element](descriptor.coordinate, descriptor.elementUniqueId, targetNode, storageURI,
            descriptor.serializationIdentifier, descriptor.modified)(Manifest.classType(descriptor.clazz))
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
          acquireNode(childId, childModificationTimestamp, fTransform, ancestors :+ targetNode)
      }
      targetNode.graph.nodes ++= children.map(node ⇒ (node.unique, node))
      targetNode.updateState(
        children = children,
        modified = null, // modification is already assigned
        projectionElementBoxes = immutable.HashMap(projectionElementBoxes: _*),
        rootElementBox = rootElementBox)
      targetNode
    }
  }
  /** Create element descriptor from YAML. */
  protected def elementDescriptorFromYaml(descriptor: Array[Byte]): Serialization.Descriptor.Element[_ <: Element] =
    yaml.YAML.loadAs(new String(descriptor, io.Codec.UTF8.charSet), classOf[Serialization.Descriptor.Element[_ <: Element]])
  /** Create YAML element descriptor. */
  protected def elementDescriptorToYAML(elementBox: ElementBox[_ <: Element]): Array[Byte] = {
    val descriptorMap = new java.util.TreeMap[String, AnyRef]()
    descriptorMap.put("coordinate", elementBox.coordinate)
    descriptorMap.put("class", elementBox.node.elementType.runtimeClass.getName)
    descriptorMap.put("element_unique_id", elementBox.elementUniqueId)
    descriptorMap.put("modified", elementBox.modified)
    descriptorMap.put("serialization_identifier", elementBox.serialization.extension)
    yaml.YAML.dump(descriptorMap).getBytes(io.Codec.UTF8.charSet)
  }
  /** Internal method that saves the element content. */
  protected def freezeElementBox(elementBox: ElementBox[_ <: Element], storageURI: URI,
    transport: Transport, ancestorsNSelf: Seq[Node.ThreadUnsafe[_ <: Element]]) {
    log.debug(s"Freeze ${elementBox}.")
    // save element (update modification time)
    elementBox.getModified match {
      case Some(modified) ⇒
        if (modified ne elementBox.get)
          throw new IllegalStateException("Element and modified element are different.")
        elementBox.save(ancestorsNSelf, Some(storageURI))
        elementBox.get.eStash.property.foreach {
          case (valueId, perTypeMap) ⇒ perTypeMap.foreach { case (typeSymbolId, value) ⇒ value.commit(modified, transport, storageURI) }
        }
      case None ⇒
        log.debug("Skip unmodified element.")
    }
    // freeze element box
    transport.freezeElementBox(ancestorsNSelf, elementBox, storageURI, elementDescriptorToYAML(elementBox))
  }
  /** Internal method that saves the node descriptor. */
  protected def freezeNode(node: Node.ThreadUnsafe[_ <: Element], storageURI: URI, transport: Transport,
    fTransform: Serialization.FreezeTransformation, ancestorsNSelf: Seq[Node.ThreadUnsafe[_ <: Element]]) {
    log.debug(s"Freeze ${node}.")
    transport.freezeNode(ancestorsNSelf, storageURI, nodeDescriptorToYAML(node.elementType, node.id, node.modified, node.state, node.unique, fTransform))
    freezeElementBox(node.state.rootElementBox, storageURI, transport, ancestorsNSelf)
    node.state.projectionElementBoxes.foreach { case (coordinate, box) ⇒ freezeElementBox(box, storageURI, transport, ancestorsNSelf) }
    node.state.children.foreach(_.safeRead { child ⇒
      val childˈ = fTransform(child.asInstanceOf[Node.ThreadUnsafe[Element]])
      freezeNode(childˈ, storageURI, transport, fTransform, ancestorsNSelf :+ childˈ)
    })
  }
  /** Create element descriptor from YAML. */
  protected def graphDescriptorFromYaml(descriptor: Array[Byte]): Serialization.Descriptor.Graph[_ <: Model.Like] =
    yaml.YAML.loadAs(new String(descriptor, io.Codec.UTF8.charSet), classOf[Serialization.Descriptor.Graph[_ <: Model.Like]])
  /** Create YAML graph descriptor. */
  protected def graphDescriptorToYAML(model: Node.ThreadUnsafe[_ <: Model.Like]): Array[Byte] = {
    val storages = model.graph.storages.map(_.toString).asJava
    val descriptorMap = new java.util.TreeMap[String, AnyRef]()
    descriptorMap.put("created", model.graph.created)
    descriptorMap.put("model_id", model.id)
    descriptorMap.put("model_type", model.elementType.runtimeClass.getName())
    descriptorMap.put("modified", model.modified)
    descriptorMap.put("origin", model.graph.origin)
    descriptorMap.put("storages", storages)
    descriptorMap.put("stored", model.graph.stored.asJava)
    yaml.YAML.dump(descriptorMap).getBytes(io.Codec.UTF8.charSet)
  }
  /** Create node descriptor from YAML content. */
  protected def nodeDescriptorFromYaml(descriptor: Array[Byte]): Serialization.Descriptor.Node[_ <: Element] =
    yaml.YAML.loadAs(new String(descriptor, io.Codec.UTF8.charSet), classOf[Serialization.Descriptor.Node[_ <: Element]])
  /** Create YAML node descriptor. */
  protected def nodeDescriptorToYAML(elementType: Manifest[_ <: Element], id: Symbol, modified: Element.Timestamp, state: NodeState[_ <: Element],
    unique: UUID, fTransform: Serialization.FreezeTransformation): Array[Byte] = {
    val elements = (Seq[ElementBox[_ <: Element]](state.rootElementBox) ++ state.projectionElementBoxes.values).map(box ⇒
      Array(box.elementUniqueId, box.modified)).asJava
    val children = state.children.map(_.safeRead { child ⇒
      val childˈ = fTransform(child.asInstanceOf[Node.ThreadUnsafe[Element]])
      Array[AnyRef](childˈ.id, childˈ.modified)
    }).asJava
    val descriptorMap = new java.util.TreeMap[String, AnyRef]()
    descriptorMap.put("children", children)
    descriptorMap.put("class", elementType.runtimeClass.getName)
    descriptorMap.put("elements", elements)
    descriptorMap.put("id", id.name)
    descriptorMap.put("modified", modified)
    descriptorMap.put("unique", unique.toString())
    yaml.YAML.dump(descriptorMap).getBytes(io.Codec.UTF8.charSet)
  }
}

object Serialization {
  /** Transformation that is applied to acquiring nodes. */
  /* (parent node, child node descriptor) => transformed child node descriptor */
  type AcquireTransformation = Function2[Option[Node[Element]], Descriptor.Node[Element], Descriptor.Node[Element]]
  /* node => copy of transformed node */
  /** Transformation that is applied to freezing nodes. */
  type FreezeTransformation = Function1[Node.ThreadUnsafe[Element], Node.ThreadUnsafe[Element]]
  /** Serialization stash. */
  val stash = new ThreadLocal[AnyRef]()

  /** Load graph with the specific origin. */
  def acquire(origin: Symbol, bootstrapStorageURI: URI, fTransform: AcquireTransformation): Graph[_ <: Model.Like] =
    inner.acquireGraph(origin, bootstrapStorageURI, fTransform, None)
  /** Load graph with the specific origin. */
  def acquire(origin: Symbol, bootstrapStorageURI: URI, modified: Option[Element.Timestamp] = None,
    fTransform: AcquireTransformation = defaultAcquireTransformation): Graph[_ <: Model.Like] =
    inner.acquireGraph(origin, bootstrapStorageURI, fTransform, modified)
  /**
   * AcquasScalaBufferConverter
   * import scala.collection.JavaConverters.seqAsJavaListConverterire transformation that keeps arguments unmodified.
   */
  def defaultAcquireTransformation(parentNode: Option[Node[Element]], nodeDescriptor: Descriptor.Node[Element]) = nodeDescriptor
  /** Freeze transformation that keeps arguments unmodified. */
  def defaultFreezeTransformation(node: Node.ThreadUnsafe[Element]): Node.ThreadUnsafe[Element] = node
  /** Save graph. */
  def freeze(graph: Graph[_ <: Model.Like], fTransform: FreezeTransformation = defaultFreezeTransformation): Element.Timestamp =
    inner.freezeGraph(graph, fTransform)
  /** Serialization implementation. */
  def inner = DI.implementation
  /** Consumer defined map of per identifier serialization. */
  def perIdentifier = DI.perIdentifier
  /** Consumer defined map of per scheme transport. */
  def perScheme = DI.perScheme

  /**
   * Descriptor of the serialized object.
   */
  object Descriptor {
    import org.digimead.tabuddy.model.element.{ Element ⇒ TAElement }
    case class Element[A <: TAElement](val clazz: Class[A], val coordinate: Coordinate, elementUniqueId: UUID,
      val modified: TAElement.Timestamp, val serializationIdentifier: Serialization.Identifier) {
      assert(clazz != null, s"Broken descriptor ${this}.")
      assert(coordinate != null, s"Broken descriptor ${this}.")
      assert(elementUniqueId != null, s"Broken descriptor ${this}.")
      assert(modified != null, s"Broken descriptor ${this}.")
      assert(serializationIdentifier != null, s"Broken descriptor ${this}.")
    }
    object Element {
      trait Constructor {
        this: YAML.Constructor ⇒
        getYAMLConstructors.put(new Tag(classOf[Element[_ <: TAElement]]), new ConstructElementDescriptor())

        private class ConstructElementDescriptor extends CustomConstruct {
          protected val keyTypes = immutable.HashMap[String, PartialFunction[YAMLNode, Unit]](
            "coordinate" -> { case n: YAMLNode ⇒ setTagSafe(n, yaml.Coordinate.tag) },
            "element_unique_id" -> { case n: YAMLNode ⇒ setTagSafe(n, yaml.UUID.tag) },
            "modified" -> { case n: YAMLNode ⇒ setTagSafe(n, yaml.Timestamp.tag) })
          def constructCustom(map: mutable.HashMap[String, AnyRef]): AnyRef =
            Element(getClass.getClassLoader().loadClass(map("class").asInstanceOf[String]).asInstanceOf[Class[_ <: TAElement]],
              map("coordinate").asInstanceOf[Coordinate],
              map("element_unique_id").asInstanceOf[UUID],
              map("modified").asInstanceOf[TAElement.Timestamp],
              {
                val value = map("serialization_identifier").asInstanceOf[String]
                Serialization.perIdentifier.keys.find(_.extension == value) getOrElse
                  { throw new IllegalStateException(s"Unable to find serialization mechanism for '${value}'.") }
              })
        }
      }
    }
    case class Node[A <: TAElement](val children: Seq[(Symbol, TAElement.Timestamp)], val clazz: Class[A],
      val elements: Seq[(UUID, TAElement.Timestamp)], val id: Symbol, val modified: TAElement.Timestamp, val unique: UUID) {
      assert(clazz != null, s"Broken descriptor ${this}.")
      assert(id != null, s"Broken descriptor ${this}.")
      assert(modified != null, s"Broken descriptor ${this}.")
      assert(unique != null, s"Broken descriptor ${this}.")
    }
    object Node {
      trait Constructor {
        this: YAML.Constructor ⇒
        getYAMLConstructors.put(new Tag(classOf[Node[_ <: TAElement]]), new ConstructNodeDescriptor())

        private class ConstructNodeDescriptor extends CustomConstruct {
          protected val keyTypes = immutable.HashMap[String, PartialFunction[YAMLNode, Unit]](
            "children" -> {
              case n: SequenceNode ⇒ n.getValue().asScala.foreach {
                case n: SequenceNode ⇒
                  val values = n.getValue()
                  setTagSafe(values.get(0), yaml.Symbol.tag)
                  setTagSafe(values.get(1), yaml.Timestamp.tag)
              }
            },
            "elements" -> {
              case n: SequenceNode ⇒ n.getValue().asScala.foreach {
                case n: SequenceNode ⇒
                  val values = n.getValue()
                  setTagSafe(values.get(0), yaml.UUID.tag)
                  setTagSafe(values.get(1), yaml.Timestamp.tag)
              }
            },
            "id" -> { case n: YAMLNode ⇒ setTagSafe(n, yaml.Symbol.tag) },
            "modified" -> { case n: YAMLNode ⇒ setTagSafe(n, yaml.Timestamp.tag) },
            "unique" -> { case n: YAMLNode ⇒ setTagSafe(n, yaml.UUID.tag) })
          def constructCustom(map: mutable.HashMap[String, AnyRef]): AnyRef =
            Node(map("children").asInstanceOf[Iterable[java.util.ArrayList[AnyRef]]].toSeq.map(i ⇒ (i.get(0).asInstanceOf[Symbol], i.get(1).asInstanceOf[TAElement.Timestamp])),
              getClass.getClassLoader().loadClass(map("class").asInstanceOf[String]).asInstanceOf[Class[_ <: TAElement]],
              map("elements").asInstanceOf[Iterable[java.util.ArrayList[AnyRef]]].toSeq.map(i ⇒ (i.get(0).asInstanceOf[UUID], i.get(1).asInstanceOf[TAElement.Timestamp])),
              map("id").asInstanceOf[Symbol],
              map("modified").asInstanceOf[TAElement.Timestamp],
              map("unique").asInstanceOf[UUID])
        }
      }
    }
    case class Graph[A <: Model.Like](val created: TAElement.Timestamp, val modelId: Symbol, val modelType: Class[A],
      val modified: TAElement.Timestamp, val origin: Symbol, val storages: Seq[URI], val stored: Seq[TAElement.Timestamp]) {
      assert(created != null, s"Broken descriptor ${this}.")
      assert(modelId != null, s"Broken descriptor ${this}.")
      assert(modelType != null, s"Broken descriptor ${this}.")
      assert(modified != null, s"Broken descriptor ${this}.")
      assert(origin != null, s"Broken descriptor ${this}.")
      assert(storages.nonEmpty)
      assert(stored.nonEmpty)
    }
    object Graph {
      trait Constructor {
        this: YAML.Constructor ⇒
        getYAMLConstructors.put(new Tag(classOf[Graph[_ <: Model.Like]]), new ConstructGraphDescriptor())

        private class ConstructGraphDescriptor extends CustomConstruct {
          protected val keyTypes = immutable.HashMap[String, PartialFunction[YAMLNode, Unit]](
            "created" -> { case n: YAMLNode ⇒ setTagSafe(n, yaml.Timestamp.tag) },
            "model_id" -> { case n: YAMLNode ⇒ setTagSafe(n, yaml.Symbol.tag) },
            "modified" -> { case n: YAMLNode ⇒ setTagSafe(n, yaml.Timestamp.tag) },
            "origin" -> { case n: YAMLNode ⇒ setTagSafe(n, yaml.Symbol.tag) },
            "stored" -> { case n: SequenceNode ⇒ n.getValue().asScala.foreach(setTagSafe(_, yaml.Timestamp.tag)) })
          def constructCustom(map: mutable.HashMap[String, AnyRef]): AnyRef =
            Graph(map("created").asInstanceOf[TAElement.Timestamp],
              map("model_id").asInstanceOf[Symbol],
              getClass.getClassLoader().loadClass(map("model_type").asInstanceOf[String]).asInstanceOf[Class[_ <: Model.Like]],
              map("modified").asInstanceOf[TAElement.Timestamp],
              map("origin").asInstanceOf[Symbol],
              map("storages").asInstanceOf[Iterable[String]].map(new URI(_)).toSeq,
              map("stored").asInstanceOf[Iterable[TAElement.Timestamp]].toSeq)
        }
      }
    }
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
    def acquireGraph(origin: Symbol, bootstrapStorageURI: URI, fTransform: AcquireTransformation, modified: Option[Element.Timestamp]): Graph[_ <: Model.Like]
    /** Save graph. */
    def freezeGraph(graph: Graph[_ <: Model.Like], fTransform: FreezeTransformation): Element.Timestamp
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
