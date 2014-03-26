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

package org.digimead.tabuddy.model.serialization

import java.io.IOException
import java.net.URI
import java.util.UUID
import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.{ Coordinate, Element }
import org.digimead.tabuddy.model.graph.{ ElementBox, Graph, Node, NodeState }
import org.digimead.tabuddy.model.serialization.transport.Transport
import org.digimead.tabuddy.model.serialization.yaml.YAML
import org.yaml.snakeyaml.nodes.{ Node ⇒ YAMLNode, SequenceNode, Tag }
import scala.collection.{ immutable, mutable }
import scala.collection.JavaConverters.{ asScalaBufferConverter, seqAsJavaListConverter }
import scala.language.implicitConversions
import scala.ref.WeakReference
import scala.util.control.ControlThrowable

/** Common serialization implementation. */
class Serialization extends Serialization.Interface with Loggable {
  /** Method that loads graph with the graph loader. */
  def acquireGraph(loader: Serialization.Loader, source: Serialization.Source[_ <: Model.Like, _ <: Element],
    graphEarlyAccess: Graph[_ <: Model.Like] ⇒ Unit, sData: SData): Graph[_ <: Model.Like] = {
    /*
     * Create graph and model node
     */
    log.debug(s"Acquire node ${source.modelDescriptor.id} from ${sData(SData.Key.storageURI)}.")
    val graph = loader.createGraph()
    graphEarlyAccess(graph)
    graph.node.safeWrite { targetNode ⇒
      graph.node.asInstanceOf[Node.ModelNodeInitializer].initializeModelNode(graph, loader.modified)
      // 1. restore retrospective.
      val recordResourcesName = Serialization.recordResourcesName(loader.modified)
      val recordResourcesURI = source.transport.append(source.storageURI, Serialization.retrospective, recordResourcesName)
      val recordResources = recordResourcesFromYAML(source.transport.read(recordResourcesURI, sData))
      val history = immutable.Map(source.graphDescriptor.records.filter(_ <= loader.modified).par.map { modified ⇒
        val recordName = Serialization.recordName(loader.modified)
        val recordURI = source.transport.append(source.storageURI, Serialization.retrospective, recordName)
        val record = recordFromYAML(source.transport.read(recordURI, sData))
        modified -> Graph.Retrospective.Indexes(record.originIndex, record.storageIndexes)
      }.seq: _*)
      graph.retrospective = Graph.Retrospective(history, recordResources.origins, recordResources.storages)
      // 2. setup projections.
      val elementBoxes = source.modelDescriptor.elements.map {
        case (elementUniqueId, elementModificationTimestamp) ⇒
          val descriptor = elementDescriptorFromYaml(source.transport.acquireElementBox(Seq(targetNode),
            elementUniqueId, elementModificationTimestamp, sData))
          ElementBox[Model.Like](descriptor.coordinate, descriptor.elementUniqueId, targetNode,
            descriptor.serializationIdentifier, sData, descriptor.modified)(Manifest.classType(descriptor.clazz))
      }
      if (!elementBoxes.exists(_.coordinate == Coordinate.root))
        throw new IllegalStateException("Root element not found.")
      val projectionBoxes: Seq[(Coordinate, ElementBox[Model.Like])] = elementBoxes.map(e ⇒ e.coordinate -> e)
      targetNode.updateState(modified = null, projectionBoxes = immutable.HashMap(projectionBoxes: _*)) // modification is already assigned
      // Graph is valid at this point
      // 3. add children.
      val children = source.modelDescriptor.children.flatMap {
        case (childId, childModificationTimestamp) ⇒
          acquireNode(childId, childModificationTimestamp, Seq(targetNode), sData)
      }
      if (children.nonEmpty) {
        targetNode.updateState(children = children, modified = null) // modification is already assigned
        targetNode.children.foreach(_.safeRead(_.registerWithAncestors()))
      }
      if (graph.modelType != graph.node.elementType)
        throw new IllegalArgumentException(s"Unexpected model type ${graph.modelType} vs ${graph.node.elementType}")
    }
    graph
  }
  /** Get graph loader for the specific origin. */
  def acquireGraphLoader(origin: Symbol, modified: Option[Element.Timestamp], sData: SData): Serialization.Loader = {
    log.debug(s"Acquire graph ${origin}.")
    val bootstrapStorageURI = sData(SData.Key.storageURI)
    val sources = Serialization.perScheme.get(bootstrapStorageURI.getScheme()) match {
      case Some(transport) ⇒
        val descriptor = graphDescriptorFromYaml(transport.acquireGraph(origin, sData.updated(SData.Key.storageURI, bootstrapStorageURI)))
        if (descriptor.origin == null)
          throw new IllegalStateException("Origin value not found in graph descriptor file.")
        if (descriptor.origin.name != origin.name)
          throw new IllegalStateException(s"Incorrect saved origin value ${descriptor.origin.name} vs required ${origin.name}.")
        val sources = getSources(bootstrapStorageURI, transport, descriptor, modified, sData)
        modified match {
          case Some(modified) ⇒
            // Pass only sources that contain required modification.
            sources.filter(_.graphDescriptor.records.contains(modified))
          case None ⇒
            // Pass all sources
            sources
        }
      case None ⇒
        throw new IllegalArgumentException(s"Unable to load graph from URI with unknown scheme ${bootstrapStorageURI.getScheme}.")
    }
    if (sources.isEmpty)
      throw new IllegalArgumentException("Unable to aquire graph loader. There are no suitable sources found.")
    modified match {
      case Some(modified) ⇒ new Serialization.Loader(sources, modified, sData - SData.Key.storageURI)
      case None ⇒ new Serialization.Loader(sources, sources.map(_.graphDescriptor.modified).max, sData - SData.Key.storageURI)
    }
  }
  /** Save graph. */
  def freezeGraph(graph: Graph[_ <: Model.Like], sData: SData): Element.Timestamp = {
    val graphRetrospective = graph.retrospective
    val graphStorages = graph.storages
    try {
      log.debug(s"Freeze ${graph}.")
      val (realStorages, formalStorages) = sData.get(SData.Key.explicitStorages) match {
        case Some(Serialization.ExplicitStorages(seq, Serialization.ExplicitStorages.ModeAppend)) ⇒
          // Save graph to explicit and original storages. Save original(formal) values with serialized data.
          val storages = (graphStorages ++ seq).map(Serialization.normalizeURI).distinct
          (storages, storages)
        case Some(Serialization.ExplicitStorages(seq, Serialization.ExplicitStorages.ModeIgnore)) ⇒
          // Save graph to explicit storages. Save original(formal) values with serialized data.
          (seq.map(Serialization.normalizeURI).distinct, graphStorages.map(Serialization.normalizeURI).distinct)
        case Some(Serialization.ExplicitStorages(seq, Serialization.ExplicitStorages.ModeReplace)) ⇒
          // Save graph to original storages. Save explicit(formal) values with serialized data.
          (graphStorages.map(Serialization.normalizeURI).distinct, seq.map(Serialization.normalizeURI).distinct)
        case None ⇒
          val storages = graphStorages.map(Serialization.normalizeURI).distinct
          (storages, storages)
      }
      if (realStorages.isEmpty)
        throw new IllegalStateException("Unable to freeze graph without any defined storages.")
      val storedTimestamps = graph.node.freezeRead { modelNode ⇒
        // freeze graph
        realStorages.map {
          case storageURI if storageURI.isAbsolute() ⇒
            Serialization.perScheme.get(storageURI.getScheme()) match {
              case Some(transport) ⇒
                val modelˈ = sData(SData.Key.freezeT)(modelNode.asInstanceOf[Node.ThreadUnsafe[Element]]).asInstanceOf[Node.ThreadUnsafe[Model.Like]]
                // 1. Add new record to history.
                if (!graphRetrospective.history.isDefinedAt(modelˈ.modified)) {
                  val origins = (graphRetrospective.origins :+ graph.origin).distinct
                  val storages = (graphRetrospective.storages ++ formalStorages).distinct
                  val record = Graph.Retrospective.Indexes(origins.indexOf(graph.origin), formalStorages.map(s ⇒ storages.indexOf(s)).toSeq)
                  if (record.originIndex < 0)
                    throw new IllegalStateException(s"Unable to find index for origin ${graph.origin} in ${origins.mkString(",")}.")
                  if (record.storageIndexes.exists(_ < 0))
                    throw new IllegalStateException(s"Unable to find index for storages ${formalStorages.mkString(",")} in ${storages.mkString(",")}.")
                  graph.retrospective = Graph.Retrospective(graphRetrospective.history + (modelˈ.modified -> record), origins, storages)
                }
                // 2. Freeze graph descriptor.
                transport.freezeGraph(modelˈ, graphDescriptorToYAML(modelˈ,
                  formalStorages.map(Serialization.normalizeURI).distinct),
                  sData.updated(SData.Key.storageURI, storageURI))
                // 3. Freeze all graph nodes.
                freezeNode(modelˈ, transport, Seq(modelˈ), sData.updated(SData.Key.storageURI, storageURI))
                // 4. Freeze all graph records if required.
                graph.retrospective.history.foreach {
                  case (modified, Graph.Retrospective.Indexes(originIndex, storageIndexes)) ⇒
                    val recordName = Serialization.recordName(modified)
                    val recordURI = transport.append(storageURI, Serialization.retrospective, recordName)
                    if (!transport.exists(recordURI, sData))
                      transport.write(recordURI, recordToYAML(originIndex, storageIndexes), sData)
                }
                val recordResourcesName = Serialization.recordResourcesName(modelˈ.modified)
                val recordResourcesURI = transport.append(storageURI, Serialization.retrospective, recordResourcesName)
                transport.write(recordResourcesURI, recordResourcesToYAML(graph.retrospective.origins, graph.retrospective.storages), sData)
                // 5. Freeze graph checksum if required.
                //freezeCheckSumm()
                Some(modelˈ.modified)
              case None ⇒
                log.error(s"Unable to save graph to URI with unknown scheme '${storageURI.getScheme}''.")
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
    } catch {
      case e: Throwable ⇒
        graph.retrospective = graphRetrospective
        throw e
    }
  }
  /** Internal method that loads node with the specific id for the specific parent. */
  protected def acquireNode(id: Symbol, modified: Element.Timestamp, ancestors: Seq[Node.ThreadUnsafe[_ <: Element]],
    sData: SData): Option[Node[_ <: Element]] = try {
    log.debug(s"Acquire node ${id}.")
    val storages = ancestors.head.graph.storages
    if (storages.isEmpty)
      throw new IllegalArgumentException("Unable to aquire element box without storages.")
    val nodeDescriptors: Seq[Option[(Serialization.Descriptor.Node[_ <: Element], URI, Transport)]] =
      storages.map {
        case storageURI if storageURI.isAbsolute() ⇒
          Serialization.perScheme.get(storageURI.getScheme()) match {
            case Some(transport) ⇒
              val descriptor = nodeDescriptorFromYaml(transport.acquireNode(ancestors, id, modified,
                sData.updated(SData.Key.storageURI, storageURI)))
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
    val descriptorˈ = sData(SData.Key.acquireT)(ancestors, mostUpToDate._1.asInstanceOf[Serialization.Descriptor.Node[Element]])
    Some(acquireNode(descriptorˈ, mostUpToDate._3, ancestors, sData))
  } catch {
    case e: Throwable ⇒
      log.error(s"Unable to load node ${id} : " + e.getMessage(), e)
      if (sData.get(SData.Key.forceLoad) == Some(true))
        None
      else
        throw e // rethrow
  }
  /** Internal method that converts node descriptor to node. */
  protected def acquireNode(nodeDescriptor: Serialization.Descriptor.Node[_ <: Element], transport: Transport,
    ancestors: Seq[Node.ThreadUnsafe[_ <: Element]], sData: SData): Node[_ <: Element] = {
    if (nodeDescriptor.elements.isEmpty)
      throw new IllegalStateException("There are no elements in the model node.")
    if (nodeDescriptor.id == null)
      throw new IllegalStateException("Id value not found in model node descriptor file.")
    if (nodeDescriptor.unique == null)
      throw new IllegalStateException("Unique value not found in model node descriptor file.")
    val targetNodeInitialState = new Node.State[Element](
      attached = true, // yes, there is
      children = Seq(),
      graph = ancestors.head.graph,
      parentNodeReference = WeakReference(ancestors.last),
      projectionBoxes = immutable.HashMap())
    val targetNode = Node[Element](nodeDescriptor.id, nodeDescriptor.unique, targetNodeInitialState, nodeDescriptor.modified)(Manifest.classType(nodeDescriptor.clazz))
    targetNode.safeWrite { targetNode ⇒
      // 1st stage: setup projections
      val elementBoxes = nodeDescriptor.elements.map {
        case (elementUniqueId, elementModificationTimestamp) ⇒
          log.debug(s"Acquire element box ${elementUniqueId}.")
          val descriptor = elementDescriptorFromYaml(transport.acquireElementBox((ancestors :+ targetNode), elementUniqueId, elementModificationTimestamp, sData))
          ElementBox[Element](descriptor.coordinate, descriptor.elementUniqueId, targetNode,
            descriptor.serializationIdentifier, sData, descriptor.modified)(Manifest.classType(descriptor.clazz))
      }
      if (!elementBoxes.exists(_.coordinate == Coordinate.root))
        throw new IllegalStateException("Root element not found.")
      val projectionBoxes: Seq[(Coordinate, ElementBox[Element])] = elementBoxes.map(e ⇒ e.coordinate -> e)
      targetNode.updateState(modified = null, projectionBoxes = immutable.HashMap(projectionBoxes: _*)) // modification is already assigned
      targetNode.registerWithAncestors()
      // Node is valid at this point
      // 2nd stage: add children
      val children = nodeDescriptor.children.flatMap {
        case (childId, childModificationTimestamp) ⇒
          acquireNode(childId, childModificationTimestamp, ancestors :+ targetNode, sData)
      }
      if (children.nonEmpty) {
        children.foreach(_.safeRead(_.registerWithAncestors()))
        targetNode.updateState(children = children, modified = null) // modification is already assigned
      }
      targetNode
    }
  }
  /** Create element descriptor from YAML. */
  protected def elementDescriptorFromYaml(descriptor: Array[Byte]): Serialization.Descriptor.Element[_ <: Element] = YAMLSerialization.wrapper(
    yaml.YAML.block.loadAs(new String(descriptor, io.Codec.UTF8.charSet), classOf[Serialization.Descriptor.Element[_ <: Element]]), descriptor)
  /** Create YAML element descriptor. */
  protected def elementDescriptorToYAML(elementBox: ElementBox[_ <: Element]): Array[Byte] = {
    val descriptorMap = new java.util.TreeMap[String, AnyRef]()
    descriptorMap.put("coordinate", elementBox.coordinate)
    descriptorMap.put("class", elementBox.node.elementType.runtimeClass.getName)
    descriptorMap.put("element_unique_id", elementBox.elementUniqueId)
    descriptorMap.put("modified", elementBox.modified)
    descriptorMap.put("serialization_identifier", elementBox.serialization.extension)
    YAMLSerialization.wrapper(yaml.YAML.block.dump(descriptorMap).getBytes(io.Codec.UTF8.charSet), descriptorMap)
  }
  /** Internal method that saves the element content. */
  protected def freezeElementBox(elementBox: ElementBox[_ <: Element], transport: Transport,
    ancestorsNSelf: Seq[Node.ThreadUnsafe[_ <: Element]], sData: SData) {
    log.debug(s"Freeze ${elementBox}.")
    elementBox.getModified match {
      case Some(modified) ⇒
        if (modified ne elementBox.e)
          throw new IllegalStateException("Element and modified element are different.")
        elementBox.save(ancestorsNSelf, sData)
        elementBox.e.eStash.property.foreach {
          case (valueId, perTypeMap) ⇒ perTypeMap.foreach {
            case (typeSymbolId, value) ⇒
              value.commit(modified, transport, transport.acquireElementLocation(ancestorsNSelf, modified.eBox, sData))
          }
        }
      case None ⇒
        log.debug("Skip unmodified element.")
    }
    // freeze element box
    transport.freezeElementBox(ancestorsNSelf, elementBox, elementDescriptorToYAML(elementBox), sData)
  }
  /** Internal method that saves the node descriptor. */
  protected def freezeNode(node: Node.ThreadUnsafe[_ <: Element], transport: Transport,
    ancestorsNSelf: Seq[Node.ThreadUnsafe[_ <: Element]], sData: SData) {
    log.debug(s"Freeze ${node}.")
    transport.freezeNode(ancestorsNSelf, nodeDescriptorToYAML(node.elementType, node.id, node.modified, node.state, node.unique, sData), sData)
    if (node.state.rootBox.getModified.nonEmpty)
      // save only modified element box
      freezeElementBox(node.state.rootBox, transport, ancestorsNSelf, sData)
    node.state.projectionBoxes.foreach {
      case (coordinate, box) ⇒
        if (box.getModified.nonEmpty)
          // save only modified element box
          freezeElementBox(box, transport, ancestorsNSelf, sData)
    }
    node.state.children.foreach(_.safeRead { child ⇒
      val childˈ = sData(SData.Key.freezeT)(child.asInstanceOf[Node.ThreadUnsafe[Element]])
      freezeNode(childˈ, transport, ancestorsNSelf :+ childˈ, sData)
    })
  }
  /** Get all latest available sources which is based on bootstrap parameters. */
  protected def getSources(bootstrapStorageURI: URI, transport: Transport, descriptor: Serialization.Descriptor.Graph[_ <: Model.Like],
    modified: Option[Element.Timestamp], sData: SData): Seq[Serialization.Source[_ <: Model.Like, _ <: Element]] = {
    // Modifications: the latest -> the earliest
    val sortedModifications = descriptor.records.sorted.reverse
    sortedModifications.foreach { modificationForLoad ⇒
      try {
        // 1. read record for the required modification.
        val recordName = Serialization.recordName(modificationForLoad)
        val recordURI = transport.append(bootstrapStorageURI, Serialization.retrospective, recordName)
        val record = recordFromYAML(transport.read(recordURI, sData))
        // 2. read record resources(array of origins and array of storages) for the required modification.
        val recordResourcesName = Serialization.recordResourcesName(modificationForLoad)
        val recordResourcesURI = transport.append(bootstrapStorageURI, Serialization.retrospective, recordResourcesName)
        val recordResources = recordResourcesFromYAML(transport.read(recordResourcesURI, sData))
        // 3. get storages that are used to store required modification.
        val origin = recordResources.origins(record.originIndex)
        val storages = record.storageIndexes.map(recordResources.storages)
        // 4. load descriptors from all storages
        if (storages.nonEmpty) {
          val sources: Seq[Option[Serialization.Source[_ <: Model.Like, _ <: Element]]] =
            storages.filterNot(_ == bootstrapStorageURI).map {
              case storageURI if storageURI.isAbsolute() ⇒ try {
                Serialization.perScheme.get(storageURI.getScheme()) match {
                  case Some(transport) ⇒
                    val descriptor = graphDescriptorFromYaml(transport.acquireGraph(origin, sData.updated(SData.Key.storageURI, storageURI)))
                    if (descriptor.origin == null)
                      throw new IllegalStateException("Origin value not found in graph descriptor file.")
                    if (descriptor.origin.name != origin.name)
                      throw new IllegalStateException(s"Incorrect saved origin value ${descriptor.origin.name} vs required ${origin.name}.")
                    val modelDescriptor = modified match {
                      case Some(modified) ⇒ // Load a model descriptor with the specific timestamp
                        getModelDescriptor(transport, descriptor, modified, sData.updated(SData.Key.storageURI, storageURI))
                      case None ⇒ // Load a model descriptor with the latest timestamp
                        getModelDescriptor(transport, descriptor, descriptor.modified, sData.updated(SData.Key.storageURI, storageURI))
                    }
                    Some(Serialization.Source(storageURI, transport, descriptor, modelDescriptor))
                  case None ⇒
                    log.error(s"Unable to acquire graph ${origin} from URI with unknown scheme ${storageURI.getScheme}.")
                    None
                }
              } catch {
                case e: IOException ⇒
                  log.warn(s"Unable to acquire graph ${origin} from ${storageURI}: " + e.getMessage())
                  None
                case e: Throwable ⇒
                  log.error(s"Unable to acquire graph ${origin} from ${storageURI}: " + e.getMessage(), e)
                  None
              }
              case storageURI ⇒
                log.fatal(s"Unable to process relative storage URI as base: ${storageURI}.")
                None
            }
          val preferredModelDescriptor = modified match {
            case Some(modified) ⇒ // Load a model descriptor with the specific timestamp
              getModelDescriptor(transport, descriptor, modified, sData)
            case None ⇒ // Load a model descriptor with the latest timestamp
              getModelDescriptor(transport, descriptor, descriptor.modified, sData)
          }
          val preferredSource = Serialization.Source(bootstrapStorageURI, transport, descriptor, preferredModelDescriptor, true)
          // Hurray!
          return preferredSource +: sources.flatten
        }
      } catch {
        case ce: ControlThrowable ⇒ throw ce
        case e: Throwable ⇒
          modified match {
            case Some(modified) ⇒
              log.warn(s"Unable to get source for ${modified}: " + e.getMessage(), e)
            case None ⇒
              log.warn(s"Unable to get latest source: " + e.getMessage(), e)
          }
      }
    }
    throw new IllegalArgumentException("There are no suitable sources found.")
  }
  /** Load model descriptor for the graph one. */
  protected def getModelDescriptor(transport: Transport,
    graphDescriptor: Serialization.Descriptor.Graph[_ <: Model.Like], modified: Element.Timestamp, sData: SData) = {
    val nodeDescriptor = nodeDescriptorFromYaml(transport.acquireModel(graphDescriptor.modelId, graphDescriptor.origin, modified, sData))
    val nodeDescriptorˈ = sData(SData.Key.acquireT)(Seq(), nodeDescriptor.asInstanceOf[Serialization.Descriptor.Node[Element]])
    if (nodeDescriptor.elements.isEmpty)
      throw new IllegalStateException("There are no elements in the model node.")
    if (nodeDescriptor.id == null)
      throw new IllegalStateException("Id value not found in model node descriptor file.")
    if (nodeDescriptor.unique == null)
      throw new IllegalStateException("Unique value not found in model node descriptor file.")
    if (nodeDescriptor.id.name != graphDescriptor.modelId.name)
      throw new IllegalStateException(s"Incorrect saved model id value ${nodeDescriptor.id.name} vs required ${graphDescriptor.modelId.name}.")
    nodeDescriptorˈ
  }
  /** Create element descriptor from YAML. */
  protected def graphDescriptorFromYaml(descriptor: Array[Byte]): Serialization.Descriptor.Graph[_ <: Model.Like] = YAMLSerialization.wrapper(
    yaml.YAML.block.loadAs(new String(descriptor, io.Codec.UTF8.charSet), classOf[Serialization.Descriptor.Graph[_ <: Model.Like]]), descriptor)
  /** Create YAML graph descriptor. */
  protected def graphDescriptorToYAML(model: Node.ThreadUnsafe[_ <: Model.Like], storages: Seq[URI]): Array[Byte] = {
    val descriptorMap = new java.util.TreeMap[String, AnyRef]()
    descriptorMap.put("created", model.graph.created)
    descriptorMap.put("model_id", model.id)
    descriptorMap.put("model_type", model.elementType.runtimeClass.getName())
    descriptorMap.put("modified", model.modified)
    descriptorMap.put("origin", model.graph.origin)
    descriptorMap.put("records", model.graph.retrospective.history.keys.toSeq.asJava)
    YAMLSerialization.wrapper(yaml.YAML.block.dump(descriptorMap).getBytes(io.Codec.UTF8.charSet), descriptorMap)
  }
  /** Create node descriptor from YAML content. */
  protected def nodeDescriptorFromYaml(descriptor: Array[Byte]): Serialization.Descriptor.Node[_ <: Element] = YAMLSerialization.wrapper(
    yaml.YAML.block.loadAs(new String(descriptor, io.Codec.UTF8.charSet), classOf[Serialization.Descriptor.Node[_ <: Element]]), descriptor)
  /** Create YAML node descriptor. */
  protected def nodeDescriptorToYAML(elementType: Manifest[_ <: Element], id: Symbol, modified: Element.Timestamp, state: NodeState[_ <: Element],
    unique: UUID, sData: SData): Array[Byte] = {
    val elements = state.projectionBoxes.values.toSeq.map(box ⇒
      Array(box.elementUniqueId, box.modified)).asJava
    val children = state.children.map(_.safeRead { child ⇒
      val childˈ = sData(SData.Key.freezeT)(child.asInstanceOf[Node.ThreadUnsafe[Element]])
      Array[AnyRef](childˈ.id, childˈ.modified)
    }).asJava
    val descriptorMap = new java.util.TreeMap[String, AnyRef]()
    descriptorMap.put("children", children)
    descriptorMap.put("class", elementType.runtimeClass.getName)
    descriptorMap.put("elements", elements)
    descriptorMap.put("id", id.name)
    descriptorMap.put("modified", modified)
    descriptorMap.put("unique", unique.toString())
    YAMLSerialization.wrapper(yaml.YAML.block.dump(descriptorMap).getBytes(io.Codec.UTF8.charSet), descriptorMap)
  }
  /** Create record content from YAML representation. */
  protected def recordFromYAML(representation: Array[Byte]): Serialization.Descriptor.Record =
    YAMLSerialization.wrapper(yaml.YAML.block.loadAs(new String(representation, io.Codec.UTF8.charSet), classOf[Serialization.Descriptor.Record]), representation)
  /** Create YAML representation from record content. */
  protected def recordToYAML(originIndex: Int, storageIndexes: Seq[Int]): Array[Byte] = {
    val recordMap = new java.util.TreeMap[String, AnyRef]()
    recordMap.put("originIndex", originIndex: Integer)
    recordMap.put("storageIndexes", storageIndexes.toSeq.asJava)
    YAMLSerialization.wrapper(yaml.YAML.block.dump(recordMap).getBytes(io.Codec.UTF8.charSet), recordMap)
  }
  /** Create record resources from YAML representation. */
  protected def recordResourcesFromYAML(representation: Array[Byte]): Serialization.Descriptor.RecordResources =
    YAMLSerialization.wrapper(yaml.YAML.block.loadAs(new String(representation, io.Codec.UTF8.charSet), classOf[Serialization.Descriptor.RecordResources]), representation)
  /** Create YAML representation from record resources. */
  protected def recordResourcesToYAML(origins: Seq[Symbol], storages: Seq[URI]): Array[Byte] = {
    val recordResourcesMap = new java.util.TreeMap[String, AnyRef]()
    recordResourcesMap.put("origins", origins.map(_.name).asJava)
    recordResourcesMap.put("storages", storages.map(_.toString()).asJava)
    YAMLSerialization.wrapper(yaml.YAML.block.dump(recordResourcesMap).getBytes(io.Codec.UTF8.charSet), recordResourcesMap)
  }
}

object Serialization extends Loggable {
  /** Transformation that is applied to acquiring nodes. */
  /* (parent node, child node descriptor) => transformed child node descriptor */
  type AcquireTransformation = Function2[Seq[Node.ThreadUnsafe[_ <: Element]], Descriptor.Node[Element], Descriptor.Node[Element]]
  /* node => copy of transformed node */
  /** Transformation that is applied to freezing nodes. */
  type FreezeTransformation = Function1[Node.ThreadUnsafe[Element], Node.ThreadUnsafe[Element]]
  /** Serialization stash. */
  val stash = new ThreadLocal[AnyRef]()

  /** Load graph with the specific origin. */
  def acquire(origin: Symbol, bootstrapStorageURI: URI, sData: SData): Graph[_ <: Model.Like] =
    acquireLoader(origin, bootstrapStorageURI, sData).load()
  /** Load graph with the specific origin. */
  def acquire(origin: Symbol, bootstrapStorageURI: URI, modified: Option[Element.Timestamp] = None, sData: SData = SData.empty): Graph[_ <: Model.Like] =
    acquireLoader(origin, bootstrapStorageURI, modified, sData).load()
  /** Get graph loader with the specific origin. */
  def acquireLoader(origin: Symbol, bootstrapStorageURI: URI): Serialization.Loader = acquireLoader(origin, bootstrapStorageURI, None, SData.empty)
  /** Get graph loader with the specific origin. */
  def acquireLoader(origin: Symbol, bootstrapStorageURI: URI, sData: SData): Serialization.Loader = acquireLoader(origin, bootstrapStorageURI, None, sData)
  /** Get graph loader with the specific origin. */
  def acquireLoader(origin: Symbol, bootstrapStorageURI: URI, modified: Option[Element.Timestamp] = None, sData: SData = SData.empty): Serialization.Loader =
    if (sData.isDefinedAt(SData.Key.acquireT))
      inner.acquireGraphLoader(origin, modified, sData.updated(SData.Key.storageURI, bootstrapStorageURI))
    else
      inner.acquireGraphLoader(origin, modified, sData.updated(SData.Key.storageURI, bootstrapStorageURI)
        .updated(SData.Key.acquireT, defaultAcquireTransformation _))
  /** Acquire transformation that keeps arguments unmodified. */
  def defaultAcquireTransformation(ancestors: Seq[Node.ThreadUnsafe[_ <: Element]], nodeDescriptor: Descriptor.Node[Element]) = nodeDescriptor
  /** Freeze transformation that keeps arguments unmodified. */
  def defaultFreezeTransformation(node: Node.ThreadUnsafe[Element]): Node.ThreadUnsafe[Element] = node
  /** Save graph. */
  def freeze(graph: Graph[_ <: Model.Like], additionalStorageURI: URI*): Element.Timestamp = freeze(graph, SData.empty, additionalStorageURI: _*)
  /** Save graph. */
  def freeze(graph: Graph[_ <: Model.Like], sData: SData, additionalStorageURI: URI*): Element.Timestamp = {
    val sDataWithT = if (sData.isDefinedAt(SData.Key.freezeT)) sData else sData.updated(SData.Key.freezeT, defaultFreezeTransformation _)
    additionalStorageURI match {
      case Nil ⇒
        inner.freezeGraph(graph, sDataWithT)
      case seq ⇒
        if (sDataWithT.isDefinedAt(SData.Key.explicitStorages))
          throw new IllegalArgumentException(s"Unable to add ${additionalStorageURI.mkString(",")}. There is already " + sData(SData.Key.explicitStorages))
        val append = Serialization.ExplicitStorages(seq, Serialization.ExplicitStorages.ModeAppend)
        inner.freezeGraph(graph, sDataWithT.updated(SData.Key.explicitStorages, append))
    }
  }
  /** Serialization implementation. */
  def inner = DI.implementation
  /** Normalize URI. Add trailing slash to URI since graph always located inside container. */
  def normalizeURI(arg: URI): URI =
    if (arg.getPath().endsWith("/"))
      arg
    else
      new URI(arg.getScheme(), arg.getAuthority(), arg.getPath() + "/", arg.getQuery(), arg.getFragment())
  /** Consumer defined map of per identifier serialization. */
  def perIdentifier = DI.perIdentifier
  /** Consumer defined map of per scheme transport. */
  def perScheme = DI.perScheme
  /** Retrospective container for recordResource and recordResources serialization data. */
  def retrospective = DI.retrospective
  /** Get record resource name (container for the history entry of the retrospective). */
  def recordName(modified: Element.Timestamp) = DI.recordResource.format(yaml.Timestamp.dump(modified))
  /** Get record resources name (container for origins and storages of the retrospective). */
  def recordResourcesName(modified: Element.Timestamp) = DI.recordResources.format(yaml.Timestamp.dump(modified))

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
      class Construct extends YAML.constructor.CustomConstruct {
        YAML.constructor.getYAMLConstructors.put(new Tag(classOf[Element[_ <: TAElement]]), this)

        protected val keyTypes = immutable.HashMap[String, PartialFunction[YAMLNode, Unit]](
          "coordinate" -> { case n: YAMLNode ⇒ YAML.constructor.setTagSafe(n, yaml.Coordinate.tag) },
          "element_unique_id" -> { case n: YAMLNode ⇒ YAML.constructor.setTagSafe(n, yaml.UUID.tag) },
          "modified" -> { case n: YAMLNode ⇒ YAML.constructor.setTagSafe(n, yaml.Timestamp.tag) })
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
    case class Node[A <: TAElement](val children: Seq[(Symbol, TAElement.Timestamp)], val clazz: Class[A],
      val elements: Seq[(UUID, TAElement.Timestamp)], val id: Symbol, val modified: TAElement.Timestamp, val unique: UUID) {
      assert(clazz != null, s"Broken descriptor ${this}.")
      assert(id != null, s"Broken descriptor ${this}.")
      assert(modified != null, s"Broken descriptor ${this}.")
      assert(unique != null, s"Broken descriptor ${this}.")
    }
    object Node {
      class Construct extends YAML.constructor.CustomConstruct {
        YAML.constructor.getYAMLConstructors.put(new Tag(classOf[Node[_ <: TAElement]]), this)

        protected val keyTypes = immutable.HashMap[String, PartialFunction[YAMLNode, Unit]](
          "children" -> {
            case n: SequenceNode ⇒ n.getValue().asScala.foreach {
              case n: SequenceNode ⇒
                val values = n.getValue()
                YAML.constructor.setTagSafe(values.get(0), yaml.Symbol.tag)
                YAML.constructor.setTagSafe(values.get(1), yaml.Timestamp.tag)
            }
          },
          "elements" -> {
            case n: SequenceNode ⇒ n.getValue().asScala.foreach {
              case n: SequenceNode ⇒
                val values = n.getValue()
                YAML.constructor.setTagSafe(values.get(0), yaml.UUID.tag)
                YAML.constructor.setTagSafe(values.get(1), yaml.Timestamp.tag)
            }
          },
          "id" -> { case n: YAMLNode ⇒ YAML.constructor.setTagSafe(n, yaml.Symbol.tag) },
          "modified" -> { case n: YAMLNode ⇒ YAML.constructor.setTagSafe(n, yaml.Timestamp.tag) },
          "unique" -> { case n: YAMLNode ⇒ YAML.constructor.setTagSafe(n, yaml.UUID.tag) })
        def constructCustom(map: mutable.HashMap[String, AnyRef]): AnyRef =
          Node(map("children").asInstanceOf[Iterable[java.util.ArrayList[AnyRef]]].toSeq.map(i ⇒ (i.get(0).asInstanceOf[Symbol], i.get(1).asInstanceOf[TAElement.Timestamp])),
            getClass.getClassLoader().loadClass(map("class").asInstanceOf[String]).asInstanceOf[Class[_ <: TAElement]],
            map("elements").asInstanceOf[Iterable[java.util.ArrayList[AnyRef]]].toSeq.map(i ⇒ (i.get(0).asInstanceOf[UUID], i.get(1).asInstanceOf[TAElement.Timestamp])),
            map("id").asInstanceOf[Symbol],
            map("modified").asInstanceOf[TAElement.Timestamp],
            map("unique").asInstanceOf[UUID])
      }
    }
    case class Graph[A <: Model.Like](val created: TAElement.Timestamp, val modelId: Symbol, val modelType: Class[A],
      val modified: TAElement.Timestamp, val origin: Symbol, val records: Seq[TAElement.Timestamp]) {
      assert(created != null, s"Broken descriptor ${this}.")
      assert(modelId != null, s"Broken descriptor ${this}.")
      assert(modelType != null, s"Broken descriptor ${this}.")
      assert(modified != null, s"Broken descriptor ${this}.")
      assert(origin != null, s"Broken descriptor ${this}.")
      assert(records.nonEmpty, s"Broken descriptor ${this}.")
    }
    object Graph {
      class Construct extends YAML.constructor.CustomConstruct {
        YAML.constructor.getYAMLConstructors.put(new Tag(classOf[Graph[_ <: Model.Like]]), this)

        protected val keyTypes = immutable.HashMap[String, PartialFunction[YAMLNode, Unit]](
          "created" -> { case n: YAMLNode ⇒ YAML.constructor.setTagSafe(n, yaml.Timestamp.tag) },
          "model_id" -> { case n: YAMLNode ⇒ YAML.constructor.setTagSafe(n, yaml.Symbol.tag) },
          "modified" -> { case n: YAMLNode ⇒ YAML.constructor.setTagSafe(n, yaml.Timestamp.tag) },
          "origin" -> { case n: YAMLNode ⇒ YAML.constructor.setTagSafe(n, yaml.Symbol.tag) },
          "records" -> { case n: SequenceNode ⇒ n.getValue().asScala.foreach(YAML.constructor.setTagSafe(_, yaml.Timestamp.tag)) })
        def constructCustom(map: mutable.HashMap[String, AnyRef]): AnyRef =
          Graph(map("created").asInstanceOf[TAElement.Timestamp],
            map("model_id").asInstanceOf[Symbol],
            getClass.getClassLoader().loadClass(map("model_type").asInstanceOf[String]).asInstanceOf[Class[_ <: Model.Like]],
            map("modified").asInstanceOf[TAElement.Timestamp],
            map("origin").asInstanceOf[Symbol],
            map("records").asInstanceOf[Iterable[TAElement.Timestamp]].toSeq)
      }
    }
    case class Record(val originIndex: Int, val storageIndexes: Seq[Int])
    object Record {
      class Construct extends YAML.constructor.CustomConstruct {
        YAML.constructor.getYAMLConstructors.put(new Tag(classOf[Record]), this)

        protected val keyTypes = immutable.HashMap[String, PartialFunction[YAMLNode, Unit]](
          "storageIndexes" -> { case n: SequenceNode ⇒ n.getValue().asScala })
        def constructCustom(map: mutable.HashMap[String, AnyRef]): AnyRef =
          Record(map("originIndex").asInstanceOf[Int], map("storageIndexes").asInstanceOf[Iterable[Int]].toSeq)
      }
    }
    case class RecordResources(val origins: Seq[Symbol], val storages: Seq[URI])
    object RecordResources {
      class Construct extends YAML.constructor.CustomConstruct {
        YAML.constructor.getYAMLConstructors.put(new Tag(classOf[RecordResources]), this)

        protected val keyTypes = immutable.HashMap[String, PartialFunction[YAMLNode, Unit]](
          "origins" -> { case n: SequenceNode ⇒ n.getValue().asScala },
          "storages" -> { case n: SequenceNode ⇒ n.getValue().asScala })
        def constructCustom(map: mutable.HashMap[String, AnyRef]): AnyRef =
          RecordResources(map("origins").asInstanceOf[Iterable[String]].map(name ⇒ Symbol(name)).toSeq,
            map("storages").asInstanceOf[Iterable[String]].map(uri ⇒ new URI(uri)).toSeq)
      }
    }
  }
  /**
   * Explicit serialization storages
   */
  case class ExplicitStorages(val storages: Seq[URI], mode: ExplicitStorages.Mode)
  object ExplicitStorages {
    sealed trait Mode
    /** Save graph to explicit and original storages. Save original values with serialized data. */
    case object ModeAppend extends Mode
    /** Save graph to explicit storages. Save original values with serialized data. */
    case object ModeIgnore extends Mode
    /** Save graph to original storages. Save explicit values with serialized data. */
    case object ModeReplace extends Mode
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
    /** Get graph loader for the specific origin. */
    def acquireGraphLoader(origin: Symbol, modified: Option[Element.Timestamp], sData: SData): Serialization.Loader
    /** Load graph with the graph loader. */
    def acquireGraph(loader: Serialization.Loader, source: Serialization.Source[_ <: Model.Like, _ <: Element],
      graphEarlyAccess: Graph[_ <: Model.Like] ⇒ Unit, sData: SData): Graph[_ <: Model.Like]
    /** Save graph. */
    def freezeGraph(graph: Graph[_ <: Model.Like], sData: SData): Element.Timestamp
  }
  /**
   * Graph loader.
   */
  class Loader(val sources: Seq[Source[_ <: Model.Like, _ <: Element]], val modified: Element.Timestamp, val sData: SData) {
    if (sources.isEmpty)
      throw new IllegalArgumentException("Unable to create loader without sources.")
    /** Model class. */
    lazy val modelTypeManifest = Manifest.classType[Model.Like](sources.head.graphDescriptor.modelType)

    /** Create graph. */
    def createGraph() = new Graph[Model.Like](sources.head.graphDescriptor.created,
      createModelNode, sources.head.graphDescriptor.origin)(modelTypeManifest)
    /** Load graph with this graph loader. */
    def load(source: Source[_ <: Model.Like, _ <: Element] = preferredSource, graphEarlyAccess: Graph[_ <: Model.Like] ⇒ Unit = (graph) ⇒ {}): Graph[_ <: Model.Like] =
      Serialization.inner.acquireGraph(this, source, graphEarlyAccess, sData.updated(SData.Key.storageURI, source.storageURI))
    /** Get preferred source. */
    def preferredSource: Source[_ <: Model.Like, _ <: Element] =
      sources.filter(s ⇒ s.preferred && s.modelDescriptor.modified == modified).headOption.getOrElse {
        sources.filter(s ⇒ s.modelDescriptor.modified == modified).headOption getOrElse sources.head
      }

    /** Create model node. */
    protected def createModelNode() = Node.model[Model.Like](sources.head.modelDescriptor.id,
      sources.head.modelDescriptor.unique, sources.head.modelDescriptor.modified)(modelTypeManifest)

    override def toString() = s"Loader(${sources.head.graphDescriptor})"
  }
  /**
   * Source with copy of graph.
   */
  case class Source[A <: Model.Like, B <: Element](val storageURI: URI, val transport: Transport,
    val graphDescriptor: Serialization.Descriptor.Graph[A],
    val modelDescriptor: Serialization.Descriptor.Node[B], preferred: Boolean = false)
  /**
   * Dependency injection routines
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    /** Implementation of the serialization. */
    lazy val implementation: Interface = injectOptional[Interface] getOrElse new Serialization
    /**
     * Per identifier serialization map.
     *
     * Each collected mechanism must be:
     *  1. an instance of Mechanism
     *  2. has name that starts with "Serialization.Mechanism."
     */
    lazy val perIdentifier: immutable.HashMap[Identifier, Mechanism] = {
      val mechanisms = bindingModule.bindings.filter {
        case (key, value) ⇒ classOf[Mechanism].isAssignableFrom(key.m.runtimeClass)
      }.map {
        case (key, value) ⇒
          key.name match {
            case Some(name) if name.startsWith("Serialization.Mechanism.") ⇒
              log.debug(s"'${name}' loaded.")
            case _ ⇒
              log.debug(s"'${key.name.getOrElse("Unnamed")}' serialization mechanism skipped.")
          }
          bindingModule.injectOptional(key).asInstanceOf[Option[Mechanism]]
      }.flatten.toSeq
      assert(mechanisms.distinct.size == mechanisms.size, "serialization mechanisms contains duplicated entities in " + mechanisms)
      immutable.HashMap(mechanisms.map(m ⇒ m.identifier -> m): _*)
    }
    /**
     * Per scheme transports map.
     *
     * Each collected transport must be:
     *  1. an instance of Transport
     *  2. has name that starts with "Serialization.Transport."
     */
    lazy val perScheme: immutable.HashMap[String, Transport] = {
      val transports = bindingModule.bindings.filter {
        case (key, value) ⇒ classOf[Transport].isAssignableFrom(key.m.runtimeClass)
      }.map {
        case (key, value) ⇒
          key.name match {
            case Some(name) if name.startsWith("Serialization.Transport.") ⇒
              log.debug(s"'${name}' loaded.")
            case _ ⇒
              log.debug(s"'${key.name.getOrElse("Unnamed")}' serialization transport skipped.")
          }
          bindingModule.injectOptional(key).asInstanceOf[Option[Transport]]
      }.flatten.toSeq
      assert(transports.distinct.size == transports.size, "serialization transports contains duplicated entities in " + transports)
      immutable.HashMap(transports.map(t ⇒ t.scheme -> t): _*)
    }
    /** Retrospective container for recordResource and recordResources serialization data. */
    lazy val retrospective = injectOptional[String]("Serialization.RetrospectiveContainer") getOrElse "retrospective"
    /** Record resource name template (container for history entry of the retrospective). */
    lazy val recordResource = injectOptional[String]("Serialization.RecordResource") getOrElse "%s-record.yaml"
    /** Get record resources name (container for origins and storages of the retrospective). */
    lazy val recordResources = injectOptional[String]("Serialization.RecordResources") getOrElse "%s-resources.yaml"
  }
}
