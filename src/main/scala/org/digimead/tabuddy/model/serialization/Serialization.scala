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

import java.io.{ FileNotFoundException, IOException }
import java.net.URI
import java.security.PublicKey
import java.util.UUID
import java.util.concurrent.CancellationException
import java.util.concurrent.atomic.AtomicReference
import org.digimead.digi.lib.aop.log
import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.{ Coordinate, Element }
import org.digimead.tabuddy.model.graph.{ ElementBox, Graph, Node, NodeState }
import org.digimead.tabuddy.model.serialization.digest.Digest
import org.digimead.tabuddy.model.serialization.signature.Signature
import org.digimead.tabuddy.model.serialization.transport.Transport
import org.digimead.tabuddy.model.serialization.yaml.YAML
import org.yaml.snakeyaml.nodes.{ Node ⇒ YAMLNode, SequenceNode, Tag }
import scala.collection.JavaConverters.{ asScalaBufferConverter, seqAsJavaListConverter }
import scala.collection.{ immutable, mutable }
import scala.language.implicitConversions
import scala.ref.WeakReference
import scala.util.control.ControlThrowable

/**
 * Common serialization implementation.
 */
class Serialization extends Serialization.Interface with Loggable {
  /** Method that loads graph with the graph loader. */
  @log(result = false) // Skip result logging. Graph contains lazy loaded elements that are loaded from toString
  def acquireGraph(loader: Serialization.Loader, graphEarlyAccess: Graph[_ <: Model.Like] ⇒ Unit, sData: SData): Graph[_ <: Model.Like] = sData.synchronized {
    val sources = sData(SData.Key.sources)
    if (sources.isEmpty)
      throw new IllegalArgumentException("Sources argument is empty.")
    log.debug("Sources by priority: " + sources.map(_.storageURI).mkString(","))
    /*
     * Create graph and model node
     */
    var graph = Option.empty[Graph[Model.Like]]
    for (source ← sources if graph.isEmpty)
      if (source.storageURI.isAbsolute()) {
        log.debug(s"Acquire graph ${source.modelDescriptor.id} meta information from ${source.storageURI}.")
        val sDataForStorage = sData.updated(SData.Key.storageURI, source.storageURI)
        val basis = loader.createGraph()
        graphEarlyAccess(basis)
        basis.node.safeWrite { targetNode ⇒
          basis.node.asInstanceOf[Node.ModelNodeInitializer].initializeModelNode(basis, loader.modified)
          val successful = try {
            // 0. Invoke user onStart callback if required.
            sDataForStorage.get(SData.Key.beforeAcquire).map(_(basis, source.transport, sDataForStorage))
            // 1. restore retrospective.
            val recordResourcesName = Serialization.recordResourcesName(loader.modified)
            val recordResourcesURI = source.transport.append(source.storageURI, Serialization.retrospective, recordResourcesName)
            val recordResources = recordResourcesFromYAML(source.transport.read(encode(recordResourcesURI, sDataForStorage), sDataForStorage))
            val history = Map(source.graphDescriptor.records.filter(_ <= loader.modified).par.flatMap { modified ⇒
              log.debug(s"Load history record ${yaml.Timestamp.dump(modified)} (${modified}).")
              var entry: Option[(Element.Timestamp, Graph.Retrospective.Indexes)] = None
              for (source ← source +: sources.filterNot(_ == source) if entry.isEmpty) try {
                val recordName = Serialization.recordName(modified)
                val recordURI = source.transport.append(source.storageURI, Serialization.retrospective, recordName)
                val record = recordFromYAML(source.transport.read(encode(recordURI, sDataForStorage),
                  sDataForStorage.updated(SData.Key.storageURI, source.storageURI)))
                entry = Some(modified -> Graph.Retrospective.Indexes(record.originIndex, record.storageIndexes))
              } catch {
                case e: SecurityException ⇒
                  log.warn(s"Unable to load history record ${modified}: " + e.getMessage)
                case e: IOException ⇒
                  log.warn(s"Unable to load history record ${modified}: " + e.getMessage)
              }
              if (entry.isEmpty) {
                if (sDataForStorage.get(SData.Key.force) == Some(true))
                  log.warn(s"Skip history record ${modified}")
                else
                  throw new IllegalStateException(s"History record ${modified} is not available.")
              }
              entry
            }.seq: _*)
            basis.retrospective = Graph.Retrospective(history, recordResources.origins, recordResources.storages)
            // Get data.
            log.debug(s"Acquire graph ${source.modelDescriptor.id} content.")
            // 2. setup projections.
            val elementBoxes = source.modelDescriptor.elements.map {
              case (elementUniqueId, elementModificationTimestamp) ⇒
                val elementBoxURI = source.transport.getElementBoxURI(Seq(), elementUniqueId, elementModificationTimestamp, sDataForStorage)
                val elementBoxDescriptor = elementBoxDescriptorFromYaml(source.transport.read(encode(elementBoxURI, sDataForStorage), sDataForStorage))
                ElementBox[Model.Like](elementBoxDescriptor.coordinate, elementBoxDescriptor.elementUniqueId,
                  targetNode, elementBoxDescriptor.serializationIdentifier, sDataForStorage,
                  elementBoxDescriptor.modified)(Manifest.classType(elementBoxDescriptor.clazz))
            }
            if (!elementBoxes.exists(_.coordinate == Coordinate.root))
              throw new IllegalStateException("Root element not found.")
            val projectionBoxes: Seq[(Coordinate, ElementBox[Model.Like])] = elementBoxes.map(e ⇒ e.coordinate -> e)
            targetNode.updateState(modified = null, projectionBoxes = immutable.HashMap(projectionBoxes: _*)) // modification is already assigned
            // Graph is valid at this point.
            // 3. add children.
            val children = source.modelDescriptor.children.flatMap {
              case (childId, childModificationTimestamp) ⇒
                try acquireNode(childId, childModificationTimestamp, Seq(targetNode), sDataForStorage)
                catch { case e: Throwable ⇒ throw new IllegalStateException("NodeAcquireException", e) } // acquireNode has their own per source loop
            }
            if (children.nonEmpty) {
              targetNode.updateState(children = children, modified = null) // modification is already assigned
              targetNode.children.foreach(_.safeRead(_.registerWithAncestors()))
            }
            if (basis.modelType != basis.node.elementType)
              throw new IllegalArgumentException(s"Unexpected model type ${basis.modelType} vs ${basis.node.elementType}")
            // 4. Invoke user onComplete callback if required.
            sDataForStorage.get(SData.Key.afterAcquire).map(_(basis, source.transport, sDataForStorage))
            graph = Some(basis)
          } catch {
            case e: FileNotFoundException ⇒ log.info(s"Unable to acquire information from ${source.storageURI}: ${e.getMessage()}")
            case e: IllegalStateException if e.getMessage() == "NodeAcquireException" ⇒ throw e.getCause()
            case e: SecurityException ⇒ log.info(s"Unable to acquire information from ${source.storageURI}: ${e.getMessage()}")
            case e: Throwable ⇒ log.error(s"Unable to acquire information from ${source.storageURI}: ${e.getMessage()}", e)
          }
        }
      } else {
        log.fatal(s"Unable to process relative storage URI as base: ${source.storageURI}.")
      }
    graph getOrElse { throw new IllegalStateException(s"Unable to acquire graph ${sources.head.modelDescriptor.id}") }
  }
  /** Get graph loader for the specific origin. */
  def acquireGraphLoader(modified: Option[Element.Timestamp], sDataOriginal: SData): Serialization.Loader = sDataOriginal.synchronized {
    // Put sData into AtomicReference container because it may be modified by 'initialize' hooks.
    val sData = new AtomicReference(sDataOriginal)
    if (sData.get.get(Signature.Key.acquire).nonEmpty)
      // Digest is REQUIRED for signature verification.
      sData.set(sData.get.updated(Digest.Key.acquire, true))
    sData.set(sData.get.updated(SData.Key.storageURI, addTrailingSlash(sData.get()(SData.Key.storageURI))))
    val storageURI = sData.get()(SData.Key.storageURI)
    log.debug(s"Acquire graph loader from ${storageURI}.")
    val sources = Serialization.perScheme.get(storageURI.getScheme()) match {
      case Some(transport) ⇒
        val graphURI = transport.getGraphURI(sData.get)
        val untrustedGraphDescriptor = graphDescriptorFromYaml(transport.read(encode(graphURI, sData.get), sData.get))
        if (untrustedGraphDescriptor.origin == null)
          throw new IllegalStateException("Origin value not found in graph descriptor file.")
        // At this point we have graphDescriptor.
        // This graph descriptor may have incorrect or broken parts
        // and it is absolutely untrusted.
        // BUT. It has all initial information.
        // It is like a public class loader that initialize OSGi framework.
        // Initialize Digest
        sData.set(Digest.initAcquire(sData.get()))
        val sources = getSources(transport, untrustedGraphDescriptor, modified, sData)
        if (sources.isEmpty)
          throw new IllegalStateException("There are no suitable sources found.")
        modified match {
          case Some(modified) ⇒
            // Pass only sources that contain required modification.
            sources.filter(_.graphDescriptor.records.contains(modified))
          case None ⇒
            // Pass all sources
            sources
        }
      case None ⇒
        throw new IllegalArgumentException(s"Unable to load graph from URI with unknown scheme ${storageURI.getScheme}.")
    }
    if (sources.isEmpty)
      throw new IllegalArgumentException("Unable to aquire graph loader. There are no suitable sources found.")
    sources.foreach(source ⇒ Source.recalculate(source, storageURI == source.storageURI, sData.get))
    modified match {
      case Some(modified) ⇒ new Serialization.Loader(sources, modified,
        sData.get)
      case None ⇒ new Serialization.Loader(sources, sources.map(_.graphDescriptor.modified).max,
        sData.get)
    }
  }
  /** Save graph. */
  def freezeGraph(graph: Graph[_ <: Model.Like], sData: SData): Element.Timestamp = sData.synchronized {
    val graphRetrospective = graph.retrospective
    val graphStorages = graph.storages
    try {
      log.debug(s"Freeze ${graph}.")
      val (publicStorages, realStorages) = sData.get(SData.Key.explicitStorages) match {
        case Some(storages: Serialization.Storages) ⇒
          val public = storages.seq.flatMap(_.public)
          if (public.distinct.size != public.size)
            throw new IllegalStateException("ExplicitStorages contain duplicated public values.")
          val real = storages.seq.flatMap(_.real)
          if (real.distinct.size != real.size)
            throw new IllegalStateException("ExplicitStorages contain duplicated real values.")
          (public, real)
        case None ⇒
          val storages = graphStorages.map(Serialization.normalizeURI).distinct.map(addTrailingSlash)
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
                try {
                  // Always freeze even if modification is the same
                  val sDataForStorageWithoutModification = sData.updated(SData.Key.storageURI, storageURI)
                  val modelˈ = sDataForStorageWithoutModification(SData.Key.freezeT)(modelNode.asInstanceOf[Node.ThreadUnsafe[Element]]).asInstanceOf[Node.ThreadUnsafe[Model.Like]]
                  val sDataForStorage = sDataForStorageWithoutModification.updated(SData.Key.modified, modelˈ.modified)
                  // 0. Invoke user onStart callback if required.
                  sDataForStorage.get(SData.Key.beforeFreeze).map(_(modelˈ.graph, transport, sDataForStorage))
                  // 1. Add new record to history.
                  val overwriteLastHistoryRecord = graphRetrospective.history.isDefinedAt(modelˈ.modified) && {
                    // There are new storages.
                    val storages = graphRetrospective.getStorages(modelˈ.modified)
                    publicStorages.exists(s ⇒ !storages.contains(s))
                  }
                  if (!graphRetrospective.history.isDefinedAt(modelˈ.modified) || overwriteLastHistoryRecord) {
                    val origins = (graphRetrospective.origins :+ graph.origin).distinct
                    val storages = (graphRetrospective.storages ++ publicStorages).distinct
                    val record = Graph.Retrospective.Indexes(origins.indexOf(graph.origin), publicStorages.map(s ⇒ storages.indexOf(s)).toSeq)
                    if (record.originIndex < 0)
                      throw new IllegalStateException(s"Unable to find index for origin ${graph.origin} in ${origins.mkString(",")}.")
                    if (record.storageIndexes.exists(_ < 0))
                      throw new IllegalStateException(s"Unable to find index for storages ${publicStorages.mkString(",")} in ${storages.mkString(",")}.")
                    graph.retrospective = Graph.Retrospective(graphRetrospective.history + (modelˈ.modified -> record), origins, storages)
                    true
                  }
                  // 2. Freeze graph descriptor.
                  val graphURI = transport.getGraphURI(sDataForStorage)
                  // Overwrite always
                  transport.write(encode(graphURI, sDataForStorage), graphDescriptorToYAML(modelˈ,
                    publicStorages.map(Serialization.normalizeURI).distinct), sDataForStorage)
                  transport.writeTimestamp(graphURI, sDataForStorage)
                  // 3. Freeze all graph nodes.
                  freezeNode(modelˈ, transport, Seq(), sDataForStorage)
                  // 4. Freeze all graph records if required.
                  graph.retrospective.history.get(modelˈ.modified) match {
                    case Some(Graph.Retrospective.Indexes(originIndex, storageIndexes)) ⇒
                      val recordName = Serialization.recordName(modelˈ.modified)
                      val recordURI = transport.append(storageURI, Serialization.retrospective, recordName)
                      if (!transport.exists(encode(recordURI, sDataForStorage), sDataForStorage) ||
                        sDataForStorage.get(SData.Key.force) == Some(true)) {
                        transport.write(encode(recordURI, sDataForStorage),
                          recordToYAML(originIndex, storageIndexes), sDataForStorage)
                        transport.writeTimestamp(recordURI, sDataForStorage)
                      }
                    case None ⇒
                      throw new IllegalStateException("Unable to find history index for the latest modification " + modelˈ.modified)
                  }
                  val recordResourcesName = Serialization.recordResourcesName(modelˈ.modified)
                  val recordResourcesURI = transport.append(storageURI, Serialization.retrospective, recordResourcesName)
                  if (!transport.exists(encode(recordResourcesURI, sDataForStorage), sDataForStorage) ||
                    sDataForStorage.get(SData.Key.force) == Some(true)) {
                    transport.write(encode(recordResourcesURI, sDataForStorage),
                      recordResourcesToYAML(graph.retrospective.origins, graph.retrospective.storages), sDataForStorage)
                    transport.writeTimestamp(recordResourcesURI, sDataForStorage)
                  }
                  // 5. Invoke user onComplete callback if required.
                  sDataForStorage.get(SData.Key.afterFreeze).map(_(modelˈ.graph, transport, sDataForStorage))
                  Some(modelˈ.modified)
                } catch {
                  case e: CancellationException ⇒
                    log.info(s"Unable to save graph to ${storageURI}: ${e.getMessage()}.")
                    None
                }
              case None ⇒
                log.error(s"Unable to save graph to URI with unknown scheme '${storageURI.getScheme}'.")
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
  protected def acquireNode(id: Symbol, modified: Element.Timestamp,
    ancestors: Seq[Node.ThreadUnsafe[_ <: Element]], sData: SData): Option[Node[_ <: Element]] = try {
    log.debug(s"Acquire node ${id}, modification ${yaml.Timestamp.dump(modified)}.")
    val sources = sData(SData.Key.sources)
    var nodeDescriptor = Option.empty[Serialization.Descriptor.Node[_ <: Element]]
    for (source ← sources if nodeDescriptor.isEmpty)
      if (source.storageURI.isAbsolute()) {
        try {
          log.debug(s"Acquire node ${id} content from ${source.storageURI}.")
          val sDataForStorage = sData.updated(SData.Key.storageURI, source.storageURI)
          Serialization.perScheme.get(source.storageURI.getScheme()) match {
            case Some(transport) ⇒
              val nodeURI = transport.getNodeURI(ancestors, id, modified, sDataForStorage)
              val nodeDescriptorˈ = nodeDescriptorFromYaml(transport.read(encode(nodeURI, sDataForStorage), sDataForStorage))
              if (nodeDescriptorˈ.elements.isEmpty)
                throw new IllegalStateException("There are no elements in the node.")
              if (nodeDescriptorˈ.id == null)
                throw new IllegalStateException("Id value not found in node descriptor file.")
              if (nodeDescriptorˈ.unique == null)
                throw new IllegalStateException("Unique value not found in model node descriptor file.")
              if (nodeDescriptorˈ.id.name != id.name)
                throw new IllegalStateException(s"Incorrect saved node id value ${nodeDescriptorˈ.id.name} vs required ${id.name}.")
              nodeDescriptor = Some(nodeDescriptorˈ)
            case None ⇒
              log.error(s"Unable to acquire node ${id} from URI with unknown scheme ${source.storageURI.getScheme}.")
          }
        } catch {
          case e: SecurityException ⇒ log.warn(s"Unable to load node ${id} ${modified}: " + e.getMessage())
          case e: IOException ⇒ log.error(s"Unable to load node ${id} ${modified}: " + e.getMessage())
          case e: Throwable ⇒ log.error(s"Unable to load node ${id} ${modified}: " + e.getMessage(), e)
        }
      } else {
        log.fatal(s"Unable to process relative storage URI as base: ${source.storageURI}.")
      }
    val descriptor = nodeDescriptor getOrElse { throw new IllegalStateException(s"Unable to acquire node ${id} ${modified}") }
    val descriptorˈ = sData(SData.Key.acquireT)(ancestors, descriptor.asInstanceOf[Serialization.Descriptor.Node[Element]])
    Some(acquireNode(descriptorˈ, ancestors, sData))
  } catch {
    case e: Throwable ⇒
      log.error(s"Unable to load node ${id} : " + e.getMessage(), e)
      if (sData.get(SData.Key.force) == Some(true))
        None
      else
        throw e // rethrow
  }
  /** Internal method that converts node descriptor to node. */
  protected def acquireNode(nodeDescriptor: Serialization.Descriptor.Node[_ <: Element],
    ancestors: Seq[Node.ThreadUnsafe[_ <: Element]], sData: SData): Node[_ <: Element] = {
    val sources = sData(SData.Key.sources)
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
          log.debug(s"Acquire element box ${elementUniqueId} for node ${nodeDescriptor.id}, modification ${yaml.Timestamp.dump(elementModificationTimestamp)}.")
          var elementBox = Option.empty[ElementBox[Element]]
          for (source ← sources if elementBox.isEmpty)
            if (source.storageURI.isAbsolute()) {
              try {
                log.debug(s"Acquire element box ${elementUniqueId} content from ${source.storageURI}.")
                val sDataForStorage = sData.updated(SData.Key.storageURI, source.storageURI)
                val elementBoxURI = source.transport.getElementBoxURI(ancestors, elementUniqueId, elementModificationTimestamp, sDataForStorage)
                val elementBoxDescriptor = elementBoxDescriptorFromYaml(source.transport.read(encode(elementBoxURI, sDataForStorage), sDataForStorage))
                elementBox = Some(ElementBox[Element](elementBoxDescriptor.coordinate, elementBoxDescriptor.elementUniqueId, targetNode,
                  elementBoxDescriptor.serializationIdentifier, sDataForStorage,
                  elementBoxDescriptor.modified)(Manifest.classType(elementBoxDescriptor.clazz)))
              } catch {
                case e: SecurityException ⇒ log.warn(s"Unable to load element box ${elementUniqueId} ${elementModificationTimestamp}: " + e.getMessage())
                case e: IOException ⇒ log.error(s"Unable to load element box ${elementUniqueId} ${elementModificationTimestamp}: " + e.getMessage())
                case e: Throwable ⇒ log.error(s"Unable to load element box ${elementUniqueId} ${elementModificationTimestamp}: " + e.getMessage(), e)
              }
            } else {
              log.fatal(s"Unable to process relative storage URI as base: ${source.storageURI}.")
            }
          elementBox getOrElse { throw new IllegalStateException(s"Unable to load element box ${elementUniqueId} ${elementModificationTimestamp}") }
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
  /** Create element box descriptor from YAML. */
  protected def elementBoxDescriptorFromYaml(descriptor: Array[Byte]): Serialization.Descriptor.Element[_ <: Element] = YAMLSerialization.wrapper(
    yaml.YAML.block.loadAs(new String(descriptor, io.Codec.UTF8.charSet), classOf[Serialization.Descriptor.Element[_ <: Element]]), descriptor)
  /** Create YAML element box descriptor. */
  protected def elementBoxDescriptorToYAML(elementBox: ElementBox[_ <: Element], sData: SData): Array[Byte] = {
    val descriptorMap = new java.util.TreeMap[String, AnyRef]()
    descriptorMap.put("coordinate", elementBox.coordinate)
    descriptorMap.put("class", elementBox.node.elementType.runtimeClass.getName)
    descriptorMap.put("element_unique_id", elementBox.elementUniqueId)
    descriptorMap.put("modified", elementBox.modified)
    descriptorMap.put("serialization_identifier", sData.get(SData.Key.explicitSerializationType).map(_.extension) getOrElse elementBox.serialization.extension)
    YAMLSerialization.wrapper(yaml.YAML.block.dump(descriptorMap).getBytes(io.Codec.UTF8.charSet), descriptorMap)
  }
  /** Internal method that saves the element content. */
  protected def freezeElementBox(elementBox: ElementBox[_ <: Element], transport: Transport,
    ancestors: Seq[Node.ThreadUnsafe[_ <: Element]], sData: SData) {
    log.debug(s"Freeze ${elementBox}.")
    // freeze element box
    val elementBoxURI = transport.getElementBoxURI(ancestors, elementBox.elementUniqueId, elementBox.modified, sData)
    if (transport.exists(encode(elementBoxURI, sData), sData) && sData.get(SData.Key.force) != Some(true))
      return
    transport.write(encode(elementBoxURI, sData), elementBoxDescriptorToYAML(elementBox, sData), sData)
    transport.writeTimestamp(elementBoxURI, sData)
    if (elementBox.getModified.map(_ ne elementBox.e) == Some(true))
      throw new IllegalStateException("Element and modified element are different.")
    elementBox.save(sData)
    elementBox.e.eStash.property.foreach {
      case (valueId, perTypeMap) ⇒ perTypeMap.foreach {
        case (typeSymbolId, value) ⇒
          val elementRootURI = transport.getSubElementURI(ancestors, elementBox.elementUniqueId, elementBox.modified, sData)
          value.commit(elementBox.e, transport, elementRootURI)
      }
    }
  }
  /** Internal method that saves the node descriptor. */
  protected def freezeNode(node: Node.ThreadUnsafe[_ <: Element], transport: Transport,
    ancestors: Seq[Node.ThreadUnsafe[_ <: Element]], sData: SData) {
    log.debug(s"Freeze ${node}.")
    val nodeURI = transport.getNodeURI(ancestors, node.id, node.modified, sData)
    if (transport.exists(encode(nodeURI, sData), sData) && sData.get(SData.Key.force) != Some(true))
      return
    transport.write(encode(nodeURI, sData),
      nodeDescriptorToYAML(node.elementType, node.id, node.modified, node.state, node.unique, sData), sData)
    transport.writeTimestamp(nodeURI, sData)
    node.state.projectionBoxes.foreach { case (coordinate, box) ⇒ freezeElementBox(box, transport, ancestors, sData) }
    node.state.children.foreach(_.safeRead { child ⇒
      val childˈ = sData(SData.Key.freezeT)(child.asInstanceOf[Node.ThreadUnsafe[Element]])
      freezeNode(childˈ, transport, ancestors :+ node, sData)
    })
  }
  /** Get all latest available sources which is based on bootstrap parameters. */
  protected def getSources(transport: Transport, untrustedGraphDescriptor: Serialization.Descriptor.Graph[_ <: Model.Like],
    modified: Option[Element.Timestamp], sData: AtomicReference[SData]): Seq[Source[_ <: Model.Like, _ <: Element]] = {
    val storageURI = sData.get()(SData.Key.storageURI)
    // Modifications: the latest -> the earliest
    val sortedModifications = untrustedGraphDescriptor.records.sorted.reverse
    sortedModifications.foreach { modificationForLoad ⇒
      try {
        /*
         * Why?
         *
         * This is an UNTRUSTED entry point.
         * If there is an error or broken digest or signature then
         *   we are unable to load even list of mirrors.
         * It is not acceptable. Load everything, even broken data.
         */
        val permissiveSData = sData.get() - Digest.Key.acquire - Signature.Key.acquire
        // 1. read record for the required modification.
        val recordName = Serialization.recordName(modificationForLoad)
        val recordURI = transport.append(storageURI, Serialization.retrospective, recordName)
        val record = recordFromYAML(transport.read(encode(recordURI, permissiveSData), permissiveSData))
        // 2. read record resources(array of origins and array of storages) for the required modification.
        val recordResourcesName = Serialization.recordResourcesName(modificationForLoad)
        val recordResourcesURI = transport.append(storageURI, Serialization.retrospective, recordResourcesName)
        val recordResources = recordResourcesFromYAML(transport.read(encode(recordResourcesURI, permissiveSData), permissiveSData))
        /*
         * OK. Fine. Now we have initial data. Apply proper security settings.
         */
        sData.get().get(SData.Key.initializeSourceSData).foreach(initialize ⇒ sData.set(initialize(modificationForLoad, transport, sData.get)))
        // 3. get storages that are used to store required modification.
        val origin = recordResources.origins(record.originIndex)
        val storages = (recordResources.storages.toSet + storageURI).toSeq.map(addTrailingSlash)
        // 4. initialize sData
        storages.foreach {
          case storageURI if storageURI.isAbsolute() ⇒ try {
            Serialization.perScheme.get(storageURI.getScheme()).foreach { transport ⇒
              sData.get().get(SData.Key.initializeSourceSData) match {
                case Some(initialize) ⇒
                  sData.set(initialize(modificationForLoad, transport, sData.get.updated(SData.Key.storageURI, storageURI)))
                case None ⇒
                  sData.set(sData.get.updated(SData.Key.storageURI, storageURI))
              }
            }
          } catch {
            case e: IOException ⇒
              log.warn(s"Unable to initialize SData ${storageURI}: " + e.getMessage())
            case e: Throwable ⇒
              log.error(s"Unable to initialize SData from ${storageURI}: " + e.getMessage(), e)
          }
          case storageURI ⇒
            log.fatal(s"Unable to process relative storage URI as base: ${storageURI}.")
        }
        // 5. load descriptors from all storages
        val sources: Seq[Option[Source[_ <: Model.Like, _ <: Element]]] =
          storages.map {
            case storageURI if storageURI.isAbsolute() ⇒ try {
              Serialization.perScheme.get(storageURI.getScheme()) match {
                case Some(transport) ⇒
                  val sDataForStorageWithSoftDigest =
                    // Load data with a correct digest or without digests at all.
                    if (sData.get.isDefinedAt(Digest.Key.acquire))
                      sData.get.updated(SData.Key.storageURI, storageURI).updated(Digest.Key.acquire, false)
                    else
                      sData.get.updated(SData.Key.storageURI, storageURI)
                  val sDataForStorage =
                    // Accept only data with a correct signature or without signatures at all.
                    if (sDataForStorageWithSoftDigest.isDefinedAt(Signature.Key.acquire))
                      sDataForStorageWithSoftDigest.updated(Signature.Key.acquire, Signature.acceptSigned)
                    else
                      sDataForStorageWithSoftDigest
                  val graphURI = transport.getGraphURI(sDataForStorage)
                  val graphDescriptor = graphDescriptorFromYaml(transport.read(encode(graphURI, sDataForStorage), sDataForStorage))
                  if (graphDescriptor.origin == null)
                    throw new IllegalStateException("Origin value not found in graph descriptor file.")
                  if (graphDescriptor.origin.name != origin.name)
                    throw new IllegalStateException(s"Incorrect saved origin value ${graphDescriptor.origin.name} vs required ${origin.name}.")
                  val modelDescriptor = modified match {
                    case Some(modified) ⇒ // Load a model descriptor with the specific timestamp
                      getModelDescriptor(transport, graphDescriptor, modified, sDataForStorage)
                    case None ⇒ // Load a model descriptor with the latest timestamp
                      getModelDescriptor(transport, graphDescriptor, graphDescriptor.modified, sDataForStorage)
                  }
                  Some(Source(storageURI, transport, graphDescriptor, modelDescriptor))
                case None ⇒
                  log.error(s"Unable to acquire graph ${origin} from URI with unknown scheme ${storageURI.getScheme}.")
                  None
              }
            } catch {
              case e: SecurityException ⇒
                log.warn(s"Unable to acquire graph ${origin} from ${storageURI}: " + e.getMessage())
                None
              case e: IOException ⇒
                log.warn(s"Unable to acquire graph ${origin} from ${storageURI}: " + e.getMessage())
                None
              case e: Throwable ⇒
                log.fatal(s"Unable to acquire graph ${origin} from ${storageURI}: " + e.getMessage(), e)
                None
            }
            case storageURI ⇒
              log.fatal(s"Unable to process relative storage URI as base: ${storageURI}.")
              None
          }
        // Hurray!
        return sources.flatten
      } catch {
        case ce: ControlThrowable ⇒ throw ce
        case e: SecurityException ⇒
          modified match {
            case Some(modified) ⇒
              log.warn(s"Unable to get source for ${modified}: " + e.getMessage())
            case None ⇒
              log.warn(s"Unable to get latest source: " + e.getMessage())
          }
        case e: Throwable ⇒
          modified match {
            case Some(modified) ⇒
              log.warn(s"Unable to get source for ${modified}: " + e.getMessage(), e)
            case None ⇒
              log.warn(s"Unable to get latest source: " + e.getMessage(), e)
          }
      }
    }
    Nil
  }
  /** Load model descriptor for the graph one. */
  protected def getModelDescriptor(transport: Transport,
    graphDescriptor: Serialization.Descriptor.Graph[_ <: Model.Like], modified: Element.Timestamp, sData: SData) = {
    val nodeURI = transport.getNodeURI(Seq(), graphDescriptor.modelId, modified, sData)
    val nodeDescriptor = nodeDescriptorFromYaml(transport.read(encode(nodeURI, sData), sData))
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
  /** Hex array characters. */
  protected val hexArray = "0123456789ABCDEF".toCharArray()

  /** Load graph with the specific origin. */
  def acquire(bootstrapStorageURI: URI, sData: SData): Graph[_ <: Model.Like] =
    acquireLoader(bootstrapStorageURI, sData).load()
  /** Load graph with the specific origin. */
  def acquire(bootstrapStorageURI: URI, modified: Option[Element.Timestamp] = None, sData: SData = SData.empty): Graph[_ <: Model.Like] =
    acquireLoader(bootstrapStorageURI, modified, sData).load()
  /** Get graph loader with the specific origin. */
  def acquireLoader(bootstrapStorageURI: URI, sData: SData): Serialization.Loader = acquireLoader(bootstrapStorageURI, None, sData)
  /** Get graph loader with the specific origin. */
  def acquireLoader(bootstrapStorageURI: URI, modified: Option[Element.Timestamp] = None, sData: SData = SData.empty): Serialization.Loader = {
    if (sData.get(Digest.Key.acquire) == Some(false) && sData.get(Signature.Key.acquire).nonEmpty)
      throw new IllegalArgumentException("Incompatible combination: Signature.Key.acquire and Digest.Key.acquire -> false")
    val acquireTReady = if (sData.isDefinedAt(SData.Key.acquireT)) sData else sData.updated(SData.Key.acquireT, defaultAcquireTransformation _)
    val storageReady = acquireTReady.updated(SData.Key.storageURI, bootstrapStorageURI)
    // Digest.initAcquire is invoked inside acquireGraphLoader
    val signatureReady = Signature.initAcquire(storageReady)
    val userReady = signatureReady.get(SData.Key.initializeAcquireSData).map(_(bootstrapStorageURI, modified, signatureReady)) getOrElse signatureReady
    val loader = inner.acquireGraphLoader(modified, userReady)
    // Adjust loader inside SData.Key.initializeLoader callback
    loader.sData.get(SData.Key.initializeLoader).map(_(loader)) getOrElse loader
  }
  /** Convert byte array to hex string. */
  def byteArrayToHexString(bytes: Array[Byte]): String = {
    val hexChars = new Array[Char](bytes.length * 2)
    for (j ← 0 until bytes.length) {
      val v = bytes(j) & 0xFF
      hexChars(j * 2) = hexArray(v >>> 4)
      hexChars(j * 2 + 1) = hexArray(v & 0x0F)
    }
    new String(hexChars)
  }
  /** Acquire transformation that keeps arguments unmodified. */
  def defaultAcquireTransformation(ancestors: Seq[Node.ThreadUnsafe[_ <: Element]], nodeDescriptor: Descriptor.Node[Element]) = nodeDescriptor
  /** Freeze transformation that keeps arguments unmodified. */
  def defaultFreezeTransformation(node: Node.ThreadUnsafe[Element]): Node.ThreadUnsafe[Element] = node
  /** Save graph. */
  def freeze(graph: Graph[_ <: Model.Like], additionalStorageURI: URI*): Element.Timestamp = freeze(graph, SData.empty, additionalStorageURI: _*)
  /** Save graph. */
  def freeze(graph: Graph[_ <: Model.Like], sData: SData, additionalStorageURI: URI*): Element.Timestamp = {
    val freezeTReady = if (sData.isDefinedAt(SData.Key.freezeT)) sData else sData.updated(SData.Key.freezeT, defaultFreezeTransformation _)
    val storageReady = additionalStorageURI match {
      case Nil ⇒
        freezeTReady
      case seq ⇒
        if (freezeTReady.isDefinedAt(SData.Key.explicitStorages))
          throw new IllegalArgumentException(s"Unable to add ${additionalStorageURI.mkString(",")}. There is already " + sData(SData.Key.explicitStorages))
        // Append seq to graph.storages
        val union = graph.storages.map(storage ⇒ Storages.Simple(storage)) ++
          seq.map(storage ⇒ Storages.Simple(storage))
        freezeTReady.updated(SData.Key.explicitStorages, Serialization.Storages(union))
    }
    val explicitStoragesReady = Storages.init(storageReady)
    // Order is important ->
    val signatureReady = Signature.initFreeze(explicitStoragesReady)
    val digestReady = Digest.initFreeze(signatureReady)
    // <- Order is important.
    val userReady = digestReady.get(SData.Key.initializeFreezeSData).map(_(graph, digestReady)) getOrElse digestReady
    inner.freezeGraph(graph, userReady)
  }
  /** Convert hex string to byte array. */
  def hexStringToByteArray(s: String): Array[Byte] = {
    val len = s.length()
    val data = new Array[Byte](len / 2)
    for (i ← 0 until len by 2)
      data(i / 2) = ((Character.digit(s.charAt(i), 16) << 4) + Character.digit(s.charAt(i + 1), 16)).toByte
    data
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
   * Base64 implementation.
   *
   * This class is based on Mikael Grev code which is published under the BSD license.
   */
  object Base64 {
    val CA = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".toCharArray
    val IA = {
      val array = new Array[Int](256)
      java.util.Arrays.fill(array, -1)
      var iS = CA.length
      for (i ← 0 until iS) {
        array(CA(i)) = i
        array('=') = 0
      }
      array
    }

    /** Decodes a BASE64 encoded byte array.*/
    def decode(sArr: String): Array[Byte] = decode(sArr.toCharArray())
    /**
     * Decodes a BASE64 encoded byte array. All illegal characters will be ignored and can handle both arrays with
     * and without line separators.
     * @param sArr The source array. Length 0 will return an empty array. <code>null</code> will throw an exception.
     * @return The decoded array of bytes. May be of length 0. Will be <code>null</code> if the legal characters
     * (including '=') isn't divideable by 4. (I.e. definitely corrupted).
     */
    def decode(sArr: Array[Char]): Array[Byte] = {
      // Check special case
      val sLen = sArr.length

      // Count illegal characters (including '\r', '\n') to know what size the returned array will be,
      // so we don't have to reallocate & copy it later.
      var sepCnt = 0 // Number of separator characters. (Actually illegal characters, but that's a bonus...)
      for (i ← 0 until sLen) // If input is "pure" (I.e. no line separators or illegal chars) base64 this loop can be commented out.
        if (IA(sArr(i) & 0xff) < 0)
          sepCnt += 1

      // Check so that legal chars (including '=') are evenly divideable by 4 as specified in RFC 2045.
      if ((sLen - sepCnt) % 4 != 0)
        return null

      var pad = 0
      var i = sLen - 1
      while (sArr(i) == '=' && i > 0) {
        pad += 1
        i -= 1
      }

      val len = ((sLen - sepCnt) * 6 >> 3) - pad

      val dArr = new Array[Byte](len) // Preallocate byte[] of exact length

      var s = 0
      var d = 0
      while (d < len) {
        // Assemble three bytes into an int from four "valid" characters.
        var i = 0
        var j = 0
        while (j < 4) { // j only increased if a valid char was found.
          var c = IA(sArr(s))
          if (c >= 0) {
            i |= (c << (18 - j * 6))
            j += 1
          }
          s += 1
        }

        // Add the bytes
        dArr(d) = (i >> 16).toByte
        d += 1
        if (d < len) {
          dArr(d) = (i >> 8).toByte
          d += 1
          if (d < len) {
            dArr(d) = i.toByte
            d += 1
          }
        }
      }

      dArr
    }
    /**
     * Encodes a raw byte array into a BASE64 <code>char[]</code> representation i accordance with RFC 2045.
     * @param sArr The bytes to convert. If <code>null</code> or length 0 an empty array will be returned.
     * @param lineSep Optional "\r\n" after 76 characters, unless end of file. With default value compatible with sun.misc.BASE64Encoder<br>
     * No line separator will be in breach of RFC 2045 which specifies max 76 per line but will be a
     * little faster.
     * @return A BASE64 encoded array. Never <code>null</code>.
     */
    def encode(sArr: Array[Byte], lineSep: String = "\n"): String = {
      // Check special case
      val sLen = if (sArr != null) sArr.length else 0
      if (sLen == 0)
        return ""

      val eLen = (sLen / 3) * 3 // Length of even 24-bits.
      val cCnt = ((sLen - 1) / 3 + 1) << 2 // Returned character count
      val dLen = if (cCnt % 76 == 0)
        cCnt + (cCnt / 76 - 1) * lineSep.length()
      else
        cCnt + cCnt / 76 * lineSep.length()
      val dArr = new Array[Char](dLen)

      // Encode even 24-bits
      var d = 0
      var cc = 0
      var s = 0
      while (s < eLen) {
        // Copy next three bytes into lower 24 bits of int, paying attension to sign.
        val i = (sArr(s) & 0xff) << 16 | (sArr(s + 1) & 0xff) << 8 | (sArr(s + 2) & 0xff)
        s += 3

        // Encode the int into four chars
        dArr(d) = CA((i >>> 18) & 0x3f)
        dArr(d + 1) = CA((i >>> 12) & 0x3f)
        dArr(d + 2) = CA((i >>> 6) & 0x3f)
        dArr(d + 3) = CA(i & 0x3f)
        d += 4

        // Add optional line separator
        if (lineSep.nonEmpty) {
          cc += 1
          if (cc == 19 && d < dLen - lineSep.length()) {
            for (i ← 0 until lineSep.length)
              dArr(d + i) = lineSep.charAt(i)
            d += lineSep.length
            cc = 0
          }
        }
      }

      // Pad and encode last bits if source isn't even 24 bits.
      val left = sLen - eLen // 0 - 2.
      if (left > 0) {
        // Prepare the int
        val i = ((sArr(eLen) & 0xff) << 10) | (if (left == 2) ((sArr(sLen - 1) & 0xff) << 2) else 0)

        // Set last four chars
        dArr(dLen - 4) = CA(i >> 12)
        dArr(dLen - 3) = CA((i >>> 6) & 0x3f)
        dArr(dLen - 2) = if (left == 2) CA(i & 0x3f) else '='
        dArr(dLen - 1) = '='
      }
      String.valueOf(dArr)
    }
  }
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
    /** Add trailing slash to URI if needed. */
    def addTrailingSlash(uri: URI): URI = {
      val path = uri.getPath()
      if (path.length() > 0 && path.charAt(path.length() - 1) != '/')
        new URI(uri.getScheme(), uri.getUserInfo(), uri.getHost(), uri.getPort(), path + "/", uri.getQuery(), uri.getFragment())
      else
        uri
    }
    /** Get graph loader for the specific origin. */
    def acquireGraphLoader(modified: Option[Element.Timestamp], sData: SData): Serialization.Loader
    /** Load graph with the graph loader. */
    def acquireGraph(loader: Serialization.Loader, graphEarlyAccess: Graph[_ <: Model.Like] ⇒ Unit, sData: SData): Graph[_ <: Model.Like]
    /** Convert URI with f(x). */
    def convert(base: URI, uri: URI, f: (String, SData) ⇒ String, sData: SData): URI = {
      val relative = base.relativize(uri)
      base.resolve(new URI(relative.getScheme(), relative.getUserInfo(), relative.getHost(), relative.getPort(),
        relative.getPath().split("/").map(f(_, sData)).mkString("/"),
        relative.getQuery(), relative.getFragment()))
    }
    /** Decode URI with convertURI. */
    def decode(uri: URI, sData: SData): URI = sData.get(SData.Key.convertURI) match {
      case Some((encodeFn, decodeFn)) ⇒ convert(sData(SData.Key.storageURI), uri, decodeFn, sData)
      case None ⇒ uri
    }
    /** Encode URI with convertURI. */
    def encode(uri: URI, sData: SData): URI = sData.get(SData.Key.convertURI) match {
      case Some((encodeFn, decodeFn)) ⇒ convert(sData(SData.Key.storageURI), uri, encodeFn, sData)
      case None ⇒ uri
    }
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
    /** Get modification history. */
    def history = sources.foldLeft(Map.empty[Element.Timestamp, Set[URI]]) {
      case (records, source) ⇒
        source.graphDescriptor.records.foldLeft(records) { (records, record) ⇒
          records.get(record) match {
            case Some(locations) ⇒ records.updated(record, (locations + source.storageURI))
            case None ⇒ records.updated(record, Set(source.storageURI))
          }
        }
    }
    /** Load graph with this graph loader. */
    def load(sources: Seq[Source[_ <: Model.Like, _ <: Element]] = sources.sortBy(-_.weigth), graphEarlyAccess: Graph[_ <: Model.Like] ⇒ Unit = (graph) ⇒ {}): Graph[_ <: Model.Like] =
      Serialization.inner.acquireGraph(this, graphEarlyAccess,
        sData.
          updated(SData.Key.storageURI, sources.head.storageURI).
          updated(SData.Key.sources, sources))

    /** Create model node. */
    protected def createModelNode() = Node.model[Model.Like](sources.head.modelDescriptor.id,
      sources.head.modelDescriptor.unique, sources.head.modelDescriptor.modified)(modelTypeManifest)

    override def toString() = s"Loader(${sources.head.graphDescriptor})"
  }
  /**
   * Serialization storages.
   *
   * @param storages sequence of tuples that is composed from public and real part.
   */
  case class Storages(val seq: Seq[Storages.Entry])
  object Storages {
    implicit def uri2Simple(storage: URI): Simple = Simple(storage)

    /** Append new enties to graph storages. */
    def append(graph: Graph[_ <: Model.Like], simpleStorages: URI*): Storages =
      new Storages((graph.storages ++ simpleStorages).map(Simple))
    /** Create Storages from a single entry. */
    def apply(entry: Storages.Entry): Storages = new Storages(Seq(entry))
    /** Create Storages from multiple entries. */
    def apply(entry: Storages.Entry, seq: Storages.Entry*): Storages = new Storages(entry +: seq)
    /** Ignore exists storages and freeze graph to real storages. */
    def ignore(graph: Graph[_ <: Model.Like], realStorages: URI*): Storages =
      new Storages(graph.storages.map(Public) ++ realStorages.map(Real))

    /** Initialize SData with ExplicitStorages. */
    def init(sData: SData): SData = sData.get(SData.Key.explicitStorages) match {
      case Some(storages) ⇒
        val updated = storages.seq.map {
          case Complex(public, real) ⇒ Complex(inner.addTrailingSlash(public), inner.addTrailingSlash(real))
          case Public(public) ⇒ Public(inner.addTrailingSlash(public))
          case Real(real) ⇒ Real(inner.addTrailingSlash(real))
          case Simple(storage) ⇒ Simple(inner.addTrailingSlash(storage))
        }.distinct
        val publicEntries = updated.flatMap(_.public)
        publicEntries.iterator.scanLeft(Set[URI]())((set, a) ⇒ set + a).zip(publicEntries.iterator).
          foreach { case (set, entry) ⇒ if (set contains entry) throw new IllegalArgumentException("There are duplicated entries with public storage " + entry) }
        val realEntities = updated.flatMap(_.real)
        realEntities.iterator.scanLeft(Set[URI]())((set, a) ⇒ set + a).zip(realEntities.iterator).
          foreach { case (set, entry) ⇒ if (set contains entry) throw new IllegalArgumentException("There are duplicated entries with real storage " + entry) }
        sData.updated(SData.Key.explicitStorages, Storages(updated))
      case None ⇒
        sData
    }

    /** Explicit storage entry. */
    sealed trait Entry {
      /** Public part that is saved to graph meta information and is available for anyone. */
      val public: Option[URI]
      /** Real part that is used to freeze graph. */
      val real: Option[URI]
    }

    /** Complex storage entry. */
    case class Complex(p: URI, r: URI) extends Entry {
      /** Public part that is saved to graph meta information and is available for anyone. */
      val public: Option[URI] = Some(p)
      /** Real part that is used to freeze graph. */
      val real: Option[URI] = Some(r)
    }
    /** Public storage entry. */
    case class Public(p: URI) extends Entry {
      /** Public part that is saved to graph meta information and is available for anyone. */
      val public: Option[URI] = Some(p)
      /** Real part that is used to freeze graph. */
      val real: Option[URI] = None
    }
    /** Real storage entry. */
    case class Real(r: URI) extends Entry {
      /** Public part that is saved to graph meta information and is available for anyone. */
      val public: Option[URI] = None
      /** Real part that is used to freeze graph. */
      val real: Option[URI] = Some(r)
    }
    /** Simple storage entry. */
    case class Simple(s: URI) extends Entry {
      /** Public part that is saved to graph meta information and is available for anyone. */
      val public: Option[URI] = Some(s)
      /** Real part that is used to freeze graph. */
      val real: Option[URI] = Some(s)
    }
  }
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
              bindingModule.injectOptional(key).asInstanceOf[Option[Mechanism]]
            case _ ⇒
              log.debug(s"'${key.name.getOrElse("Unnamed")}' serialization mechanism skipped.")
              None
          }
      }.flatten.toSeq
      assert(mechanisms.distinct.size == mechanisms.size, "serialization mechanisms contain duplicated entities in " + mechanisms)
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
              bindingModule.injectOptional(key).asInstanceOf[Option[Transport]]
            case _ ⇒
              log.debug(s"'${key.name.getOrElse("Unnamed")}' serialization transport skipped.")
              None
          }
      }.flatten.toSeq
      assert(transports.distinct.size == transports.size, "serialization transports contain duplicated entities in " + transports)
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
