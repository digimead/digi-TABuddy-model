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

package org.digimead.tabuddy.model.serialization.transport

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.IOException
import java.net.URI
import java.util.UUID
import scala.collection.JavaConverters._
import scala.collection.immutable
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.serialization.Serialization
import org.digimead.tabuddy.model.serialization.Serialization.interface2implementation
import org.digimead.tabuddy.model.element.Coordinate

/**
 * Local transport.
 */
class Local extends Serialization.Transport with Loggable {
  /** Load element box with the specific UUID for the specific container. */
  def acquireElementBox(objectId: UUID, node: Node.ThreadUnsafe, storageURI: URI): ElementBox[_ <: Element] = {
    log.debug(s"Acquire element box ${objectId} from ${storageURI}.")
    val storageDirectory = new File(storageURI)
    val nodeDirectory = getNodeDirectory(storageDirectory, node, false)
    val elementDirectoryName = "%X-%X".format(objectId.getMostSignificantBits(), objectId.getLeastSignificantBits())
    val elementDirectory = new File(nodeDirectory, elementDirectoryName)
    val elementDescriptionFile = new File(elementDirectory, descriptionResourceName)
    val elementDescription = Serialization.elementDescriptionFromYaml(new String(read(elementDescriptionFile.toURI), "UTF-8"))
    val elementBox = ElementBox[Element](elementDescription.coordinate, elementDescription.elementUniqueId, node, elementDescription.modified,
      elementDescription.serializationIdentifier)(Manifest.classType(elementDescription.clazz))
    elementBox
  }
  /** Load graph from the specific URI. */
  def acquireGraph(origin: Symbol, storageURI: URI): Graph[_ <: Model.Like] = {
    log.debug(s"Acquire graph ${origin} from ${storageURI}.")
    if (!storageURI.isAbsolute())
      throw new IllegalArgumentException(s"Storage URI(${storageURI}) must be absolute.")
    val storageDirectory = new File(storageURI)
    val graphDirectory = getGraphDirectory(storageDirectory, origin, false)
    /*
     * Load graph description.
     */
    val graphDescriptionFile = new File(graphDirectory, descriptionResourceName)
    val graphDescription = {
      val description = Serialization.graphDescriptionFromYaml(new String(read(graphDescriptionFile.toURI), "UTF-8"))
      if (!description.storages.contains(storageURI)) description.copy(storages = storageURI +: description.storages) else description
    }
    if (graphDescription.origin == null)
      throw new IllegalStateException("Origin value not found in graph description file.")
    if (graphDescription.origin.name != origin.name)
      throw new IllegalStateException(s"Incorrect saved origin value ${graphDescription.origin.name} vs required ${origin.name}.")
    /*
     * Load model description.
     */
    val modelDirectory = new File(graphDirectory, modelDirectoryName)
    val nodeDirectory = new File(modelDirectory, graphDescription.modelId.name)
    val nodeDescriptionFile = new File(nodeDirectory, descriptionResourceName)
    val nodeDescription = Serialization.nodeDescriptionFromYaml(new String(read(nodeDescriptionFile.toURI), "UTF-8"))
    if (nodeDescription.elements.isEmpty)
      throw new IllegalStateException("There are no elements in the model node.")
    if (nodeDescription.id == null)
      throw new IllegalStateException("Id value not found in model node description file.")
    if (nodeDescription.unique == null)
      throw new IllegalStateException("Unique value not found in model node description file.")
    if (nodeDescription.id.name != graphDescription.modelId.name)
      throw new IllegalStateException(s"Incorrect saved model id value ${nodeDescription.id.name} vs required ${graphDescription.modelId.name}.")
    /*
     * Create graph and model node
     */
    val targetModelNode = Node.model(nodeDescription.id, nodeDescription.unique)
    val graph = new Graph(graphDescription.created, targetModelNode, origin)(Manifest.classType(graphDescription.modelType))
    graph.storages = graphDescription.storages
    targetModelNode.safeWrite { targetNode ⇒
      targetModelNode.initializeModelNode(graph, graphDescription.modified)
      val elementBoxes = nodeDescription.elements.map { elementId ⇒
        Serialization.acquireElementBox(elementId, targetNode)
      }
      val (rootElementsPart, projectionElementsPart) = elementBoxes.partition(_.coordinate == Coordinate.root)
      if (rootElementsPart.isEmpty)
        throw new IllegalStateException("Root element not found.")
      if (rootElementsPart.size > 1)
        throw new IllegalStateException("There are few root elements.")
      val rootElementBox = rootElementsPart.head
      val projectionElementBoxes: Seq[(Coordinate, ElementBox[_ <: Element])] = projectionElementsPart.map(e ⇒ e.coordinate -> e)
      targetNode.updateState(rootElementBox = rootElementBox, projectionElementBoxes = immutable.HashMap(projectionElementBoxes: _*))
      if (graph.modelType != graph.node.getRootElementBox.elementType)
        throw new IllegalArgumentException(s"Unexpected model type ${graph.modelType} vs ${graph.node.getRootElementBox.elementType}")
    }
    graph
  }
  /** Load node with the specific UUID for the specific parent. */
  def acquireNode(uniqueId: UUID, parentNode: Node): Node = {
    null
  }
  /** Delete resource. */
  def delete(uri: URI) = if (new File(uri).delete())
    throw new IOException(s"Unable to delete ${uri}.")
  /** Save element to the specific URI. */
  def freezeElementBox(elementBox: ElementBox[_ <: Element], storageURI: URI) {
    log.debug(s"Freeze ${elementBox}.")
    val storageDirectory = new File(storageURI)
    val elementDirectory = getElementDirectory(storageDirectory, elementBox, true)
    val elementDescriptionFile = new File(elementDirectory, descriptionResourceName)
    val elementDirectoryURI = elementDirectory.toURI
    printToFile(elementDescriptionFile)(_.println(Serialization.elementDescriptionToYAML(elementBox)))
    // save element
    elementBox.getModified match {
      case Some(modified) ⇒
        log.debug(s"Freeze element to ${elementDirectoryURI}.")
        if (modified ne elementBox.get)
          throw new IllegalStateException("Element and modified element are different.")
        write(elementBox.save(), elementDirectoryURI.resolve(elementResourceName + "." + elementBox.serialization.extension))
        elementBox.get.eStash.property.foreach {
          case (valueId, perTypeMap) ⇒
            perTypeMap.foreach {
              case (typeSymbolId, value) ⇒
                value.commit(elementBox.get, this, elementDirectory.toURI())
            }
        }
      case None ⇒
        log.debug("Skip unmodified element.")
    }
  }
  /** Save graph to the specific URI. */
  def freezeGraph(graph: Graph[_ <: Model.Like], storageURI: URI) {
    log.debug(s"Freeze ${graph}.")
    val storageDirectory = new File(storageURI)
    val graphDirectory = getGraphDirectory(storageDirectory, graph.origin, true)
    val graphDescriptionFile = new File(graphDirectory, descriptionResourceName)
    printToFile(graphDescriptionFile)(_.println(Serialization.graphDescriptionToYAML(graph)))
    graph.node.safeRead { node ⇒ freezeNode(node, storageURI) }
  }
  /** Save node to the specific URI. */
  def freezeNode(node: Node.ThreadUnsafe, storageURI: URI, recursive: Boolean = true) {
    log.debug(s"Freeze ${node}.")
    node.freezeRead { node ⇒
      val storageDirectory = new File(storageURI)
      val nodeDirectory = getNodeDirectory(storageDirectory, node, true)
      val nodeDescriptionFile = new File(nodeDirectory, descriptionResourceName)
      printToFile(nodeDescriptionFile)(_.println(Serialization.nodeDescriptionToYAML(node)))
      freezeElementBox(node.rootElementBox, storageURI)
      node.children.foreach { node ⇒ node.safeRead(freezeNode(_, storageURI)) }
    }
  }
  /** Read resource. */
  def read(uri: URI): Array[Byte] = {
    val bis = new BufferedInputStream(new FileInputStream(new File(uri)))
    try { Stream.continually(bis.read).takeWhile(_ != -1).map(_.toByte).toArray }
    finally { try { bis.close() } catch { case e: IOException ⇒ } }
  }
  /** Squeeze model. */
  def squeeze() {}
  /** Write resource. */
  def write(content: Array[Byte], uri: URI) {
    val bos = new BufferedOutputStream(new FileOutputStream(new File(uri)))
    try { bos.write(content) }
    finally { try { bos.close() } catch { case e: IOException ⇒ } }
  }

  /** Get or create element directory. */
  protected def getElementDirectory(base: File, elementBox: ElementBox[_ <: Element], create: Boolean): File = {
    val elementBoxDirectoryName = "%X-%X".format(elementBox.elementUniqueId.getMostSignificantBits(), elementBox.elementUniqueId.getLeastSignificantBits())
    val relativePart = ((elementBox.node +: elementBox.node.safeRead(_.ancestors)).
      reverse.map(_.id.name) :+ elementBoxDirectoryName).mkString(File.separator)
    val graphDirectory = new File(base, elementBox.node.graph.origin.name)
    val modelDirectory = new File(graphDirectory, modelDirectoryName)
    val elementDirectory = new File(modelDirectory, relativePart)
    if (!elementDirectory.exists())
      if (create) {
        if (!elementDirectory.mkdirs())
          throw new IOException(s"Unable to create ${elementDirectory}.")
      } else {
        throw new IOException(s"Directory ${elementDirectory} not exists.")
      }
    elementDirectory
  }
  /** Get or create graph directory. */
  protected def getGraphDirectory(base: File, origin: Symbol, create: Boolean): File = {
    val graphDirectory = new File(base, origin.name)
    if (!graphDirectory.exists())
      if (create) {
        if (!graphDirectory.mkdirs())
          throw new IOException(s"Unable to create ${graphDirectory}.")
      } else {
        throw new IOException(s"Directory ${graphDirectory} not exists.")
      }
    val modelDirectory = new File(base, modelDirectoryName)
    if (!modelDirectory.exists())
      if (create) {
        if (!modelDirectory.mkdirs())
          throw new IOException(s"Unable to create ${modelDirectory}.")
      } else {
        throw new IOException(s"Directory ${modelDirectory} not exists.")
      }
    graphDirectory
  }
  /** Get or create node directory. */
  protected def getNodeDirectory(base: File, node: Node.ThreadUnsafe, create: Boolean): File = {
    val relativePart = (node +: node.ancestors).reverse.map(_.id.name).mkString(File.separator)
    val graphDirectory = new File(base, node.graph.origin.name)
    val modelDirectory = new File(graphDirectory, modelDirectoryName)
    val nodeDirectory = new File(modelDirectory, relativePart)
    if (!nodeDirectory.exists())
      if (create) {
        if (!nodeDirectory.mkdirs())
          throw new IOException(s"Unable to create ${nodeDirectory}.")
      } else {
        throw new IOException(s"Directory ${nodeDirectory} not exists.")
      }
    nodeDirectory
  }
  /** Print to file. */
  protected def printToFile(f: java.io.File)(op: java.io.PrintWriter ⇒ Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}

