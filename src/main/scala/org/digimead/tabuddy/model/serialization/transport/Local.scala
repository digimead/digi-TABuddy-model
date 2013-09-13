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

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.serialization.Serialization

/**
 * Local transport.
 */
class Local extends Transport with Loggable {
  /** Load element with the specific UUID for the specific container. */
  def acquireElement[A <: Element](elementBox: ElementBox[A], storageURI: URI)(implicit m: Manifest[A]): A = {
    log.debug(s"Acquire element ${elementBox} from ${storageURI}.")
    val serializationMechanism = Serialization.perIdentifier.get(elementBox.serialization) match {
      case Some(mechanism) ⇒
        mechanism
      case None ⇒
        throw new IllegalArgumentException(s"Serialization for the specified ${elementBox.serialization} not found.")
    }
    val storageDirectory = new File(storageURI)
    val nodeDirectory = elementBox.node.safeRead(getNodeDirectory(storageDirectory, _, false))
    val elementDirectoryName = "%X-%X".format(elementBox.elementUniqueId.getMostSignificantBits(), elementBox.elementUniqueId.getLeastSignificantBits())
    val elementDirectory = new File(nodeDirectory, elementDirectoryName)
    val elementFile = new File(elementDirectory, "element." + elementBox.serialization.extension)
    val elementBinary = read(elementFile.toURI)
    serializationMechanism.load(elementBox, elementBinary)
  }
  /** Load element box descriptor with the specific UUID for the specific container. */
  def acquireElementBox(objectId: UUID, node: Node.ThreadUnsafe, storageURI: URI): Serialization.Descriptor.Element[_ <: Element] = {
    log.debug(s"Acquire element box ${objectId} descriptor from ${storageURI}.")
    val storageDirectory = new File(storageURI)
    val nodeDirectory = getNodeDirectory(storageDirectory, node, false)
    val elementDirectoryName = "%X-%X".format(objectId.getMostSignificantBits(), objectId.getLeastSignificantBits())
    val elementDirectory = new File(nodeDirectory, elementDirectoryName)
    val elementDescriptorFile = new File(elementDirectory, descriptorResourceName)
    Serialization.inner.elementDescriptorFromYaml(new String(read(elementDescriptorFile.toURI), "UTF-8"))
  }
  /** Load graph from the specific URI. */
  def acquireGraph(origin: Symbol, storageURI: URI): Serialization.Descriptor.Graph[_ <: Model.Like] = {
    log.debug(s"Acquire graph ${origin} descriptor from ${storageURI}.")
    if (!storageURI.isAbsolute())
      throw new IllegalArgumentException(s"Storage URI(${storageURI}) must be absolute.")
    val storageDirectory = new File(storageURI)
    val graphDirectory = getGraphDirectory(storageDirectory, origin, false)
    val graphDescriptorFile = new File(graphDirectory, descriptorResourceName)
    Serialization.inner.graphDescriptorFromYaml(new String(read(graphDescriptorFile.toURI), "UTF-8"))
  }
  /** Load model node descriptor with the specific id. */
  def acquireModel(id: Symbol, origin: Symbol, storageURI: URI): Serialization.Descriptor.Node = {
    log.debug(s"Acquire node ${id} descriptor from ${storageURI}.")
    val storageDirectory = new File(storageURI)
    val graphDirectory = new File(storageDirectory, origin.name)
    val modelDirectory = new File(graphDirectory, modelDirectoryName)
    val nodeDirectory = new File(modelDirectory, id.name)
    val nodeDescriptorFile = new File(nodeDirectory, descriptorResourceName)
    Serialization.inner.nodeDescriptorFromYaml(new String(read(nodeDescriptorFile.toURI), "UTF-8"))
  }
  /** Load node descriptor with the specific id for the specific parent. */
  def acquireNode(id: Symbol, parentNode: Node.ThreadUnsafe, storageURI: URI): Serialization.Descriptor.Node = {
    log.debug(s"Acquire node ${id} descriptor from ${storageURI}.")
    val storageDirectory = new File(storageURI)
    val parentNodeDirectory = getNodeDirectory(storageDirectory, parentNode, true)
    val nodeDirectory = new File(parentNodeDirectory, id.name)
    val nodeDescriptorFile = new File(nodeDirectory, descriptorResourceName)
    Serialization.inner.nodeDescriptorFromYaml(new String(read(nodeDescriptorFile.toURI), "UTF-8"))
  }
  /** Delete resource. */
  def delete(uri: URI) = if (new File(uri).delete())
    throw new IOException(s"Unable to delete ${uri}.")
  /** Save element to the specific URI. */
  def freezeElementBox(elementBox: ElementBox[_ <: Element], storageURI: URI) {
    log.debug(s"Freeze ${elementBox}.")
    val storageDirectory = new File(storageURI)
    val elementDirectory = getElementDirectory(storageDirectory, elementBox, true)
    val elementDescriptorFile = new File(elementDirectory, descriptorResourceName)
    val elementDirectoryURI = elementDirectory.toURI
    printToFile(elementDescriptorFile)(_.println(Serialization.inner.elementDescriptorToYAML(elementBox)))
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
    val graphDescriptorFile = new File(graphDirectory, descriptorResourceName)
    printToFile(graphDescriptorFile)(_.println(Serialization.inner.graphDescriptorToYAML(graph)))
    graph.node.safeRead { node ⇒ freezeNode(node, storageURI) }
  }
  /** Save node to the specific URI. */
  def freezeNode(node: Node.ThreadUnsafe, storageURI: URI, recursive: Boolean = true) {
    log.debug(s"Freeze ${node}.")
    node.freezeRead { node ⇒
      val storageDirectory = new File(storageURI)
      val nodeDirectory = getNodeDirectory(storageDirectory, node, true)
      val nodeDescriptorFile = new File(nodeDirectory, descriptorResourceName)
      printToFile(nodeDescriptorFile)(_.println(Serialization.inner.nodeDescriptorToYAML(node)))
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

