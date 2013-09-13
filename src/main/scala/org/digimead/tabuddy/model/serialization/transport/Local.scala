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
import org.digimead.tabuddy.model.element.Value
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
    val element = new File(elementDirectory, "element." + elementBox.serialization.extension).toURI
    log.debug(s"Acquire ${elementBox} from ${element}.")
    val elementBinary = read(element)
    serializationMechanism.load(elementBox, elementBinary)
  }
  /** Load element box descriptor with the specific UUID for the specific container. */
  def acquireElementBox(objectId: UUID, node: Node.ThreadUnsafe, storageURI: URI): Array[Byte] = {
    val storageDirectory = new File(storageURI)
    val nodeDirectory = getNodeDirectory(storageDirectory, node, false)
    val elementDirectoryName = "%X-%X".format(objectId.getMostSignificantBits(), objectId.getLeastSignificantBits())
    val elementDirectory = new File(nodeDirectory, elementDirectoryName)
    val elementDescriptor = new File(elementDirectory, descriptorResourceName).toURI
    log.debug(s"Acquire descriptor from ${elementDescriptor}.")
    read(elementDescriptor)
  }
  /** Load graph from the specific URI. */
  def acquireGraph(origin: Symbol, storageURI: URI): Array[Byte] = {
    if (!storageURI.isAbsolute())
      throw new IllegalArgumentException(s"Storage URI(${storageURI}) must be absolute.")
    val storageDirectory = new File(storageURI)
    val graphDirectory = getGraphDirectory(storageDirectory, origin, false)
    val graphDescriptor = new File(graphDirectory, descriptorResourceName).toURI
    log.debug(s"Acquire descriptor from ${graphDescriptor}.")
    read(graphDescriptor)
  }
  /** Load model node descriptor with the specific id. */
  def acquireModel(id: Symbol, origin: Symbol, storageURI: URI): Array[Byte] = {
    val storageDirectory = new File(storageURI)
    val graphDirectory = new File(storageDirectory, origin.name)
    val modelDirectory = new File(graphDirectory, modelDirectoryName)
    val nodeDirectory = new File(modelDirectory, id.name)
    val nodeDescriptor = new File(nodeDirectory, descriptorResourceName).toURI
    log.debug(s"Acquire descriptor from ${nodeDescriptor}.")
    read(nodeDescriptor)
  }
  /** Load node descriptor with the specific id for the specific parent. */
  def acquireNode(id: Symbol, parentNode: Node.ThreadUnsafe, storageURI: URI): Array[Byte] = {
    val storageDirectory = new File(storageURI)
    val parentNodeDirectory = getNodeDirectory(storageDirectory, parentNode, true)
    val nodeDirectory = new File(parentNodeDirectory, id.name)
    val nodeDescriptor = new File(nodeDirectory, descriptorResourceName).toURI
    log.debug(s"Acquire descriptor from ${nodeDescriptor}.")
    read(nodeDescriptor)
  }
  /** Delete resource. */
  def delete(uri: URI) = if (new File(uri).delete())
    throw new IOException(s"Unable to delete ${uri}.")
  /** Save element to the specific URI. */
  def freezeElement(element: Element, storageURI: URI, elementContent: Array[Byte]) {
    val storageDirectory = new File(storageURI)
    val elementDirectory = getElementDirectory(storageDirectory, element.eBox, true)
    val elementURI = elementDirectory.toURI.resolve(elementResourceName + "." + element.eBox.serialization.extension)
    log.debug(s"Freeze ${element} to ${elementURI}.")
    write(elementContent, elementURI)
  }
  /** Save element to the specific URI. */
  def freezeElementBox(elementBox: ElementBox[_ <: Element], storageURI: URI, elementBoxDescriptorContent: Array[Byte]) {
    val storageDirectory = new File(storageURI)
    val elementDirectory = getElementDirectory(storageDirectory, elementBox, true)
    val elementDescriptorFile = new File(elementDirectory, descriptorResourceName).toURI
    log.debug(s"Freeze descriptor to ${elementDescriptorFile}.")
    write(elementBoxDescriptorContent, elementDescriptorFile)
  }
  /** Save graph to the specific URI. */
  def freezeGraph(graph: Graph[_ <: Model.Like], storageURI: URI, graphDescriptorContent: Array[Byte]) {
    val storageDirectory = new File(storageURI)
    val graphDirectory = getGraphDirectory(storageDirectory, graph.origin, true)
    val graphDescriptorFile = new File(graphDirectory, descriptorResourceName).toURI
    log.debug(s"Freeze descriptor to ${graphDescriptorFile}.")
    write(graphDescriptorContent, graphDescriptorFile)
  }
  /** Save node to the specific URI. */
  def freezeNode(node: Node.ThreadUnsafe, storageURI: URI, nodeDescriptorContent: Array[Byte]) {
    val storageDirectory = new File(storageURI)
    val nodeDirectory = getNodeDirectory(storageDirectory, node, true)
    val nodeDescriptorFile = new File(nodeDirectory, descriptorResourceName).toURI
    log.debug(s"Freeze descriptor to ${nodeDescriptorFile}.")
    write(nodeDescriptorContent, nodeDescriptorFile)
  }
  /** Save custom value to the specific URI. */
  def freezeValue(value: Value[_ <: AnyRef with java.io.Serializable], element: Element, storageURI: URI, elementContent: Array[Byte]) {

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
}

