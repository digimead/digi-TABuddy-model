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
import java.io.InputStream
import java.net.URI
import java.util.UUID

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Value
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.serialization.Serialization
import org.digimead.tabuddy.model.serialization.YAMLSerialization
import org.digimead.tabuddy.model.serialization.yaml.Timestamp

/**
 * Local transport.
 */
class Local extends Transport with Loggable {
  /** Node directory name template. */
  val nodeNameTemplate = "e %s {%08X}" // hash prevents case insensitivity collision
  /** Descriptor resource template. */
  val descriptorResourceSimple = descriptorResourceName + "." + YAMLSerialization.Identifier.extension
  /** Descriptor resource template. */
  val descriptorResourceNameTemplate = descriptorResourceName + "-%s." + YAMLSerialization.Identifier.extension

  /** Load element with the specific UUID for the specific container. */
  def acquireElementLocation(ancestorsNSelf: Seq[Node[_ <: Element]], elementBox: ElementBox[_ <: Element], storageURI: URI, part: String*): URI = {
    val serializationMechanism = Serialization.perIdentifier.get(elementBox.serialization) match {
      case Some(mechanism) ⇒
        mechanism
      case None ⇒
        throw new IllegalArgumentException(s"Serialization for the specified ${elementBox.serialization} not found.")
    }
    val storageDirectory = new File(storageURI)
    val elementDirectory = getElementDirectory(storageDirectory, ancestorsNSelf, elementBox, false).toURI
    append(elementDirectory, part: _*)
  }
  /** Load element box descriptor with the specific UUID for the specific container. */
  def acquireElementBox(ancestors: Seq[Node[_ <: Element]], elementUniqueId: UUID, modified: Element.Timestamp, storageURI: URI): Array[Byte] = {
    val storageDirectory = new File(storageURI)
    val nodeDirectory = getNodeDirectory(storageDirectory, ancestors, false)
    val elementDirectoryName = "%X-%X-%s".format(elementUniqueId.getMostSignificantBits(),
      elementUniqueId.getLeastSignificantBits(), Timestamp.dump(modified))
    val elementDirectory = new File(nodeDirectory, elementDirectoryName)
    val elementDescriptor = new File(elementDirectory, descriptorResourceSimple).toURI
    log.debug(s"Acquire descriptor from ${elementDescriptor}.")
    read(elementDescriptor)
  }
  /** Load graph from the specific URI. */
  def acquireGraph(origin: Symbol, storageURI: URI): Array[Byte] = {
    if (!storageURI.isAbsolute())
      throw new IllegalArgumentException(s"Storage URI(${storageURI}) must be absolute.")
    val storageDirectory = new File(storageURI)
    val graphDirectory = getGraphDirectory(storageDirectory, origin, false)
    val graphDescriptor = new File(graphDirectory, descriptorResourceSimple).toURI
    log.debug(s"Acquire descriptor from ${graphDescriptor}.")
    read(graphDescriptor)
  }
  /** Load model node descriptor with the specific id. */
  def acquireModel(id: Symbol, origin: Symbol, modified: Element.Timestamp, storageURI: URI): Array[Byte] = {
    val storageDirectory = new File(storageURI)
    val graphDirectory = new File(storageDirectory, origin.name)
    val modelDirectory = new File(graphDirectory, modelDirectoryName)
    val nodeDirectory = new File(modelDirectory, nodeNameTemplate.format(id.name, id.name.hashCode()))
    val nodeDescriptor = new File(nodeDirectory, descriptorResourceNameTemplate.format(Timestamp.dump(modified))).toURI
    log.debug(s"Acquire descriptor from ${nodeDescriptor}.")
    read(nodeDescriptor)
  }
  /** Load node descriptor with the specific id for the specific parent. */
  def acquireNode(ancestors: Seq[Node[_ <: Element]], id: Symbol, modified: Element.Timestamp, storageURI: URI): Array[Byte] = {
    val storageDirectory = new File(storageURI)
    val parentNodeDirectory = getNodeDirectory(storageDirectory, ancestors, true)
    val nodeDirectory = new File(parentNodeDirectory, nodeNameTemplate.format(id.name, id.name.hashCode()))
    val nodeDescriptor = new File(nodeDirectory, descriptorResourceNameTemplate.format(Timestamp.dump(modified))).toURI
    log.debug(s"Acquire descriptor from ${nodeDescriptor}.")
    read(nodeDescriptor)
  }
  /** Delete resource. */
  def delete(uri: URI) = if (new File(uri).delete())
    throw new IOException(s"Unable to delete ${uri}.")
  /** Save element to the specific URI. */
  def freezeElementBox(ancestorsNSelf: Seq[Node[_ <: Element]], elementBox: ElementBox[_ <: Element], storageURI: URI, elementBoxDescriptorContent: Array[Byte]) {
    val storageDirectory = new File(storageURI)
    val elementDirectory = getElementDirectory(storageDirectory, ancestorsNSelf, elementBox, true)
    val elementDescriptorFile = new File(elementDirectory, descriptorResourceSimple).toURI
    log.debug(s"Freeze descriptor to ${elementDescriptorFile}.")
    write(elementBoxDescriptorContent, elementDescriptorFile)
  }
  /** Save graph to the specific URI. */
  def freezeGraph(model: Node[_ <: Model.Like], storageURI: URI, graphDescriptorContent: Array[Byte]) {
    val storageDirectory = new File(storageURI)
    val graphDirectory = getGraphDirectory(storageDirectory, model.graph.origin, true)
    val graphDescriptorFile = new File(graphDirectory, descriptorResourceSimple).toURI
    log.debug(s"Freeze descriptor to ${graphDescriptorFile}.")
    write(graphDescriptorContent, graphDescriptorFile)
  }
  /** Save node to the specific URI. */
  def freezeNode(ancestorsNSelf: Seq[Node[_ <: Element]], storageURI: URI, nodeDescriptorContent: Array[Byte]) {
    val storageDirectory = new File(storageURI)
    val nodeDirectory = getNodeDirectory(storageDirectory, ancestorsNSelf, true)
    val nodeDescriptorFile = new File(nodeDirectory, descriptorResourceNameTemplate.format(Timestamp.dump(ancestorsNSelf.last.modified))).toURI
    log.debug(s"Freeze descriptor to ${nodeDescriptorFile}.")
    write(nodeDescriptorContent, nodeDescriptorFile)
  }
  /** Save custom value to the specific URI. */
  def freezeValue(value: Value[_ <: AnyRef with java.io.Serializable], element: Element, storageURI: URI, elementContent: Array[Byte]) {

  }
  /** Open stream */
  def open(uri: URI): InputStream = new FileInputStream(new File(uri))
  /** Read resource. */
  def read(uri: URI): Array[Byte] = {
    val bis = new BufferedInputStream(new FileInputStream(new File(uri)))
    try { Stream.continually(bis.read).takeWhile(_ != -1).map(_.toByte).toArray }
    finally { try { bis.close() } catch { case e: IOException ⇒ } }
  }
  /** Squeeze model. */
  def squeeze() {}
  /** Write resource. */
  def write(content: InputStream, uri: URI) {
    val contentFile = new File(uri)
    val contentDirectory = contentFile.getParentFile()
    if (!contentDirectory.isDirectory())
      if (!contentDirectory.mkdirs())
        throw new IOException(s"Unable to create ${contentDirectory}.")
    val bis = new BufferedInputStream(content)
    val bos = new BufferedOutputStream(new FileOutputStream(contentFile))
    val buffer = new Array[Byte](4096)
    try { Stream.continually(bis.read(buffer)).takeWhile(_ != -1).foreach(_ ⇒ bos.write(buffer)) }
    finally {
      try { bis.close() } catch { case e: IOException ⇒ }
      try { bos.close() } catch { case e: IOException ⇒ }
    }
  }
  /** Write resource. */
  def write(content: Array[Byte], uri: URI) {
    val contentFile = new File(uri)
    val contentDirectory = contentFile.getParentFile()
    if (!contentDirectory.isDirectory())
      if (!contentDirectory.mkdirs())
        throw new IOException(s"Unable to create ${contentDirectory}.")
    val bos = new BufferedOutputStream(new FileOutputStream(contentFile))
    try { bos.write(content) }
    finally { try { bos.close() } catch { case e: IOException ⇒ } }
  }

  /** Get or create element directory. */
  protected def getElementDirectory(base: File, nodes: Seq[Node[_ <: Element]],
    elementBox: ElementBox[_ <: Element], create: Boolean): File = {
    val elementBoxDirectoryName = "%X-%X-%s".format(elementBox.elementUniqueId.getMostSignificantBits(),
      elementBox.elementUniqueId.getLeastSignificantBits(), Timestamp.dump(elementBox.modified))
    val relativePart = (nodes.map { node ⇒
      nodeNameTemplate.format(node.id.name, node.id.name.hashCode())
    } :+ elementBoxDirectoryName).mkString(File.separator)
    val graphDirectory = new File(base, elementBox.node.graph.origin.name)
    val modelDirectory = new File(graphDirectory, modelDirectoryName)
    val elementDirectory = new File(modelDirectory, relativePart)
    if (!elementDirectory.exists() && create)
      if (!elementDirectory.mkdirs())
        throw new IOException(s"Unable to create ${elementDirectory}.")
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
    if (!modelDirectory.exists() && create)
      if (!modelDirectory.mkdirs())
        throw new IOException(s"Unable to create ${modelDirectory}.")
    graphDirectory
  }
  /** Get or create node directory. */
  protected def getNodeDirectory(base: File, nodes: Seq[Node[_ <: Element]], create: Boolean): File = {
    val relativePart = nodes.map { node ⇒
      nodeNameTemplate.format(node.id.name, node.id.name.hashCode())
    }.mkString(File.separator)
    val graphDirectory = new File(base, nodes.head.graph.origin.name)
    val modelDirectory = new File(graphDirectory, modelDirectoryName)
    val nodeDirectory = new File(modelDirectory, relativePart)
    if (!nodeDirectory.exists() && create)
      if (!nodeDirectory.mkdirs())
        throw new IOException(s"Unable to create ${nodeDirectory}.")
    nodeDirectory
  }
}

