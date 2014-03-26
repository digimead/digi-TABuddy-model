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

package org.digimead.tabuddy.model.serialization.transport

import java.io.{ BufferedInputStream, BufferedOutputStream, File, FileInputStream, FileOutputStream, IOException, InputStream }
import java.net.URI
import java.util.UUID
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.{ ElementBox, Node }
import org.digimead.tabuddy.model.serialization.yaml.Timestamp
import org.digimead.tabuddy.model.serialization.{ Serialization, SData, YAMLSerialization }

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
  /** Transport scheme. */
  val scheme: String = "file"

  /** Load element with the specific UUID for the specific container. */
  def acquireElementLocation(ancestorsNSelf: Seq[Node[_ <: Element]], elementBox: ElementBox[_ <: Element], sData: SData, part: String*): URI = {
    val serializationMechanism = Serialization.perIdentifier.get(elementBox.serialization) match {
      case Some(mechanism) ⇒
        mechanism
      case None ⇒
        throw new IllegalArgumentException(s"Serialization for the specified ${elementBox.serialization} not found.")
    }
    val storageDirectory = new File(sData(SData.Key.storageURI))
    val elementDirectory = getElementDirectory(storageDirectory, ancestorsNSelf, elementBox, false).toURI
    append(elementDirectory, part: _*)
  }
  /** Load element box descriptor with the specific UUID for the specific container. */
  def acquireElementBox(ancestors: Seq[Node[_ <: Element]], elementUniqueId: UUID, modified: Element.Timestamp, sData: SData): Array[Byte] = {
    val storageDirectory = new File(sData(SData.Key.storageURI))
    val nodeDirectory = getNodeDirectory(storageDirectory, ancestors, false)
    val elementDirectoryName = "%s %X-%X-%s".format(boxPrefix, elementUniqueId.getMostSignificantBits(),
      elementUniqueId.getLeastSignificantBits(), Timestamp.dump(modified))
    val elementDirectory = new File(nodeDirectory, elementDirectoryName)
    val elementDescriptor = new File(elementDirectory, "%s %s".format(boxPrefix, descriptorResourceSimple)).toURI
    log.debug(s"Acquire descriptor from ${elementDescriptor}.")
    read(elementDescriptor, sData)
  }
  /** Load graph from the specific URI. */
  def acquireGraph(origin: Symbol, sData: SData): Array[Byte] = {
    val storageURI = sData(SData.Key.storageURI)
    if (!storageURI.isAbsolute())
      throw new IllegalArgumentException(s"Storage URI(${storageURI}) must be absolute.")
    val storageDirectory = new File(storageURI)
    val graphDescriptor = new File(storageDirectory, descriptorResourceSimple).toURI
    log.debug(s"Acquire descriptor from ${graphDescriptor}.")
    read(graphDescriptor, sData)
  }
  /** Load model node descriptor with the specific id. */
  def acquireModel(id: Symbol, origin: Symbol, modified: Element.Timestamp, sData: SData): Array[Byte] = {
    val storageDirectory = new File(sData(SData.Key.storageURI))
    val graphDirectory = new File(storageDirectory, dataDirectoryName)
    val nodeDirectory = new File(graphDirectory, nodeNameTemplate.format(id.name, id.name.hashCode()))
    val nodeDescriptor = new File(nodeDirectory, "%s %s".format(nodePrefix,
      descriptorResourceNameTemplate.format(Timestamp.dump(modified)))).toURI
    log.debug(s"Acquire descriptor from ${nodeDescriptor}.")
    read(nodeDescriptor, sData)
  }
  /** Load node descriptor with the specific id for the specific parent. */
  def acquireNode(ancestors: Seq[Node[_ <: Element]], id: Symbol, modified: Element.Timestamp, sData: SData): Array[Byte] = {
    val storageDirectory = new File(sData(SData.Key.storageURI))
    val parentNodeDirectory = getNodeDirectory(storageDirectory, ancestors, true)
    val nodeDirectory = new File(parentNodeDirectory, nodeNameTemplate.format(id.name, id.name.hashCode()))
    val nodeDescriptor = new File(nodeDirectory, "%s %s".format(nodePrefix,
      descriptorResourceNameTemplate.format(Timestamp.dump(modified)))).toURI
    log.debug(s"Acquire descriptor from ${nodeDescriptor}.")
    read(nodeDescriptor, sData)
  }
  /** Delete resource. */
  def delete(uri: URI, sData: SData) = if (new File(uri).delete())
    throw new IOException(s"Unable to delete ${uri}.")
  /** Check resource. */
  def exists(uri: URI, sData: SData) = new File(uri).canRead()
  /** Save element to the specific URI. */
  def freezeElementBox(ancestorsNSelf: Seq[Node[_ <: Element]], elementBox: ElementBox[_ <: Element], elementBoxDescriptorContent: Array[Byte], sData: SData) {
    val storageDirectory = new File(sData(SData.Key.storageURI))
    val elementDirectory = getElementDirectory(storageDirectory, ancestorsNSelf, elementBox, true)
    val elementDescriptorFile = new File(elementDirectory, "%s %s".format(boxPrefix, descriptorResourceSimple)).toURI
    log.debug(s"Freeze descriptor to ${elementDescriptorFile}.")
    write(elementDescriptorFile, elementBoxDescriptorContent, sData)
  }
  /** Save graph to the specific URI. */
  def freezeGraph(model: Node[_ <: Model.Like], graphDescriptorContent: Array[Byte], sData: SData) {
    val storageDirectory = new File(sData(SData.Key.storageURI))
    val graphDescriptorFile = new File(storageDirectory, descriptorResourceSimple).toURI
    log.debug(s"Freeze descriptor to ${graphDescriptorFile}.")
    write(graphDescriptorFile, graphDescriptorContent, sData)
  }
  /** Save node to the specific URI. */
  def freezeNode(ancestorsNSelf: Seq[Node[_ <: Element]], nodeDescriptorContent: Array[Byte], sData: SData) {
    val storageDirectory = new File(sData(SData.Key.storageURI))
    val nodeDirectory = getNodeDirectory(storageDirectory, ancestorsNSelf, true)
    val nodeDescriptorFile = new File(nodeDirectory, "%s %s".format(nodePrefix,
      descriptorResourceNameTemplate.format(Timestamp.dump(ancestorsNSelf.last.modified)))).toURI
    log.debug(s"Freeze descriptor to ${nodeDescriptorFile}.")
    write(nodeDescriptorFile, nodeDescriptorContent, sData)
  }
  /** Open stream */
  def open(uri: URI, sData: SData): InputStream = new FileInputStream(new File(uri))
  /** Read resource. */
  def read(uri: URI, sData: SData): Array[Byte] = {
    val bis = new BufferedInputStream(new FileInputStream(new File(uri)))
    try { Stream.continually(bis.read).takeWhile(_ != -1).map(_.toByte).toArray }
    finally { try { bis.close() } catch { case e: IOException ⇒ } }
  }
  /** Write resource. */
  def write(uri: URI, content: InputStream, sData: SData) {
    val contentFile = new File(uri)
    val contentDirectory = contentFile.getParentFile()
    if (!contentDirectory.isDirectory())
      if (!contentDirectory.mkdirs())
        throw new IOException(s"Unable to create ${contentDirectory}.")
    val bis = new BufferedInputStream(content)
    val bos = new BufferedOutputStream(new FileOutputStream(contentFile))
    val buffer = new Array[Byte](4096)
    try { Stream.continually(bis.read(buffer)).takeWhile(_ != -1).foreach(i ⇒ bos.write(buffer, 0, i)) }
    finally {
      try { bis.close() } catch { case e: IOException ⇒ }
      try { bos.close() } catch { case e: IOException ⇒ }
    }
  }
  /** Write resource. */
  def write(uri: URI, content: Array[Byte], sData: SData) {
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
    val elementBoxDirectoryName = "%s %X-%X-%s".format(boxPrefix, elementBox.elementUniqueId.getMostSignificantBits(),
      elementBox.elementUniqueId.getLeastSignificantBits(), Timestamp.dump(elementBox.modified))
    val relativePart = (nodes.map { node ⇒
      nodeNameTemplate.format(node.id.name, node.id.name.hashCode())
    } :+ elementBoxDirectoryName).mkString(File.separator)
    val graphDirectory = new File(base, dataDirectoryName)
    val elementDirectory = new File(graphDirectory, relativePart)
    if (!elementDirectory.exists() && create)
      if (!elementDirectory.mkdirs())
        throw new IOException(s"Unable to create ${elementDirectory}.")
    elementDirectory
  }
  /** Get or create node directory. */
  protected def getNodeDirectory(base: File, nodes: Seq[Node[_ <: Element]], create: Boolean): File = {
    val relativePart = nodes.map { node ⇒
      nodeNameTemplate.format(node.id.name, node.id.name.hashCode())
    }.mkString(File.separator)
    val graphDirectory = new File(base, dataDirectoryName)
    val nodeDirectory = new File(graphDirectory, relativePart)
    if (!nodeDirectory.exists() && create)
      if (!nodeDirectory.mkdirs())
        throw new IOException(s"Unable to create ${nodeDirectory}.")
    nodeDirectory
  }
}

