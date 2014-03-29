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

  /** Get element box URI. */
  def getElementBoxURI(ancestors: Seq[Node[_ <: Element]], elementUniqueId: UUID, elementModified: Element.Timestamp, sData: SData): URI = {
    val storageDirectory = new File(sData(SData.Key.storageURI))
    val elementDirectory = getElementDirectory(storageDirectory, ancestors, elementUniqueId, elementModified, true)
    new File(elementDirectory, "%s %s".format(boxPrefix, descriptorResourceSimple)).toURI
  }
  /** Get graph URI. */
  def getGraphURI(origin: Symbol, sData: SData): URI = {
    val storageDirectory = new File(sData(SData.Key.storageURI))
    new File(storageDirectory, descriptorResourceSimple).toURI
  }
  /** Get node URI. */
  def getNodeURI(ancestors: Seq[Node[_ <: Element]], nodeId: Symbol, nodeModified: Element.Timestamp, sData: SData): URI = {
    val storageDirectory = new File(sData(SData.Key.storageURI))
    val nodeDirectory = getNodeDirectory(storageDirectory, ancestors, nodeId, true)
    new File(nodeDirectory, "%s %s".format(nodePrefix,
      descriptorResourceNameTemplate.format(Timestamp.dump(nodeModified)))).toURI
  }
  /** Get sub element URI. */
  def getSubElementURI(ancestors: Seq[Node[_ <: Element]], elementUniqueId: UUID, elementModified: Element.Timestamp, sData: SData, part: String*): URI = {
    val storageDirectory = new File(sData(SData.Key.storageURI))
    val elementDirectory = getElementDirectory(storageDirectory, ancestors, elementUniqueId, elementModified, false).toURI
    append(elementDirectory, part: _*)
  }
  /** Delete resource. */
  def delete(uri: URI, sData: SData) = {
    log.debug("Delete " + uri)
    if (new File(uri).delete())
      throw new IOException(s"Unable to delete ${uri}.")
  }
  /** Check resource. */
  def exists(uri: URI, sData: SData) = {
    log.debug("Check " + uri)
    new File(uri).canRead()
  }
  /** Open stream */
  def open(uri: URI, sData: SData): InputStream = {
    log.debug("Open " + uri)
    new FileInputStream(new File(uri))
  }
  /** Read resource. */
  def read(uri: URI, sData: SData): Array[Byte] = {
    log.debug("Read " + uri)
    val bis = new BufferedInputStream(new FileInputStream(new File(uri)))
    val array = try { Stream.continually(bis.read).takeWhile(_ != -1).map(_.toByte).toArray }
    finally { try { bis.close() } catch { case e: IOException ⇒ } }
    sData.get(SData.Key.onRead).map(_(uri, array, sData))
    array
  }
  /** Write resource. */
  def write(uri: URI, content: Array[Byte], sData: SData) {
    log.debug("Write " + uri)
    val contentFile = new File(uri)
    val contentDirectory = contentFile.getParentFile()
    if (!contentDirectory.isDirectory())
      if (!contentDirectory.mkdirs())
        throw new IOException(s"Unable to create ${contentDirectory}.")
    val bos = new BufferedOutputStream(new FileOutputStream(contentFile))
    try { bos.write(content) }
    finally { try { bos.close() } catch { case e: IOException ⇒ } }
    sData.get(SData.Key.onWrite).map(_(uri, content, sData))
  }

  /** Get or create element directory. */
  protected def getElementDirectory(base: File, nodes: Seq[Node[_ <: Element]],
    elementUniqueId: UUID, elementModified: Element.Timestamp, create: Boolean): File = {
    val elementBoxDirectoryName = "%s %X-%X-%s".format(boxPrefix, elementUniqueId.getMostSignificantBits(),
      elementUniqueId.getLeastSignificantBits(), Timestamp.dump(elementModified))
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
  protected def getNodeDirectory(base: File, nodes: Seq[Node[_ <: Element]], nodeId: Symbol, create: Boolean): File = {
    val relativePart = (nodes.map(_.id.name) :+ nodeId.name).
      map(name ⇒ nodeNameTemplate.format(name, name.hashCode())).mkString(File.separator)
    val graphDirectory = new File(base, dataDirectoryName)
    val nodeDirectory = new File(graphDirectory, relativePart)
    if (!nodeDirectory.exists() && create)
      if (!nodeDirectory.mkdirs())
        throw new IOException(s"Unable to create ${nodeDirectory}.")
    nodeDirectory
  }
}

