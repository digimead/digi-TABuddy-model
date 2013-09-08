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

import java.io.File
import java.io.IOException
import java.util.UUID
import scala.collection.JavaConverters._
import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Node
import org.yaml.snakeyaml.DumperOptions
import org.yaml.snakeyaml.Yaml
import scala.language.implicitConversions
import java.io.ByteArrayInputStream
import org.apache.pdfbox.io.ASCII85InputStream
import scala.io.Codec
import java.io.ByteArrayOutputStream
import org.apache.pdfbox.io.ASCII85OutputStream

trait Serialization[A] extends Loggable {
  val serializationType: Manifest[A]

  /** Load element. */
  def acquireElement[B <: Element](objectId: UUID, parentNode: Node)(implicit m: Manifest[B]): Option[B]
  /** Save element. */
  def freezeElement(element: Element)
}

object Serialization {
  implicit def interface2implementation(m: Serialization.type): Interface = m.inner

  /** Decode ISO8859 string from Ascii85. Implementation based on Apache PDFBox. */
  def decode85(from: String) = {
    val list = new java.util.ArrayList[Byte]()
    val in_byte: ByteArrayInputStream = new ByteArrayInputStream(from.getBytes(Codec.ISO8859.charSet))
    val in_ascii = new ASCII85InputStream(in_byte)
    scala.io.Source.fromInputStream(in_ascii)(Codec.ISO8859).getLines().mkString("")
  }
  /** Encode ISO8859 string to Ascii85. Implementation based on Apache PDFBox. */
  def encode85(from: String): String = {
    val out_byte = new ByteArrayOutputStream()
    val out_ascii = new ASCII85OutputStream(out_byte)
    out_ascii.write(from.getBytes(Codec.ISO8859.charSet))
    out_ascii.flush()
    out_byte.toString(Codec.ISO8859.name)
  }
  /** Serialization implementation. */
  def inner() = DI.implementation

  class Plain extends Interface with Loggable
  trait Interface {
    this: Loggable ⇒
    /** YAML de/serializer. */
    lazy val yaml = {
      val options = new DumperOptions()
      options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK)
      new Yaml(options)
    }

    /** Load element from an Iterable[A] with fnLoadElement(). */
    def acquireElementBox[B <: Element](objectId: UUID, parentNode: Node)(implicit m: Manifest[B]): Option[B] = {
      null
    }
    /** Load graph from the specific base directory. */
    def acquireGraph(id: Symbol, base: File): Graph[_ <: Model.Like] = {
      null
    }
    /** Load node with the specific UUID for the specific parent. */
    def acquireNode(uniqueId: UUID, parentNode: Node): Node = {
      null
    }
    /** Save element. */
    def freezeElementBox(elementBox: ElementBox[_ <: Element]) {
      log.debug(s"Freeze ${elementBox}.")
      if (elementBox.node.graph.storages.isEmpty) {
        log.debug("Unable to freeze node without any defined storages.")
        return
      }
      val fileStorages = elementBox.node.graph.storages.filter(storage ⇒ storage.getScheme() == "file" && storage.isAbsolute())
      if (elementBox.node.graph.storages.isEmpty) {
        log.debug("Unable to freeze node without any defined storages with 'file' scheme.")
        return
      }
      fileStorages.foreach { storageURI ⇒
        val storageDirectory = new File(storageURI)
        val elementDirectory = getOrCreateElementDirectory(storageDirectory, elementBox)
        //val graphDescriptionFile = new File(graphDirectory, "description")
        //printToFile(graphDescriptionFile)(_.println(graphDescription(graph)))
        //freezeNode(modelNode)
      }
    }
    /** Save graph. */
    def freezeGraph(graph: Graph[_ <: Model.Like]) {
      log.debug(s"Freeze ${graph}.")
      if (graph.storages.isEmpty) {
        log.debug("Unable to freeze graph without any defined storages.")
        return
      }
      val fileStorages = graph.storages.filter(storage ⇒ storage.getScheme() == "file" && storage.isAbsolute())
      if (graph.storages.isEmpty) {
        log.debug("Unable to freeze graph without any defined storages with 'file' scheme.")
        return
      }
      graph.node.freezeRead { modelNode ⇒
        fileStorages.foreach { storageURI ⇒
          val storageDirectory = new File(storageURI)
          val graphDirectory = getOrCreateGraphDirectory(storageDirectory, graph)
          val graphDescriptionFile = new File(graphDirectory, "description")
          printToFile(graphDescriptionFile)(_.println(graphDescription(graph)))
          freezeNode(modelNode)
        }
      }
    }
    /** Save node. */
    def freezeNode(node: Node, recursive: Boolean = true) {
      log.debug(s"Freeze ${node}.")
      if (node.graph.storages.isEmpty) {
        log.debug("Unable to freeze node without any defined storages.")
        return
      }
      val fileStorages = node.graph.storages.filter(storage ⇒ storage.getScheme() == "file" && storage.isAbsolute())
      if (node.graph.storages.isEmpty) {
        log.debug("Unable to freeze node without any defined storages with 'file' scheme.")
        return
      }
      node.freezeRead { node ⇒
        fileStorages.foreach { storageURI ⇒
          val storageDirectory = new File(storageURI)
          val nodeDirectory = getOrCreateNodeDirectory(storageDirectory, node)
          val nodeDescriptionFile = new File(nodeDirectory, "description")
          printToFile(nodeDescriptionFile)(_.println(nodeDescription(node)))
          freezeElementBox(node.rootElementBox)
          node.children.par.foreach(freezeNode(_))
        }
      }
    }
    /** Get or create element directory. */
    protected def getOrCreateElementDirectory(base: File, elementBox: ElementBox[_ <: Element]): File = {
      val relativePart = (elementBox.elementUniqueId +: (elementBox.node +: elementBox.node.threadSafe(_.ancestors)).
        reverse.map(_.id.name)).mkString(File.separator)
      //      if (!graphDirectory.exists())
      //        if (!graphDirectory.mkdirs())
      //          throw new IOException(s"Unable to create ${graphDirectory}.")
      //      graphDirectory
      null
    }
    /** Get or create graph directory. */
    protected def getOrCreateGraphDirectory(base: File, graph: Graph[_ <: Model.Like]): File = {
      val graphDirectory = new File(base, graph.origin.name)
      if (!graphDirectory.exists())
        if (!graphDirectory.mkdirs())
          throw new IOException(s"Unable to create ${graphDirectory}.")
      graphDirectory
    }
    /** Get or create node directory. */
    protected def getOrCreateNodeDirectory(base: File, node: Node.ThreadUnsafe): File = {
      val relativePart = (node +: node.ancestors).reverse.map(_.id.name).mkString(File.separator)
      val nodeDirectory = new File(base, relativePart)
      if (!nodeDirectory.exists())
        if (!nodeDirectory.mkdirs())
          throw new IOException(s"Unable to create ${nodeDirectory}.")
      nodeDirectory
    }
    /** Get graph description content. */
    protected def graphDescription(graph: Graph[_ <: Model.Like]): String = {
      val map = new java.util.HashMap[String, AnyRef]()
      map.put("created_milli", graph.created.milliseconds: java.lang.Long)
      map.put("created_nano", graph.created.nanoShift: java.lang.Long)
      map.put("modified_milli", graph.modified.milliseconds: java.lang.Long)
      map.put("modified_nano", graph.modified.nanoShift: java.lang.Long)
      map.put("origin", graph.origin.name)
      yaml.dump(map)
    }
    /** Get node description content. */
    protected def nodeDescription(node: Node.ThreadUnsafe): String = {
      val map = new java.util.HashMap[String, AnyRef]()
      map.put("modified_milli", node.modified.milliseconds: java.lang.Long)
      map.put("modified_nano", node.modified.nanoShift: java.lang.Long)
      yaml.dump(map)
    }
    /** Print to file. */
    protected def printToFile(f: java.io.File)(op: java.io.PrintWriter ⇒ Unit) {
      val p = new java.io.PrintWriter(f)
      try { op(p) } finally { p.close() }
    }
  }
  /**
   * Dependency injection routines
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    /** Implementation of the serialization. */
    lazy val implementation: Interface = injectOptional[Interface] getOrElse new Plain
  }
}
