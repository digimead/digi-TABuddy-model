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
import java.net.URI
import java.util.UUID

import scala.Option.option2Iterable
import scala.collection.JavaConverters._
import scala.collection.immutable

import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.{ Element ⇒ TAElement }
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Node
import org.yaml.snakeyaml.DumperOptions
import org.yaml.snakeyaml.Yaml

import scala.language.implicitConversions

/**
 * Interface for serialization of an element content.
 */
trait Serialization extends Loggable {
  /** Identifier of the serialization mechanism. */
  val identifier: Serialization.Identifier

  /** Load element. */
  def acquireElement[A <: Element](objectId: UUID, parentNode: Node, from: Array[Byte])(implicit m: Manifest[A]): Option[A]
  /** Save element. */
  def freezeElement(element: Element): Array[Byte]
}

object Serialization {
  implicit def interface2implementation(m: Serialization.type): Interface = m.inner

  /** Serialization implementation. */
  def inner = DI.implementation
  /** Consumer defined map of per scheme transport. */
  def perScheme = DI.perScheme
  /** Consumer defined map of per extension serialization. */
  def perExtension = DI.perExtension

  /** Common serialization implementation. */
  class Common extends Interface with Loggable
  /** Description content of the serialized objects. */
  object Description {
    import org.digimead.tabuddy.model.element.{ Element ⇒ TAElement }
    case class Element[A <: TAElement](val clazz: Class[A], val coordinate: Coordinate, elementUniqueId: UUID,
      val modified: TAElement.Timestamp, val serializationIdentifier: Serialization.Identifier)
    case class Node(val children: Seq[UUID], val elements: Seq[UUID], val id: Symbol,
      val modified: TAElement.Timestamp, val unique: UUID)
    case class Graph[A <: Model.Like](val created: TAElement.Timestamp, val modelId: Symbol, val modelType: Class[A],
      val modified: TAElement.Timestamp, val origin: Symbol, val storages: Seq[URI])
  }
  /**
   * Serialization interface.
   */
  trait Interface {
    this: Loggable ⇒
    /** YAML de/serializer. */
    lazy val yaml = {
      val options = new DumperOptions()
      options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK)
      new Yaml(options)
    }

    /** Load element box with the specific UUID for the specific container. */
    def acquireElementBox(objectId: UUID, node: Node.ThreadUnsafe): ElementBox[_ <: Element] = {
      log.debug(s"Acquire element box ${objectId}.")
      if (node.graph.storages.isEmpty)
        throw new IllegalArgumentException("Unable to aquire element box without any defined storages.")
      val elementBoxes: Seq[Option[ElementBox[_ <: Element]]] = node.graph.storages.map {
        case storageURI if storageURI.isAbsolute() ⇒
          Serialization.perScheme.get(storageURI.getScheme()) match {
            case Some(transport) ⇒
              Some(transport.acquireElementBox(objectId, node, storageURI))
            case None ⇒
              log.error(s"Unable to save element box to URI with unknown scheme ${storageURI.getScheme}.")
              None
          }
        case storageURI ⇒
          log.fatal(s"Unable to process relative storage URI as base: ${storageURI}.")
          None
      }
      val mostUpToDate = elementBoxes.flatten.maxBy(_.unmodified)
      // TODO Synchronize obsolete elementBoxes
      mostUpToDate
    }
    /** Load graph from the specific base directory. */
    def acquireGraph(id: Symbol, base: File): Graph[_ <: Model.Like] = {
      log.debug(s"Acquire graph ${id}.")
      // Bootstrap graph from here. After that we may check another locations with more up to date elements.
      val bootstrapURI = base.getAbsoluteFile.toURI
      log.debug(s"Bootstrap graph from ${bootstrapURI}.")
      Serialization.perScheme.get(bootstrapURI.getScheme()) match {
        case Some(transport) ⇒ transport.acquireGraph(id, bootstrapURI)
        case None ⇒ throw new IllegalArgumentException(s"Unable to load graph from URI with unknown scheme ${bootstrapURI.getScheme}.")
      }
    }
    /** Load node with the specific UUID for the specific parent. */
    def acquireNode(uniqueId: UUID, parentNode: Node): Node = {
      null
    }
    /** Save element. */
    def freezeElementBox(elementBox: ElementBox[_ <: Element]) {
      log.debug(s"Freeze ${elementBox}.")
      if (elementBox.node.graph.storages.isEmpty) {
        log.debug("Unable to freeze element box without any defined storages.")
        return
      }
      elementBox.node.safeRead { node ⇒
        node.graph.storages.foreach {
          case storageURI if storageURI.isAbsolute() ⇒
            Serialization.perScheme.get(storageURI.getScheme()) match {
              case Some(transport) ⇒ transport.freezeElementBox(elementBox, storageURI)
              case None ⇒ log.error(s"Unable to save element box to URI with unknown scheme ${storageURI.getScheme}.")
            }
          case storageURI ⇒
            log.fatal(s"Unable to process relative storage URI as base: ${storageURI}.")
        }
      }
    }
    /** Save graph. */
    def freezeGraph(graph: Graph[_ <: Model.Like]) {
      log.debug(s"Freeze ${graph}.")
      if (graph.storages.isEmpty) {
        log.debug("Unable to freeze graph without any defined storages.")
        return
      }
      graph.node.freezeRead { modelNode ⇒
        graph.storages.foreach {
          case storageURI if storageURI.isAbsolute() ⇒
            Serialization.perScheme.get(storageURI.getScheme()) match {
              case Some(transport) ⇒ transport.freezeGraph(graph, storageURI)
              case None ⇒ log.error(s"Unable to save graph to URI with unknown scheme ${storageURI.getScheme}.")
            }
          case storageURI ⇒
            log.fatal(s"Unable to process relative storage URI as base: ${storageURI}.")
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
      node.freezeRead { node ⇒
        node.graph.storages.foreach {
          case storageURI if storageURI.isAbsolute() ⇒
            Serialization.perScheme.get(storageURI.getScheme()) match {
              case Some(transport) ⇒ transport.freezeNode(node, storageURI)
              case None ⇒ log.error(s"Unable to save node to URI with unknown scheme ${storageURI.getScheme}.")
            }
          case storageURI ⇒
            log.fatal(s"Unable to process relative storage URI as base: ${storageURI}.")
        }
      }
    }
    /** Create element description from YAML content. */
    def elementDescriptionFromYaml(description: String): Description.Element[_ <: Element] = {
      var axes: Seq[Axis[_ <: AnyRef with Serializable]] = Seq()
      var clazz: Class[_ <: Element] = null
      var element_unique_id: UUID = null
      var modified_milli = 0L
      var modified_nano = 0L
      var serialization: Serialization.Identifier = null
      Serialization.yaml.load(description).
        asInstanceOf[java.util.LinkedHashMap[String, AnyRef]].asScala.foreach {
          case ("axes", value: java.util.HashMap[_, _]) ⇒
            axes = value.asInstanceOf[java.util.HashMap[String, String]].asScala.
              map { case (id, axis) ⇒ Axis(Symbol(id), null) }.toSeq
          case ("class", value: String) ⇒ clazz = getClass.getClassLoader().loadClass(value).asInstanceOf[Class[_ <: Element]]
          case ("element_unique_id", value: String) ⇒ element_unique_id = UUID.fromString(value)
          case ("modified_milli", value: java.lang.Integer) ⇒ modified_milli = value.toLong
          case ("modified_milli", value: java.lang.Long) ⇒ modified_milli = value
          case ("modified_nano", value: java.lang.Integer) ⇒ modified_nano = value.toLong
          case ("modified_nano", value: java.lang.Long) ⇒ modified_nano = value
          case ("serialization_identifier", value: String) ⇒
            serialization = Serialization.perExtension.keys.find(_.extension == value).
              getOrElse { throw new IllegalStateException(s"Unable to find serialization mechanism for '${value}'.") }
          case (key, value) ⇒
            log.warn(s"Unknown element description entry: ${key} -> ${value}.")
        }
      Description.Element(clazz, Coordinate(axes: _*), element_unique_id,
        Element.timestamp(modified_milli, modified_nano), serialization)
    }
    /** Create YAML element description content. */
    def elementDescriptionToYAML(elementBox: ElementBox[_ <: Element]): String = {
      val axisMap = new java.util.HashMap[String, AnyRef]()
      elementBox.coordinate.coordinate.foreach { axis ⇒
        axisMap.put(axis.id.name, axis.value.toString())
      }
      val modified_milli = elementBox.get.eModified.milliseconds
      val modified_nano = elementBox.get.eModified.nanoShift
      val descriptionMap = new java.util.HashMap[String, AnyRef]()
      descriptionMap.put("axes", axisMap)
      descriptionMap.put("class", elementBox.elementType.runtimeClass.getName)
      descriptionMap.put("modified_milli", modified_milli: java.lang.Long)
      descriptionMap.put("modified_nano", modified_nano: java.lang.Long)
      descriptionMap.put("serialization_identifier", elementBox.serialization.extension)
      yaml.dump(descriptionMap)
    }
    /** Create element description from YAML content. */
    def graphDescriptionFromYaml(description: String): Description.Graph[_ <: Model.Like] = {
      var created_milli = 0L
      var created_nano = 0L
      var model_id: Symbol = null
      var model_type: Class[_ <: Model.Like] = null
      var modified_milli = 0L
      var modified_nano = 0L
      var origin: Symbol = null
      var storages: Seq[URI] = Seq()
      Serialization.yaml.load(description).
        asInstanceOf[java.util.LinkedHashMap[String, AnyRef]].asScala.foreach {
          case ("created_milli", value: java.lang.Integer) ⇒ created_milli = value.toLong
          case ("created_milli", value: java.lang.Long) ⇒ created_milli = value
          case ("created_nano", value: java.lang.Integer) ⇒ created_nano = value.toLong
          case ("created_nano", value: java.lang.Long) ⇒ created_nano = value
          case ("model_id", value: String) ⇒ model_id = Symbol(value)
          case ("model_type", value: String) ⇒ model_type = getClass.getClassLoader().loadClass(value).asInstanceOf[Class[_ <: Model.Like]]
          case ("modified_milli", value: java.lang.Integer) ⇒ modified_milli = value.toLong
          case ("modified_milli", value: java.lang.Long) ⇒ modified_milli = value
          case ("modified_nano", value: java.lang.Integer) ⇒ modified_nano = value.toLong
          case ("modified_nano", value: java.lang.Long) ⇒ modified_nano = value
          case ("origin", value: String) ⇒ origin = Symbol(value)
          case ("storages", value: java.util.ArrayList[_]) ⇒
            storages = value.asInstanceOf[java.util.ArrayList[String]].asScala.map(new URI(_))
          case (key, value) ⇒
            log.warn(s"Unknown graph description entry: ${key} -> ${value}.")
        }
      Description.Graph(Element.timestamp(created_milli, created_nano), model_id, model_type,
        Element.timestamp(modified_milli, modified_nano), origin, storages)
    }

    /** Create YAML graph description content. */
    def graphDescriptionToYAML(graph: Graph[_ <: Model.Like]): String = {
      val storages = graph.storages.map(_.toString).asJava
      val descriptionMap = new java.util.HashMap[String, AnyRef]()
      descriptionMap.put("created_milli", graph.created.milliseconds: java.lang.Long)
      descriptionMap.put("created_nano", graph.created.nanoShift: java.lang.Long)
      descriptionMap.put("model_id", graph.node.id.name)
      descriptionMap.put("model_type", graph.node.getRootElementBox.elementType.runtimeClass.getName())
      descriptionMap.put("modified_milli", graph.modified.milliseconds: java.lang.Long)
      descriptionMap.put("modified_nano", graph.modified.nanoShift: java.lang.Long)
      descriptionMap.put("origin", graph.origin.name)
      descriptionMap.put("storages", storages)
      yaml.dump(descriptionMap)
    }
    /** Create node description from YAML content. */
    def nodeDescriptionFromYaml(description: String): Description.Node = {
      var children = Seq[UUID]()
      var elements = Seq[UUID]()
      var id: Symbol = null
      var unique: UUID = null
      var modified_milli = 0L
      var modified_nano = 0L
      Serialization.yaml.load(description).
        asInstanceOf[java.util.LinkedHashMap[String, AnyRef]].asScala.foreach {
          case ("children_ids", value: java.util.ArrayList[_]) ⇒
            children = value.asInstanceOf[java.util.ArrayList[String]].asScala.map(UUID.fromString(_))
          case ("element_ids", value: java.util.ArrayList[_]) ⇒
            elements = value.asInstanceOf[java.util.ArrayList[String]].asScala.map(UUID.fromString(_))
          case ("id", value: String) ⇒ id = Symbol(value)
          case ("modified_milli", value: java.lang.Integer) ⇒ modified_milli = value.toLong
          case ("modified_milli", value: java.lang.Long) ⇒ modified_milli = value
          case ("modified_nano", value: java.lang.Integer) ⇒ modified_nano = value.toLong
          case ("modified_nano", value: java.lang.Long) ⇒ modified_nano = value
          case ("unique", value: String) ⇒ unique = UUID.fromString(value)
          case (key, value) ⇒
            log.warn(s"Unknown node description entry: ${key} -> ${value}.")
        }
      Description.Node(children, elements, id, Element.timestamp(modified_milli, modified_nano), unique)
    }
    /** Create YAML node description content. */
    def nodeDescriptionToYAML(node: Node.ThreadUnsafe): String = {
      val elementIds = (Seq(node.rootElementBox) ++ node.projectionElementBoxes.values).map(_.elementUniqueId.toString).asJava
      val childrenIds = node.toSeq.map(_.unique.toString()).asJava
      val descriptionMap = new java.util.HashMap[String, AnyRef]()
      descriptionMap.put("children_ids", childrenIds)
      descriptionMap.put("element_ids", elementIds)
      descriptionMap.put("id", node.id.name)
      descriptionMap.put("modified_milli", node.modified.milliseconds: java.lang.Long)
      descriptionMap.put("modified_nano", node.modified.nanoShift: java.lang.Long)
      descriptionMap.put("unique", node.unique.toString())
      yaml.dump(descriptionMap)
    }
  }

  /**
   * Serialization transport that provides implementation for the specific URI scheme
   */
  trait Transport {
    this: Loggable ⇒
    /** Element resource name. */
    val elementResourceName = "element"
    /** Description resource name. */
    val descriptionResourceName = "description"
    /** Model directory name. */
    val modelDirectoryName = "model"

    /** Load element box with the specific UUID for the specific container. */
    def acquireElementBox(objectId: UUID, parentNode: Node.ThreadUnsafe, storageURI: URI): ElementBox[_ <: Element]
    /** Load graph from the specific base directory. */
    def acquireGraph(origin: Symbol, storageURI: URI): Graph[_ <: Model.Like]
    /** Load node with the specific UUID for the specific parent. */
    def acquireNode(uniqueId: UUID, parentNode: Node): Node
    /** Delete resource. */
    def delete(uri: URI)
    /** Save element to the specific URI. */
    def freezeElementBox(elementBox: ElementBox[_ <: Element], storageURI: URI)
    /** Save graph to the specific URI. */
    def freezeGraph(graph: Graph[_ <: Model.Like], storageURI: URI)
    /** Save node to the specific URI. */
    def freezeNode(node: Node.ThreadUnsafe, storageURI: URI, recursive: Boolean = true)
    /** Read resource. */
    def read(uri: URI): Array[Byte]
    /** Squeeze model. */
    def squeeze()
    /** Write resource. */
    def write(content: Array[Byte], uri: URI)
  }
  /** Serialization identifier that is associated with serialization mechanism. */
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
   * Dependency injection routines
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    /** Implementation of the serialization. */
    lazy val implementation: Interface = injectOptional[Interface] getOrElse new Common
    /** Per scheme serialization. */
    lazy val perExtension: immutable.HashMap[Identifier, Serialization] = injectOptional[immutable.HashMap[Identifier, Serialization]] getOrElse
      immutable.HashMap(BuiltinSerialization.Identifier -> new BuiltinSerialization,
        YAMLSerialization.Identifier -> new YAMLSerialization)
    /** Per scheme transports. */
    lazy val perScheme: immutable.HashMap[String, Transport] = injectOptional[immutable.HashMap[String, Transport]] getOrElse
      immutable.HashMap("file" -> new transport.Local())
  }
}
