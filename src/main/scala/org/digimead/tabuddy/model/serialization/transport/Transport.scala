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

import java.net.URI
import java.util.UUID

import scala.collection.JavaConverters._

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.serialization.Serialization
import org.digimead.tabuddy.model.serialization.Serialization.interface2implementation

/**
 * Serialization transport that provides implementation for the specific URI scheme
 */
trait Transport {
  this: Loggable ⇒
  /** Element resource name. */
  val elementResourceName = "element"
  /** Descriptor resource name. */
  val descriptorResourceName = "descriptor.yaml"
  /** Model directory name. */
  val modelDirectoryName = "model"

  /** Load element with the specific UUID for the specific container. */
  def acquireElement[A <: Element](elementBox: ElementBox[A], storageURI: URI)(implicit m: Manifest[A]): A
  /** Load element box descriptor with the specific UUID for the specific container. */
  def acquireElementBox(elementUniqueId: UUID, parentNode: Node.ThreadUnsafe, storageURI: URI): Serialization.Descriptor.Element[_ <: Element]
  /** Load graph descriptor with the specific origin from the specific URI. */
  def acquireGraph(origin: Symbol, storageURI: URI): Serialization.Descriptor.Graph[_ <: Model.Like]
  /** Load model node descriptor with the specific id. */
  def acquireModel(id: Symbol, origin: Symbol, storageURI: URI): Serialization.Descriptor.Node
  /** Load node descriptor with the specific id for the specific parent. */
  def acquireNode(id: Symbol, parentNode: Node.ThreadUnsafe, storageURI: URI): Serialization.Descriptor.Node
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

  /** Create element descriptor from YAML. */
  protected def elementDescriptorFromYaml(descriptor: String): Serialization.Descriptor.Element[_ <: Element] = {
    var axes: Seq[Axis[_ <: AnyRef with Serializable]] = Seq()
    var clazz: Class[_ <: Element] = null
    var element_unique_id: UUID = null
    var modified_milli = 0L
    var modified_nano = 0L
    var serialization: Serialization.Identifier = null
    Serialization.yaml.load(descriptor).
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
          serialization = Serialization.perIdentifier.keys.find(_.extension == value).
            getOrElse { throw new IllegalStateException(s"Unable to find serialization mechanism for '${value}'.") }
        case (key, value) ⇒
          log.warn(s"Unknown element descriptor entry: ${key} -> ${value}.")
      }
    Serialization.Descriptor.Element(clazz, Coordinate(axes: _*), element_unique_id,
      Element.timestamp(modified_milli, modified_nano), serialization)
  }
  /** Create YAML element descriptor. */
  protected def elementDescriptorToYAML(elementBox: ElementBox[_ <: Element]): String = {
    val axisMap = new java.util.HashMap[String, AnyRef]()
    elementBox.coordinate.coordinate.foreach { axis ⇒
      axisMap.put(axis.id.name, axis.value.toString())
    }
    val modified_milli = elementBox.get.modification.milliseconds
    val modified_nano = elementBox.get.modification.nanoShift
    val descriptorMap = new java.util.HashMap[String, AnyRef]()
    descriptorMap.put("axes", axisMap)
    descriptorMap.put("class", elementBox.elementType.runtimeClass.getName)
    descriptorMap.put("element_unique_id", elementBox.elementUniqueId.toString)
    descriptorMap.put("modified_milli", modified_milli: java.lang.Long)
    descriptorMap.put("modified_nano", modified_nano: java.lang.Long)
    descriptorMap.put("serialization_identifier", elementBox.serialization.extension)
    Serialization.yaml.dump(descriptorMap)
  }
  /** Create element descriptor from YAML. */
  protected def graphDescriptorFromYaml(descriptor: String): Serialization.Descriptor.Graph[_ <: Model.Like] = {
    var created_milli = 0L
    var created_nano = 0L
    var model_id: Symbol = null
    var model_type: Class[_ <: Model.Like] = null
    var modified_milli = 0L
    var modified_nano = 0L
    var origin: Symbol = null
    var storages: Seq[URI] = Seq()
    Serialization.yaml.load(descriptor).
      asInstanceOf[java.util.LinkedHashMap[String, AnyRef]].asScala.foreach {
        case ("created_milli", value: java.lang.Long) ⇒ created_milli = value
        case ("created_milli", value: java.lang.Integer) ⇒ created_milli = value.toLong
        case ("created_nano", value: java.lang.Long) ⇒ created_nano = value
        case ("created_nano", value: java.lang.Integer) ⇒ created_nano = value.toLong
        case ("model_id", value: String) ⇒ model_id = Symbol(value)
        case ("model_type", value: String) ⇒ model_type = getClass.getClassLoader().loadClass(value).asInstanceOf[Class[_ <: Model.Like]]
        case ("modified_milli", value: java.lang.Long) ⇒ modified_milli = value
        case ("modified_milli", value: java.lang.Integer) ⇒ modified_milli = value.toLong
        case ("modified_nano", value: java.lang.Long) ⇒ modified_nano = value
        case ("modified_nano", value: java.lang.Integer) ⇒ modified_nano = value.toLong
        case ("origin", value: String) ⇒ origin = Symbol(value)
        case ("storages", value: java.util.ArrayList[_]) ⇒
          storages = value.asInstanceOf[java.util.ArrayList[String]].asScala.map(new URI(_))
        case (key, value) ⇒
          log.warn(s"Unknown graph descriptor entry: ${key} -> ${value}.")
      }
    Serialization.Descriptor.Graph(Element.timestamp(created_milli, created_nano), model_id, model_type,
      Element.timestamp(modified_milli, modified_nano), origin, storages)
  }
  /** Create YAML graph descriptor. */
  protected def graphDescriptorToYAML(graph: Graph[_ <: Model.Like]): String = {
    val storages = graph.storages.map(_.toString).asJava
    val descriptorMap = new java.util.HashMap[String, AnyRef]()
    descriptorMap.put("created_milli", graph.created.milliseconds: java.lang.Long)
    descriptorMap.put("created_nano", graph.created.nanoShift: java.lang.Long)
    descriptorMap.put("model_id", graph.node.id.name)
    descriptorMap.put("model_type", graph.node.getRootElementBox.elementType.runtimeClass.getName())
    descriptorMap.put("modified_milli", graph.modification.milliseconds: java.lang.Long)
    descriptorMap.put("modified_nano", graph.modification.nanoShift: java.lang.Long)
    descriptorMap.put("origin", graph.origin.name)
    descriptorMap.put("storages", storages)
    Serialization.yaml.dump(descriptorMap)
  }
  /** Create node descriptor from YAML content. */
  protected def nodeDescriptorFromYaml(descriptor: String): Serialization.Descriptor.Node = {
    var children = Seq[Symbol]()
    var elements = Seq[UUID]()
    var id: Symbol = null
    var unique: UUID = null
    var modified_milli = 0L
    var modified_nano = 0L
    Serialization.yaml.load(descriptor).
      asInstanceOf[java.util.LinkedHashMap[String, AnyRef]].asScala.foreach {
        case ("children_ids", value: java.util.ArrayList[_]) ⇒
          children = value.asInstanceOf[java.util.ArrayList[String]].asScala.map(Symbol(_))
        case ("element_ids", value: java.util.ArrayList[_]) ⇒
          elements = value.asInstanceOf[java.util.ArrayList[String]].asScala.map(UUID.fromString(_))
        case ("id", value: String) ⇒ id = Symbol(value)
        case ("modified_milli", value: java.lang.Integer) ⇒ modified_milli = value.toLong
        case ("modified_milli", value: java.lang.Long) ⇒ modified_milli = value
        case ("modified_nano", value: java.lang.Integer) ⇒ modified_nano = value.toLong
        case ("modified_nano", value: java.lang.Long) ⇒ modified_nano = value
        case ("unique", value: String) ⇒ unique = UUID.fromString(value)
        case (key, value) ⇒
          log.warn(s"Unknown node descriptor entry: ${key} -> ${value}.")
      }
    Serialization.Descriptor.Node(children, elements, id, Element.timestamp(modified_milli, modified_nano), unique)
  }
  /** Create YAML node descriptor. */
  protected def nodeDescriptorToYAML(node: Node.ThreadUnsafe): String = {
    val elementIds = (Seq(node.rootElementBox) ++ node.projectionElementBoxes.values).map(_.elementUniqueId.toString).asJava
    val childrenIds = node.toSeq.map(_.id.name).asJava
    val descriptorMap = new java.util.HashMap[String, AnyRef]()
    descriptorMap.put("children_ids", childrenIds)
    descriptorMap.put("element_ids", elementIds)
    descriptorMap.put("id", node.id.name)
    descriptorMap.put("modified_milli", node.modification.milliseconds: java.lang.Long)
    descriptorMap.put("modified_nano", node.modification.nanoShift: java.lang.Long)
    descriptorMap.put("unique", node.unique.toString())
    Serialization.yaml.dump(descriptorMap)
  }
}
