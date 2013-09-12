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
import java.util.UUID
import scala.annotation.tailrec
import scala.collection.JavaConverters
import scala.collection.mutable
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.dsl.DSLType.dsltype2implementation
import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Element.Scope
import org.digimead.tabuddy.model.element.Element.Timestamp
import org.digimead.tabuddy.model.element.Reference
import org.digimead.tabuddy.model.element.Stash
import org.digimead.tabuddy.model.element.Value
import org.digimead.tabuddy.model.graph.{ Node ⇒ TANode }
import org.yaml.snakeyaml.DumperOptions
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.MappingNode
import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.ScalarNode
import org.yaml.snakeyaml.nodes.SequenceNode
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.representer.Represent
import org.yaml.snakeyaml.representer.Representer
import java.net.URI
import org.digimead.tabuddy.model.graph.ElementBox

class YAMLSerialization extends Mechanism {
  /** Identifier of the serialization mechanism. */
  val identifier = YAMLSerialization.Identifier

  /** Load element. */
  def load[A <: Element](elementBox: ElementBox[A], from: Array[Byte])(implicit m: Manifest[A]): A = {
    log.debug(s"Load ${elementBox}.")
    /*   if (ma.runtimeClass == classOf[Nothing])
      throw new IllegalArgumentException("Element type is undefined")
    //    if (mb.runtimeClass == classOf[Nothing])
    //      throw new IllegalArgumentException("Stash type is undefined")
    var hash = mutable.HashMap[UUID, Element]()
    // load elements
    var data = loadElement()
    val yaml = new Yaml(YAMLSerialization.ElementConstructor)
    while (data.nonEmpty) {
      try {
        Option(yaml.load(data.get).asInstanceOf[Element]) match {
          case Some(element) =>
            filter(element).foreach(element => hash(element.eNodeId) = element)
          case None =>
            log.error("unable to unpack data:\n " + data.get)
        }
      } catch {
        // catch all throwables, return None if any
        case e: Throwable =>
          log.error("unable to acuire elements: " + e, e)
      }
      data = loadElement()
    }
    // build structure
    var rootElements = Seq[Element]()
    hash.foreach {
      case (unique, element) =>
      /*        val parent = element.eStash.context.container.unique
        hash.get(parent) match {
          case Some(parentElement) if parentElement == element =>
            // parent is cyclic reference
            if (element.eScope == Model.scope) {
              // drop all other expectants
              rootElements = Seq(element)
            } else
              log.fatal("detected a cyclic reference inside an unexpected element " + element)
          case Some(parentElement) =>
            // parent found
            //parentElement.eChildren += Element.box(element.eStash.context, Some(element))
          case None =>
            // parent not found
            element.eAs[A].foreach(element => rootElements = rootElements) // :+ element)
        }*/
    }
    // return result
    hash.clear
    rootElements.find(_.isInstanceOf[Model]) match {
      case Some(model) =>
        // return model as expected type
        //model.eStash.model = Some(model.asInstanceOf[Model.Generic])
        //model.asInstanceOf[Model.Generic].eIndexRebuid()
        //model.eAs[A, B]
        None
      case None if rootElements.size == 1 =>
        // return other element as expected type
        rootElements.head.eAs[A]
      case None if rootElements.isEmpty =>
        log.error("there is no root elements detected")
        None
      case None =>
        log.error("there are more than one root elements detected: " + rootElements.mkString(","))
        None
    }*/
    null.asInstanceOf[A]
  }
  /**
   * Get serialized element.
   * Filter/adjust children with filter()
   * Save adjusted child to [String] with saveElement().
   */
  def save(element: Element): Array[Byte] = {
    //    val options = new DumperOptions()
    //    options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK)
    //    val yaml = new Yaml(new YAMLSerialization.ElementRepresenter, new DumperOptions())
    //    freezeWorker(yaml, saveElement, element)
    null
  }
  //@tailrec
  private def freezeWorker(yaml: Yaml, saveElement: (Element, String) ⇒ Unit,
    elements: Element*) {
    if (elements.isEmpty)
      return
    /*val saved = elements.map { element =>
     val serialized = element.eCopy(null, List())
      filter(serialized) match {
        case Some(filtered) =>
          saveElement(element, yaml.dump(filtered))
          element.eChildren.toSeq.sortBy(_.eId.name) // simplify the debugging with sortBy
        case None =>
          log.debug("skip freeze element " + element)
          Seq()
      }
    }
    freezeWorker(yaml, saveElement, filter, saved.flatten: _*)*/
  }
}

object YAMLSerialization extends Loggable {
  /** Convert YAML to the element */
  def from(data: String): Option[Element] = {
    //    val yaml = new Yaml(ElementConstructor)
    //    Option(yaml.load(data).asInstanceOf[Element])
    None
  }
  /** Convert the current element without children to YAML */
  def to(element: Element): String = {
    val options = new DumperOptions()
    options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK)
    //    val yaml = new Yaml(new ElementRepresenter, new DumperOptions())
    //yaml.dump(element.eCopy(null, List()))
    null
  }

  /*  object ElementConstructor extends Constructor(classOf[Element]) {
    val tagContext = new Tag(classOf[Context])
    val tagCoordinate = new Tag(classOf[Coordinate])
    val tagElement = new Tag(classOf[Element])
    val tagProperty = new Tag(classOf[YAMLSerialization.Property])
    val tagReference = new Tag(classOf[Reference])
    val tagStash = new Tag(classOf[Stash])
    this.yamlConstructors.put(tagContext, new ContextConstruct())
    this.yamlConstructors.put(tagCoordinate, new CoordinateConstruct())
    this.yamlConstructors.put(tagElement, new ElementConstruct())
    this.yamlConstructors.put(tagProperty, new PropertyConstruct())
    this.yamlConstructors.put(tagReference, new ReferenceConstruct())
    this.yamlConstructors.put(tagStash, new StashConstruct())

    class AxisConstruct extends CustomConstruct {
      def constructCustom(map: mutable.HashMap[String, Node]): AnyRef = try {
        val axis = for {
          id <- map.get("id").flatMap(node => Option(constructObject(node).asInstanceOf[String]))
          axisType <- map.get("type").flatMap(node => Option(constructObject(node).asInstanceOf[String]))
          value <- map.get("value").flatMap(node => Option(constructObject(node).asInstanceOf[String]))
          typeSymbol = Symbol(axisType)
        } yield DSLType.convertFromString(typeSymbol, value).map(deserializedValue =>
          Axis(Symbol(id), deserializedValue)(Manifest.classType(DSLType.symbolClassMap(typeSymbol))))
        if (axis.isEmpty)
          log.error("unable to unpack axis")
        axis.getOrElse(null)
      } catch {
        case e: Throwable =>
          log.error("unable to unpack axis: " + e, e)
          null
      }
    }
    class ContextConstruct extends CustomConstruct {
      def constructCustom(map: mutable.HashMap[String, Node]): AnyRef = try {
        val context = for {
          container <- map.get("container").flatMap { node => node.setTag(tagReference); Option(constructObject(node).asInstanceOf[Reference]) }
          digest = map.get("digest").flatMap(node => Option(constructObject(node).asInstanceOf[String]))
          file = map.get("file").flatMap(node => Option(constructObject(node).asInstanceOf[String]).map(new URI(_)))
          line = map.get("line").flatMap(node => Option(constructObject(node).asInstanceOf[Int]))
        } yield Context(container, file, line, digest)
        if (context.isEmpty)
          log.error("unable to unpack context")
        context.getOrElse(null)
      } catch {
        case e: Throwable =>
          log.error("unable to unpack context: " + e, e)
          null
      }
    }
    class CoordinateConstruct extends CustomConstruct {
      def constructCustom(map: mutable.HashMap[String, Node]): AnyRef = try {
        map.get("axes") match {
          case Some(snode: SequenceNode) =>
            snode.setListType(classOf[Axis[_ <: AnyRef with java.io.Serializable]])
            //           Coordinate(constructObject(snode).asInstanceOf[java.util.ArrayList[Axis[_ <: AnyRef with java.io.Serializable]]]: _*)
            Coordinate()
          case None =>
            Coordinate()
          case node =>
            log.debug("unexpected axes node: " + node)
            null
        }
        Coordinate()
      } catch {
        case e: Throwable =>
          log.error("unable to unpack context: " + e, e)
          null
      }
    }
    class ElementConstruct extends CustomConstruct {
      def constructCustom(map: mutable.HashMap[String, Node]): AnyRef = try {
        val element = for {
          stash <- map.get("stash").flatMap { node => node.setTag(tagStash); Option(constructObject(node).asInstanceOf[Stash]) }
          elementType <- map.get("type").flatMap(node => Option(constructObject(node).asInstanceOf[String]))
          elementClass = Class.forName(elementType)
          elementCtor = elementClass.getConstructor(stash.getClass())
        } yield elementCtor.newInstance(stash).asInstanceOf[Element]
        if (element.isEmpty)
          log.error("unable to unpack element")
        element.getOrElse(null)
      } catch {
        case e: Throwable =>
          log.error("unable to unpack element: " + e, e)
          null
      }
    }
    class PropertyConstruct extends CustomConstruct {
      def constructCustom(map: mutable.HashMap[String, Node]): AnyRef = try {
        val property = for {
          context <- map.get("context").flatMap { node => node.setTag(tagContext); Option(constructObject(node).asInstanceOf[Context]) }
          data <- map.get("data").flatMap(node => Option(constructObject(node).asInstanceOf[String]))
          id <- map.get("id").flatMap(node => Option(constructObject(node).asInstanceOf[String]))
          static <- map.get("static").flatMap(node => Option(constructObject(node).asInstanceOf[Boolean]))
          typeSymbol <- map.get("type").flatMap(node => Option(constructObject(node).asInstanceOf[String]))
        } yield YAMLSerialization.Property(context, typeSymbol, data, id, static)
        if (property.isEmpty)
          log.error("unable to unpack property")
        property.getOrElse(null)
      } catch {
        case e: Throwable =>
          log.error("unable to unpack property: " + e, e)
          null
      }
    }
    class ReferenceConstruct extends CustomConstruct {
      def constructCustom(map: mutable.HashMap[String, Node]): AnyRef = try {
        val reference = for {
          coordinate <- map.get("coordinate").flatMap { node => node.setTag(tagCoordinate); Option(constructObject(node).asInstanceOf[Coordinate]) }
          origin <- map.get("origin").flatMap(node => Option(constructObject(node).asInstanceOf[String]))
          uniqueHi <- map.get("uniqueHi").flatMap(node => Option(constructObject(node).toString).map(_.toLong))
          uniqueLo <- map.get("uniqueLo").flatMap(node => Option(constructObject(node).toString).map(_.toLong))
        } yield Reference(Symbol(origin),
          new UUID(uniqueHi, uniqueLo), coordinate)
        if (reference.isEmpty)
          log.error("unable to unpack reference")
        reference.getOrElse(null)
      } catch {
        case e: Throwable =>
          log.error("unable to unpack reference: " + e, e)
          null
      }
    }
    class StashConstruct extends CustomConstruct {
      def constructCustom(map: mutable.HashMap[String, Node]): AnyRef = try {
        val stash = for {
          stashType <- map.get("type").flatMap(node => Option(constructObject(node).asInstanceOf[String]))
          context <- map.get("context").flatMap { node => node.setTag(tagContext); Option(constructObject(node).asInstanceOf[Context]) }
          coordinate <- map.get("coordinate").flatMap { node => node.setTag(tagCoordinate); Option(constructObject(node).asInstanceOf[Coordinate]) }
          createdHi <- map.get("createdHi").flatMap(node => Option(constructObject(node).toString).map(_.toLong))
          createdLo <- map.get("createdLo").flatMap(node => Option(constructObject(node).toString).map(_.toLong))
          id <- map.get("id").flatMap(node => Option(constructObject(node).asInstanceOf[String]))
          modifiedHi <- map.get("modifiedHi").flatMap(node => Option(constructObject(node).toString).map(_.toLong))
          modifiedLo <- map.get("modifiedLo").flatMap(node => Option(constructObject(node).toString).map(_.toLong))
          scopeClassName <- map.get("scopeClass").flatMap(node => Option(constructObject(node).asInstanceOf[String]))
          scopeName <- map.get("scopeName").flatMap(node => Option(constructObject(node).asInstanceOf[String]))
          uniqueHi <- map.get("uniqueHi").flatMap(node => Option(constructObject(node).toString).map(_.toLong))
          uniqueLo <- map.get("uniqueLo").flatMap(node => Option(constructObject(node).toString).map(_.toLong))
          property <- map.get("property").flatMap {
            case snode: SequenceNode =>
              //for (node <- snode.getValue()) node.setTag(tagProperty)
              Option(ElementConstructor.constructObject(snode).asInstanceOf[java.util.List[YAMLSerialization.Property]])
          }
          created = Element.Timestamp(createdHi, createdLo)
          modified = Element.Timestamp(modifiedHi, modifiedLo)
          unique = new UUID(uniqueHi, uniqueLo)
          properies = unpackProperties(property)
        } yield {
          val scopeClass = Class.forName(scopeClassName)
          val scopeCtor = scopeClass.getConstructor(classOf[Symbol])
          val scope = scopeCtor.newInstance(Symbol(scopeName)).asInstanceOf[Element.Scope]
          val stashClass = Class.forName(stashType)
          val stashCtor = stashClass.getConstructor(
            classOf[Context],
            classOf[Coordinate],
            classOf[Element.Timestamp],
            classOf[Symbol],
            classOf[Element.Timestamp],
            classOf[Element.Scope],
            classOf[UUID],
            classOf[Stash.Data])
          val stash = stashCtor.newInstance(context, coordinate, created, Symbol(id), modified, scope, unique, properies).asInstanceOf[Stash]
          assert(stash.scope.getClass().getName() == scopeClassName, "Incorrect scope class: got %s, expected %s".format(stash.scope.getClass().getName(), scopeClassName))
          stash
        }
        if (stash.isEmpty)
          log.error("unable to unpack stash")
        stash.getOrElse(null)
      } catch {
        case e: Throwable =>
          log.error("unable to unpack stash: " + e, e)
          null
      }
      protected def unpackProperties(list: java.util.List[YAMLSerialization.Property]): Stash.Data = {
        val property = new Stash.Data
        /* list.foreach { raw =>
          val context = raw.context
          val typeSymbol = Symbol(raw.typeSymbol)
          val valueID = Symbol(raw.id)
          if (DSLType.symbols(typeSymbol)) {
            if (!property.isDefinedAt(valueID))
              property(valueID) = new mutable.HashMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]] with mutable.SynchronizedMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]]
            DSLType.convertFromString(typeSymbol, raw.data) match {
              case Some(data) if raw.static =>
                property(valueID)(typeSymbol) = new Value.Static(data, context)(Manifest.classType(DSLType.symbolClassMap(typeSymbol)))
              case Some(data) =>
                property(valueID)(typeSymbol) = new Value.Dynamic(() => data, context)(Manifest.classType(DSLType.symbolClassMap(typeSymbol)))
              case None =>
                log.error("unable to unpack value '%s=%s".format(raw.id, raw.data))
            }
          } else {
            log.error("unable to unpack property %s with unknown symbol %s".format(valueID, typeSymbol))
          }
        }*/
        property
      }
    }
    abstract class CustomConstruct extends ConstructMapping {
      override protected def createEmptyJavaBean(node: MappingNode): AnyRef =
        null // skip a bean creation
      override def constructJavaBean2ndStep(node: MappingNode, obj: AnyRef): AnyRef = {
        var map = mutable.HashMap[String, Node]()
        flattenMapping(node)
        /*        for (tuple <- node.getValue()) {
          // key must be scalar
          val keyNode = if (tuple.getKeyNode().isInstanceOf[ScalarNode])
            tuple.getKeyNode().asInstanceOf[ScalarNode]
          else
            throw new YAMLException("Keys must be scalars but found: " + tuple.getKeyNode());
          val valueNode = tuple.getValueNode()
          // keys can only be Strings
          keyNode.setType(classOf[String])
          val key = constructObject(keyNode).asInstanceOf[String]
          map(key) = valueNode
        }*/
        constructCustom(map)
      }
      def constructCustom(map: mutable.HashMap[String, Node]): AnyRef
    }
  }

  class ElementRepresenter extends Representer {
    multiRepresenters.put(classOf[Axis[_ <: AnyRef with java.io.Serializable]], new AxisRepresent)
    multiRepresenters.put(classOf[Context], new ContextRepresent)
    multiRepresenters.put(classOf[Coordinate], new CoordinateRepresent)
    multiRepresenters.put(classOf[Element], new ElementRepresent)
    multiRepresenters.put(classOf[YAMLSerialization.Property], new PropertyRepresent)
    multiRepresenters.put(classOf[Reference], new ReferenceRepresent)
    multiRepresenters.put(classOf[Stash], new StashRepresent)

    class AxisRepresent extends Represent {
      override def representData(data: AnyRef): Node = {
        val axis = data.asInstanceOf[Axis[_ <: AnyRef with java.io.Serializable]]
        val map = new java.util.HashMap[String, AnyRef]()
        DSLType.classSymbolMap.get(axis.m.runtimeClass) match {
          case Some(typeSymbol) =>
            DSLType.convertToString(typeSymbol, axis.value) match {
              case Some(valueData) =>
                map.put("id", axis.id.name)
                map.put("type", typeSymbol.name)
                map.put("value", valueData)
              case None =>
                log.error("unable to convert axis value " + axis.value)
            }
          case None =>
            log.error("unable to convert axis with class %s, suitable type symbol not found".format(axis.m.runtimeClass))
        }
        representMapping(Tag.MAP, map, null)
      }
    }
    class ContextRepresent extends Represent {
      override def representData(data: AnyRef): Node = {
        val context = data.asInstanceOf[Context]
        val map = new java.util.HashMap[String, AnyRef]()
        map.put("origin", context.origin)
        map.put("unique", context.unique)
        context.digest.foreach(map.put("digest", _))
        context.file.foreach(map.put("file", _))
        context.line.foreach(line => map.put("line", Int.box(line)))
        representMapping(Tag.MAP, map, null)
      }
    }
    class CoordinateRepresent extends Represent {
      override def representData(data: AnyRef): Node = {
        val coordinate = data.asInstanceOf[Coordinate]
        val map = new java.util.HashMap[String, AnyRef]()
        //map.put("axes", seqAsJavaList(coordinate.coordinate.as))
        representMapping(Tag.MAP, map, null)
      }
    }
    class ElementRepresent extends Represent {
      override def representData(data: AnyRef): Node = {
        val element = data.asInstanceOf[Element]
        val map = new java.util.HashMap[String, AnyRef]()
        map.put("type", element.getClass().getName())
        map.put("stash", element.eStash)
        representMapping(Tag.MAP, map, null)
      }
    }
    class PropertyRepresent extends Represent {
      override def representData(data: AnyRef): Node = {
        val property = data.asInstanceOf[YAMLSerialization.Property]
        val map = new java.util.HashMap[String, AnyRef]()
        map.put("context", property.context)
        map.put("type", property.typeSymbol)
        map.put("data", property.data)
        map.put("id", property.id)
        map.put("static", Boolean.box(property.static))
        representMapping(Tag.MAP, map, null)
      }
    }
    class ReferenceRepresent extends Represent {
      override def representData(data: AnyRef): Node = {
        val reference = data.asInstanceOf[Reference]
        val map = new java.util.HashMap[String, AnyRef]()
        map.put("coordinate", reference.coordinate)
        map.put("origin", reference.origin.name)
        map.put("uniqueHi", Long.box(reference.unique.getMostSignificantBits()))
        map.put("uniqueLo", Long.box(reference.unique.getLeastSignificantBits()))
        representMapping(Tag.MAP, map, null)
      }
    }
    class StashRepresent extends Represent {
      override def representData(data: AnyRef): Node = {
        val stash = data.asInstanceOf[Stash]
        var properties = Seq[YAMLSerialization.Property]()
        stash.property.keys.foreach { valueID =>
          stash.property(valueID).foreach {
            case (typeSymbol, value) if DSLType.symbols(typeSymbol) =>
              DSLType.convertToString(typeSymbol, value.get) match {
                case Some(valueData) =>
                  properties = properties :+ YAMLSerialization.Property(value.context, typeSymbol.name, valueData, valueID.name, value.isInstanceOf[Value.Static[_]])
                case None =>
                  log.error("unable to convert value " + value)
              }
            case (typeSymbol, value) =>
              log.error("unable to convert properties with symbol %s, suitable type not found".format(typeSymbol))
          }
        }
        val map = new java.util.HashMap[String, AnyRef]()
        map.put("type", stash.getClass().getName())
        //map.put("coordinate", stash.coordinate)
        map.put("createdHi", Long.box(stash.created.milliseconds))
        map.put("createdLo", Long.box(stash.created.nanoShift))
        //map.put("id", stash.id.name)
        map.put("modifiedHi", Long.box(stash.modified.milliseconds))
        map.put("modifiedLo", Long.box(stash.modified.nanoShift))
        map.put("scopeClass", stash.scope.getClass.getName())
        map.put("scopeName", stash.scope.modificator.name)
        //map.put("uniqueHi", Long.box(stash.unique.getMostSignificantBits()))
        //map.put("uniqueLo", Long.box(stash.unique.getLeastSignificantBits()))
        //map.put("property", seqAsJavaList(properties))
        representMapping(Tag.MAP, map, null)
      }
    }
  }

  case class Property(val context: Context, val typeSymbol: String, val data: String, val id: String, val static: Boolean)*/
  /** YAMLSerialization identifier. */
  object Identifier extends Serialization.Identifier { val extension = "yaml" }
}
