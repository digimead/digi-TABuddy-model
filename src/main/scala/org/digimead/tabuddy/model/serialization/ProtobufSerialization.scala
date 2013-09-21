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

import scala.Option.option2Iterable
import scala.annotation.tailrec
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.ContextProtos
import org.digimead.tabuddy.model.CoordinateProtos
import org.digimead.tabuddy.model.ElementProtos
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.PropertyProtos
import org.digimead.tabuddy.model.ReferenceProtos
import org.digimead.tabuddy.model.StashProtos
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.dsl.DSLType.dsltype2implementation
import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element.Scope
import org.digimead.tabuddy.model.element.Element.Timestamp
import org.digimead.tabuddy.model.element.Reference
import org.digimead.tabuddy.model.element.Value

/*class ProtobufSerialization extends Serialization[Array[Byte]] {
  /** Default extension name for serialized resource. */
  val extension: String = "pbuf"

    /**
   * Load elements from Iterable[Array[Byte]] with loadElement().
   * Filter/adjust loaded element with filter()
   * Return deserialized element.
   */
  def acquire[A <: Element[B], B <: Stash](loadElement: () => Option[Array[Byte]],
    filter: (Element.Generic) => Option[Element.Generic] = filterAccept)(implicit ma: Manifest[A], mb: Manifest[B]): Option[A] = {
    if (ma.runtimeClass == classOf[Nothing])
      throw new IllegalArgumentException("Element type is undefined")
    if (mb.runtimeClass == classOf[Nothing])
      throw new IllegalArgumentException("Stash type is undefined")
    var hash = mutable.HashMap[UUID, Element.Generic]()
    // load elements
    var data = loadElement()
    while (data.nonEmpty) {
      try {
        val protoElements = ElementProtos.Element.Bundle.parseFrom(data.get)
        // unpack elements
        protoElements.getElementList().map { proto =>
          ProtobufSerialization.unpackElement(proto) match {
            case Some(element) =>
              filter(element).foreach(element => hash(element.eNode.unique) = element)
            case None =>
              log.error("unable to unpack data " + proto)
          }
        }
      } catch {
        // catch all throwables, return None if any
        case e: Throwable =>
          log.error("unable to acuire elements: " + e)
      }
      data = loadElement()
    }
    // build structure
    var rootElements = Seq[Element.Generic]()
    hash.foreach {
      case (unique, element) =>
        val parent = element.eStash.context.container.unique
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
            parentElement.eChildren += element
          case None =>
            // parent not found
            element.eAs[A, B].foreach(element => rootElements = rootElements :+ element)
        }
    }
    // return result
    hash.clear
    rootElements.find(_.isInstanceOf[Model.Interface[_]]) match {
      case Some(model) =>
        // return model as expected type
        //model.eStash.model = Some(model.asInstanceOf[Model.Generic])
        model.asInstanceOf[Model.Generic].eIndexRebuid()
        model.eAs[A, B]
      case None if rootElements.size == 1 =>
        // return other element as expected type
        rootElements.head.eAs[A, B]
      case None if rootElements.isEmpty =>
        log.error("there is no root elements detected")
        None
      case None =>
        log.error("there are more than one root elements detected: " + rootElements.mkString(","))
        None
    }
  }
  /**
   * Get serialized element.
   * Filter/adjust children with filter()
   * Save adjusted child to [Array[Byte]] with saveElement().
   */
  def freeze(element: Element.Generic,
    saveElement: (Element.Generic, Array[Byte]) => Unit,
    filter: (Element.Generic) => Option[Element.Generic] = filterAccept) =
    freezeWorker(saveElement, filter, element)
  @tailrec
  private def freezeWorker(saveElement: (Element.Generic, Array[Byte]) => Unit,
    filter: (Element.Generic) => Option[Element.Generic],
    elements: Element.Generic*) {
    if (elements.isEmpty)
      return
    val saved = elements.map { element =>
      val serialized = element.eCopy(List())
      filter(serialized) match {
        case Some(filtered) =>
          val elementsBundleBuilder = ElementProtos.Element.Bundle.newBuilder()
          elementsBundleBuilder.addElement(ProtobufSerialization.packElement(filtered))
          saveElement(element, elementsBundleBuilder.build().toByteArray())
          element.eChildren.toSeq.sortBy(_.eId.name) // simplify the debugging with sortBy
        case None =>
          log.debug("skip freeze element " + element)
          Seq()
      }
    }
    freezeWorker(saveElement, filter, saved.flatten: _*)
  }

}

object ProtobufSerialization extends Loggable {
  /** Convert Protocol Buffers data to the element */
  def from(data: Array[Byte]): Option[Element.Generic] =
    unpackElement(ElementProtos.Element.parseFrom(data))
  /** Convert the current element without children to Protocol Buffers data */
  def to(element: Element.Generic): Array[Byte] =
    packElement(element.eCopy(List())).toByteArray()

  /**
   * Pack Axis[T] to CoordinateProtos.Coordinate.Axis
   */
  def packAxis[T <: AnyRef with java.io.Serializable](axis: Axis[T])(implicit m: Manifest[T]): CoordinateProtos.Coordinate.Axis = {
    val builder = CoordinateProtos.Coordinate.Axis.newBuilder()
    DSLType.classSymbolMap.get(axis.m.runtimeClass) match {
      case Some(typeSymbol) =>
        DSLType.convertToString(typeSymbol, axis.value) match {
          case Some(valueData) =>
            builder.setId(axis.id.name).
              setType(typeSymbol.name).
              setValue(valueData)
          case None =>
            log.error("unable to convert axis value " + axis.value)
        }
      case None =>
        log.error("unable to convert axis with class %s, suitable type symbol not found".format(axis.m.runtimeClass))
    }
    builder.build()
  }
  /**
   * Pack Context to ContextProtos.Context
   */
  def packContext(context: Context): ContextProtos.Context = {
    val builder = ContextProtos.Context.newBuilder().
      setContainer(packReference(context.container))
    context.digest.foreach(builder.setDigest)
    context.file.foreach(file => builder.setFile(file.getAbsolutePath()))
    context.line.foreach(builder.setLine)
    builder.build()
  }
  /**
   * Pack Coordinate to CoordinateProtos.Coordinate
   */
  def packCoordinate(coordinate: Coordinate): CoordinateProtos.Coordinate = {
    val builder = CoordinateProtos.Coordinate.newBuilder()
    coordinate.coordinate.foreach(axis => builder.addAxis(packAxis(axis)))
    builder.build()
  }
  /**
   * Pack Element.Generic to ElementProtos.Element
   */
  def packElement(element: Element.Generic): ElementProtos.Element =
    ElementProtos.Element.newBuilder().
      setType(element.getClass().getName()).
      setStash(packStash(element.eStash)).
      build()
  /**
   * Pack Element.Generic to ElementProtos.Element
   */
  def packProperties(property: Stash.Data): Iterable[PropertyProtos.Property.Bundle] = {
    val bundles = mutable.HashMap[Symbol, PropertyProtos.Property.Bundle.Builder]()
    property.keys.foreach { valueID =>
      property(valueID).foreach {
        case (typeSymbol, value) if DSLType.symbols(typeSymbol) =>
          DSLType.convertToString(typeSymbol, value.get) match {
            case Some(valueData) =>
              if (!bundles.isDefinedAt(typeSymbol)) {
                bundles(typeSymbol) = PropertyProtos.Property.Bundle.newBuilder()
                bundles(typeSymbol).setType(typeSymbol.name)
              }
              bundles(typeSymbol).addProperty(PropertyProtos.Property.newBuilder().
                setContext(packContext(value.context)).
                setData(valueData).
                setId(valueID.name).
                setStatic(value.isInstanceOf[Value.Static[_]]).
                build())
            case None =>
              log.error("unable to convert value " + value)
          }
        case (typeSymbol, value) =>
          log.error("unable to convert properties with symbol %s, suitable type not found".format(typeSymbol))
      }
    }
    bundles.values.map(_.build())
  }
  /**
   * Pack Reference to ReferenceProtos.Reference
   */
  def packReference(reference: Reference): ReferenceProtos.Reference = {
    ReferenceProtos.Reference.newBuilder().
      setCoordinate(packCoordinate(reference.coordinate)).
      setOrigin(reference.origin.name).
      setUniqueHi(reference.unique.getMostSignificantBits()).
      setUniqueLo(reference.unique.getLeastSignificantBits()).
      build()
  }
  /**
   * Pack Stash to StashProtos.Stash
   */
  def packStash(stash: Stash): StashProtos.Stash = {
    val builder = StashProtos.Stash.newBuilder().
      setType(stash.getClass().getName()).
      setContext(packContext(stash.context)).
      setCoordinate(packCoordinate(stash.coordinate)).
      setCreatedHi(stash.created.milliseconds).
      setCreatedLo(stash.created.nanoShift).
      setId(stash.id.name).
      setModifiedHi(stash.modified.milliseconds).
      setModifiedLo(stash.modified.nanoShift).
      setScopeClass(stash.scope.getClass.getName()).
      setScopeName(stash.scope.modificator.name).
      setUniqueHi(stash.unique.getMostSignificantBits()).
      setUniqueLo(stash.unique.getLeastSignificantBits())
    packProperties(stash.property).foreach(builder.addProperty)
    builder.build()
  }
  def unpackAxis(proto: CoordinateProtos.Coordinate.Axis): Option[Axis[_ <: AnyRef with java.io.Serializable]] = {
    val typeSymbol = Symbol(proto.getType())
    DSLType.convertFromString(typeSymbol, proto.getValue()).map(deserializedValue =>
      Axis(Symbol(proto.getId()), deserializedValue)(Manifest.classType(DSLType.symbolClassMap(typeSymbol))))
  }
  def unpackContext(proto: ContextProtos.Context): Option[Context] = {
    val context = for {
      container <- unpackReference(proto.getContainer())
      file = if (proto.hasFile()) Some(new File(proto.getFile())) else None
      line = if (proto.hasLine()) Some(proto.getLine()) else None
      digest = if (proto.hasDigest()) Some(proto.getDigest()) else None
    } yield Context(container, file, line, digest)
    if (context.isEmpty)
      log.error("unable to unpack context")
    context
  }
  def unpackCoordinate(proto: CoordinateProtos.Coordinate): Option[Coordinate] = {
    val axes = proto.getAxisList().map(unpackAxis)
    if (axes.forall(_.nonEmpty)) {
      Some(Coordinate(axes.flatten: _*))
    } else {
      log.error("unable to unpack axes")
      None
    }
  }
  def unpackElement(proto: ElementProtos.Element): Option[Element.Generic] = try {
    val element = for {
      stash <- unpackStash(proto.getStash())
      elementClass = Class.forName(proto.getType())
      elementCtor = elementClass.getConstructor(stash.getClass())
    } yield elementCtor.newInstance(stash).asInstanceOf[Element.Generic]
    if (element.isEmpty)
      log.error("unable to unpack element")
    element
  } catch {
    // catch all throwables, return None if any
    case e: Throwable =>
      log.error("unable to unpack element: " + e)
      None
  }
  def unpackProperties(list: java.util.List[PropertyProtos.Property.Bundle]): org.digimead.tabuddy.model.element.Stash.Data = {
    val property = new org.digimead.tabuddy.model.element.Stash.Data
    list.foreach { protoBundle =>
      protoBundle.getPropertyList().foreach { protoProperty =>
        val result = for {
          context <- unpackContext(protoProperty.getContext())
          typeSymbol = Symbol(protoBundle.getType())
          valueID = Symbol(protoProperty.getId())
        } if (DSLType.symbols(typeSymbol)) {
          if (!property.isDefinedAt(valueID))
            property(valueID) = new mutable.HashMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]] with mutable.SynchronizedMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]]
          DSLType.convertFromString(typeSymbol, protoProperty.getData()) match {
            case Some(data) if protoProperty.getStatic() =>
              property(valueID)(typeSymbol) = new Value.Static(data, context)(Manifest.classType(DSLType.symbolClassMap(typeSymbol)))
            case Some(data) =>
              property(valueID)(typeSymbol) = new Value.Dynamic(() => data, context)(Manifest.classType(DSLType.symbolClassMap(typeSymbol)))
            case None =>
              log.error("unable to unpack value '%s=%s".format(protoProperty.getId(), protoProperty.getData()))
          }
        } else {
          log.error("unable to unpack property %s with unknown symbol %s".format(valueID, typeSymbol))
        }
      }
    }
    property
  }
  def unpackReference(proto: ReferenceProtos.Reference): Option[Reference] = {
    val reference = for {
      coordinate <- unpackCoordinate(proto.getCoordinate())
    } yield Reference(Symbol(proto.getOrigin()),
      new UUID(proto.getUniqueHi(), proto.getUniqueLo()), coordinate)
    if (reference.isEmpty)
      log.error("unable to unpack reference")
    reference
  }
  def unpackStash(proto: StashProtos.Stash): Option[Stash] = try {
    val stash = for {
      context <- unpackContext(proto.getContext())
      coordinate <- unpackCoordinate(proto.getCoordinate())
      created = Element.Timestamp(proto.getCreatedHi(), proto.getCreatedLo())
      id = Symbol(proto.getId())
      scopeClassName = proto.getScopeClass()
      scopeName = proto.getScopeName()
      modified = Element.Timestamp(proto.getModifiedHi(), proto.getModifiedLo())
      unique = new UUID(proto.getUniqueHi(), proto.getUniqueLo())
      properies = unpackProperties(proto.getPropertyList())
    } yield {
      val scopeClass = Class.forName(scopeClassName)
      val scopeCtor = scopeClass.getConstructor(classOf[Symbol])
      val scope = scopeCtor.newInstance(Symbol(scopeName)).asInstanceOf[Element.Scope]
      val stashClass = Class.forName(proto.getType())
      val stashCtor = stashClass.getConstructor(
        classOf[Context],
        classOf[Coordinate],
        classOf[Element.Timestamp],
        classOf[Symbol],
        classOf[Element.Timestamp],
        classOf[Element.Scope],
        classOf[UUID],
        classOf[org.digimead.tabuddy.model.element.Stash.Data])
      val stash = stashCtor.newInstance(context, coordinate, created, id, modified, scope, unique, properies).asInstanceOf[Stash]
      assert(stash.scope.getClass().getName() == scopeClassName, "Incorrect scope class: got %s, expected %s".format(stash.scope.getClass().getName(), scopeClassName))
      stash
    }
    if (stash.isEmpty)
      log.error("unable to unpack stash")
    stash
  } catch {
    // catch all throwables, return None if any
    case e: Throwable =>
      log.error("unable to unpack stash: " + e)
      None
  }
}
*/