/**
 * This file is part of the TABuddy project.
 * Copyright (c) 2012-2013 Alexey Aksenov ezh@ezh.msk.ru
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Global License version 3
 * as published by the Free Software Foundation with the addition of the
 * following permission added to Section 15 as permitted in Section 7(a):
 * FOR ANY PART OF THE COVERED WORK IN WHICH THE COPYRIGHT IS OWNED
 * BY Limited Liability Company «MEZHGALAKTICHESKIJ TORGOVYJ ALIANS»,
 * Limited Liability Company «MEZHGALAKTICHESKIJ TORGOVYJ ALIANS» DISCLAIMS
 * THE WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Global License for more details.
 * You should have received a copy of the GNU Affero General Global License
 * along with this program; if not, see http://www.gnu.org/licenses or write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA, 02110-1301 USA, or download the license from the following URL:
 * http://www.gnu.org/licenses/agpl.html
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Global License.
 *
 * In accordance with Section 7(b) of the GNU Affero General Global License,
 * you must retain the producer line in every report, form or document
 * that is created or manipulated using TABuddy.
 *
 * You can be released from the requirements of the license by purchasing
 * a commercial license. Buying such a license is mandatory as soon as you
 * develop commercial activities involving the TABuddy software without
 * disclosing the source code of your own applications.
 * These activities include: offering paid services to customers,
 * serving files in a web or/and network application,
 * shipping TABuddy with a closed source product.
 *
 * For more information, please contact Digimead Team at this
 * address: ezh@ezh.msk.ru
 */

package org.digimead.tabuddy.model.serialization

import java.io.File
import java.util.UUID

import scala.Option.option2Iterable
import scala.annotation.tailrec
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable

import org.digimead.digi.lib.log.logger.RichLogger.rich2slf4j
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
import org.digimead.tabuddy.model.element.Context
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Element.Timestamp
import org.digimead.tabuddy.model.element.Reference
import org.digimead.tabuddy.model.element.Stash
import org.digimead.tabuddy.model.element.Stash.Data
import org.digimead.tabuddy.model.element.Value

class ProtobufSerialization extends Serialization[Array[Byte]] {
  /**
   * Load elements from Iterable[Array[Byte]] with loadElement().
   * Filter/adjust loaded element with filter()
   * Return deserialized element.
   */
  def acquire[A <: Element[B], B <: Stash](loadElement: () => Option[Array[Byte]],
    filter: (Element.Generic) => Option[Element.Generic] = filterAccept)(implicit ma: Manifest[A], mb: Manifest[B]): Option[A] = {
    var hash = mutable.HashMap[UUID, Element.Generic]()
    // load elements
    var data = loadElement()
    while (data.nonEmpty) {
      try {
        val protoElements = ElementProtos.Element.Bundle.parseFrom(data.get)
        // unpack elements
        protoElements.getElementList().map { proto =>
          unpackElement(proto) match {
            case Some(element) =>
              filter(element).foreach(element => hash(element.eUnique) = element)
            case None =>
              log.error("unable to unpack element " + proto)
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
        model.eStash.model = Some(model.asInstanceOf[Model.Generic])
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
          elementsBundleBuilder.addElement(packElement(filtered))
          saveElement(element, elementsBundleBuilder.build().toByteArray())
          element.eChildren.toSeq.sortBy(_.eId.name) // simplify the debugging with sortBy
        case None =>
          log.debug("skip freeze element " + element)
          Seq()
      }
    }
    freezeWorker(saveElement, filter, saved.flatten: _*)
  }
  /**
   * Pack Axis[T] to CoordinateProtos.Coordinate.Axis
   */
  protected def packAxis[T <: AnyRef with java.io.Serializable](axis: Axis[T])(implicit m: Manifest[T]): CoordinateProtos.Coordinate.Axis = {
    CoordinateProtos.Coordinate.Axis.newBuilder().
      setId(axis.id.name).
      setType(m.runtimeClass.getName()).
      setValue(String.valueOf(axis.value)).
      build()
  }
  /**
   * Pack Context to ContextProtos.Context
   */
  protected def packContext(context: Context): ContextProtos.Context = {
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
  protected def packCoordinate(coordinate: Coordinate): CoordinateProtos.Coordinate = {
    val builder = CoordinateProtos.Coordinate.newBuilder()
    coordinate.coordinate.foreach(axis => builder.addAxis(packAxis(axis)))
    builder.build()
  }
  /**
   * Pack Element.Generic to ElementProtos.Element
   */
  protected def packElement(element: Element.Generic): ElementProtos.Element =
    ElementProtos.Element.newBuilder().
      setType(element.getClass().getName()).
      setStash(packStash(element.eStash)).
      build()
  /**
   * Pack Element.Generic to ElementProtos.Element
   */
  protected def packProperties(property: Stash.Data): Iterable[PropertyProtos.Property.Bundle] = {
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
                setStatic(valueData.isInstanceOf[Value.Static[_]]).
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
  protected def packReference(reference: Reference): ReferenceProtos.Reference = {
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
  protected def packStash(stash: Stash): StashProtos.Stash = {
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
  protected def unpackAxis(proto: CoordinateProtos.Coordinate.Axis): Option[Axis[_ <: AnyRef with java.io.Serializable]] = {
    DSLType.convertFromString(Symbol(proto.getType()), proto.getValue()).map(value =>
      Axis(Symbol(proto.getId()), proto.getValue()))
  }
  protected def unpackContext(proto: ContextProtos.Context): Option[Context] = {
    val context = for {
      container <- unpackReference(proto.getContainer())
      file = if (proto.hasFile()) Some(new File(proto.getFile())) else None
      line = if (proto.hasLine()) Some(proto.getLine()) else None
      digest = if (proto.hasDigest()) Some(proto.getDigest()) else None
    } yield Context(container, file, line, digest)
    if (context.isEmpty)
      log.debug("unable to unpack context")
    context
  }
  protected def unpackCoordinate(proto: CoordinateProtos.Coordinate): Option[Coordinate] = {
    val axes = proto.getAxisList().map(unpackAxis)
    if (axes.forall(_.nonEmpty)) {
      Some(Coordinate(axes.flatten: _*))
    } else {
      log.debug("unable to unpack axes")
      None
    }
  }
  protected def unpackElement(proto: ElementProtos.Element): Option[Element.Generic] = try {
    val element = for {
      stash <- unpackStash(proto.getStash())
      elementClass = Class.forName(proto.getType())
      elementCtor = elementClass.getConstructor(stash.getClass())
    } yield elementCtor.newInstance(stash).asInstanceOf[Element.Generic]
    if (element.isEmpty)
      log.debug("unable to unpack element")
    element
  } catch {
    // catch all throwables, return None if any
    case e: Throwable =>
      log.debug("unable to unpack element: " + e)
      None
  }
  protected def unpackProperties(list: java.util.List[PropertyProtos.Property.Bundle]): org.digimead.tabuddy.model.element.Stash.Data = {
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
  protected def unpackReference(proto: ReferenceProtos.Reference): Option[Reference] = {
    val reference = for {
      coordinate <- unpackCoordinate(proto.getCoordinate())
    } yield Reference(Symbol(proto.getOrigin()),
      new UUID(proto.getUniqueHi(), proto.getUniqueLo()), coordinate)
    if (reference.isEmpty)
      log.debug("unable to unpack reference")
    reference
  }
  protected def unpackStash(proto: StashProtos.Stash): Option[Stash] = try {
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
        classOf[Element.Scope],
        classOf[UUID],
        classOf[org.digimead.tabuddy.model.element.Stash.Data])
      val stash = stashCtor.newInstance(context, coordinate, created, id, scope, unique, properies).asInstanceOf[Stash]
      stash.modified = modified
      assert(stash.scope.getClass().getName() == scopeClassName, "Incorrect scope class: got %s, expected %s".format(stash.scope.getClass().getName(), scopeClassName))
      stash
    }
    if (stash.isEmpty)
      log.debug("unable to unpack stash")
    stash
  } catch {
    // catch all throwables, return None if any
    case e: Throwable =>
      log.debug("unable to unpack stash: " + e)
      None
  }
}
