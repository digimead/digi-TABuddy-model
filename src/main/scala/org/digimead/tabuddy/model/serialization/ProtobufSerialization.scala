/**
 * This file is part of the TABuddy project.
 * Copyright (c) 2012 Alexey Aksenov ezh@ezh.msk.ru
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
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.util.control.ControlThrowable

import org.digimead.digi.lib.log.Loggable
import org.digimead.digi.lib.log.logger.RichLogger.rich2slf4j
import org.digimead.tabuddy.model.ContextProtos
import org.digimead.tabuddy.model.CoordinateProtos
import org.digimead.tabuddy.model.Element
import org.digimead.tabuddy.model.Element.Context
import org.digimead.tabuddy.model.Element.Coordinate
import org.digimead.tabuddy.model.ElementProtos
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.PropertyProtos
import org.digimead.tabuddy.model.ReferenceProtos
import org.digimead.tabuddy.model.Stash
import org.digimead.tabuddy.model.Stash.Data
import org.digimead.tabuddy.model.StashProtos
import org.digimead.tabuddy.model.Value
import org.digimead.tabuddy.model.dsltype.DSLType
import org.digimead.tabuddy.model.dsltype.DSLType.dsltype2implementation

class ProtobufSerialization extends Loggable {
  /** Load element from Array[Byte]. */
  def acquire[A <: Element.Generic](frozen: Array[Byte]): Option[A] = try {
    log.debug("acuire elements from bundle with size %d byte(s)".format(frozen.length))
    val protoElements = ElementProtos.Element.Bundle.parseFrom(frozen)
    log.debug("%d element(s) extracted".format(protoElements.getElementCount()))
    val elements = new mutable.HashMap[Element.Reference, Element.Generic]
    var head: Option[Element.Generic] = None
    // unpack elements
    protoElements.getElementList().map { proto =>
      unpackElement(proto) match {
        case Some(element) =>
          elements(element.eReference) = element
          if (head.isEmpty)
            head = Some[Element.Generic](element)
        case None =>
          log.error("unable to unpack element " + proto)
          if (head.isEmpty)
            return None
      }
    }
    // bind elements
    elements.values.foreach { element =>
      elements.get(element.eStash.context.container) match {
        case Some(parent) =>
          if (!parent.eq(element))
            parent.elementChildren += element
        case None if Some[Element.Generic](element) != head =>
          log.error("lost parent for " + element)
        case None =>
      }
    }
    // process head, return result
    head flatMap {
      case model: Model.Generic if model.eStash.scope == "Model" =>
        model.eIndexRebuid
        Some(model.asInstanceOf[A])
      case other =>
        Some(other.asInstanceOf[A])

    }
  } catch {
    case e: ControlThrowable => throw e
    case e =>
      log.error("unable to acuire elements: " + e)
      None
  }
  /** Save element to Array[Byte]. */
  def freeze(element: Element.Generic): Array[Byte] = {
    val elementsBundleBuilder = ElementProtos.Element.Bundle.newBuilder()
    val elements = (element +: element.eFilter(_ => true))
    log.debug("freeze %d elements to bundle".format(elements.length))
    elements.foreach(element => elementsBundleBuilder.addElement(packElement(element)))
    val result = elementsBundleBuilder.build().toByteArray()
    log.debug("%s bytes packed to bundle".format(result.length))
    result
  }
  /**
   * Pack Element.Axis[T] to CoordinateProtos.Coordinate.Axis
   */
  protected def packAxis[T <: java.io.Serializable](axis: Element.Axis[T])(implicit m: Manifest[T]): CoordinateProtos.Coordinate.Axis = {
    CoordinateProtos.Coordinate.Axis.newBuilder().
      setId(axis.id.name).
      setType(m.erasure.getName()).
      setValue(String.valueOf(axis.value)).
      build()
  }
  /**
   * Pack Element.Context to ContextProtos.Context
   */
  protected def packContext(context: Element.Context): ContextProtos.Context = {
    val builder = ContextProtos.Context.newBuilder().
      setContainer(packReference(context.container))
    context.digest.foreach(builder.setDigest)
    context.file.foreach(file => builder.setFile(file.getAbsolutePath()))
    context.line.foreach(builder.setLine)
    builder.build()
  }
  /**
   * Pack Element.Coordinate to CoordinateProtos.Coordinate
   */
  protected def packCoordinate(coordinate: Element.Coordinate): CoordinateProtos.Coordinate = {
    val builder = CoordinateProtos.Coordinate.newBuilder()
    coordinate.coordinate.foreach(axis => builder.addAxis(packAxis(axis)))
    builder.build()
  }
  /**
   * Pack Element.Generic to ElementProtos.Element
   */
  protected def packElement(element: Element.Generic): ElementProtos.Element = {
    ElementProtos.Element.newBuilder().
      setType(element.getClass().getName()).
      setStash(packStash(element.eStash)).
      build()
  }
  /**
   * Pack Element.Generic to ElementProtos.Element
   */
  protected def packProperties(property: Stash.Data): Iterable[PropertyProtos.Property.Bundle] =
    property.keys.flatMap { keyType =>
      val properyBundleBuilder = PropertyProtos.Property.Bundle.newBuilder()
      DSLType.getTypeSignature(Class.forName(keyType)) match {
        case Some(signature) =>
          properyBundleBuilder.setType(signature)
          property(keyType).foreach {
            case (valueID, value) =>
              DSLType.save(value.get) match {
                case Some((valueType, valueData)) =>
                  properyBundleBuilder.addProperty(PropertyProtos.Property.newBuilder().
                    setContext(packContext(value.context)).
                    setData(valueData).
                    setId(valueID.name).
                    setStatic(valueData.isInstanceOf[Value.Static[_]]).
                    build())
                case None =>
                  log.error("unable to convert value " + value)
              }
          }
          Some(properyBundleBuilder.build())
        case None =>
          log.error("unable to convert properties with type %s, suitable signature not found".format(keyType))
          None
      }
    }
  /**
   * Pack Element.Reference to ReferenceProtos.Reference
   */
  protected def packReference(reference: Element.Reference): ReferenceProtos.Reference = {
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
      setScope(stash.scope).
      setUniqueHi(stash.unique.getMostSignificantBits()).
      setUniqueLo(stash.unique.getLeastSignificantBits())
    packProperties(stash.property).foreach(builder.addProperty)
    builder.build()
  }
  protected def unpackAxis(proto: CoordinateProtos.Coordinate.Axis): Option[Element.Axis[_ <: java.io.Serializable]] = {
    DSLType.load(proto.getType(), proto.getValue()).map(value =>
      Element.Axis(Symbol(proto.getId()), proto.getValue()))
  }
  protected def unpackContext(proto: ContextProtos.Context): Option[Element.Context] = {
    val context = for {
      container <- unpackReference(proto.getContainer())
      file = if (proto.hasFile()) Some(new File(proto.getFile())) else None
      line = if (proto.hasLine()) Some(proto.getLine()) else None
      digest = if (proto.hasDigest()) Some(proto.getDigest()) else None
    } yield Element.Context(container, file, line, digest)
    if (context.isEmpty)
      log.debug("unable to unpack context")
    context
  }
  protected def unpackCoordinate(proto: CoordinateProtos.Coordinate): Option[Element.Coordinate] = {
    val axes = proto.getAxisList().map(unpackAxis)
    if (axes.forall(_.nonEmpty)) {
      Some(Element.Coordinate(axes.flatten: _*))
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
    case e =>
      log.debug("unable to unpack element: " + e)
      None
  }
  protected def unpackProperties(list: java.util.List[PropertyProtos.Property.Bundle]): org.digimead.tabuddy.model.Stash.Data = {
    val property = new org.digimead.tabuddy.model.Stash.Data
    list.foreach { protoBundle =>
      val valueType = protoBundle.getType()
      DSLType.getTypeClass(valueType) match {
        case Some(typeJVM) =>
          val typeJVMName = typeJVM.getName()
          property(typeJVMName) = new mutable.HashMap[Symbol, Value[_ <: java.io.Serializable]] with mutable.SynchronizedMap[Symbol, Value[_ <: java.io.Serializable]]
          protoBundle.getPropertyList().foreach { protoProperty =>
            val result = for {
              context <- unpackContext(protoProperty.getContext())
              data <- DSLType.load(valueType, protoProperty.getData()).asInstanceOf[Option[java.io.Serializable]]
            } yield if (protoProperty.getStatic())
              property(typeJVMName)(Symbol(protoProperty.getId())) = new Value.Static(data, context)
            else
              property(typeJVMName)(Symbol(protoProperty.getId())) = new Value.Dynamic(() => data, context)
            if (result.isEmpty)
              log.error("unable to unpack value '%s=%s".format(protoProperty.getId(), protoProperty.getData()))
          }
        case None =>
          log.error("unable to unpack value with unknown signature " + valueType)
          None
      }
    }
    property
  }
  protected def unpackReference(proto: ReferenceProtos.Reference): Option[Element.Reference] = {
    val reference = for {
      coordinate <- unpackCoordinate(proto.getCoordinate())
    } yield Element.Reference(Symbol(proto.getOrigin()),
      new UUID(proto.getUniqueHi(), proto.getUniqueLo()), coordinate)
    if (reference.isEmpty)
      log.debug("unable to unpack reference")
    reference
  }
  protected def unpackStash(proto: StashProtos.Stash): Option[Stash] = try {
    val stash = for {
      context <- unpackContext(proto.getContext())
      coordinate <- unpackCoordinate(proto.getCoordinate())
      created = Stash.Timestamp(proto.getCreatedHi(), proto.getCreatedLo())
      id = Symbol(proto.getId())
      modified = Stash.Timestamp(proto.getModifiedHi(), proto.getModifiedLo())
      unique = new UUID(proto.getUniqueHi(), proto.getUniqueLo())
      properies = unpackProperties(proto.getPropertyList())
    } yield {
      val stashClass = Class.forName(proto.getType())
      val stashCtor = this.getClass().getConstructor(
        classOf[Element.Context],
        classOf[Element.Coordinate],
        classOf[Stash.Timestamp],
        classOf[Symbol],
        classOf[UUID],
        classOf[org.digimead.tabuddy.model.Stash.Data])
      val stash = stashCtor.newInstance(context, coordinate, created, id, unique, properies).asInstanceOf[Stash]
      stash.modified = modified
      stash
    }
    if (stash.isEmpty)
      log.debug("unable to unpack stash")
    stash
  } catch {
    case e =>
      log.debug("unable to unpack stash: " + e)
      None
  }
}
