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

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.ObjectStreamClass
import java.util.UUID

import scala.annotation.tailrec
import scala.collection.mutable

import org.digimead.digi.lib.log.logger.RichLogger.rich2slf4j
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Stash

class BuiltinSerialization extends Serialization[Array[Byte]] {
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
        val bais = new ByteArrayInputStream(data.get)
        val in = new BuiltinSerialization.CustomObjectInputStream(bais)
        filter(in.readObject().asInstanceOf[Element.Generic]).foreach(element => hash(element.eUnique) = element)
        in.close()
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
            if (element.isInstanceOf[Model.Interface[_]]) {
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
        case Some(serialized) =>
          val baos = new ByteArrayOutputStream()
          val out = new ObjectOutputStream(baos)
          out.writeObject(serialized)
          saveElement(element, baos.toByteArray())
          baos.close()
          element.eChildren.toSeq.sortBy(_.eId.name) // simplify the debugging with sortBy
        case None =>
          log.debug("skip freeze element " + element)
          Seq()
      }
    }
    freezeWorker(saveElement, filter, saved.flatten: _*)
  }
}

object BuiltinSerialization {
  /** Convert binary data to the element */
  def from(data: Array[Byte]): Option[Element.Generic] = {
    val bais = new ByteArrayInputStream(data)
    val in = new CustomObjectInputStream(bais)
    val element = in.readObject().asInstanceOf[Element.Generic]
    in.close()
    Option(element)
  }
  /** Convert the current element without children to binary data */
  def to(element: Element.Generic): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(baos)
    out.writeObject(element.eCopy(List()))
    val data = baos.toByteArray()
    baos.close()
    data
  }
  /**
   * ObjectInputStream helper that try to load classes from thread ContextClassLoader
   */
  class CustomObjectInputStream(in: InputStream) extends ObjectInputStream(in) {
    override def resolveClass(desc: ObjectStreamClass): Class[_] = try {
      val currentTccl = Thread.currentThread.getContextClassLoader()
      return currentTccl.loadClass(desc.getName())
    } catch {
      case e: Exception =>
        super.resolveClass(desc)
    }
  }
}
