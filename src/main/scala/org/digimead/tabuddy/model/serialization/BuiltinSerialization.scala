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

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.ObjectStreamClass
import java.util.UUID
import scala.annotation.tailrec
import scala.collection.mutable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.Node

class BuiltinSerialization extends Serialization {
  /** Identifier of the serialization mechanism. */
  val identifier = BuiltinSerialization.Identifier

  /** Load element. */
  def acquireElement[A <: Element](objectId: UUID, parentNode: Node, from: Array[Byte])(implicit m: Manifest[A]): Option[A] = {
    if (m.runtimeClass == classOf[Nothing])
      throw new IllegalArgumentException("Element type is undefined")
    /*var hash = mutable.HashMap[UUID, Element]()
    // load elements
    var data = loadElement()
    while (data.nonEmpty) {
      try {
        val bais = new ByteArrayInputStream(data.get)
        val in = new BuiltinSerialization.CustomObjectInputStream(bais)
        filter(in.readObject().asInstanceOf[Element]).foreach(element ⇒ hash(element.eNodeId) = element)
        in.close()
      } catch {
        // catch all throwables, return None if any
        case e: Throwable ⇒
          log.error("unable to acuire elements: " + e)
      }
      data = loadElement()
    }
    // build structure
    var rootElements = Seq[Element]()
    hash.foreach {
      case (unique, element) ⇒
        val parent = element.eStash.context.container.unique
        hash.get(parent) match {
          case Some(parentElement) if parentElement == element ⇒
            // parent is cyclic reference
            if (element.isInstanceOf[Model.Like]) {
              // drop all other expectants
              rootElements = Seq(element)
            } else
              log.fatal("detected a cyclic reference inside an unexpected element " + element)
          case Some(parentElement) ⇒
            // parent found
            parentElement.eChildren += element
          case None ⇒
            // parent not found
            element.eAs[A, B].foreach(element ⇒ rootElements = rootElements :+ element)
        }
    }
    // return result
    hash.clear*/
    /*rootElements.find(_.isInstanceOf[Model.Like]) match {
      case Some(model) ⇒
        // return model as expected type
        //model.eStash.model = Some(model.asInstanceOf[Model.Generic])
        model.asInstanceOf[Model.Like].eIndexRebuid()
        model.eAs[A, B]
      case None if rootElements.size == 1 ⇒
        // return other element as expected type
        rootElements.head.eAs[A, B]
      case None if rootElements.isEmpty ⇒
        log.error("there is no root elements detected")
        None
      case None ⇒
        log.error("there are more than one root elements detected: " + rootElements.mkString(","))
        None
    }*/
    None
  }
  /** Save element. */
  def freezeElement(element: Element): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(baos)
    out.writeObject(element)
    baos.close()
    baos.toByteArray()
  }
}

object BuiltinSerialization {
  /** Convert binary data to the element */
  def from(data: Array[Byte]): Option[Element] = {
    val bais = new ByteArrayInputStream(data)
    val in = new CustomObjectInputStream(bais)
    val element = in.readObject().asInstanceOf[Element]
    in.close()
    Option(element)
  }
  /** Convert the current element without children to binary data */
  def to(element: Element): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(baos)
    out.writeObject(element)
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
      case e: Exception ⇒
        super.resolveClass(desc)
    }
  }
  /** BuiltinSerialization identifier. */
  object Identifier extends Serialization.Identifier { val extension = "bcode" }
}
