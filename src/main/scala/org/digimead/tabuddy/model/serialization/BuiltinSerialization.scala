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
import java.net.URI

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.serialization.transport.Transport

class BuiltinSerialization extends Mechanism with Loggable {
  /** Identifier of the serialization mechanism. */
  val identifier = BuiltinSerialization.Identifier

  /**
   * Load element.
   *
   * @param elementBox box of the loaded element
   * @param storageURI storage URI
   * @param transport serialization transport
   *
   * @return element
   */
  def load[A <: Element](elementBox: ElementBox[A], storageURI: URI, transport: Transport)(implicit m: Manifest[A]): A = {
    log.debug(s"Load ${elementBox}.")
    if (m.runtimeClass == classOf[Nothing])
      throw new IllegalArgumentException("Element type is undefined.")
    val ancestorsNSelf = elementBox.node.safeRead(node ⇒ node.ancestors.reverse :+ node)
    val elementContainerURI = transport.acquireElementLocation(ancestorsNSelf, elementBox, storageURI)
    val elementURI = new URI(elementContainerURI + "/" + transport.elementResourceName + "." + BuiltinSerialization.Identifier.extension)
    val elementContent = transport.read(elementURI)
    Serialization.stash.set(elementBox)
    val bais = new ByteArrayInputStream(elementContent)
    val in = new BuiltinSerialization.CustomObjectInputStream(bais)
    val element = in.readObject().asInstanceOf[Element]
    in.close()
    element.asInstanceOf[A]
  }
  /**
   * Save element.
   *
   * @param element element to save
   * @param transport serialization transport
   * @param storageURI storage URI
   * @param ancestorsNSelf sequence of ancestors
   */
  def save(ancestorsNSelf: Seq[Node[_ <: Element]], element: Element, storageURI: URI, transport: Transport) {
    val elementContainerURI = transport.acquireElementLocation(ancestorsNSelf, element.eBox, storageURI)
    val elementURI = new URI(elementContainerURI + "/" + transport.elementResourceName + "." + BuiltinSerialization.Identifier.extension)
    log.debug(s"Save ${element.eBox} to ${elementURI}.")
    val baos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(baos)
    out.writeObject(element)
    baos.close()
    transport.write(baos.toByteArray(), elementURI)
  }
}

object BuiltinSerialization {
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
