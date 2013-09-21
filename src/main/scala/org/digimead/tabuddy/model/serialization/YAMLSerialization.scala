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

import java.net.URI

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Stash
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.serialization.transport.Transport

class YAMLSerialization extends Mechanism with Loggable {
  /** Identifier of the serialization mechanism. */
  val identifier = YAMLSerialization.Identifier

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
    if (m.runtimeClass == classOf[Nothing])
      throw new IllegalArgumentException("Element type is undefined.")
    val ancestorsNSelf = elementBox.node.safeRead(node ⇒ node.ancestors.reverse :+ node)
    val elementContainerURI = transport.acquireElementLocation(ancestorsNSelf, elementBox, storageURI)
    val elementURI = transport.append(elementContainerURI, transport.elementResourceName + "." + YAMLSerialization.Identifier.extension)
    val optionalURI = transport.append(elementContainerURI, transport.optionalResourceName + "." + YAMLSerialization.Identifier.extension)
    log.debug(s"Load ${elementBox} from ${elementURI}.")
    val elementContent = transport.read(elementURI)
    log.debug(s"Load optional ${elementBox} from ${optionalURI}.")
    val optionalContent = transport.read(optionalURI)
    val optional = yaml.YAML.loadAs(new String(optionalContent, io.Codec.UTF8.charSet), classOf[yaml.Optional]).asInstanceOf[yaml.Optional]
    Serialization.stash.set(optional)
    val stash = yaml.YAML.loadAs(new String(elementContent, io.Codec.UTF8.charSet), classOf[Stash.Like]).asInstanceOf[A#StashType]
    Element(elementBox, stash)
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
    val elementURI = transport.append(elementContainerURI, transport.elementResourceName + "." + YAMLSerialization.Identifier.extension)
    val optionalURI = transport.append(elementContainerURI, transport.optionalResourceName + "." + YAMLSerialization.Identifier.extension)
    val optional = yaml.Optional.getOptional(element.eStash)
    log.debug(s"Save ${element.eBox} to ${elementURI}.")
    transport.write(yaml.YAML.dump(element.eStash).getBytes(io.Codec.UTF8.charSet), elementURI)
    log.debug(s"Save optional ${element.eBox} to ${optionalURI}.")
    transport.write(yaml.YAML.dump(optional).getBytes(io.Codec.UTF8.charSet), optionalURI)
  }
}

object YAMLSerialization extends Loggable {

  /** YAMLSerialization identifier. */
  object Identifier extends Serialization.Identifier { val extension = "yaml" }
}
