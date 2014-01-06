/**
 * TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2012-2014 Alexey Aksenov ezh@ezh.msk.ru
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
  def load[A <: Element](elementBox: ElementBox[A], storageURI: URI, transport: Transport)(implicit m: Manifest[A]): A = YAMLSerialization.globalLock.synchronized  {
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
    val optional = yaml.YAML.block.loadAs(new String(optionalContent, io.Codec.UTF8.charSet), classOf[yaml.Optional]).asInstanceOf[yaml.Optional]
    Serialization.stash.set(optional)
    val stash = yaml.YAML.block.loadAs(new String(elementContent, io.Codec.UTF8.charSet), classOf[Stash.Like]).asInstanceOf[A#StashType]
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
  def save(ancestorsNSelf: Seq[Node[_ <: Element]], element: Element, storageURI: URI, transport: Transport) = YAMLSerialization.globalLock.synchronized {
    val elementContainerURI = transport.acquireElementLocation(ancestorsNSelf, element.eBox, storageURI)
    val elementURI = transport.append(elementContainerURI, transport.elementResourceName + "." + YAMLSerialization.Identifier.extension)
    val optionalURI = transport.append(elementContainerURI, transport.optionalResourceName + "." + YAMLSerialization.Identifier.extension)
    val optional = yaml.Optional.getOptional(element.eStash)
    log.debug(s"Save ${element.eBox} to ${elementURI}.")
    transport.write(yaml.YAML.block.dump(element.eStash).getBytes(io.Codec.UTF8.charSet), elementURI)
    log.debug(s"Save optional ${element.eBox} to ${optionalURI}.")
    transport.write(yaml.YAML.block.dump(optional).getBytes(io.Codec.UTF8.charSet), optionalURI)
  }
}

/*

  java.util.NoSuchElementException:
  at java.util.LinkedHashMap$LinkedHashIterator.nextEntry(LinkedHashMap.java:375)
  at java.util.LinkedHashMap$ValueIterator.next(LinkedHashMap.java:388)
  at org.yaml.snakeyaml.scanner.ScannerImpl.nextPossibleSimpleKey(ScannerImpl.java:436)
  at org.yaml.snakeyaml.scanner.ScannerImpl.needMoreTokens(ScannerImpl.java:281)
  at org.yaml.snakeyaml.scanner.ScannerImpl.checkToken(ScannerImpl.java:225)
  at org.yaml.snakeyaml.parser.ParserImpl$ParseImplicitDocumentStart.produce(ParserImpl.java:195)
  at org.yaml.snakeyaml.parser.ParserImpl.peekEvent(ParserImpl.java:158)
  at org.yaml.snakeyaml.parser.ParserImpl.checkEvent(ParserImpl.java:143)
  at org.yaml.snakeyaml.composer.Composer.getSingleNode(Composer.java:104)
  at org.yaml.snakeyaml.constructor.BaseConstructor.getSingleData(BaseConstructor.java:120)
  at org.yaml.snakeyaml.Yaml.loadFromReader(Yaml.java:481)
  at org.yaml.snakeyaml.Yaml.loadAs(Yaml.java:458)

  java.util.ConcurrentModificationException:
  at java.util.LinkedHashMap$LinkedHashIterator.nextEntry(LinkedHashMap.java:373)
  at java.util.LinkedHashMap$ValueIterator.next(LinkedHashMap.java:388)
  at org.yaml.snakeyaml.scanner.ScannerImpl.stalePossibleSimpleKeys(ScannerImpl.java:455)
  at org.yaml.snakeyaml.scanner.ScannerImpl.fetchMoreTokens(ScannerImpl.java:291)
  at org.yaml.snakeyaml.scanner.ScannerImpl.checkToken(ScannerImpl.java:226)
  at org.yaml.snakeyaml.parser.ParserImpl$ParseImplicitDocumentStart.produce(ParserImpl.java:195)
  at org.yaml.snakeyaml.parser.ParserImpl.peekEvent(ParserImpl.java:158)
  at org.yaml.snakeyaml.parser.ParserImpl.checkEvent(ParserImpl.java:143)
  at org.yaml.snakeyaml.composer.Composer.getSingleNode(Composer.java:104)
  at org.yaml.snakeyaml.constructor.BaseConstructor.getSingleData(BaseConstructor.java:120)

  java.util.ConcurrentModificationException:
  at java.util.AbstractList$Itr.checkForComodification(AbstractList.java:372)
  at java.util.AbstractList$Itr.next(AbstractList.java:343)
  at org.yaml.snakeyaml.serializer.Serializer.anchorNode(Serializer.java:142)
  at org.yaml.snakeyaml.serializer.Serializer.anchorNode(Serializer.java:145)
  at org.yaml.snakeyaml.serializer.Serializer.serialize(Serializer.java:108)
  at org.yaml.snakeyaml.Yaml.dumpAll(Yaml.java:272)
  at org.yaml.snakeyaml.Yaml.dumpAll(Yaml.java:262)
  at org.yaml.snakeyaml.Yaml.dumpAll(Yaml.java:234)
  at org.yaml.snakeyaml.Yaml.dump(Yaml.java:209)
  at org.digimead.tabuddy.model.serialization.Serialization.graphDescriptorToYAML(Serialization.scala:340)

  :-( Shit. Wrong library. Replacement required.

*/

object YAMLSerialization extends Loggable {
  /** SnakeYAML is unstable. */
  val globalLock = new Object()
  /** YAMLSerialization identifier. */
  object Identifier extends Serialization.Identifier { val extension = "yaml" }
}
