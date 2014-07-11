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
import org.digimead.digi.lib.log.api.XLoggable
import org.digimead.tabuddy.model.element.{ Element, Stash }
import org.digimead.tabuddy.model.graph.{ ElementBox, Node }
import org.digimead.tabuddy.model.serialization.transport.Transport
import org.digimead.tabuddy.model.serialization.yaml.Optional

class YAMLSerialization extends Mechanism with XLoggable {
  /** Identifier of the serialization mechanism. */
  val identifier = YAMLSerialization.Identifier

  /**
   * Load element.
   *
   * @param elementBox element to load
   * @param transport serialization transport
   * @param sData serialization data with parameters
   *
   * @return element
   */
  def load[A <: Element](elementBox: ElementBox[A], transport: Transport, sData: SData)(implicit m: Manifest[A]): A = {
    if (m.runtimeClass == classOf[Nothing])
      throw new IllegalArgumentException("Element type is undefined.")
    val ancestors = elementBox.node.safeRead(node ⇒ node.ancestors.reverse)
    val elementContainerURI = transport.getSubElementURI(ancestors, elementBox.elementUniqueId, elementBox.modified, sData)
    val elementURI = transport.append(elementContainerURI, transport.elementResourceName + "." + YAMLSerialization.Identifier.extension.name)
    val optionalURI = transport.append(elementContainerURI, transport.optionalResourceName + "." + YAMLSerialization.Identifier.extension.name)
    log.debug(s"Load ${elementBox} from ${elementURI}.")
    val elementContent = transport.read(Serialization.inner.encode(elementURI, sData), sData)
    log.debug(s"Load optional ${elementBox} from ${optionalURI}.")
    val optionalContent = transport.read(Serialization.inner.encode(optionalURI, sData), sData)
    val optionalYAML = new String(optionalContent, io.Codec.UTF8.charSet)
    val optional = YAMLSerialization.wrapper(yaml.YAML.block.loadAs(optionalYAML, classOf[yaml.Optional]).asInstanceOf[yaml.Optional], optionalYAML)
    Serialization.stash.set(optional)
    val stashYAML = new String(elementContent, io.Codec.UTF8.charSet)
    val stash = YAMLSerialization.wrapper(yaml.YAML.block.loadAs(stashYAML, classOf[Stash.Like]).asInstanceOf[A#StashType], stashYAML)
    Element(elementBox, stash)
  }
  /**
   * Save element.
   *
   * @param elementBox element to save
   * @param transport serialization transport
   * @param sData serialization data with parameters
   */
  def save[A <: Element](elementBox: ElementBox[A], transport: Transport, sData: SData) = {
    val ancestors = elementBox.node.safeRead(node ⇒ node.ancestors.reverse)
    val elementContainerURI = transport.getSubElementURI(ancestors, elementBox.elementUniqueId, elementBox.modified, sData)
    val elementURI = transport.append(elementContainerURI, transport.elementResourceName + "." + YAMLSerialization.Identifier.extension.name)
    val optionalURI = transport.append(elementContainerURI, transport.optionalResourceName + "." + YAMLSerialization.Identifier.extension.name)
    val optional = yaml.Optional.getOptional(elementBox.e.eStash)
    log.debug(s"Save ${elementBox} to ${elementURI}.")
    transport.write(Serialization.inner.encode(elementURI, sData),
      YAMLSerialization.wrapper(yaml.YAML.block.dump(elementBox.e.eStash).getBytes(io.Codec.UTF8.charSet), elementBox.e.eStash), sData)
    transport.writeTimestamp(elementURI, sData)
    log.debug(s"Save optional ${elementBox} to ${optionalURI}.")
    transport.write(Serialization.inner.encode(optionalURI, sData),
      YAMLSerialization.wrapper(yaml.YAML.block.dump(optional).getBytes(io.Codec.UTF8.charSet), optional), sData)
    transport.writeTimestamp(optionalURI, sData)
  }
}

/*

  SnakeYAML isn't thread safe. Even different instances.

  org.yaml.snakeyaml.parser.ParserException: null; expected '<document start>', but found Scalar;  in 'string', line 1, column 15:
    created: 143673A05FEms.671FF263ns
                  ^
  at org.yaml.snakeyaml.parser.ParserImpl$ParseDocumentStart.produce(ParserImpl.java:225)
  at org.yaml.snakeyaml.parser.ParserImpl.peekEvent(ParserImpl.java:158)
  at org.yaml.snakeyaml.parser.ParserImpl.checkEvent(ParserImpl.java:143)
  at org.yaml.snakeyaml.composer.Composer.getSingleNode(Composer.java:108)
  at org.yaml.snakeyaml.constructor.BaseConstructor.getSingleData(BaseConstructor.java:120)
  at org.yaml.snakeyaml.Yaml.loadFromReader(Yaml.java:481)
  at org.yaml.snakeyaml.Yaml.loadAs(Yaml.java:458)
  at org.digimead.tabuddy.model.serialization.YAMLSerialization.load(YAMLSerialization.scala:56)

  org.yaml.snakeyaml.parser.ParserException: null; expected '<document start>', but found Scalar;  in 'string', line 1, column 19:
    - - Note
            ^
  at org.yaml.snakeyaml.parser.ParserImpl$ParseDocumentStart.produce(ParserImpl.java:225)
  at org.yaml.snakeyaml.parser.ParserImpl.peekEvent(ParserImpl.java:158)
  at org.yaml.snakeyaml.parser.ParserImpl.checkEvent(ParserImpl.java:143)
  at org.yaml.snakeyaml.composer.Composer.getSingleNode(Composer.java:108)
  at org.yaml.snakeyaml.constructor.BaseConstructor.getSingleData(BaseConstructor.java:120)
  at org.yaml.snakeyaml.Yaml.loadFromReader(Yaml.java:481)
  at org.yaml.snakeyaml.Yaml.loadAs(Yaml.java:458)

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

  ... and huge pack of others

  :-( Shit. Wrong library. Replacement required.

*/

object YAMLSerialization extends XLoggable {
  /** SnakeYAML is unstable. */
  protected val globalLock = new Object()

  /**
   * SnakeYAML wrapper that make operations thread safe
   */
  def wrapper[T](f: ⇒ T, from: Any) = globalLock.synchronized {
    try f catch {
      case e: Throwable ⇒
        log.error("YAML error at: --->" + from + "<---", e)
        throw e
    }
  }

  /**
   * YAMLSerialization identifier.
   */
  object Identifier extends Serialization.Identifier {
    /** Serialization description. */
    val description = "a human-readable data serialization format"
    /** File extension. */
    val extension = 'yaml
  }
}
