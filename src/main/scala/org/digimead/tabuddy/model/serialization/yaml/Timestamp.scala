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

package org.digimead.tabuddy.model.serialization.yaml

import org.digimead.tabuddy.model.element.Element
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.AbstractConstruct
import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.ScalarNode
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.representer.{ Represent ⇒ YAMLRepresent }

/**
 * YAML de/serialization helper for Element.Timestamp.
 */
object Timestamp {
  val tag = new Tag(Tag.PREFIX + "ts")

  /** Convert Element.Timestamp to string. */
  def dump(arg: Element.Timestamp): String = YAML.block.dump(arg).trim
  /** Convert string to Element.Timestamp. */
  def load(arg: String): Element.Timestamp = YAML.block.loadAs(arg, classOf[Element.Timestamp]).asInstanceOf[Element.Timestamp]

  class Construct extends AbstractConstruct {
    YAML.constructor.getYAMLConstructors.put(Timestamp.tag, this)
    YAML.constructor.getYAMLConstructors.put(new Tag(classOf[Element.Timestamp]), this)

    override def construct(node: Node): AnyRef = {
      val value = node.asInstanceOf[ScalarNode].getValue()
      if (value == null)
        return null
      val left = value.takeWhile(_ != '.')
      Element.timestamp(java.lang.Long.parseLong(left.substring(0, left.size - 2), 16),
        java.lang.Long.parseLong(value.substring(left.size + 1, value.size - 2), 16))
    }
  }
  class Represent extends YAMLRepresent {
    YAML.representer.getMultiRepresenters.put(classOf[Element.Timestamp], this)

    def representData(data: AnyRef): Node = YAML.representer.representScalar(Tag.STR,
      "%Xms.%Xns".format(data.asInstanceOf[Element.Timestamp].milliseconds, data.asInstanceOf[Element.Timestamp].nanoShift), null)
  }
}
