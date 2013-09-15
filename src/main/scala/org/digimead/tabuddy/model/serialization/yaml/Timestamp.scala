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
import org.yaml.snakeyaml.DumperOptions
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.AbstractConstruct
import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.ScalarNode
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.representer.Represent

/**
 * YAML de/serialization helper for Element.Timestamp.
 */
object Timestamp {
  val tag = new Tag(Tag.PREFIX + "ts")

  /** Convert string to Element.Timestamp. */
  def load(arg: String): Element.Timestamp = YAML.loadAs(arg, classOf[Element.Timestamp]).asInstanceOf[Element.Timestamp]
  /** Convert Element.Timestamp to string. */
  def dump(arg: Element.Timestamp): String = YAML.dump(arg).trim

  trait Constructor {
    this: YAML.Constructor ⇒
    getYAMLConstructors.put(Timestamp.tag, new ConstructTimestamp())
    getYAMLConstructors.put(new Tag(classOf[Element.Timestamp]), new ConstructTimestamp())

    private class ConstructTimestamp extends AbstractConstruct {
      override def construct(node: Node): AnyRef = {
        val scalar = node.asInstanceOf[ScalarNode]
        val nodeValue = scalar.getValue()
        val left = nodeValue.takeWhile(_ != '.')
        Element.timestamp(java.lang.Long.parseLong(left.substring(0, left.size - 2), 16),
          java.lang.Long.parseLong(nodeValue.substring(left.size + 1, nodeValue.size - 2), 16))
      }
    }
  }
  trait Representer {
    this: YAML.Representer ⇒
    getMultiRepresenters.put(classOf[Element.Timestamp], new RepresentTimestamp())

    class RepresentTimestamp extends Represent {
      def representData(data: AnyRef): Node = representScalar(Tag.STR,
        "%Xms.%Xns".format(data.asInstanceOf[Element.Timestamp].milliseconds, data.asInstanceOf[Element.Timestamp].nanoShift), null)
    }
  }
}
