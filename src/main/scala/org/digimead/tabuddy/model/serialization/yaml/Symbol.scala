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

import org.yaml.snakeyaml.constructor.AbstractConstruct
import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.ScalarNode
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.representer.{ Represent ⇒ YAMLRepresent }
import org.yaml.snakeyaml.error.YAMLException

/**
 * YAML de/serialization helper for scala.Symbol.
 */
object Symbol {
  val tag = new Tag(Tag.PREFIX + "sym")

  /** Convert Symbol to string. */
  def dump(arg: scala.Symbol): String = YAML.dump(arg).trim
  /** Convert string to Symbol. */
  def load(arg: String): scala.Symbol = YAML.loadAs(arg, classOf[scala.Symbol]).asInstanceOf[scala.Symbol]

  class Construct extends AbstractConstruct {
    YAML.constructor.getYAMLConstructors.put(Symbol.tag, this)
    YAML.constructor.getYAMLConstructors.put(new Tag(classOf[scala.Symbol]), this)

    override def construct(node: Node): AnyRef = Symbol.synchronized {
      val value = node.asInstanceOf[ScalarNode].getValue()
      if (value == null)
        throw new YAMLException("Unable to construct symbol from null.")
      scala.Symbol(value)
    }
  }
  class Represent extends YAMLRepresent {
    YAML.representer.getMultiRepresenters.put(classOf[scala.Symbol], this)

    def representData(data: AnyRef): Node =
      YAML.representer.representScalar(Tag.STR, data.asInstanceOf[scala.Symbol].name, null)
  }
}
