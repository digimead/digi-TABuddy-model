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

import java.util.{ UUID ⇒ JUUID }

import org.yaml.snakeyaml.constructor.AbstractConstruct
import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.ScalarNode
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.representer.{ Represent ⇒ YAMLRepresent }

/**
 * YAML de/serialization helper for java.util.UUID.
 */
object UUID {
  val tag = new Tag(Tag.PREFIX + "uuid")

  /** Convert UUID to string. */
  def dump(arg: JUUID): String = YAML.dump(arg).trim
  /** Convert string to UUID. */
  def load(arg: String): JUUID = YAML.loadAs(arg, classOf[JUUID]).asInstanceOf[JUUID]

  class Construct extends AbstractConstruct {
    YAML.constructor.getYAMLConstructors.put(UUID.tag, this)
    YAML.constructor.getYAMLConstructors.put(new Tag(classOf[JUUID]), this)

    override def construct(node: Node): AnyRef = {
      val value = node.asInstanceOf[ScalarNode].getValue()
      if (value == null)
        return null
      JUUID.fromString(value)
    }
  }
  class Represent extends YAMLRepresent {
    YAML.representer.getMultiRepresenters.put(classOf[JUUID], this)

    def representData(data: AnyRef): Node =
      YAML.representer.representScalar(Tag.STR, data.asInstanceOf[JUUID].toString(), null)
  }
}
