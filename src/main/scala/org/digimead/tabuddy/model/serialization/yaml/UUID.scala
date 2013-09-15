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
import org.yaml.snakeyaml.representer.Represent

/**
 * YAML de/serialization helper for java.util.UUID.
 */
object UUID {
  val tag = new Tag(Tag.PREFIX + "uuid")

  /** Convert string to UUID. */
  def load(arg: String): JUUID = YAML.loadAs(arg, classOf[JUUID]).asInstanceOf[JUUID]
  /** Convert UUID to string. */
  def dump(arg: JUUID): String = YAML.dump(arg).trim

  trait Constructor {
    this: YAML.Constructor ⇒
    getYAMLConstructors.put(UUID.tag, new ConstructUUID())
    getYAMLConstructors.put(new Tag(classOf[JUUID]), new ConstructUUID())

    private class ConstructUUID extends AbstractConstruct {
      override def construct(node: Node): AnyRef = JUUID.fromString(node.asInstanceOf[ScalarNode].getValue())
    }
  }
  trait Representer {
    this: YAML.Representer ⇒
    getMultiRepresenters.put(classOf[JUUID], new RepresentUUID())

    class RepresentUUID extends Represent {
      def representData(data: AnyRef): Node = representScalar(Tag.STR, data.asInstanceOf[JUUID].toString(), null)
    }
  }
}
