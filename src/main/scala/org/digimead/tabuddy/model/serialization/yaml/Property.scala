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

import scala.collection.JavaConverters._

import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.SequenceNode
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.representer.{ Represent ⇒ YAMLRepresent }

object Property {
  val tag = new Tag(Tag.PREFIX + "property")

  /** Convert Stash to string. */
  def dump(arg: Wrapper): String = YAML.dump(arg).trim
  /** Convert string to Stash. */
  def load(arg: String): Wrapper = YAML.loadAs(arg, classOf[Wrapper]).asInstanceOf[Wrapper]

  class Construct extends YAML.constructor.ConstructSequence {
    YAML.constructor.getYAMLConstructors.put(Property.tag, this)
    YAML.constructor.getYAMLConstructors.put(new Tag(classOf[Wrapper]), this)

    override def construct(node: Node): AnyRef = node match {
      case snode: SequenceNode ⇒
        YAML.constructor.setTagSafe(snode.getValue().get(0), Symbol.tag)
        YAML.constructor.setTagSafe(snode.getValue().get(1), Symbol.tag)
        val seq = YAML.constructor.constructSequence(snode).asInstanceOf[java.util.ArrayList[AnyRef]]
        Wrapper(seq.get(1).asInstanceOf[scala.Symbol], seq.get(2).asInstanceOf[String], seq.get(0).asInstanceOf[scala.Symbol])
      case node ⇒
        throw new YAMLException(s"Unexpected Coordinate node: ${node}.")
    }
  }
  class Represent extends YAMLRepresent {
    YAML.representer.getMultiRepresenters.put(classOf[Wrapper], this)

    def representData(data: AnyRef): Node = {
      val wrapper = data.asInstanceOf[Wrapper]
      YAML.representer.representSequence(Tag.SEQ, Seq(wrapper.id, wrapper.typeSymbol, wrapper.data).asJava, true)
    }
  }
  case class Wrapper(val typeSymbol: Symbol, val data: String, val id: Symbol)
}
