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

import org.digimead.tabuddy.model.element.{ Element ⇒ EElement }
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.SequenceNode
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.representer.{ Represent ⇒ YAMLRepresent }

object Scope {
  val tag = new Tag(Tag.PREFIX + "scope")

  /** Convert Scope to string. */
  def dump(arg: EElement.Scope): String = YAML.block.dump(arg).trim
  /** Convert string to Scope. */
  def load(arg: String): EElement.Scope = YAML.block.loadAs(arg, classOf[EElement.Scope]).asInstanceOf[EElement.Scope]

  class Construct extends YAML.constructor.ConstructSequence {
    YAML.constructor.getYAMLConstructors.put(Scope.tag, this)
    YAML.constructor.getYAMLConstructors.put(new Tag(classOf[EElement.Scope]), this)

    override def construct(node: Node): AnyRef = node match {
      case snode: SequenceNode ⇒
        YAML.constructor.setTagSafe(snode.getValue().get(0), Symbol.tag)
        val seq = YAML.constructor.constructSequence(snode).asInstanceOf[java.util.ArrayList[AnyRef]]
        val modificator = seq.get(0).asInstanceOf[scala.Symbol]
        val scopeClass = getClass.getClassLoader().loadClass(seq.get(1).asInstanceOf[String]).asInstanceOf[Class[_ <: EElement.Scope]]
        scopeClass.getConstructor(classOf[Symbol]).newInstance(modificator)
      case node ⇒
        throw new YAMLException(s"Unexpected Scope node: ${node}.")
    }
  }
  class Represent extends YAMLRepresent {
    YAML.representer.getMultiRepresenters.put(classOf[EElement.Scope], this)

    def representData(data: AnyRef): Node = {
      val scope = data.asInstanceOf[EElement.Scope]
      YAML.representer.representSequence(Tag.SEQ, Seq(scope.modificator, scope.getClass().getName).asJava, true)
    }
  }
}
