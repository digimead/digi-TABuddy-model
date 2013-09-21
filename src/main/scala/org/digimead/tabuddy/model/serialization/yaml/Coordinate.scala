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

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.element.{ Axis ⇒ EAxis }
import org.digimead.tabuddy.model.element.{ Coordinate ⇒ ECoordinate }
import org.digimead.tabuddy.model.serialization.yaml.YAML.yaml2implementation
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.SequenceNode
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.representer.{ Represent ⇒ YAMLRepresent }

object Coordinate extends Loggable {
  val tag = new Tag(Tag.PREFIX + "coord")

  /** Convert Coordinate to string. */
  def dump(arg: ECoordinate): String = YAML.dump(arg).trim
  /** Convert string to Coordinate. */
  def load(arg: String): ECoordinate = YAML.loadAs(arg, classOf[ECoordinate]).asInstanceOf[ECoordinate]

  class Construct extends YAML.constructor.ConstructSequence {
    YAML.constructor.getYAMLConstructors.put(Coordinate.tag, this)
    YAML.constructor.getYAMLConstructors.put(new Tag(classOf[ECoordinate]), this)

    override def construct(node: Node): AnyRef = node match {
      case snode: SequenceNode ⇒
        snode.getValue().iterator().asScala.foreach(YAML.constructor.setTagSafe(_, Axis.tag))
        ECoordinate(YAML.constructor.constructSequence(snode).asInstanceOf[java.util.ArrayList[EAxis[_ <: AnyRef with java.io.Serializable]]].asScala: _*)
      case node ⇒
        throw new YAMLException(s"Unexpected Coordinate node: ${node}.")
    }
  }
  class Represent extends YAMLRepresent {
    YAML.representer.getMultiRepresenters.put(classOf[ECoordinate], this)

    def representData(data: AnyRef): Node =
      YAML.representer.representSequence(Tag.SEQ, data.asInstanceOf[ECoordinate].coordinate.asJava, null)
  }
}
