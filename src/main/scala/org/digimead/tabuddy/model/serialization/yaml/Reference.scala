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

import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.collection.mutable

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.element.{ Coordinate ⇒ ECoordinate }
import org.digimead.tabuddy.model.element.{ Reference ⇒ EReference }
import org.digimead.tabuddy.model.serialization.yaml.YAML.yaml2implementation
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.MappingNode
import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.NodeTuple
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.representer.{ Represent ⇒ YAMLRepresent }

/**
 * YAML de/serialization helper for Reference.
 */
object Reference extends Loggable {
  val tag = new Tag(Tag.PREFIX + "ref")

  /** Convert Reference to string. */
  def dump(arg: EReference): String = YAML.dump(arg).trim
  /** Convert string to Reference. */
  def load(arg: String): EReference = YAML.loadAs(arg, classOf[EReference]).asInstanceOf[EReference]

  class Construct extends YAML.constructor.CustomConstruct {
    YAML.constructor.getYAMLConstructors.put(Reference.tag, this)
    YAML.constructor.getYAMLConstructors.put(new Tag(classOf[EReference]), this)

    protected val keyTypes = immutable.HashMap[String, PartialFunction[Node, Unit]](
      "coordinate" -> { case n: Node ⇒ YAML.constructor.setTagSafe(n, Coordinate.tag) },
      "origin" -> { case n: Node ⇒ YAML.constructor.setTagSafe(n, Symbol.tag) },
      "model" -> { case n: Node ⇒ YAML.constructor.setTagSafe(n, UUID.tag) },
      "node" -> { case n: Node ⇒ YAML.constructor.setTagSafe(n, UUID.tag) })
    def constructCustom(map: mutable.HashMap[String, AnyRef]): AnyRef =
      EReference(map("origin").asInstanceOf[scala.Symbol], map("model").asInstanceOf[JUUID],
        map("node").asInstanceOf[JUUID], map("coordinate").asInstanceOf[ECoordinate])
  }
  class Represent extends YAMLRepresent {
    YAML.representer.getMultiRepresenters.put(classOf[EReference], this)

    def representData(data: AnyRef): Node = {
      val reference = data.asInstanceOf[EReference]
      val map = new java.util.TreeMap[String, AnyRef]()
      map.put("coordinate", reference.coordinate)
      map.put("origin", reference.origin)
      map.put("model", reference.model)
      map.put("node", reference.node)
      YAML.representer.representMapping(Tag.MAP, map, null)
    }
  }
}
