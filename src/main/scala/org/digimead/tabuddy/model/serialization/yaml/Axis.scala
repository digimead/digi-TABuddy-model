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

import scala.collection.immutable
import scala.collection.mutable

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.dsl.DSLType.dsltype2implementation
import org.digimead.tabuddy.model.element.{ Axis ⇒ EAxis }
import org.digimead.tabuddy.model.serialization.yaml.YAML.yaml2implementation
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.representer.Represent

/**
 * YAML de/serialization helper for Axis.
 */
object Axis extends Loggable {
  val tag = new Tag(Tag.PREFIX + "axis")

  /** Convert string to Axis. */
  def load(arg: String): EAxis[_ <: AnyRef with java.io.Serializable] =
    YAML.loadAs(arg, classOf[EAxis[_ <: AnyRef with java.io.Serializable]]).asInstanceOf[EAxis[_ <: AnyRef with java.io.Serializable]]
  /** Convert Axis to string. */
  def dump(arg: EAxis[_ <: AnyRef with java.io.Serializable]): String = YAML.dump(arg).trim

  trait Constructor {
    this: YAML.Constructor ⇒
    getYAMLConstructors.put(Axis.tag, new ConstructCoordinate())
    getYAMLConstructors.put(new Tag(classOf[EAxis[_ <: AnyRef with java.io.Serializable]]), new ConstructCoordinate())

    private class ConstructCoordinate extends CustomConstruct {
      protected val keyTypes = immutable.HashMap[String, PartialFunction[Node, Unit]](
        "id" -> { case n: Node ⇒ n.setTag(Symbol.tag) },
        "type" -> { case n: Node ⇒ n.setTag(Symbol.tag) })
      def constructCustom(map: mutable.HashMap[String, AnyRef]): AnyRef = {
        val id = map("id").asInstanceOf[scala.Symbol]
        val typeSymbol = map("type").asInstanceOf[scala.Symbol]
        val value = map("value").asInstanceOf[String]
        DSLType.convertFromString(typeSymbol, value) match {
          case Some(deserializedValue) ⇒ EAxis(id, deserializedValue)(Manifest.classType(DSLType.symbolClassMap(typeSymbol)))
          case None ⇒ throw new YAMLException(s"Unable to unpack axis from ${map}. Incorrect value '${value}'.")
        }
      }
    }
  }
  trait Representer {
    this: YAML.Representer ⇒
    getMultiRepresenters.put(classOf[EAxis[_ <: AnyRef with java.io.Serializable]], new RepresentAxis())

    class RepresentAxis extends Represent {
      def representData(data: AnyRef): Node = {
        val arg = data.asInstanceOf[EAxis[_ <: AnyRef with java.io.Serializable]]
        val map = new java.util.HashMap[String, AnyRef]()
        DSLType.classSymbolMap.get(arg.m.runtimeClass) match {
          case Some(typeSymbol) ⇒
            DSLType.convertToString(typeSymbol, arg.value) match {
              case Some(valueData) ⇒
                map.put("id", arg.id)
                map.put("type", typeSymbol)
                map.put("value", valueData)
              case None ⇒
                throw new YAMLException(s"Unable to convert axis value ${arg.value}.")
            }
          case None ⇒
            throw new YAMLException(s"Unable to convert axis with class ${arg.m.runtimeClass}: suitable type symbol not found.")
        }
        representMapping(Tag.MAP, map, null)
      }
    }
  }
}
