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
import scala.collection.immutable
import scala.collection.mutable

import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.element.Value
import org.digimead.tabuddy.model.element.{ Element ⇒ EElement }
import org.digimead.tabuddy.model.element.{ Stash ⇒ EStash }
import org.digimead.tabuddy.model.serialization.yaml.YAML.yaml2implementation
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.MappingNode
import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.SequenceNode
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.representer.Represent

/** Container for optional information: Value Id -> Type Id -> (Static/Dynamic, Context) */
case class Optional(values: immutable.HashMap[scala.Symbol, immutable.HashMap[scala.Symbol, (Boolean, Value.Context)]])

object Optional {
  val tag = new Tag(Tag.PREFIX + "optional")

  /** Convert Optional to string. */
  def dump(arg: Optional): String = YAML.dump(arg).trim
  /** Get optional container from element. */
  def getOptional(element: EElement): Optional = getOptional(element.eStash)
  /** Get optional container from stash. */
  def getOptional(stash: EStash.Like): Optional = {
    val perElement = stash.property.map {
      case (valueId, valueMap) ⇒
        val perValueId = valueMap.map {
          case (typeSymbol, value) if DSLType.symbols(typeSymbol) ⇒
            typeSymbol -> (value.getClass().isAssignableFrom(classOf[Value.Static[_]]), value.context)
          case (typeSymbol, value) ⇒
            throw new YAMLException(s"Unable to process properties with symbol ${typeSymbol}. Suitable type not found.")
        }
        valueId -> perValueId
    }
    Optional(perElement)
  }
  /** Convert string to Optional. */
  def load(arg: String): Optional = YAML.loadAs(arg, classOf[Optional]).asInstanceOf[Optional]

  trait Constructor {
    this: YAML.Constructor ⇒
    getYAMLConstructors.put(Optional.tag, new ConstructOptional())
    getYAMLConstructors.put(new Tag(classOf[Optional]), new ConstructOptional())

    private class ConstructOptional extends CustomConstruct {
      protected val keyTypes = immutable.HashMap[String, PartialFunction[Node, Unit]](
        "values" -> {
          case n: MappingNode ⇒ n.getValue().iterator().asScala.foreach(_.getValueNode() match {
            case n: MappingNode ⇒ n.getValue().iterator().asScala.foreach(_.getValueNode() match {
              case n: SequenceNode ⇒
                setTagSafe(n.getValue().get(2), UUID.tag)
            })
          })
        })
      def constructCustom(map: mutable.HashMap[String, AnyRef]): AnyRef = {
        val valuesSeq = map("values").asInstanceOf[mutable.Map[String, java.util.LinkedHashMap[String, java.util.List[AnyRef]]]].toSeq.map {
          case (valueId, perTypeMapYAML) ⇒
            val perTypeSeq = perTypeMapYAML.asScala.toSeq.map {
              case (typeId, info) ⇒
                val isStatic = info.get(0).asInstanceOf[java.lang.Boolean].booleanValue()
                val contextLine = info.get(1).asInstanceOf[java.lang.Integer]
                val contextId = info.get(2).asInstanceOf[java.util.UUID]
                Symbol.load(typeId) -> (isStatic, new Value.Context(if (contextId != null) Some(contextId) else None,
                  if (contextLine != null) Some(contextLine) else None))
            }
            Symbol.load(valueId) -> immutable.HashMap(perTypeSeq: _*)
        }
        Optional(immutable.HashMap(valuesSeq: _*))
      }
    }
  }
  trait Representer {
    this: YAML.Representer ⇒
    getMultiRepresenters.put(classOf[Optional], new RepresentOptional())

    class RepresentOptional extends Represent {
      def representData(data: AnyRef): Node = {
        val optional = data.asInstanceOf[Optional]
        val valuesMap = new java.util.TreeMap[String, java.util.TreeMap[String, Array[AnyRef]]]()
        optional.values.foreach {
          case (id, perTypeMap) ⇒
            val perTypeMapYAML = Option(valuesMap.get(id.name)).getOrElse {
              val perTypeMap = new java.util.TreeMap[String, Array[AnyRef]]()
              valuesMap.put(Symbol.dump(id), perTypeMap)
              perTypeMap
            }
            perTypeMap.foreach {
              case (typeId, (isStatic, context)) ⇒
                perTypeMapYAML.put(Symbol.dump(typeId), Array[AnyRef](isStatic: java.lang.Boolean,
                  context.line.map(i ⇒ i: java.lang.Integer).getOrElse(null),
                  context.objectId.getOrElse(null)))
            }
        }
        val map = new java.util.TreeMap[String, AnyRef]()
        map.put("values", valuesMap)
        representMapping(Tag.MAP, map, null)
      }
    }
  }
}
