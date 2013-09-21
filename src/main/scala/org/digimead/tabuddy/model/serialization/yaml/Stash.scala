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

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.dsl.DSLType.dsltype2implementation
import org.digimead.tabuddy.model.element.{ Element ⇒ EElement }
import org.digimead.tabuddy.model.element.{ Stash ⇒ EStash }
import org.digimead.tabuddy.model.element.Stash.Like
import org.digimead.tabuddy.model.element.Value
import org.digimead.tabuddy.model.serialization.Serialization
import org.digimead.tabuddy.model.serialization.yaml.YAML.yaml2implementation
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.SequenceNode
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.representer.{ Represent ⇒ YAMLRepresent }

/**
 * YAML de/serialization helper for Stash.
 */
object Stash extends Loggable {
  val tag = new Tag(Tag.PREFIX + "stash")
  /** Value is static by default. */
  val defaultStatic = true

  /** Convert Stash to string. */
  def dump(arg: EStash.Like): String = YAML.dump(arg).trim
  /** Convert string to Stash. */
  def load(arg: String): EStash.Like = YAML.loadAs(arg, classOf[EStash.Like]).asInstanceOf[EStash.Like]

  class Construct extends YAML.constructor.CustomConstruct {
    YAML.constructor.getYAMLConstructors.put(Stash.tag, this)
    YAML.constructor.getYAMLConstructors.put(new Tag(classOf[EStash.Like]), this)

    protected val keyTypes = immutable.HashMap[String, PartialFunction[Node, Unit]](
      "created" -> { case n: Node ⇒ YAML.constructor.setTagSafe(n, Timestamp.tag) },
      "modified" -> { case n: Node ⇒ YAML.constructor.setTagSafe(n, Timestamp.tag) },
      "properties" -> { case n: SequenceNode ⇒ n.getValue().asScala.foreach(YAML.constructor.setTagSafe(_, Property.tag)) },
      "scope" -> { case n: Node ⇒ YAML.constructor.setTagSafe(n, Scope.tag) })
    def constructCustom(map: mutable.HashMap[String, AnyRef]): AnyRef = {
      val created = map("created").asInstanceOf[EElement.Timestamp]
      val modified = map("modified").asInstanceOf[EElement.Timestamp]
      val property = unpackProperties(map("properties").asInstanceOf[Iterable[Property.Wrapper]])
      val scope = map("scope").asInstanceOf[EElement.Scope]
      val stashClass = getClass.getClassLoader().loadClass(map("class").asInstanceOf[String]).asInstanceOf[Class[_ <: EStash.Like]]
      val newStashCtor = stashClass.getConstructors.find(_.getParameterTypes() match {
        case Array(createdArg, modifiedArg, dataArg, scopeArg) ⇒
          scopeArg.isAssignableFrom(scope.getClass) && createdArg.isAssignableFrom(created.getClass()) &&
            modifiedArg.isAssignableFrom(modified.getClass()) && dataArg.isAssignableFrom(property.getClass())
        case _ ⇒ false
      }) getOrElse {
        throw new NoSuchMethodException(s"Unable to find proper constructor for stash ${this.getClass()}.")
      }
      newStashCtor.newInstance(created, modified, property, scope).asInstanceOf[AnyRef]
    }

    /** Convert Iterable[Property.Wrapper] to Stash.Data. */
    protected def unpackProperties(list: Iterable[Property.Wrapper]): EStash.Data = {
      // get local thread value with optional information
      val optional = Serialization.stash.get match {
        case optional: Optional ⇒
          Some(optional)
        case other ⇒
          log.error("Unable to find optional information.")
          None
      }
      val properties = for ((id, properties) ← list.groupBy(_.id).toSeq) yield {
        val values: Seq[(scala.Symbol, Value[_ <: AnyRef with java.io.Serializable])] =
          for (property ← properties.toSeq) yield if (DSLType.symbols(property.typeSymbol)) {
            val static = optional.flatMap(_.values.get(id)).flatMap(_.get(property.typeSymbol)) match {
              case Some(isStatic) ⇒
                isStatic
              case None ⇒
                log.error(s"Unable to find optional information for value ${id} → ${property.typeSymbol}.")
                defaultStatic
            }
            (DSLType.convertFromString(property.typeSymbol, property.data) match {
              case Some(data) if static ⇒
                property.typeSymbol -> new Value.Static(data)(Manifest.classType(DSLType.symbolClassMap(property.typeSymbol)))
              case Some(data) ⇒
                if (data.isInstanceOf[Value[_ <: AnyRef with java.io.Serializable]])
                  property.typeSymbol -> data.asInstanceOf[Value[_ <: AnyRef with java.io.Serializable]]
                else
                  property.typeSymbol -> new Value.Dynamic(() ⇒ data)(Manifest.classType(DSLType.symbolClassMap(property.typeSymbol)))
              case None ⇒
                throw new YAMLException(s"Unable to unpack value '${property.id}=${property.data}.")
            }): (scala.Symbol, Value[_ <: AnyRef with java.io.Serializable])
          } else
            throw new YAMLException(s"Unable to unpack property ${property.id} with unknown symbol ${property.typeSymbol}.")
        id -> immutable.HashMap[Symbol, Value[_ <: AnyRef with java.io.Serializable]](values: _*)
      }
      immutable.HashMap[scala.Symbol, immutable.HashMap[scala.Symbol, Value[_ <: AnyRef with java.io.Serializable]]](properties: _*)
    }
  }
  class Represent extends YAMLRepresent {
    YAML.representer.getMultiRepresenters.put(classOf[EStash.Like], this)

    def representData(data: AnyRef): Node = {
      val stash = data.asInstanceOf[EStash.Like]
      var properties = Seq[Property.Wrapper]()
      stash.property.keys.foreach { valueID ⇒
        stash.property(valueID).foreach {
          case (typeSymbol, value) if DSLType.symbols(typeSymbol) ⇒
            DSLType.convertToString(typeSymbol, value.get) match {
              case Some(valueData) ⇒
                properties = properties :+ Property.Wrapper(typeSymbol, valueData, valueID)
              case None ⇒
                throw new YAMLException(s"Unable to convert value '${value}'.")
            }
          case (typeSymbol, value) ⇒
            throw new YAMLException(s"Unable to convert properties with symbol ${typeSymbol}. Suitable type not found.")
        }
      }
      val map = new java.util.TreeMap[String, AnyRef]()
      map.put("class", stash.getClass().getName)
      map.put("created", stash.created)
      map.put("modified", stash.modified)
      map.put("properties", properties.sortBy(_.id.name).asJava)
      map.put("scope", stash.scope)
      YAML.representer.representMapping(Tag.MAP, map, null)
    }
  }
}
