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

import java.io.Writer

import scala.Option.option2Iterable
import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.collection.mutable

import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.yaml.snakeyaml.DumperOptions
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Construct
import org.yaml.snakeyaml.constructor.{ Constructor ⇒ YAMLConstructor }
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.MappingNode
import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.ScalarNode
import org.yaml.snakeyaml.nodes.SequenceNode
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.representer.Represent
import org.yaml.snakeyaml.representer.{ Representer ⇒ YAMLRepresenter }

/**
 * Provide YAML API for application.
 */
object YAML extends Loggable {
  /** Default YAML constructor. */
  lazy val constructor = DI.constructor
  /** Default YAML representer. */
  lazy val representer = DI.representer

  /** YAML de/serializer with BLOCK flow style. */
  lazy val blockOption = {
    val options = new DumperOptions()
    options.setAllowUnicode(true)
    options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK)
    DI.constructs.foreach(c ⇒ log.debug(s"Add ${c} as YAML contruct to BLOCK serializer."))
    DI.represents.foreach(r ⇒ log.debug(s"Add ${r} as YAML represent to BLOCK serializer."))
    options
  }
  /** YAML de/serializer with FLAT flow style. */
  lazy val flatOption = {
    val options = new DumperOptions()
    options.setAllowUnicode(true)
    options.setDefaultFlowStyle(DumperOptions.FlowStyle.FLOW)
    DI.constructs.foreach(c ⇒ log.debug(s"Add ${c} as YAML contruct to FLOW serializer."))
    DI.represents.foreach(r ⇒ log.debug(s"Add ${r} as YAML represent to FLOW serializer."))
    options
  }

  /** Create the new serializer each time. Prevents overflow. */
  def block = new Yaml(DI.constructor, DI.representer, blockOption)
  /** Create the new serializer each time. Prevents overflow. */
  def flat = new Yaml(DI.constructor, DI.representer, flatOption)

  /** YAML constructor. */
  class Constructor extends YAMLConstructor {
    // Overriding protected methods is workaround for java.lang.IllegalAccessError
    override def constructObject(node: Node): AnyRef =
      super.constructObject(node)
    override def constructMapping(node: MappingNode): java.util.Map[AnyRef, AnyRef] =
      super.constructMapping(node)
    override def constructSequence(node: SequenceNode): java.util.List[_] =
      super.constructSequence(node)
    def getYAMLClassConstructors =
      this.yamlClassConstructors
    def getYAMLConstructors =
      this.yamlConstructors

    /** Set new tag to node if not null. */
    def setTagSafe(node: Node, tag: Tag) = if (node.getTag() != Tag.NULL)
      node.setTag(tag)

    class ConstructSequence extends super.ConstructSequence
    abstract class CustomConstruct extends ConstructMapping {
      /** Map with explicit type per key. */
      protected val keyTypes: immutable.HashMap[String, PartialFunction[Node, Unit]]

      override protected def createEmptyJavaBean(node: MappingNode): AnyRef = null
      override def constructJavaBean2ndStep(node: MappingNode, obj: AnyRef): AnyRef = {
        var map = mutable.HashMap[String, AnyRef]()
        flattenMapping(node)
        for (tuple ← node.getValue().asScala) {
          // key must be scalar
          val keyNode = if (tuple.getKeyNode().isInstanceOf[ScalarNode])
            tuple.getKeyNode().asInstanceOf[ScalarNode]
          else
            throw new YAMLException("Keys must be scalars but found: " + tuple.getKeyNode());
          val valueNode = tuple.getValueNode()
          // keys can only be Strings
          keyNode.setType(classOf[String])
          val key = constructObject(keyNode).asInstanceOf[String]
          keyTypes.get(key).foreach(_(valueNode))
          map(key) = constructObject(valueNode) match {
            case s: java.lang.Iterable[_] ⇒ s.asScala
            case m: java.util.Map[_, _] ⇒ m.asScala
            case v ⇒ v
          }
        }
        constructCustom(map)
      }
      def constructCustom(map: mutable.HashMap[String, AnyRef]): AnyRef
    }
  }
  /** YAML representer. */
  class Representer extends YAMLRepresenter {
    // Overriding protected methods is workaround for java.lang.IllegalAccessError
    def getMultiRepresenters = this.multiRepresenters
    override def representMapping(tag: Tag, mapping: java.util.Map[_, AnyRef], flowStyle: java.lang.Boolean): Node = {
      // workaround for proper null representation
      this.objectToRepresent = new Object
      super.representMapping(tag, mapping, flowStyle)
    }
    override def representScalar(tag: Tag, value: String): Node = {
      // workaround for proper null representation
      this.objectToRepresent = new Object
      super.representScalar(tag, value)
    }
    override def representScalar(tag: Tag, value: String, style: Character): Node = {
      // workaround for proper null representation
      this.objectToRepresent = new Object
      super.representScalar(tag, value, style)
    }
    override def representSequence(tag: Tag, sequence: java.lang.Iterable[_], flowStyle: java.lang.Boolean): Node = {
      // workaround for proper null representation
      this.objectToRepresent = new Object
      super.representSequence(tag, sequence, flowStyle)
    }
  }
  /**
   * Dependency injection routines.
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    /** YAML constructor. */
    val constructor = injectOptional[Constructor] getOrElse new Constructor
    /** YAML representer. */
    val representer = injectOptional[Representer] getOrElse new Representer
    /**
     * Collection of YAML constructors.
     *
     * Each collected YAML constructor must be:
     *  1. an instance of Construct
     *  2. has name that starts with "YAML.Construct."
     */
    lazy val constructs = {
      val constructs = bindingModule.bindings.filter {
        case (key, value) ⇒ classOf[Construct].isAssignableFrom(key.m.runtimeClass)
      }.map {
        case (key, value) ⇒
          key.name match {
            case Some(name) if name.startsWith("YAML.Construct.") ⇒
              log.debug(s"'${name}' loaded.")
            case _ ⇒
              log.debug(s"'${key.name.getOrElse("Unnamed")}' YAML construct skipped.")
          }
          bindingModule.injectOptional(key).asInstanceOf[Option[Construct]]
      }.flatten.toSeq
      assert(constructs.distinct.size == constructs.size, "YAML constructs contains duplicated entities in " + constructs)
      constructs
    }
    /**
     * Collection of YAML representers.
     *
     * Each collected YAML representer must be:
     *  1. an instance of Represent
     *  2. has name that starts with "YAML.Represent."
     */
    lazy val represents = {
      val represents = bindingModule.bindings.filter {
        case (key, value) ⇒ classOf[Represent].isAssignableFrom(key.m.runtimeClass)
      }.map {
        case (key, value) ⇒
          key.name match {
            case Some(name) if name.startsWith("YAML.Represent.") ⇒
              log.debug(s"'${name}' loaded.")
            case _ ⇒
              log.debug(s"'${key.name.getOrElse("Unnamed")}' YAML represent skipped.")
          }
          bindingModule.injectOptional(key).asInstanceOf[Option[Represent]]
      }.flatten.toSeq
      assert(represents.distinct.size == represents.size, "YAML represents contains duplicated entities in " + represents)
      represents
    }
  }
}
