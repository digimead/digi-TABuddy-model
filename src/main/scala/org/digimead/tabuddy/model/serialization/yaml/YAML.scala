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

import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.tabuddy.model.serialization.Serialization
import org.yaml.snakeyaml.DumperOptions
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.{ Constructor ⇒ YAMLConstructor }
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.MappingNode
import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.ScalarNode
import org.yaml.snakeyaml.nodes.SequenceNode
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.representer.{ Representer ⇒ YAMLRepresenter }

import scala.language.implicitConversions

/**
 * Provide YAML API for application.
 */
object YAML {
  implicit def yaml2implementation(y: YAML.type): Yaml = yaml

  /** YAML de/serializer. */
  lazy val yaml = {
    val options = new DumperOptions()
    options.setAllowUnicode(true)
    options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK)
    new Yaml(DI.constructor, DI.representer, options)
  }

  /** Default YAML constructor. */
  def constructor = DI.constructor
  /** Default YAML representer. */
  def representer = DI.representer

  /** Base YAML constructor. */
  class Constructor extends YAMLConstructor {
    // Workaround for java.lang.IllegalAccessError
    override protected def constructObject(node: Node): AnyRef =
      super.constructObject(node)
    override protected def constructMapping(node: MappingNode): java.util.Map[AnyRef, AnyRef] =
      super.constructMapping(node)
    override protected def constructSequence(node: SequenceNode): java.util.List[_] =
      super.constructSequence(node)
    protected def getYAMLClassConstructors =
      this.yamlClassConstructors
    protected def getYAMLConstructors =
      this.yamlConstructors

    /** Set new tag to node if not null. */
    def setTagSafe(node: Node, tag: Tag) = if (node.getTag() != Tag.NULL)
      node.setTag(tag)

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
  /** Default constructor base trait. */
  trait DefaultConstructor
    extends Axis.Constructor
    with Coordinate.Constructor
    with Optional.Constructor
    with Property.Constructor
    with Reference.Constructor
    with Scope.Constructor
    with Serialization.Descriptor.Element.Constructor
    with Serialization.Descriptor.Graph.Constructor
    with Serialization.Descriptor.Node.Constructor
    with Stash.Constructor
    with Symbol.Constructor
    with Timestamp.Constructor
    with UUID.Constructor {
    this: Constructor ⇒
  }
  /** Default representer base trait. */
  trait DefaultRepresenter
    extends Axis.Representer
    with Coordinate.Representer
    with Optional.Representer
    with Property.Representer
    with Reference.Representer
    with Scope.Representer
    with Stash.Representer
    with Symbol.Representer
    with Timestamp.Representer
    with UUID.Representer {
    this: Representer ⇒
  }
  /** Base YAML representer. */
  class Representer extends YAMLRepresenter {
    // Workaround for java.lang.IllegalAccessError
    protected def getMultiRepresenters = this.multiRepresenters
    protected override def representMapping(tag: Tag, mapping: java.util.Map[_, AnyRef], flowStyle: java.lang.Boolean): Node = {
      // workaround for proper null representation
      this.objectToRepresent = new Object
      super.representMapping(tag, mapping, flowStyle)
    }
    protected override def representScalar(tag: Tag, value: String): Node = {
      // workaround for proper null representation
      this.objectToRepresent = new Object
      super.representScalar(tag, value)
    }
    protected override def representScalar(tag: Tag, value: String, style: Character): Node = {
      // workaround for proper null representation
      this.objectToRepresent = new Object
      super.representScalar(tag, value, style)
    }
    protected override def representSequence(tag: Tag, sequence: java.lang.Iterable[_], flowStyle: java.lang.Boolean): Node = {
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
    val constructor = injectOptional[Constructor] getOrElse new Constructor with DefaultConstructor
    /** YAML representer. */
    val representer = injectOptional[Representer] getOrElse new Representer with DefaultRepresenter
  }
}
