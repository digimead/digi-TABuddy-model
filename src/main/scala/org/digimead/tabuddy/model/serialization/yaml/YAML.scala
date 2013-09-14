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

import org.digimead.digi.lib.api.DependencyInjection
import org.yaml.snakeyaml.DumperOptions
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.{ Constructor ⇒ YAMLConstructor }
import org.yaml.snakeyaml.nodes.Node
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
    options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK)
    new Yaml(DI.constructor, DI.representer, options)
  }

  /** Base YAML constructor. */
  class Constructor extends YAMLConstructor {
    // Workaround for java.lang.IllegalAccessError
    protected def getYAMLConstructors = this.yamlConstructors
  }
  /** Base YAML representer. */
  class Representer extends YAMLRepresenter {
    // Workaround for java.lang.IllegalAccessError
    protected def getMultiRepresenters = this.multiRepresenters
    protected override def representScalar(tag: Tag, value: String): Node = super.representScalar(tag, value)
    protected override def representScalar(tag: Tag, value: String, style: Character): Node = super.representScalar(tag, value, style)
  }
  /**
   * Dependency injection routines.
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    /** YAML constructor. */
    val constructor = injectOptional[Constructor] getOrElse new Constructor with Timestamp.Constructor
    /** YAML representer. */
    val representer = injectOptional[Representer] getOrElse new Representer with Timestamp.Representer
  }
}
