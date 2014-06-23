/**
 * TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2012-2014 Alexey Aksenov ezh@ezh.msk.ru
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

package org.digimead.tabuddy.model.element

import java.util.UUID
import org.digimead.digi.lib.api.XDependencyInjection
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.graph.{ ElementBox, Graph }
import scala.collection.immutable
import scala.language.implicitConversions

/**
 * Element reference that points to a particular unique element.
 */
// origin - exclusive attribute of Graph
// model - model unique id
// node - node unique id
// coordinate - exclusive attribute of ElementBox
case class Reference(origin: Symbol, model: UUID, node: UUID, coordinate: Coordinate) {
  override def toString() = "[%s://%s at %s]".format(origin.name, node, coordinate)
}

object Reference {
  implicit def reference2implementation(m: Reference.type): Interface = m.inner

  def inner = DI.implementation

  /**
   * Default reference singleton implementation.
   */
  trait Interface {
    /** Registry with all available graphs. */
    // Graph is mutable, so there is no reason to save his latest model modification timestamp.
    @volatile protected var registryMap = immutable.HashMap[(Symbol, UUID), Seq[Graph[_ <: Model.Like]]]()

    /** Get registry map. */
    def registry = registryMap
    /** Add graph of the consume to registry map. */
    def register(graph: Graph[_ <: Model.Like]) {
      // prevent modification while register
      graph.node.safeWrite { model ⇒
        registry.get(graph.origin, model.unique) match {
          case Some(seq) ⇒
            registryMap = registry + ((graph.origin, model.unique) -> (seq :+ graph))
          case None ⇒
            registryMap = registry + ((graph.origin, model.unique) -> Seq(graph))
        }
      }
    }
    /** Resolve the reference against the specific timestamp. */
    // Heavy weight operation.
    def resolve(reference: Reference, modelModificationTimestamp: Element.Timestamp): Option[_ <: Element] = {
      registry.get(reference.origin, reference.model).flatMap { seq ⇒
        seq.find(_.node.modified == modelModificationTimestamp).flatMap(_.nodes.get(reference.node).
          flatMap(_.projectionBoxes.get(reference.coordinate): Option[ElementBox[_ <: Element]]))
      }.map(_.e)
    }
    /** Remove graph of the consume from registry map. */
    def unregister(graph: Graph[_ <: Model.Like]) {
      // prevent modification while unregister
      graph.node.safeWrite { model ⇒
        registry.get(graph.origin, model.unique) match {
          case Some(seq) ⇒
            registryMap = registry + ((graph.origin, model.unique) -> seq.filterNot(_.eq(graph)))
          case None ⇒
        }
      }
    }
  }
  /**
   * Dependency injection routines
   */
  private object DI extends XDependencyInjection.PersistentInjectable {
    /** Reference implementation. */
    lazy val implementation = injectOptional[Interface] getOrElse new AnyRef with Interface
  }
}
