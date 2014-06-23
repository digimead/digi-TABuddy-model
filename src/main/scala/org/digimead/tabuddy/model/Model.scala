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

package org.digimead.tabuddy.model

import java.io.ObjectInputStream
import java.util.UUID
import org.digimead.digi.lib.log.api.XLoggable
import org.digimead.tabuddy.model.element.{ Coordinate, Element, LocationGeneric }
import org.digimead.tabuddy.model.graph.{ ElementBox, Node }
import scala.language.implicitConversions

/**
 * Common model.
 */
class Model(val eStash: Model.Stash)(@transient val eBox: ElementBox[Model])
  extends Model.Like with ModelIndex with XLoggable {
  type StashType = Model.Stash
  type RelativeType = Model.Relative[ElementType]
  type ElementType = Model

  /** Get relative representation. */
  override def eRelative: RelativeType = new Model.Relative(this)

  /** Built in serialization helper. */
  private def readObject(in: ObjectInputStream) {
    in.defaultReadObject()
    readObjectHelper()
  }
}

/**
 * Singleton that provides model interface implementation with ability
 * - load, save and reset model
 * - control context of elements
 * - save and retrieve element and context information
 */
object Model extends XLoggable {
  val scope = new Scope()

  /** Assert that A is not generic. */
  def assertNonGeneric[A](implicit m: Manifest[A]): Unit = if (m.runtimeClass == classOf[java.lang.Object])
    throw new IllegalArgumentException(s"Generic type '${m}' assertion failed.")

  /** Part of DSL.Builder for end user. */
  trait DSL {
    case class ModelLocation(val id: Symbol, val coordinate: Coordinate = Coordinate.root,
      val scope: Scope = Model.scope, val unique: Option[UUID] = None)(implicit val elementType: Manifest[Model],
        val stashClass: Class[_ <: Model#StashType]) extends LocationGeneric {
      type ElementType = Model
    }
  }
  object DSL {
    trait RichGeneric {
      this: org.digimead.tabuddy.model.dsl.DSL.RichGeneric ⇒
      /** Safe cast element to Model.Like. */
      def asModel = element.eAs[Model.Like]
    }
    trait RichSpecific[A <: Model.Like] {
      this: org.digimead.tabuddy.model.dsl.DSL.RichSpecific[A] ⇒
    }
  }
  /**
   * Base trait for all models.
   * Any concrete model may be represent as this trait.
   */
  trait Like extends Record.Like with ModelIndex {
    this: XLoggable ⇒
    type ElementType <: Like
    type RelativeType <: Relative[ElementType]
    type StashType <: Model.Stash.Like

    /** Dump the model content. */
    override def eDump(brief: Boolean, padding: Int = 2): String = synchronized {
      def dumpProperties() = {
        val result = eStash.property.map {
          case (id, sequence) ⇒
            sequence.map {
              case (typeSymbol, value) ⇒
                "%s: %s".format(id, value)
            }
        }.flatten
        if (result.nonEmpty) "\n  " + result.toSeq.sorted.mkString("\n  ") else ""
      }
      val pad = " " * padding
      val properties = if (brief) "" else dumpProperties()
      val self = "%s: %s → %s".format(eStash.scope, eId, eCoordinate) + properties
      val childrenDump = eNode.safeRead(_.iterator.map(_.projectionBoxes.values).flatten.
        map(_.e.eDump(brief, padding)).mkString("\n").split("\n").map(pad + _).mkString("\n").trim)
      if (childrenDump.isEmpty) self else self + "\n" + pad + childrenDump
    }
    /** Get Model for this element. */
    override def eModel = this
    /** Get a container */
    override def eParent: Option[Node[_ <: Element]] = None

    override def canEqual(that: Any): Boolean = that.isInstanceOf[Model.Like]

    override def toString() = "%s://%s[%s@GLOBAL]".format(eOrigin.name, eStash.scope, eId.name)
  }
  /** Relative representation of Model.Like. */
  class Relative[A <: Like](e: A) extends Record.Relative[A](e)
  /** The marker object that describes model scope */
  class Scope(override val modificator: Symbol = 'Model) extends Record.Scope(modificator) {
    override def canEqual(other: Any): Boolean = other.isInstanceOf[Model.Scope]
  }
  /**
   * Model common stash trait.
   * Any concrete model's stash may be represent as this trait.
   */
  class Stash(val created: Element.Timestamp,
    val modified: Element.Timestamp,
    val property: org.digimead.tabuddy.model.element.Stash.Data,
    val scope: Model.Scope) extends Stash.Like {
    /** Stash type. */
    type StashType = Model.Stash
    /** Scope type. */
    type ScopeType = Model.Scope
  }
  object Stash {
    trait Like extends Record.Stash.Like {
      /** Stash type. */
      type Stash <: Model.Like
      /** Scope type. */
      type ScopeType <: Model.Scope

      override def canEqual(that: Any): Boolean = that.isInstanceOf[Model.Stash.Like]
    }
  }
}
