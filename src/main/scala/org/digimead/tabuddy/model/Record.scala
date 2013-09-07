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

package org.digimead.tabuddy.model

import java.util.UUID

import scala.Array.canBuildFrom

import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.LocationGeneric
import org.digimead.tabuddy.model.element.Value
import org.digimead.tabuddy.model.element.Value.string2someValue
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.ElementBox.box2interface

/**
 * Record element.
 */
class Record(stashArg: Record.Stash)(@transient val eBox: ElementBox[Record])
  extends Record.Like with Loggable {
  type ElementType = Record
  type StashType = Record.Stash

  /** Get current stash. */
  def eStash = stashArg
}

object Record extends Loggable {
  val scope = new Scope()

  /** Part of DSL.Builder for end user. */
  trait DSL {
    case class RecordLocation(val id: Symbol, val unique: Option[UUID] = None,
      val coordinate: Coordinate = Coordinate.root)(implicit val elementType: Manifest[Record],
        val stashClass: Class[_ <: Record#StashType]) extends LocationGeneric[Record] {
      val scope = Record.scope
    }
  }
  object DSL {
    trait RichGeneric {
      this: org.digimead.tabuddy.model.dsl.DSL.RichGeneric ⇒
      /** Create a new record or retrieve exists one. */
      def record(id: Symbol, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*): Record =
        withRecord(id, rawCoordinate: _*)(x ⇒ x.immutable)
      /**
       * Create a new record or retrieve exists one and apply fTransform to
       *
       * @return record
       */
      def takeRecord[A](id: Symbol, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(fTransform: Mutable[Record] ⇒ A): Record =
        withRecord(id, rawCoordinate: _*)((x) ⇒ { fTransform(x); x.immutable })
      /**
       * Create a new record or retrieve exists one and apply fTransform to.
       *
       * @return fTransform result
       */
      def withRecord[A](id: Symbol, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(fTransform: Mutable[Record] ⇒ A): A = {
        val coordinate = Coordinate(rawCoordinate: _*)
        // Modify parent node.
        element.eNode.threadSafe { parentNode ⇒
          parentNode.find { _.id == id } match {
            case Some(childNode) ⇒
              childNode.threadSafe { node ⇒
                log.debug(s"Get or create record element for exists ${node}.")
                implicit val shashClass = classOf[Record.Stash]
                val record = ElementBox.getOrCreate[Record](coordinate, node, Record.scope, parentNode.rootElementBox.serialization)
                fTransform(new Mutable(record))
              }
            case None ⇒
              parentNode.createChild(id, UUID.randomUUID()).threadSafe { node ⇒
                log.debug(s"Get or create record element for new ${node}.")
                implicit val shashClass = classOf[Record.Stash]
                val record = ElementBox.getOrCreate[Record](coordinate, node, Record.scope, parentNode.rootElementBox.serialization)
                fTransform(new Mutable(record))
              }
          }
        }
      }
      /** Safe cast element to Record.Like. */
      def toRecord() = element.eAs[Record.Like]
    }
    trait RichSpecific[A <: Record.Like] {
      this: org.digimead.tabuddy.model.dsl.DSL.RichSpecific[A] ⇒
    }
  }
  /** Base trait for all records. */
  trait Like extends Element {
    this: Loggable ⇒
    type ElementType <: Like
    type StashType <: Stash.Like

    def name = eGetOrElseRoot[String]('name).map(_.get) getOrElse ""
    def eDump(brief: Boolean, padding: Int = 2): String = {
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
      val self = if (name.isEmpty)
        "%s: %s".format(eStash.scope, eStash.id) + properties
      else
        "%s: %s \"%s\"".format(eStash.scope, eStash.id, name) + properties
      val childrenDump = eNode.threadSafe(_.iterator.map(_.getElementBoxes).flatten.
        map(_.get.eDump(brief, padding)).mkString("\n").split("\n").map(pad + _).mkString("\n").trim)
      if (childrenDump.isEmpty) self else self + "\n" + pad + childrenDump
    }
    def eGetOrElseRoot(id: Symbol, typeSignature: Symbol): Option[Value[_ <: AnyRef with java.io.Serializable]] =
      eGet(id, typeSignature) orElse {
        if (eStash.coordinate.isRoot)
          // we are already at root but value is absent
          None
        else
          // try to find value at root node
          eRoot.flatMap(_.eGet(id, typeSignature))
      }
    /** Get mutable representation. */
    override def eMutable(): Record.Mutable[ElementType] = new Record.Mutable(this.asInstanceOf[ElementType])

    override def canEqual(that: Any): Boolean = that.isInstanceOf[Record.Like]
  }
  /** Mutable representation of Record.Like. */
  class Mutable[A <: Like](e: A) extends Element.Mutable[A](e) {
    def name = element.name
    def name_=(value: String) { element = element.eSet('name, value, "").asInstanceOf[A] }
  }
  /** The marker object that describes record scope. */
  class Scope(override val modificator: Symbol = 'Record) extends Element.Scope(modificator) {
    def canEqual(other: Any): Boolean = other.isInstanceOf[Record.Scope]
  }
  /** Record stash. */
  class Stash(val coordinate: Coordinate,
    val created: Element.Timestamp,
    val id: Symbol,
    val modified: Element.Timestamp,
    val origin: Symbol,
    val property: org.digimead.tabuddy.model.element.Stash.Data,
    val scope: Scope,
    val unique: UUID)
    extends Stash.Like {
    /** Stash type. */
    type StashType = Record.Stash
    /** Scope type. */
    type ScopeType = Record.Scope
  }
  object Stash {
    trait Like extends org.digimead.tabuddy.model.element.Stash.Like {
      /** Stash type. */
      type Stash <: Record.Like
      /** Scope type. */
      type ScopeType <: Record.Scope
    }
  }
}
