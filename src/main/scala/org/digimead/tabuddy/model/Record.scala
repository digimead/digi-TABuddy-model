/**
 * TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2012-2015 Alexey Aksenov ezh@ezh.msk.ru
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
import org.digimead.tabuddy.model.element.{ Coordinate, Element, LocationGeneric, Value }
import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.graph.ElementBox

/**
 * Record element.
 */
class Record(val eStash: Record.Stash)(@transient val eBox: ElementBox[Record])
    extends Record.Like with XLoggable {
  type ElementType = Record
  type StashType = Record.Stash
  type RelativeType = Record.Relative[ElementType]

  /** Get relative representation. */
  override def eRelative: RelativeType = new Record.Relative(this)

  /** Built in serialization helper. */
  private def readObject(in: ObjectInputStream) {
    in.defaultReadObject()
    readObjectHelper()
  }
}

object Record extends XLoggable {
  val scope = new Scope()

  /** Part of DSL.Builder for end user. */
  trait DSL {
    case class RecordLocation(val id: Symbol, val coordinate: Coordinate = Coordinate.root,
        val scope: Scope = Record.scope, val unique: Option[UUID] = None)(implicit val elementType: Manifest[Record],
            val stashClass: Class[_ <: Record#StashType]) extends LocationGeneric {
      type ElementType = Record
    }
  }
  object DSL {
    trait RichGeneric {
      this: org.digimead.tabuddy.model.dsl.DSL.RichGeneric ⇒
      /** Create a new record or retrieve exists one. */
      def record(id: Symbol, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*): Record =
        withRecord(id, rawCoordinate: _*)(x ⇒ x.absolute)
      /** Create a new record or retrieve exists one. */
      def record(id: Symbol, scope: Scope, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*): Record =
        withRecord(id, scope, rawCoordinate: _*)(x ⇒ x.absolute)
      /**
       * Create a new record or retrieve exists one and apply fTransform to
       *
       * @return record
       */
      def getRecord[A](id: Symbol, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(fTransform: Relative[Record] ⇒ A): Record =
        withRecord(id, rawCoordinate: _*)((x) ⇒ { fTransform(x); x.absolute })
      /**
       * Create a new record or retrieve exists one and apply fTransform to
       *
       * @return record
       */
      def getRecord[A](id: Symbol, scope: Scope, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(fTransform: Relative[Record] ⇒ A): Record =
        withRecord(id, scope, rawCoordinate: _*)((x) ⇒ { fTransform(x); x.absolute })
      /**
       * Create a new record or retrieve exists one and apply fTransform to.
       *
       * @return fTransform result
       */
      def withRecord[A](id: Symbol, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(fTransform: Relative[Record] ⇒ A): A = {
        val coordinate = Coordinate(rawCoordinate: _*)
        withElement[Record, A](id, coordinate, Record.scope, classOf[Record.Stash], (record) ⇒ fTransform(new Relative(record)))
      }
      /**
       * Create a new record or retrieve exists one and apply fTransform to.
       *
       * @return fTransform result
       */
      def withRecord[A](id: Symbol, scope: Scope, rawCoordinate: Axis[_ <: AnyRef with java.io.Serializable]*)(fTransform: Relative[Record] ⇒ A): A = {
        val coordinate = Coordinate(rawCoordinate: _*)
        withElement[Record, A](id, coordinate, scope, classOf[Record.Stash], (record) ⇒ fTransform(new Relative(record)))
      }
      /** Safe cast element to Record.Like. */
      def asRecord = element.eAs[Record.Like]
    }
    trait RichSpecific[A <: Record.Like] {
      this: org.digimead.tabuddy.model.dsl.DSL.RichSpecific[A] ⇒
    }
  }
  /** Base trait for all records. */
  trait Like extends Element {
    this: XLoggable ⇒
    type ElementType <: Like
    type RelativeType <: Relative[ElementType]
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
        "%s: %s → %s".format(eStash.scope, eId, eCoordinate) + properties
      else
        "%s: %s \"%s\" → %s".format(eStash.scope, eId, name, eCoordinate) + properties
      val childrenDump = eNode.safeRead(_.iterator.map(_.projectionBoxes.values).flatten.
        map(_.e.eDump(brief, padding)).mkString("\n").split("\n").map(pad + _).mkString("\n").trim)
      if (childrenDump.isEmpty) self else self + "\n" + pad + childrenDump
    }
    def eGetOrElseRoot(id: Symbol, typeSignature: Symbol): Option[Value[_ <: AnyRef with java.io.Serializable]] =
      eGet(id, typeSignature) orElse {
        if (eCoordinate.isRoot)
          // we are already at root but value is absent
          None
        else
          // try to find value at root node
          eRoot.eGet(id, typeSignature)
      }

    override def canEqual(that: Any): Boolean = that.isInstanceOf[Record.Like]
  }
  /** Relative representation of Record.Like. */
  class Relative[A <: Like](e: A) extends Element.Relative[A](e) {
    def name = absolute.name
    def name_=(value: String) { absolute.eSet('name, value, "") }
  }
  /** The marker object that describes record scope. */
  class Scope(override val modificator: Symbol = 'Record) extends Element.Scope(modificator) {
    def canEqual(other: Any): Boolean = other.isInstanceOf[Record.Scope]
  }
  /** Record stash. */
  class Stash(val created: Element.Timestamp,
    val modified: Element.Timestamp,
    val property: org.digimead.tabuddy.model.element.Stash.Data,
    val scope: Scope)
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
