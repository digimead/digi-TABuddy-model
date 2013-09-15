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

package org.digimead.tabuddy.model.dsl

import java.net.URI

import scala.Array.canBuildFrom
import scala.collection.immutable

import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.serialization.transport.Transport

class ComplexDSLTypes extends DSLType {
  protected lazy val typeClassSymbolMap = immutable.HashMap[Class[_], Symbol](
    classOf[Array[Symbol]] -> 'ArrayOfSymbol)

  /**
   * Commit complex type (if needed) while saving
   */
  def commit(typeSymbol: Symbol, value: AnyRef with java.io.Serializable,
    element: Element, transport: Transport, elementURI: URI) {}
  /**
   * Convert value from string
   */
  def convertFromString: PartialFunction[(Symbol, String), _ <: AnyRef with java.io.Serializable] = {
    case (_, null) ⇒ null
    case ('ArrayOfSymbol, valueData) ⇒ valueData.split(" ").map(Symbol(_)).toArray
  }
  /**
   * Save value to string
   */
  def convertToString: PartialFunction[(Symbol, _ <: AnyRef with java.io.Serializable), String] = {
    case (_, null) ⇒ null
    case ('ArrayOfSymbol, valueData) ⇒ valueData.asInstanceOf[Array[Symbol]].map(_.name).mkString(" ")
  }
}
