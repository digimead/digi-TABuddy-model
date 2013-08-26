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

import scala.collection.immutable
import org.digimead.digi.lib.log.api.Loggable

class BasicDSLTypes extends DSLType with Loggable {
  protected lazy val typeClassSymbolMap = immutable.HashMap[Class[_], Symbol](
    classOf[java.lang.Byte] -> 'Byte,
    classOf[java.lang.Double] -> 'Double,
    classOf[java.lang.Float] -> 'Float,
    classOf[java.lang.Integer] -> 'Integer,
    classOf[java.lang.Long] -> 'Long,
    classOf[java.lang.Short] -> 'Short,
    classOf[java.lang.Boolean] -> 'Boolean,
    classOf[java.lang.String] -> 'String)

  /**
   * Convert value from string
   */
  def convertFromString: PartialFunction[(Symbol, String), _ <: AnyRef with java.io.Serializable] = {
    case ('Byte, valueData) => valueData.toByte
    case ('Double, valueData) => valueData.toDouble
    case ('Float, valueData) => valueData.toFloat
    case ('Integer, valueData) => valueData.toInt
    case ('Long, valueData) => valueData.toLong
    case ('Short, valueData) => valueData.toShort
    case ('Boolean, valueData) => valueData.toBoolean
    case ('String, valueData) => valueData
  }
  /**
   * Save value to string
   */
  def convertToString: PartialFunction[(Symbol, _ <: AnyRef with java.io.Serializable), String] = {
    case ('Byte, valueData) => String.valueOf(valueData)
    case ('Double, valueData) => String.valueOf(valueData)
    case ('Float, valueData) => String.valueOf(valueData)
    case ('Integer, valueData) => String.valueOf(valueData)
    case ('Long, valueData) => String.valueOf(valueData)
    case ('Short, valueData) => String.valueOf(valueData)
    case ('Boolean, valueData) => String.valueOf(valueData)
    case ('String, valueData) => valueData.toString
  }
}
