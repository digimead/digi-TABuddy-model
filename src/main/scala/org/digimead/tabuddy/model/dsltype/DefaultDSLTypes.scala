/**
 * This file is part of the TABuddy project.
 * Copyright (c) 2012 Alexey Aksenov ezh@ezh.msk.ru
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Global License version 3
 * as published by the Free Software Foundation with the addition of the
 * following permission added to Section 15 as permitted in Section 7(a):
 * FOR ANY PART OF THE COVERED WORK IN WHICH THE COPYRIGHT IS OWNED
 * BY Limited Liability Company «MEZHGALAKTICHESKIJ TORGOVYJ ALIANS»,
 * Limited Liability Company «MEZHGALAKTICHESKIJ TORGOVYJ ALIANS» DISCLAIMS
 * THE WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Global License for more details.
 * You should have received a copy of the GNU Affero General Global License
 * along with this program; if not, see http://www.gnu.org/licenses or write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA, 02110-1301 USA, or download the license from the following URL:
 * http://www.gnu.org/licenses/agpl.html
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Global License.
 *
 * In accordance with Section 7(b) of the GNU Affero General Global License,
 * you must retain the producer line in every report, form or document
 * that is created or manipulated using TABuddy.
 *
 * You can be released from the requirements of the license by purchasing
 * a commercial license. Buying such a license is mandatory as soon as you
 * develop commercial activities involving the TABuddy software without
 * disclosing the source code of your own applications.
 * These activities include: offering paid services to customers,
 * serving files in a web or/and network application,
 * shipping TABuddy with a closed source product.
 *
 * For more information, please contact Digimead Team at this
 * address: ezh@ezh.msk.ru
 */

package org.digimead.tabuddy.model.dsltype

import scala.collection.immutable

class DefaultDSLTypes extends DSLType {
  protected lazy val typeClassSignatureMap = immutable.HashMap[Class[_], String](
    classOf[java.lang.Byte] -> "Byte",
    classOf[java.lang.Double] -> "Double",
    classOf[java.lang.Float] -> "Float",
    classOf[java.lang.Integer] -> "Integer",
    classOf[java.lang.Long] -> "Long",
    classOf[java.lang.Short] -> "Short",
    classOf[java.lang.Boolean] -> "Boolean",
    classOf[java.lang.String] -> "String")
  protected lazy val typeSignatureClassMap = immutable.HashMap[String, Class[_]](typeClassSignatureMap.map(t => (t._2, t._1)).toSeq: _*)
  /**
   * Load value from string
   */
  def load: PartialFunction[(String, String), _ <: java.io.Serializable] = {
    case ("Byte", valueData) => valueData.toByte
    case ("Double", valueData) => valueData.toDouble
    case ("Float", valueData) => valueData.toFloat
    case ("Integer", valueData) => valueData.toInt
    case ("Long", valueData) => valueData.toLong
    case ("Short", valueData) => valueData.toShort
    case ("Boolean", valueData) => valueData.toBoolean
    case ("String", valueData) => valueData
  }
  /**
   * Save value to string
   */
  def save[T <: java.io.Serializable]: PartialFunction[T, (String, String)] = {
    case x: java.lang.Byte => ("Byte", String.valueOf(x))
    case x: java.lang.Double => ("Double", String.valueOf(x))
    case x: java.lang.Float => ("Float", String.valueOf(x))
    case x: java.lang.Integer => ("Integer", String.valueOf(x))
    case x: java.lang.Long => ("Long", String.valueOf(x))
    case x: java.lang.Short => ("Short", String.valueOf(x))
    case x: java.lang.Boolean => ("Boolean", String.valueOf(x))
    case x: java.lang.String => ("String", x)
  }
  /**
   * Convert JVM type to string signature
   */
  def getTypeSignature(clazz: Class[_]): Option[String] = typeClassSignatureMap.get(clazz)
  /**
   * Convert string signature to JVM type
   */
  def getTypeClass(signature: String): Option[Class[_]] = typeSignatureClassMap.get(signature)
  /**
   * Returns all known types
   */
  def getTypes(): Seq[String] = typeSignatureClassMap.keys.toSeq
}
