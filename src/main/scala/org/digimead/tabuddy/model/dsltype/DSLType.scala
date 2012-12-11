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

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.Loggable

trait DSLType {
  /**
   * Load value from string
   */
  def load: PartialFunction[(String, String), _ <: java.io.Serializable]
  /**
   * Save value to string
   */
  def save[T <: java.io.Serializable]: PartialFunction[T, (String, String)]
  /**
   * Convert JVM type to string signature
   */
  def getTypeSignature(clazz: Class[_]): Option[String]
  /**
   * Convert string signature to JVM type
   */
  def getTypeClass(signature: String): Option[Class[_]]
  /**
   * Returns all known types
   */
  def getTypes(): Seq[String]
}

/**
 * Object that contains all DSL types and provides conversion routines
 */
object DSLType extends DependencyInjection.PersistentInjectable with Loggable {
  implicit def dsltype2implementation(m: DSLType.type): Interface = m.implementation
  implicit def bindingModule = DependencyInjection()
  @volatile private var converter = inject[Seq[DSLType]]
  @volatile private var implementation: Interface = inject[Interface]

  def inner() = implementation
  def commitInjection() {}
  def updateInjection() {
    converter = inject[Seq[DSLType]]
    implementation = inject[Interface]
  }

  trait Interface {
    /**
     * Load value from string
     */
    def load(valueType: String, valueData: String): Option[_ <: java.io.Serializable] = {
      DSLType.converter.foreach(converter =>
        try {
          return Some(converter.load(valueType, valueData))
        } catch {
          case e: MatchError =>
        })
      None
    }
    /**
     * Save value to string
     */
    def save[T <: java.io.Serializable](value: T): Option[(String, String)] = {
      DSLType.converter.foreach(converter =>
        try {
          return Some(converter.save(value))
        } catch {
          case e: MatchError =>
        })
      None
    }
    /**
     * Convert JVM type to string signature
     */
    def getTypeSignature(clazz: Class[_]): Option[String] = {
      DSLType.converter.foreach(converter =>
        try {
          converter.getTypeSignature(clazz).map(sig => return Some(sig))
        } catch {
          case e: MatchError =>
        })
      None
    }
    /**
     * Convert string signature to JVM type
     */
    def getTypeClass(signature: String): Option[Class[_]] = {
      DSLType.converter.foreach(converter =>
        try {
          converter.getTypeClass(signature).map(typeJVM => return Some(typeJVM))
        } catch {
          case e: MatchError =>
        })
      None
    }
    /**
     * Returns all known types
     */
    def getTypes(): Seq[String] = DSLType.converter.map(_.getTypes).flatten
  }
}
