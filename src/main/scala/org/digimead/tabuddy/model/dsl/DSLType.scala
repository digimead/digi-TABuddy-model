/**
 * This file is part of the TABuddy project.
 * Copyright (c) 2012-2013 Alexey Aksenov ezh@ezh.msk.ru
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

package org.digimead.tabuddy.model.dsl

import scala.Option.option2Iterable
import scala.collection.immutable

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.Loggable

import com.escalatesoft.subcut.inject.BindingModule

import language.implicitConversions

trait DSLType {
  /** The map of class -> symbol */
  protected val typeClassSymbolMap: immutable.HashMap[Class[_], Symbol]
  /** The map of symbol -> class */
  protected lazy val typeSymbolClassMap: immutable.HashMap[Symbol, Class[_]] =
    immutable.HashMap[Symbol, Class[_]](typeClassSymbolMap.map(t => (t._2, t._1)).toSeq: _*)

  /**
   * Convert value from string
   */
  def convertFromString: PartialFunction[(Symbol, String), _ <: AnyRef with java.io.Serializable]
  /**
   * Convert value to string
   */
  def convertToString: PartialFunction[(Symbol, _ <: AnyRef with java.io.Serializable), String]
  /**
   * Convert JVM type to type symbol
   */
  def getTypeSymbol(clazz: Class[_]): Option[Symbol] = typeClassSymbolMap.get(clazz)
  /**
   * Convert type symbol to JVM type
   */
  def getTypeClass(symbol: Symbol): Option[Class[_]] = typeSymbolClassMap.get(symbol)
  /**
   * Returns all known types
   */
  def getTypes(): Seq[Symbol] = typeSymbolClassMap.keys.toSeq
}

/**
 * Object that contains all DSL types and provides conversion routines
 */
object DSLType extends  Loggable {
  implicit def dsltype2implementation(m: DSLType.type): Interface = m.inner

  def inner() = DI.implementation
  def types() = DI.dsltypes

  trait Interface {
    /** General type names like String, Boolean, ... */
    lazy val symbols: immutable.HashSet[Symbol] = immutable.HashSet[Symbol](getSymbols(): _*)
    /** JVM classes like java.lang.String, java.lang.Boolean, ... */
    lazy val classes: immutable.HashSet[Class[_]] = immutable.HashSet[Class[_]](getClasses(): _*)
    /** Type name -> JVM class map */
    lazy val symbolClassMap: immutable.HashMap[Symbol, Class[_ <: AnyRef with java.io.Serializable]] = getSymbolClassMap()
    /** JVM class -> type name map */
    lazy val classSymbolMap: immutable.HashMap[Class[_], Symbol] = getClassSymbolMap()
    /** Type name -> converter map */
    lazy val symbolConverterMap: immutable.HashMap[Symbol, DSLType] = getSymbolConverterMap()

    /**
     * Convert value from string
     */
    def convertFromString[T <: AnyRef with java.io.Serializable](valueData: String)(implicit m: Manifest[T]): Option[T] =
      classSymbolMap.get(m.runtimeClass).flatMap(symbolType => convertFromString(symbolType, valueData).asInstanceOf[Option[T]])
    /**
     * Convert value from string
     */
    def convertFromString(typeSymbol: Symbol, valueData: String): Option[_ <: AnyRef with java.io.Serializable] =
      symbolConverterMap.get(typeSymbol) match {
        case Some(converter) =>
          Some(converter.convertFromString(typeSymbol, valueData))
        case None =>
          None
      }
    /**
     * Convert value to string
     */
    def convertToString[T <: AnyRef with java.io.Serializable](valueData: T)(implicit m: Manifest[T]): Option[String] =
      classSymbolMap.get(m.runtimeClass).flatMap(symbolType => convertToString(symbolType, valueData))
    /**
     * Convert value to string
     */
    def convertToString(typeSymbol: Symbol, valueData: AnyRef with java.io.Serializable): Option[String] =
      symbolConverterMap.get(typeSymbol) match {
        case Some(converter) =>
          Some(converter.convertToString(typeSymbol, valueData))
        case None =>
          None
      }
    /** Get type symbols */
    protected def getSymbols(): Seq[Symbol] =
      DSLType.types.map(_.getTypes).flatten
    /** Get type classes */
    protected def getClasses(): Seq[Class[_]] =
      DSLType.types.map(_.getTypes).flatten.map(symbol =>
        DSLType.types.flatMap(_.getTypeClass(symbol))).flatten
    /** Get symbol -> class map */
    protected def getSymbolClassMap(): immutable.HashMap[Symbol, Class[_ <: AnyRef with java.io.Serializable]] = {
      val tuples: Seq[(Symbol, Class[_ <: AnyRef with java.io.Serializable])] = getSymbols().map(symbol => {
        val maybeClazz = DSLType.types.find(_.getTypes.contains(symbol)).flatMap(_.getTypeClass(symbol))
        maybeClazz.map(clazz => (symbol, clazz.asInstanceOf[Class[_ <: AnyRef with java.io.Serializable]]))
      }).flatten
      immutable.HashMap[Symbol, Class[_ <: AnyRef with java.io.Serializable]](tuples: _*)
    }
    /** Get class -> symbol map */
    protected def getClassSymbolMap(): immutable.HashMap[Class[_], Symbol] =
      immutable.HashMap[Class[_], Symbol](getSymbolClassMap().map(t => (t._2, t._1)).toSeq: _*)
    /** Get symbol -> converter map */
    protected def getSymbolConverterMap(): immutable.HashMap[Symbol, DSLType] = {
      val tuples: Seq[(Symbol, DSLType)] = getSymbols().map(symbol => {
        val maybeConverter = DSLType.types.find(_.getTypes.contains(symbol))
        maybeConverter.map(converter => (symbol, converter))
      }).flatten
      immutable.HashMap[Symbol, DSLType](tuples: _*)
    }
  }
  /**
   * Dependency injection routines
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    implicit def bindingModule = DependencyInjection()
    /** DSLType implementation DI cache */
    @volatile var implementation = inject[Interface]
    /** Registered DSLType DI cache */
    @volatile var dsltypes = inject[Seq[DSLType]]

    override def injectionAfter(newModule: BindingModule) {
      dsltypes = inject[Seq[DSLType]]
      implementation = inject[Interface]
      val types = dsltypes.map(_.getTypes).flatten
      assert(types.distinct.size == types.size, "DSL types contains diplicated entities in " + types)
    }
    override def injectionBefore(newModule: BindingModule) {
      DependencyInjection.assertLazy[Interface](None, newModule)
      DependencyInjection.assertLazy[Seq[DSLType]](None, newModule)
    }
  }
}
