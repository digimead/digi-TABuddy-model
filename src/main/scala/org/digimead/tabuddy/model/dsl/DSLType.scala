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

import scala.Option.option2Iterable
import scala.collection.immutable

import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Value

import language.implicitConversions

trait DSLType {
  /** The map of class -> symbol */
  protected val typeClassSymbolMap: immutable.HashMap[Class[_], Symbol]
  /** The map of symbol -> class */
  protected lazy val typeSymbolClassMap: immutable.HashMap[Symbol, Class[_]] =
    immutable.HashMap[Symbol, Class[_]](typeClassSymbolMap.map(t => (t._2, t._1)).toSeq: _*)

  /**
   * Commit complex type (if needed) while saving
   */
  def commit(typeSymbol: Symbol, value: AnyRef with java.io.Serializable, element: Element) {}
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
object DSLType extends Loggable {
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
     * Commit complex type (if needed) while saving
     */
    def commit[T](value: Value[T], element: Element)(implicit m: Manifest[T]): Unit =
      classSymbolMap.get(m.runtimeClass).map(symbolType => commit(symbolType, value, element))
    /**
     * Commit complex type (if needed) while saving
     */
    def commit(typeSymbol: Symbol, value: AnyRef with java.io.Serializable, element: Element): Unit =
      symbolConverterMap.get(typeSymbol).foreach(converter => converter.commit(typeSymbol, value, element))
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
    /** DSLType implementation DI cache */
    lazy val implementation = inject[Interface]
    /**
     * Collection of DSL types.
     *
     * Each collected DSL type must be:
     *  1. an instance of DSLType
     *  2. has name that starts with "DSLType."
     */
    lazy val dsltypes = {
      val types = bindingModule.bindings.filter {
        case (key, value) => classOf[DSLType].isAssignableFrom(key.m.runtimeClass)
      }.map {
        case (key, value) =>
          key.name match {
            case Some(name) if name.startsWith("DSLType.") =>
              log.debug(s"'${name}' loaded.")
            case _ =>
              log.debug(s"'${key.name.getOrElse("Unnamed")}' DSL type skipped.")
          }
          bindingModule.injectOptional(key).asInstanceOf[Option[DSLType]]
      }.flatten.toSeq
      assert(types.distinct.size == types.size, "DSL types contains diplicated entities in " + types)
      types
    }
  }
}
