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
import scala.language.implicitConversions
import scala.ref.WeakReference
import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Stash
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.element.Element
import org.digimead.digi.lib.aop.log
import org.digimead.digi.lib.log.api.Loggable
import java.util.UUID
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Reference
import scala.util.DynamicVariable
import scala.collection.mutable
import java.io.File
import org.digimead.tabuddy.model.element.Value
import java.net.URI
import org.digimead.tabuddy.model.graph.Context
import org.digimead.tabuddy.model.graph.Node

/**
 * Common model.
 * Any concrete model may be represent as this trait.
 */
class Model(stashArg: Model.Stash)(@transient implicit val eBox: ElementBox[Model])
  extends Model.Like with ModelIndex with Loggable {
  type StashType = Model.Stash
  type ElementType = Model

  def eStash = stashArg
}

/**
 * Singleton that provides model interface implementation with ability
 * - load, save and reset model
 * - control context of elements
 * - save and retrieve element and context information
 */
object Model extends Loggable {
  val scope = new Scope()

  /** Create model */
  //  def apply(coordinate: Coordinate = Coordinate.root, created: Element.Timestamp, id: Symbol,
  //    modified: Element.Timestamp, origin: Symbol, property: Stash.Data = new Stash.Data,
  //    unique: UUID)(implicit box: WeakReference[ElementBox[Model]]): Model =
  //    new Model(new Stash(coordinate, created, id, modified, origin, property, Model.scope, unique))

  /** Assert that A is not generic. */
  def assertNonGeneric[A](implicit m: Manifest[A]): Unit = if (m.runtimeClass == classOf[java.lang.Object])
    throw new IllegalArgumentException(s"Generic type '${m}' assertion failed.")
  //def create() = DI.createModel
  /** The local origin that is alias of a user or a system or an anything other */
  def defaultOrigin() = DI.defaultOrigin

  trait Like extends Element {
    this: ModelIndex with Loggable =>
    type StashType <: Model.Stash.Like
    type ElementType <: Like

    //stashArg.model = Some(this)
    //eRegister(this)

    //override def eModel = this
    //def eStash = stashArg
    //def eStash_=(value: StashProjection): Unit = {}

    /** create new instance with specific stash */
    //protected def eNewInstance(stash: StashProjection): this.type = new Model(stash).asInstanceOf[this.type]

    //override def toString() = "%s://%s[%s@GLOBAL]".format(eStash.context.container.origin.name, eStash.scope, eStash.id.name)
    /**
     * Add context information to context map
     */
    @log
    def contextAdd(context: Context): Unit = {
      log.info("add new context [%s]".format(context))
      eStash.contextMap.get(context.file) match {
        case Some(fileMap) =>
          eStash.contextMap(context.file) = (eStash.contextMap(context.file) :+ context).sortBy(_.line)
        case None =>
          eStash.contextMap(context.file) = Seq(context)
      }
    }
    /**
     * Delete context information from context map if any
     */
    @log
    def contextDel(context: Context) = {
      log.info("delete context [%s]".format(context))
    }
    /**
     * Get context information by file/line
     */
    @log
    def contextGet(file: Option[File], line: Int): Option[Context] = {
      None
    }
    /**
     * Create context information from document map for element
     */
    @log
    def contextBuildFromDocument(element: Element, line: Int): Option[Context] = {
      val map = eStash.documentMap.value
      if (line < 1) return None
      if (map.isEmpty) return None
      for (i <- 0 until map.size) yield {
        map(i).line match {
          case Some(mapLine) if mapLine > line =>
            if (i == 0) {
              log.fatal("incorrect shift for buildContextFromDocument want: %s, i: %d, map: %s".format(line, i, map.mkString("\n")))
              return None
            } else
              return Some(Context(element.eReference, map(i - 1).file, Some(line - (map.head.line.getOrElse(0) - 1)), map(i - 1).digest))
          case _ =>
        }
      }
      Some(Context(element.eReference, map.last.file, Some(line - (map.head.line.getOrElse(0) - 1)), map.last.digest))
    }
    /**
     * Create context information from the specific container
     */
    def contextForChild(container: Element, t: Option[StackTraceElement]): Context = t match {
      case Some(stack) if stack.getFileName() == "(inline)" && eStash.documentMap.value.nonEmpty =>
        // loaded as runtime Scala code && documentMap defined
        Context(container.eReference, None, Some(stack.getLineNumber()), None)
      case _ =>
        // everything other - virtual context
        Context(container)
    }
    /**
     * Set current thread local context information
     * for document parser, for example
     */
    @log
    def contextSet(documentMap: Seq[Context]) {
      if (documentMap.nonEmpty)
        log.debugWhere("set local document context [%s]".format(documentMap.mkString(", ")))
      else
        log.debugWhere("reset local document context")
      eStash.documentMap.value = documentMap
    }
    def name = eGetOrElseRoot[String]('name).map(_.get) getOrElse ""
    def name_=(value: String) = eSet('name, value, "")
    /** Dump the model content. */
    def eDump(brief: Boolean, padding: Int = 2): String = synchronized {
      def dumpProperties() = {
        val result = eStash.property.map {
          case (id, sequence) =>
            sequence.map {
              case (typeSymbol, value) =>
                "%s: %s".format(id, value)
            }
        }.flatten
        if (result.nonEmpty) "\n  " + result.toSeq.sorted.mkString("\n  ") else ""
      }
      val pad = " " * padding
      val properties = if (brief) "" else dumpProperties()
      val self = "%s: %s".format(eStash.scope, eStash.id) + properties
      val childrenDump = eChildren.map(_.eDump(brief, padding)).mkString("\n").split("\n").map(pad + _).mkString("\n").trim
      if (childrenDump.isEmpty) self else self + "\n" + pad + childrenDump
    }
    /** Get a property or else get the property from the root element. */
    def eGetOrElseRoot(id: Symbol, typeSymbol: Symbol): Option[Value[_ <: AnyRef with java.io.Serializable]] =
      // TODO
      // NOT TRUEThe model is always at the root
      eGet(id, typeSymbol)
    /** Get a container */
    override def eParent(): Option[Node] = None
  }
  /** The marker object that describes model scope */
  class Scope(override val modificator: Symbol = 'Model) extends Element.Scope(modificator) {
    def canEqual(other: Any): Boolean = other.isInstanceOf[org.digimead.tabuddy.model.Model.Scope]
  }
  /**
   * Model common stash trait.
   * Any concrete model's stash may be represent as this trait.
   */
  class Stash(val coordinate: Coordinate,
    val created: Element.Timestamp,
    val id: Symbol,
    val modified: Element.Timestamp,
    val origin: Symbol,
    val property: org.digimead.tabuddy.model.element.Stash.Data,
    val scope: Model.Scope,
    val unique: UUID) extends Stash.Like {
    /** Stash type. */
    type StashType = Stash
    /** Scope type. */
    type ScopeType = Model.Scope
  }
  object Stash {
    trait Like extends org.digimead.tabuddy.model.element.Stash.Like {
      /** Stash type. */
      type Stash <: Like
      /** Scope type. */
      type ScopeType <: Model.Scope
      /** Map of all sorted contexts by line number per file */
      val contextMap = new mutable.HashMap[Option[URI], Seq[Context]] with mutable.SynchronizedMap[Option[URI], Seq[Context]]
      /**
       * Thread local context, empty - REPL, non empty - document
       * for example, after include preprocessor:
       * line 3, File A, digest File A
       * line 100, File B, digest File B
       * line 150, File C, digest File C
       * line 300, File A, digest File A
       *
       * so line 1 .. 2 - runtime header
       * line 3 .. 99 - file A
       * line 100 .. 149 - file B (include)
       * line 150 .. 299 - file C (include)
       * line 300 .. end - file A
       */
      @transient val documentMap = new DynamicVariable[Seq[Context]](Seq())
    }
  }
  /**
   * Dependency injection routines.
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    def createModel() = {
      val model = inject[Model.Like]
      //      currentModel = Option(model)
      //      previousModel.foreach { previous =>
      //        val undoF = () => {}
      //        Element.Event.publish(Element.Event.ModelReplace(previous, model, model.eModified)(undoF))
      //      }
      model
    }
    /** The local origin that is alias of a user or a system or an anything other */
    def defaultOrigin() = inject[Symbol]("Model.Origin")
  }
}
