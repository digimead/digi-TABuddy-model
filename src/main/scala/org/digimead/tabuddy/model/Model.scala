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

package org.digimead.tabuddy.model

import java.io.File
import java.util.UUID

import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.DynamicVariable

import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.aop.log
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.element.Children
import org.digimead.tabuddy.model.element.Context
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Reference
import org.digimead.tabuddy.model.element.Value

import com.escalatesoft.subcut.inject.BindingModule

import language.implicitConversions

/**
 * Class that implements model interface
 */
class Model[StashProjection <: Model.Stash](stashArg: StashProjection) extends Model.Interface[StashProjection] {
  stashArg.model = Some(this)
  eRegister(this)

  override def eModel = this
  def eStash = stashArg
  def eStash_=(value: StashProjection): Unit = {}

  /** create new instance with specific stash */
  protected def eNewInstance(stash: StashProjection): this.type = new Model(stash).asInstanceOf[this.type]

  override def toString() = "%s://%s[%s@GLOBAL]".format(eStash.context.container.origin.name, eStash.scope, eStash.id.name)
}

/**
 * Singleton that provides model interface implementation with ability
 * - load, save and reset model
 * - control context of elements
 * - save and retrieve element and context information
 */
object Model extends Loggable {
  type Generic = Interface[_ <: Stash]
  implicit def model2implementation(m: Model.type): Model.Generic = m.inner
  val scope = new Scope()

  /** Assert that A is not generic. */
  def assertNonGeneric[A](implicit m: Manifest[A]): Unit = if (m.runtimeClass == classOf[java.lang.Object])
    throw new IllegalArgumentException(s"Generic type '${m}' assertion failed.")
  def inner() = DI.inner
  def origin() = DI.origin

  /**
   * General model interface
   */
  trait Interface[StashProjection <: Model.Stash] extends Element[StashProjection] with ModelIndex {
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
    def contextBuildFromDocument(element: Element.Generic, line: Int): Option[Context] = {
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
    def contextForChild(container: Element.Generic, t: Option[StackTraceElement]): Context = t match {
      case Some(stack) if stack.getFileName() == "(inline)" && eStash.documentMap.value.nonEmpty =>
        // loaded as runtime Scala code && documentMap defined
        Context(container.eReference, None, Some(stack.getLineNumber()), None)
      case _ =>
        // everything other - virtual context
        Context.virtual(container)
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
    /**
     * Attach element to the model
     * Register element in various model indexes
     */
    def eAttach(element: Element.Generic): Unit = synchronized {
      log.debug("attach %s to %s".format(element, this))
      eRegister(element)
    }
    /** Copy constructor */
    override def eCopy(stash: Option[StashProjection], children: Option[List[Element.Generic]]): this.type = {
      val result = super.eCopy(stash, children)
      val iterator = result.eChildren.iteratorRecursive()
      iterator.foreach(element => element.eStash.model = Some(result.eModel))
      result
    }
    /**
     * Detach element from the model
     * Unregister element in various model indexes
     */
    def eDetach[T <: Element.Generic](element: T, copy: Boolean = false): Unit = synchronized {
      log.debug("dettach %s from %s".format(element.eReference, this.eReference))
      eUnregister(element)
    }
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
      // The model is always at the root
      eGet(id, typeSymbol)
    /** Get a container */
    override def eParent(): Option[Element.Generic] = None
    /** Get the root element from the current origin if any. */
    def eRoot() = Some(this).asInstanceOf[Option[this.type]]
    /** Get the root element from the particular origin if any */
    def eRoot(origin: Symbol) = Some(this).asInstanceOf[Option[this.type]]
    /** Reset current model */
    def reset(model: Model.Generic = inner) {
      log.debug("reset model")
      log.debug("dispose depricated " + this)
      // detach model
      eFilter(_ => true).foreach { element =>
        element.eStash.model = None
        element.eChildren.clear
      }
      eChildren.clear
      eIndexRebuid
      log.debug("activate " + model)
      val previous = inner
      DI.currentModel = Some(model)
      val undoF = () => {}
      Element.Event.publish(Element.Event.ModelReplace(previous, model, model.eModified)(undoF))
    }
    /** Stub for setter of model's data */
    def stash_=(value: Model.Stash) =
      throw new UnsupportedOperationException
  }
  /** The marker object that describes model scope */
  class Scope(override val modificator: Symbol = 'Model) extends Element.Scope(modificator) {
    def canEqual(other: Any): Boolean = other.isInstanceOf[org.digimead.tabuddy.model.Model.Scope]
  }
  /**
   * Model specific stash realization
   */
  class Stash(val created: Element.Timestamp, val id: Symbol, val unique: UUID, val property: org.digimead.tabuddy.model.element.Stash.Data) extends org.digimead.tabuddy.model.element.Stash {
    /** Common constructor for serialization  */
    def this(context: Context, coordinate: Coordinate, created: Element.Timestamp,
      id: Symbol, scope: Element.Scope, unique: UUID, property: org.digimead.tabuddy.model.element.Stash.Data) = this(created, id, unique, property)
    /** User constructor */
    def this(id: Symbol, unique: UUID) = this(Element.timestamp(), id, unique, new org.digimead.tabuddy.model.element.Stash.Data)
    lazy val context: Context = Context(Reference(Model.DI.origin, unique, Coordinate.root), None, None, None)
    /** Map of all sorted contexts by line number per file */
    val contextMap = new mutable.HashMap[Option[File], Seq[Context]] with mutable.SynchronizedMap[Option[File], Seq[Context]]
    lazy val coordinate: Coordinate = Coordinate.root
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
    /** Map of all model elements */
    val elements = new mutable.HashMap[UUID, Element.Generic] with mutable.SynchronizedMap[UUID, Element.Generic]
    lazy val scope: Element.Scope = Model.scope // ignore the custom scope

    def copy(context: Context = this.context,
      coordinate: Coordinate = this.coordinate,
      created: Element.Timestamp = this.created,
      id: Symbol = this.id,
      modified: Element.Timestamp = this.modified,
      scope: Element.Scope = this.scope,
      unique: UUID = this.unique,
      model: Option[Model.Generic] = this.model,
      property: org.digimead.tabuddy.model.element.Stash.Data = this.property) = {
      assert(context == this.context, "incorrect context %s, must be %s".format(context, this.context))
      assert(coordinate == this.coordinate, "incorrect coordinate %s, must be %s".format(context, this.context))
      assert(scope == this.scope, "incorrect scope %s, must be %s".format(scope, this.scope))
      val newStashCtor = this.getClass().getConstructor(
        classOf[Context],
        classOf[Coordinate],
        classOf[Element.Timestamp],
        classOf[Symbol],
        classOf[Element.Scope],
        classOf[UUID],
        classOf[org.digimead.tabuddy.model.element.Stash.Data])
      val data = new org.digimead.tabuddy.model.element.Stash.Data
      copyDeepProperty(property, data)
      val newStash = newStashCtor.newInstance(context, coordinate, created, id, scope, unique, data)
      newStash.model = model
      newStash.modified = modified
      newStash.asInstanceOf[this.type]
    }
    /** Validates stash on creation for circular reference */
    override protected def validateForSelfReference() {}
  }
  /**
   * Dependency injection routines
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    /** The current model cache */
    @volatile var currentModel: Option[Interface[_ <: Model.Stash]] = None
    /** The previous model cache */
    @volatile var previousModel: Option[Interface[_ <: Model.Stash]] = None

    def inner() = currentModel getOrElse {
      val model = inject[Interface[_ <: Model.Stash]]
      currentModel = Option(model)
      previousModel.foreach { previous =>
        val undoF = () => {}
        Element.Event.publish(Element.Event.ModelReplace(previous, model, model.eModified)(undoF))
      }
      model
    }
    /** The local origin that is alias of a user or a system or an anything other */
    def origin() = inject[Symbol]("Model.Origin")

    override def injectionCommit() = {
      currentModel = None
      inner()
    }
  }
}

