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

package org.digimead.tabuddy.model

import java.io.File
import java.util.UUID

import scala.Option.option2Iterable
import scala.collection.mutable
import scala.util.DynamicVariable

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.aop.log
import org.digimead.digi.lib.log.Loggable
import org.digimead.digi.lib.log.logger.RichLogger.rich2slf4j

/**
 * Class that implements model interface
 */
class Model[A <: Model.Stash](s: A) extends Model.Interface {
  lazy val persistentStash = s
  if (persistentStash.model.isEmpty)
    persistentStash.model = Some(this)
  eRegister(this)

  override def model = this
  override def model_=(value: Model.Interface) = throw new UnsupportedOperationException
  override def toString() = "%s[%s] %s".format(stash.scope, stash.id.toString, stash.unique.toString)
}

/**
 * Singleton that provides model interface implementation with ability
 * - load, save and reset model
 * - control context of elements
 * - save and retrieve element and context information
 */
object Model extends DependencyInjection.PersistentInjectable with Loggable {
  implicit def model2implementation(m: Model.type): Interface = m.implementation
  implicit def bindingModule = DependencyInjection()
  @volatile private var implementation: Interface = inject[Interface]

  def inner() = implementation
  def commitInjection() {}
  def updateInjection() { implementation = inject[Interface] }

  /**
   * General model interface
   */
  trait Interface extends Element[Stash, Interface] with ModelIndex {
    /** Map of all model elements */
    val elements = new mutable.HashMap[UUID, Element[_, _]] with mutable.SynchronizedMap[UUID, Element[_, _]]
    /** Map of all sorted contexts by line number per file */
    val contextMap = new mutable.HashMap[Option[File], Seq[Element.Context]] with mutable.SynchronizedMap[Option[File], Seq[Element.Context]]
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
    @transient protected val documentMap = new DynamicVariable[Seq[Element.Context]](Seq())
    /**
     * Model persistent stash
     */
    val persistentStash: Model.Stash

    /**
     * Add context information to context map
     */
    @log
    def contextAdd(context: Element.Context): Unit = {
      log.info("add new context [%s]".format(context))
      contextMap.get(context.file) match {
        case Some(fileMap) =>
          contextMap(context.file) = (contextMap(context.file) :+ context).sortBy(_.line)
        case None =>
          contextMap(context.file) = Seq(context)
      }
    }
    /**
     * Delete context information from context map if any
     */
    @log
    def contextDel(context: Element.Context) = {
      log.info("delete context [%s]".format(context))
    }
    /**
     * Get context information by file/line
     */
    @log
    def contextGet(file: Option[File], line: Int): Option[Element.Context] = {
      None
    }
    /**
     * Create context information from document map for element
     */
    @log
    def contextBuildFromDocument(element: Element.Generic, line: Int): Option[Element.Context] = {
      val map = documentMap.value
      if (line < 1) return None
      if (map.isEmpty) return None
      for (i <- 0 until map.size) yield {
        map(i).line match {
          case Some(mapLine) if mapLine > line =>
            if (i == 0) {
              log.fatal("incorrect shift for buildContextFromDocument want: %s, i: %d, map: %s".format(line, i, map.mkString("\n")))
              return None
            } else
              return Some(Element.Context(element.reference, map(i - 1).file, Some(line - (map.head.line.getOrElse(0) - 1)), map(i - 1).digest))
          case _ =>
        }
      }
      Some(Element.Context(element.reference, map.last.file, Some(line - (map.head.line.getOrElse(0) - 1)), map.last.digest))
    }
    /**
     * Create context information from the specific container
     */
    def contextForChild(container: Element.Generic, t: Option[StackTraceElement]): Element.Context = t match {
      case Some(stack) if stack.getFileName() == "(inline)" && documentMap.value.nonEmpty =>
        // loaded as runtime Scala code && documentMap defined
        Element.Context(container.reference, None, Some(stack.getLineNumber()), None)
      case _ =>
        // everything other - virtual context
        Element.virtualContext(container)
    }
    /**
     * Set current thread local context information
     * for document parser, for example
     */
    @log
    def contextSet(documentMap: Seq[Element.Context]) {
      if (documentMap.nonEmpty)
        log.debugWhere("set local document context [%s]".format(documentMap.mkString(", ")))
      else
        log.debugWhere("reset local document context")
      this.documentMap.value = documentMap
    }
    /** Dump the model content. */
    def dump(padding: Int): String =
      throw new UnsupportedOperationException
    /**
     * Attach element to the model
     */
    def eAttach(container: Element.Generic, element: Element.Generic) {
      log.debug("attach %s to %s".format(element, container))
      val all = element.filter(_ => true)
      val attached = all.filter(_.stash.model.nonEmpty)
      assert(attached.isEmpty, "Unable to reattach %s, please detach it first".format(element))
      assert(container.stash.model == Some(this),
        "Unable to attach %s to unknown container %s".format(element, container))
      all.foreach(_.stash.model = Some(this))
      val newStash = element.stash.copy(context = element.stash.context.copy(container = container.reference),
        model = container.stash.model)
      Element.check(element, newStash) // throw exception
      element.stash.model = stash.model
      container.children = container.children :+ element
      eRegister(element)
    }
    /**
     * Detach element from the model
     */
    def eDetach[T <: Element.Generic](element: T, copy: Boolean = false): T = {
      val detached = if (copy) {
        element.copy()
      } else {
        model.e(element.stash.context.container).foreach(parent =>
          parent.children = parent.children.filterNot(_ == element))
        element
      }
      element.filter(_ => true).foreach(_.stash.model = None)
      element.stash.model = None
      element
    }
    /** Get a property or else get the property from the root element. */
    def getOrElseRoot[A <: java.io.Serializable](id: Symbol)(implicit m: Manifest[A]): Option[Value[A]] = get(id)
    /** Reset current model */
    def reset(model: Model.Interface = inject[Model.Interface]) {
      log.debug("reset model")
      log.debug("dispose depricated " + this)
      filter(_ => true).foreach { element =>
        element.stash.model = None
        element.children = List()
      }
      children = List()
      eIndexRebuid
      log.debug("activate " + model)
      implementation = model
    }
    /** Get the root element from the current origin if any. */
    def root() = Some(this)
    /** Get the root element from the particular origin if any */
    def root(origin: Symbol) = Some(this)
    /** Getter of model's data. */
    def stash = persistentStash
    /** Stub for setter of model's data */
    def stash_=(value: Model.Stash) =
      throw new UnsupportedOperationException
  }
  class Stash(val id: Symbol, val unique: UUID, var model: Option[Model.Interface],
    val property: org.digimead.tabuddy.model.Stash.Data) extends org.digimead.tabuddy.model.Stash {
    /** Common constructor for serialization  */
    def this(context: Element.Context, coordinate: Element.Coordinate, id: Symbol, unique: UUID,
      model: Option[Model.Interface], property: org.digimead.tabuddy.model.Stash.Data) =
      this(id, unique, model, property)
    /** User constructor */
    def this(id: Symbol, unique: UUID) = this(id, unique, None, new org.digimead.tabuddy.model.Stash.Data)
    lazy val context: Element.Context = Element.Context(Element.Reference(id, unique, Element.Coordinate.root), None, None, None)
    lazy val coordinate: Element.Coordinate = Element.Coordinate.root
    val scope: String = "Model"

    def copy(context: Element.Context = this.context,
      coordinate: Element.Coordinate = this.coordinate,
      id: Symbol = this.id,
      scope: String = this.scope,
      unique: UUID = this.unique,
      model: Option[Model.Interface] = this.model,
      lastModification: Long = this.lastModification,
      property: org.digimead.tabuddy.model.Stash.Data = this.property) = {
      assert(context == this.context, "incorrect context %s, must be %s".format(context, this.context))
      assert(coordinate == this.coordinate, "incorrect coordinate %s, must be %s".format(context, this.context))
      assert(scope == this.scope, "incorrect scope %s, must be %s".format(scope, this.scope))
      val newStash = new Stash(id, unique, model, property)
      newStash.lastModification = lastModification
      newStash
    }
  }
}
