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

import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.DynamicVariable

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.aop.log
import org.digimead.digi.lib.log.Loggable
import org.digimead.digi.lib.log.logger.RichLogger.rich2slf4j

/**
 * Class that implements model interface
 */
class Model[StashProjection <: Model.Stash](stashArg: StashProjection) extends Model.Interface[StashProjection] {
  stashArg.model = Some(this)
  eRegister(this)

  override def eModel = this
  override def eModel_=(value: Model.Generic) = throw new UnsupportedOperationException
  def eStash = stashArg
  def eStash_=(value: StashProjection): Unit = {}
  override def toString() = "%s[%s] %s".format(eStash.scope, eStash.id.toString, eStash.unique.toString)
}

/**
 * Singleton that provides model interface implementation with ability
 * - load, save and reset model
 * - control context of elements
 * - save and retrieve element and context information
 */
object Model extends DependencyInjection.PersistentInjectable with Loggable {
  type Generic = Interface[_ <: Stash]
  implicit def model2implementation(m: Model.type): Model.Generic = m.implementation
  implicit def bindingModule = DependencyInjection()
  @volatile private var implementation: Model.Generic = inject[Interface[Stash]]
  @volatile var snapshots = Seq[Long]()

  def inner() = implementation
  def commitInjection() {}
  def updateInjection() { implementation = inject[Interface[_ <: Model.Stash]] }

  /**
   * General model interface
   */
  trait Interface[StashProjection <: Model.Stash] extends Element[StashProjection] with ModelIndex {
    /**
     * Add context information to context map
     */
    @log
    def contextAdd(context: Element.Context): Unit = {
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
              return Some(Element.Context(element.eReference, map(i - 1).file, Some(line - (map.head.line.getOrElse(0) - 1)), map(i - 1).digest))
          case _ =>
        }
      }
      Some(Element.Context(element.eReference, map.last.file, Some(line - (map.head.line.getOrElse(0) - 1)), map.last.digest))
    }
    /**
     * Create context information from the specific container
     */
    def contextForChild(container: Element.Generic, t: Option[StackTraceElement]): Element.Context = t match {
      case Some(stack) if stack.getFileName() == "(inline)" && eStash.documentMap.value.nonEmpty =>
        // loaded as runtime Scala code && documentMap defined
        Element.Context(container.eReference, None, Some(stack.getLineNumber()), None)
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
      eStash.documentMap.value = documentMap
    }
    def description = eGetOrElseRoot[String]('description).map(_.get) getOrElse ""
    def description_=(value: String) = eSet('description, value)
    /** Dump the model content. */
    def eDump(padding: Int = 2): String = {
      val pad = " " * padding
      val self = "%s: %s".format(eStash.scope, eStash.id)
      val childrenDump = elementChildren.map(_.eDump(padding)).mkString("\n").split("\n").map(pad + _).mkString("\n").trim
      if (childrenDump.isEmpty) self else self + "\n" + pad + childrenDump
    }
    /**
     * Attach element to the model
     */
    def eAttach(container: Element.Generic, element: Element.Generic) {
      log.debug("attach %s to %s".format(element, container))
      val all = element.eFilter(_ => true)
      val attached = all.filter(_.eStash.model.nonEmpty)
      assert(attached.isEmpty, "Unable to reattach %s, please detach it first".format(element))
      assert(container.eStash.model == Some(this),
        "Unable to attach %s to unknown container %s".format(element, container))
      all.foreach(_.eStash.model = Some(this))
      val newStash = element.eStash.copy(context = element.eStash.context.copy(container = container.eReference),
        model = container.eStash.model)
      Element.check(element, newStash) // throw exception
      element.eStash.model = eStash.model
      container.elementChildren += element
      eRegister(element)
    }
    /**
     * Detach element from the model
     */
    def eDetach[T <: Element.Generic](element: T, copy: Boolean = false): T = {
      val detached = if (copy) {
        element.eCopy()
      } else {
        eModel.e(element.eStash.context.container).foreach(parent => parent.elementChildren -= element)
        element
      }
      element.eFilter(_ => true).foreach(_.eStash.model = None)
      element.eStash.model = None
      element
    }
    /** Get a property or else get the property from the root element. */
    def eGetOrElseRoot[A <: java.io.Serializable](id: Symbol)(implicit m: Manifest[A]): Option[Value[A]] = eGet[A](id)
    /** Reset current model */
    def reset(model: Model.Generic = inject[Interface[Stash]]) {
      log.debug("reset model")
      log.debug("dispose depricated " + this)
      eFilter(_ => true).foreach { element =>
        element.eStash.model = None
        element.elementChildren.clear
      }
      elementChildren.clear
      eIndexRebuid
      log.debug("activate " + model)
      implementation = model
    }
    /** Get the root element from the current origin if any. */
    def eRoot() = Some(this).asInstanceOf[Option[this.type]]
    /** Get the root element from the particular origin if any */
    def eRoot(origin: Symbol) = Some(this).asInstanceOf[Option[this.type]]
    /** Stub for setter of model's data */
    def stash_=(value: Model.Stash) =
      throw new UnsupportedOperationException
  }
  class Stash(val created: Stash.Timestamp, val id: Symbol, val unique: UUID, val property: org.digimead.tabuddy.model.Stash.Data) extends org.digimead.tabuddy.model.Stash {
    /** Common constructor for serialization  */
    def this(context: Element.Context, coordinate: Element.Coordinate, created: Stash.Timestamp,
      id: Symbol, unique: UUID, property: org.digimead.tabuddy.model.Stash.Data) = this(created, id, unique, property)
    /** User constructor */
    def this(id: Symbol, unique: UUID) = this(Stash.timestamp, id, unique, new org.digimead.tabuddy.model.Stash.Data)
    lazy val context: Element.Context = Element.Context(Element.Reference(id, unique, Element.Coordinate.root), None, None, None)
    /** Map of all sorted contexts by line number per file */
    val contextMap = new mutable.HashMap[Option[File], Seq[Element.Context]] with mutable.SynchronizedMap[Option[File], Seq[Element.Context]]
    lazy val coordinate: Element.Coordinate = Element.Coordinate.root
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
    @transient val documentMap = new DynamicVariable[Seq[Element.Context]](Seq())
    /** Map of all model elements */
    val elements = new mutable.HashMap[UUID, Element.Generic] with mutable.SynchronizedMap[UUID, Element.Generic]
    val scope: String = "Model"

    def copy(context: Element.Context = this.context,
      coordinate: Element.Coordinate = this.coordinate,
      created: Stash.Timestamp = this.created,
      id: Symbol = this.id,
      modified: Stash.Timestamp = this.modified,
      scope: String = this.scope,
      unique: UUID = this.unique,
      model: Option[Model.Generic] = this.model,
      property: org.digimead.tabuddy.model.Stash.Data = this.property) = {
      assert(context == this.context, "incorrect context %s, must be %s".format(context, this.context))
      assert(coordinate == this.coordinate, "incorrect coordinate %s, must be %s".format(context, this.context))
      assert(scope == this.scope, "incorrect scope %s, must be %s".format(scope, this.scope))
      val newStashCtor = this.getClass().getConstructor(
        classOf[Element.Context],
        classOf[Element.Coordinate],
        classOf[Stash.Timestamp],
        classOf[Symbol],
        classOf[UUID],
        classOf[org.digimead.tabuddy.model.Stash.Data])
      val newStash = newStashCtor.newInstance(context, coordinate, id, created, unique, property)
      newStash.model = model
      newStash.modified = modified
      newStash
    }
  }
}

