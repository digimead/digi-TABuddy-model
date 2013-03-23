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

package org.digimead.tabuddy.model.element

import scala.collection.mutable

import org.digimead.digi.lib.log.logger.RichLogger.rich2slf4j
import org.digimead.tabuddy.model.Model

/**
 * Element children container
 */
class Children(val origin: Element.Generic) extends ElementSet with mutable.SynchronizedSet[Element.Generic] {
  override def +=(element: Element.Generic): this.type = {
    origin.eStash.model.foreach(model => eAttach(model: Model.Generic, origin, element))
    super.+=(element)
  }
  override def -=(element: Element.Generic): this.type = {
    val result = super.-=(element)
    origin.eStash.model.foreach(model => eDetach(model, element))
    result
  }
  override def clear(): Unit = {
    origin.eStash.model.foreach(model =>
      this.foreach(child => eDetach(model, child)))
    super.clear
  }

  /**
   * Creates a new iterator over all elements contained in this iterable object and it's children.
   * @param transformChildren - function that provide sorting/filtering capability
   * @return the new iterator
   */
  def iteratorRecursive(transformChildren: (Children) => Seq[Element.Generic] = _.toSeq): Iterator[Element.Generic] =
    new Iterator[Element.Generic] {
      var iterator = Children.this.iterator
      var children = transformChildren(Children.this)
      def hasNext: Boolean = if (iterator.hasNext) true else {
        children.headOption match {
          case Some(head) =>
            iterator = head.eChildren.iteratorRecursive(transformChildren)
            children = children.tail
            iterator.hasNext
          case None =>
            false
        }
      }
      def next(): Element.Generic = iterator.next
    }

  /**
   * Attach element to the model
   */
  protected def eAttach(model: Model.Generic, container: Element.Generic, element: Element.Generic) = synchronized {
    log.debug("attach %s to %s".format(element.eReference, container.eReference))
    assert(element.eStash.model.isEmpty && !element.eChildren.iteratorRecursive().exists(_.eStash.model.nonEmpty),
      "Unable to reattach %s, please detach it first".format(element))
    assert(container.eStash.model == Some(model),
      "Unable to attach %s to unknown container %s".format(element, container))
    // modify element
    val newStash = element.eStash.copy(context = element.eStash.context.copy(container = container.eReference),
      model = container.eStash.model)
    Element.check(element, newStash) // throw exception
    element.eChildren.iteratorRecursive().foreach(_.eStash.model = Some(model))
    element.asInstanceOf[Element[Stash]].eStash = newStash // update stash with new that points to the new container and the model
    model.eAttach(element)
  }
  /**
   * Detach element from the model
   */
  protected def eDetach[T <: Element.Generic](model: Model.Generic, element: T, copy: Boolean = false): T = synchronized {
    val detached = if (copy) {
      log.debug("dettach copy of %s from %s".format(element.eReference, model.eReference))
      // detach copy
      element.eCopy()
    } else {
      // detach original
      model.eDetach(element)
      element
    }
    element.eFilter(_ => true).foreach(_.eStash.model = None)
    element.eStash.model = None
    element
  }
}
