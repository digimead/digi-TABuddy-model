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

package org.digimead.tabuddy.model.element

import scala.collection.mutable

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
  def iteratorRecursive(transformChildren: (Element.Generic, Children) => Seq[Element.Generic] = (parent, children) => children.toSeq): Iterator[Element.Generic] =
    new Iterator[Element.Generic] {
      val iterator = transformChildren(origin, Children.this).foldLeft(Iterator[Element.Generic]())((acc, child) =>
        acc ++ Iterator(child) ++ child.eChildren.iteratorRecursive(transformChildren))
      def hasNext: Boolean = iterator.hasNext
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
