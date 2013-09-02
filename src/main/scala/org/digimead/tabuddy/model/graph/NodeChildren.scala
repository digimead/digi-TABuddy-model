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

package org.digimead.tabuddy.model.graph

import scala.ref.WeakReference

/**
 * Element children container
 */
class NodeChildren(val containerNode: WeakReference[Node]) extends NodeSet {
  override def +=(elementNode: Node): this.type = {
    elementNode.threadSafe { node => node.parentNode = containerNode.get }
    super.+=(elementNode)
  }
  override def -=(elementNode: Node): this.type = {
    val result = super.-=(elementNode)
    elementNode.threadSafe { node => node.parentNode = None }
    result
  }
  override def clear(): Unit = {
    this.foreach(childNode => childNode.threadSafe { node => node.parentNode = None })
    super.clear
  }

  /**
   * Creates a new iterator over all elements contained in this iterable object and it's children.
   * @param transformNodeChildren - function that provide sorting/filtering capability
   * @return the new iterator
   */
  def iteratorRecursive(transformNodeChildren: (Node, NodeChildren) => Seq[Node] = (parent, children) => children.toSeq): Iterator[Node] =
    new Iterator[Node] {
      val iterator = containerNode.get.map { parent =>
        transformNodeChildren(parent, NodeChildren.this).foldLeft(Iterator[Node]())((acc, child) =>
          acc ++ Iterator(child) ++ child.threadSafe { _.children.iteratorRecursive(transformNodeChildren) })
      }.getOrElse(Iterator[Node]())
      def hasNext: Boolean = iterator.hasNext
      def next(): Node = iterator.next
    }

  /**
   * Attach element to the model
   */
  /*protected def eAttach(model: Model, container: Element, element: Element) = synchronized {
    log.debug("attach %s to %s".format(element.eReference, container.eReference))
    assert(element.eStash.model.isEmpty && !element.eNodeChildren.iteratorRecursive().exists(_.get.eStash.model.nonEmpty),
      "Unable to reattach %s, please detach it first".format(element))
    assert(container.eStash.model == Some(model),
      "Unable to attach %s to unknown container %s".format(element, container))
    // modify element
    val newStash = element.eStash.copy(context = element.eStash.context.copy(container = container.eReference),
      model = container.eStash.model)
    //Element.check(element, newStash) // throw exception
    //element.eNodeChildren.iteratorRecursive().foreach { element =>
    //  element.eParent.get.eNodeChildren.update(element.eCopy(stash = element.eStash.copy(model = Some(model))), true)
   // }
    //element.asInstanceOf[Element].eStash = newStash // update stash with new that points to the new container and the model
    //model.eAttach(element)
  }*/
  /**
   * Detach element from the model
   */
  /*protected def eDetach[T <: Element](model: Model, element: T, copy: Boolean = false): T = synchronized {
    val detached = if (copy) {
      //log.debug("dettach copy of %s from %s".format(element.eReference, model.eReference))
      // detach copy
      element.eCopy()
    } else {
      // detach original
      model.eDetach(element)
      element
    }
    //element.eFilter(_ => true).foreach(_.eStash.model = None)
    //element.eStash.model = None
    element
  }*/
}
