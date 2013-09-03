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

import scala.collection.mutable
import scala.ref.WeakReference

/**
 * Element children container
 */
class NodeChildren(val containerNode: WeakReference[Node]) extends mutable.LinkedHashSet[Node] {
  override def +=(elementNode: Node): this.type = {
    elementNode.threadSafe { node ⇒ node.parentNode = containerNode.get }
    super.+=(elementNode)
  }
  override def -=(elementNode: Node): this.type = {
    val result = super.-=(elementNode)
    elementNode.threadSafe { node ⇒ node.parentNode = None }
    result
  }
  override def clear(): Unit = {
    this.foreach(childNode ⇒ childNode.threadSafe { node ⇒ node.parentNode = None })
    super.clear
  }

  /**
   * Creates a new iterator over all elements contained in this iterable object and it's children.
   * @param transformNodeChildren - function that provide sorting/filtering capability
   * @return the new iterator
   */
  def iteratorRecursive(transformNodeChildren: (Node, NodeChildren) ⇒ Seq[Node] = (parent, children) ⇒ children.toSeq): Iterator[Node] =
    new Iterator[Node] {
      val iterator = containerNode.get.map { parent ⇒
        transformNodeChildren(parent, NodeChildren.this).foldLeft(Iterator[Node]())((acc, child) ⇒
          acc ++ Iterator(child) ++ child.threadSafe { _.children.iteratorRecursive(transformNodeChildren) })
      }.getOrElse(Iterator[Node]())
      def hasNext: Boolean = iterator.hasNext
      def next(): Node = iterator.next
    }
}
