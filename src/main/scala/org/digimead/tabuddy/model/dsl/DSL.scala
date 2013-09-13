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

import java.util.UUID

import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.Record
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.LocationGeneric
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.ElementBox.box2interface
import org.digimead.tabuddy.model.predef.Note
import org.digimead.tabuddy.model.predef.Task

abstract class DSL() {
  /** Type that points to something with generic (common for every element) DSL routine. */
  type DSLGeneric = ElementGenericDSL

  trait ElementGenericDSL extends DSL.RichGeneric
    with Record.DSL.RichGeneric
    with Model.DSL.RichGeneric
    with Note.DSL.RichGeneric
    with Task.DSL.RichGeneric

  class ElementSpecificDSL[A <: Element](val element: A) extends DSL.RichSpecific[A] with DSLGeneric
  class RecordSpecificDSL[A <: Record.Like](e: A) extends ElementSpecificDSL(e) with Record.DSL.RichSpecific[A]
  class NoteSpecificDSL[A <: Note.Like](e: A) extends RecordSpecificDSL(e) with Note.DSL.RichSpecific[A]
  class TaskSpecificDSL[A <: Task.Like](e: A) extends NoteSpecificDSL(e) with Task.DSL.RichSpecific[A]
  class ModelSpecificDSL[A <: Model.Like](e: A) extends RecordSpecificDSL(e) with Model.DSL.RichSpecific[A]
}

object DSL {
  /** Base trait for element generic DSL builder. */
  trait RichGeneric {
    implicit val element: Element

    /** Create or retrieve child of the current element. */
    def |[A <: Element](l: LocationGeneric[A]): A =
      element.eNode.safeWrite { node ⇒
        node.iterator.find(node ⇒ node.id == l.id && l.unique.map(_ == node.unique).getOrElse(true)) match {
          case Some(child) ⇒
            child.safeWrite { child ⇒
              Option(child.rootElementBox).map(_.elementType).foreach(existsElementType ⇒
                if (!existsElementType.runtimeClass.isAssignableFrom(l.elementType.runtimeClass))
                  throw new IllegalArgumentException(s"Unable to cast ${l.elementType.runtimeClass} to ${existsElementType}."))
              child.getProjection(l.coordinate) match {
                case Some(box) ⇒ box.get.asInstanceOf[A]
                case None ⇒ ElementBox.getOrCreate(l.coordinate, child, l.scope, null)(l.elementType, l.stashClass)
              }
            }
          case None ⇒
            node.createChild(l.id, l.unique.getOrElse(UUID.randomUUID())).safeWrite { node ⇒
              ElementBox.getOrCreate(l.coordinate, node, l.scope, null)(l.elementType, l.stashClass)
            }
        }
      }
    /** Retrieve child of the current element. */
    def &[A <: Element](l: LocationGeneric[A]): A =
      element.eFind[A](e ⇒ e.eId == l.id && e.eCoordinate == l.coordinate &&
        l.unique.map(_ == e.eNodeId).getOrElse(true))(l.elementType).
        getOrElse { throw new IllegalArgumentException(s"Unable to find ${l}.") }

    /**
     * Create a new element or retrieve exists one and apply fTransform to.
     *
     * @return fTransform result
     */
    protected def withElement[A <: Element, B](id: Symbol, coordinate: Coordinate, scope: A#StashType#ScopeType, stash: Class[_ <: A#StashType],
      fTransform: A ⇒ B)(implicit m: Manifest[A]): B = {
      // Double checked locking
      element.eNode.safeRead { parentNode ⇒
        parentNode.find(_.id == id).flatMap(_.getProjection(coordinate).
          map(e ⇒ fTransform(e.get().asInstanceOf[A])))
      }.getOrElse {
        // Modify parent node.
        element.eNode.safeWrite { parentNode ⇒
          parentNode.find(_.id == id).map { childNode ⇒
            // node exists
            childNode.getProjection(coordinate).map(e ⇒ fTransform(e.get().asInstanceOf[A])).getOrElse {
              childNode.safeWrite { childNode ⇒ fTransform(ElementBox.getOrCreate[A](coordinate, childNode, scope, parentNode.rootElementBox.serialization)(m, stash)) }
            }
          } getOrElse {
            // node not exists
            parentNode.createChild(id, UUID.randomUUID()).safeWrite { childNode ⇒
              fTransform(ElementBox.getOrCreate[A](coordinate, childNode, scope, parentNode.rootElementBox.serialization)(m, stash))
            }
          }
        }
      }
    }
  }
  /** Base trait for element specific DSL builder. */
  trait RichSpecific[A <: Element] {
    implicit val element: A
  }
}
