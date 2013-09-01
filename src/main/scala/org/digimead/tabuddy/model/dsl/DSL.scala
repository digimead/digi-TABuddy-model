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

import org.digimead.digi.lib.aop.log
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.Record
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.LocationGeneric
import scala.language.implicitConversions
import java.util.UUID
import org.digimead.tabuddy.model.serialization.Serialization
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.element.Coordinate
import scala.ref.WeakReference
import java.util.concurrent.atomic.AtomicReference
import org.digimead.tabuddy.model.graph.Context
import org.digimead.tabuddy.model.element.Stash

trait DSL[Rich <: DSL.RichElement] {
  val builder: Element => Rich
  implicit def element2rich(e: Element): Rich = builder(e)
  //implicit def model2rich(e: Model.type): T = builder(e)

  def model()
}

object DSL {
  /**
   * Base class for DSL builders
   */
  trait RichElement {
    val element: Element

    /**
     * Create or retrieve element child
     */
    /*def |[A <: Record.Interface[B], B <: Record.Stash](l: LocationGeneric[A, B])(implicit ma: Manifest[A], mb: Manifest[B]): A =
      Record(ma.runtimeClass.asInstanceOf[Class[A]], mb.runtimeClass.asInstanceOf[Class[B]], Some(DLS_element), l.id, l.scope, l.coordinate.coordinate, (n: A) => {})
    /**
     * Retrieve element child if any
     */
    def &[A <: Record.Interface[B], B <: Record.Stash](l: LocationGeneric[A, B])(implicit ma: Manifest[A], mb: Manifest[B]): Option[A] =
      DLS_element.eFind[A, B](l.id, l.coordinate)*/
  }
}
