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

import org.digimead.tabuddy.model.predef.Note
import org.digimead.tabuddy.model.predef.Task
import org.digimead.tabuddy.model.dsl.DSL
import org.digimead.tabuddy.model.element.Element

import language.implicitConversions

/* with Model.DSL.RichElement[A]
  with Record.DSL.RichElement[A]
  with Note.DSL.RichElement[A]
  with Task.DSL.RichElement[A]*/

object TestDSL extends DSL with Record.DSL with Model.DSL {
  implicit def e2DSL[A <: Element](e: A) = new ElementSpecificDSL(e)
  implicit def me2DSL[A <: Element](me: Element.Mutable[A]) = new ElementSpecificDSL(me.element)
  implicit def eRecord2DSL[A <: Record.Like](e: A) = new RecordSpecificDSL(e)
  implicit def meRecord2DSL[A <: Record.Like](me: Element.Mutable[A]) = new RecordSpecificDSL(me.element)
  implicit def eNote2DSL[A <: Note.Like](e: A) = new NoteSpecificDSL(e)
  implicit def meNote2DSL[A <: Note.Like](me: Element.Mutable[A]) = new NoteSpecificDSL(me.element)
  implicit def eTask2DSL[A <: Task.Like](e: A) = new TaskSpecificDSL(e)
  implicit def meTask2DSL[A <: Task.Like](me: Element.Mutable[A]) = new TaskSpecificDSL(me.element)
  implicit def eModel2DSL[A <: Model.Like](e: A) = new ModelSpecificDSL(e)
  implicit def meModel2DSL[A <: Model.Like](me: Element.Mutable[A]) = new ModelSpecificDSL(me.element)

  implicit val recordStashClass: Class[_ <: Record.Stash] = classOf[Record.Stash]
  implicit val modeStashClass: Class[_ <: Model.Stash] = classOf[Model.Stash]
}
