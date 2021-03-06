/**
 * TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2012-2014 Alexey Aksenov ezh@ezh.msk.ru
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

package org.digimead.tabuddy.model.serialization

import scala.collection.mutable

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.XLoggable
import org.digimead.lib.test.LoggingHelper
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.ModelIndex
import org.digimead.tabuddy.model.Record
//import org.digimead.tabuddy.model.TestDSL._
import org.digimead.tabuddy.model.dsl.DSLType
//import org.digimead.tabuddy.model.element.Context
import org.digimead.tabuddy.model.element.Value
//import org.digimead.tabuddy.model.predef.Note
import org.scalatest.FunSpec
import org.scalatest.Matchers

class ProtobufSerializationSpec extends FunSpec with Matchers with LoggingHelper with XLoggable {
  before { DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false) }

  describe("A ProtobufSerialization") {
    it("should provide serialization mechanism for Model") {
      //      Model.reset()
      /*val record = Model.record('root) { r => }
      val note = Model.note('note) { n => }
      val task = Model.task('task) { t => }
      Model.eChildren should have size (3)
      Model.eStash.property('a) =
        mutable.HashMap(
          DSLType.classSymbolMap(classOf[java.lang.String]) -> new Value.Static("123", Context.virtual(Model.inner.asInstanceOf[Element.Generic])),
          DSLType.classSymbolMap(classOf[java.lang.Integer]) -> new Value.Static(234, Context.virtual(Model.inner.asInstanceOf[Element.Generic])))
      // serialize
      val s = new ProtobufSerialization
      var frozen: Seq[Array[Byte]] = Seq()
      def fFilter(element: Element.Generic): Option[Element.Generic] = {
        log.___glance("filter element %s with ancestors %s".format(element, element.eAncestorReferences.mkString(",")))
        element match {
          case model: Model.Generic =>
            val index = model.getClass.getDeclaredMethod("index")
            val hash = index.invoke(model).asInstanceOf[ModelIndex#HashMapPerId] // @transient
            log.___glance("index hash " + hash)
          case element =>
        }
        Some(element)
      }
      def fSave(element: Element.Generic, data: Array[Byte]) {
        log.___glance("save element %s, serialized data size is %d bytes".format(element, data.size))
        assert(data.size > 0, "incorrect data size")
        frozen = frozen :+ data
      }
      s.freeze(Model, fSave, fFilter)
      frozen should not be ('empty)
      frozen = scala.util.Random.shuffle(frozen)
      // deserialize
      val frozenIterator = frozen.iterator
      def fLoad() = if (frozenIterator.hasNext) {
        val data = frozenIterator.next
        log.___glance("load element with size %d bytes".format(data.size))
        assert(data.size > 0, "incorrect data size")
        Some(data)
      } else None
      val deserializedModel = s.acquire[Model[Model.Stash], Model.Stash](fLoad, fFilter).get

      // check
      // model
      deserializedModel.eChildren should have size (3)
      // container always point to current active model
      deserializedModel.eStash.context.container should be(Model.eReference)
      deserializedModel.eStash.id.name should be(Model.eStash.id.name)
      deserializedModel.eStash.unique should be(Model.eStash.unique)
      deserializedModel.eStash.modified should be(Model.eStash.modified)
      deserializedModel.eStash.property should be(Model.eStash.property)
      deserializedModel.eStash.property('a)(DSLType.classSymbolMap(classOf[java.lang.String])).get should be("123")
      deserializedModel.eStash.property('a)(DSLType.classSymbolMap(classOf[java.lang.Integer])).get should be(234)
      // record
      deserializedModel.eChildren.head.eq(Model.eChildren.head) should be(false)
      deserializedModel.e(deserializedModel.eReference) should not be ('empty)
      deserializedModel.e(deserializedModel.eChildren.head.eReference) should not be ('empty)*/
    }
    it("should provide serialization mechanism for Element") {
      //Model.reset()
      /*var save: Record[Record.Stash] = null
      // this model is around 160 bytes
      val record = Model.record('root) { r =>
        save = r.record('level2) { r =>
          r.name = "123"
          r.record('level3) { r =>
            r.name = "456"
          }
        }
      }
      Model.e(save.eReference) should be(Some(save))
      val note = Model.note('note) { n => }
      val task = Model.task('task) { t => }
      Model.eFilter(_ => true) should have size (5)
      // serialize
      val s = new ProtobufSerialization
      var frozen: Seq[Array[Byte]] = Seq()
      def fFilter(element: Element.Generic): Option[Element.Generic] = {
        log.___glance("filter element %s with ancestors %s".format(element, element.eAncestorReferences.mkString(",")))
        element match {
          case model: Model.Generic =>
            val index = model.getClass.getDeclaredMethod("index")
            val hash = index.invoke(model).asInstanceOf[ModelIndex#HashMapPerId] // @transient
            log.___glance("index hash " + hash)
          case element =>
        }
        Some(element)
      }
      def fSave(element: Element.Generic, data: Array[Byte]) {
        log.___glance("save element %s, serialized data size is %d bytes".format(element, data.size))
        assert(data.size > 0, "incorrect data size")
        frozen = frozen :+ data
      }
      s.freeze(save, fSave, fFilter)
      frozen should not be ('empty)
      frozen = scala.util.Random.shuffle(frozen)
      // deserialize
      val frozenIterator = frozen.iterator
      def fLoad() = if (frozenIterator.hasNext) {
        val data = frozenIterator.next
        log.___glance("load element with size %d bytes".format(data.size))
        assert(data.size > 0, "incorrect data size")
        Some(data)
      } else None
      val dl2 = s.acquire[Record[Record.Stash], Record.Stash](fLoad, fFilter).get
      dl2.eId.name should be("level2")
      dl2.eChildren should have size (1)
      dl2.eStash.model should be(None)
      dl2.eStash.context.container should be(record.eReference)
      dl2.name should be("123")
      val dl3 = dl2.eChildren.head.asInstanceOf[Record[Record.Stash]]
      dl3.eId.name should be("level3")
      dl3.eChildren should be('empty)
      dl3.eStash.model should be(None)
      dl3.eStash.context.container should be(dl2.eReference)
      dl3.name should be("456")
      dl2.name = "789"
      dl3.name = "098"
      save.name should be("123")
      save.eChildren.head.asInstanceOf[Record[Record.Stash]].name should be("456")
      dl2.eReference should be(save.eReference)
      dl2.eReference.unique.hashCode() should be(save.eReference.unique.hashCode())
      dl2.eReference.origin.hashCode() should be(save.eReference.origin.hashCode())
      dl2.eReference.coordinate.hashCode() should be(save.eReference.coordinate.hashCode())
      dl2.eReference.hashCode() should be(save.eReference.hashCode())
      Model.e(save.eReference) should be(Some(save))
      Model.e(dl2.eReference) should be(Some(save))
      evaluating { Model.eChildren += dl2 } should produce[AssertionError]*/
    }
    it("should filter elements on save/load") {
      //Model.reset()
      /*val record = Model.record('root) { r =>
        r.record('level2) { r =>
          r.name = "123"
          r.record('level3) { r =>
            r.name = "456"
          }
        }
      }
      val note1 = Model.note('note1) { n => }
      val note2 = Model.note('note2) { n => }
      val task = Model.task('task) { t => }
      Model.eFilter(_ => true) should have size (6)
      // freeze
      val s = new ProtobufSerialization
      var frozen: Seq[Array[Byte]] = Seq()
      def fFilterSave(element: Element.Generic): Option[Element.Generic] = {
        log.___glance("filter element %s with ancestors %s".format(element, element.eAncestorReferences.mkString(",")))
        element match {
          case note: Note.Generic if note.eId == 'note1 =>
            // alter note1
            note.name = "save_filter_added"
            Some(note)
          case record: Record.Generic if record.eScope == Record.scope =>
            // drop record and  children
            log.___glance("drop " + record)
            None
          case element =>
            // pass all other
            Some(element)
        }
      }
      def fSave(element: Element.Generic, data: Array[Byte]) {
        log.___glance("save element %s, serialized data size is %d bytes".format(element, data.size))
        assert(data.size > 0, "incorrect data size")
        frozen = frozen :+ data
      }
      log.___glance("---------------------------------------------------------")
      s.freeze(Model, fSave, fFilterSave)
      frozen should not be ('empty)
      frozen should have size (4) // Model + 6 elements - 3 filtered
      frozen = scala.util.Random.shuffle(frozen)
      // deserialize
      val frozenIterator = frozen.iterator
      def fFilterLoad(element: Element.Generic): Option[Element.Generic] = {
        log.___glance("filter element %s with ancestors %s".format(element, element.eAncestorReferences.mkString(",")))
        element match {
          case note: Note.Generic if note.eId == 'note2 =>
            // alter note2
            note.name = "load_filter_added"
            Some(note)
          case element =>
            // pass all other
            Some(element)
        }
      }
      def fLoad() = if (frozenIterator.hasNext) {
        val data = frozenIterator.next
        log.___glance("load element with size %d bytes".format(data.size))
        assert(data.size > 0, "incorrect data size")
        Some(data)
      } else None
      val deserializedModel = s.acquire[Model[Model.Stash], Model.Stash](fLoad, fFilterLoad).get
      deserializedModel.eFilter(_.eScope == Record.scope) should be('empty)
      deserializedModel.eFilter(_ => true) should have size (3)
      //        log.___gaze("--- " + (deserializedModel | NoteLocation('note1)).eStash.property)
      val deserializedNote1 = deserializedModel | NoteLocation('note1)
      deserializedNote1.name should be("save_filter_added")
      val deserializedNote2 = deserializedModel | NoteLocation('note2)
      deserializedNote2.name should be("load_filter_added")*/
    }
  }

  override def beforeAll(configMap: org.scalatest.ConfigMap) { adjustLoggingBeforeAll(configMap) }
}
