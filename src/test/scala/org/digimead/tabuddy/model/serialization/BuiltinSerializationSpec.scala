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

package org.digimead.tabuddy.model.serialization

import java.util.UUID
import scala.collection.immutable
import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.LoggingHelper
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.TestDSL
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Value
import org.digimead.tabuddy.model.element.Value.string2someValue
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Graph.graph2interface
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.digimead.lib.test.StorageHelper
import java.io.File

class BuiltinSerializationSpec extends FunSpec with ShouldMatchers with StorageHelper with LoggingHelper with Loggable {
  after { adjustLoggingAfter }
  before {
    DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
    adjustLoggingBefore
  }

  describe("A SimpleSerialization") {
    it("should provide serialization mechanism for Model") {
      withTempFolder { folder ⇒
        import TestDSL._
        // graph
        val graph = Graph[Model]('john1, Model.scope, new BuiltinSerialization, UUID.randomUUID())
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eMutable
        val record_0 = model.takeRecord('baseLevel) { r ⇒
          r.takeRecord('level1a) { r ⇒
            r.takeRecord('level2a) { r ⇒
              r.name = "record_2a"
            }
            r.name = "record_1a"
          }
          r.takeRecord('level1b) { r ⇒
            r.takeRecord('level2b) { r ⇒
              r.name = "record_2b"
            }
            r.name = "record_1b"
          }
          r.name = "record_0"
        }.eMutable
        graph.node.threadSafe(_.iteratorRecursive().size) should be(5)
        model.eNode.threadSafe(_.size) should be(1)
        model.copy(model.eStash.copy(property = model.eStash.property + ('a -> immutable.HashMap(DSLType.classSymbolMap(classOf[String]) -> new Value.Static("123", Value.Context(model))))))
        log.___glance("Model dump:\n" + model.eDump(false))

        // serialize
        new File(folder, "john1") should not be ('exists)
        graph.storages = graph.storages :+ folder.getAbsoluteFile().toURI()
        Serialization.freezeGraph(graph)
        new File(folder, "john1") should be('directory)
        new File(folder, "john1/description") should be('file)
        new File(folder, "john1/baseLevel/description") should be('file)
        new File(folder, "john1/baseLevel/level1a/description") should be('file)
        new File(folder, "john1/baseLevel/level1b/description") should be('file)
        new File(folder, "john1/baseLevel/level1a/level2a/description") should be('file)
        new File(folder, "john1/baseLevel/level1b/level2b/description") should be('file)
      }
      /*
      var frozen: Seq[Array[Byte]] = Seq()
      def fFilter[A <: Element](element: A): Option[A] = {
        log.___glance(s"Filter element ${element} with ancestors ${element.eAncestors.mkString(",")}.")
        element match {
          case model: Model.Like ⇒
            log.___glance("Model element detected: " + model)
          case element ⇒
        }
        Some(element)
      }
      def fSave(element: Element, data: Array[Byte]) {
        log.___glance("Save element %s, serialized data size is %d bytes".format(element, data.size))
        assert(data.size > 0, "incorrect data size")
        frozen = frozen :+ data
      }
      graph.node.freezeRead(_.iteratorRecursive().foreach { node ⇒
        node.getProjections.foreach {
          case (coordinate, box) ⇒ box.save()
        }
      })*/
      /*      s.freeze(Model, fSave, fFilter)
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
      deserializedModel.eStash.model should be(Some(deserializedModel))
      deserializedModel.eStash.property should be(Model.eStash.property)
      deserializedModel.eStash.property(DSLType.classSymbolMap(classOf[String]))('a).get should be("123")
      deserializedModel.e(deserializedModel.eReference) should not be ('empty)
      // record
      deserializedModel.eChildren.head.eq(Model.eChildren.head) should be(false)
      deserializedModel.eChildren.head.eStash.model should be(Some(deserializedModel))
      deserializedModel.e(deserializedModel.eChildren.head.eReference) should not be ('empty)*/
    }
    it("should provide serialization mechanism for Element") {
      //      Model.reset()
      /*var save: Record[Record.Stash] = null
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
      val s = new BuiltinSerialization
      var frozen: Seq[Array[Byte]] = Seq()
      def fFilter[T <: Element.Generic](element: T): Option[T] = {
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
      //      Model.reset()
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
      val s = new BuiltinSerialization
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
      val deserializedNote1 = deserializedModel | NoteLocation('note1)
      deserializedNote1.name should be("save_filter_added")
      val deserializedNote2 = deserializedModel | NoteLocation('note2)
      deserializedNote2.name should be("load_filter_added")*/
    }
    it("should have a simple API") {
      // 1st variant
      var frozen: Seq[Array[Byte]] = Seq()
      /*val record = Model.record('test1) { record => }
      // serialize
      val a = new BuiltinSerialization
      a.freeze(record, (element, data) => { frozen = frozen :+ data })
      // deserialize
      val b = new BuiltinSerialization
      val frozenIterator = frozen.iterator
      intercept[IllegalArgumentException] { // try to deserialize element without type information
        b.acquire(() => { if (frozenIterator.hasNext) Some(frozenIterator.next) else None })
      }
      val test = b.acquire[Record[Record.Stash], Record.Stash](() => { if (frozenIterator.hasNext) Some(frozenIterator.next) else None })
      // compare
      assert(test.get === record)

      // 2nd variant
      val serialized = BuiltinSerialization.to(record)
      val deserialized = BuiltinSerialization.from(serialized)
      assert(deserialized.get === record)*/
    }
  }

  override def beforeAll(configMap: Map[String, Any]) { adjustLoggingBeforeAll(configMap) }
}
