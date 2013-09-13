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

import java.io.File
import java.util.UUID

import scala.collection.immutable

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.LoggingHelper
import org.digimead.lib.test.StorageHelper
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.TestDSL
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.element.Value
import org.digimead.tabuddy.model.element.Value.string2someValue
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Graph.graph2interface
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

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
        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID())
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
        graph.node.safeRead(_.iteratorRecursive().size) should be(5)
        model.eNode.safeRead(_.size) should be(1)
        model.copy(model.eStash.copy(property = model.eStash.property + ('a -> immutable.HashMap(DSLType.classSymbolMap(classOf[String]) -> new Value.Static("123", Value.Context(model))))))
        log.___glance("Model dump:\n" + model.eDump(false))

        // serialize
        new File(folder, "john1") should not be ('exists)
        graph.storages = graph.storages :+ folder.getAbsoluteFile().toURI()
        Serialization.freeze(graph)
        new File(folder, "john1") should be('directory)
        new File(folder, "john1").length() should be > (0L)
        new File(folder, "john1/descriptor.yaml") should be('file)
        new File(folder, "john1/descriptor.yaml").length() should be > (0L)
        new File(folder, "john1/model/john1/descriptor.yaml") should be('file)
        new File(folder, "john1/model/john1/descriptor.yaml").length() should be > (0L)
        new File(folder, "john1/model/john1/baseLevel/descriptor.yaml") should be('file)
        new File(folder, "john1/model/john1/baseLevel/descriptor.yaml").length() should be > (0L)
        new File(folder, "john1/model/john1/baseLevel/level1a/descriptor.yaml") should be('file)
        new File(folder, "john1/model/john1/baseLevel/level1a/descriptor.yaml").length() should be > (0L)
        new File(folder, "john1/model/john1/baseLevel/level1b/descriptor.yaml") should be('file)
        new File(folder, "john1/model/john1/baseLevel/level1b/descriptor.yaml").length() should be > (0L)
        new File(folder, "john1/model/john1/baseLevel/level1a/level2a/descriptor.yaml") should be('file)
        new File(folder, "john1/model/john1/baseLevel/level1a/level2a/descriptor.yaml").length() should be > (0L)
        new File(folder, "john1/model/john1/baseLevel/level1b/level2b/descriptor.yaml") should be('file)
        new File(folder, "john1/model/john1/baseLevel/level1b/level2b/descriptor.yaml").length() should be > (0L)
        //val testTxtSource = scala.io.Source.fromFile(new File(folder, "john1/model/john1/description"))
        //val str = testTxtSource.getLines.mkString("\n")
        //testTxtSource.close()

        // deserialize
        val graph2 = Serialization.acquire(graph.origin, folder.toURI)
        /* compare graph */
        graph2 should not be (null)
        graph2 should be(graph)
        graph2.origin.name should be(graph.origin.name)
        graph2.created should be(graph.created)
        graph2.modelType should be(graph.modelType)
        graph2.modification should be(graph.modification)
        graph.node.safeRead { node ⇒
          graph2.node.safeRead { node2 ⇒
            /* compare node */
            node2 should be(node)
            node2.id.name should be(node.id.name)
            node2.unique should be(node.unique)
            node2.children should be(node.children)
            node2.modification should be(node.modification)
            node2.projectionElementBoxes should be(node.projectionElementBoxes)
            node2.rootElementBox should be(node.rootElementBox)

            node2.graph should be(graph2)
            node2.parentNodeReference.get should be(Some(node2))

            /* compare root element box */
            node2.rootElementBox.coordinate should be(node.rootElementBox.coordinate)
            node2.rootElementBox.elementUniqueId should be(node.rootElementBox.elementUniqueId)
            node2.rootElementBox.unmodified should be(node.rootElementBox.unmodified)
            node2.rootElementBox.elementType should be(node.rootElementBox.elementType)
            node2.rootElementBox.node should be(node.rootElementBox.node)
            node2.rootElementBox.serialization should be(node.rootElementBox.serialization)

            /* compare root element */
            val element = node.rootElementBox.get
            val element2 = node2.rootElementBox.get

            element should not be (null)
            element2 should not be (null)
            node.rootElementBox.getModified should not be (None)
            node2.rootElementBox.getModified should be(None)
            element.ne(element2) should be(true)
            element2 should be(element)

            node2.iteratorRecursive().toVector should be(node.iteratorRecursive().toVector)

            // check
            // model
            graph2.node.safeRead(_.iteratorRecursive().toSeq) should have size (5)
            // container always point to current active model
            graph2.model.eId.name should be(graph.model.eId.name)
            graph2.model.eObjectId should be(graph.model.eObjectId)
            graph2.model.eNodeId should be(graph.model.eNodeId)
            graph2.model.modification should be(graph.model.modification)
            graph2.model.eModel should be(graph2.model)
            graph2.model.eModel should be(graph.model)
            graph2.model.eStash.property should be(graph.model.eStash.property)
            graph2.model.eStash.property('AAAKey)(DSLType.classSymbolMap(classOf[String])).get should be("AAA")
            graph2.model.e(graph2.model.eReference) should not be ('empty)
            // record
            graph2.node.safeRead(_.head).eq(graph.node.safeRead(_.head)) should be(false)
            graph2.node.safeRead(_.head).safeRead(_.rootElementBox.get()).eModel should be(graph2.model)
            graph2.model.e(graph2.node.safeRead(_.head).safeRead(_.rootElementBox.get()).eReference) should not be ('empty)
          }
        }
      }

    }
    it("should provide serialization mechanism for elements") {
      withTempFolder { folder ⇒
        import TestDSL._

        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID())
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eMutable
        val record_root = model.takeRecord('root) { r ⇒
          r.takeRecord('level2) { r ⇒
            r.name = "123"
            r.takeRecord('level3) { r ⇒
              r.name = "456"
            }
          }
        }
        val record_level2 = record_root & RecordLocation('level2)
        val record_level3 = record_level2 & RecordLocation('level3)

        model.e(record_level3.eReference).get.eq(record_level3) should be(true)
        val note = model.note('note)
        val task = model.task('task)
        model.eNode.safeRead(_.iteratorRecursive().toSeq) should have size (5)

        graph.storages = graph.storages :+ folder.getAbsoluteFile().toURI()
        Serialization.freeze(graph)
        val graph2 = Serialization.acquire(graph.origin, folder.toURI)

        graph.node.safeRead { node ⇒
          graph2.node.safeRead { node2 ⇒
            node.iteratorRecursive().corresponds(node2.iteratorRecursive()) { (a, b) ⇒ a.ne(b) && a.modification == b.modification }
          }
        }

        /*
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
