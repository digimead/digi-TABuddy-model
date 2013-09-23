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

import java.util.UUID

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.LoggingHelper
import org.digimead.tabuddy.model.element.Axis.intToAxis
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Stash
import org.digimead.tabuddy.model.element.Value
import org.digimead.tabuddy.model.element.Value.int2someValue
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.ElementBox.box2interface
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Graph.graph2interface
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.predef.Note
import org.digimead.tabuddy.model.predef.Task
import org.digimead.tabuddy.model.serialization.StubSerialization
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import com.escalatesoft.subcut.inject.NewBindingModule

import language.implicitConversions

class ElementSpec extends FunSpec with ShouldMatchers with LoggingHelper with Loggable {
  lazy val diConfig = org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default
  after { adjustLoggingAfter }
  before {
    DependencyInjection(diConfig, false)
    adjustLoggingBefore
  }

  def multithread[A](nIterations: Int, nThreads: Int, visible: Boolean = true)(f: Int ⇒ A) {
    if (org.apache.log4j.Logger.getRootLogger().getAllAppenders().nextElement().getClass().getSimpleName() != "NullAppender") {
      log.___glance("Debug logging enabled - multithreading disabled.")
      f(0)
      return
    }
    val ts = System.currentTimeMillis()
    val threads = for (t ← 0 until nThreads * 2 if t % 2 == 0) yield {
      val x = t * nIterations
      new Thread(new Runnable {
        def run = for (i ← x until x + nIterations) { f(i) }
      })
    }
    threads.foreach(_.start)
    threads.foreach(_.join)
    if (visible)
      System.err.println("TOTAL: " + (System.currentTimeMillis() - ts))
  }

  describe("An Element") {
    it("should have proper equality") {
      import TestDSL._
      // graph 1
      val graph1 = Graph[Model]('john1, Model.scope, StubSerialization.Identifier, UUID.randomUUID())
      val model1 = graph1.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      val rA1 = model1.takeRecord('rA) { r ⇒
        r.takeRecord('rAB) { r ⇒
          r.takeRecord('rLeaf) { r ⇒
            r.name = "123"
          }
        }
      }.eRelative
      val rAB1 = (rA1 & RecordLocation('rAB)).eRelative
      val rLeaf1 = (rAB1 & RecordLocation('rLeaf)).eRelative
      // graph 2
      val graph2 = graph1.copy(origin = 'john2)
      val model2 = graph2.model.eRelative
      val rA2 = (model2 & RecordLocation('rA)).eRelative
      val rAB2 = (rA2 & RecordLocation('rAB)).eRelative
      val rLeaf2 = (rAB2 & RecordLocation('rLeaf)).eRelative

      model1.absolute.canEqual(model2.absolute) should be(true)
      model1.eStash.canEqual(model2.eStash) should be(true)

      rA1.eStash should be(rA1.eStash)
      val curtom_rA1 = new Record(rA1.eStash)(rAB2.eBox) // different eBox
      curtom_rA1.canEqual(rA1.absolute) should be(true)
      curtom_rA1.eStash should be(rA1.eStash)
      curtom_rA1 should not be (rA1.absolute)

      val rLeaf2_orig = rLeaf2.absolute
      rLeaf2.name should be("123")
      rLeaf2.name = "321"
      rLeaf2_orig should not be (rLeaf2.absolute)

      model1 should be(model1.absolute)
      model1.absolute should be(model1)
    }
    it("should have proper copy") {
      import TestDSL._
      val graph = Graph[Model]('john, Model.scope, StubSerialization.Identifier, UUID.randomUUID())
      graph should not be null
      val myModel = graph.model
      myModel should not be null
      // 100000 iterations x 10 threads x 3 elements = 3000000 elements
      // 3000000 is about 4.5GB mem
      // 300000/10Th processed within 3000ms = ~100000 operations per second on notebook/home pc
      // ~?/1Th eCopy(write) ops in single thread
      // ~?/10Th eCopy(write) ops in multi thread
      multithread(1000, 10) { i ⇒
        val record1 = myModel.withRecord('root) { r ⇒ r }
        record1 should not be null
        val record2 = record1.eCopy(record1.eNode, ('x, i))
        record2 should not be null
        val note1 = myModel.note('note)
        note1 should not be null
        val note2 = note1.eCopy(note1.eNode, ('x, i))
        note2 should not be null
        val task1 = myModel.task('task)
        task1 should not be null
        val task2 = task1.eCopy(task1.eNode, ('x, i))
        task2 should not be null
      }
      val rootX = myModel.record('root).eRelative
      val oldCreated = rootX.eStash.created
      val newCreated = Element.timestamp()
      oldCreated should not be (newCreated)
      rootX.eStash.created should be(oldCreated)
      rootX.copy(rootX.eStash.copy(created = newCreated))
      rootX.eStash.created should be(newCreated)
      graph.copy().node.safeRead { node ⇒
        graph.node.safeRead { node2 ⇒
          node.graph.storages should be(node2.graph.storages)
          node.graph.stored should be(node2.graph.stored)
          node.iteratorRecursive().corresponds(node2.iteratorRecursive()) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)
    }
    it("should have proper constraints") {
      import TestDSL._
      val graph = Graph[Model]('john, Model.scope, StubSerialization.Identifier, UUID.randomUUID())
      val myModel = graph.model
      multithread(1000, 10) { i ⇒
        val r1 = graph.model.withRecord('a) { r ⇒ r }
        val rcoord = r1.eCoordinate
        val rid = r1.eId
        val runique = r1.eNode.unique
        // create element projection at different coordinate
        log.___glance(s"Copy ${r1} to ('x, 1).")
        val r1projection = r1.eCopy(r1.eNode, ('x, 1 + i))
        val r2 = graph.model.withRecord('a, ('x, 2 + i)) { r ⇒ r }
        //graph.node.getChildren.size should be(1)
        //graph.node.getChildren.head.getProjections.size should be(2)
        // create element with different type
        evaluating { graph.model.note('a, ('x, 3 + i)) } should produce[IllegalArgumentException]
        //graph.node.getChildren.head.getProjections.size should be(2)
        //log.___glance(s"${r1.eBox.get} projections are ${r1.eNode.getProjections}")
        val r3 = graph.model.withRecord('b, ('xcbv, 4 + i)) { r ⇒ r }
        val r4 = r3.withRecord('c, ('ertw, 4 + i)) { r ⇒ r }
      }
      val r3 = graph.model.withRecord('b, ('qwe, 4)) { r ⇒ r }
      val r4 = r3.withRecord('c, ('sdf, 4)) { r ⇒ r }
      //log.___glance("? \n" + Graph.dump(graph, false))
      // successful create new element
      r4.eNode.safeWrite { node ⇒
        val timestamp = Element.timestamp()
        val stash = new Record.Stash(timestamp, timestamp, new Stash.Data, Record.scope)
        ElementBox[Record](Coordinate(('cxv, 6)), node, node.rootBox.serialization, stash)
      } should not be (null)
    }
    it("should register elements in model") {
      import TestDSL._
      val graph = Graph[Model]('john, Model.scope, StubSerialization.Identifier, UUID.randomUUID())
      val myModel = graph.model
      val r1 = myModel.withRecord('a) { r ⇒ r }
      myModel.e(r1.eReference) should be(Some(r1: Element))
      myModel.e(myModel.eReference) should be(Some(myModel))
    }
    it("should have proper copy constructor") {
      import TestDSL._
      val graph = Graph[Model]('john, Model.scope, StubSerialization.Identifier, UUID.randomUUID())
      val myModel = graph.model
      var save: Record = null
      val record: Record = myModel.takeRecord('root) { r1 ⇒
        r1.name = "root"
        r1.eGet[String]('name).get.get should be("root")
        r1.name should be("root")
        val a: Record = r1
        a.name should be("root")
        save = r1.takeRecord('level2) { r2 ⇒
          r2.name = "level2"
          r2.eGet[String]('name).get.get should be("level2")
          r2.takeRecord('level3) { r3 ⇒
            r3.name = "level3"
            r3.name should be("level3")
          }
          r2.name = "level2a"
        }
      }
      record.name should be("root")
      val record2 = record.eNode.safeRead(_.head.rootBox.e.asInstanceOf[Record])
      record2.name should be("level2a")
      record2 should be(save)
      val record3 = record2.eNode.safeRead(_.head.rootBox.e.asInstanceOf[Record])
      record3.name should be("level3")
      save = save.eSet[Integer]('test, 123)
      val saveValue = save.eGet[Integer]('test).get
      val copy = save.eCopy(save.eNode, ('a, 1))
      copy.eId.name should be("level2")
      copy.name should be("level2a")
      copy.eNode.safeRead(_.head.rootBox.e.asInstanceOf[Record].name) should be("level3")
      val copyValue = copy.eGet[Integer]('test).get
      record.eReference.model should be(copy.eReference.model)
      record.eReference.node should not be (copy.eReference.node)
      val newRecord = record.eSet('test, Some(copyValue))
      val recordValue = newRecord.eGet[Integer]('test).get
      recordValue.get should be(copyValue.get)
      myModel.e(save.eReference) should be('nonEmpty)
      record.eNode.safeWrite(_ -= save.eNode)
      myModel.e(save.eReference) should be(None)
      record.eNode.safeWrite(_ += save.eNode)
      myModel.e(save.eReference) should be('nonEmpty)
      save.eModel should be(myModel)
      var newChild: Node[_ <: Element] = null
      val newRecord2 = save.eNode.parent.map(_.safeWrite { parent ⇒
        parent.createChild[Record]('new, UUID.randomUUID()).safeWrite { child ⇒
          newChild = child
          save.eCopy(child, save.eCoordinate, save.eStash, save.eBox.serialization)
        }
      })
      newRecord2 should be('nonEmpty)
      //System.err.println("!!!" + Graph.dump(graph, false))
      save.eReference.coordinate should be(newRecord2.get.eReference.coordinate)
      save.eModel should be(newRecord2.get.eModel)
      myModel.e(newRecord2.get.eReference) should be(newRecord2)
      myModel.e(newRecord2.get.eReference) should not be ('empty)
      record.eNode.safeWrite(_ -= newChild)
      myModel.e(newRecord2.get.eReference) should be(None)
    }
    it("should determinate ancestors") {
      import TestDSL._
      val graph = Graph[Model]('john, Model.scope, StubSerialization.Identifier, UUID.randomUUID())
      val myModel = graph.model
      var recordL2: Record = null
      var recordL3: Record = null
      val recordL1 = myModel.takeRecord('level1) { r1 ⇒
        recordL2 = r1.takeRecord('level2) { r2 ⇒
          recordL3 = r2.record('level3)
        }
      }
      myModel.eAncestors should be('empty)
      recordL1.eAncestors should be(Seq(myModel.eNode))
      recordL2.eAncestors should be(Seq(recordL1.eNode, myModel.eNode))
      recordL3.eAncestors should be(Seq(recordL2.eNode, recordL1.eNode, myModel.eNode))
    }
    it("should provide the convertation ability") {
      import TestDSL._
      val graph = Graph[Model]('john, Model.scope, StubSerialization.Identifier, UUID.randomUUID())
      val myModel = graph.model
      val note = myModel.note('root)
      note should not be (null)
      note.getClass should be(classOf[Note])
      val element = note.asInstanceOf[Element]
      // Element.Generic -> Record -> Note -> Task
      // unable
      element.eAs[Model.Like].nonEmpty should be(false)
      // unable
      element.eAs[Task.Like].nonEmpty should be(false)
      // ok
      element.eAs[Note].nonEmpty should be(true)
      // ok
      element.eAs[Note.Like].nonEmpty should be(true)
      // ok
      element.eAs[Record.Like].nonEmpty should be(true)
    }
    it("should provide recursive iterator") {
      import TestDSL._
      val graph = Graph[Model]('john, Model.scope, StubSerialization.Identifier, UUID.randomUUID())
      val myModel = graph.model
      var e2: Record = null
      var e3: Record = null
      val e1 = myModel.takeRecord('root) { r ⇒
        r.name = "root"
        e2 = r.takeRecord('level2) { r ⇒
          r.name = "level2"
          e3 = r.takeRecord('level3) { r ⇒
            r.name = "level3"
          }
        }
      }
      myModel.eNode.safeRead { modelNode ⇒
        val modelIterator = modelNode.iteratorRecursive()
        assert(modelIterator.length === 3) // Model + 3 children
        val modelChildren = modelNode.iteratorRecursive().foldLeft(Seq[Node[_ <: Element]]())((acc, e) ⇒ acc :+ e).sortBy(_.unique)
        Seq(e1.eNode, e2.eNode, e3.eNode).sortBy(_.unique).sameElements(modelChildren)
        assert(e2.eNode.safeRead { _.iteratorRecursive().length === 1 })
        assert(e3.eNode.safeRead { _.iteratorRecursive().length === 0 })
      }
    }
    it("should throw an exception if type is unknown") {
      import TestDSL._
      val graph = Graph[Model]('john, Model.scope, StubSerialization.Identifier, UUID.randomUUID())
      val myModel = graph.model
      val record = myModel.record('test)
      val unknownData = ElementSpec.UnknownType(0)
      //val unknownValue = new Value.Static(unknownData, Context())
      //intercept[IllegalArgumentException] {
      //  record.eSet('file, Some(unknownValue))
      // }
    }
  }

  override def beforeAll(configMap: Map[String, Any]) { adjustLoggingBeforeAll(configMap) }
}

object ElementSpec {
  case class UnknownType(val x: Int)
}

