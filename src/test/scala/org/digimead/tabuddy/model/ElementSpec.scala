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
import org.digimead.tabuddy.model.dsl.DSL
import org.digimead.tabuddy.model.element.Axis.intToAxis
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Stash
import org.digimead.tabuddy.model.graph.Context
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.ElementBox.box2interface
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Graph.graph2interface
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.predef.Note
import org.digimead.tabuddy.model.predef.Task
import org.digimead.tabuddy.model.serialization.Stub
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import com.escalatesoft.subcut.inject.NewBindingModule

import language.implicitConversions

class ElementSpec extends FunSpec with ShouldMatchers with LoggingHelper with Loggable {
  lazy val diConfig = new NewBindingModule(module => {
    module.bind[Symbol => Node] toSingle { (symbol: Symbol) => null }
  }) ~ org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default
  after { adjustLoggingAfter }
  before {
    DependencyInjection(diConfig, false)
    adjustLoggingBefore
  }

  def multithread[A](nIterations: Int, nThreads: Int, visible: Boolean = true)(f: Int => A) {
    if (org.apache.log4j.Logger.getRootLogger().getAllAppenders().nextElement().getClass().getSimpleName() != "NullAppender") {
      log.___glance("Debug logging enabled - multithreading disabled.")
      f(0)
      return
    }
    val ts = System.currentTimeMillis()
    val threads = for (t <- 0 until nThreads * 2 if t % 2 == 0) yield {
      val x = t * nIterations
      new Thread(new Runnable {
        def run = for (i <- x until x + nIterations) { f(i) }
      })
    }
    threads.foreach(_.start)
    threads.foreach(_.join)
    if (visible)
      System.err.println("TOTAL: " + (System.currentTimeMillis() - ts))
  }

  describe("An Element") {
    ignore("should have proper copy") {
      import ElementSpec.ElementSpecDSL._
      implicit val modeStashClass = classOf[Model.Stash]
      val graph = Graph[Model]('john, Model.scope, new Stub, UUID.randomUUID())
      graph should not be null
      val myModel = graph.model
      myModel should not be null
      // 100000 iterations x 10 threads x 3 elements = 3000000 elements
      // 3000000 is about 4.5GB mem
      // 300000/10Th processed within 19012ms = ~15315 eCopy(write) operations per second on home PC
      // ~4600/1Th eCopy(write) ops in single thread
      // ~12500/10Th eCopy(write) ops in multi thread
      multithread(1000, 10) { i =>
        val record1 = myModel.record('root) { r => r }
        record1 should not be null
        val record2 = record1.eCopy(record1.eNode, ('x, i))
        record2 should not be null
        val note1 = myModel.note('note) { n => }
        note1 should not be null
        val note2 = note1.eCopy(note1.eNode, ('x, i))
        note2 should not be null
        val task1 = myModel.task('task) { t => }
        task1 should not be null
        val task2 = task1.eCopy(task1.eNode, ('x, i))
        task2 should not be null
      }
    }
    ignore("should have proper constraints") {
      import ElementSpec.ElementSpecDSL._
      implicit val modeStashClass = classOf[Model.Stash]
      val graph = Graph[Model]('john, Model.scope, new Stub, UUID.randomUUID())
      val myModel = graph.model
      multithread(1000, 10) { i =>
        val r1 = graph.model.record('a) { r => r }
        val rctx = r1.eContext
        val rcoord = r1.eStash.coordinate
        val rid = r1.eStash.id
        val runique = r1.eStash.unique
        // create element projection at different coordinate
        log.___glance(s"Copy ${r1} to ('x, 1).")
        val r1projection = r1.eCopy(r1.eNode, ('x, 1 + i))
        val r2 = graph.model.record('a, ('x, 2 + i)) { r => r }
        //graph.node.getChildren.size should be(1)
        //graph.node.getChildren.head.getProjections.size should be(2)
        // create element with different type
        evaluating { graph.model.note('a, ('x, 3 + i)) { n => } } should produce[IllegalArgumentException]
        //graph.node.getChildren.head.getProjections.size should be(2)
        //log.___glance(s"${r1.eBox.get} projections are ${r1.eNode.getProjections}")
        val r3 = graph.model.record('b, ('xcbv, 4 + i)) { r => r }
        val r4 = r3.record('c, ('ertw, 4 + i)) { r => r }
      }
      val r3 = graph.model.record('b, ('qwe, 4)) { r => r }
      val r4 = r3.record('c, ('sdf, 4)) { r => r }
      //log.___glance("? \n" + Graph.dump(graph, false))
      // create element with inconsistent context origin
      evaluating {
        r4.eNode.threadSafe { node =>
          implicit val shashClass = classOf[Record.Stash]
          val timestamp = Element.timestamp()
          ElementBox[Record](Context('q, r3.eNode.unique), Coordinate.root, timestamp, node, timestamp, Record.scope, node.rootElementBox.serialization)
        }
      } should produce[IllegalArgumentException]
      // create element with inconsistent context unique
      evaluating {
        r4.eNode.threadSafe { node =>
          implicit val shashClass = classOf[Record.Stash]
          val timestamp = Element.timestamp()
          ElementBox[Record](Context(node.graph.get.origin, UUID.randomUUID()), Coordinate.root, timestamp, node, timestamp, Record.scope, node.rootElementBox.serialization)
        }
      } should produce[IllegalArgumentException]
      // successful create new element
      r4.eNode.threadSafe { node =>
        val timestamp = Element.timestamp()
        val stash = new Record.Stash(Coordinate(('cxv, 6)), timestamp, node.id, timestamp, node.graph.get.origin, new Stash.Data, Record.scope, node.unique)
        ElementBox[Record](Context(node.graph.get.origin, r3.eNode.unique), node, node.rootElementBox.serialization, stash)
      }
      // create element with inconsistent context origin
      evaluating {
        r4.eNode.threadSafe { node =>
          val timestamp = Element.timestamp()
          val stash = new Record.Stash(Coordinate.root, timestamp, node.id, timestamp, node.graph.get.origin, new Stash.Data, Record.scope, node.unique)
          ElementBox[Record](Context('q, r3.eNode.unique), node, node.rootElementBox.serialization, stash)
        }
      } should produce[IllegalArgumentException]
      // create element with inconsistent context unique
      evaluating {
        r4.eNode.threadSafe { node =>
          val timestamp = Element.timestamp()
          val stash = new Record.Stash(Coordinate.root, timestamp, node.id, timestamp, node.graph.get.origin, new Stash.Data, Record.scope, node.unique)
          ElementBox[Record](Context(node.graph.get.origin, UUID.randomUUID()), node, node.rootElementBox.serialization, stash)
        }
      } should produce[IllegalArgumentException]
      // create element with inconsistent stash id
      evaluating {
        r4.eNode.threadSafe { node =>
          val timestamp = Element.timestamp()
          val stash = new Record.Stash(Coordinate.root, timestamp, 'hjgfjfghf, timestamp, node.graph.get.origin, new Stash.Data, Record.scope, node.unique)
          ElementBox[Record](Context(node.graph.get.origin, r3.eNode.unique), node, node.rootElementBox.serialization, stash)
        }
      } should produce[IllegalArgumentException]
      // create element with inconsistent stash unique
      evaluating {
        r4.eNode.threadSafe { node =>
          val timestamp = Element.timestamp()
          val stash = new Record.Stash(Coordinate.root, timestamp, node.id, timestamp, node.graph.get.origin, new Stash.Data, Record.scope, UUID.randomUUID())
          ElementBox[Record](Context(node.graph.get.origin, r3.eNode.unique), node, node.rootElementBox.serialization, stash)
        }
      } should produce[IllegalArgumentException]
      // create element with inconsistent stash origin
      evaluating {
        r4.eNode.threadSafe { node =>
          val timestamp = Element.timestamp()
          val stash = new Record.Stash(Coordinate.root, timestamp, node.id, timestamp, 'asdf, new Stash.Data, Record.scope, UUID.randomUUID())
          ElementBox[Record](Context(node.graph.get.origin, r3.eNode.unique), node, node.rootElementBox.serialization, stash)
        }
      } should produce[IllegalArgumentException]
    }
    ignore("should register elements in model") {
      import ElementSpec.ElementSpecDSL._
      implicit val modeStashClass = classOf[Model.Stash]
      val graph = Graph[Model]('john, Model.scope, new Stub, UUID.randomUUID())
      val myModel = graph.model
      val r1 = myModel.record('a) { r => r }
      r1.eBox.context.origin should not be (null)
      r1.eBox.context.unique should not be (null)
      myModel.e(r1.eBox.context) should be(Some(myModel.eBox.node))
      myModel.e(r1.eReference) should be(Some(r1))
      myModel.e(myModel.eReference) should be(Some(myModel))
      r1.eBox.context.origin.name should be(r1.eReference.origin.name)
      r1.eBox.context.unique should be(myModel.eReference.unique)
    }
    it("should have proper copy constructor") {
      import ElementSpec.ElementSpecDSL._
      implicit val modeStashClass = classOf[Model.Stash]
      val graph = Graph[Model]('john, Model.scope, new Stub, UUID.randomUUID())
      val myModel = graph.model
      var save: Record = null
      val record: Record = myModel.record('root) { r1 =>
        r1.name = "root"
        r1.element.eGet[String]('name).get.get should be("root")
        r1.name should be("root")
        val a: Record = r1
        a.name should be("root")
        save = r1.record('level2) { r2 =>
          r2.name = "level2"
          r2.element.eGet[String]('name).get.get should be("level2")
          r2.record('level3) { r3 =>
            r3.name = "level3"
            r3.name should be("level3")
          }
          r2.name = "level2a"
          r2
        }
        r1
      }
      record.name should be("root")
      val record2 = record.eBox.node.getChildren.head.getProjection(Coordinate.root).get.get.asInstanceOf[Record]
      record2.name should be("level2a")
      record2 should be(save)
      val record3 = record2.eBox.node.getChildren.head.getProjection(Coordinate.root).get.get.asInstanceOf[Record]
      record3.name should be("level3")
      save = save.eSet[Integer]('test, 123)
      val saveValue = save.eGet[Integer]('test).get
      saveValue.context.unique should be(save.eReference.unique)
      val copy = save.eCopy(save.eNode, ('a, 1))
      copy.eId.name should be("level2a")
      copy.name should be("level2a")
      /*copy.eChildren.head.asInstanceOf[Record[Record.Stash]].name should be("level3")
      val copyValue = copy.eGet[Integer]('test).get
      copyValue.context.container.unique should be(copy.eReference.unique)
      record.eReference.unique should not be (copy.eReference.unique)
      record.eSet('test, Some(copyValue))
      val recordValue = record.eGet[Integer]('test).get
      recordValue.get should be(copyValue.get)
      recordValue.context.container.unique should be(record.eReference.unique)
      record.eChildren -= save
      Model.e(save.eReference) should be(None)
      save.eStash.model should be(None)
      record.eChildren += save
      val newRecord = save.eCopy(save.eStash.copy(id = 'new))
      save.eUnique should be(newRecord.eUnique)
      save.eReference should be(newRecord.eReference)
      save.eStash.model should be(Some(Model.inner))
      Model.e(newRecord.eReference) should not be ('empty)
      record.eChildren -= save
      Model.e(save.eReference) should be(None)
      record.eChildren += newRecord*/
    }
    /*it("should determinate ancestors") {
      Model.reset()
      var recordL2: Record[Record.Stash] = null
      var recordL3: Record[Record.Stash] = null
      val recordL1 = Model.record('level1) { r =>
        recordL2 = r.record('level2) { r =>
          recordL3 = r.record('level3) { r =>
          }
        }
      }
      Model.eAncestors should be('empty)
      Model.eAncestorReferences should be('empty)
      recordL1.eAncestors should be(Seq(Model.inner))
      recordL1.eAncestorReferences should be(Seq(Model.inner.eReference))
      recordL2.eAncestors should be(Seq(recordL1, Model.inner))
      recordL2.eAncestorReferences should be(Seq(recordL1.eReference, Model.inner.eReference))
      recordL3.eAncestors should be(Seq(recordL2, recordL1, Model.inner))
      recordL3.eAncestorReferences should be(Seq(recordL2.eReference, recordL1.eReference, Model.inner.eReference))
    }
    it("should provide the convertation ability") {
      Model.reset()
      val note = Model.note('root) { n => }
      note should not be (null)
      note.getClass should be(classOf[Note[_]])
      val element = note.asInstanceOf[Element.Generic]
      // Element.Generic -> Record -> Note -> Task
      // unable - lack of required fields
      element.eAs[Task[Task.Stash], Task.Stash].nonEmpty should be(false)
      // ok
      element.eAs[Note[Note.Stash], Note.Stash].nonEmpty should be(true)
      // unable - lack of required fields in stash
      element.eAs[Note[Task.Stash], Task.Stash].nonEmpty should be(false)
      // ok
      element.eAs[Record[Record.Stash], Record.Stash].nonEmpty should be(true)
    }
    it("should provide recursive iterator") {
      Model.reset()
      var e2: Record[Record.Stash] = null
      var e3: Record[Record.Stash] = null
      val e1 = Model.record('root) { r =>
        r.name = "root"
        e2 = r.record('level2) { r =>
          r.name = "level2"
          e3 = r.record('level3) { r =>
            r.name = "level3"
          }
        }
      }
      val modelIterator = Model.eChildren.iteratorRecursive()
      assert(modelIterator.length === 3) // Model + 3 children
      val modelChildren = Model.eChildren.iteratorRecursive().foldLeft(Seq[Element.Generic]())((acc, e) => acc :+ e).sortBy(_.eUnique)
      Seq(e1, e2, e3).sortBy(_.eUnique).sameElements(modelChildren)
      assert(e2.eChildren.iteratorRecursive().length === 1)
      assert(e3.eChildren.iteratorRecursive().length === 0)
    }
    it("should throw an exception if type is unknown") {
      Model.reset()
      val record = Model.record('test) { record => }
      val unknownData = ElementSpec.UnknownType(0)
      val unknownValue = new Value.Static(unknownData, Context.empty)
      intercept[IllegalArgumentException] {
        record.eSet('file, Some(unknownValue))
      }
    }*/
  }

  override def beforeAll(configMap: Map[String, Any]) { adjustLoggingBeforeAll(configMap) }
}

object ElementSpec {
  case class UnknownType(val x: Int)

  object ElementSpecDSL {
    class TestDSL(val element: Element)
      extends DSL.RichElement
      with Record.DSL.RichElement
      with Note.DSL.RichElement
      with Task.DSL.RichElement
    implicit def e2DSL[A <: Element](e: A): TestDSL = new TestDSL(e)
    implicit def me2DSL[A <: Element](me: Element.Mutable[A]): TestDSL = new TestDSL(me.element)
  }
}
