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
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Value.string2someValue
import org.digimead.tabuddy.model.element.compare.CompareByTimespamp
import org.digimead.tabuddy.model.element.compare.CompareByTimestampAndThenContent
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Graph.graph2interface
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.serialization.Stub
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import com.escalatesoft.subcut.inject.NewBindingModule

class CompareSpec extends FunSpec with ShouldMatchers with LoggingHelper with Loggable {
  lazy val diConfig = new NewBindingModule(module ⇒ {
    module.bind[Symbol ⇒ Node] toSingle { (symbol: Symbol) ⇒ null }
  }) ~ org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default
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

  describe("A Compare By Modification") {
    it("should provide proper comparison") {
      import TestDSL._
      multithread(1000, 10) { i ⇒
        val graph1 = Graph[Model]('john1, Model.scope, new Stub, UUID.randomUUID())
        var myModel1 = graph1.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB")
        var save: Record = null
        val record = myModel1.takeRecord('root) { r ⇒
          save = r.takeRecord('level2) { r ⇒
            r.name = "123"
          }
        }
        val graph2 = Graph[Model]('john2, Model.scope, new Stub, UUID.randomUUID())
        var myModel2 = graph2.model
        graph2.model.eId.name should be("john2")
        CompareByTimespamp.doWith {
          Element.comparator.value should be(CompareByTimespamp)
          myModel1.compare(myModel2) should be(-1) // myModel1 < myModel2
          myModel1.eGet[String]('AAAKey).get.get should be("AAA")
          myModel1.eGet[String]('BBBKey).get.get should be("BBB")
          (myModel1.eId.name) should not be (myModel2.eId.name)
          (myModel1.eUnique) should not be (myModel2.eUnique)
          (myModel1.eStash.origin.name) should not be (myModel2.eStash.origin.name)
          (myModel1.eStash.created) should not be (myModel2.eStash.created)
          (myModel1.eStash.modified) should not be (myModel2.eStash.modified)
          (myModel1.eStash.property) should not be (myModel2.eStash.property)
          // Copy
          myModel2 = myModel1.eCopy(graph2.node)
          (myModel1.eId.name) should not be (myModel2.eId.name)
          (myModel1.eUnique) should not be (myModel2.eUnique)
          (myModel1.eStash.origin.name) should not be (myModel2.eStash.origin.name)
          (myModel1.eStash.created) should be(myModel2.eStash.created)
          (myModel1.eStash.modified) should be(myModel2.eStash.modified)
          (myModel1.eStash.scope) should be(myModel2.eStash.scope)
          (myModel1.eStash.property) should be(myModel2.eStash.property)

          val myModel1Mutable = myModel1.eMutable
          val model1BranchNodes = myModel1Mutable.eNode.threadSafe(_.iteratorRecursive.toIndexedSeq)
          model1BranchNodes should be(Seq(record.eNode, save.eNode))

          myModel1.compare(myModel2) should be(0) // myModel1 == myModel2
          myModel1Mutable.immutable should be(myModel1)
          myModel1Mutable.compare(myModel1) should be(0)
          myModel1Mutable.name = "111"
          myModel1Mutable.compare(myModel1) should be(1) // myModel1Mutable modified after Model1 (myModel1Mutable > Model1)
          val rootRecord = myModel1 | TestDSL.RecordLocation('root)
          rootRecord should be(record)
          // parent modified after child
          record.compare(record) should be(0)

          myModel1Mutable.compare(record) should be(1) // Model modified after record
          myModel1Mutable.compare(save) should be(1) // Model modified after save
          save.compare(record) should be(1) // save modified after record

          myModel1Mutable.name = "123"
          myModel1Mutable.compare(record) should be(1) // Model modified after record
          myModel1Mutable.compare(save) should be(1) // Model modified after record

          val saveMutable = save.eMutable
          saveMutable.name = "321"
          model1BranchNodes.map(_.getRootElementBox.get).sorted.map(_.eId.name) should be(Seq("root", "level2"))
          myModel1Mutable.compare(record) should be(1) // Model modified after
          myModel1Mutable.compare(saveMutable) should be(-1) // Model modified before
          saveMutable.compare(record) should be(1) // saveMutable modified after

          (myModel1Mutable | RecordLocation('root)).compare(record) should be(0)

          record.eMutable.name = "321"
          model1BranchNodes.map(_.getRootElementBox.get).sorted.map(_.eId.name) should be(Seq("level2", "root"))
        }
      }
    }
  }
  describe("A Compare By Content") {
    it("should provide proper comparison") {
      import TestDSL._
      multithread(1000, 10) { i ⇒
        val graph1 = Graph[Model]('john1, Model.scope, new Stub, UUID.randomUUID())
        val model1 = graph1.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eMutable
        val rA1 = model1.takeRecord('rA) { r ⇒
          Thread.sleep(10)
          r.takeRecord('rB) { r ⇒
            Thread.sleep(10)
            r.takeRecord('rLeaf) { r ⇒
              r.name = "123"
            }
          }
        }.eMutable
        val rB1 = (rA1 & RecordLocation('rB)).get.eMutable
        val rLeaf1 = (rB1 & RecordLocation('rLeaf)).get.eMutable

        val graph2 = graph1.copy('john2)
        graph1.model.eStash.created should be(graph2.model.eStash.created)
        val model2 = graph2.model.eMutable
        val rA2 = (model2 & RecordLocation('rA)).get.eMutable
        val rB2 = (rA2 & RecordLocation('rB)).get.eMutable
        val rLeaf2 = (rB2 & RecordLocation('rLeaf)).get.eMutable

        CompareByTimestampAndThenContent.doWith {
          Element.comparator.value should be(CompareByTimestampAndThenContent)
          rB1.compare(rA1) should be(1) // rB1 modified after rA1 record
          model1.immutable should not be (model2.immutable)
          rA1.immutable should not be (rA2.immutable)
          rB1.immutable should not be (rB2.immutable)
          rLeaf1.immutable should not be (rLeaf2.immutable)
          model1.eModel.eq(model1.immutable) should be(true)
          model2.eModel.eq(model2.immutable) should be(true)
          model1.compare(model2) should be(0)
          rA1.compare(rA2) should be(0)
          rB1.compare(rB2) should be(0)
          rLeaf1.compare(rLeaf2) should be(0)
          rA1.compare(rB2) should not be (0)
          rB1.compare(rLeaf2) should not be (0)
          // modify timestamp
          rA2.copy(rA2.eStash.copy(modified = Element.timestamp()))
          rB2.copy(rB2.eStash.copy(modified = Element.timestamp()))
          rLeaf2.copy(rLeaf2.eStash.copy(modified = Element.timestamp()))
          rA1.compare(rA2) should be(0)
          rB1.compare(rB2) should be(0)
          rLeaf1.compare(rLeaf2) should be(0)
          rA1.compare(rB2) should not be (0)
          rB1.compare(rLeaf2) should not be (0)

          // modify model
          model1.compare(model2) should be(0)
          model1 should not be (model2)
          model1.name = "111"
          model1.compare(model2) should be(1) // model1 modified after Model
          model1 should not be (model2)
          (model1 | RecordLocation('root)).eModel should be(model1)
          model1.name = model2.name
          model1.compare(model2) should be(0)
          model1 should not be (model2)

          // parent modified after child
          rA1.compare(rA2) should be(0)
          rB1.compare(rA1) should be(1) // rB1 modified after rA1
          model1.compare(rA1) should be(1) // model1 modified after rA1
          model1.compare(rB1) should be(1) // model1 modified after rB1
          model1.compare(model2) should be(0) // model1 modified after model2, but content is the same
          model1.name = "111"
          model1.compare(model2) should be(1) // model1 modified after model2, but content is the same
          model2.compare(rA2) should be(-1) // model2 modified before rA1
          model2.name = "123"
          model2.compare(rA2) should be(1) // model2 modified after rA1
          model1.compare(rA2) should be(1) // Model modified after rA2
          rA2.name = "321"
          model1.compare(rA2) should be(-1) // model1 modified before rA2
          model1.compare(rB2) should be(1) // model1 modified after rB2
          rA1.compare(rA2) should be(-1) // rA1 modified before rA2
          rA1.name = rA2.name
          rA1.compare(rA2) should be(0)
          model1.name = "333"
          model1.compare(model2) should be(1) // model1 modified after model2
          model2.name = "444"
          model1.compare(model2) should be(-1) // model1 modified before model2
          model1.name = "444"
          (model1 | RecordLocation('rA) | RecordLocation('rB)).eMutable.name = "AAA"
          (model1 | RecordLocation('rA) | RecordLocation('rB)).compare(rB2) should be(1)
          rB2.name = "AAA"
          (model1 | RecordLocation('rA) | RecordLocation('rB)).compare(rB2) should be(0)
        }
      }
    }
  }

  override def beforeAll(configMap: Map[String, Any]) { adjustLoggingBeforeAll(configMap) }
}
