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
import org.digimead.tabuddy.model.serialization.StubSerialization
import org.scalatest.FunSpec
import org.scalatest.Matchers

class CompareSpec extends FunSpec with Matchers with LoggingHelper with Loggable {
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

  describe("A Compare By Modification") {
    it("should provide proper comparison") {
      import TestDSL._
      multithread(1000, 10) { i ⇒
        val graph1 = Graph[Model]('john1, Model.scope, StubSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        var myModel1 = graph1.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB")
        var save: Record = null
        val record = myModel1.takeRecord('root) { r ⇒
          save = r.takeRecord('level2) { r ⇒
            r.name = "123"
          }
        }
        val graph2 = Graph[Model]('john2, Model.scope, StubSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        var myModel2 = graph2.model
        graph2.model.eId.name should be("john2")
        CompareByTimespamp.doWith {
          Element.comparator.value should be(CompareByTimespamp)
          myModel1.compare(myModel2) should be(-1) // myModel1 < myModel2
          myModel1.eGet[String]('AAAKey).get.get should be("AAA")
          myModel1.eGet[String]('BBBKey).get.get should be("BBB")
          (myModel1.eId.name) should not be (myModel2.eId.name)
          (myModel1.eNode.unique) should not be (myModel2.eNode.unique)
          (myModel1.eOrigin.name) should not be (myModel2.eOrigin.name)
          (myModel1.eStash.created) should not be (myModel2.eStash.created)
          (myModel1.eStash.modified) should not be (myModel2.eStash.modified)
          (myModel1.eStash.property) should not be (myModel2.eStash.property)
          // Copy
          myModel2 = myModel1.eCopy(graph2.node)
          (myModel1.eId.name) should not be (myModel2.eId.name)
          (myModel1.eNode.unique) should not be (myModel2.eNode.unique)
          (myModel1.eOrigin.name) should not be (myModel2.eOrigin.name)
          (myModel1.eStash.created) should be(myModel2.eStash.created)
          (myModel1.eStash.modified) should be(myModel2.eStash.modified)
          (myModel1.eStash.scope) should be(myModel2.eStash.scope)
          (myModel1.eStash.property) should be(myModel2.eStash.property)

          val myModel1Relative = myModel1.eRelative
          val model1BranchNodes = myModel1Relative.eNode.safeRead(_.iteratorRecursive.toIndexedSeq)
          model1BranchNodes should be(Seq(record.eNode, save.eNode))

          myModel1.compare(myModel2) should be(0) // myModel1 == myModel2
          myModel1Relative.absolute should be(myModel1)
          myModel1Relative.compare(myModel1) should be(0)
          myModel1Relative.name = "111"
          myModel1Relative.compare(myModel1) should be(1) // myModel1Relative modified after Model1 (myModel1Relative > Model1)
          val rootRecord = myModel1 | TestDSL.RecordLocation('root)
          rootRecord should be(record)
          // parent modified after child
          record.compare(record) should be(0)

          myModel1Relative.compare(record) should be(1) // Model modified after record
          myModel1Relative.compare(save) should be(1) // Model modified after save
          save.compare(record) should be(1) // save modified after record

          myModel1Relative.name = "123"
          myModel1Relative.compare(record) should be(1) // Model modified after record
          myModel1Relative.compare(save) should be(1) // Model modified after record

          val saveRelative = save.eRelative
          saveRelative.name = "321"
          model1BranchNodes.map(_.rootBox.e).sorted.map(_.eId.name) should be(Seq("root", "level2"))
          myModel1Relative.compare(record) should be(1) // Model modified after
          myModel1Relative.compare(saveRelative) should be(-1) // Model modified before
          saveRelative.compare(record) should be(1) // saveRelative modified after

          (myModel1Relative | RecordLocation('root)).compare(record) should be(0)

          record.eRelative.name = "321"
          model1BranchNodes.map(_.rootBox.e).sorted.map(_.eId.name) should be(Seq("level2", "root"))
        }
      }
    }
  }
  describe("A Compare By Content") {
    it("should provide proper comparison") {
      import TestDSL._
      multithread(1000, 10) { i ⇒
        val graph1 = Graph[Model]('john1, Model.scope, StubSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model1 = graph1.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        val rA1 = model1.takeRecord('rA) { r ⇒
          r.takeRecord('rB) { r ⇒
            r.takeRecord('rLeaf) { r ⇒
              r.name = "123"
            }
          }
        }.eRelative
        val rB1 = (rA1 & RecordLocation('rB)).eRelative
        val rLeaf1 = (rB1 & RecordLocation('rLeaf)).eRelative

        val graph2 = graph1.copy(origin = 'john2) { g ⇒ }
        graph1.model.eStash.created should be(graph2.model.eStash.created)
        val model2 = graph2.model.eRelative
        val rA2 = (model2 & RecordLocation('rA)).eRelative
        val rB2 = (rA2 & RecordLocation('rB)).eRelative
        val rLeaf2 = (rB2 & RecordLocation('rLeaf)).eRelative

        CompareByTimestampAndThenContent.doWith {
          Element.comparator.value should be(CompareByTimestampAndThenContent)
          rB1.compare(rA1) should be(1) // rB1 modified after rA1 record
          model1.absolute should not be (model2.absolute)
          rA1.absolute should not be (rA2.absolute)
          rB1.absolute should not be (rB2.absolute)
          rLeaf1.absolute should not be (rLeaf2.absolute)
          model1.eModel.eq(model1.absolute) should be(true)
          model2.eModel.eq(model2.absolute) should be(true)
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
          val a = rA1.eGet[String]('name).get
          val b = rA2.eGet[String]('name).get
          a should be(b)
          rA1.compare(rA2) should be(0) // rA1 modified after, but rA1 == rA2
          model1.name = "333"
          model1.compare(model2) should be(1) // model1 modified after model2
          model2.name = "444"
          model1.compare(model2) should be(-1) // model1 modified before model2
          model1.name = "444"
          (model1 | RecordLocation('rA) | RecordLocation('rB)).eRelative.name = "AAA"
          (model1 | RecordLocation('rA) | RecordLocation('rB)).compare(rB2) should be(1)
          rB2.name = "AAA"
          (model1 | RecordLocation('rA) | RecordLocation('rB)).compare(rB2) should be(0)
        }
      }
    }
  }

  override def beforeAll(configMap: org.scalatest.ConfigMap) { adjustLoggingBeforeAll(configMap) }
}
