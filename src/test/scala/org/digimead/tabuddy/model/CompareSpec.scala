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
import org.digimead.tabuddy.model.element.Value.string2someValue
import org.digimead.tabuddy.model.element.compare.CompareByTimespamp
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Graph.graph2interface
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.serialization.Stub
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import com.escalatesoft.subcut.inject.NewBindingModule
import org.digimead.tabuddy.model.dsl.DSL
import org.digimead.tabuddy.model.element.Element

class CompareSpec extends FunSpec with ShouldMatchers with LoggingHelper with Loggable {
  lazy val diConfig = new NewBindingModule(module => {
    module.bind[Symbol => Node] toSingle { (symbol: Symbol) => null }
  }) ~ org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default
  after { adjustLoggingAfter }
  before {
    DependencyInjection(diConfig, false)
    adjustLoggingBefore
  }

  describe("A Compare By Modification") {
    it("should provide proper comparison") {
      import TestDSL._
      val graph1 = Graph[Model]('john1, Model.scope, new Stub, UUID.randomUUID())
      var myModel1 = graph1.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB")
      var save: Record = null
      val record = myModel1.takeRecord('root) { r =>
        save = r.takeRecord('level2) { r =>
          r.name = "123"
        }
      }
      val graph2 = Graph[Model]('john2, Model.scope, new Stub, UUID.randomUUID())
      var myModel2 = graph2.model
      graph2.model.eId.name should be("john2")
      CompareByTimespamp.doWith {
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
        (myModel1.eStash.modified) should not be (myModel2.eStash.modified)
        (myModel1.eStash.property) should be(myModel2.eStash.property)

        myModel1.compare(myModel2) should be(-1) // myModel1 < myModel2
        val myModel1Mutable = myModel1.mutable
        myModel1Mutable.immutable should be(myModel1)
        myModel1Mutable.compare(myModel1) should be(0)
        myModel1Mutable.name = "111"
        myModel1Mutable.compare(myModel1) should be(1) // myModel1Mutable modified after Model1 (myModel1Mutable > Model1)
        val rootRecord = myModel1 | TestDSL.RecordLocation('root)
        rootRecord should be (record)
        // parent modified after child
        /*record.compare(record) should be(0)
        save.compare(record) should be(-1) // save modified before record
        Model.compare(record) should be(1) // Model modified after record
        Model.compare(save) should be(1) // Model modified after save
        Model.name = "123"
        Model.compare(record) should be(1) // Model modified after record
        Model.compare(save) should be(1) // Model modified after record
        save.name = "321"
        Model.compare(record) should be(1) // Model modified after record
        Model.compare(save) should be(1) // Model modified after save
        record.compare(save) should be(1) // record modified after save
        (model1 | RecordLocation('root)).compare(record) should be(-1)*/
      }
    }
  }
  describe("A Compare By Content") {
    it("should provide proper comparison") {
      //Model.reset()
      /*      var save: Record[Record.Stash] = null
      val record = Model.record('root) { r =>
        save = r.record('level2) { r =>
          r.name = "123"
        }
      }
      CompareByTimestampAndContent.doWith {
        val model1 = Model.eCopy()
        Model.eModel.eq(Model.inner) should be(true)
        model1.eModel.eq(model1) should be(true)
        model1.compare(Model) should be(0)
        model1.name = "111"
        model1.compare(Model) should be(1) // model1 modified after Model
        (model1 | RecordLocation('root)).eModel should be(model1)
        model1.name = Model.name
        model1.compare(Model) should be(0)
        // parent modified after child
        record.compare(record) should be(0)
        save.compare(record) should be(-1) // save modified before record
        Model.compare(record) should be(1) // Model modified after record
        Model.compare(save) should be(1) // Model modified after save
        Model.name = "123"
        Model.compare(record) should be(1) // Model modified after record
        Model.compare(save) should be(1) // Model modified after record
        save.name = "321"
        Model.compare(record) should be(1) // Model modified after record
        Model.compare(save) should be(1) // Model modified after save
        record.compare(save) should be(1) // record modified after save
        model1.compare(Model) should be(0) // model1 modified before Model BUT model1 content and Model content are the same
        Model.name = "333"
        model1.compare(Model) should be(-1) // model1 modified before Model
        model1.name = "444"
        model1.compare(Model) should be(1) // model1 modified before Model
        Model.name = "444"
        //(model1 | RecordLocation('root) | RecordLocation('level2)).name = "321"
        model1.compare(Model) should be(0) // model1 modified after Model but content is equal
      }*/
    }
  }

  override def beforeAll(configMap: Map[String, Any]) { adjustLoggingBeforeAll(configMap) }
}
