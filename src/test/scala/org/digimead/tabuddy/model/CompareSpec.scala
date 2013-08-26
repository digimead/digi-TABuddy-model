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

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.LoggingHelper
import org.digimead.tabuddy.model.Model.model2implementation
import org.digimead.tabuddy.model.TestDSL._
import org.digimead.tabuddy.model.element.compare.CompareByTimespamp
import org.digimead.tabuddy.model.element.compare.CompareByTimestampAndContent
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.digimead.tabuddy.model.element.compare.CompareByTimespamp

class CompareSpec extends FunSpec with ShouldMatchers with LoggingHelper with Loggable {
  after { adjustLoggingAfter }
  before {
    DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
    adjustLoggingBefore
  }

  describe("A Compare By Modification") {
    it("should provide proper comparison") {
      Model.reset()
      var save: Record[Record.Stash] = null
      val record = Model.record('root) { r =>
        save = r.record('level2) { r =>
          r.name = "123"
        }
      }
      CompareByTimespamp.doWith {
        val model1 = Model.eCopy()
        Model.eModel.eq(Model.inner) should be(true)
        model1.eModel.eq(model1) should be(true)
        model1.compare(Model) should be(0)
        model1.name = "111"
        model1.compare(Model) should be(1) // model1 modified after Model
        (model1 | RecordLocation('root)).eModel should be(model1)
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
        (model1 | RecordLocation('root)).compare(record) should be(-1)
      }
    }
  }
  describe("A Compare By Content") {
    it("should provide proper comparison") {
      Model.reset()
      var save: Record[Record.Stash] = null
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
      }
    }
  }

  override def beforeAll(configMap: Map[String, Any]) { adjustLoggingBeforeAll(configMap) }
}
