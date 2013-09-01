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
//import org.digimead.tabuddy.model.TestDSL.element2rich
//import org.digimead.tabuddy.model.TestDSL.model2rich
import org.digimead.tabuddy.model.element.Coordinate
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class RecordSpec extends FunSpec with ShouldMatchers with LoggingHelper with Loggable {
  after { adjustLoggingAfter }
  before {
    DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
    adjustLoggingBefore
  }

  describe("A Record") {
    it("should create new instance with apply()") {
      /*val record1 = Record.apply(classOf[Record[Record.Stash]], classOf[Record.Stash], Some(Model.inner), 'test1, Record.scope, Coordinate.root.coordinate, (n: Record[Record.Stash]) => { "" })
      val record2 = Record.apply(Model, 'test2, Coordinate.root.coordinate, (n: Record[Record.Stash]) => { "" })
      val record2a = Record.apply(Model, 'test2, Coordinate.root.coordinate, (n: Record[Record.Stash]) => { "" })
      assert(record2a eq record2)
    }
    it("should support nested elements") {
      var record_1a: Record[Record.Stash] = null
      var record_2a: Record[Record.Stash] = null
      var record_1b: Record[Record.Stash] = null
      var record_2b: Record[Record.Stash] = null
      // define record
      val record_0 = Model.record('baseLevel) { r =>
        record_1a = r.record('level1a) { r =>
          record_2a = r.record('level2a) { r =>
            r.name = "record_2a"
          }
          r.name = "record_1a"
        }
        record_1b = r.record('level1b) { r =>
          record_2b = r.record('level2b) { r =>
            r.name = "record_2b"
          }
          r.name = "record_1b"
        }
        r.name = "record_0"
      }
      // check description
      record_0.name should be("record_0")
      record_1a.name should be("record_1a")
      record_2a.name should be("record_2a")
      record_1b.name should be("record_1b")
      record_2b.name should be("record_2b")
      // check child elements
      record_0.eChildren should equal(Set(record_1a, record_1b))
      record_1a.eChildren should equal(Set(record_2a))
      record_1b.eChildren should equal(Set(record_2b))
      // check elements with same id
      val treeA = Model.record('test) { r =>
        r.record('test) { r =>
          r.record('test) { _.name = "ok" }
        }
      }
      //find[Note[Note.Stash]](treeA, 'test, 'test, 'test).map(_.name) should be(Some("ok"))*/
    }
  }

  override def beforeAll(configMap: Map[String, Any]) { adjustLoggingBeforeAll(configMap) }
}
