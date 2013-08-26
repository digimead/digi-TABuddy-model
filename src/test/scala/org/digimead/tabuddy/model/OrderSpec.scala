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
import org.digimead.tabuddy.model.Model.Stash
import org.digimead.tabuddy.model.Model.model2implementation
import org.digimead.tabuddy.model.TestDSL._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class OrderSpec extends FunSpec with ShouldMatchers with LoggingHelper with Loggable {
  after { adjustLoggingAfter }
  before {
    DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
    adjustLoggingBefore
  }

  describe("An Element") {
    it("should be comparable") {
      Model.reset()
      // create
      val record1 = Model | RecordLocation('record1)
      val record2 = Model | RecordLocation('record2)
      log.___glance("compare %s %d#%d against %s %d#%d".format(record1, record1.eStash.modified.milliseconds, record1.eStash.modified.nanoShift,
        record2, record2.eStash.modified.milliseconds, record2.eStash.modified.nanoShift))
      Seq(record2, record1).sorted should be(Seq(record1, record2))
      record1 < record2 should be(true)
      // modify property
      val record1copy = record1.eCopy()
      record1copy eq record1 should be(false)
      record1copy.eStash eq record1.eStash should be(false)
      record1copy.canEqual(record1.getClass(), record1.eStash.getClass()) should be(true)
      record1copy.canEqual(record1.getClass(), classOf[Model.Stash]) should be(false)
      assert(record1copy.eStash.context === record1.eStash.context)
      assert(record1copy.eStash.coordinate === record1.eStash.coordinate)
      assert(record1copy.eStash.created === record1.eStash.created)
      assert(record1copy.eStash.id === record1.eStash.id)
      assert(record1copy.eStash.scope === record1.eStash.scope)
      assert(record1copy.eStash.unique === record1.eStash.unique)
      assert(record1copy.eStash === record1.eStash)
      assert(record1copy === record1)
      record1.name = "123"
      record1 > record2 should be(true)
      // record1 changed
      assert(record1copy.eStash != record1.eStash)
    }
  }

  override def beforeAll(configMap: Map[String, Any]) { adjustLoggingBeforeAll(configMap) }
}
