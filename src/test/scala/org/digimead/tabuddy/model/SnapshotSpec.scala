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
import org.digimead.digi.lib.aop.log
import org.scalatest.matchers.ShouldMatchers
import com.escalatesoft.subcut.inject.NewBindingModule
//import org.digimead.tabuddy.model.TestDSL._
import org.scalatest.FunSpec
import org.digimead.lib.test.LoggingHelper
import org.digimead.digi.lib.log.api.Loggable

class SnapshotSpec extends FunSpec with ShouldMatchers with LoggingHelper with Loggable {
  after { adjustLoggingAfter }
  before {
    DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
    adjustLoggingBefore
  }

  describe("A Snapshot") {
    it("should contain persistent values") {
      /*        Model.name should be ("")
        Model.sCurrent should be(Element.Snapshot(0L))
        val globalSnapshot = Model.sTake()
        globalSnapshot should not be (null)
        globalSnapshot.sCurrent should not be (Element.Snapshot(0L))
        //log.___glance("!!!" + Model.stashMap)
        // set property
        globalSnapshot.name  should be ("")
        Model.name = "123"
        Model.name should be ("123")
        globalSnapshot.name  should be ("")*/

      //val snapshot = Model.sTake()
      //snapshot
      /*val snapshotPointer = Model.snapshotTake()
        Model.withSnapshot(asdf) {
          snapshot =>
            snapshot.compareTo(Model)
        }*/
    }
  }

  override def beforeAll(configMap: Map[String, Any]) { adjustLoggingBeforeAll(configMap) }
}
