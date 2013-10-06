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

package org.digimead.tabuddy.model.dsl

import java.util.UUID

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.LoggingHelper
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.TestDSL
import org.digimead.tabuddy.model.element.Value.string2someValue
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Graph.graph2interface
import org.digimead.tabuddy.model.serialization.StubSerialization
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class BasicDSLTypesSpec extends FunSpec with ShouldMatchers with LoggingHelper with Loggable {
  after { adjustLoggingAfter }
  before {
    DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
    adjustLoggingBefore
  }

  describe("A BasicDSLTypes") {
    it("should have proper equality") {
      val dslType = new BasicDSLTypes
      dslType.getTypeSymbol(classOf[Null]) should be(Some('Null))
      dslType.getTypeSymbol(classOf[Integer]) should be(Some('Integer))
      dslType.getTypeSymbol(classOf[String]) should be(Some('String))
    }
    it("should have proper converter") {
      val dslType = new BasicDSLTypes
      val saved = dslType.convertToString('Null, null)
      saved should be(null)
      val restored = dslType.convertFromString('Null, null)
      restored should be(null)
    }
  }

  override def beforeAll(configMap: Map[String, Any]) { adjustLoggingBeforeAll(configMap) }
}