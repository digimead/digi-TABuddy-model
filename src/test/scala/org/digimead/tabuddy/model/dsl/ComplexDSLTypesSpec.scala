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

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.LoggingHelper
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ComplexDSLTypesSpec extends FunSpec with ShouldMatchers with LoggingHelper with Loggable {
  after { adjustLoggingAfter }
  before {
    DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
    adjustLoggingBefore
  }

  describe("A ComplexDSLTypes") {
    it("should have proper equality") {
      val dslType = new ComplexDSLTypes
      dslType.getTypes should be(Seq('ArrayOfSymbol))
      dslType.getTypeSymbol(classOf[Array[Int]]) should be(None)
      dslType.getTypeSymbol(classOf[Array[Symbol]]) should be(Some('ArrayOfSymbol))
      dslType.getTypeSymbol(classOf[Array[String]]) should be(None)
      dslType.getTypeSymbol(classOf[Seq[String]]) should be(None)
      dslType.getTypeSymbol(classOf[Seq[Symbol]]) should be(None)
      dslType.getTypeSymbol(classOf[List[Symbol]]) should be(None)
    }
    it("should have proper converter") {
      val dslType = new ComplexDSLTypes
      val saved = dslType.convertToString('ArrayOfSymbol, Array('abba, 'mamba, 'dubba))
      saved should be("abba mamba dubba")
      val restored = dslType.convertFromString('ArrayOfSymbol, saved)
      restored should be(Array('abba, 'mamba, 'dubba))
    }
  }

  override def beforeAll(configMap: Map[String, Any]) { adjustLoggingBeforeAll(configMap) }
}
