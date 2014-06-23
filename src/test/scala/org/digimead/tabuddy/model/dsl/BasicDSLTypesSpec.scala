/**
 * TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2012-2014 Alexey Aksenov ezh@ezh.msk.ru
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
import org.digimead.digi.lib.log.api.XLoggable
import org.digimead.lib.test.LoggingHelper
import org.scalatest.{ FunSpec, Matchers }

class BasicDSLTypesSpec extends FunSpec with Matchers with LoggingHelper with XLoggable {
  before { DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false) }

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

  override def beforeAll(configMap: org.scalatest.ConfigMap) { adjustLoggingBeforeAll(configMap) }
}
