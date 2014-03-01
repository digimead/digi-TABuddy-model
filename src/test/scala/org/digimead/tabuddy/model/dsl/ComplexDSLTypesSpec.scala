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

import java.util.UUID
import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.LoggingHelper
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.TestDSL
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.StubSerialization
import org.scalatest.{ FunSpec, Matchers }

class ComplexDSLTypesSpec extends FunSpec with Matchers with LoggingHelper with Loggable {
  before { DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false) }

  describe("A ComplexDSLTypes") {
    it("should have proper equality") {
      val dslType = new ComplexDSLTypes
      dslType.types should be(Seq('ArrayOfSymbol, 'Reference))
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
    it("should provide basic functions") {
      import TestDSL._

      // define record
      val graph = Graph[Model]('john1, Model.scope, StubSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      val record_0 = model.takeRecord('baseLevel) { r ⇒
        r.takeRecord('level1a) { r ⇒
          r.takeRecord('level2a) { r ⇒
            r.name = "record_2a"
          }
          r.name = "record_1a"
        }
        r.takeRecord('level1b) { r ⇒
          r.takeRecord('level2b) { r ⇒
            r.name = "record_2b"
          }
          r.name = "record_1b"
        }
        r.name = "record_0"
      }.eRelative
      model.transform(RecordLocation('baseLevel)) { record ⇒
        record.name should be("record_0")
      }
    }
  }

  override def beforeAll(configMap: org.scalatest.ConfigMap) { adjustLoggingBeforeAll(configMap) }
}
