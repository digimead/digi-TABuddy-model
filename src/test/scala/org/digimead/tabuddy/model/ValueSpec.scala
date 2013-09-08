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
import org.digimead.tabuddy.model.element.Value
import org.digimead.tabuddy.model.element.Value.string2someValue
import org.digimead.tabuddy.model.element.Value.value2x
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Graph.graph2interface
import org.digimead.tabuddy.model.serialization.Stub
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ValueSpec extends FunSpec with ShouldMatchers with LoggingHelper with Loggable {
  after { adjustLoggingAfter }
  before {
    DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
    adjustLoggingBefore
  }

  describe("A Value") {
    it("should have a proper equality") {
      import TestDSL._
      val graph = Graph[Model]('john1, Model.scope, new Stub, UUID.randomUUID())
      val model = graph.model
      val container = model.record('test).eMutable
      new Value.Static("123", Value.Context(container)) === new Value.Static("123", Value.Context(container)) should be(true)
      new Value.Static("123", Value.Context(container)) == new Value.Static("123", Value.Context(container)) should be(true)
//      new Value.Static("123", Value.Context(container)) === new Value.Static("123", Value.Context(container.eReference, None, Some(0), None)) should be(false)
//      new Value.Static("123", Value.Context(container)) == new Value.Static("123", Value.Context(container.eReference, None, Some(0), None)) should be(true)
      new Value.Static("123", Value.Context(container)) === new Value.Static(Int.box(123), Value.Context(container)) should be(false)
      new Value.Static("123", Value.Context(container)) === new Value.Dynamic(() ⇒ "123", Value.Context(container)) should be(false)
      new Value.Static("123", Value.Context(container)) == new Value.Dynamic(() ⇒ "123", Value.Context(container)) should be(true)
      new Value.Static("123", Value.Context(container)) == new Value.Dynamic(() ⇒ Int.box(123), Value.Context(container)) should be(false)
    }
    it("should should be affected by implicit conversions") {
      import TestDSL._
      val graph = Graph[Model]('john1, Model.scope, new Stub, UUID.randomUUID())
      val model = graph.model
      val rootWorkspace = model.record('test).eMutable
      implicit val container = rootWorkspace.immutable
      // x2value: convert T -> Option[Value[T]]
      val context = Value.Context(rootWorkspace)
      var test: Option[Value[String]] = None
      test = "123"
//      test.get.context.unique should not be (UUID.fromString("00000000-0000-0000-0000-000000000000"))
//      test.get.context.unique should be(rootWorkspace.eReference.unique)
      test.get === new Value.Static("123", test.get.context) should be(true)
      test.get == new Value.Static("123", test.get.context) should be(true)
      // value2x: convert Value[T] -> T
      val a: String = test.get
      a should be("123")
    }
    it("should should may have an empty container") {
      import TestDSL._
      val graph = Graph[Model]('john1, Model.scope, new Stub, UUID.randomUUID())
      val model = graph.model
      val rootWorkspace = model.record('test).eMutable
      // x2value: convert T -> Option[Value[T]]
      val context = Value.Context(rootWorkspace)
      var test: Option[Value[String]] = None
      test = "123"
//      test.get.context.unique should be(UUID.fromString("00000000-0000-0000-0000-000000000000"))
      test.get === new Value.Static("123", test.get.context) should be(true)
      test.get == new Value.Static("123", test.get.context) should be(true)
      // value2x: convert Value[T] -> T
      val a: String = test.get
      a should be("123")
    }
    it("should search a default value at the root node") {
      import TestDSL._
      val graph = Graph[Model]('john1, Model.scope, new Stub, UUID.randomUUID())
      val model = graph.model
      val rootWorkspace = model.record('test).eMutable
      rootWorkspace.eCoordinate.isRoot should be(true)
      rootWorkspace.name should be("")
      val otherWorkspace = model.record('test, ('a, 0)).eMutable
      // child of same root with same id MUST have same unique values
      rootWorkspace.eId.name should be(otherWorkspace.eId.name)
      rootWorkspace.eNodeId should be(otherWorkspace.eNodeId)
      //rootWorkspace.unique
      otherWorkspace.eCoordinate.isRoot should be(false)
      otherWorkspace.name should be("")
      rootWorkspace.name = "test"
      rootWorkspace.name should be("test")
      // default value from root element
      otherWorkspace.name should be("test")
      otherWorkspace.name = "testOther"
      otherWorkspace.name should be("testOther")
      rootWorkspace.name should be("test")
    }
    it("should create values with builder functions") {
      import TestDSL._
      val graph = Graph[Model]('john1, Model.scope, new Stub, UUID.randomUUID())
      val model = graph.model
      val record1 = model.record('test)
      val value1 = Value.static("A")
      val value2 = Value.static(record1, "A")
      val value3 = Value.dinamic(() ⇒ "B")
      val value4 = Value.dinamic(record1, () ⇒ "B")
      value1.get should be(value2.get)
      value3.get should be(value4.get)
      value1.context should be(Value.Context())
      value3.context should be(Value.Context())
      value2.context should not be (Value.Context())
      value4.context should not be (Value.Context())
//      value2.context.unique should be(record1.eNodeId)
//      value2.context.origin.name should be(record1.eOrigin.name)
//      value4.context.unique should be(record1.eNodeId)
//      value4.context.origin.name should be(record1.eOrigin.name)
    }
  }
  describe("A Value.Context") {
    it("should have a proper equality") {
      import TestDSL._
      val graph = Graph[Model]('john1, Model.scope, new Stub, UUID.randomUUID())
      val model = graph.model
      val rootWorkspace = model.record('test)
//      Value.Context(rootWorkspace.eReference, None, None, None) should be(Context(rootWorkspace.eReference, None, None, None))
//      Value.Context(rootWorkspace.eReference, Some(null), None, None) should not be (Context(rootWorkspace.eReference, None, None, None))
//      Value.Context(rootWorkspace.eReference, None, Some(0), None) should not be (Context(rootWorkspace.eReference, None, None, None))
//      Value.Context(rootWorkspace.eReference, None, None, Some(null)) should not be (Context(rootWorkspace.eReference, None, None, None))
    }
  }

  override def beforeAll(configMap: Map[String, Any]) { adjustLoggingBeforeAll(configMap) }
}
