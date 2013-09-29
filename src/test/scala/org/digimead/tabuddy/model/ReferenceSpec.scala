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
import org.digimead.lib.test.StorageHelper
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Reference
import org.digimead.tabuddy.model.element.Reference.reference2implementation
import org.digimead.tabuddy.model.element.Value.string2someValue
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Graph.graph2interface
import org.digimead.tabuddy.model.serialization.BuiltinSerialization
import org.digimead.tabuddy.model.serialization.Serialization
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ReferenceSpec extends FunSpec with StorageHelper with ShouldMatchers with LoggingHelper with Loggable {
  after { adjustLoggingAfter }
  before {
    DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
    adjustLoggingBefore
  }

  describe("A Reference") {
    it("should have proper default registry implemetation") {
      withTempFolder { folder ⇒
        import TestDSL._

        // graph
        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
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

        Reference.resolve(record_0.eReference, Element.timestamp(0, 0)) should be('empty)
        Reference.register(graph)
        Reference.resolve(record_0.eReference, Element.timestamp(0, 0)) should be('empty)
        graph.storages = graph.storages :+ folder.getAbsoluteFile().toURI()
        val timestamp1 = Serialization.freeze(graph)
        timestamp1 should be(graph.stored.last)
        timestamp1 should be(graph.node.modified)
        timestamp1 should be(graph.modified)
        Reference.resolve(record_0.eReference, Element.timestamp(0, 0)) should be('empty)
        Reference.resolve(record_0.eReference, timestamp1) should not be ('empty)
        val timestamp2 = Serialization.freeze(graph)
        timestamp2 should be(timestamp1) // no changes
        graph.stored should have size (1)
        record_0.name = "111"
        val timestamp3 = Serialization.freeze(graph)
        timestamp3 should not be (timestamp1)
        Reference.resolve(record_0.eReference, timestamp1) should be('empty)
        Reference.resolve(record_0.eReference, timestamp3) should not be ('empty)
        Reference.register(Serialization.acquire(graph.origin, graph.storages.head, Some(timestamp1))(_ ⇒ ()))
        Reference.resolve(record_0.eReference, timestamp1) should not be ('empty)
        Reference.resolve(record_0.eReference, timestamp3) should not be ('empty)
        val modified1 = graph.modified
        val copy = graph.copy()(_ ⇒ ())
        Reference.register(copy)
        graph.modified should be(modified1)
        copy should be(graph)
        graph.node.safeRead { node ⇒
          copy.node.safeRead { node2 ⇒
            node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a == b }
          }
        } should be(true)
        copy.modified should be(graph.modified)
        record_0.name = "222"
        val timestamp4 = Serialization.freeze(graph)
        Reference.resolve(record_0.eReference, timestamp1) should not be ('empty)
        Reference.resolve(record_0.eReference, timestamp2) should not be ('empty)
        Reference.resolve(record_0.eReference, timestamp3) should not be ('empty)
        Reference.resolve(record_0.eReference, timestamp4) should not be ('empty)
        Reference.resolve(record_0.eReference, timestamp1).flatMap(_.eAs[Record].flatMap(_.name)).map(_.get) should be(Some("record_0"))
        Reference.resolve(record_0.eReference, timestamp2).flatMap(_.eAs[Record].flatMap(_.name)).map(_.get) should be(Some("record_0"))
        Reference.resolve(record_0.eReference, timestamp3).flatMap(_.eAs[Record].flatMap(_.name)).map(_.get) should be(Some("111"))
        Reference.resolve(record_0.eReference, timestamp4).flatMap(_.eAs[Record].flatMap(_.name)).map(_.get) should be(Some("222"))
      }
    }
  }
}
