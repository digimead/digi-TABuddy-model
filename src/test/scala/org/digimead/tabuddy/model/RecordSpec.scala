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

package org.digimead.tabuddy.model

import TestDSL._
import java.util.UUID
import java.util.concurrent.atomic.AtomicReference
import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.LoggingHelper
import org.digimead.tabuddy.model.element.{ Coordinate, Element, Stash }
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.StubSerialization
import org.scalatest.{ FunSpec, Matchers }

class RecordSpec extends FunSpec with Matchers with LoggingHelper with Loggable {
  before { DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false) }

  describe("A Record") {
    it("should create new instance") {
      import TestDSL._
      val graph = Graph[Model]('john1, Model.scope, StubSerialization.Identifier, UUID.randomUUID()) { g ⇒ }

      val record1Timestamp = Element.timestamp()
      // 1. create node
      val record1 = graph.node.safeWrite(_.createChild[Record]('test1, UUID.randomUUID()).safeWrite { test1Node ⇒
        // 2. create element box
        val elementElementForwardReference = new AtomicReference[Record](null)
        val elementBox = ElementBox[Record](Coordinate.root, UUID.randomUUID(), Right(elementElementForwardReference),
          Element.Timestamp(0, 0))(elementType = implicitly[Manifest[Record]],
            node = test1Node, serialization = graph.model.eBox.serialization)
        test1Node.updateState(projectionBoxes = test1Node.projectionBoxes.updated(Coordinate.root, elementBox))
        // 3. create element
        elementElementForwardReference.set(Element[Record](elementBox, record1Timestamp, record1Timestamp, new Stash.Data(), Record.scope))
        // 4. initialize box and get element
        elementBox.e
      })

      val record2Timestamp = Element.timestamp()
      // 1. create node
      val record2 = graph.node.safeWrite(_.createChild[Record]('test2, UUID.randomUUID()).safeWrite { test2Node ⇒
        // 2. create element box
        val elementElementForwardReference = new AtomicReference[Record](null)
        val elementBox = ElementBox[Record](Coordinate.root, UUID.randomUUID(), Right(elementElementForwardReference),
          Element.Timestamp(0, 0))(elementType = implicitly[Manifest[Record]],
            node = test2Node, serialization = graph.model.eBox.serialization)
        test2Node.updateState(projectionBoxes = test2Node.projectionBoxes.updated(Coordinate.root, elementBox))
        // 3. create element
        elementElementForwardReference.set(Element[Record](elementBox, record2Timestamp, record2Timestamp, new Stash.Data(), Record.scope))
        // 4. initialize box and get element
        elementBox.e
      })

      record1 should not be (record2)
      record1.eId.name should be("test1")
      record2.eId.name should be("test2")
      graph.model.eNode.safeRead(_.iteratorRecursive.toSeq.size) should be(2)
    }
    it("should support nested elements") {
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
      // check description
      record_0.name should be("record_0")
      (record_0 & RecordLocation('level1a)).name should be("record_1a")
      (record_0 & RecordLocation('level1a) & RecordLocation('level2a)).name should be("record_2a")
      (record_0 & RecordLocation('level1b)).name should be("record_1b")
      (record_0 & RecordLocation('level1b) & RecordLocation('level2b)).name should be("record_2b")
      // check child elements
      val record_1a = record_0 & RecordLocation('level1a)
      val record_1b = record_0 & RecordLocation('level1b)
      record_0.eNode.safeRead(_.children.map(_.rootBox.e).toSet) should equal(Set(record_1a, record_1b))
      val record_2a = record_0 & RecordLocation('level1a) & RecordLocation('level2a)
      record_1a.eNode.safeRead(_.children.map(_.rootBox.e).toSet) should equal(Set(record_2a))
      val record_2b = record_0 & RecordLocation('level1b) & RecordLocation('level2b)
      record_1b.eNode.safeRead(_.children.map(_.rootBox.e).toSet) should equal(Set(record_2b))

      val treeA = model.takeRecord('baseLevel) { r ⇒
        r.takeRecord('level1a) { r ⇒
          r.takeRecord('level2a) { r ⇒
            r.name should be("record_2a")
            r.name = "ok"
          }
        }
      }
      model.eNode.safeRead { _.iteratorRecursive.find(_.id == 'level2a) }.
        flatMap(_.rootBox.e.eAs[Record].map(_.name)) should be(Some("ok"))
    }
  }

  override def beforeAll(configMap: org.scalatest.ConfigMap) { adjustLoggingBeforeAll(configMap) }
}
