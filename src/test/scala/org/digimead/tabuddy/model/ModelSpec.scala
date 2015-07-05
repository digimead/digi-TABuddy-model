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
import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.XLoggable
import org.digimead.lib.test.LoggingHelper
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.StubSerialization
import org.scalatest.{ FunSpec, Matchers }

class ModelSpec extends FunSpec with Matchers with LoggingHelper with XLoggable {
  lazy val diConfig = org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default
  before { DependencyInjection(diConfig, false) }

  describe("A Model") {
    it("should attach and detach element") {
      import TestDSL._

      val graph = Graph[Model]('john1, Model.scope, StubSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model1 = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      val rA1 = model1.getRecord('rA) { r ⇒
        r.getRecord('rB) { r ⇒
          r.getRecord('rLeaf) { r ⇒
            r.name = "123"
          }
        }
      }.eRelative
      val rB1 = (rA1 & RecordLocation('rB)).eRelative
      val rLeaf1 = (rB1 & RecordLocation('rLeaf)).eRelative

      val graphCopy = graph.copy(origin = 'john2)(_ ⇒ ())
      graphCopy.nodes.size should be(graph.nodes.size)
      graphCopy.nodes.values.toSeq.sortBy(_.unique) should be(graph.nodes.values.toSeq.sortBy(_.unique))
      graphCopy.nodes.values.toSeq.sortBy(_.unique).corresponds(graph.nodes.values.toSeq.sortBy(_.unique))(_.unique == _.unique) should be(true)
      graphCopy.nodes.values.toSeq.sortBy(_.unique).corresponds(graph.nodes.values.toSeq.sortBy(_.unique))(_ ne _) should be(true)
      graphCopy.node.safeRead(_.iteratorRecursive).size should be(graphCopy.nodes.values.size - 1)
      (graphCopy.node.safeRead(_.iteratorRecursive).toSeq :+ graphCopy.node).sortBy(_.unique) should be(graphCopy.nodes.values.toSeq.sortBy(_.unique))
      (graphCopy.node.safeRead(_.iteratorRecursive).toSeq :+ graphCopy.node).sortBy(_.unique).corresponds(graphCopy.nodes.values.toSeq.sortBy(_.unique))(_ eq _) should be(true)
      graphCopy.model should equal(graphCopy.model.eModel)
      graphCopy.model.eq(graphCopy.model.eModel) should be(true)
      val recordCopy = graphCopy.model.eNode.safeRead(_.head).rootBox.e
      recordCopy.eModel.eq(graphCopy.model.eModel) should be(true)
      recordCopy.eId.name should be("rA")
      recordCopy.eNode.safeRead(_.size) should be(1)
      model1.eNode.safeRead(_.iteratorRecursive.toSeq) should have size (3)
      model1.eNode.safeRead(_.iterator.toSeq) should have size (1)
      rB1.eNode.safeRead(_.iterator.toSeq) should have size (1)
      rLeaf1.eNode.safeRead(_.children) should be('empty)
      (graphCopy.model & RecordLocation('rA) & RecordLocation('rB) & RecordLocation('rLeaf)).eNode.safeRead(_.children) should be('empty)

      val m1 = graph.modified
      val q = (graph.model & RecordLocation('rA) & RecordLocation('rB) & RecordLocation('rLeaf) | RecordLocation('rX)).eRelative
      graph.modified should be > (m1)
      val m2 = graph.modified
      q.eSet[String]('abc, "abc")
      graph.modified should be > (m2)
    }
    it("should proper nodes copy functions") {
      import TestDSL._

      val graph = Graph[Model]('john1, Model.scope, StubSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model1 = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      val rA1 = model1.getRecord('rA) { r ⇒
        r.getRecord('rB) { r ⇒
          r.getRecord('rLeaf) { r ⇒
            r.name = "123"
          }
        }
      }.eRelative
      graph.node.safeRead(_.iteratorRecursive.size) should be(3)
      val rA2Node = rA1.eNode.copy(id = 'rA2, unique = UUID.randomUUID())
      rA1.eNode.safeRead { node ⇒
        rA2Node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒
            a.ne(b) && a.unique != b.unique && a.id == b.id && a.modified == b.modified && a.elementType == b.elementType
          }
        }
      } should be(true)
      graph.node.safeRead(_.iteratorRecursive.size) should be(6)
      val rA3Node = rA1.eNode.copy(id = 'rA3, unique = UUID.randomUUID(), recursive = false)
      println(model1.eDump(true))
      graph.node.safeRead(_.iteratorRecursive.size) should be(7)
    }
  }

  override def beforeAll(configMap: org.scalatest.ConfigMap) { adjustLoggingBeforeAll(configMap) }
}
