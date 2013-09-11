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
import org.digimead.tabuddy.model.element.Value.string2someValue
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Graph.graph2interface
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.serialization.StubSerialization
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import com.escalatesoft.subcut.inject.NewBindingModule

class ModelSpec extends FunSpec with ShouldMatchers with LoggingHelper with Loggable {
  lazy val diConfig = org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default
  after { adjustLoggingAfter }
  before {
    DependencyInjection(diConfig, false)
    adjustLoggingBefore
  }

  describe("A Model") {
    it("should attach and detach element") {
      import TestDSL._
      val graph = Graph[Model]('john1, Model.scope, StubSerialization.Identifier, UUID.randomUUID())
      val model1 = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eMutable
      val rA1 = model1.takeRecord('rA) { r ⇒
        r.takeRecord('rB) { r ⇒
          r.takeRecord('rLeaf) { r ⇒
            r.name = "123"
          }
        }
      }.eMutable
      val rB1 = (rA1 & RecordLocation('rB)).eMutable
      val rLeaf1 = (rB1 & RecordLocation('rLeaf)).eMutable

      val graphCopy = graph.copy('john2)
      graphCopy.nodes.size should be(graph.nodes.size)
      graphCopy.nodes.values.toSeq.sortBy(_.unique) should be(graph.nodes.values.toSeq.sortBy(_.unique))
      graphCopy.nodes.values.toSeq.sortBy(_.unique).corresponds(graph.nodes.values.toSeq.sortBy(_.unique))(_.unique == _.unique) should be(true)
      graphCopy.nodes.values.toSeq.sortBy(_.unique).corresponds(graph.nodes.values.toSeq.sortBy(_.unique))(_ ne _) should be(true)
      graphCopy.node.safeRead(_.iteratorRecursive()).size should be(graphCopy.nodes.values.size - 1)
      (graphCopy.node.safeRead(_.iteratorRecursive()).toSeq :+ graphCopy.node).sortBy(_.unique) should be(graphCopy.nodes.values.toSeq.sortBy(_.unique))
      (graphCopy.node.safeRead(_.iteratorRecursive()).toSeq :+ graphCopy.node).sortBy(_.unique).corresponds(graphCopy.nodes.values.toSeq.sortBy(_.unique))(_ eq _) should be(true)
      graphCopy.model should equal(graphCopy.model.eModel)
      graphCopy.model.eq(graphCopy.model.eModel) should be(true)
      val recordCopy = graphCopy.model.eNode.safeRead(_.head).getRootElementBox.get
      recordCopy.eModel.eq(graphCopy.model.eModel) should be(true)
      recordCopy.eId.name should be("rA")
      recordCopy.eNode.safeRead(_.size) should be(1)
      model1.eNode.safeRead(_.iteratorRecursive().toSeq) should have size (3)
      model1.eNode.safeRead(_.iterator.toSeq) should have size (1)
      rB1.eNode.safeRead(_.iterator.toSeq) should have size (1)
      rLeaf1.eNode.safeRead(_.children) should be('empty)
      (graphCopy.model & RecordLocation('rA) & RecordLocation('rB) & RecordLocation('rLeaf)).eNode.safeRead(_.children) should be('empty)
    }
  }

  override def beforeAll(configMap: Map[String, Any]) { adjustLoggingBeforeAll(configMap) }
}
