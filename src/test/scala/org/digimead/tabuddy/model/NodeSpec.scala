/**
 * TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2014 Alexey Aksenov ezh@ezh.msk.ru
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
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.LoggingHelper
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.StubSerialization
import org.scalatest.{ FunSpec, Matchers }
import scala.language.implicitConversions

class NodeSpec extends FunSpec with Matchers with LoggingHelper with Loggable {
  lazy val diConfig = org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default
  before { DependencyInjection(diConfig, false) }

  describe("A Node") {
    it("should preserve modification timestamp when is not attached.") {
      import TestDSL._
      val graph1 = Graph[Model]('john1, Model.scope, StubSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model1 = graph1.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      val rA1 = model1.takeRecord('rA) { r ⇒
        r.takeRecord('rAB) { r ⇒
          r.takeRecord('rLeaf) { r ⇒
            r.name = "123"
          }
        }
      }.eRelative

      val modifiedOriginal = graph1.modified
      val rA1Node = rA1.eNode // rA1 is relative
      rA1Node.safeRead(_.state.attached) should be(true)
      val rA1NodeCopy = rA1.eNode.copy(attach = true)
      rA1Node.safeRead(_.state.attached) should be(false)
      rA1NodeCopy.safeRead(_.state.attached) should be(true)
      rA1NodeCopy.eq(rA1Node) should be(false)
      rA1NodeCopy.eq(rA1.eNode) should not be (true)
      val children = rA1.eNode.parent.get.freezeRead(_.children)
      children.exists(_ eq rA1NodeCopy) should be(true)
      children.exists(_ eq rA1Node) should be(false)
      val modifiedAfterCopyWithAttach = graph1.modified
      modifiedOriginal should not be (modifiedAfterCopyWithAttach)

      val rA1NodeCopyNA = rA1.eNode.copy(attach = false)
      children.exists(_ eq rA1NodeCopy) should be(true)
      children.exists(_ eq rA1NodeCopyNA) should be(false)
      modifiedAfterCopyWithAttach should be(graph1.modified)
      rA1NodeCopyNA.safeRead(_.state.attached) should be(false)
      // modification on unattached nodes is not propagated
      rA1NodeCopyNA.rootBox.e.eSet('AAAKey, "ABC")
      modifiedAfterCopyWithAttach should be(graph1.modified)
      rA1NodeCopyNA.safeRead(_.state.attached) should be(false)
      val size = graph1.nodes.size
      rA1NodeCopyNA.attach.get.eq(rA1NodeCopy) should be(true)
      graph1.nodes.size should be(size)
      rA1NodeCopyNA.safeRead(_.state.attached) should be(true)
      modifiedAfterCopyWithAttach should be < (graph1.modified)
    }
  }
}
