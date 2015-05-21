/**
 * TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2015 Alexey Aksenov ezh@ezh.msk.ru
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

package org.digimead.tabuddy.model.predef

import java.util.UUID
import java.util.concurrent.atomic.AtomicReference
import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.XLoggable
import org.digimead.lib.test.LoggingHelper
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.TestDSL._
import org.digimead.tabuddy.model.element.{ Coordinate, Element, Stash }
import org.digimead.tabuddy.model.graph.ElementBox
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.StubSerialization
import org.scalatest.{ FunSpec, Matchers }

class TaskSpec extends FunSpec with Matchers with LoggingHelper with XLoggable {
  before { DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false) }

  describe("A Task") {
    it("should support nested elements with custom scope") {
      val graph = Graph[Model]('john1, Model.scope, StubSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val task_common_0 = graph.model.task('a_c)
      val task_common_1 = graph.model.withTask('b_c) { r ⇒ r }
      val task_common_2 = graph.model.takeTask('c_c) { r ⇒ }

      task_common_0.eStash.scope.modificator.name should be("Task")
      task_common_0.eNode.id.name should be("a_c")
      task_common_1.eStash.scope.modificator.name should be("Task")
      task_common_1.eNode.id.name should be("b_c")
      task_common_2.eStash.scope.modificator.name should be("Task")
      task_common_2.eNode.id.name should be("c_c")

      val scope = new CustomScope

      val task_custom_0 = graph.model.task('x_c, scope)
      val task_custom_1 = graph.model.withTask('y_c, scope) { r ⇒ r }
      val task_custom_2 = graph.model.takeTask('z_c, scope) { r ⇒ }

      task_custom_0.eStash.scope.modificator.name should be("CustomTask")
      task_custom_0.eNode.id.name should be("x_c")
      task_custom_1.eStash.scope.modificator.name should be("CustomTask")
      task_custom_1.eNode.id.name should be("y_c")
      task_custom_2.eStash.scope.modificator.name should be("CustomTask")
      task_custom_2.eNode.id.name should be("z_c")
    }
  }

  class CustomScope extends Task.Scope('CustomTask)

  override def beforeAll(configMap: org.scalatest.ConfigMap) { adjustLoggingBeforeAll(configMap) }
}
