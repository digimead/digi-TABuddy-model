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

package org.digimead.tabuddy.model.serialization

import java.io.File
import java.util.UUID
import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.{ LoggingHelper, StorageHelper }
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.TestDSL
import org.digimead.tabuddy.model.graph.Graph
import org.scalatest.{ FunSpec, Matchers }

class ExplicitStoragesSpec extends FunSpec with Matchers with StorageHelper with LoggingHelper with Loggable {
  before { DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false) }

  describe("An ExplicitStorages") {
    it("should provide ExplicitStorages.Append mode") {
      withTempFolder { folder ⇒
        import TestDSL._

        // graph
        /*val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        // serialize
        new File(folderA, "john1") should not be ('exists)
        new File(folderB, "john1") should not be ('exists)
        Serialization.freeze(graph, folderA.getAbsoluteFile().toURI())
        graph.retrospective.storages should have size (1)
        Serialization.freeze(graph, SData(SData.Key.explicitStorages ->
          Serialization.ExplicitStorages(Seq(folderB.getAbsoluteFile().toURI()), Serialization.ExplicitStorages.ModeAppend)))
        graph.retrospective.storages should have size (2)
    //    graph.storages should have size (1)
        new File(folderA, "john1") should not be ('exists)
        new File(folderB, "john1") should be('exists)

        val graph2 = Serialization.acquire(graph.origin, folderB.toURI)
        graph2.storages should not be (graph.storages)
      //  graph2.stored should be(graph.stored)
        graph2.storages should contain only (Serialization.normalizeURI(folderA.getAbsoluteFile().toURI()), Serialization.normalizeURI(folderB.getAbsoluteFile().toURI()))*/
      }
    }
    it("should provide ExplicitStorages.Ignore mode") {
      withTempFolder { folder ⇒
        import TestDSL._

        // graph
        /*val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        // serialize
        new File(folderA, "john1") should not be ('exists)
        new File(folderB, "john1") should not be ('exists)
        graph.storages = graph.storages :+ folderA.getAbsoluteFile().toURI()
        graph.storages should have size (1)
        graph.stored should be('empty)
        Serialization.freeze(graph,
          storages = Some(Serialization.ExplicitStorages(Seq(folderB.getAbsoluteFile().toURI()), Serialization.ExplicitStorages.ModeIgnore)))
        graph.storages should have size (1)
        new File(folderA, "john1") should not be ('exists)
        new File(folderB, "john1") should be('exists)

        val graph2 = Serialization.acquire(graph.origin, folderB.toURI)
        graph2.storages should not be (graph.storages)
        graph2.stored should be(graph.stored)
        // 1st storage from descriptor + 2nd storage form current location
        graph2.storages should contain only (Serialization.normalizeURI(folderA.getAbsoluteFile().toURI()), Serialization.normalizeURI(folderB.getAbsoluteFile().toURI()))*/
      }
    }
    it("should provide ExplicitStorages.Replace mode") {
      withTempFolder { folder ⇒
        import TestDSL._

        // graph
        /*val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        // serialize
        new File(folderA, "john1") should not be ('exists)
        new File(folderB, "john1") should not be ('exists)
        graph.storages = graph.storages :+ folderA.getAbsoluteFile().toURI()
        graph.storages should have size (1)
        graph.stored should be('empty)
        Serialization.freeze(graph,
          storages = Some(Serialization.ExplicitStorages(Seq(folderB.getAbsoluteFile().toURI()), Serialization.ExplicitStorages.ModeReplace)))
        graph.storages should have size (1)
        new File(folderA, "john1") should not be ('exists)
        new File(folderB, "john1") should be('exists)

        val graph2 = Serialization.acquire(graph.origin, folderB.toURI)
        graph2.storages should not be (graph.storages)
        graph2.stored should be(graph.stored)
        graph2.storages should contain only (Serialization.normalizeURI(Serialization.normalizeURI(folderB.getAbsoluteFile().toURI())))*/
      }
    }
  }

  override def beforeAll(configMap: org.scalatest.ConfigMap) { adjustLoggingBeforeAll(configMap) }
}
