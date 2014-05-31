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

import com.escalatesoft.subcut.inject.NewBindingModule
import java.io.File
import java.util.UUID
import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.{ LoggingHelper, StorageHelper }
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.TestDSL
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.transport.{ Local, Transport }
import org.mockito.{ Matchers ⇒ MM, Mockito }
import org.scalatest.{ FunSpec, Matchers }

class SerializationSpec extends FunSpec with Matchers with StorageHelper with LoggingHelper with Loggable {
  lazy val testTransport = Mockito.spy(new Local)

  before {
    DependencyInjection(new NewBindingModule(module ⇒ {
      module.bind[Transport] identifiedBy ("Serialization.Transport.Local") toSingle { testTransport }
    }) ~ org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
  }

  describe("A Serialization") {
    it("should load the latest revision of graph from a multilocation source") {
      withTempFolder { folder ⇒
        import TestDSL._

        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val folderPrivate = new File(folder, "private")
        val folderShared = new File(folder, "shared")
        val aTS = graph.modified

        info("save graph to the shared location")
        graph.storages should be('empty)
        Serialization.freeze(graph, folderShared.toURI)
        graph.storages should have size (1)
        Serialization.acquire(folderShared.toURI).storages should have size (1)
        info("save graph to the private location")
        Serialization.freeze(graph, SData(SData.Key.explicitStorages ->
          Serialization.ExplicitStorages(Seq(folderPrivate.toURI), Serialization.ExplicitStorages.ModeIgnore)))
        graph.storages should have size (1)
        Serialization.acquire(folderShared.toURI).storages should have size (1)
        info("update graph within the shared location")
        graph.model.eSet('AAAKey, "AAA")
        val bTS = graph.modified
        Serialization.freeze(graph)
        Serialization.acquire(folderShared.toURI).storages should have size (1)
        Serialization.acquire(folderShared.toURI).modified should be(bTS)

        bTS should be > (aTS)

        info("load the latest graph from the private location")
        val graphLatest = Serialization.acquire(folderPrivate.toURI)
        graphLatest.storages should have size (1)
        graphLatest.storages.toSet should be(Set(folderShared.toURI))
        graphLatest.modified should be(bTS)
      }
    }
  }

  override def beforeAll(configMap: org.scalatest.ConfigMap) {
    adjustLoggingBeforeAll(configMap)
    //addFileAppender()
  }
}
