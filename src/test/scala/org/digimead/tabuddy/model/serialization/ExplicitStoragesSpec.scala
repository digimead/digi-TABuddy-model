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

class ExplicitStoragesSpec extends FunSpec with Matchers with StorageHelper with LoggingHelper with Loggable {
  lazy val testTransport = Mockito.spy(new Local)

  before {
    DependencyInjection(new NewBindingModule(module ⇒ {
      module.bind[Transport] identifiedBy ("Serialization.Transport.Local") toSingle { testTransport }
    }) ~ org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
  }

  describe("An ExplicitStorages") {
    /** Save graph to explicit and original storages. Save merged values with serialized data. */
    it("should support an ExplicitStorages.Append") {
      withTempFolder { folder ⇒
        import TestDSL._

        // graph
        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        // serialize
        new File(folderA, "descriptor.yaml") should not be ('exists)
        new File(folderB, "descriptor.yaml") should not be ('exists)
        // Unable to freeze graph without any defined storages.
        an[IllegalStateException] should be thrownBy Serialization.freeze(graph)

        // Use ExplicitStorages.Append via URI variable length argument
        Mockito.reset(testTransport)
        Mockito.verifyZeroInteractions(testTransport)
        // Write:
        // graph descriptor
        // node descriptor
        // element + element descriptor
        // retrospective record
        // retrospective resources
        Serialization.freeze(graph, folderA.getAbsoluteFile().toURI())
        Mockito.verify(testTransport, Mockito.times(6)).write(MM.anyObject(), MM.anyObject[Array[Byte]](), MM.anyObject())

        Mockito.reset(testTransport)
        Mockito.verifyZeroInteractions(testTransport)
        // Write:
        // graph descriptor
        Serialization.freeze(graph, folderA.getAbsoluteFile().toURI())
        Mockito.verify(testTransport, Mockito.times(1)).write(MM.anyObject(), MM.anyObject[Array[Byte]](), MM.anyObject())
        graph.retrospective.storages should have size (1)

        info("Add new storage to stored graph.")
        // Use ExplicitStorages.Append via URI variable length argument
        Mockito.reset(testTransport)
        Mockito.verifyZeroInteractions(testTransport)
        Serialization.freeze(graph, SData(SData.Key.explicitStorages ->
          Serialization.ExplicitStorages(Seq(folderB.getAbsoluteFile().toURI()), Serialization.ExplicitStorages.ModeAppend)))
        Mockito.verify(testTransport, Mockito.times(7)).write(MM.anyObject(), MM.anyObject[Array[Byte]](), MM.anyObject())
        graph.retrospective.storages should have size (2)
        new File(folderA, "descriptor.yaml") should be('exists)
        new File(folderB, "descriptor.yaml") should be('exists)

        val graph2 = Serialization.acquire(folderB.toURI)
        graph2.storages.toSet should be(graph.storages.toSet)
        graph2.retrospective should be(graph.retrospective)
        graph2.storages should contain only (Serialization.normalizeURI(folderA.getAbsoluteFile().toURI()), Serialization.normalizeURI(folderB.getAbsoluteFile().toURI()))
      }
    }
    /** Save graph to explicit storages. Save original values with serialized data. */
    it("should support an ExplicitStorages.Ignore") {
      withTempFolder { folder ⇒
        import TestDSL._

        // graph
        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        val folderC = new File(folder, "C")
        // serialize
        new File(folderA, "descriptor.yaml") should not be ('exists)
        new File(folderB, "descriptor.yaml") should not be ('exists)
        new File(folderC, "descriptor.yaml") should not be ('exists)

        Serialization.freeze(graph, SData(SData.Key.explicitStorages ->
          Serialization.ExplicitStorages(Seq(folderB.getAbsoluteFile().toURI()), Serialization.ExplicitStorages.ModeIgnore)))
        graph.storages should have size (0)
        new File(folderA, "descriptor.yaml") should not be ('exists)
        new File(folderB, "descriptor.yaml") should be('exists)
        new File(folderC, "descriptor.yaml") should not be ('exists)

        // Graph without storages but with one retrospective record.
        val graph2 = Serialization.acquire(folderB.toURI)
        graph2.storages.toSet should be(graph.storages.toSet)
        graph2.storages should be(empty)
        graph2.retrospective should be(graph.retrospective)
        graph2.retrospective.getOrigin(graph2.modified).name should be(graph2.origin.name)
        graph2.retrospective.getStorages(graph2.modified) should be(empty)
        graph2.retrospective.history.size should be(1)

        Serialization.freeze(graph2, folderA.toURI())
        graph2.storages should contain only (folderA.toURI())
        new File(folderA, "descriptor.yaml") should be('exists)
        new File(folderB, "descriptor.yaml") should be('exists)
        new File(folderC, "descriptor.yaml") should not be ('exists)

        val graph3 = Serialization.acquire(folderA.toURI)
        graph3.retrospective should be(graph2.retrospective)

        Serialization.freeze(graph2, SData(SData.Key.explicitStorages ->
          Serialization.ExplicitStorages(Seq(folderC.getAbsoluteFile().toURI()), Serialization.ExplicitStorages.ModeIgnore)))
        graph2.storages.toSet should contain only (folderA.getAbsoluteFile().toURI())
        new File(folderA, "descriptor.yaml") should be('exists)
        new File(folderB, "descriptor.yaml") should be('exists)
        new File(folderC, "descriptor.yaml") should be('exists)

        val graph4 = Serialization.acquire(folderC.toURI)
        graph4.retrospective should be(graph2.retrospective)
      }
    }
    /** Save graph to original storages. Save explicit values with serialized data. */
    it("should provide ExplicitStorages.Replace mode") {
      withTempFolder { folder ⇒
        import TestDSL._

        // graph
        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        val folderC = new File(folder, "C")
        // serialize
        new File(folderA, "descriptor.yaml") should not be ('exists)
        new File(folderB, "descriptor.yaml") should not be ('exists)
        new File(folderC, "descriptor.yaml") should not be ('exists)
        Serialization.freeze(graph, folderA.toURI())
        graph.retrospective.history should have size (1)
        new File(folderA, "descriptor.yaml") should be('exists)
        new File(folderB, "descriptor.yaml") should not be ('exists)
        new File(folderC, "descriptor.yaml") should not be ('exists)
        graph.storages.toSet should contain only (folderA.getAbsoluteFile().toURI())

        Serialization.freeze(graph, SData(SData.Key.explicitStorages -> Serialization.ExplicitStorages(Seq(folderB.getAbsoluteFile().toURI(),
          folderC.getAbsoluteFile().toURI()), Serialization.ExplicitStorages.ModeReplace)))
        new File(folderA, "descriptor.yaml") should be('exists)
        new File(folderB, "descriptor.yaml") should not be ('exists)
        graph.retrospective.history should have size (1)
        graph.storages.size should be(2)
        val want = Seq(folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI()).map(_.toString().replaceAll("""/$""", "")).toSet
        graph.storages.map(_.toString().replaceAll("""/$""", "")).toSet should be(want)
      }
    }
  }

  override def beforeAll(configMap: org.scalatest.ConfigMap) { adjustLoggingBeforeAll(configMap) }
}
