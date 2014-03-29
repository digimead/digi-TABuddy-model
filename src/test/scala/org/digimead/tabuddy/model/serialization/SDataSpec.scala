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
import java.io.InputStream
import java.net.URI
import java.util.UUID
import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.{ LoggingHelper, StorageHelper }
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.TestDSL
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.graph.{ ElementBox, Graph, Node }
import org.digimead.tabuddy.model.serialization.transport.{ Local, Transport }
import org.hamcrest.BaseMatcher
import org.hamcrest.Description
import org.mockito.Mockito
import org.mockito.{ Matchers ⇒ MM, Mockito }
import org.scalatest.{ FreeSpec, Matchers }
import scala.collection.immutable
import sun.misc.{ BASE64Decoder, BASE64Encoder }
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class SDataSpec extends FreeSpec with Matchers with StorageHelper with LoggingHelper with Loggable {
  lazy val testTransport = Mockito.spy(new Local)

  before {
    DependencyInjection(new NewBindingModule(module ⇒ {
      module.bind[Transport] identifiedBy ("Serialization.Transport.Local") toSingle { testTransport }
    }) ~ org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
  }

  "Serialization data implementation should be consistent" in {
    val state = SData(SData.key[Int]("zero") -> 0,
      SData.key[Int]("one") -> 1,
      SData.key[Int]("two") -> 2,
      SData.key[Int]("three") -> 3,
      SData.key[Int]("four") -> 4,
      SData.key[Int]("five") -> 5)
    state.underlying shouldBe a[immutable.HashMap[_, _]]
    state.take(1).underlying shouldBe a[immutable.Map.Map1[_, _]]
    state.take(4).underlying shouldBe a[immutable.Map.Map4[_, _]]
    state.take(10).underlying shouldBe a[immutable.HashMap[_, _]]

    val state2 = SData(SData.key[Int]("zero") -> 10)
    val state12 = state ++ state2
    state12.size should be(6)
    state12(SData.key[Int]("zero")) should be(10)
  }

  "Serialization data should passed across objects hierarchy" in {
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

      // serialize
      new File(folder, "john1") should not be ('exists)
      val graphURI = folder.getAbsoluteFile().toURI()
      graph.retrospective.history should be('empty)

      Mockito.reset(testTransport)
      Mockito.verifyZeroInteractions(testTransport)
      val sData = SData(SData.key[String]("test") -> "test")
      val timestamp = Serialization.freeze(graph, sData, graphURI)
      Mockito.verify(testTransport, Mockito.times(21)).write(MM.anyObject(), MM.anyObject[Array[Byte]](), MM.argThat(new BaseMatcher {
        def matches(state: Any): Boolean = state match {
          case sData: SData ⇒ sData.size == 4 // key test, key storages, key transform, key storage
          case _ ⇒ false
        }
        def describeTo(description: Description) {}
      }))
      Mockito.verify(testTransport, Mockito.never).read(MM.anyObject(), MM.anyObject())
      Mockito.reset(testTransport)
      Mockito.verifyZeroInteractions(testTransport)
      // deserialize
      val loader = Serialization.acquireLoader(graphURI, sData)
      loader.sData(SData.key[String]("test")) should be("test")
      loader.sData.isDefinedAt(SData.Key.acquireT) should be(true)
      loader.sData.size should be(2)
      val graph2 = loader.load()
      graph.node.safeRead { node ⇒
        graph2.node.safeRead { node2 ⇒
          node2.iteratorRecursive.toVector should be(node.iteratorRecursive.toVector)
        }
      }
      Mockito.verify(testTransport, Mockito.never).write(MM.anyObject(), MM.anyObject[Array[Byte]](), MM.anyObject())
      Mockito.verify(testTransport, Mockito.times(17)).read(MM.anyObject(), MM.argThat(new BaseMatcher {
        def matches(state: Any): Boolean = state match {
          case sData: SData ⇒ sData.size == 3 // key test, key transform, key storage
          case _ ⇒ false
        }
        def describeTo(description: Description) {}
      }))
    }
  }
  "Serialization data should support 'force' option" in {
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
      Serialization.freeze(graph)
      Mockito.verify(testTransport, Mockito.times(1)).write(MM.anyObject(), MM.anyObject[Array[Byte]](), MM.anyObject())
      graph.retrospective.storages should have size (1)

      Mockito.reset(testTransport)
      Mockito.verifyZeroInteractions(testTransport)
      // Write:
      // graph descriptor
      Serialization.freeze(graph, SData(SData.Key.force -> true))
      // 6 times!
      Mockito.verify(testTransport, Mockito.times(6)).write(MM.anyObject(), MM.anyObject[Array[Byte]](), MM.anyObject())
      graph.retrospective.storages should have size (1)
    }
  }
  "Serialization data should support 'onAcquire' and 'onFreeze' options" in {
    withTempFolder { folder ⇒
      import TestDSL._

      val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative

      @volatile var onFreezeFlag = false
      val onFreeze = (xGraph: Graph[_ <: Model.Like], transport: Transport, sData: SData) ⇒ {
        onFreezeFlag = true
        xGraph should be(graph)
        transport shouldBe a[Local]
        sData.isDefinedAt(SData.Key.onFreeze) should be(true)
      }
      onFreezeFlag should be(false)
      Serialization.freeze(graph, SData(SData.Key.onFreeze -> onFreeze), folder.getAbsoluteFile().toURI())
      onFreezeFlag should be(true)

      @volatile var onAcquireFlag = false
      @volatile var onAcquireGraph: Graph[_ <: Model.Like] = null
      val onAcquire = (xGraph: Graph[_ <: Model.Like], transport: Transport, sData: SData) ⇒ {
        onAcquireFlag = true
        onAcquireGraph = xGraph
        transport shouldBe a[Local]
        sData.isDefinedAt(SData.Key.onAcquire) should be(true)
      }
      onAcquireFlag should be(false)
      Serialization.acquire(folder.getAbsoluteFile().toURI(), SData(SData.Key.onAcquire -> onAcquire))
      onAcquireFlag should be(true)
      onAcquireGraph should be(graph)
    }
  }
  "Serialization data should support 'onRead' and 'onWrite' options" in {
    withTempFolder { folder ⇒
      import TestDSL._

      val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative

      val onReadCounter = new AtomicInteger
      val onWriteCounter = new AtomicInteger
      Serialization.freeze(graph, SData(SData.Key.onWrite ->
        { (_: URI, _: Array[Byte], _: SData) ⇒ onWriteCounter.incrementAndGet() }), folder.getAbsoluteFile().toURI())
      // Write:
      // graph descriptor
      // node descriptor
      // element + element descriptor
      // retrospective record
      // retrospective resources
      onWriteCounter.get should be(6)

      Serialization.acquire(folder.getAbsoluteFile().toURI(), SData(SData.Key.onRead ->
        { (_: URI, _: Array[Byte], _: SData) ⇒ onReadCounter.incrementAndGet() }))
      // Read:
      // graph descriptor
      // retrospective record
      // retrospective resources
      // node descriptor
      // retrospective record
      // retrospective resources
      // element descriptor
      onReadCounter.get should be(7)
    }
  }
  "Complex 'onRead', 'onWrite', 'onAcquire' and 'onFreeze' options test" in {
    withTempFolder { folder ⇒
      import TestDSL._

      val onReadCounter = new AtomicInteger
      val onWriteCounter = new AtomicInteger
      val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      val sData = SData(SData.key[AtomicInteger]("totalRead") -> new AtomicInteger,
        SData.key[AtomicInteger]("totalWrite") -> new AtomicInteger,
        SData.key[mutable.ArrayBuffer[URI]]("read") -> new mutable.ArrayBuffer[URI] with mutable.SynchronizedBuffer[URI],
        SData.key[mutable.ArrayBuffer[URI]]("write") -> new mutable.ArrayBuffer[URI] with mutable.SynchronizedBuffer[URI],
        SData.Key.onRead -> ((uri: URI, _: Array[Byte], sData: SData) ⇒ sData(SData.key[mutable.ArrayBuffer[URI]]("read")) += uri),
        SData.Key.onWrite -> ((uri: URI, _: Array[Byte], sData: SData) ⇒ sData(SData.key[mutable.ArrayBuffer[URI]]("write")) += uri),
        SData.Key.onFreeze -> ((xGraph: Graph[_ <: Model.Like], transport: Transport, sData: SData) ⇒
          sData(SData.key[AtomicInteger]("totalWrite")).set(sData(SData.key[mutable.ArrayBuffer[URI]]("write")).size)),
        SData.Key.onAcquire -> ((xGraph: Graph[_ <: Model.Like], transport: Transport, sData: SData) ⇒
          sData(SData.key[AtomicInteger]("totalRead")).set(sData(SData.key[mutable.ArrayBuffer[URI]]("read")).size)))

      sData(SData.key[mutable.ArrayBuffer[URI]]("read")).size should be(0)
      sData(SData.key[AtomicInteger]("totalRead")).get should be(0)
      sData(SData.key[mutable.ArrayBuffer[URI]]("write")).size should be(0)
      sData(SData.key[AtomicInteger]("totalWrite")).get() should be(0)

      Serialization.freeze(graph, sData, folder.getAbsoluteFile().toURI())
      sData(SData.key[mutable.ArrayBuffer[URI]]("read")).size should be(0)
      sData(SData.key[AtomicInteger]("totalRead")).get should be(0)
      sData(SData.key[mutable.ArrayBuffer[URI]]("write")).size should be(6)
      sData(SData.key[AtomicInteger]("totalWrite")).get() should be(6)

      Serialization.acquire(folder.getAbsoluteFile().toURI(), sData)
      sData(SData.key[mutable.ArrayBuffer[URI]]("read")).size should be(7)
      sData(SData.key[AtomicInteger]("totalRead")).get should be(7)
      sData(SData.key[mutable.ArrayBuffer[URI]]("write")).size should be(6)
      sData(SData.key[AtomicInteger]("totalWrite")).get() should be(6)
    }
  }

  override def beforeAll(configMap: org.scalatest.ConfigMap) { adjustLoggingBeforeAll(configMap) }

  object StringXORer {
    def encode(s: String, key: String) = base64Encode(xorWithKey(s.getBytes(), key.getBytes()))
    def decode(s: String, key: String) = new String(xorWithKey(base64Decode(s), key.getBytes()))
    def xorWithKey(a: Array[Byte], key: Array[Byte]): Array[Byte] = {
      val out = new Array[Byte](a.length)
      for (i ← 0 until a.length)
        out(i) = (a(i) ^ key(i % key.length)).toByte
      out
    }
    def base64Decode(s: String): Array[Byte] = new BASE64Decoder().decodeBuffer(s)
    def base64Encode(bytes: Array[Byte]) = new BASE64Encoder().encode(bytes).replaceAll("\\s", "");
  }
}
