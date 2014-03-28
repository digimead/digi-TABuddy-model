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

class SDataSpec extends FreeSpec with Matchers with StorageHelper with LoggingHelper with Loggable {
  lazy val testTransport = Mockito.spy(new Test)

  before {
    DependencyInjection(new NewBindingModule(module ⇒ {
      module.bind[Transport] identifiedBy ("Serialization.Transport.Test") toSingle { testTransport }
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
      val graphURI = new URI(folder.getAbsoluteFile().toURI().toString().replaceAll("^file:", "test:"))
      graph.retrospective.history should be('empty)

      Mockito.reset(testTransport)
      Mockito.verifyZeroInteractions(testTransport)
      val sData = SData(SData.key[String]("test") -> "test")
      val timestamp = Serialization.freeze(graph, sData, graphURI)
      Mockito.verify(testTransport, Mockito.never).write(MM.anyObject(), MM.anyObject[InputStream](), MM.anyObject())
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
      val loader = Serialization.acquireLoader(graph.origin, graphURI, sData)
      loader.sData(SData.key[String]("test")) should be("test")
      loader.sData.isDefinedAt(SData.Key.acquireT) should be(true)
      loader.sData.size should be(2)
      val graph2 = loader.load()
      graph.node.safeRead { node ⇒
        graph2.node.safeRead { node2 ⇒
          node2.iteratorRecursive.toVector should be(node.iteratorRecursive.toVector)
        }
      }
      Mockito.verify(testTransport, Mockito.never).write(MM.anyObject(), MM.anyObject[InputStream](), MM.anyObject())
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
  override def beforeAll(configMap: org.scalatest.ConfigMap) { adjustLoggingBeforeAll(configMap) }

  class Test extends Local {
    override val scheme: String = "test"

    /** Delete resource. */
    override def delete(uri: URI, sData: SData) =
      super.delete(x(uri), x(sData))
    /** Delete resource. */
    override def exists(uri: URI, sData: SData) =
      super.exists(x(uri), x(sData))
    /** Get element box URI. */
    override def getElementBoxURI(ancestors: Seq[Node[_ <: Element]], elementUniqueId: UUID, elementModified: Element.Timestamp, sData: SData): URI =
      super.getElementBoxURI(ancestors, elementUniqueId, elementModified, x(sData))
    /** Get graph URI. */
    override def getGraphURI(origin: Symbol, sData: SData): URI =
      super.getGraphURI(origin, x(sData))
    /** Get node URI. */
    override def getNodeURI(ancestors: Seq[Node[_ <: Element]], nodeId: Symbol, nodeModified: Element.Timestamp, sData: SData): URI =
      super.getNodeURI(ancestors, nodeId, nodeModified, x(sData))
    /** Get sub element URI. */
    override def getSubElementURI(ancestors: Seq[Node[_ <: Element]], elementUniqueId: UUID, elementModified: Element.Timestamp, sData: SData, part: String*): URI =
      super.getSubElementURI(ancestors, elementUniqueId, elementModified, x(sData))
    /** Open stream */
    override def open(uri: URI, sData: SData): InputStream =
      super.open(x(uri), sData: SData)
    /** Read resource. */
    override def read(uri: URI, sData: SData): Array[Byte] =
      super.read(x(uri), sData: SData)
    /** Write resource. */
    override def write(uri: URI, content: InputStream, sData: SData) =
      super.write(x(uri), content, sData: SData)
    /** Write resource. */
    override def write(uri: URI, content: Array[Byte], sData: SData) =
      super.write(x(uri), content, sData: SData)

    protected def x(sData: SData): SData = sData.get(SData.Key.storageURI) match {
      case Some(uri) ⇒ if (uri.getScheme() == scheme)
        sData.updated(SData.Key.storageURI, new URI("file", uri.getUserInfo(), uri.getAuthority(), uri.getPort(), uri.getPath(), uri.getQuery(), uri.getFragment()))
      else
        sData
      case None ⇒ sData
    }
    protected def x(uri: URI): URI =
      if (uri.getScheme() == scheme)
        new URI("file", uri.getUserInfo(), uri.getAuthority(), uri.getPort(), uri.getPath(), uri.getQuery(), uri.getFragment())
      else
        uri
  }
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
