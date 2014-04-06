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
import java.io.FileInputStream
import java.io.OutputStream
import java.io.{ File, FileNotFoundException, InputStream }
import java.net.URI
import java.security.DigestInputStream
import java.security.DigestOutputStream
import java.security.MessageDigest
import java.security.SecureRandom
import java.util.Formatter
import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger
import javax.crypto.Cipher
import javax.crypto.CipherInputStream
import javax.crypto.CipherOutputStream
import javax.crypto.KeyGenerator
import javax.crypto.spec.IvParameterSpec
import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.{ LoggingHelper, StorageHelper }
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.TestDSL
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.Node
import org.digimead.tabuddy.model.graph.{ ElementBox, Graph, Node }
import org.digimead.tabuddy.model.serialization.transport.{ Local, Transport }
import org.hamcrest.{ BaseMatcher, Description }
import org.mockito.{ Matchers ⇒ MM, Mockito }
import org.scalatest.{ FreeSpec, Matchers }
import org.yaml.snakeyaml.reader.ReaderException
import scala.collection.immutable
import scala.collection.mutable
import sun.misc.{ BASE64Decoder, BASE64Encoder }

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
          case sData: SData ⇒ sData.size == 6 // modified, test, storages, transform, storages, freeze hook
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
      loader.sData.size should be(3) // key test, key storage, key transform
      val graph2 = loader.load()
      graph.node.safeRead { node ⇒
        graph2.node.safeRead { node2 ⇒
          node2.iteratorRecursive.toVector should be(node.iteratorRecursive.toVector)
        }
      }
      Mockito.verify(testTransport, Mockito.never).write(MM.anyObject(), MM.anyObject[Array[Byte]](), MM.anyObject())
      Mockito.verify(testTransport, Mockito.times(18)).read(MM.anyObject(), MM.argThat(new BaseMatcher {
        def matches(state: Any): Boolean = state match {
          case sData: SData ⇒ sData.isDefinedAt(SData.key[String]("test"))
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
        sData.isDefinedAt(SData.Key.afterFreeze) should be(true)
      }
      onFreezeFlag should be(false)
      Serialization.freeze(graph, SData(SData.Key.afterFreeze -> onFreeze), folder.getAbsoluteFile().toURI())
      onFreezeFlag should be(true)

      @volatile var onAcquireFlag = false
      @volatile var onAcquireGraph: Graph[_ <: Model.Like] = null
      val onAcquire = (xGraph: Graph[_ <: Model.Like], transport: Transport, sData: SData) ⇒ {
        onAcquireFlag = true
        onAcquireGraph = xGraph
        transport shouldBe a[Local]
        sData.isDefinedAt(SData.Key.afterAcquire) should be(true)
      }
      onAcquireFlag should be(false)
      Serialization.acquire(folder.getAbsoluteFile().toURI(), SData(SData.Key.afterAcquire -> onAcquire))
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
      Serialization.freeze(graph, SData(SData.Key.afterWrite ->
        { (_: URI, _: Array[Byte], _: Transport, _: SData) ⇒ onWriteCounter.incrementAndGet() }), folder.getAbsoluteFile().toURI())
      // Write:
      // graph descriptor
      // node descriptor
      // element + element descriptor
      // retrospective record
      // retrospective resources
      onWriteCounter.get should be(6)

      Serialization.acquire(folder.getAbsoluteFile().toURI(), SData(SData.Key.afterRead ->
        { (_: URI, _: Array[Byte], _: Transport, _: SData) ⇒ onReadCounter.incrementAndGet() }))
      // Read:
      // graph descriptor
      // retrospective record // acquireGraph
      // retrospective resources // acquireGraph
      // node descriptor
      // retrospective record // getSources
      // retrospective resources // getSources
      // element descriptor
      onReadCounter.get should be(8)
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
        SData.Key.afterRead -> ((uri: URI, _: Array[Byte], transport: Transport, sData: SData) ⇒ sData(SData.key[mutable.ArrayBuffer[URI]]("read")) += uri),
        SData.Key.afterWrite -> ((uri: URI, _: Array[Byte], transport: Transport, sData: SData) ⇒ sData(SData.key[mutable.ArrayBuffer[URI]]("write")) += uri),
        SData.Key.afterFreeze -> ((xGraph: Graph[_ <: Model.Like], transport: Transport, sData: SData) ⇒
          sData(SData.key[AtomicInteger]("totalWrite")).set(sData(SData.key[mutable.ArrayBuffer[URI]]("write")).size)),
        SData.Key.afterAcquire -> ((xGraph: Graph[_ <: Model.Like], transport: Transport, sData: SData) ⇒
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
      sData(SData.key[mutable.ArrayBuffer[URI]]("read")).size should be(8)
      sData(SData.key[AtomicInteger]("totalRead")).get should be(8)
      sData(SData.key[mutable.ArrayBuffer[URI]]("write")).size should be(6)
      sData(SData.key[AtomicInteger]("totalWrite")).get() should be(6)
    }
  }
  "Serialization data should support 'encodeURI' option" in {
    withTempFolder { folder ⇒
      import TestDSL._

      val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      val sData = SData(SData.Key.convertURI -> (((name: String, sData: SData) ⇒ "Test_" + name,
        (name: String, sData: SData) ⇒ name.replaceAll("""^Test_""", ""))),
        SData.key[mutable.ArrayBuffer[URI]]("read") -> new mutable.ArrayBuffer[URI] with mutable.SynchronizedBuffer[URI],
        SData.key[mutable.ArrayBuffer[URI]]("write") -> new mutable.ArrayBuffer[URI] with mutable.SynchronizedBuffer[URI],
        SData.Key.afterRead -> ((uri: URI, _: Array[Byte], transport: Transport, sData: SData) ⇒ sData(SData.key[mutable.ArrayBuffer[URI]]("read")) += uri),
        SData.Key.afterWrite -> ((uri: URI, _: Array[Byte], transport: Transport, sData: SData) ⇒ sData(SData.key[mutable.ArrayBuffer[URI]]("write")) += uri))

      val baseURI = new URI("aaa:/bbb/")
      val testURI = new URI("aaa:/bbb/ccc/ddd.eee")
      val encoded = Serialization.inner.convert(baseURI, testURI, (name: String, sData: SData) ⇒ "Test_" + name, sData)
      encoded should be(new URI("aaa:/bbb/Test_ccc/Test_ddd.eee"))
      val decoded = Serialization.inner.convert(baseURI, encoded, (name: String, sData: SData) ⇒ name.replaceAll("""^Test_""", ""), sData)
      decoded should be(testURI)

      Serialization.freeze(graph, sData, folder.getAbsoluteFile().toURI())
      val pathsWrite = sData(SData.key[mutable.ArrayBuffer[URI]]("write")).map(folder.getAbsoluteFile().toURI().relativize(_).getPath())
      pathsWrite.forall(_.startsWith("Test_")) should be(true)

      a[FileNotFoundException] should be thrownBy Serialization.acquire(folder.getAbsoluteFile().toURI())
      val graph2 = Serialization.acquire(folder.getAbsoluteFile().toURI(), sData)
      val pathsRead = sData(SData.key[mutable.ArrayBuffer[URI]]("write")).map(folder.getAbsoluteFile().toURI().relativize(_).getPath())
      pathsRead.forall(_.startsWith("Test_")) should be(true)

      graph.retrospective should be(graph2.retrospective)
      graph2.node.safeRead { node ⇒
        graph.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)

      info("simple XOR encoding")
      graph.retrospective = Graph.Retrospective.empty(graph.origin)
      val sDataXOR = SData(SData.Key.convertURI -> ((name: String, sData: SData) ⇒ StringXORer.encode(name, "secret"),
        (name: String, sData: SData) ⇒ StringXORer.decode(name, "secret")),
        SData.key[mutable.ArrayBuffer[URI]]("read") -> new mutable.ArrayBuffer[URI] with mutable.SynchronizedBuffer[URI],
        SData.key[mutable.ArrayBuffer[URI]]("write") -> new mutable.ArrayBuffer[URI] with mutable.SynchronizedBuffer[URI],
        SData.Key.afterRead -> ((uri: URI, _: Array[Byte], transport: Transport, sData: SData) ⇒ sData(SData.key[mutable.ArrayBuffer[URI]]("read")) += uri),
        SData.Key.afterWrite -> ((uri: URI, _: Array[Byte], transport: Transport, sData: SData) ⇒ sData(SData.key[mutable.ArrayBuffer[URI]]("write")) += uri))
      Serialization.freeze(graph, sDataXOR, folder.getAbsoluteFile().toURI())
      val pathsWriteXOR = sDataXOR(SData.key[mutable.ArrayBuffer[URI]]("write")).map(folder.getAbsoluteFile().toURI().relativize(_).getPath())
      pathsWriteXOR.map(_.split("/").head) should be(Seq("FwAQERcdAxEMAEsNEggP", "FwQXEw==", "FwQXEw==", "FwQXEw==", "AQAXAAoHAwAABgwCFg==", "AQAXAAoHAwAABgwCFg=="))

      val graph3 = Serialization.acquire(folder.getAbsoluteFile().toURI(), sDataXOR)
      graph.retrospective should be(graph3.retrospective)
      graph3.node.safeRead { node ⇒
        graph.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)
    }
  }
  "Serialization data should support 'encodeFilter' and 'decodeFilter' options" in {
    withTempFolder { folder ⇒
      import TestDSL._

      info("simple MD5 + DES")
      // MD5 digest + DES
      val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      val kg = KeyGenerator.getInstance("DES")
      kg.init(new SecureRandom())
      val key = kg.generateKey()

      val sDataWrite = SData(SData.Key.encodeFilter -> ((os: OutputStream, uri: URI, transport: Transport, sData: SData) ⇒ {
        val c = Cipher.getInstance("DES/CFB8/NoPadding")
        c.init(Cipher.ENCRYPT_MODE, key)
        val md5 = MessageDigest.getInstance("MD5")
        val stream = new CipherOutputStream(new DigestOutputStream(os, md5), c)
        sData(SData.key[ThreadLocal[(Array[Byte], MessageDigest)]]("digest")).set(c.getIV(), md5)
        stream
      }),
        SData.key[ThreadLocal[(Array[Byte], MessageDigest)]]("digest") -> new ThreadLocal[(Array[Byte], MessageDigest)](), // thread local MessageDigest algorithm
        SData.key[mutable.Map[URI, (Array[Byte], String)]]("hash") -> new mutable.HashMap[URI, (Array[Byte], String)] with mutable.SynchronizedMap[URI, (Array[Byte], String)],
        SData.Key.afterWrite -> ((uri: URI, _: Array[Byte], transport: Transport, sData: SData) ⇒ {
          val threadLocal = sData(SData.key[ThreadLocal[(Array[Byte], MessageDigest)]]("digest"))
          Option(threadLocal.get).foreach {
            case (iv, digest) ⇒
              val formatter = new Formatter()
              digest.digest().foreach(b ⇒ formatter.format("%02x", b: java.lang.Byte))
              sData(SData.key[mutable.Map[URI, (Array[Byte], String)]]("hash")) += uri -> (iv, formatter.toString())
              digest.reset()
          }
          threadLocal.set(null)
        }))
      Serialization.freeze(graph, sDataWrite, folder.getAbsoluteFile().toURI())
      val hashes = sDataWrite(SData.key[mutable.Map[URI, (Array[Byte], String)]]("hash"))
      hashes.size should be(6)
      hashes.foreach {
        case (uri, (iv, hash)) ⇒
          val md5 = MessageDigest.getInstance("MD5")
          val fis = new FileInputStream(new File(uri))
          val dis = new DigestInputStream(fis, md5)
          while (dis.read() != -1) {}
          val formatter = new Formatter()
          md5.digest().foreach(b ⇒ formatter.format("%02x", b: java.lang.Byte))
          formatter.toString() should be(hash)
      }

      a[ReaderException] should be thrownBy Serialization.acquire(folder.getAbsoluteFile().toURI())

      val sDataRead = SData(SData.Key.decodeFilter -> ((is: InputStream, uri: URI, transport: Transport, sData: SData) ⇒ {
        val (iv, hash) = sData(SData.key[mutable.Map[URI, (Array[Byte], String)]]("hash"))(uri)
        val c = Cipher.getInstance("DES/CFB8/NoPadding")
        c.init(Cipher.DECRYPT_MODE, key, new IvParameterSpec(iv))
        val md5 = MessageDigest.getInstance("MD5")
        val stream = new CipherInputStream(new DigestInputStream(is, md5), c)
        sData(SData.key[ThreadLocal[MessageDigest]]("digest")).set(md5)
        stream
      }),
        SData.key[ThreadLocal[MessageDigest]]("digest") -> new ThreadLocal[MessageDigest](), // thread local MessageDigest algorithm
        SData.key[mutable.Map[URI, (Array[Byte], String)]]("hash") -> sDataWrite(SData.key[mutable.Map[URI, (Array[Byte], String)]]("hash")),
        SData.Key.afterRead -> ((uri: URI, _: Array[Byte], transport: Transport, sData: SData) ⇒ {
          Option(sData(SData.key[ThreadLocal[MessageDigest]]("digest"))).foreach { digest ⇒
            val formatter = new Formatter()
            digest.get().digest().foreach(b ⇒ formatter.format("%02x", b: java.lang.Byte))
            assert(sData(SData.key[mutable.Map[URI, (Array[Byte], String)]]("hash"))(uri)._2 == formatter.toString())
            digest.get().reset()
            digest.set(null)
          }
        }))
      val graph2 = Serialization.acquire(folder.getAbsoluteFile().toURI(), sDataRead)
      graph.retrospective should be(graph2.retrospective)
      graph2.node.safeRead { node ⇒
        graph.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)
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
