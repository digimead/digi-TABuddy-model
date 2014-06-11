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
import java.io.{ File, FilterInputStream, FilterOutputStream, InputStream, OutputStream }
import java.net.URI
import java.security.{ KeyPairGenerator, PublicKey }
import java.util.UUID
import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.{ LoggingHelper, StorageHelper }
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.TestDSL
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.digest.{ Digest, SimpleDigest }
import org.digimead.tabuddy.model.serialization.signature.{ Signature, SimpleSignature }
import org.digimead.tabuddy.model.serialization.transport.{ Local, Transport }
import org.mockito.Mockito
import org.scalatest.{ FunSpec, Matchers }
import scala.io.Codec.charset2codec

class SerializationSpec extends FunSpec with Matchers with StorageHelper with LoggingHelper with Loggable {
  lazy val testTransport = Mockito.spy(new Local)

  before {
    DependencyInjection(new NewBindingModule(module ⇒ {
      module.bind[Transport] identifiedBy ("Serialization.Transport.Local") toSingle { testTransport }
    }) ~ org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
  }

  describe("A Serialization") {
    it("should load the latest revision of graph from pack of sources") {
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
          Serialization.Storages(Serialization.Storages.Real(folderPrivate.toURI))))
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
    // X.acquireLoader(..., SData(Digest.Key.acquire -> initiate Digest.historyPerURI initialization
    it("should have proper digests across multiple sources") {
      withTempFolder { folder ⇒
        import TestDSL._

        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        val folderC = new File(folder, "C")
        val folderD = new File(folder, "D")
        val folderE = new File(folder, "E")
        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }

        info("freeze modification 1 of graph to folderA")
        graph.model.eSet('Key, "1")
        Serialization.freeze(graph, folderA.toURI)
        val loader1 = Serialization.acquireLoader(folderA.toURI, SData(Digest.Key.acquire -> false))
        val records1 = loader1.sources.head.graphDescriptor.records.sorted
        val history1 = loader1.sData(Digest.historyPerURI)(folderA.toURI())
        records1.map(r ⇒ history1(r)._1.toString()).toList should be(List("SimpleDigestParameters(SHA-512)"))

        info("freeze modification 2 of graph to folderA")
        graph.model.eSet('Key, "2")
        Serialization.freeze(graph, SData(Digest.Key.freeze ->
          Map(folderA.toURI -> SimpleDigest("MD5"))))
        val loader2 = Serialization.acquireLoader(folderA.toURI, SData(Digest.Key.acquire -> false))
        val records2 = loader2.sources.head.graphDescriptor.records.sorted
        val history2 = loader2.sData(Digest.historyPerURI)(folderA.toURI())
        records2.map(r ⇒ history2(r)._1.toString()).toList should be(List("SimpleDigestParameters(SHA-512)", "SimpleDigestParameters(MD5)"))
        loader2.load()
        val digestA = new File(new File(Digest.digestURI(folderA.toURI, Serialization.perScheme("file"), graph.modified)), Digest.containerName)
        digestA should be('exists)
        val digestASize = digestA.length

        info("freeze last modification of graph to folderA and to folderB")
        Serialization.freeze(graph, SData(SData.Key.explicitStorages -> Serialization.Storages.append(graph, folderB.toURI)))
        val loader2b = Serialization.acquireLoader(folderB.toURI, SData(Digest.Key.acquire -> false))
        val records2b = loader2b.sources.head.graphDescriptor.records.sorted
        val history2b = loader2b.sData(Digest.historyPerURI)(folderB.toURI())
        records2b.map(r ⇒ history2b(r)._1.toString()).toList should be(List("NoDigest", "SimpleDigestParameters(SHA-512)"))
        loader2b.load()
        digestA.length() should be(digestASize)

        info("freeze last modification of graph to folderC")
        Serialization.freeze(graph, SData(SData.Key.force -> true,
          SData.Key.explicitStorages -> Serialization.Storages.append(graph, folderC.toURI)))
        val loader2c = Serialization.acquireLoader(folderC.toURI, SData(Digest.Key.acquire -> false))
        val records2c = loader2c.sources.head.graphDescriptor.records.sorted
        val history2c = loader2c.sData(Digest.historyPerURI)(folderC.toURI())
        records2c.map(r ⇒ history2c(r)._1.toString()).toList should be(List("NoDigest", "SimpleDigestParameters(SHA-512)"))
        loader2c.load()
        digestA.length() should be(digestASize)

        info("freeze last modification of graph to folderD")
        Serialization.freeze(graph, SData(SData.Key.explicitStorages ->
          Serialization.Storages(
            graph.storages.map(Serialization.Storages.Simple) :+
              Serialization.Storages.Complex(folderE.toURI(), folderD.toURI()))))
        val loader2d = Serialization.acquireLoader(folderD.toURI, SData(Digest.Key.acquire -> false))
        val records2d = loader2d.sources.head.graphDescriptor.records.sorted
        val history2d = loader2d.sData(Digest.historyPerURI)(folderD.toURI())
        records2d.map(r ⇒ history2d(r)._1.toString()).toList should be(List("NoDigest", "SimpleDigestParameters(SHA-512)"))
        loader2d.load()

        //val tmp = new File("/tmp/data")
        //deleteFolder(tmp)
        //tmp.mkdirs()
        //copy(folderA, new File(tmp, folderA.getName()))
        //copy(folderB, new File(tmp, folderB.getName()))
        //copy(folderC, new File(tmp, folderC.getName()))
        //copy(folderD, new File(tmp, folderD.getName()))

        val graphX = loader2d.load()
        graphX.storages.map(_.toString()).toSet should be(Set(folderA.toURI().toString(),
          folderB.toURI().toString(), folderC.toURI().toString(), folderE.toURI().toString() + "/"))

        val graphA = Serialization.acquireLoader(folderA.toURI, SData(Digest.Key.acquire -> true)).load()
        val graphB = Serialization.acquireLoader(folderB.toURI, SData(Digest.Key.acquire -> true)).load()
        val graphC = Serialization.acquireLoader(folderC.toURI, SData(Digest.Key.acquire -> true)).load()
      }
    }
    it("should have proper signatures across multiple sources") {
      withTempFolder { folder ⇒
        import TestDSL._

        val keyGenRSA = KeyPairGenerator.getInstance("RSA")
        keyGenRSA.initialize(1024)
        val pairRSA = keyGenRSA.genKeyPair()

        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        val folderC = new File(folder, "C")
        val folderD = new File(folder, "D")
        val folderE = new File(folder, "E")
        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }

        info("freeze modification 1 of graph to folder A")
        graph.model.eSet('Key, "1")
        Serialization.freeze(graph, SData(Signature.Key.freeze -> Map(folderA.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()))), folderA.toURI)
        val loader1 = Serialization.acquireLoader(folderA.toURI, SData(Signature.Key.acquire -> Signature.acceptAll))
        val records1 = loader1.sources.head.graphDescriptor.records.sorted
        val dHistory1 = loader1.sData(Digest.historyPerURI)(folderA.toURI())
        val sHistory1 = Signature.history(loader1)
        records1.map(r ⇒ dHistory1(r)._1.toString()).toList should be(List("SimpleDigestParameters(SHA-512)"))
        sHistory1(records1(0)).map {
          case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
        } should be(Map("A/" -> "RSA"))

        info("freeze modification 2 of graph to folder A")
        graph.model.eSet('Key, "2")
        Serialization.freeze(graph, SData(Signature.Key.freeze -> Map(folderA.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()))))
        val loader2 = Serialization.acquireLoader(folderA.toURI, SData(Signature.Key.acquire -> Signature.acceptAll))
        val records2 = loader2.sources.head.graphDescriptor.records.sorted
        val dHistory2 = loader2.sData(Digest.historyPerURI)(folderA.toURI())
        val sHistory2 = Signature.history(loader2)
        records2.map(r ⇒ dHistory2(r)._1.toString()).toList should be(List("SimpleDigestParameters(SHA-512)", "SimpleDigestParameters(SHA-512)"))
        loader2.load()
        sHistory2(records2(0)).map {
          case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
        } should be(Map("A/" -> "RSA"))
        sHistory2(records2(1)).map {
          case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
        } should be(Map("A/" -> "RSA"))

        info("freeze last modification of graph to folder A and B")
        Serialization.freeze(graph,
          SData(Signature.Key.freeze -> Map(
            folderA.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()),
            folderB.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()))),
          folderB.toURI)
        val loader2b = Serialization.acquireLoader(folderB.toURI, SData(Signature.Key.acquire -> Signature.acceptAll))
        val records2b = loader2b.sources.head.graphDescriptor.records.sorted
        val dHistory2b = loader2b.sData(Digest.historyPerURI)(folderB.toURI())
        val sHistory2b = Signature.history(loader2b)
        records2b.map(r ⇒ dHistory2b(r)._1.toString()).toList should be(List("NoDigest", "SimpleDigestParameters(SHA-512)"))
        loader2b.load()
        sHistory2b(records2b(0)).map {
          case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
        } should be(Map("A/" -> "RSA", "B/" -> ""))
        sHistory2b(records2b(1)).map {
          case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
        } should be(Map("A/" -> "RSA", "B/" -> "RSA"))

        info("freeze last modification of graph to folder A, B and C")
        Serialization.freeze(graph, SData(Signature.Key.freeze -> Map(
          folderA.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()),
          folderB.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()),
          folderC.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()))),
          folderC.toURI)
        val loader2c = Serialization.acquireLoader(folderC.toURI, SData(Signature.Key.acquire -> Signature.acceptAll))
        val records2c = loader2c.sources.head.graphDescriptor.records.sorted
        val dHistory2c = loader2c.sData(Digest.historyPerURI)(folderC.toURI())
        val sHistory2c = Signature.history(loader2c)
        records2c.map(r ⇒ dHistory2c(r)._1.toString()).toList should be(List("NoDigest", "SimpleDigestParameters(SHA-512)"))
        loader2c.load()
        sHistory2c(records2c(0)).map {
          case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
        } should be(Map("A/" -> "RSA", "B/" -> "", "C/" -> ""))
        sHistory2c(records2c(1)).map {
          case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
        } should be(Map("A/" -> "RSA", "B/" -> "RSA", "C/" -> "RSA"))

        info("freeze last modification of graph to folder A, B, C and D -> E")
        Serialization.freeze(graph, SData(Signature.Key.freeze -> Map(
          folderA.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()),
          folderB.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()),
          folderC.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()),
          folderD.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate())),
          SData.Key.explicitStorages -> Serialization.Storages(
            graph.storages.map(Serialization.Storages.Simple) :+
              Serialization.Storages.Complex(folderE.toURI(), folderD.toURI()))))
        val loader2d = Serialization.acquireLoader(folderD.toURI, SData(Signature.Key.acquire -> Signature.acceptAll))
        val records2d = loader2d.sources.head.graphDescriptor.records.sorted
        val dHistory2d = loader2d.sData(Digest.historyPerURI)(folderD.toURI())
        val sHistory2d = Signature.history(loader2d)
        records2d.map(r ⇒ dHistory2d(r)._1.toString()).toList should be(List("NoDigest", "SimpleDigestParameters(SHA-512)"))
        loader2d.load()
        sHistory2d(records2d(0)).map {
          case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
        } should be(Map("A/" -> "RSA", "B/" -> "", "C/" -> "", "D/" -> ""))
        sHistory2d(records2d(1)).map {
          case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
        } should be(Map("A/" -> "RSA", "B/" -> "RSA", "C/" -> "RSA", "D/" -> "RSA", "E/" -> ""))

        //val tmp = new File("/tmp/data")
        //deleteFolder(tmp)
        //tmp.mkdirs()
        //copy(folderA, new File(tmp, folderA.getName()))
        //copy(folderB, new File(tmp, folderB.getName()))
        //copy(folderC, new File(tmp, folderC.getName()))
        //copy(folderD, new File(tmp, folderD.getName()))

        val graphX = loader2d.load()
        graphX.storages.map(_.toString()).toSet should be(Set(folderA.toURI().toString(),
          folderB.toURI().toString(), folderC.toURI().toString(), folderE.toURI().toString() + "/"))

        val graphA = Serialization.acquireLoader(folderA.toURI, SData(Signature.Key.acquire -> Signature.acceptSigned)).load()
        val graphB = Serialization.acquireLoader(folderB.toURI, SData(Signature.Key.acquire -> Signature.acceptSigned)).load()
        val graphC = Serialization.acquireLoader(folderC.toURI, SData(Signature.Key.acquire -> Signature.acceptSigned)).load()
        val graphD = Serialization.acquireLoader(folderC.toURI, SData(Signature.Key.acquire -> Signature.acceptSigned)).load()

        info("move folder D -> E and aquire graph")
        folderD.renameTo(folderE)
        var keys = Seq.empty[Option[PublicKey]]
        val loader2e = Serialization.acquireLoader(folderE.toURI, SData(Signature.Key.acquire -> {
          (key: Option[PublicKey]) ⇒
            keys = keys :+ key
            key.nonEmpty
        }))
        val records2e = loader2e.sources.head.graphDescriptor.records.sorted
        val dHistory2e = loader2e.sData(Digest.historyPerURI)(folderE.toURI())
        val sHistory2e = Signature.history(loader2e)
        records2e.map(r ⇒ dHistory2e(r)._1.toString()).toList should be(List("NoDigest", "SimpleDigestParameters(SHA-512)"))
        loader2e.load()
        sHistory2e(records2e(0)).map {
          case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
        } should be(Map("A/" -> "RSA", "B/" -> "", "C/" -> "", "E/" -> ""))
        sHistory2e(records2e(1)).map {
          case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
        } should be(Map("A/" -> "RSA", "B/" -> "RSA", "C/" -> "RSA", "E/" -> "RSA"))
        val verified = keys.flatten
        verified.forall(_ == pairRSA.getPublic()) should be(true)
        verified should have size (2)
        val dHistory2eComplete = Digest.history(loader2e)
        dHistory2eComplete(records2e(0)).map {
          case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
        } should be(Map("A/" -> "SHA-512", "B/" -> "", "C/" -> "", "E/" -> ""))
        dHistory2eComplete(records2e(1)).map {
          case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
        } should be(Map("A/" -> "SHA-512", "B/" -> "SHA-512", "C/" -> "SHA-512", "E/" -> "SHA-512"))
      }
    }
    it("should have proper signatures for nested sources") {
      withTempFolder { folder ⇒
        import TestDSL._

        val keyGenRSA = KeyPairGenerator.getInstance("RSA")
        keyGenRSA.initialize(1024)
        val Alice = keyGenRSA.genKeyPair()
        val Bob = keyGenRSA.genKeyPair()
        val Mallory = keyGenRSA.genKeyPair()
        val Trudy = keyGenRSA.genKeyPair()

        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        val folderC = new File(folder, "C")
        val folderD = new File(folder, "D")
        val folderE = new File(folder, "E")
        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }

        def debugKey(key: PublicKey) = key match {
          case key if key == Alice.getPublic() ⇒ log.___glance("key Alice")
          case key if key == Bob.getPublic() ⇒ log.___glance("key Bob")
          case key if key == Mallory.getPublic() ⇒ log.___glance("key Mallory")
          case key if key == Trudy.getPublic() ⇒ log.___glance("key Trudy")
        }

        info("freeze modification 1 of graph to folderA and copy to folderB")
        graph.model.takeRecord('rA) { _.name = "1" }
        val modificationA = graph.modified
        Serialization.freeze(graph, SData(Signature.Key.freeze -> Map(
          folderA.toURI -> SimpleSignature(Alice.getPublic(), Alice.getPrivate()))), folderA.toURI)
        Serialization.freeze(graph, SData(Signature.Key.freeze -> Map(
          folderA.toURI -> SimpleSignature(Bob.getPublic(), Bob.getPrivate()),
          folderB.toURI -> SimpleSignature(Bob.getPublic(), Bob.getPrivate())),
          SData.Key.explicitStorages -> Serialization.Storages.append(graph, folderB.toURI)))

        // storage A modification A
        def testAA() {
          val loader = Serialization.acquireLoader(folderA.toURI, SData(Signature.Key.acquire -> Signature.acceptSigned))
          val history = Signature.history(loader)
          val records = history.keys.toSeq.sorted
          history(records(0)).map {
            case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.publicKey)
          } should be(Map("A/" -> Alice.getPublic()))
        }
        // storage B modification A
        def testBA() {
          val loader = Serialization.acquireLoader(folderB.toURI, SData(Signature.Key.acquire -> Signature.acceptSigned))
          val history = Signature.history(loader)
          val records = history.keys.toSeq.sorted
          history(records(0)).map {
            case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.publicKey)
          } should be(Map("A/" -> Alice.getPublic(), "B/" -> Bob.getPublic()))
        }

        testAA()
        testBA()

        info("freeze modification 2 of graph to folderB and copy to folderC")
        graph.model.takeRecord('rB) { _.name = "2" }
        val modificationB = graph.modified
        Serialization.freeze(graph, SData(Signature.Key.freeze -> Map(
          folderA.toURI -> SimpleSignature(Mallory.getPublic(), Mallory.getPrivate()),
          folderB.toURI -> SimpleSignature(Mallory.getPublic(), Mallory.getPrivate()),
          folderC.toURI -> SimpleSignature(Mallory.getPublic(), Mallory.getPrivate())),
          SData.Key.explicitStorages ->
            Serialization.Storages(Serialization.Storages.Simple(folderB.toURI), Serialization.Storages.Simple(folderC.toURI))))

        // storage B modification B
        def testBB() {
          val loader = Serialization.acquireLoader(folderB.toURI, SData(Signature.Key.acquire -> Signature.acceptSigned))
          val history = Signature.history(loader)
          val records = history.keys.toSeq.sorted
          history(records(0)).map {
            case (a, b) ⇒ (folder.toURI.relativize(a).toString(), try b.publicKey catch { case e: Throwable ⇒ null })
          } should be(Map("A/" -> Alice.getPublic(), "B/" -> Bob.getPublic(), "C/" -> null))
          history(records(1)).map {
            case (a, b) ⇒ (folder.toURI.relativize(a).toString(), try b.publicKey catch { case e: Throwable ⇒ null })
          } should be(Map("B/" -> Mallory.getPublic(), "C/" -> Mallory.getPublic()))
        }

        testAA()
        testBB()

        info("freeze modification 3 of graph to folderC and copy to folderD")
        graph.model.takeRecord('rC) { _.name = "3" }
        val modificationC = graph.modified
        Serialization.freeze(graph, SData(Signature.Key.freeze -> Map(
          folderA.toURI -> SimpleSignature(Trudy.getPublic(), Trudy.getPrivate()),
          folderB.toURI -> SimpleSignature(Trudy.getPublic(), Trudy.getPrivate()),
          folderC.toURI -> SimpleSignature(Trudy.getPublic(), Trudy.getPrivate()),
          folderD.toURI -> SimpleSignature(Trudy.getPublic(), Trudy.getPrivate())),
          SData.Key.explicitStorages ->
            Serialization.Storages(Serialization.Storages.Simple(folderC.toURI), Serialization.Storages.Simple(folderD.toURI))))

        // storage C modification C
        def testCC() {
          val loader = Serialization.acquireLoader(folderC.toURI, SData(Signature.Key.acquire -> Signature.acceptSigned))
          val history = Signature.history(loader)
          val records = history.keys.toSeq.sorted
          history(records(0)).map {
            case (a, b) ⇒ (folder.toURI.relativize(a).toString(), try b.publicKey catch { case e: Throwable ⇒ null })
          } should be(Map("A/" -> Alice.getPublic(), "B/" -> Bob.getPublic(), "C/" -> null, "D/" -> null))
          history(records(1)).map {
            case (a, b) ⇒ (folder.toURI.relativize(a).toString(), try b.publicKey catch { case e: Throwable ⇒ null })
          } should be(Map("B/" -> Mallory.getPublic(), "C/" -> Mallory.getPublic(), "D/" -> null))
          history(records(2)).map {
            case (a, b) ⇒ (folder.toURI.relativize(a).toString(), try b.publicKey catch { case e: Throwable ⇒ null })
          } should be(Map("C/" -> Trudy.getPublic(), "D/" -> Trudy.getPublic()))
        }

        testAA()
        testBB()
        testCC()

        val tmp = new File("/tmp/data")
        deleteFolder(tmp)
        tmp.mkdirs()
        copy(folderA, new File(tmp, folderA.getName()))
        copy(folderB, new File(tmp, folderB.getName()))
        copy(folderC, new File(tmp, folderC.getName()))
        copy(folderD, new File(tmp, folderD.getName()))

        var keys = Seq.empty[Option[PublicKey]]

        info("acquire from folder A with any valid signature")
        keys = Seq.empty
        // Approve signature for digest with modificationA
        val loaderA = Serialization.acquireLoader(folderA.toURI, SData(Signature.Key.acquire -> {
          (key: Option[PublicKey]) ⇒
            keys = keys :+ key
            key.foreach(debugKey)
            key.nonEmpty
        }))
        keys.flatten should be(List()) // Callback was replaced with Signature.acceptSigned
        keys = Seq.empty
        val recordsA = loaderA.sources.head.graphDescriptor.records.sorted
        recordsA should have size (1)
        loaderA.load().storages should be(Seq(folderA.toURI))
        keys.flatten should be(List(Alice.getPublic()))

        info("acquire from folder A with Alice signature")
        keys = Seq.empty
        val loaderAAlice = Serialization.acquireLoader(folderA.toURI, SData(Signature.Key.acquire -> {
          (key: Option[PublicKey]) ⇒
            keys = keys :+ key
            key == Some(Alice.getPublic())
        }))
        keys.flatten should be(List()) // Callback was replaced with Signature.acceptSigned
        loaderAAlice.load().storages should be(Seq(folderA.toURI))
        val denyAllLoader = Serialization.acquireLoader(folderA.toURI, SData(Signature.Key.acquire -> {
          (key: Option[PublicKey]) ⇒ false
        }))
        an[IllegalStateException] should be thrownBy denyAllLoader.load()

        info("acquire from folder B")
        // Approve signature for digest with modificationC from folder B
        keys = Seq.empty
        val loaderB = Serialization.acquireLoader(folderB.toURI, SData(Signature.Key.acquire -> {
          (key: Option[PublicKey]) ⇒
            keys = keys :+ key
            key.foreach(debugKey)
            key.nonEmpty
        }))
        keys.flatten.toSet should be(Set())
        // ok, we have size 3
        // because Mallory freeze the latest modification to folder B
        loaderB.sources should have size (3)
        loaderB.sources.map(_.storageURI).toSet should be(Set(folderA.toURI, folderB.toURI, folderC.toURI))
        keys = Seq.empty
        // now we load from C and we got the latest modification that points to C and D by Trudy
        val graphB = loaderB.load()
        val recordsB = graphB.retrospective.history.map(_._1)
        recordsB should have size (3)
        graphB.storages.toSet should be(Set(folderC.toURI, folderD.toURI()))
        keys.flatten should have size (4)
        keys.flatten.toSet should be(Set(Bob.getPublic(), Mallory.getPublic(), Trudy.getPublic()))

        info("acquire from folder B with Alice signature")
        // modificationC
        val loaderBAlice = Serialization.acquireLoader(folderB.toURI, SData(
          Signature.Key.acquire -> { (key: Option[PublicKey]) ⇒ key == Some(Alice.getPublic()) }))
        loaderBAlice.sources.map(_.storageURI).toSet should be(Set(folderA.toURI, folderB.toURI, folderC.toURI))
        loaderBAlice.modified should be(modificationC)
        an[IllegalStateException] should be thrownBy loaderBAlice.load()
        val loaderBAliceForce = Serialization.acquireLoader(folderB.toURI, SData(
          SData.Key.force -> true, Signature.Key.acquire -> { (key: Option[PublicKey]) ⇒ key == Some(Alice.getPublic()) }))
        an[IllegalStateException] should be thrownBy loaderBAliceForce.load()
        // modificationB
        val loaderBAliceModificationB = Serialization.acquireLoader(folderB.toURI, Some(modificationB), SData(
          Signature.Key.acquire -> { (key: Option[PublicKey]) ⇒ key == Some(Alice.getPublic()) }))
        an[IllegalStateException] should be thrownBy loaderBAliceModificationB.load()
        val loaderBAliceForceModificationB = Serialization.acquireLoader(folderB.toURI, Some(modificationB), SData(
          SData.Key.force -> true, Signature.Key.acquire -> { (key: Option[PublicKey]) ⇒ key == Some(Alice.getPublic()) }))
        an[IllegalStateException] should be thrownBy loaderBAliceForceModificationB.load()
        // modificationA
        val loaderBAliceModificationA = Serialization.acquireLoader(folderB.toURI, Some(modificationA), SData(
          Signature.Key.acquire -> { (key: Option[PublicKey]) ⇒ key == Some(Alice.getPublic()) }))
        val graphBAliceModificationA = loaderBAliceModificationA.load()
        graphBAliceModificationA.storages should be(Seq(folderA.toURI))
        graphBAliceModificationA.retrospective.history should have size (1)

        info("acquire from folder B with Bob/Mallory signature")
        // modificationC
        val loaderBMallory = Serialization.acquireLoader(folderB.toURI, SData(Signature.Key.acquire -> {
          (key: Option[PublicKey]) ⇒ key == Some(Mallory.getPublic())
        }))
        loaderBMallory.sources.map(_.storageURI).toSet should be(Set(folderA.toURI, folderB.toURI, folderC.toURI))
        loaderBMallory.modified should be(modificationC)
        an[IllegalStateException] should be thrownBy loaderBMallory.load()
        val loaderBMalloryForce = Serialization.acquireLoader(folderB.toURI, SData(Signature.Key.acquire -> {
          (key: Option[PublicKey]) ⇒ key == Some(Mallory.getPublic())
        }))
        an[IllegalStateException] should be thrownBy loaderBMalloryForce.load()
        // modificationB
        val loaderBMalloryModificationB = Serialization.acquireLoader(folderB.toURI, Some(modificationB), SData(
          Signature.Key.acquire -> { (key: Option[PublicKey]) ⇒ key == Some(Mallory.getPublic()) }))
        an[IllegalStateException] should be thrownBy loaderBMalloryModificationB.load()
        val loaderBMalloryForceModificationB = Serialization.acquireLoader(folderB.toURI, Some(modificationB), SData(
          SData.Key.force -> true, Signature.Key.acquire -> { (key: Option[PublicKey]) ⇒ key == Some(Mallory.getPublic()) }))
        val graphBMalloryForceModificationB = loaderBMalloryForceModificationB.load()
        graphBMalloryForceModificationB.retrospective.history should have size (1)
        var elements = Seq[Element]()
        val visitor = new Element.Visitor() {
          def visit(element: Element) = synchronized { elements = elements :+ element; None }
        }
        graphBMalloryForceModificationB.visit(visitor, false).toIndexedSeq // rA and rB
        elements should have size (2)
        // modificationA
        // Mallory
        val loaderBMalloryModificationA = Serialization.acquireLoader(folderB.toURI, Some(modificationA), SData(
          Signature.Key.acquire -> { (key: Option[PublicKey]) ⇒ key == Some(Mallory.getPublic()) }))
        an[IllegalStateException] should be thrownBy loaderBMalloryModificationA.load()
        val loaderBMalloryForceModificationA = Serialization.acquireLoader(folderB.toURI, Some(modificationB), SData(
          SData.Key.force -> true, Signature.Key.acquire -> { (key: Option[PublicKey]) ⇒ key == Some(Mallory.getPublic()) }))
        loaderBMalloryForceModificationA.load() // load from C
        // Bob
        val loaderBBobModificationA = Serialization.acquireLoader(folderB.toURI, Some(modificationA), SData(
          Signature.Key.acquire -> { (key: Option[PublicKey]) ⇒ key == Some(Bob.getPublic()) }))
        loaderBBobModificationA.load() // load from B

        info("acquire from folder C")
        keys = Seq.empty
        val loaderC = Serialization.acquireLoader(folderC.toURI, SData(Signature.Key.acquire -> {
          (key: Option[PublicKey]) ⇒
            keys = keys :+ key
            key.foreach(debugKey)
            key.nonEmpty
        }))
        keys should be('empty)
        // we have size 4
        // because Trudy freeze the latest modification to folder C
        loaderC.sources should have size (4)
        loaderC.sources.map(_.storageURI).toSet should be(Set(folderA.toURI, folderB.toURI, folderC.toURI, folderD.toURI))
        keys = Seq.empty
        // now we load from D and we got the latest modification that points to C and D by Trudy
        val graphC = loaderC.load()
        val recordsC = graphC.retrospective.history.map(_._1)
        recordsC should have size (3)
        graphC.storages.toSet should be(Set(folderC.toURI, folderD.toURI()))
        keys.flatten should have size (3)
        keys.flatten.toSet should be(Set(Alice.getPublic(), Mallory.getPublic(), Trudy.getPublic()))

        info("acquire from folder D")
        keys = Seq.empty
        val loaderD = Serialization.acquireLoader(folderD.toURI, SData(Signature.Key.acquire -> {
          (key: Option[PublicKey]) ⇒
            keys = keys :+ key
            key.foreach(debugKey)
            key.nonEmpty
        }))
        keys should be('empty)
        loaderD.sources should have size (4)
        loaderD.sources.map(_.storageURI).toSet should be(Set(folderA.toURI, folderB.toURI, folderC.toURI, folderD.toURI))
        keys = Seq.empty
        // now we load from D again and we got the latest modification that points to C and D by Trudy
        val graphD = loaderD.load()
        val recordsD = graphD.retrospective.history.map(_._1)
        recordsD should have size (3)
        graphD.storages.toSet should be(Set(folderC.toURI, folderD.toURI()))
      }
    }
    it("should load complex graph from nested locations") {
      withTempFolder { folder ⇒
        import TestDSL._

        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        val folderC = new File(folder, "C")
        val folderD = new File(folder, "D")
        val folderE = new File(folder, "E")
        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }

        info("freeze modification 1 of graph to folderA and copy to folderB")
        graph.model.takeRecord('rA) { _.name = "1" }
        val modificationA = graph.modified
        Serialization.freeze(graph, folderA.toURI)
        Serialization.freeze(graph, SData(SData.Key.explicitStorages ->
          Serialization.Storages.append(graph, folderB.toURI)))

        info("freeze modification 2 of graph to folderB and copy to folderC")
        graph.model.takeRecord('rB) { _.name = "2" }
        val modificationB = graph.modified
        Serialization.freeze(graph, SData(SData.Key.explicitStorages ->
          Serialization.Storages(Serialization.Storages.Simple(folderB.toURI), Serialization.Storages.Simple(folderC.toURI))))

        info("freeze modification 3 of graph to folderC and copy to folderD")
        graph.model.takeRecord('rC) { _.name = "3" }
        val modificationC = graph.modified
        Serialization.freeze(graph, SData(SData.Key.explicitStorages ->
          Serialization.Storages(Serialization.Storages.Simple(folderC.toURI), Serialization.Storages.Simple(folderD.toURI))))

        info("freeze modification 4 of graph to folderE")
        graph.model.takeRecord('rE) { _.name = "4" }
        val modificationE = graph.modified
        Serialization.freeze(graph, SData(SData.Key.explicitStorages ->
          Serialization.Storages(Serialization.Storages.Simple(folderE.toURI))))

        val graphALoader = Serialization.acquireLoader(folderA.toURI, SData(Digest.Key.acquire -> true))
        graphALoader.sources should have size (1)
        graphALoader.sources.map(_.graphDescriptor.modified).sorted should be(Seq(modificationA))
        val graphA = graphALoader.load()
        graphA.modified should be(modificationA)

        val graphBLoader = Serialization.acquireLoader(folderB.toURI, SData(Digest.Key.acquire -> true))
        graphBLoader.sources should have size (3)
        graphBLoader.sources.map(_.graphDescriptor.modified).sorted should be(Seq(modificationA, modificationB, modificationC))
        val graphB = graphBLoader.load()
        graphB.modified should be(modificationC)

        val graphCLoader = Serialization.acquireLoader(folderC.toURI, SData(Digest.Key.acquire -> true))
        graphCLoader.sources should have size (4)
        graphCLoader.sources.map(_.graphDescriptor.modified).sorted should be(Seq(modificationA, modificationB, modificationC, modificationC))
        val graphC = graphCLoader.load()
        graphC.modified should be(modificationC)

        val graphDLoader = Serialization.acquireLoader(folderD.toURI, SData(Digest.Key.acquire -> true))
        graphDLoader.sources should have size (4)
        graphDLoader.sources.map(_.graphDescriptor.modified).sorted should be(Seq(modificationA, modificationB, modificationC, modificationC))
        val graphD = graphDLoader.load()
        graphD.modified should be(modificationC)

        val graphELoader = Serialization.acquireLoader(folderE.toURI, SData(Digest.Key.acquire -> true))
        graphELoader.sources should have size (5)
        graphELoader.sources.map(_.graphDescriptor.modified).sorted should be(Seq(modificationA, modificationB, modificationC, modificationC, modificationE))
        val graphE = graphELoader.load()
        graphE.modified should be(modificationE)

        //val tmp = new File("/tmp/data")
        //deleteFolder(tmp)
        //tmp.mkdirs()
        //copy(folderA, new File(tmp, folderA.getName()))
        //copy(folderB, new File(tmp, folderB.getName()))
        //copy(folderC, new File(tmp, folderC.getName()))
        //copy(folderD, new File(tmp, folderD.getName()))
        //copy(folderE, new File(tmp, folderE.getName()))
      }
    }
    it("should have proper container and content encryption") {
      withTempFolder { folder ⇒
        import TestDSL._

        val folderA = new File(folder, "A")
        folderA.mkdirs()
        val folderB = new File(folder, "B")
        folderB.mkdirs()
        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }

        val keyGenRSA = KeyPairGenerator.getInstance("RSA")
        keyGenRSA.initialize(1024)
        val Alice = keyGenRSA.genKeyPair()

        val writeFilter = (os: OutputStream, uri: URI, transport: Transport, sData: SData) ⇒ {
          if (sData(SData.Key.storageURI) == folderA.toURI()) new TestOutputStream(os) else os
        }

        graph.model.takeRecord('rA) { _.name = "1" }
        Serialization.freeze(graph, SData(
          SData.Key.writeFilter -> writeFilter,
          SData.Key.convertURI ->
            (((name: String, sData: SData) ⇒ if (sData(SData.Key.storageURI) == folderA.toURI()) "Test_" + name else name,
              (name: String, sData: SData) ⇒ if (sData(SData.Key.storageURI) == folderA.toURI()) name.replaceAll("""^Test_""", "") else name)),
          Signature.Key.freeze ->
            Map(
              folderA.toURI -> SimpleSignature(Alice.getPublic(), Alice.getPrivate()),
              folderB.toURI -> SimpleSignature(Alice.getPublic(), Alice.getPrivate()))),
          folderA.toURI, folderB.toURI())

        visitPath(folderA, f ⇒ f.getName() should startWith("Test_"))
        visitPath(folderB, f ⇒ f.getName() should not startWith ("Test_"))
        var originalFiles = Seq[File]()
        visitPath(folderB, file ⇒ originalFiles = originalFiles :+ file)
        val originalSize = originalFiles.size
        var convertedFiles = Seq[File]()
        visitPath(folderA, file ⇒ convertedFiles = convertedFiles :+ file)
        val convertedSize = convertedFiles.size
        originalSize should be(convertedSize)
        originalFiles.sorted.corresponds(convertedFiles.sorted) { (a, b) ⇒
          if (a.isDirectory())
            ("Test_" + a.getName) == b.getName()
          else if (a.getName().endsWith(".timestamp")) {
            // Timestamp markers are unencrypted
            val sa = io.Source.fromFile(a)(io.Codec.ISO8859.charSet)
            val aData = sa.map(_.toByte).toArray
            sa.close()
            val sb = io.Source.fromFile(b)(io.Codec.ISO8859.charSet)
            val bData = sb.map(_.toByte).toArray
            sb.close()
            java.util.Arrays.equals(aData, bData) &&
              ("Test_" + a.getName) == b.getName()
          } else {
            val sa = io.Source.fromFile(a)(io.Codec.ISO8859.charSet)
            val aDataOrig = sa.map(_.toByte).toArray
            val aData = xorWithKey(aDataOrig, Array(xorN))
            sa.close()
            val sb = io.Source.fromFile(b)(io.Codec.ISO8859.charSet)
            val bData = sb.map(_.toByte).toArray
            sb.close()

            if (a.getName == "digest") {
              val digestA = new String(aDataOrig)
              val digestB = new String(xorWithKey(bData, Array(xorN))).replaceAll("""Test_""", "")
              if (digestA != digestB) {
                println(new String(aDataOrig, io.Codec.UTF8.charSet))
                println(new String(xorWithKey(bData, Array(xorN)), io.Codec.UTF8.charSet))
                fail("A != B for " + a)
              }
              digestA == digestB &&
                ("Test_" + a.getName) == b.getName()
            } else if (a.getName == "signature") {
              ("Test_" + a.getName) == b.getName()
            } else {
              if (!java.util.Arrays.equals(aData, bData))
                fail(s"${a.getName()}\n\nA ORIG:\n\n${new String(aDataOrig, io.Codec.UTF8.charSet)}\n\nA:\n\n${new String(aData, io.Codec.UTF8.charSet)}\n\nvs\n\n${new String(bData, io.Codec.UTF8.charSet)}")
              java.util.Arrays.equals(aData, bData) &&
                ("Test_" + a.getName) == b.getName()
            }
          }
        } should be(true)

        deleteFolder(folderB)

        val readFilter = (is: InputStream, uri: URI, transport: Transport, sData: SData) ⇒ {
          if (sData(SData.Key.storageURI) == folderA.toURI()) new TestInputStream(is) else is
        }

        val graph2 = Serialization.acquire(folderA.toURI, SData(
          SData.Key.readFilter -> readFilter,
          SData.Key.convertURI ->
            (((name: String, sData: SData) ⇒ if (sData(SData.Key.storageURI) == folderA.toURI()) "Test_" + name else name,
              (name: String, sData: SData) ⇒ if (sData(SData.Key.storageURI) == folderA.toURI()) name.replaceAll("""^Test_""", "") else name)),
          Signature.Key.acquire -> Signature.acceptSigned))
      }
    }
  }

  override def beforeAll(configMap: org.scalatest.ConfigMap) {
    adjustLoggingBeforeAll(configMap)
    addFileAppender()
  }
  val xorN = 123.toByte
  /** XOR data. */
  def xorWithKey(a: Array[Byte], key: Array[Byte]): Array[Byte] = {
    val out = new Array[Byte](a.length)
    for (i ← 0 until a.length)
      out(i) = (a(i) ^ key(i % key.length)).toByte
    out
  }

  /**
   * Test FilterInputStream
   */
  class TestInputStream(val inputStream: InputStream) extends FilterInputStream(inputStream) {
    override def read() = ???
    override def read(b: Array[Byte], off: Int, len: Int) = {
      val result = inputStream.read(b, off, len)
      System.arraycopy(xorWithKey(b.drop(off).take(len), Array(xorN)), 0, b, off, len)
      result
    }
    override def read(b: Array[Byte]) = read(b, 0, b.length)
  }
  /**
   * Test FilterOutputStream
   */
  class TestOutputStream(val outputStream: OutputStream) extends FilterOutputStream(outputStream) {
    override def write(b: Int) = ???
    override def write(b: Array[Byte], off: Int, len: Int) = outputStream.write(xorWithKey(b.drop(off).take(len), Array(xorN)))
    override def write(b: Array[Byte]) = write(b, 0, b.length)
  }
}
