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

package org.digimead.tabuddy.model.serialization.signature

import com.escalatesoft.subcut.inject.NewBindingModule
import java.io.{ BufferedInputStream, BufferedOutputStream, BufferedReader, File, FileInputStream, FileOutputStream, FileWriter, InputStreamReader, PrintStream, RandomAccessFile }
import java.net.URI
import java.security.{ KeyPairGenerator, PublicKey }
import java.util.UUID
import java.util.concurrent.atomic.AtomicReference
import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.{ LoggingHelper, StorageHelper }
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.TestDSL
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.{ Serialization, YAMLSerialization }
import org.digimead.tabuddy.model.serialization.SData
import org.digimead.tabuddy.model.serialization.digest.Digest
import org.digimead.tabuddy.model.serialization.digest.SimpleDigest
import org.digimead.tabuddy.model.serialization.transport.{ Local, Transport }
import org.digimead.tabuddy.model.serialization.yaml.Timestamp
import org.hamcrest.{ BaseMatcher, Description }
import org.mockito.{ Matchers ⇒ MM, Mockito }
import org.scalatest.{ FreeSpec, Matchers }
import scala.ref.SoftReference
import sun.security.rsa.RSAPublicKeyImpl

class SimpleSignatureSpec extends FreeSpec with Matchers with StorageHelper with LoggingHelper with Loggable {
  lazy val testSignature = Mockito.spy(new TestSimple)
  lazy val testTransport = Mockito.spy(new Local)

  before {
    DependencyInjection(new NewBindingModule(module ⇒ {
      module.bind[Mechanism] identifiedBy ("Signature.Mechanism.Simple") toSingle { testSignature }
      module.bind[Transport] identifiedBy ("Serialization.Transport.Local") toSingle { testTransport }
    }) ~ org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
  }

  "Signature should handle DSA keys" in {
    withTempFolder { folder ⇒
      val keyGen = KeyPairGenerator.getInstance("DSA")
      keyGen.initialize(1024)
      val generatedKeyPair = keyGen.genKeyPair()
      val ss = Signature.perIdentifier(SimpleSignature.Identifier).asInstanceOf[TestSimple]

      val pubB64 = ss.savePublicKey(generatedKeyPair.getPublic())
      // Test: openssl dsa -pubin -in key -text
      val pubB64Lines = pubB64.split("\n")
      pubB64Lines.head should be("-----BEGIN PUBLIC KEY-----")
      pubB64Lines.last should be("-----END PUBLIC KEY-----")
      val pubBin = ss.loadPublicKey("DSA", pubB64)
      pubBin should be(generatedKeyPair.getPublic())

      val privB64 = ss.savePrivateKey(generatedKeyPair.getPrivate())
      // Test: openssl dsa -in key -text
      val privB64Lines = privB64.split("\n")
      privB64Lines.head should be("-----BEGIN PRIVATE KEY-----")
      privB64Lines.last should be("-----END PRIVATE KEY-----")
      val privBin = ss.loadPrivateKey("DSA", privB64)
      privBin should be(generatedKeyPair.getPrivate())

      SimpleSignature(generatedKeyPair.getPublic(), generatedKeyPair.getPrivate()).publicKey should be(pubBin)

      val fileAWithRandomData = new File(folder, s"DSAfile")
      fileAWithRandomData.createNewFile()
      var signature = Array.empty[Byte]
      var toWrite = 0
      var toRead = 0

      val sign = java.security.Signature.getInstance("SHA1withDSA")
      sign.initSign(generatedKeyPair.getPrivate())
      val out = new PrintStream(new Signature.SignatureOutputStream(
        new BufferedOutputStream(new FileOutputStream(fileAWithRandomData)), sign, s ⇒
          signature = s.sign()) {
        override def write(b: Int) {
          super.write(b)
          toWrite += 1
        }
        override def write(b: Array[Byte], off: Int, len: Int) {
          super.write(b, off, len)
          toWrite += len
        }
      })
      out.println("test")
      out.flush()
      out.close()
      signature should not be (empty)
      toWrite should be(5)
      fileAWithRandomData.length() should be(5)

      val verifier = java.security.Signature.getInstance("SHA1withDSA")
      verifier.initVerify(generatedKeyPair.getPublic())
      val in = new BufferedReader(new InputStreamReader(new Signature.SignatureInputStream(
        new BufferedInputStream(new FileInputStream(fileAWithRandomData)), verifier, s ⇒
          verifier.verify(signature) should be(true)) {
        override def read(): Int = {
          val result = super.read()
          if (result != -1)
            toRead += result
          result
        }
        override def read(b: Array[Byte], off: Int, len: Int) = {
          val result = super.read(b, off, len)
          if (result > 0)
            toRead += result
          result
        }
      }))
      var line = in.readLine()
      while (line != null) {
        line = in.readLine()
      }
      in.close()
      toRead should be(5)
    }
  }
  "Signature should handle RSA keys" in {
    withTempFolder { folder ⇒
      val keyGen = KeyPairGenerator.getInstance("RSA")
      keyGen.initialize(1024)
      val generatedKeyPair = keyGen.genKeyPair()
      val ss = Signature.perIdentifier(SimpleSignature.Identifier).asInstanceOf[TestSimple]

      val pubB64 = ss.savePublicKey(generatedKeyPair.getPublic())
      // Test: openssl rsa -pubin -in key -text
      val pubB64Lines = pubB64.split("\n")
      pubB64Lines.head should be("-----BEGIN PUBLIC KEY-----")
      pubB64Lines.last should be("-----END PUBLIC KEY-----")
      val pubBin = ss.loadPublicKey("RSA", pubB64)
      pubBin should be(generatedKeyPair.getPublic())

      val privB64 = ss.savePrivateKey(generatedKeyPair.getPrivate())
      // Test: openssl rsa -in key -text
      val privB64Lines = privB64.split("\n")
      privB64Lines.head should be("-----BEGIN PRIVATE KEY-----")
      privB64Lines.last should be("-----END PRIVATE KEY-----")
      val privBin = ss.loadPrivateKey("RSA", privB64)
      privBin should be(generatedKeyPair.getPrivate())

      SimpleSignature(generatedKeyPair.getPublic(), generatedKeyPair.getPrivate()).publicKey should be(pubBin)

      val fileAWithRandomData = new File(folder, s"RSAfile")
      fileAWithRandomData.createNewFile()
      var signature = Array.empty[Byte]
      var toWrite = 0
      var toRead = 0

      val sign = java.security.Signature.getInstance("SHA1withRSA")
      sign.initSign(generatedKeyPair.getPrivate())
      val out = new PrintStream(new Signature.SignatureOutputStream(
        new BufferedOutputStream(new FileOutputStream(fileAWithRandomData)), sign, s ⇒
          signature = s.sign()) {
        override def write(b: Int) {
          super.write(b)
          toWrite += 1
        }
        override def write(b: Array[Byte], off: Int, len: Int) {
          super.write(b, off, len)
          toWrite += len
        }
      })
      out.println("test")
      out.flush()
      out.close()
      signature should not be (empty)
      toWrite should be(5)
      fileAWithRandomData.length() should be(5)

      val verifier = java.security.Signature.getInstance("SHA1withRSA")
      verifier.initVerify(generatedKeyPair.getPublic())
      val in = new BufferedReader(new InputStreamReader(new Signature.SignatureInputStream(
        new BufferedInputStream(new FileInputStream(fileAWithRandomData)), verifier, s ⇒
          verifier.verify(signature) should be(true)) {
        override def read(): Int = {
          val result = super.read()
          if (result != -1)
            toRead += result
          result
        }
        override def read(b: Array[Byte], off: Int, len: Int) = {
          val result = super.read(b, off, len)
          if (result > 0)
            toRead += result
          result
        }
      }))
      var line = in.readLine()
      while (line != null) {
        line = in.readLine()
      }
      in.close()
      toRead should be(5)
    }
  }
  "Signature should add default Digest arguments" in {
    withTempFolder { folder ⇒
      import TestDSL._

      val keyGenRSA = KeyPairGenerator.getInstance("RSA")
      keyGenRSA.initialize(1024)
      val pairRSA = keyGenRSA.genKeyPair()
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }

      val modification = Timestamp.dump(graph.modified)
      val folderA = new File(folder, "A")
      val fileADigestType = new File(folderA, s"digest/${modification}/type")
      val fileADigestData = new File(folderA, s"digest/${modification}/digest")
      val fileASignatureType = new File(folderA, s"signature/${modification}/type")
      val fileASignatureData = new File(folderA, s"signature/${modification}/signature")

      Serialization.freeze(graph, SData(Signature.Key.freeze -> Map(folderA.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()))),
        folderA.getAbsoluteFile().toURI())
      fileADigestType should be('exists)
      fileADigestData should be('exists)
      fileASignatureType should be('exists)
      fileASignatureData should be('exists)
    }
  }
  "SimpleSignature should (de)serialize mechanism parameters" in {
    val keyGenRSA = KeyPairGenerator.getInstance("RSA")
    keyGenRSA.initialize(1024)
    val pairRSA = keyGenRSA.genKeyPair()
    val orig = SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate())
    val copy = Signature.perIdentifier(orig.mechanism.identifier)(orig.algorithm, orig.arguments: _*)
    copy should be(orig)
  }
  "Test Graph.Loader creation for graph with 1 source without signature" in {
    withTempFolder { folder ⇒
      import TestDSL._

      val folderA = new File(folder, "A")
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      Serialization.freeze(graph, folderA.getAbsoluteFile().toURI())

      val loader = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      loader.sData(Signature.historyPerURI).values.flatMap(_.values.map(_._1)) should be(List(Signature.NoSignature))
    }
  }
  "Test Graph.Loader creation for graph with 1 source" in {
    withTempFolder { folder ⇒
      import TestDSL._

      val keyGenRSA = KeyPairGenerator.getInstance("RSA")
      keyGenRSA.initialize(1024)
      val pairRSA = keyGenRSA.genKeyPair()
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }

      val modification = Timestamp.dump(graph.modified)
      val folderA = new File(folder, "A")
      val fileASignatureType = new File(folderA, s"signature/${modification}/type")
      val fileASignatureData = new File(folderA, s"signature/${modification}/signature")

      Serialization.freeze(graph, SData(Signature.Key.freeze -> Map(folderA.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()))),
        folderA.getAbsoluteFile().toURI())
      fileASignatureType should be('exists)
      fileASignatureData should be('exists)

      info("test permissive good")
      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderGood = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { _ ⇒ true }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      val graphGood = loaderGood.load()

      info("test strict good")
      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderGood2 = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      val graphGood2 = loaderGood2.load()

      graphGood.node.safeRead { node ⇒
        graphGood2.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)

      info("test permissive broken")

      {
        val file = new RandomAccessFile(fileASignatureData, "rws")
        val text = new Array[Byte](fileASignatureData.length().toInt)
        file.readFully(text)
        file.seek(0)
        file.writeBytes("broken_signature digest\n")
        file.write(text)
        file.close()
      }

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      an[IllegalStateException] should be thrownBy Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { _ ⇒ true }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())

      info("test strict broken")
      log.___glance("test strict broken")
      Mockito.reset(testSignature)
      an[IllegalStateException] should be thrownBy Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())

      info("test strict broken v2")
      log.___glance("test strict broken v2")
      val fw2 = new FileWriter(fileASignatureData)
      fw2.write("broken")
      fw2.close()

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      an[IllegalStateException] should be thrownBy Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())

      info("test strict broken v3")
      log.___glance("test strict broken v3")
      fileASignatureData.delete()

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      an[IllegalStateException] should be thrownBy Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())

      info("test strict broken v4")
      log.___glance("test strict broken v4")
      val fw3 = new FileWriter(fileASignatureType)
      fw3.write("broken")
      fw3.close()

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderBad4 = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      loaderBad4.sData(Signature.historyPerURI)(folderA.toURI())(loaderBad4.modified)._1 should be(Signature.NoSignature)

      info("test strict broken v5")
      log.___glance("test strict broken v5")
      fileASignatureType.delete()

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderBad5 = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      loaderBad5.sData(Signature.historyPerURI)(folderA.toURI())(loaderBad4.modified)._1 should be(Signature.NoSignature)

      loaderGood.sData.get(SimpleSignature.printStream) should be(None)
      loaderGood2.sData.get(SimpleSignature.printStream) should be(None)
      loaderBad4.sData.get(SimpleSignature.printStream) should be(None)
      loaderBad5.sData.get(SimpleSignature.printStream) should be(None)
      loaderGood.sources should be(loaderGood2.sources)
      loaderGood.sources should be(loaderBad4.sources)
      loaderGood.sources should be(loaderBad5.sources)
      loaderGood.modified should be(loaderGood2.modified)
      loaderGood.modified should be(loaderBad4.modified)
      loaderGood.modified should be(loaderBad5.modified)
    }
  }
  "Test Graph.Loader creation for graph with 2 sources (symmetric)" in {
    withTempFolder { folder ⇒
      import TestDSL._

      val keyGenRSA = KeyPairGenerator.getInstance("RSA")
      keyGenRSA.initialize(1024)
      val pairRSA = keyGenRSA.genKeyPair()
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }

      val modification = Timestamp.dump(graph.modified)
      val folderA = new File(folder, "A")
      val fileASignatureType = new File(folderA, s"signature/${modification}/type")
      val fileASignatureData = new File(folderA, s"signature/${modification}/signature")
      val folderB = new File(folder, "B")
      val fileBSignatureType = new File(folderB, s"signature/${modification}/type")
      val fileBSignatureData = new File(folderB, s"signature/${modification}/signature")

      Serialization.freeze(graph, SData(Signature.Key.freeze -> Map(
        folderA.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()),
        folderB.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()))),
        folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI())
      fileASignatureType should be('exists)
      fileASignatureData should be('exists)
      fileBSignatureType should be('exists)
      fileBSignatureData should be('exists)

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)

      info("test permissive good")
      Mockito.reset(testSignature)
      val loaderGood = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { _ ⇒ true }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(4)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      val graphGood = loaderGood.load()

      info("test strict good")
      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderGood2 = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      // getSignatureMap from SimpleSignature.readFilter (check that map is available)
      // getSignatureMap from SimpleSignature.checkSignature
      Mockito.verify(testSignature, Mockito.times(4)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      val graphGood2 = loaderGood2.load()

      graphGood.node.safeRead { node ⇒
        graphGood2.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)

      info("test permissive broken")

      {
        val file = new RandomAccessFile(fileASignatureData, "rws")
        val text = new Array[Byte](fileASignatureData.length().toInt)
        file.readFully(text)
        file.seek(0)
        file.writeBytes("broken_signature digest\n")
        file.write(text)
        file.close()
      }

      {
        val file = new RandomAccessFile(fileBSignatureData, "rws")
        val text = new Array[Byte](fileBSignatureData.length().toInt)
        file.readFully(text)
        file.seek(0)
        file.writeBytes("broken_signature digest\n")
        file.write(text)
        file.close()
      }

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      an[IllegalStateException] should be thrownBy Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { _ ⇒ true }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())

      info("test strict broken")
      log.___glance("test strict broken")
      Mockito.reset(testSignature)
      an[IllegalStateException] should be thrownBy Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())

      info("test strict broken v2")
      log.___glance("test strict broken v2")
      val fw2 = new FileWriter(fileASignatureData)
      fw2.write("broken")
      fw2.close()
      val fw2X = new FileWriter(fileBSignatureData)
      fw2X.write("broken")
      fw2X.close()

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      an[IllegalStateException] should be thrownBy Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())

      info("test strict broken v3")
      log.___glance("test strict broken v3")
      fileASignatureData.delete()
      fileBSignatureData.delete()

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      an[IllegalStateException] should be thrownBy Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())

      info("test strict broken v4")
      log.___glance("test strict broken v4")
      val fw3 = new FileWriter(fileASignatureType)
      fw3.write("broken")
      fw3.close()
      val fw3X = new FileWriter(fileBSignatureType)
      fw3X.write("broken")
      fw3X.close()

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderBad4 = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      loaderBad4.sData(Signature.historyPerURI)(folderA.toURI())(loaderBad4.modified)._1 should be(Signature.NoSignature)

      info("test strict broken v5")
      log.___glance("test strict broken v5")
      fileASignatureType.delete()
      fileBSignatureType.delete()

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderBad5 = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(0)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      loaderBad5.sData(Signature.historyPerURI)(folderA.toURI())(loaderBad4.modified)._1 should be(Signature.NoSignature)

      loaderGood.sData.get(SimpleSignature.printStream) should be(None)
      loaderGood2.sData.get(SimpleSignature.printStream) should be(None)
      loaderBad4.sData.get(SimpleSignature.printStream) should be(None)
      loaderBad5.sData.get(SimpleSignature.printStream) should be(None)
      loaderGood.sources should be(loaderGood2.sources)
      loaderGood.sources should be(loaderBad4.sources)
      loaderGood.sources should be(loaderBad5.sources)
      loaderGood.modified should be(loaderGood2.modified)
      loaderGood.modified should be(loaderBad4.modified)
      loaderGood.modified should be(loaderBad5.modified)
    }
  }
  "Test Graph.Loader creation for graph with 2 sources (asymmetric/A broken)" in {
    withTempFolder { folder ⇒
      import TestDSL._

      val keyGenRSA = KeyPairGenerator.getInstance("RSA")
      keyGenRSA.initialize(1024)
      val pairRSA = keyGenRSA.genKeyPair()
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }

      val modification = Timestamp.dump(graph.modified)
      val folderA = new File(folder, "A")
      val fileASignatureType = new File(folderA, s"signature/${modification}/type")
      val fileASignatureData = new File(folderA, s"signature/${modification}/signature")
      val folderB = new File(folder, "B")
      val fileBSignatureType = new File(folderB, s"signature/${modification}/type")
      val fileBSignatureData = new File(folderB, s"signature/${modification}/signature")

      Serialization.freeze(graph, SData(Signature.Key.freeze -> Map(
        folderA.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()),
        folderB.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()))),
        folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI())
      fileASignatureType should be('exists)
      fileASignatureData should be('exists)
      fileBSignatureType should be('exists)
      fileBSignatureData should be('exists)

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)

      info("test permissive good")
      Mockito.reset(testSignature)
      val loaderGood = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { _ ⇒ true }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(4)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      val graphGood = loaderGood.load()

      info("test strict good")
      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderGood2 = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      // getSignatureMap from SimpleSignature.readFilter (check that map is available)
      // getSignatureMap from SimpleSignature.checkSignature
      Mockito.verify(testSignature, Mockito.times(4)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      val graphGood2 = loaderGood2.load()

      graphGood.node.safeRead { node ⇒
        graphGood2.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)

      info("test permissive broken")

      {
        val file = new RandomAccessFile(fileASignatureData, "rws")
        val text = new Array[Byte](fileASignatureData.length().toInt)
        file.readFully(text)
        file.seek(0)
        file.writeBytes("broken_signature digest\n")
        file.write(text)
        file.close()
      }

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderBad = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { _ ⇒ true }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(3)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      val graphBad = loaderBad.load()

      graphGood2.node.safeRead { node ⇒
        graphBad.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)

      info("test strict broken")
      log.___glance("test strict broken")
      Mockito.reset(testSignature)
      val loaderBadX = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(3)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())

      info("test strict broken v2")
      log.___glance("test strict broken v2")
      val fw2 = new FileWriter(fileASignatureData)
      fw2.write("broken")
      fw2.close()

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderBad2 = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(3)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())

      info("test strict broken v3")
      log.___glance("test strict broken v3")
      fileASignatureData.delete()

      Mockito.reset(testSignature)
      val loaderBad3 = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(3)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())

      info("test strict broken v4")
      log.___glance("test strict broken v4")
      val fw3 = new FileWriter(fileASignatureType)
      fw3.write("broken")
      fw3.close()

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderBad4 = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      loaderBad4.sData(Signature.historyPerURI)(folderA.toURI())(loaderBad4.modified)._1 should be(Signature.NoSignature)
      loaderBad4.sData(Signature.historyPerURI)(folderB.toURI())(loaderBad4.modified)._1.mechanism should be(Signature.perIdentifier(SimpleSignature.Identifier))

      info("test strict broken v5")
      log.___glance("test strict broken v5")
      fileASignatureType.delete()

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderBad5 = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      loaderBad5.sData(Signature.historyPerURI)(folderA.toURI())(loaderBad4.modified)._1 should be(Signature.NoSignature)
      loaderBad5.sData(Signature.historyPerURI)(folderB.toURI())(loaderBad5.modified)._1.mechanism should be(Signature.perIdentifier(SimpleSignature.Identifier))

      loaderGood.sData.get(SimpleSignature.printStream) should be(None)
      loaderGood2.sData.get(SimpleSignature.printStream) should be(None)
      loaderBad.sData.get(SimpleSignature.printStream) should be(None)
      loaderBad4.sData.get(SimpleSignature.printStream) should be(None)
      loaderBad5.sData.get(SimpleSignature.printStream) should be(None)
      loaderGood.sources should be(loaderGood2.sources)
      loaderGood.sources.filterNot(_.storageURI.toString.endsWith("/A/")) should be(loaderBad.sources)
      loaderGood.sources should be(loaderBad4.sources)
      loaderGood.sources should be(loaderBad5.sources)
      loaderGood.modified should be(loaderGood2.modified)
      loaderGood.modified should be(loaderBad.modified)
      loaderGood.modified should be(loaderBad4.modified)
      loaderGood.modified should be(loaderBad5.modified)
      loaderGood.sData.keySet should be(loaderBad.sData.keySet)
      loaderGood.sData(Signature.historyPerURI)(folderA.toURI()).keySet should be(loaderBad.sData(Signature.historyPerURI)(folderA.toURI()).keySet)
      loaderGood.sData(Signature.historyPerURI)(folderA.toURI()).values.map(_._1).toSet should be(loaderBad.sData(Signature.historyPerURI)(folderA.toURI()).values.map(_._1).toSet)
    }
  }
  "Test Graph.Loader creation for graph with 2 sources (asymmetric/B broken)" in {
    withTempFolder { folder ⇒
      import TestDSL._

      val keyGenRSA = KeyPairGenerator.getInstance("RSA")
      keyGenRSA.initialize(1024)
      val pairRSA = keyGenRSA.genKeyPair()
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }

      val modification = Timestamp.dump(graph.modified)
      val folderA = new File(folder, "A")
      val fileASignatureType = new File(folderA, s"signature/${modification}/type")
      val fileASignatureData = new File(folderA, s"signature/${modification}/signature")
      val folderB = new File(folder, "B")
      val fileBSignatureType = new File(folderB, s"signature/${modification}/type")
      val fileBSignatureData = new File(folderB, s"signature/${modification}/signature")

      Serialization.freeze(graph, SData(Signature.Key.freeze -> Map(
        folderA.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()),
        folderB.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()))),
        folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI())
      fileASignatureType should be('exists)
      fileASignatureData should be('exists)
      fileBSignatureType should be('exists)
      fileBSignatureData should be('exists)

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)

      info("test permissive good")
      Mockito.reset(testSignature)
      val loaderGood = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { _ ⇒ true }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(4)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      val graphGood = loaderGood.load()

      info("test strict good")
      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderGood2 = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      // getSignatureMap from SimpleSignature.readFilter (check that map is available)
      // getSignatureMap from SimpleSignature.checkSignature
      Mockito.verify(testSignature, Mockito.times(4)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      val graphGood2 = loaderGood2.load()

      graphGood.node.safeRead { node ⇒
        graphGood2.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)

      info("test permissive broken")

      {
        val file = new RandomAccessFile(fileBSignatureData, "rws")
        val text = new Array[Byte](fileBSignatureData.length().toInt)
        file.readFully(text)
        file.seek(0)
        file.writeBytes("broken_signature digest\n")
        file.write(text)
        file.close()
      }

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderBad = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { _ ⇒ true }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(3)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      val graphBad = loaderBad.load()

      graphGood2.node.safeRead { node ⇒
        graphBad.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)

      info("test strict broken")
      log.___glance("test strict broken")
      Mockito.reset(testSignature)
      val loaderBadX = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(3)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())

      info("test strict broken v2")
      log.___glance("test strict broken v2")
      val fw2 = new FileWriter(fileBSignatureData)
      fw2.write("broken")
      fw2.close()

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderBad2 = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(3)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())

      info("test strict broken v3")
      log.___glance("test strict broken v3")
      fileBSignatureData.delete()

      Mockito.reset(testSignature)
      val loaderBad3 = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(3)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())

      info("test strict broken v4")
      log.___glance("test strict broken v4")
      val fw3 = new FileWriter(fileBSignatureType)
      fw3.write("broken")
      fw3.close()

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderBad4 = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      loaderBad4.sData(Signature.historyPerURI)(folderA.toURI())(loaderBad4.modified)._1.mechanism should be(Signature.perIdentifier(SimpleSignature.Identifier))
      loaderBad4.sData(Signature.historyPerURI)(folderB.toURI())(loaderBad4.modified)._1 should be(Signature.NoSignature)

      info("test strict broken v5")
      log.___glance("test strict broken v5")
      fileBSignatureType.delete()

      Mockito.reset(testSignature)
      Serialization.acquireLoader(folderA.toURI())
      Mockito.verifyNoMoreInteractions(testSignature)
      Mockito.reset(testSignature)
      val loaderBad5 = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> { key ⇒ key.nonEmpty }))
      Mockito.verify(testSignature, Mockito.times(1)).initAcquire(MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).apply(MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).readFilter(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(2)).getSignatureMap(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      Mockito.verify(testSignature, Mockito.times(1)).checkSignature(MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject(), MM.anyObject())
      loaderBad5.sData(Signature.historyPerURI)(folderA.toURI())(loaderBad5.modified)._1.mechanism should be(Signature.perIdentifier(SimpleSignature.Identifier))
      loaderBad5.sData(Signature.historyPerURI)(folderB.toURI())(loaderBad4.modified)._1 should be(Signature.NoSignature)

      loaderGood.sData.get(SimpleSignature.printStream) should be(None)
      loaderGood2.sData.get(SimpleSignature.printStream) should be(None)
      loaderBad.sData.get(SimpleSignature.printStream) should be(None)
      loaderBad4.sData.get(SimpleSignature.printStream) should be(None)
      loaderBad5.sData.get(SimpleSignature.printStream) should be(None)
      loaderGood.sources should be(loaderGood2.sources)
      loaderGood.sources.filterNot(_.storageURI.toString.endsWith("/B/")) should be(loaderBad.sources)
      loaderGood.sources should be(loaderBad4.sources)
      loaderGood.sources should be(loaderBad5.sources)
      loaderGood.modified should be(loaderGood2.modified)
      loaderGood.modified should be(loaderBad.modified)
      loaderGood.modified should be(loaderBad4.modified)
      loaderGood.modified should be(loaderBad5.modified)
      loaderGood.sData.keySet should be(loaderBad.sData.keySet)
      loaderGood.sData(Signature.historyPerURI)(folderA.toURI()).keySet should be(loaderBad.sData(Signature.historyPerURI)(folderA.toURI()).keySet)
      loaderGood.sData(Signature.historyPerURI)(folderA.toURI()).values.map(_._1).toSet should be(loaderBad.sData(Signature.historyPerURI)(folderA.toURI()).values.map(_._1).toSet)
    }
  }
  "Signature should generated for every graph digest" in {
    withTempFolder { folder ⇒
      import TestDSL._

      val keyGenDSA = KeyPairGenerator.getInstance("DSA")
      keyGenDSA.initialize(1024)
      val pairDSA = keyGenDSA.genKeyPair()
      val keyGenRSA = KeyPairGenerator.getInstance("RSA")
      keyGenRSA.initialize(4096)
      val pairRSA = keyGenRSA.genKeyPair()

      pairRSA.getPublic().asInstanceOf[RSAPublicKeyImpl].getModulus().bitLength() should be(4096)

      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      val modification = Timestamp.dump(graph.modified)
      val folderA = new File(folder, "A")
      val fileASignatureType = new File(folderA, s"signature/${modification}/type")
      val fileASignatureData = new File(folderA, s"signature/${modification}/signature")
      val folderB = new File(folder, "B")
      val fileBSignatureType = new File(folderB, s"signature/${modification}/type")
      val fileBSignatureData = new File(folderB, s"signature/${modification}/signature")
      val folderC = new File(folder, "C")
      val fileCSignatureType = new File(folderC, s"signature/${modification}/type")
      val fileCSignatureData = new File(folderC, s"signature/${modification}/signature")
      folderA should not be ('exists)
      folderB should not be ('exists)
      folderC should not be ('exists)
      val sDataFreeze = SData(Signature.Key.freeze -> Map(folderA.toURI -> Signature.NoSignature,
        folderB.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()),
        folderC.toURI -> SimpleSignature(pairDSA.getPublic(), pairDSA.getPrivate())),
        Digest.Key.freeze -> Map(folderA.toURI -> Digest.NoDigest,
          folderB.toURI -> SimpleDigest("MD5"),
          folderC.toURI -> SimpleDigest("SHA-512")))
      Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
      folderA should be('exists)
      fileASignatureType should not be ('exists)
      fileASignatureData should not be ('exists)
      folderB should be('exists)
      fileBSignatureType should be('exists)
      fileBSignatureType.length() should be(836)
      val fileBSignatureTypeLines = scala.io.Source.fromFile(fileBSignatureType).getLines.toList
      fileBSignatureTypeLines.take(5) should be(Seq("simple", "", "RSA", "", "-----BEGIN PUBLIC KEY-----"))
      fileBSignatureTypeLines.takeRight(5) should be(Seq("-----END PUBLIC KEY-----", "", "SHA1withRSA", "", "SunRsaSign"))
      fileBSignatureData should be('exists)
      scala.io.Source.fromFile(fileBSignatureData).getLines.size should be(2)
      folderC should be('exists)
      fileCSignatureType should be('exists)
      fileCSignatureType.length() should be(683)
      val fileCSignatureTypeLines = scala.io.Source.fromFile(fileCSignatureType).getLines.toList
      fileCSignatureTypeLines.take(5) should be(Seq("simple", "", "DSA", "", "-----BEGIN PUBLIC KEY-----"))
      fileCSignatureTypeLines.takeRight(5) should be(Seq("-----END PUBLIC KEY-----", "", "SHA1withDSA", "", "SUN"))
      fileCSignatureData should be('exists)
      scala.io.Source.fromFile(fileCSignatureData).getLines.size should be(2)

      model.eSet('AAAKey, "AAA1").eSet('BBBKey, "BBB1").eRelative
      model.takeRecord('baseLevel1) { r ⇒ r.name = "123" }
      Serialization.freeze(graph, sDataFreeze)
      val modification2 = Timestamp.dump(graph.modified)
      modification should not be (modification2)
      val fileASignatureTypeX = new File(folderA, s"signature/${modification2}/type")
      fileASignatureTypeX should not be ('exists)
      val fileBSignatureTypeX = new File(folderB, s"signature/${modification2}/type")
      fileBSignatureTypeX should be('exists)
      fileBSignatureTypeX.length() should be(836)
      val fileCSignatureTypeX = new File(folderC, s"signature/${modification2}/type")
      fileCSignatureTypeX should be('exists)
      fileCSignatureTypeX.length() should be(683)

      @volatile var signatureAcquireKeys = Seq.empty[PublicKey]
      val signatureAcquire = (key: Option[PublicKey]) ⇒ {
        signatureAcquireKeys = signatureAcquireKeys ++ key
        key.nonEmpty
      }
      an[IllegalArgumentException] should be thrownBy Serialization.acquire(folderB.getAbsoluteFile().toURI(),
        SData(Signature.Key.acquire -> signatureAcquire, Digest.Key.acquire -> false))

      val graph2 = Serialization.acquire(folderB.getAbsoluteFile().toURI(),
        SData(Signature.Key.acquire -> signatureAcquire))
      /*
       * -> acquireGraphLoader -> getSources
       * -> acquireGraph
       * 1st modification
       * B digest None recordFromYAML(source.transport.read(encode(recordURI, sDataForStorage),
       *                   sDataForStorage.updated(SData.Key.storageURI, source.storageURI)))
       * B digest Some recordFromYAML(source.transport.read(encode(recordURI, sDataForStorage),
       *                   sDataForStorage.updated(SData.Key.storageURI, source.storageURI)))
       * 2nd modification
       * B digest None recordFromYAML(source.transport.read(encode(recordURI, sDataForStorage),
       *                   sDataForStorage.updated(SData.Key.storageURI, source.storageURI)))
       * B digest Some recordFromYAML(source.transport.read(encode(recordURI, sDataForStorage),
       *                   sDataForStorage.updated(SData.Key.storageURI, source.storageURI)))
       */
      signatureAcquireKeys should have size (2) // pairRSA from acquireGraph
      signatureAcquireKeys.toSet should be(Set(pairRSA.getPublic()))

      val keyGenRSA2 = KeyPairGenerator.getInstance("RSA")
      keyGenRSA.initialize(1024)
      val pairRSA2 = keyGenRSA.genKeyPair()
      graph2.model.takeRecord('baseLevel2) { r ⇒ r.name = "234" }
      pairRSA2.getPublic() should not be (pairRSA.getPublic())

      val sDataFreeze2 = SData(Signature.Key.freeze -> Map(folderA.toURI -> Signature.NoSignature,
        folderB.toURI -> SimpleSignature(pairRSA2.getPublic(), pairRSA2.getPrivate()),
        folderC.toURI -> SimpleSignature(pairRSA2.getPublic(), pairRSA2.getPrivate())))
      Serialization.freeze(graph2, sDataFreeze2)

      var signatureAcquireKeys2 = Seq.empty[PublicKey]
      val signatureAcquire2 = (key: Option[PublicKey]) ⇒ {
        signatureAcquireKeys2 = signatureAcquireKeys2 ++ key
        key.nonEmpty
      }
      val graph3 = Serialization.acquire(folderB.getAbsoluteFile().toURI(),
        SData(Signature.Key.acquire -> signatureAcquire2, Digest.Key.acquire -> true))
      /*
       * -> acquireGraphLoader -> getSources
       * B digest None readFilter graphDescriptorFromYaml(transport.read(encode(graphURI, sDataForStorage), sDataForStorage))
       * B digest Some checkSignature graphDescriptorFromYaml(transport.read(encode(graphURI, sDataForStorage), sDataForStorage))
       * C digest None readFilter graphDescriptorFromYaml(transport.read(encode(graphURI, sDataForStorage), sDataForStorage))
       * C digest Some checkSignature graphDescriptorFromYaml(transport.read(encode(graphURI, sDataForStorage), sDataForStorage))
       * -> acquireGraph
       * with pairRSA2 (third modification)
       * B digest None recordFromYAML(source.transport.read(encode(recordURI, sDataForStorage),
       *                   sDataForStorage.updated(SData.Key.storageURI, source.storageURI)))
       * B digest Some recordFromYAML(source.transport.read(encode(recordURI, sDataForStorage),
       *                   sDataForStorage.updated(SData.Key.storageURI, source.storageURI)))
       * with pairRSA (first and second modification)
       * 1st modification
       * B digest None recordFromYAML(source.transport.read(encode(recordURI, sDataForStorage),
       *                   sDataForStorage.updated(SData.Key.storageURI, source.storageURI)))
       * B digest Some recordFromYAML(source.transport.read(encode(recordURI, sDataForStorage),
       *                   sDataForStorage.updated(SData.Key.storageURI, source.storageURI)))
       * 2nd modification
       * B digest None recordFromYAML(source.transport.read(encode(recordURI, sDataForStorage),
       *                   sDataForStorage.updated(SData.Key.storageURI, source.storageURI)))
       * B digest Some recordFromYAML(source.transport.read(encode(recordURI, sDataForStorage),
       *                   sDataForStorage.updated(SData.Key.storageURI, source.storageURI)))
       */
      signatureAcquireKeys2 should have size (3)
      signatureAcquireKeys2.toSet should be(Set(pairRSA.getPublic(), pairRSA2.getPublic()))

      graph3.node.safeRead { node ⇒
        graph2.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)
    }
  }
  "Signature history should be projection of Graph.Retrospective: everything OK" in {
    withTempFolder { folder ⇒
      import TestDSL._

      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val keyGenDSA1 = KeyPairGenerator.getInstance("DSA")
      keyGenDSA1.initialize(1024)
      val pairDSA1 = keyGenDSA1.genKeyPair()
      val sDataFreeze = SData(Digest.Key.freeze ->
        Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")),
        Signature.Key.freeze ->
          Map(folderA.toURI -> Signature.NoSignature,
            folderB.toURI -> SimpleSignature(pairDSA1.getPublic(), pairDSA1.getPrivate()),
            folderC.toURI -> SimpleSignature(pairDSA1.getPublic(), pairDSA1.getPrivate())))

      info("There are no retrospective records")
      Signature.history(graph) should be(empty)

      Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
      val graphLoaderWith1Record = Serialization.acquireLoader(folderA.toURI(),
        SData(Signature.Key.acquire -> Signature.acceptAll))
      val graphWith1Record = graphLoaderWith1Record.load()

      info("There is a single retrospective record")
      val s1l = Signature.history(graphLoaderWith1Record)
      val s1g = Signature.history(graph)
      s1l should be(s1g)
      s1l.size should be(1)
      s1l.head._2.size should be(3)

      model.takeRecord('baseLevel) { r ⇒ r.name = "222" }
      val keyGenDSA2 = KeyPairGenerator.getInstance("DSA")
      keyGenDSA2.initialize(1024)
      val pairDSA2 = keyGenDSA2.genKeyPair()
      Serialization.freeze(graph, SData(Signature.Key.freeze ->
        Map(folderA.toURI -> Signature.NoSignature,
          folderB.toURI -> SimpleSignature(pairDSA2.getPublic(), pairDSA2.getPrivate()),
          folderC.toURI -> SimpleSignature(pairDSA2.getPublic(), pairDSA2.getPrivate()))))
      val graphLoaderWith2Records = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> Signature.acceptAll))

      info("There are two retrospective records")
      val s2l = Signature.history(graphLoaderWith2Records)
      val s2g = Signature.history(graph)
      s2l should be(s2g)
      s2l.size should be(2)
      s2l.head._2.size should be(3)
      val s2keys = s2l.keys.toSeq
      s2l(s2keys.head) should not be (s2l(s2keys.last))

      model.takeRecord('baseLevel) { r ⇒ r.name = "333" }
      val keyGenRSA3 = KeyPairGenerator.getInstance("RSA")
      keyGenRSA3.initialize(1024)
      val pairRSA3 = keyGenRSA3.genKeyPair()
      Serialization.freeze(graph, SData(Signature.Key.freeze ->
        Map(folderA.toURI -> Signature.NoSignature,
          folderB.toURI -> SimpleSignature(pairRSA3.getPublic(), pairRSA3.getPrivate()),
          folderC.toURI -> SimpleSignature(pairRSA3.getPublic(), pairRSA3.getPrivate()))))
      val graphLoaderWith3Records = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> Signature.acceptAll))

      info("There are three retrospective records")
      val s3l = Signature.history(graphLoaderWith3Records)
      val s3g = Signature.history(graph)
      s3l should be(s3g)
      s3l.size should be(3)
      s3l.map(_._2.size).toSeq should be(Seq(3, 3, 3))
      val s3keys = s3l.keys.toSeq.sorted
      s3keys.last should be(graph.modified)

      s3l(s3keys(0)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "DSA", "C/" -> "DSA"))
      s3l(s3keys(1)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "DSA", "C/" -> "DSA"))
      s3l(s3keys(2)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "RSA", "C/" -> "RSA"))

      s3l(s3keys(0)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(),
          try Option(b.publicKey) catch { case e: Throwable ⇒ None })
      } should be(Map("A/" -> None, "B/" -> Some(pairDSA1.getPublic()), "C/" -> Some(pairDSA1.getPublic())))
      s3l(s3keys(1)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(),
          try Option(b.publicKey) catch { case e: Throwable ⇒ None })
      } should be(Map("A/" -> None, "B/" -> Some(pairDSA2.getPublic()), "C/" -> Some(pairDSA2.getPublic())))
      s3l(s3keys(2)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(),
          try Option(b.publicKey) catch { case e: Throwable ⇒ None })
      } should be(Map("A/" -> None, "B/" -> Some(pairRSA3.getPublic()), "C/" -> Some(pairRSA3.getPublic())))
    }
  }
  "Signature history should be projection of Graph.Retrospective: everything OK, but only signed loaded" in {
    withTempFolder { folder ⇒
      import TestDSL._

      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel1) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val keyGenDSA1 = KeyPairGenerator.getInstance("DSA")
      keyGenDSA1.initialize(1024)
      val pairDSA1 = keyGenDSA1.genKeyPair()
      val sDataFreeze = SData(Digest.Key.freeze ->
        Map(folderA.toURI -> SimpleDigest("MD2"), folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")),
        Signature.Key.freeze ->
          Map(folderA.toURI -> Signature.NoSignature,
            folderB.toURI -> SimpleSignature(pairDSA1.getPublic(), pairDSA1.getPrivate()),
            folderC.toURI -> SimpleSignature(pairDSA1.getPublic(), pairDSA1.getPrivate())))

      info("There are no retrospective records")
      Signature.history(graph) should be(empty)

      Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())

      info("There is a single retrospective record")

      val firstAcceptSignedLoader = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> Signature.acceptSigned))
      Mockito.reset(testTransport)
      val firstAcceptSignedLoaderGraph = firstAcceptSignedLoader.load()
      Mockito.verify(testTransport, Mockito.times(7)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderA.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())
      Mockito.verify(testTransport, Mockito.times(0)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderB.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())
      Mockito.verify(testTransport, Mockito.times(9)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())

      val graphLoaderWith1Record = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> Signature.acceptAll))
      val graphWith1Record = graphLoaderWith1Record.load()
      val s1l = Signature.history(graphLoaderWith1Record)
      val s1g = Signature.history(graph)
      s1l should be(s1g)
      s1l.size should be(1)
      s1l.head._2.size should be(3)

      val firstAcceptAllLoader = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> Signature.acceptAll))
      Mockito.reset(testTransport)
      val firstAcceptAllLoaderGraph = firstAcceptAllLoader.load()
      Mockito.verify(testTransport, Mockito.times(9)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderA.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())
      Mockito.verify(testTransport, Mockito.times(0)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderB.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())
      Mockito.verify(testTransport, Mockito.times(0)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())

      model.takeRecord('baseLevel2) { r ⇒ r.name = "222" }
      val keyGenDSA2 = KeyPairGenerator.getInstance("DSA")
      keyGenDSA2.initialize(1024)
      val pairDSA2 = keyGenDSA2.genKeyPair()
      Serialization.freeze(graph, SData(Signature.Key.freeze ->
        Map(folderA.toURI -> Signature.NoSignature,
          folderB.toURI -> Signature.NoSignature,
          folderC.toURI -> SimpleSignature(pairDSA2.getPublic(), pairDSA2.getPrivate()))))
      val graphLoaderWith2Records = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> Signature.acceptAll))

      info("There are two retrospective records")
      val s2l = Signature.history(graphLoaderWith2Records)
      val s2g = Signature.history(graph)
      s2l should be(s2g)
      s2l.size should be(2)
      s2l.head._2.size should be(3)
      val s2keys = s2l.keys.toSeq
      s2l(s2keys.head) should not be (s2l(s2keys.last))

      model.takeRecord('baseLevel3) { r ⇒ r.name = "333" }
      val keyGenRSA3 = KeyPairGenerator.getInstance("RSA")
      keyGenRSA3.initialize(1024)
      val pairRSA3 = keyGenRSA3.genKeyPair()
      Serialization.freeze(graph, SData(Signature.Key.freeze ->
        Map(folderA.toURI -> Signature.NoSignature,
          folderB.toURI -> SimpleSignature(pairRSA3.getPublic(), pairRSA3.getPrivate()),
          folderC.toURI -> Signature.NoSignature)))
      val graphLoaderWith3Records = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> Signature.acceptSigned))
      // Serialization.scala L124
      graphLoaderWith3Records.sData(Digest.Key.acquire) should be(true)
      graphLoaderWith3Records.sources should have size (3)
      graphLoaderWith3Records.sData(Signature.historyPerURI) should have size (3)
      graphLoaderWith3Records.sData(Signature.historyPerURI).values.map(_.size).toSeq should be(Seq(3, 3, 3))

      info("There are three retrospective records")
      val s3l = Signature.history(graphLoaderWith3Records)
      val s3g = Signature.history(graph)
      s3l should be(s3g)
      s3l.size should be(3)
      s3l.map(_._2.size).toSeq should be(Seq(3, 3, 3))
      val s3keys = s3l.keys.toSeq.sorted
      s3keys.last should be(graph.modified)

      s3l(s3keys(0)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "DSA", "C/" -> "DSA"))
      s3l(s3keys(1)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "", "C/" -> "DSA"))
      s3l(s3keys(2)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "RSA", "C/" -> ""))

      s3l(s3keys(0)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(),
          try Option(b.publicKey) catch { case e: Throwable ⇒ None })
      } should be(Map("A/" -> None, "B/" -> Some(pairDSA1.getPublic()), "C/" -> Some(pairDSA1.getPublic())))
      s3l(s3keys(1)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(),
          try Option(b.publicKey) catch { case e: Throwable ⇒ None })
      } should be(Map("A/" -> None, "B/" -> None, "C/" -> Some(pairDSA2.getPublic())))
      s3l(s3keys(2)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(),
          try Option(b.publicKey) catch { case e: Throwable ⇒ None })
      } should be(Map("A/" -> None, "B/" -> Some(pairRSA3.getPublic()), "C/" -> None))

      val graphLoaderNone = Serialization.acquireLoader(folderA.toURI(),
        SData(Signature.Key.acquire -> ((_: Option[PublicKey]) ⇒ false)))
      graphLoaderNone.sData(Digest.Key.acquire) should be(true)

      val graphLoaderAll = Serialization.acquireLoader(folderA.toURI(),
        SData(Signature.Key.acquire -> Signature.acceptAll))
      graphLoaderAll.sData(Digest.Key.acquire) should be(true)
      val sHAll = Signature.history(graphLoaderAll)
      val sKeysAll = sHAll.keys.toSeq.sorted
      sHAll(sKeysAll(0)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "DSA", "C/" -> "DSA"))
      sHAll(sKeysAll(1)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "", "C/" -> "DSA"))
      sHAll(sKeysAll(2)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "RSA", "C/" -> ""))
      sHAll should have size (3)
      val dHAll = Digest.history(graphLoaderAll)
      val dKeysAll = sHAll.keys.toSeq.sorted
      dHAll(dKeysAll(0)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "MD2", "B/" -> "MD5", "C/" -> "SHA-512"))
      dHAll(dKeysAll(1)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "MD2", "B/" -> "MD5", "C/" -> "SHA-512"))
      dHAll(dKeysAll(2)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "MD2", "B/" -> "MD5", "C/" -> "SHA-512"))
      dHAll should have size (3)
      Mockito.reset(testTransport)
      /*   // 1. restore retrospective.
       *   Serialization -> acquireGraph
       * 1 .../retrospective/145747327E5ms.1C7618E3Cns-resources.yaml
       *   Serialization -> acquireGraph
       * 2 .../retrospective/14574730AB3ms.9E5E5B0ns-record.yaml
       * 3 .../retrospective/145747326F9ms.1B951D849ns-record.yaml
       * 4 .../retrospective/145747327E5ms.1C7618E3Cns-record.yaml
       *   // 2. setup projections.
       * 5 .../data/box%2058F071D85E644124-A7C8824733C50F53-14574730A9Cms.8904272ns/box%20descriptor.yaml
       * 6 .../data/e%20john1%20%7B0609C486%7D/e%20baseLevel1%20%7BC27EA75E%7D/node%20descriptor-14574730AB3ms.9E5E5B0ns.yaml
       *   // 3. add children.
       *   Serialization -> acquireGraphLoader -> acquireNode
       * 7 .../data/e%20john1%20%7B0609C486%7D/box%209AF11A6B8D174336-B8E78A0582F45E1C-14574730AABms.97836A1ns/box%20descriptor.yaml
       * 8 .../data/e%20john1%20%7B0609C486%7D/e%20baseLevel1%20%7BC27EA75E%7D/e%20level1a%20%7B0428D0D4%7D/node%20descriptor-14574730AB3ms.9E5E5B0ns.yaml
       * 9 .../data/e%20john1%20%7B0609C486%7D/e%20baseLevel1%20%7BC27EA75E%7D/box%20CF28D5545ABD42D0-900AC5294DB25D81-14574730AB0ms.9BE57FFns/box%20descriptor.yaml
       * 10 .../data/e%20john1%20%7B0609C486%7D/e%20baseLevel1%20%7BC27EA75E%7D/e%20level1a%20%7B0428D0D4%7D/e%20level2a%20%7B0428D0F3%7D/node%20descriptor-14574730AB3ms.9E5E5B0ns.yaml
       * 11 .../data/e%20john1%20%7B0609C486%7D/e%20baseLevel1%20%7BC27EA75E%7D/e%20level1a%20%7B0428D0D4%7D/box%205FA1ED20CA25435F-AED0C78653D48AE5-14574730AB3ms.9E5E5B0ns/box%20descriptor.yaml
       * 12 .../data/e%20john1%20%7B0609C486%7D/e%20baseLevel2%20%7BC27EA75F%7D/node%20descriptor-145747326F9ms.1B951D849ns.yaml
       * 13 .../data/e%20john1%20%7B0609C486%7D/box%20FA95B6F1CCDF4B64-AB8F745CD3423213-145747326F9ms.1B951D849ns/box%20descriptor.yaml
       * 14 .../data/e%20john1%20%7B0609C486%7D/e%20baseLevel3%20%7BC27EA760%7D/node%20descriptor-145747327E5ms.1C7618E3Cns.yaml
       * 15 .../data/e%20john1%20%7B0609C486%7D/box%20ABC64E8EFBFD471F-A3D9C2CF89F6CE4A-145747327E5ms.1C7618E3Cns/box%20descriptor.yaml
       */
      val graphAll = graphLoaderAll.load()
      Mockito.verify(testTransport, Mockito.times(15)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderA.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())
      Mockito.verify(testTransport, Mockito.times(0)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderB.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())
      Mockito.verify(testTransport, Mockito.times(0)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())
      graphAll.node.safeRead { node ⇒
        graph.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)

      val graphLoaderSigned = Serialization.acquireLoader(folderA.toURI(),
        SData(Signature.Key.acquire -> Signature.acceptSigned))
      graphLoaderSigned.sData(Digest.Key.acquire) should be(true)
      val sHSigned = Signature.history(graphLoaderSigned)
      val sKeysSigned = sHSigned.keys.toSeq.sorted
      sHSigned(sKeysSigned(0)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "DSA", "C/" -> "DSA"))
      sHSigned(sKeysSigned(1)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "", "C/" -> "DSA"))
      sHSigned(sKeysSigned(2)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "RSA", "C/" -> ""))
      sHSigned should have size (3)
      val dHSigned = Digest.history(graphLoaderSigned)
      val dKeysSigned = sHSigned.keys.toSeq.sorted
      dHSigned(dKeysSigned(0)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "MD2", "B/" -> "MD5", "C/" -> "SHA-512"))
      dHSigned(dKeysSigned(1)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "MD2", "B/" -> "MD5", "C/" -> "SHA-512"))
      dHSigned(dKeysSigned(2)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "MD2", "B/" -> "MD5", "C/" -> "SHA-512"))
      dHSigned should have size (3)
      Mockito.reset(testTransport)
      val graphSigned = graphLoaderSigned.load()
      Mockito.verify(testTransport, Mockito.times(12)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderA.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())
      Mockito.verify(testTransport, Mockito.times(7)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderB.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())
      Mockito.verify(testTransport, Mockito.times(12)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())
      graphSigned.node.safeRead { node ⇒
        graph.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)
    }
  }
  "Signature history should be projection of Graph.Retrospective: something wrong" in {
    withTempFolder { folder ⇒
      import TestDSL._

      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      assert(true)
    }
  }
  "Signature history should be projection of Graph.Retrospective: mostly damaged" in {
    withTempFolder { folder ⇒
      import TestDSL._

      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      assert(true)
    }
  }
  "Signature history should be projection of Graph.Retrospective: everything failed" in {
    withTempFolder { folder ⇒
      import TestDSL._

      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      assert(true)
    }
  }

  override def beforeAll(configMap: org.scalatest.ConfigMap) {
    adjustLoggingBeforeAll(configMap)
    //addFileAppender()
  }

  class TestSimple extends SimpleSignature {
    override def getSignatureMap(context: AtomicReference[SoftReference[AnyRef]],
      modified: Element.Timestamp, transport: Transport,
      publicKey: PublicKey, sAlgorithm: String, sProvider: Option[String],
      sData: SData): Map[URI, Array[Byte]] =
      super.getSignatureMap(context, modified, transport, publicKey, sAlgorithm, sProvider, sData)
    override def checkSignature(publicKey: PublicKey, sAlgorithm: String, sProvider: Option[String],
      verifier: java.security.Signature, context: AtomicReference[SoftReference[AnyRef]],
      modified: Element.Timestamp, uri: URI, transport: Transport, sData: SData) =
      super.checkSignature(publicKey, sAlgorithm, sProvider, verifier, context, modified, uri, transport, sData)
  }
}
