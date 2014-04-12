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
import java.io.{ BufferedInputStream, BufferedOutputStream, BufferedReader, File, FileInputStream, FileOutputStream, InputStreamReader, PrintStream }
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
import org.digimead.tabuddy.model.serialization.transport.{ Local, Transport }
import org.digimead.tabuddy.model.serialization.yaml.Timestamp
import org.digimead.tabuddy.model.serialization.{ SData, Serialization, YAMLSerialization }
import org.mockito.Mockito
import org.scalatest.{ FreeSpec, Matchers }
import scala.collection.immutable
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
  "Signature should generated for every graph digest" taggedAs (TestDSL.Mark) in {
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
      val sDataFreeze = SData(Signature.Key.freeze -> immutable.Map(folderA.toURI -> Signature.NoSignature,
        folderB.toURI -> SimpleSignature(pairRSA.getPublic(), pairRSA.getPrivate()),
        folderC.toURI -> SimpleSignature(pairDSA.getPublic(), pairDSA.getPrivate())),
        Digest.Key.freeze -> immutable.Map(folderA.toURI -> Digest.NoDigest,
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
      scala.io.Source.fromFile(fileBSignatureData).getLines.size should be(1)
      folderC should be('exists)
      fileCSignatureType should be('exists)
      fileCSignatureType.length() should be(683)
      val fileCSignatureTypeLines = scala.io.Source.fromFile(fileCSignatureType).getLines.toList
      fileCSignatureTypeLines.take(5) should be(Seq("simple", "", "DSA", "", "-----BEGIN PUBLIC KEY-----"))
      fileCSignatureTypeLines.takeRight(5) should be(Seq("-----END PUBLIC KEY-----", "", "SHA1withDSA", "", "SUN"))
      fileCSignatureData should be('exists)
      scala.io.Source.fromFile(fileCSignatureData).getLines.size should be(1)

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

      val signatureAcquire = (uri: URI, modified: Element.Timestamp, validation: Option[(PublicKey, Boolean)], sData: SData) ⇒
        validation match {
          case Some((publicKey, true)) if publicKey.getAlgorithm == "RSA" ⇒
            publicKey should be(pairRSA.getPublic())
            info("RSA validation passed for " + uri)
          case Some((publicKey, true)) if publicKey.getAlgorithm == "DSA" ⇒
            publicKey should be(pairDSA.getPublic())
            info("DSA validation passed for " + uri)
          case unexpected ⇒
            fail("Unexpected: " + unexpected)
        }
      val graph2 = Serialization.acquire(folderB.getAbsoluteFile().toURI(),
        SData(Signature.Key.acquire -> signatureAcquire, Digest.Key.acquire -> true))

      val keyGenRSA2 = KeyPairGenerator.getInstance("RSA")
      keyGenRSA.initialize(1024)
      val pairRSA2 = keyGenRSA.genKeyPair()
      graph2.model.takeRecord('baseLevel2) { r ⇒ r.name = "234" }
      pairRSA2.getPublic() should not be (pairRSA.getPublic())

      val sDataFreeze2 = SData(Signature.Key.freeze -> immutable.Map(folderA.toURI -> Signature.NoSignature,
        folderB.toURI -> SimpleSignature(pairRSA2.getPublic(), pairRSA2.getPrivate()),
        folderC.toURI -> SimpleSignature(pairRSA2.getPublic(), pairRSA2.getPrivate())))
      Serialization.freeze(graph2, sDataFreeze2)

      var keys = Seq.empty[(URI, PublicKey)]
      val signatureAcquire2 = (uri: URI, modified: Element.Timestamp, validation: Option[(PublicKey, Boolean)], sData: SData) ⇒
        validation match {
          case Some((publicKey, true)) if publicKey.getAlgorithm == "RSA" ⇒
            info("RSA validation passed for " + uri)
            keys = keys :+ (uri -> publicKey)
          case Some((publicKey, true)) if publicKey.getAlgorithm == "DSA" ⇒
            info("DSA validation passed for " + uri)
            keys = keys :+ (uri -> publicKey)
          case unexpected ⇒
            fail("Unexpected: " + unexpected)
        }
      val graph3 = Serialization.acquire(folderB.getAbsoluteFile().toURI(),
        SData(Signature.Key.acquire -> signatureAcquire2, Digest.Key.acquire -> true))
      keys.map { case (k, v) ⇒ (folder.toURI().relativize(k).toString().substring(0, 1), v) }.toList should be(List(("B", pairRSA2.getPublic()),
        ("C", pairRSA2.getPublic()), ("B", pairRSA.getPublic()), ("B", pairRSA.getPublic())))

      graph3.node.safeRead { node ⇒
        graph2.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)
    }
  }

  override def beforeAll(configMap: org.scalatest.ConfigMap) {
    adjustLoggingBeforeAll(configMap)
    addFileAppender()
  }

  class TestSimple extends SimpleSignature
}
