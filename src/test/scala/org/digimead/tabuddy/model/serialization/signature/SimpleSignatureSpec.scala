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
import org.hamcrest.{ BaseMatcher, Description }
import org.mockito.{ ArgumentCaptor, Matchers ⇒ MM, Mockito }
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
            if (publicKey == pairRSA.getPublic())
              info("RSA validation passed for " + uri)
            else {
              info("RSA validation failed for " + uri)
              throw new SecurityException
            }
          case Some((publicKey, true)) if publicKey.getAlgorithm == "DSA" ⇒
            if (publicKey == pairDSA.getPublic())
              info("DSA validation passed for " + uri)
            else {
              info("DSA validation failed for " + uri)
              throw new SecurityException
            }
          case unexpected ⇒
            info("Unexpected: " + unexpected)
            throw new SecurityException
        }

      an[IllegalArgumentException] should be thrownBy Serialization.acquire(folderB.getAbsoluteFile().toURI(),
        SData(Signature.Key.acquire -> signatureAcquire, Digest.Key.acquire -> false))

      val graph2 = Serialization.acquire(folderB.getAbsoluteFile().toURI(),
        SData(Signature.Key.acquire -> signatureAcquire))

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
            keys = keys :+ (sData(SData.Key.storageURI) -> publicKey)
          case Some((publicKey, true)) if publicKey.getAlgorithm == "DSA" ⇒
            info("DSA validation passed for " + uri)
            keys = keys :+ (sData(SData.Key.storageURI) -> publicKey)
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
        immutable.Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")),
        Signature.Key.freeze ->
          immutable.Map(folderA.toURI -> Signature.NoSignature,
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
        immutable.Map(folderA.toURI -> Signature.NoSignature,
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
        immutable.Map(folderA.toURI -> Signature.NoSignature,
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
  "Signature history should be projection of Graph.Retrospective: everything OK, but only signed loaded" taggedAs (TestDSL.Mark) in {
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
        immutable.Map(folderA.toURI -> SimpleDigest("MD2"), folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")),
        Signature.Key.freeze ->
          immutable.Map(folderA.toURI -> Signature.NoSignature,
            folderB.toURI -> SimpleSignature(pairDSA1.getPublic(), pairDSA1.getPrivate()),
            folderC.toURI -> SimpleSignature(pairDSA1.getPublic(), pairDSA1.getPrivate())))

      info("There are no retrospective records")
      Signature.history(graph) should be(empty)

      Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
      val graphLoaderWith1Record = Serialization.acquireLoader(folderA.toURI(), SData(Signature.Key.acquire -> Signature.acceptAll))
      val graphWith1Record = graphLoaderWith1Record.load()

      info("There is a single retrospective record")
      val s1l = Signature.history(graphLoaderWith1Record)
      val s1g = Signature.history(graph)
      s1l should be(s1g)
      s1l.size should be(1)
      s1l.head._2.size should be(3)

      model.takeRecord('baseLevel2) { r ⇒ r.name = "222" }
      val keyGenDSA2 = KeyPairGenerator.getInstance("DSA")
      keyGenDSA2.initialize(1024)
      val pairDSA2 = keyGenDSA2.genKeyPair()
      Serialization.freeze(graph, SData(Signature.Key.freeze ->
        immutable.Map(folderA.toURI -> Signature.NoSignature,
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
        immutable.Map(folderA.toURI -> Signature.NoSignature,
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

      val graphLoaderAll = Serialization.acquireLoader(folderA.toURI(),
        SData(Signature.Key.acquire -> Signature.acceptAll))
      graphLoaderAll.sData(Digest.Key.acquire) should be(true)
      val graphLoaderSigned = Serialization.acquireLoader(folderA.toURI(),
        SData(Signature.Key.acquire -> Signature.acceptSigned))
      graphLoaderSigned.sData(Digest.Key.acquire) should be(true)
      val graphLoaderNone = Serialization.acquireLoader(folderA.toURI(),
        SData(Signature.Key.acquire ->
          ((uri: URI, _: Element.Timestamp, v: Option[(PublicKey, Boolean)], _: SData) ⇒ throw new SecurityException)))
      graphLoaderNone.sData(Digest.Key.acquire) should be(true)

      Mockito.reset(testTransport)
      val inOrderAll = Mockito.inOrder(testTransport)
      log.___glance("AAAAAAALLLLLLLLLLLLLLLLLLLL")
      val graphAll = graphLoaderAll.load()
      inOrderAll.verify(testTransport, Mockito.times(15)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderA.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())
      inOrderAll.verify(testTransport, Mockito.times(0)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderB.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())
      inOrderAll.verify(testTransport, Mockito.times(0)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())
      graphAll.node.safeRead { node ⇒
        graph.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)

      Mockito.reset(testTransport)
      val inOrderSigned = Mockito.inOrder(testTransport)
      log.___glance("SIGNNNNED")
      val graphSigned = graphLoaderSigned.load()
      inOrderSigned.verify(testTransport, Mockito.times(11)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderA.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())
      inOrderSigned.verify(testTransport, Mockito.times(1)).read(MM.argThat(new BaseMatcher {
        def matches(uri: Any): Boolean = uri.toString().startsWith(folderB.toURI().toString())
        def describeTo(description: Description) {}
      }), MM.any())
      inOrderSigned.verify(testTransport, Mockito.times(0)).read(MM.argThat(new BaseMatcher {
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

  class TestSimple extends SimpleSignature
}
