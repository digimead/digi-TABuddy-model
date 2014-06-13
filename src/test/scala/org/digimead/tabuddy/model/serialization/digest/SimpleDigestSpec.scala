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

package org.digimead.tabuddy.model.serialization.digest

import com.escalatesoft.subcut.inject.NewBindingModule
import java.io.{ BufferedOutputStream, File, FileOutputStream, FilterInputStream, FilterOutputStream, InputStream, OutputStream, PrintStream, RandomAccessFile }
import java.net.URI
import java.util.UUID
import java.util.concurrent.atomic.AtomicReference
import org.apache.log4j.Level
import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.{ LoggingHelper, StorageHelper }
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.TestDSL
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.SData
import org.digimead.tabuddy.model.serialization.transport.{ Local, Transport }
import org.digimead.tabuddy.model.serialization.yaml.Timestamp
import org.digimead.tabuddy.model.serialization.{ Serialization, YAMLSerialization }
import org.hamcrest.{ BaseMatcher, Description }
import org.mockito.{ ArgumentCaptor, Matchers ⇒ MM, Mockito }
import org.scalatest.{ FreeSpec, Matchers }
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.{ immutable, mutable }
import scala.ref.SoftReference

class SimpleDigestSpec extends FreeSpec with Matchers with StorageHelper with LoggingHelper with Loggable {
  lazy val testDigest = Mockito.spy(new TestSimple)
  lazy val testTransport = Mockito.spy(new Local)

  /** Log test. */
  val logTest: ArgumentCaptor[org.apache.log4j.spi.LoggingEvent] ⇒ Unit = {
    logCaptor ⇒
      logCaptor.getAllValues().asScala.find { event ⇒
        val level = event.getLevel()
        event.getMessage() != null &&
          (level == Level.WARN || level == Level.ERROR || level == Level.FATAL)
      }.map { event ⇒
        fail(s"Unexpected log message detected. ${event.getLevel()}: ${event.getMessage()}" +
          Option(event.getThrowableStrRep()).map(_.mkString("\n")))
      }
  }

  before {
    DependencyInjection(new NewBindingModule(module ⇒ {
      module.bind[Mechanism] identifiedBy ("Digest.Mechanism.SimpleDigest") toSingle { testDigest }
      module.bind[Transport] identifiedBy ("Serialization.Transport.Local") toSingle { testTransport }
    }) ~ org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
  }

  "Complex test for Raw, MD5, SHA512 digest" in {
    implicit val option = Mockito.atLeast(1)
    withMockitoLogCaptor {
      withTempFolder { folder ⇒
        import TestDSL._

        // graph
        val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        model.takeRecord('baseLevel) { r ⇒
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
        }

        val modification = Timestamp.dump(graph.modified)
        val folderA = new File(folder, "A")
        val fileADigestType = new File(folderA, s"digest/${modification}/type")
        val fileADigestData = new File(folderA, s"digest/${modification}/digest")
        val folderB = new File(folder, "B")
        val fileBDigestType = new File(folderB, s"digest/${modification}/type")
        val fileBDigestData = new File(folderB, s"digest/${modification}/digest")
        val folderC = new File(folder, "C")
        val fileCDigestType = new File(folderC, s"digest/${modification}/type")
        val fileCDigestData = new File(folderC, s"digest/${modification}/digest")
        folderA should not be ('exists)
        folderB should not be ('exists)
        folderC should not be ('exists)

        for (i ← 0 until 3) {
          info("Iteration " + i)
          val sDataFreeze = SData(Digest.Key.freeze ->
            Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")))
          Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
          folderA should be('exists)
          fileADigestType should not be ('exists)
          fileADigestData should not be ('exists)
          folderB should be('exists)
          fileBDigestType should be('exists)
          fileBDigestType.length() should be(12)
          scala.io.Source.fromFile(fileBDigestType).getLines.toList should be(Seq("simple", "", "MD5"))
          fileBDigestData should be('exists)
          scala.io.Source.fromFile(fileBDigestData).getLines.size should be(27)
          folderC should be('exists)
          fileCDigestType should be('exists)
          fileCDigestType.length() should be(16)
          scala.io.Source.fromFile(fileCDigestType).getLines.toList should be(Seq("simple", "", "SHA-512"))
          fileCDigestData should be('exists)
          scala.io.Source.fromFile(fileCDigestData).getLines.size should be(27)

          val graphWithoutDigest = Serialization.acquire(folderB.getAbsoluteFile().toURI())
          graph.retrospective should be(graphWithoutDigest.retrospective)
          graphWithoutDigest.node.safeRead { node ⇒
            graph.node.safeRead { node2 ⇒
              node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
            }
          } should be(true)

          val sDataAcquire = SData(Digest.Key.acquire -> false)

          Mockito.reset(testDigest)
          Mockito.reset(testTransport)
          val inOrder = Mockito.inOrder(testDigest, testTransport)
          val graphWithDigestLoader = Serialization.acquireLoader(folderC.getAbsoluteFile().toURI(), sDataAcquire)
          // Bootstrap loader:
          // 1st - read C/descriptor.yaml
          // 2nd - read -record.yaml
          // 3rd - read -resources.yaml
          inOrder.verify(testTransport, Mockito.times(3)).read(MM.argThat(new BaseMatcher {
            def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
            def describeTo(description: Description) {}
          }), MM.argThat(new BaseMatcher {
            def matches(sData: Any): Boolean =
              !sData.asInstanceOf[SData].isDefinedAt(Digest.historyPerURI) ||
                sData.asInstanceOf[SData](Digest.historyPerURI).isEmpty
            def describeTo(description: Description) {}
          }))
          // and create Source for folderA
          // 1st - read A/descriptor.yaml
          // 2nd - read A/.../node%20descriptor-NNNms.NNNns.yaml (graph root node )
          inOrder.verify(testTransport, Mockito.times(2)).read(MM.argThat(new BaseMatcher {
            def matches(uri: Any): Boolean = uri.toString().startsWith(folderA.toURI().toString())
            def describeTo(description: Description) {}
          }), MM.anyObject())
          // and create Source for folderB
          // 1st - read B/descriptor.yaml
          // 2nd - read B/.../node%20descriptor-NNNms.NNNns.yaml (graph root node )
          // 2 files with digest from storage B
          inOrder.verify(testTransport).read(MM.argThat(new BaseMatcher {
            def matches(uri: Any): Boolean = uri.toString().startsWith(folderB.toURI().toString())
            def describeTo(description: Description) {}
          }), MM.anyObject())
          //          inOrder.verify(testDigest).approve(MM.argThat(new BaseMatcher {
          //            def matches(uri: Any): Boolean = uri.toString().startsWith(folderB.toURI().toString())
          //            def describeTo(description: Description) {}
          //          }), MM.anyObject())
          inOrder.verify(testTransport).read(MM.argThat(new BaseMatcher {
            def matches(uri: Any): Boolean = uri.toString().startsWith(folderB.toURI().toString())
            def describeTo(description: Description) {}
          }), MM.anyObject())
          //          inOrder.verify(testDigest).approve(MM.argThat(new BaseMatcher {
          //            def matches(uri: Any): Boolean = uri.toString().startsWith(folderB.toURI().toString())
          //            def describeTo(description: Description) {}
          //          }), MM.anyObject())
          // and create Source for folderC
          // 1st - read C/descriptor.yaml
          // 2nd - read C/.../node%20descriptor-NNNms.NNNns.yaml (graph root node )
          // 2 files with digest from storage C
          inOrder.verify(testTransport, Mockito.times(1)).read(MM.argThat(new BaseMatcher {
            def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
            def describeTo(description: Description) {}
          }), MM.anyObject())
          //          inOrder.verify(testDigest, Mockito.times(1)).approve(MM.argThat(new BaseMatcher {
          //            def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
          //            def describeTo(description: Description) {}
          //          }), MM.anyObject())
          inOrder.verify(testTransport, Mockito.times(1)).read(MM.argThat(new BaseMatcher {
            def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
            def describeTo(description: Description) {}
          }), MM.anyObject())
          //          inOrder.verify(testDigest, Mockito.times(1)).approve(MM.argThat(new BaseMatcher {
          //            def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
          //            def describeTo(description: Description) {}
          //          }), MM.anyObject())
          // So there were 4 files with digest at all
          //          inOrder.verify(testDigest, Mockito.never()).approve(MM.anyObject(), MM.anyObject())
          inOrder.verify(testTransport, Mockito.never()).read(MM.anyObject(), MM.anyObject())

          Mockito.reset(testDigest)
          Mockito.reset(testTransport)
          val graphWithDigest = graphWithDigestLoader.load()
          //          Mockito.verify(testDigest, Mockito.times(13)).approve(MM.argThat(new BaseMatcher {
          //            def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
          //            def describeTo(description: Description) {}
          //          }), MM.anyObject())
          //          Mockito.verify(testDigest, Mockito.times(13)).approve(MM.argThat(new BaseMatcher {
          //            def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
          //            def describeTo(description: Description) {}
          //          }), MM.anyObject())
          //          Mockito.verify(testDigest, Mockito.times(0)).approve(MM.argThat(new BaseMatcher {
          //            def matches(uri: Any): Boolean = !uri.toString().startsWith(folderC.toURI().toString())
          //            def describeTo(description: Description) {}
          //          }), MM.anyObject())

          graph.retrospective should be(graphWithDigest.retrospective)
          graphWithDigest.node.safeRead { node ⇒
            graph.node.safeRead { node2 ⇒
              node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
            }
          } should be(true)

          deleteFolder(folderA)
          deleteFolder(folderB)
          deleteFolder(folderC)
        }
      }
    }(logTest)
  }
  "SimpleDigest should (de)serialize mechanism parameters" in {
    val orig = SimpleDigest("MD5")
    val copy = Digest.perIdentifier(orig.mechanism.identifier)(orig.algorithm, orig.arguments: _*)
    copy should be(orig)
  }
  "Test Graph.Loader creation for graph with 1 source" in {
    implicit val option = Mockito.atLeast(1)
    withMockitoLogCaptor {
      withTempFolder { folder ⇒
        import TestDSL._

        val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }

        val modification = Timestamp.dump(graph.modified)
        val folderA = new File(folder, "A")
        val fileADigestType = new File(folderA, s"digest/${modification}/type")
        val fileADigestData = new File(folderA, s"digest/${modification}/digest")
        Serialization.freeze(graph, SData(Digest.Key.freeze -> Map()), folderA.toURI())
        fileADigestType should be('exists)
        fileADigestData should be('exists)

        val graphWithDigestLoader1 = Serialization.acquireLoader(folderA.getAbsoluteFile().toURI())
        graphWithDigestLoader1.sources should have size (1)
        graphWithDigestLoader1.sData.get(Digest.historyPerURI) should be('empty)
        val graphWithDigestLoader2 = Serialization.acquireLoader(folderA.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))
        graphWithDigestLoader2.sources should have size (1)
        graphWithDigestLoader2.sData.get(Digest.historyPerURI) should not be ('empty)
        graphWithDigestLoader2.sData(Digest.historyPerURI).values.flatMap(_.values.map(_._2.get.get)).forall(_.isEmpty) should be(true)
        val graphWithDigestLoader3 = Serialization.acquireLoader(folderA.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> true))
        graphWithDigestLoader3.sources should have size (1)
        graphWithDigestLoader3.sData.get(Digest.historyPerURI) should not be ('empty)
        graphWithDigestLoader3.sData(Digest.historyPerURI).values.flatMap(_.values.map(_._2.get.get)).forall(_.isEmpty) should be(true)
      }
    }({
      logCaptor ⇒
        logCaptor.getAllValues().asScala.find { event ⇒
          val level = event.getLevel()
          event.getMessage() != null && (level == Level.WARN || level == Level.ERROR || level == Level.FATAL)
        }.map { event ⇒
          fail(s"Unexpected log message detected. ${event.getLevel()}: ${event.getMessage()}" +
            Option(event.getThrowableStrRep()).map(_.mkString("\n")))
        }
    })
  }
  "Test Graph.Loader creation for graph with 1 source and modified descriptor.yaml" in {
    implicit val option = Mockito.atLeast(1)
    withMockitoLogCaptor {
      withTempFolder { folder ⇒
        import TestDSL._

        val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }

        val modification = Timestamp.dump(graph.modified)
        val folderA = new File(folder, "A")
        val fileADigestType = new File(folderA, s"digest/${modification}/type")
        val fileADigestData = new File(folderA, s"digest/${modification}/digest")
        Serialization.freeze(graph, SData(Digest.Key.freeze -> Map()), folderA.toURI())
        fileADigestType should be('exists)
        fileADigestData should be('exists)

        val fileToModify = new File(folderA, s"descriptor.yaml")
        val file = new RandomAccessFile(fileToModify, "rws")
        val text = new Array[Byte](fileToModify.length().toInt)
        file.readFully(text)
        file.seek(0)
        file.write(new String(text, io.Codec.UTF8.charSet).replaceAll("created: .*ns", "created: 10000000000ms.10000000ns").getBytes(io.Codec.UTF8.charSet))
        file.close()

        val graphWithDigestLoader1 = Serialization.acquireLoader(folderA.getAbsoluteFile().toURI())
        graphWithDigestLoader1.sources should have size (1)
        val graph1 = graphWithDigestLoader1.load()
        graph1.created should be(Element.timestamp(0x10000000000L, 0x10000000L))
        an[IllegalStateException] should be thrownBy Serialization.acquireLoader(folderA.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))
        an[IllegalStateException] should be thrownBy Serialization.acquireLoader(folderA.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> true))
      }
    }({
      logCaptor ⇒
        logCaptor.getAllValues().asScala.find { event ⇒
          val level = event.getLevel()
          event.getMessage() != null &&
            !event.getMessage().toString.startsWith("Unable to acquire graph 'john1 from file") &&
            (level == Level.WARN || level == Level.ERROR || level == Level.FATAL)
        }.map { event ⇒
          fail(s"Unexpected log message detected. ${event.getLevel()}: ${event.getMessage()}" +
            Option(event.getThrowableStrRep()).map(_.mkString("\n")))
        }
    })
  }
  "Test Graph.Loader creation for graph with 3 sources and modified descriptor.yaml" in {
    implicit val option = Mockito.atLeast(1)
    withMockitoLogCaptor {
      withTempFolder { folder ⇒
        import TestDSL._

        val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }

        val modification = Timestamp.dump(graph.modified)
        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        val folderC = new File(folder, "C")
        val fileADigestType = new File(folderA, s"digest/${modification}/type")
        val fileADigestData = new File(folderA, s"digest/${modification}/digest")
        val fileBDigestType = new File(folderB, s"digest/${modification}/type")
        val fileBDigestData = new File(folderB, s"digest/${modification}/digest")
        val fileCDigestType = new File(folderC, s"digest/${modification}/type")
        val fileCDigestData = new File(folderC, s"digest/${modification}/digest")
        Serialization.freeze(graph, SData(Digest.Key.freeze -> Map(folderA.toURI() -> Digest.NoDigest)),
          folderA.toURI(), folderB.toURI(), folderC.toURI())
        fileADigestType should not be ('exists)
        fileADigestData should not be ('exists)
        fileBDigestType should be('exists)
        fileBDigestData should be('exists)
        fileCDigestType should be('exists)
        fileCDigestData should be('exists)

        val fileToModify = new File(folderB, s"descriptor.yaml")
        val file = new RandomAccessFile(fileToModify, "rws")
        val text = new Array[Byte](fileToModify.length().toInt)
        file.readFully(text)
        file.seek(0)
        file.write(new String(text, io.Codec.UTF8.charSet).replaceAll("created: .*ns", "created: 10000000000ms.10000000ns").getBytes(io.Codec.UTF8.charSet))
        file.close()

        val graphWithDigestLoader1 = Serialization.acquireLoader(folderA.getAbsoluteFile().toURI())
        graphWithDigestLoader1.sources should have size (3)
        val graphWithDigestLoader2 = Serialization.acquireLoader(folderA.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))
        graphWithDigestLoader2.sources should have size (2)
        val graphWithDigestLoader3 = Serialization.acquireLoader(folderA.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> true))
        graphWithDigestLoader3.sources should have size (2)
      }
    }({
      logCaptor ⇒
        logCaptor.getAllValues().asScala.find { event ⇒
          val level = event.getLevel()
          event.getMessage() != null &&
            !event.getMessage().toString.startsWith("Unable to acquire graph 'john1 from file") &&
            (level == Level.WARN || level == Level.ERROR || level == Level.FATAL)
        }.map { event ⇒
          fail(s"Unexpected log message detected. ${event.getLevel()}: ${event.getMessage()}" +
            Option(event.getThrowableStrRep()).map(_.mkString("\n")))
        }
    })
  }
  "Digest should append default parameters when freeze graph" in {
    implicit val option = Mockito.atLeast(1)
    withMockitoLogCaptor {
      withTempFolder { folder ⇒
        import TestDSL._

        val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }

        val modification = Timestamp.dump(graph.modified)
        val folderA = new File(folder, "A")
        val fileADigestType = new File(folderA, s"digest/${modification}/type")
        val fileADigestData = new File(folderA, s"digest/${modification}/digest")
        val sDataTest = new AtomicReference[SData]()

        Serialization.freeze(graph, SData(SData.Key.beforeFreeze -> {
          (_: Graph[_ <: Model.Like], _: Transport, sData: SData) ⇒ assert(sDataTest.getAndSet(sData) == null)
        }, Digest.Key.freeze -> Map()), folderA.getAbsoluteFile().toURI())
        fileADigestType should be('exists)
        fileADigestData should be('exists)
        sDataTest.get()(Digest.Key.freeze) should be(Map(folderA.toURI() -> Digest.default))

        graph.model.eSet('AAAKey, "AAA")

        val modification2 = Timestamp.dump(graph.modified)
        val fileADigestType2 = new File(folderA, s"digest/${modification2}/type")
        val fileADigestData2 = new File(folderA, s"digest/${modification2}/digest")
        sDataTest.set(null)

        Serialization.freeze(graph, SData(SData.Key.beforeFreeze -> {
          (_: Graph[_ <: Model.Like], _: Transport, sData: SData) ⇒ assert(sDataTest.getAndSet(sData) == null)
        }, Digest.Key.freeze -> Map()))
        fileADigestType2 should be('exists)
        fileADigestData2 should be('exists)
      }
    }({
      logCaptor ⇒
        logCaptor.getAllValues().asScala.find { event ⇒
          val level = event.getLevel()
          event.getMessage() != null && (level == Level.WARN || level == Level.ERROR || level == Level.FATAL)
        }.map { event ⇒
          fail(s"Unexpected log message detected. ${event.getLevel()}: ${event.getMessage()}" +
            Option(event.getThrowableStrRep()).map(_.mkString("\n")))
        }
    })
  }
  "Acquire process should skip sources with incorrect check sum" in {
    implicit val option = Mockito.atLeast(1)
    withMockitoLogCaptor {
      withTempFolder { folder ⇒
        import TestDSL._

        // graph
        val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        model.takeRecord('baseLevel) { r ⇒
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
        }

        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        val folderC = new File(folder, "C")
        val sDataFreeze = SData(Digest.Key.freeze ->
          Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")))
        Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())

        val folderBsum = new File(Digest.digestURI(folderB.toURI(), testTransport, graph.modified, Digest.containerName))
        val lines = scala.io.Source.fromFile(folderBsum).getLines.toList.map(_ match {
          case line if line.endsWith("descriptor.yaml") ⇒
            val hash = line.takeWhile(_ != ' ')
            "".padTo(hash.length(), "0").mkString + line.drop(hash.length())
          case line ⇒
            line
        })
        // Overwrite folderBsum
        val pos = new PrintStream(new BufferedOutputStream(new FileOutputStream(folderBsum)))
        try {
          lines.foreach(pos.println)
          pos.flush()
        } finally try pos.close() catch { case e: Throwable ⇒ }

        val sDataAcquire = SData(Digest.Key.acquire -> false)

        Mockito.reset(testDigest)
        Mockito.reset(testTransport)
        val inOrder = Mockito.inOrder(testDigest, testTransport)
        val graphWithDigestLoader = Serialization.acquireLoader(folderA.getAbsoluteFile().toURI(), sDataAcquire)
        graphWithDigestLoader.sources.size should be(2)
        // Bootstrap loader:
        // 1st - read A/descriptor.yaml
        // 2nd - read -record.yaml
        // 3rd - read -resources.yaml
        // and create Source for folderA
        // 1st - read A/descriptor.yaml
        // 2nd - read A/.../node%20descriptor-NNNms.NNNns.yaml (graph root node )
        inOrder.verify(testTransport, Mockito.times(5)).read(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderA.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())
        // and create Source for folderB
        // 1st - read B/descriptor.yaml
        // 2nd - read B/.../node%20descriptor-NNNms.NNNns.yaml (graph root node )
        // 2 files with digest from storage B
        inOrder.verify(testTransport, Mockito.times(1)).read(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderB.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())
        // and create Source for folderC
        // 1st - read C/descriptor.yaml
        // 2nd - read C/.../node%20descriptor-NNNms.NNNns.yaml (graph root node )
        // 2 files with digest from storage C
        inOrder.verify(testTransport, Mockito.times(1)).read(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())
        //        inOrder.verify(testDigest, Mockito.times(1)).approve(MM.argThat(new BaseMatcher {
        //          def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
        //          def describeTo(description: Description) {}
        //        }), MM.anyObject())
        inOrder.verify(testTransport, Mockito.times(1)).read(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())
        //        inOrder.verify(testDigest, Mockito.times(1)).approve(MM.argThat(new BaseMatcher {
        //          def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
        //          def describeTo(description: Description) {}
        //        }), MM.anyObject())
        // So there were 4 files with digest at all
        //        inOrder.verify(testDigest, Mockito.never()).approve(MM.anyObject(), MM.anyObject())
        inOrder.verify(testTransport, Mockito.never()).read(MM.anyObject(), MM.anyObject())
      }
    }({
      logCaptor ⇒
        logCaptor.getAllValues().asScala.find { event ⇒
          val level = event.getLevel()
          event.getMessage() != null &&
            !event.getMessage().toString.startsWith("Unable to acquire graph 'john1 from file") &&
            (level == Level.WARN || level == Level.ERROR || level == Level.FATAL)
        }.map { event ⇒
          fail(s"Unexpected log message detected. ${event.getLevel()}: ${event.getMessage()}" +
            Option(event.getThrowableStrRep()).map(_.mkString("\n")))
        }
    })
  }
  "Acquire process should use only trusted sources with Digest.Key.acquire -> true" in {
    implicit val option = Mockito.atLeast(1)
    withMockitoLogCaptor {
      withTempFolder { folder ⇒
        import TestDSL._

        // graph
        val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        val folderC = new File(folder, "C")
        val sDataFreeze = SData(Digest.Key.freeze ->
          Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")))
        Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
        val sDataAcquire = SData(Digest.Key.acquire -> true,
          SData.Key.beforeAcquire -> ((_: Graph[_ <: Model.Like], _: Transport, sData: SData) ⇒ {
            sData(SData.Key.sources).map(_.storageURI).toList should be(List(folderA.toURI(), folderC.toURI(), folderB.toURI()))
          }))
        val graphWithDigestLoader = Serialization.acquireLoader(folderA.getAbsoluteFile().toURI(), sDataAcquire)
        graphWithDigestLoader.sources.foreach { source ⇒
          source.storageURI match {
            case a if a == folderA.toURI ⇒
              source.weigth should be(2000)
            case b if b == folderB.toURI ⇒
              source.weigth should be(1150)
            case c if c == folderC.toURI ⇒
              source.weigth should be(1300)
          }
        }
        Mockito.reset(testDigest)
        Mockito.reset(testTransport)
        val graphWithDigest = graphWithDigestLoader.load()
        Mockito.verify(testTransport, Mockito.times(7)).read(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderA.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())
        // failed reads from folderA + records file + resources file
        Mockito.verify(testTransport, Mockito.times(9)).read(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())

        graphWithDigest.node.safeRead { node ⇒
          graph.node.safeRead { node2 ⇒
            node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
          }
        } should be(true)
      }
    }({
      logCaptor ⇒
        logCaptor.getAllValues().asScala.find { event ⇒
          val level = event.getLevel()
          event.getMessage() != null &&
            !event.getMessage().toString.startsWith("Unable to load ") &&
            !event.getMessage().toString.startsWith("Unable to acquire graph 'john1 from file") &&
            (level == Level.WARN || level == Level.ERROR || level == Level.FATAL)
        }.map { event ⇒
          fail(s"Unexpected log message detected. ${event.getLevel()}: ${event.getMessage()}" +
            Option(event.getThrowableStrRep()).map(_.mkString("\n")))
        }
    })
  }
  "Acquire process should load partially damaged node with Digest.Key.acquire -> true successfully" in {
    implicit val option = Mockito.atLeast(1)
    withMockitoLogCaptor {
      withTempFolder { folder ⇒
        import TestDSL._

        // graph
        val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        val folderC = new File(folder, "C")
        val sDataFreeze = SData(Digest.Key.freeze ->
          Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")))
        Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
        val sDataAcquire = SData(Digest.Key.acquire -> true,
          SData.Key.beforeAcquire -> ((_: Graph[_ <: Model.Like], _: Transport, sData: SData) ⇒ {
            sData(SData.Key.sources).map(_.storageURI).toList should be(List(folderB.toURI(), folderC.toURI(), folderA.toURI()))
          }))
        val graphWithDigestLoader = Serialization.acquireLoader(folderB.getAbsoluteFile().toURI(), sDataAcquire)
        graphWithDigestLoader.sources.foreach { source ⇒
          source.storageURI match {
            case a if a == folderA.toURI ⇒
              source.weigth should be(1000)
            case b if b == folderB.toURI ⇒
              source.weigth should be(2150)
            case c if c == folderC.toURI ⇒
              source.weigth should be(1300)
          }
        }

        // Damage level1a.
        val eLevelA = new File(folderB, "data/e john1 {0609C486}/e baseLevel {92A93F33}/e level1a {0428D0D4}")
        eLevelA should be('exists)
        deleteFolder(eLevelA)
        eLevelA should not be ('exists)

        Mockito.reset(testDigest)
        Mockito.reset(testTransport)
        val graphWithDigest = graphWithDigestLoader.load()
        // recover
        Mockito.verify(testTransport, Mockito.times(3)).read(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())
        // failed reads from folderA + records file + resources file
        Mockito.verify(testTransport, Mockito.times(9)).read(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderB.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())

        Mockito.verify(testTransport, Mockito.never).read(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderA.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())

        graphWithDigest.node.safeRead { node ⇒
          graph.node.safeRead { node2 ⇒
            node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
          }
        } should be(true)
      }
    }({
      logCaptor ⇒
        logCaptor.getAllValues().asScala.find { event ⇒
          val level = event.getLevel()
          event.getMessage() != null &&
            !event.getMessage().toString.startsWith("Unable to load ") &&
            !event.getMessage().toString.startsWith("Unable to acquire graph 'john1 from file") &&
            (level == Level.WARN || level == Level.ERROR || level == Level.FATAL)
        }.map { event ⇒
          fail(s"Unexpected log message detected. ${event.getLevel()}: ${event.getMessage()}" +
            Option(event.getThrowableStrRep()).map(_.mkString("\n")))
        }
    })
  }
  "Acquire process should fail while loading fully damaged node with Digest.Key.acquire -> true" in {
    implicit val option = Mockito.atLeast(1)
    withMockitoLogCaptor {
      withTempFolder { folder ⇒
        import TestDSL._

        // graph
        val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        val folderC = new File(folder, "C")
        val sDataFreeze = SData(Digest.Key.freeze ->
          Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")))
        Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
        val sDataAcquire = SData(Digest.Key.acquire -> true,
          SData.Key.beforeAcquire -> ((_: Graph[_ <: Model.Like], _: Transport, sData: SData) ⇒ {
            sData(SData.Key.sources).map(_.storageURI).toList should be(List(folderB.toURI(), folderC.toURI(), folderA.toURI()))
          }))
        val graphWithDigestLoader = Serialization.acquireLoader(folderB.getAbsoluteFile().toURI(), sDataAcquire)
        graphWithDigestLoader.sources.foreach { source ⇒
          source.storageURI match {
            case a if a == folderA.toURI ⇒
              source.weigth should be(1000)
            case b if b == folderB.toURI ⇒
              source.weigth should be(2150)
            case c if c == folderC.toURI ⇒
              source.weigth should be(1300)
          }
        }

        // Damage level1a.
        val eLevelA_B = new File(folderB, "data/e john1 {0609C486}/e baseLevel {92A93F33}/e level1a {0428D0D4}")
        eLevelA_B should be('exists)
        deleteFolder(eLevelA_B)
        eLevelA_B should not be ('exists)
        val eLevelA_C = new File(folderC, "data/e john1 {0609C486}/e baseLevel {92A93F33}/e level1a {0428D0D4}")
        eLevelA_C should be('exists)
        deleteFolder(eLevelA_C)
        eLevelA_C should not be ('exists)

        Mockito.reset(testDigest)
        Mockito.reset(testTransport)
        an[IllegalStateException] should be thrownBy graphWithDigestLoader.load()
      }
    }({
      logCaptor ⇒
        logCaptor.getAllValues().asScala.find { event ⇒
          val level = event.getLevel()
          event.getMessage() != null &&
            !event.getMessage().toString.startsWith("Unable to load node ") &&
            !event.getMessage().toString.startsWith("Unable to acquire graph 'john1 from file") &&
            (level == Level.WARN || level == Level.ERROR || level == Level.FATAL)
        }.map { event ⇒
          fail(s"Unexpected log message detected. ${event.getLevel()}: ${event.getMessage()}" +
            Option(event.getThrowableStrRep()).map(_.mkString("\n")))
        }
    })
  }
  "Acquire process should load graph without fully damaged node with Digest.Key.acquire -> true and force -> true" in {
    implicit val option = Mockito.atLeast(1)
    withMockitoLogCaptor {
      withTempFolder { folder ⇒
        import TestDSL._

        // graph
        val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        model.takeRecord('baseLevel) { r ⇒
          r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } }
          r.takeRecord('level1b) { r ⇒ r.takeRecord('level2b) { r ⇒ } }
        }

        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        val folderC = new File(folder, "C")
        val sDataFreeze = SData(Digest.Key.freeze ->
          Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")))
        Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
        val sDataAcquire = SData(Digest.Key.acquire -> true, SData.Key.force -> true,
          SData.Key.beforeAcquire -> ((_: Graph[_ <: Model.Like], _: Transport, sData: SData) ⇒ {
            sData(SData.Key.sources).map(_.storageURI).toList should be(List(folderB.toURI(), folderC.toURI(), folderA.toURI()))
          }))
        val graphWithDigestLoader = Serialization.acquireLoader(folderB.getAbsoluteFile().toURI(), sDataAcquire)
        graphWithDigestLoader.sources.foreach { source ⇒
          source.storageURI match {
            case a if a == folderA.toURI ⇒
              source.weigth should be(1000)
            case b if b == folderB.toURI ⇒
              source.weigth should be(2150)
            case c if c == folderC.toURI ⇒
              source.weigth should be(1300)
          }
        }

        // Damage level1a.
        val eLevelA_B = new File(folderB, "data/e john1 {0609C486}/e baseLevel {92A93F33}/e level1a {0428D0D4}")
        eLevelA_B should be('exists)
        deleteFolder(eLevelA_B)
        eLevelA_B should not be ('exists)
        val eLevelA_C = new File(folderC, "data/e john1 {0609C486}/e baseLevel {92A93F33}/e level1a {0428D0D4}")
        eLevelA_C should be('exists)
        deleteFolder(eLevelA_C)
        eLevelA_C should not be ('exists)

        Mockito.reset(testDigest)
        Mockito.reset(testTransport)
        val graphWithoutELevel1a = graphWithDigestLoader.load()
        val nodes = graphWithoutELevel1a.node.safeRead(_.iteratorRecursive.toList)
        // try to load from folderA, but fail
        Mockito.verify(testTransport, Mockito.times(1)).read(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderA.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())
        Mockito.verify(testTransport, Mockito.times(10)).read(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderB.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())
        // try to load from folderC, but fail
        Mockito.verify(testTransport, Mockito.times(1)).read(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())
        // level1a and level2a are damaged at folderB and folderC, folderA without digests
        nodes.map(_.id.name) should be(List("baseLevel", "level1b", "level2b"))

        Mockito.reset(testDigest)
        Mockito.reset(testTransport)
        val graphWithDigest = Serialization.acquire(folderB.getAbsoluteFile().toURI(),
          sDataAcquire.
            updated(Digest.Key.acquire, false).
            updated(SData.Key.beforeAcquire, ((_: Graph[_ <: Model.Like], _: Transport, sData: SData) ⇒ {
              sData(SData.Key.sources).map(_.storageURI).toList should be(List(folderB.toURI(), folderC.toURI(), folderA.toURI()))
            })))
        // try to load from folderA, and succeed
        Mockito.verify(testTransport, Mockito.times(5)).read(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderA.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())
        Mockito.verify(testTransport, Mockito.times(18)).read(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderB.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())
        // try to load from folderC, but fail
        Mockito.verify(testTransport, Mockito.times(5)).read(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())
        graphWithDigest.node.safeRead { node ⇒
          graph.node.safeRead { node2 ⇒
            node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
          }
        } should be(true)
      }
    }({
      logCaptor ⇒
        logCaptor.getAllValues().asScala.find { event ⇒
          val level = event.getLevel()
          event.getMessage() != null &&
            !event.getMessage().toString.startsWith("Unable to load node ") &&
            !event.getMessage().toString.startsWith("Unable to load element box ") &&
            !event.getMessage().toString.startsWith("Unable to acquire graph 'john1 from file") &&
            (level == Level.WARN || level == Level.ERROR || level == Level.FATAL)
        }.map { event ⇒
          fail(s"Unexpected log message detected. ${event.getLevel()}: ${event.getMessage()}" +
            Option(event.getThrowableStrRep()).map(_.mkString("\n")))
        }
    })
  }
  "Acquire process should handle multiple digest chunks" in {
    implicit val option = Mockito.atLeast(1)
    withMockitoLogCaptor {
      withTempFolder { folder ⇒
        import TestDSL._

        // graph
        val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ r.name = "111" } } }

        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        val folderC = new File(folder, "C")
        val sDataFreeze = SData(Digest.Key.freeze ->
          Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")))
        Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
        Serialization.acquire(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))
        Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
        Serialization.acquire(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))
        Serialization.freeze(graph, sDataFreeze.updated(SData.Key.force, true), folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
        val graph2 = Serialization.acquire(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))

        // modify
        graph2.model.takeRecord('baseLevel) { r ⇒
          r.takeRecord('level1a) { r ⇒
            r.takeRecord('level2a) { r ⇒ r.name = "222" }
            r.takeRecord('level2aX) { r ⇒ r.name = "222" }
          }
        }
        val sDataFreeze2 = SData(Digest.Key.freeze ->
          Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> Digest.NoDigest, folderC.toURI -> Digest.NoDigest))
        Serialization.freeze(graph2, sDataFreeze2, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
        val graph3 = Serialization.acquire(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))

        graph3.model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ r.name = "333" } } }
        val sDataFreeze3 = SData(Digest.Key.freeze ->
          Map(folderA.toURI -> SimpleDigest("MD2"), folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> Digest.NoDigest))
        Serialization.freeze(graph3, sDataFreeze3, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())

        val graph4Loader = Serialization.acquireLoader(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))
        val records = graph4Loader.sources.head.graphDescriptor.records.sorted
        val history = graph4Loader.sData(Digest.historyPerURI)
        val historyForFolderA = history(folderA.toURI())
        val historyForFolderB = history(folderB.toURI())
        val historyForFolderC = history(folderC.toURI())

        records.map(r ⇒ historyForFolderA(r)._1.toString()).toList should be(List("NoDigest", "NoDigest", "SimpleDigestParameters(MD2)"))
        records.map(r ⇒ historyForFolderB(r)._1.toString()).toList should be(List("SimpleDigestParameters(MD5)", "NoDigest", "SimpleDigestParameters(MD5)"))
        records.map(r ⇒ historyForFolderC(r)._1.toString()).toList should be(List("SimpleDigestParameters(SHA-512)", "NoDigest", "NoDigest"))

        val graph4 = graph4Loader.load()
        graph4.node.safeRead { node ⇒
          graph3.node.safeRead { node2 ⇒
            node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
          }
        } should be(true)
      }
    }({
      logCaptor ⇒
        logCaptor.getAllValues().asScala.find { event ⇒
          val level = event.getLevel()
          event.getMessage() != null &&
            (level == Level.WARN || level == Level.ERROR || level == Level.FATAL)
        }.map { event ⇒
          fail(s"Unexpected log message detected. ${event.getLevel()}: ${event.getMessage()}" +
            Option(event.getThrowableStrRep()).map(_.mkString("\n")))
        }
    })
  }
  "Acquire process should fail to load graph if one or more chunks without digest with Digest.Key.acquire -> true" in {
    implicit val option = Mockito.atLeast(1)
    withMockitoLogCaptor {
      withTempFolder { folder ⇒
        import TestDSL._

        // graph
        val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ r.name = "111" } } }

        val folderA = new File(folder, "A")
        val folderB = new File(folder, "B")
        val folderC = new File(folder, "C")
        val sDataFreeze = SData(Digest.Key.freeze ->
          Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")))
        Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
        Serialization.acquire(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))
        Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
        Serialization.acquire(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))
        Serialization.freeze(graph, sDataFreeze.updated(SData.Key.force, true), folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
        val graph2 = Serialization.acquire(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))

        // modify
        graph2.model.takeRecord('baseLevel) { r ⇒
          r.takeRecord('level1a) { r ⇒
            r.takeRecord('level2a) { r ⇒ r.name = "222" }
            r.takeRecord('level2aX) { r ⇒ r.name = "222" }
          }
        }
        val sDataFreeze2 = SData(Digest.Key.freeze ->
          Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> Digest.NoDigest, folderC.toURI -> Digest.NoDigest))
        Serialization.freeze(graph2, sDataFreeze2, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
        val graph3 = Serialization.acquire(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))

        graph3.model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ r.name = "333" } } }
        val sDataFreeze3 = SData(Digest.Key.freeze ->
          Map(folderA.toURI -> SimpleDigest("MD2"), folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> Digest.NoDigest))
        Serialization.freeze(graph3, sDataFreeze3, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())

        val graph4Loader = Serialization.acquireLoader(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> true))
        val records = graph4Loader.sources.head.graphDescriptor.records.sorted
        val history = graph4Loader.sData(Digest.historyPerURI)
        val historyForFolderA = history(folderA.toURI())
        val historyForFolderB = history(folderB.toURI())
        val historyForFolderC = history(folderC.toURI())

        records.map(r ⇒ historyForFolderA(r)._1.toString()).toList should be(List("NoDigest", "NoDigest", "SimpleDigestParameters(MD2)"))
        records.map(r ⇒ historyForFolderB(r)._1.toString()).toList should be(List("SimpleDigestParameters(MD5)", "NoDigest", "SimpleDigestParameters(MD5)"))
        records.map(r ⇒ historyForFolderC(r)._1.toString()).toList should be(List("SimpleDigestParameters(SHA-512)", "NoDigest", "NoDigest"))

        an[IllegalStateException] should be thrownBy graph4Loader.load()
      }
    }({
      logCaptor ⇒
        logCaptor.getAllValues().asScala.find { event ⇒
          val level = event.getLevel()
          event.getMessage() != null &&
            !event.getMessage().toString.startsWith("Unable to acquire information from file") &&
            !event.getMessage().toString.startsWith("Unable to load ") &&
            !event.getMessage().toString.startsWith("Skip history record ") &&
            (level == Level.WARN || level == Level.ERROR || level == Level.FATAL)
        }.map { event ⇒
          fail(s"Unexpected log message detected. ${event.getLevel()}: ${event.getMessage()}" +
            Option(event.getThrowableStrRep()).map(_.mkString("\n")))
        }
    })
  }
  "Acquire process should load only trusted part of graph if one or more chunks without digest, Digest.Key.acquire -> true and force -> true" in {
    withTempFolder { folder ⇒
      import TestDSL._

      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ r.name = "111" } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val sDataFreeze = SData(Digest.Key.freeze ->
        Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")))
      Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
      Serialization.acquire(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))
      Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
      Serialization.acquire(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))
      Serialization.freeze(graph, sDataFreeze.updated(SData.Key.force, true), folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
      val graph2 = Serialization.acquire(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))

      // modify
      graph2.model.takeRecord('baseLevel) { r ⇒
        r.takeRecord('level1a) { r ⇒
          r.takeRecord('level2a) { r ⇒ r.name = "222" }
          r.takeRecord('level2aX) { r ⇒ r.name = "222" }
        }
      }
      val sDataFreeze2 = SData(Digest.Key.freeze ->
        Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> Digest.NoDigest, folderC.toURI -> Digest.NoDigest))
      Serialization.freeze(graph2, sDataFreeze2, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
      val graph3 = Serialization.acquire(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))

      graph3.model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ r.name = "333" } } }
      val sDataFreeze3 = SData(Digest.Key.freeze ->
        Map(folderA.toURI -> SimpleDigest("MD2"), folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> Digest.NoDigest))
      Serialization.freeze(graph3, sDataFreeze3, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())

      val graph4Loader = Serialization.acquireLoader(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> true, SData.Key.force -> true))
      val records = graph4Loader.sources.head.graphDescriptor.records.sorted
      val history = graph4Loader.sData(Digest.historyPerURI)
      val historyForFolderA = history(folderA.toURI())
      val historyForFolderB = history(folderB.toURI())
      val historyForFolderC = history(folderC.toURI())

      records.map(r ⇒ historyForFolderA(r)._1.toString()).toList should be(List("NoDigest", "NoDigest", "SimpleDigestParameters(MD2)"))
      records.map(r ⇒ historyForFolderB(r)._1.toString()).toList should be(List("SimpleDigestParameters(MD5)", "NoDigest", "SimpleDigestParameters(MD5)"))
      records.map(r ⇒ historyForFolderC(r)._1.toString()).toList should be(List("SimpleDigestParameters(SHA-512)", "NoDigest", "NoDigest"))

      val graph4 = graph4Loader.load()
      val nodes = graph4.node.safeRead(_.iteratorRecursive.toList)
      // level2aX has't digest
      nodes.map(_.id.name) should be(List("baseLevel", "level1a", "level2a"))
    }
  }
  "Freeze process should use the latest digest configuration by default" in {
    withTempFolder { folder ⇒
      import TestDSL._

      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val sDataFreeze = SData(Digest.Key.freeze ->
        Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")))
      Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())

      // modify
      graph.model.takeRecord('baseLevel) { _.takeRecord('level1a) { _.takeRecord('level2aX) { _.name = "222" } } }
      Serialization.freeze(graph)

      // modify
      graph.model.takeRecord('baseLevel) { _.takeRecord('level1a) { _.takeRecord('level2aX) { _.name = "333" } } }
      Serialization.freeze(graph)

      val graph2Loader = Serialization.acquireLoader(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> true))
      val records = graph2Loader.sources.head.graphDescriptor.records.sorted
      val history = graph2Loader.sData(Digest.historyPerURI)
      val historyForFolderA = history(folderA.toURI())
      val historyForFolderB = history(folderB.toURI())
      val historyForFolderC = history(folderC.toURI())

      records.map(r ⇒ historyForFolderA(r)._1.toString()).toList should be(List("NoDigest", "NoDigest", "NoDigest"))
      records.map(r ⇒ historyForFolderB(r)._1.toString()).toList should be(List("SimpleDigestParameters(MD5)", "SimpleDigestParameters(MD5)", "SimpleDigestParameters(MD5)"))
      records.map(r ⇒ historyForFolderC(r)._1.toString()).toList should be(List("SimpleDigestParameters(SHA-512)", "SimpleDigestParameters(SHA-512)", "SimpleDigestParameters(SHA-512)"))
      val graph2 = graph2Loader.load()
      graph.node.safeRead { node ⇒
        graph2.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)
    }
  }
  "Freeze process should use a default digest parameters for new storage" in {
    withTempFolder { folder ⇒
      import TestDSL._

      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val sDataFreeze = SData(Digest.Key.freeze ->
        Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5")))
      Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI())

      // modify
      graph.model.takeRecord('baseLevel) { _.takeRecord('level1a) { _.takeRecord('level2aX) { _.name = "222" } } }
      val folderC = new File(folder, "C")
      Serialization.freeze(graph, folderC.getAbsoluteFile().toURI())

      val graph2Loader = Serialization.acquireLoader(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> true))
      val records = graph2Loader.sources.head.graphDescriptor.records.sorted
      val history = graph2Loader.sData(Digest.historyPerURI)
      val historyForFolderA = history(folderA.toURI())
      val historyForFolderB = history(folderB.toURI())
      val historyForFolderC = history(folderC.toURI())

      records.map(r ⇒ historyForFolderA(r)._1.toString()).toList should be(List("NoDigest", "NoDigest"))
      records.map(r ⇒ historyForFolderB(r)._1.toString()).toList should be(List("SimpleDigestParameters(MD5)", "SimpleDigestParameters(MD5)"))
      records.map(r ⇒ historyForFolderC(r)._1.toString()).toList should be(List("NoDigest", "SimpleDigestParameters(SHA-1)"))
      val graph2 = graph2Loader.load()
      graph.node.safeRead { node ⇒
        graph2.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)
    }
  }
  "Digest mechanism should support Digest.Key.readFilter and Digest.Key.writeFilter hooks" in {
    withTempFolder { folder ⇒
      import TestDSL._

      val readMap = new mutable.HashMap[URI, TestInputStream] with mutable.SynchronizedMap[URI, TestInputStream]
      val writeMap = new mutable.HashMap[URI, TestOutputStream] with mutable.SynchronizedMap[URI, TestOutputStream]

      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val readFilter = (is: InputStream, uri: URI, transport: Transport, sData: SData) ⇒ {
        val stream = new TestInputStream(is)
        val base = Digest.digestURI(sData(SData.Key.storageURI), transport, graph.modified)
        readMap(base.resolve(uri)) = stream
        stream
      }
      val writeFilter = (os: OutputStream, uri: URI, transport: Transport, sData: SData) ⇒ {
        val stream = new TestOutputStream(os)
        val base = Digest.digestURI(sData(SData.Key.storageURI), transport, graph.modified)
        writeMap(base.resolve(uri)) = stream
        stream
      }

      val sDataFreeze = SData(Digest.Key.writeFilter -> writeFilter, Digest.Key.freeze ->
        Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")))
      Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
      readMap should be(empty)
      writeMap should have size (2)
      writeMap.foreach { case (uri, stream) ⇒ new File(uri).length() should be(stream.doneBytes) }

      val graph2 = Serialization.acquire(folderB.getAbsoluteFile().toURI(),
        SData(Digest.Key.readFilter -> readFilter, Digest.Key.acquire -> true))
      graph.node.safeRead { node ⇒
        graph2.node.safeRead { node2 ⇒
          node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
        }
      } should be(true)

      readMap should have size (2)
      writeMap should have size (2)

      readMap.foreach { case (uri, stream) ⇒ new File(uri).length() should be(stream.doneBytes) }

      readMap.keySet should be(writeMap.keySet)
    }
  }
  "Digest loading should be stopped by Digest.Key.readFilter hook" in {
    withTempFolder { folder ⇒
      import TestDSL._

      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val sDataFreeze = SData(Digest.Key.freeze ->
        Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")))
      Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())

      val readFilter = (is: InputStream, uri: URI, transport: Transport, sData: SData) ⇒ {
        new TestInputStream(is) {
          override def close() {
            super.close()
            throw new SecurityException("Terminate process. Unauthorized content.")
          }
        }
      }
      an[IllegalStateException] should be thrownBy
        Serialization.acquire(folderB.getAbsoluteFile().toURI(),
          SData(Digest.Key.readFilter -> readFilter, Digest.Key.acquire -> true))
    }
  }
  "Digest history should be projection of Graph.Retrospective: everything OK" in {
    withTempFolder { folder ⇒
      import TestDSL._

      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val sDataFreeze = SData(Digest.Key.freeze ->
        Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")))

      info("There are no retrospective records")
      Digest.history(graph) should be(empty)

      Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
      val graphLoaderWith1Record = Serialization.acquireLoader(folderA.toURI(), SData(Digest.Key.acquire -> false))
      val graphWith1Record = graphLoaderWith1Record.load()

      info("There is a single retrospective record")
      val h1l = Digest.history(graphLoaderWith1Record)
      val h1g = Digest.history(graph)
      h1l should be(h1g)
      h1l.size should be(1)
      h1l.head._2.size should be(3)

      model.takeRecord('baseLevel) { r ⇒ r.name = "222" }
      Serialization.freeze(graph)
      val graphLoaderWith2Records = Serialization.acquireLoader(folderA.toURI(), SData(Digest.Key.acquire -> false))

      info("There are two retrospective records")
      val h2l = Digest.history(graphLoaderWith2Records)
      val h2g = Digest.history(graph)
      h2l should be(h2g)
      h2l.size should be(2)
      h2l.head._2.size should be(3)
      val h2keys = h2l.keys.toSeq
      h2l(h2keys.head) should be(h2l(h2keys.last))

      model.takeRecord('baseLevel) { r ⇒ r.name = "333" }
      Serialization.freeze(graph, SData(Digest.Key.freeze ->
        Map(folderA.toURI -> SimpleDigest("MD5"), folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("MD5"))))
      val graphLoaderWith3Records = Serialization.acquireLoader(folderA.toURI(), SData(Digest.Key.acquire -> false))

      info("There are three retrospective records")
      val h3l = Digest.history(graphLoaderWith3Records)
      val h3g = Digest.history(graph)
      h3l should be(h3g)
      h3l.size should be(3)
      h3l.map(_._2.size).toSeq should be(Seq(3, 3, 3))
      val h3keys = h3l.keys.toSeq.sorted
      h3keys.last should be(graph.modified)
      h3l(h3keys(0)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "MD5", "C/" -> "SHA-512"))
      h3l(h3keys(1)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "MD5", "C/" -> "SHA-512"))
      h3l(h3keys(2)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "MD5", "B/" -> "MD5", "C/" -> "MD5"))
    }
  }
  "Digest history should be projection of Graph.Retrospective: something wrong" in {
    withTempFolder { folder ⇒
      import TestDSL._

      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val sDataFreeze = SData(Digest.Key.freeze ->
        Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")))

      info("There are no retrospective records")
      Digest.history(graph) should be(empty)

      Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
      val graphLoaderWith1Record = Serialization.acquireLoader(folderA.toURI(), SData(Digest.Key.acquire -> false))
      val graphWith1Record = graphLoaderWith1Record.load()

      info("There is a single retrospective record")
      val h1l = Digest.history(graphLoaderWith1Record)
      val h1g = Digest.history(graph)
      h1l should be(h1g)
      h1l.size should be(1)
      h1l.head._2.size should be(3)

      model.takeRecord('baseLevel) { r ⇒ r.name = "222" }
      Serialization.freeze(graph)
      val graphLoaderWith2Records = Serialization.acquireLoader(folderA.toURI(), SData(Digest.Key.acquire -> false))

      info("There are two retrospective records")
      val h2l = Digest.history(graphLoaderWith2Records)
      val h2g = Digest.history(graph)
      h2l should be(h2g)
      h2l.size should be(2)
      h2l.head._2.size should be(3)
      val h2keys = h2l.keys.toSeq
      h2l(h2keys.head) should be(h2l(h2keys.last))

      info(s"Damage 'digest' in ${folderB} and 'type' in ${folderC}")
      val file2BDigestType = new File(folderB, s"digest/${Timestamp.dump(graphLoaderWith2Records.modified)}/type")
      val file2CDigestData = new File(folderC, s"digest/${Timestamp.dump(graphLoaderWith2Records.modified)}/digest")
      assert(file2BDigestType.delete(), "Unable to delete " + file2BDigestType)
      assert(file2CDigestData.delete(), "Unable to delete " + file2CDigestData)

      info("There are two retrospective records where one of them is broken")
      val graphLoaderWith2RecordsX = Serialization.acquireLoader(folderA.toURI(), SData(Digest.Key.acquire -> false))
      val h2lx = Digest.history(graphLoaderWith2RecordsX)
      val h2gx = Digest.history(graph)
      h2lx should be(h2gx)
      h2lx.size should be(2)
      h2lx.toSeq.sortBy(_._1).last._2.size should be(3)
      val h2keysx = h2lx.keys.toSeq
      h2lx(h2keysx.head) should not be (h2lx(h2keysx.last))

      model.takeRecord('baseLevel) { r ⇒ r.name = "333" }
      Serialization.freeze(graph)
      val graphLoaderWith3Records = Serialization.acquireLoader(folderA.toURI(), SData(Digest.Key.acquire -> false))

      info("There are three retrospective records")
      val h3l = Digest.history(graphLoaderWith3Records)
      val h3g = Digest.history(graph)
      h3l should be(h3g)
      h3l.size should be(3)
      h3l.map(_._2.size).toSeq should be(Seq(3, 3, 3))
      val h3keys = h3l.keys.toSeq.sorted
      h3keys.last should be(graph.modified)
      h3l(h3keys(0)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "MD5", "C/" -> "SHA-512"))
      h3l(h3keys(1)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "", "C/" -> "SHA-512"))
      h3l(h3keys(2)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "", "C/" -> "SHA-512"))
    }
  }
  "Digest history should be projection of Graph.Retrospective: mostly damaged" in {
    withTempFolder { folder ⇒
      import TestDSL._

      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val sDataFreeze = SData(Digest.Key.freeze ->
        Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")))

      info("There are no retrospective records")
      Digest.history(graph) should be(empty)

      Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
      val graphLoaderWith1Record = Serialization.acquireLoader(folderA.toURI(), SData(Digest.Key.acquire -> false))
      val graphWith1Record = graphLoaderWith1Record.load()

      info("There is a single retrospective record")
      val h1l = Digest.history(graphLoaderWith1Record)
      val h1g = Digest.history(graph)
      h1l should be(h1g)
      h1l.size should be(1)
      h1l.head._2.size should be(3)

      model.takeRecord('baseLevel) { r ⇒ r.name = "222" }
      Serialization.freeze(graph)
      val graphLoaderWith2Records = Serialization.acquireLoader(folderA.toURI(), SData(Digest.Key.acquire -> false))

      info("There are two retrospective records")
      val h2l = Digest.history(graphLoaderWith2Records)
      val h2g = Digest.history(graph)
      h2l should be(h2g)
      h2l.size should be(2)
      h2l.head._2.size should be(3)
      val h2keys = h2l.keys.toSeq
      h2l(h2keys.head) should be(h2l(h2keys.last))

      info(s"Damage 'digest' in ${folderB} and 'type' in ${folderC}")
      val file2BDigestType = new File(folderB, s"digest/${Timestamp.dump(graphLoaderWith2Records.modified)}/type")
      val file2CDigestData = new File(folderC, s"digest/${Timestamp.dump(graphLoaderWith2Records.modified)}/digest")
      assert(file2BDigestType.delete(), "Unable to delete " + file2BDigestType)
      assert(file2CDigestData.delete(), "Unable to delete " + file2CDigestData)

      info("There are two retrospective records where one of them is broken")
      val graphLoaderWith2RecordsX = Serialization.acquireLoader(folderA.toURI(), SData(Digest.Key.acquire -> false))
      val h2lx = Digest.history(graphLoaderWith2RecordsX)
      val h2gx = Digest.history(graph)
      h2lx should be(h2gx)
      h2lx.size should be(2)
      h2lx.toSeq.sortBy(_._1).last._2.size should be(3)
      val h2keysx = h2lx.keys.toSeq
      h2lx(h2keysx.head) should not be (h2lx(h2keysx.last))

      model.takeRecord('baseLevel) { r ⇒ r.name = "333" }
      Serialization.freeze(graph)
      val graphLoaderWith3Records = Serialization.acquireLoader(folderA.toURI(), SData(Digest.Key.acquire -> false))

      info("There are three retrospective records")
      val h3l = Digest.history(graphLoaderWith3Records)
      val h3g = Digest.history(graph)
      h3l should be(h3g)
      h3l.size should be(3)
      h3l.map(_._2.size).toSeq should be(Seq(3, 3, 3))
      val h3keys = h3l.keys.toSeq.sorted
      h3keys.last should be(graph.modified)
      h3l(h3keys(0)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "MD5", "C/" -> "SHA-512"))
      h3l(h3keys(1)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "", "C/" -> "SHA-512"))
      h3l(h3keys(2)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "", "C/" -> "SHA-512"))

      info(s"Damage 'digest' and 'type' in ${folderC}")
      val file3BDigestType = new File(folderB, s"digest/${Timestamp.dump(graphLoaderWith3Records.modified)}/type")
      val file3BDigestData = new File(folderB, s"digest/${Timestamp.dump(graphLoaderWith3Records.modified)}/digest")
      val file3CDigestType = new File(folderC, s"digest/${Timestamp.dump(graphLoaderWith3Records.modified)}/type")
      val file3CDigestData = new File(folderC, s"digest/${Timestamp.dump(graphLoaderWith3Records.modified)}/digest")
      file3BDigestType should not be ('exists)
      file3BDigestData should not be ('exists)
      for (fileToModify ← Seq(file3CDigestType, file3CDigestData)) {
        val file = new RandomAccessFile(fileToModify, "rws")
        val text = new Array[Byte](fileToModify.length().toInt)
        file.readFully(text)
        file.seek(0)
        file.writeBytes("anoheutnoahetuh")
        file.write(text)
        file.close()
      }
      val graphLoaderWith3RecordsX = Serialization.acquireLoader(folderA.toURI(), SData(Digest.Key.acquire -> false))
      val h3lx = Digest.history(graphLoaderWith3RecordsX)
      val h3gx = Digest.history(graph)
      h3lx should be(h3gx)
      h3lx.size should be(3)
      h3lx.map(_._2.size).toSeq should be(Seq(3, 3, 3))
      val h3keysx = h3l.keys.toSeq.sorted
      h3keysx.last should be(graph.modified)
      h3lx(h3keysx(0)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "MD5", "C/" -> "SHA-512"))
      h3lx(h3keysx(1)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "", "C/" -> "SHA-512"))
      h3lx(h3keysx(2)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "", "C/" -> ""))
    }
  }
  "Digest history should be projection of Graph.Retrospective: everything failed" in {
    withTempFolder { folder ⇒
      import TestDSL._

      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val sDataFreeze = SData(Digest.Key.freeze ->
        Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("SHA-512")))

      Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())

      model.takeRecord('baseLevel) { r ⇒ r.name = "222" }
      Serialization.freeze(graph)
      model.takeRecord('baseLevel) { r ⇒ r.name = "333" }
      Serialization.freeze(graph, SData(Digest.Key.freeze ->
        Map(folderA.toURI -> SimpleDigest("MD5"), folderB.toURI -> SimpleDigest("MD5"), folderC.toURI -> SimpleDigest("MD5"))))
      val graphLoaderWith3Records = Serialization.acquireLoader(folderA.toURI(), SData(Digest.Key.acquire -> false))

      info("There are three retrospective records")
      val h3l = Digest.history(graphLoaderWith3Records)
      val h3g = Digest.history(graph)
      h3l should be(h3g)
      h3l.size should be(3)
      h3l.map(_._2.size).toSeq should be(Seq(3, 3, 3))
      val h3keys = h3l.keys.toSeq.sorted
      h3keys.last should be(graph.modified)
      h3l(h3keys(0)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "MD5", "C/" -> "SHA-512"))
      h3l(h3keys(1)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "MD5", "C/" -> "SHA-512"))
      h3l(h3keys(2)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "MD5", "B/" -> "MD5", "C/" -> "MD5"))

      val Seq(modified, modified2, modified3) = graph.retrospective.history.keys.toSeq.sorted
      assert(modified < modified3)

      info("Damage 1st modification")
      val file1ADigestType = new File(folderA, s"digest/${Timestamp.dump(modified)}/type")
      val file1ADigestData = new File(folderA, s"digest/${Timestamp.dump(modified)}/digest")
      val file1BDigestType = new File(folderB, s"digest/${Timestamp.dump(modified)}/type")
      val file1BDigestData = new File(folderB, s"digest/${Timestamp.dump(modified)}/digest")
      val file1CDigestType = new File(folderC, s"digest/${Timestamp.dump(modified)}/type")
      val file1CDigestData = new File(folderC, s"digest/${Timestamp.dump(modified)}/digest")
      file1ADigestType should not be ('exists)
      file1ADigestData should not be ('exists)
      file1BDigestType should be('exists)
      file1BDigestData should be('exists)
      file1CDigestType should be('exists)
      file1CDigestData should be('exists)
      for (fileToModify ← Seq(file1BDigestType, file1BDigestData, file1CDigestType, file1CDigestData)) {
        val file = new RandomAccessFile(fileToModify, "rws")
        val text = new Array[Byte](fileToModify.length().toInt)
        file.readFully(text)
        file.seek(0)
        file.writeBytes("anoheutnoahetuh")
        file.write(text)
        file.close()
      }
      info("Damage 2nd modification")
      val file2ADigestType = new File(folderA, s"digest/${Timestamp.dump(modified2)}/type")
      val file2ADigestData = new File(folderA, s"digest/${Timestamp.dump(modified2)}/digest")
      val file2BDigestType = new File(folderB, s"digest/${Timestamp.dump(modified2)}/type")
      val file2BDigestData = new File(folderB, s"digest/${Timestamp.dump(modified2)}/digest")
      val file2CDigestType = new File(folderC, s"digest/${Timestamp.dump(modified2)}/type")
      val file2CDigestData = new File(folderC, s"digest/${Timestamp.dump(modified2)}/digest")
      file2ADigestType should not be ('exists)
      file2ADigestData should not be ('exists)
      file2BDigestType should be('exists)
      file2BDigestData should be('exists)
      file2CDigestType should be('exists)
      file2CDigestData should be('exists)
      for (fileToModify ← Seq(file2BDigestType, file2BDigestData, file2CDigestType, file2CDigestData)) {
        val file = new RandomAccessFile(fileToModify, "rws")
        val text = new Array[Byte](fileToModify.length().toInt)
        file.readFully(text)
        file.seek(0)
        file.writeBytes("anoheutnoahetuh")
        file.write(text)
        file.close()
      }
      info("Damage 3rd modification")
      val file3ADigestType = new File(folderA, s"digest/${Timestamp.dump(graphLoaderWith3Records.modified)}/type")
      val file3ADigestData = new File(folderA, s"digest/${Timestamp.dump(graphLoaderWith3Records.modified)}/digest")
      val file3BDigestType = new File(folderB, s"digest/${Timestamp.dump(graphLoaderWith3Records.modified)}/type")
      val file3BDigestData = new File(folderB, s"digest/${Timestamp.dump(graphLoaderWith3Records.modified)}/digest")
      val file3CDigestType = new File(folderC, s"digest/${Timestamp.dump(graphLoaderWith3Records.modified)}/type")
      val file3CDigestData = new File(folderC, s"digest/${Timestamp.dump(graphLoaderWith3Records.modified)}/digest")
      file3ADigestType should be('exists)
      file3ADigestData should be('exists)
      file3BDigestType should be('exists)
      file3BDigestData should be('exists)
      file3CDigestType should be('exists)
      file3CDigestData should be('exists)
      for (fileToModify ← Seq(file3ADigestType, file3ADigestData, file3BDigestType, file3BDigestData, file3CDigestType, file3CDigestData)) {
        val file = new RandomAccessFile(fileToModify, "rws")
        val text = new Array[Byte](fileToModify.length().toInt)
        file.readFully(text)
        file.seek(0)
        file.writeBytes("anoheutnoahetuh")
        file.write(text)
        file.close()
      }
      info("There are three broken retrospective records")
      val graphLoaderWith3RecordsX = Serialization.acquireLoader(folderA.toURI(), SData(Digest.Key.acquire -> false))
      val h3lx = Digest.history(graphLoaderWith3RecordsX)
      val h3gx = Digest.history(graph)
      h3lx should be(h3gx)
      h3lx.size should be(3)
      h3lx.map(_._2.size).toSeq should be(Seq(3, 3, 3))
      val h3keysx = h3lx.keys.toSeq.sorted
      h3keysx.last should be(graph.modified)
      h3lx(h3keysx(0)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "", "C/" -> ""))
      h3lx(h3keysx(1)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "", "C/" -> ""))
      h3lx(h3keysx(2)).map {
        case (a, b) ⇒ (folder.toURI.relativize(a).toString(), b.algorithm)
      } should be(Map("A/" -> "", "B/" -> "", "C/" -> ""))
    }
  }
  "Digest should support 'writeFilter' and 'readFilter' options" in {
    withTempFolder { folder ⇒
      import TestDSL._

      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }
      graph.model.takeRecord('rA) { _.name = "1" }

      val writeFilter = (os: OutputStream, uri: URI, transport: Transport, sData: SData) ⇒ new TestOutputStream2(os)

      Serialization.freeze(graph, SData(SData.Key.writeFilter -> writeFilter, Digest.Key.freeze ->
        Map(folder.toURI -> SimpleDigest("SHA-512"))), folder.toURI)

      an[Throwable] should be thrownBy Serialization.acquire(folder.toURI)

      val readFilter = (is: InputStream, uri: URI, transport: Transport, sData: SData) ⇒ new TestInputStream2(is)
      val graph2 = Serialization.acquire(folder.toURI, SData(SData.Key.readFilter -> readFilter, Digest.Key.acquire -> true))

      graph.node.safeRead { node ⇒
        graph2.node.safeRead { node2 ⇒
          node2.iteratorRecursive.toVector should be(node.iteratorRecursive.toVector)
        }
      }
    }
  }

  val xorN = 123.toByte
  /** XOR data. */
  def xorWithKey(a: Array[Byte], key: Array[Byte]): Array[Byte] = {
    val out = new Array[Byte](a.length)
    for (i ← 0 until a.length)
      out(i) = (a(i) ^ key(i % key.length)).toByte
    out
  }

  override def beforeAll(configMap: org.scalatest.ConfigMap) {
    adjustLoggingBeforeAll(configMap)
    //addFileAppender()
  }

  /**
   * Test FilterInputStream
   */
  class TestInputStream(val inputStream: InputStream) extends FilterInputStream(inputStream) {
    @volatile var doneBytes = 0L

    override def read(): Int = {
      val result = super.read()
      if (result != -1)
        doneBytes = doneBytes + 1
      result
    }
    override def read(b: Array[Byte], off: Int, len: Int): Int = {
      val result = super.read(b, off, len)
      if (result > 0)
        doneBytes = doneBytes + result
      result
    }
    override def read(b: Array[Byte]): Int = read(b, 0, b.length)
  }
  /**
   * Test FilterOutputStream
   */
  class TestOutputStream(val outputStream: OutputStream) extends FilterOutputStream(outputStream) {
    @volatile var doneBytes = 0L

    override def write(b: Int) = {
      super.write(b)
      doneBytes = doneBytes + 1
    }
    override def write(b: Array[Byte], off: Int, len: Int) {
      super.write(b, off, len)
      doneBytes + len
    }
    override def write(b: Array[Byte]) = write(b, 0, b.length)
  }
  class TestSimple extends SimpleDigest
  /**
   * Test FilterInputStream
   */
  class TestInputStream2(val inputStream: InputStream) extends FilterInputStream(inputStream) {
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
  class TestOutputStream2(val outputStream: OutputStream) extends FilterOutputStream(outputStream) {
    override def write(b: Int) = ???
    override def write(b: Array[Byte], off: Int, len: Int) = outputStream.write(xorWithKey(b.drop(off).take(len), Array(xorN)))
    override def write(b: Array[Byte]) = write(b, 0, b.length)
  }
}
