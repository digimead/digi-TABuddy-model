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
import java.io.{ BufferedOutputStream, File, FileOutputStream, InputStream, PrintStream }
import java.net.URI
import java.util.UUID
import java.util.concurrent.atomic.AtomicReference
import org.apache.log4j.Level
import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.{ LoggingHelper, StorageHelper }
import org.digimead.tabuddy.model.{ Model, TestDSL }
import org.digimead.tabuddy.model.TestDSL
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.{ Serialization, YAMLSerialization }
import org.digimead.tabuddy.model.serialization.SData
import org.digimead.tabuddy.model.serialization.transport.{ Local, Transport }
import org.digimead.tabuddy.model.serialization.yaml.Timestamp
import org.hamcrest.{ BaseMatcher, Description }
import org.mockito.{ ArgumentCaptor, Matchers ⇒ MM, Mockito }
import org.scalatest.{ FreeSpec, Matchers }
import scala.collection.{ immutable, mutable }
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.ref.SoftReference

class SimpleSpec extends FreeSpec with Matchers with StorageHelper with LoggingHelper with Loggable {
  lazy val testDigest = Mockito.spy(new TestSimple)
  lazy val testTransport = Mockito.spy(new Local)
  @volatile var test = true

  @volatile var map1: immutable.Map[URI, mutable.Map[URI, Array[Byte]]] = null
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
      module.bind[Mechanism] identifiedBy ("Digest.Mechanism.Simple") toSingle { testDigest }
      module.bind[Transport] identifiedBy ("Serialization.Transport.Local") toSingle { testTransport }
    }) ~ org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
  }

  "Complex test for Raw, MD5, SHA512 digest" in {
    implicit val option = Mockito.atLeast(1)
    withMockitoLogCaptor {
      withTempFolder { folder ⇒
        import TestDSL._

        test = true
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
            immutable.Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> Simple("MD5"), folderC.toURI -> Simple("SHA-512")))
          Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
          folderA should be('exists)
          fileADigestType should not be ('exists)
          fileADigestData should not be ('exists)
          folderB should be('exists)
          fileBDigestType should be('exists)
          fileBDigestType.length() should be(11)
          scala.io.Source.fromFile(fileBDigestType).getLines.toList should be(Seq("simple", "MD5"))
          fileBDigestData should be('exists)
          scala.io.Source.fromFile(fileBDigestData).getLines.size should be(27)
          folderC should be('exists)
          fileCDigestType should be('exists)
          fileCDigestType.length() should be(15)
          scala.io.Source.fromFile(fileCDigestType).getLines.toList should be(Seq("simple", "SHA-512"))
          fileCDigestData should be('exists)
          scala.io.Source.fromFile(fileCDigestData).getLines.size should be(27)

          map1(folderA.getAbsoluteFile().toURI).size should be(0)
          map1(folderB.getAbsoluteFile().toURI).size should be(27)
          map1(folderC.getAbsoluteFile().toURI).size should be(27)

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
            def matches(sData: Any): Boolean = !sData.asInstanceOf[SData].isDefinedAt(Digest.historyPerURI) ||
              sData.asInstanceOf[SData](Digest.historyPerURI).keys.size == 1
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
          inOrder.verify(testDigest).approve(MM.argThat(new BaseMatcher {
            def matches(uri: Any): Boolean = uri.toString().startsWith(folderB.toURI().toString())
            def describeTo(description: Description) {}
          }), MM.anyObject())
          inOrder.verify(testTransport).read(MM.argThat(new BaseMatcher {
            def matches(uri: Any): Boolean = uri.toString().startsWith(folderB.toURI().toString())
            def describeTo(description: Description) {}
          }), MM.anyObject())
          inOrder.verify(testDigest).approve(MM.argThat(new BaseMatcher {
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
          inOrder.verify(testDigest, Mockito.times(1)).approve(MM.argThat(new BaseMatcher {
            def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
            def describeTo(description: Description) {}
          }), MM.anyObject())
          inOrder.verify(testTransport, Mockito.times(1)).read(MM.argThat(new BaseMatcher {
            def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
            def describeTo(description: Description) {}
          }), MM.anyObject())
          inOrder.verify(testDigest, Mockito.times(1)).approve(MM.argThat(new BaseMatcher {
            def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
            def describeTo(description: Description) {}
          }), MM.anyObject())
          // So there were 4 files with digest at all
          inOrder.verify(testDigest, Mockito.never()).approve(MM.anyObject(), MM.anyObject())
          inOrder.verify(testTransport, Mockito.never()).read(MM.anyObject(), MM.anyObject())

          Mockito.reset(testDigest)
          Mockito.reset(testTransport)
          val graphWithDigest = graphWithDigestLoader.load()
          Mockito.verify(testDigest, Mockito.times(13)).approve(MM.argThat(new BaseMatcher {
            def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
            def describeTo(description: Description) {}
          }), MM.anyObject())
          Mockito.verify(testDigest, Mockito.times(13)).approve(MM.argThat(new BaseMatcher {
            def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
            def describeTo(description: Description) {}
          }), MM.anyObject())
          Mockito.verify(testDigest, Mockito.times(0)).approve(MM.argThat(new BaseMatcher {
            def matches(uri: Any): Boolean = !uri.toString().startsWith(folderC.toURI().toString())
            def describeTo(description: Description) {}
          }), MM.anyObject())

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
  "Acquire process should skip sources with incorrect check sum" in {
    implicit val option = Mockito.atLeast(1)
    withMockitoLogCaptor {
      withTempFolder { folder ⇒
        import TestDSL._

        test = false
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
          immutable.Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> Simple("MD5"), folderC.toURI -> Simple("SHA-512")))
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
        inOrder.verify(testDigest, Mockito.times(1)).approve(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())
        inOrder.verify(testTransport, Mockito.times(1)).read(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())
        inOrder.verify(testDigest, Mockito.times(1)).approve(MM.argThat(new BaseMatcher {
          def matches(uri: Any): Boolean = uri.toString().startsWith(folderC.toURI().toString())
          def describeTo(description: Description) {}
        }), MM.anyObject())
        // So there were 4 files with digest at all
        inOrder.verify(testDigest, Mockito.never()).approve(MM.anyObject(), MM.anyObject())
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
    withTempFolder { folder ⇒
      import TestDSL._

      test = true
      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val sDataFreeze = SData(Digest.Key.freeze ->
        immutable.Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> Simple("MD5"), folderC.toURI -> Simple("SHA-512")))
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
  }
  "Acquire process should load partially damaged node with Digest.Key.acquire -> true successfully" in {
    withTempFolder { folder ⇒
      import TestDSL._

      test = true
      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val sDataFreeze = SData(Digest.Key.freeze ->
        immutable.Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> Simple("MD5"), folderC.toURI -> Simple("SHA-512")))
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
  }
  "Acquire process should fail while loading fully damaged node with Digest.Key.acquire -> true" in {
    withTempFolder { folder ⇒
      import TestDSL._

      test = true
      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val sDataFreeze = SData(Digest.Key.freeze ->
        immutable.Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> Simple("MD5"), folderC.toURI -> Simple("SHA-512")))
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
  }
  "Acquire process should load graph without fully damaged node with Digest.Key.acquire -> true and force -> true" in {
    withTempFolder { folder ⇒
      import TestDSL._

      test = true
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
        immutable.Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> Simple("MD5"), folderC.toURI -> Simple("SHA-512")))
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
      val graphWithDigest = Serialization.acquire(folderB.getAbsoluteFile().toURI(), sDataAcquire.updated(Digest.Key.acquire, false))
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
  }
  "Acquire process should handle multiple digest chunks" in {
    withTempFolder { folder ⇒
      import TestDSL._

      test = false
      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ r.name = "111" } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val sDataFreeze = SData(Digest.Key.freeze ->
        immutable.Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> Simple("MD5"), folderC.toURI -> Simple("SHA-512")))
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
        immutable.Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> Digest.NoDigest, folderC.toURI -> Digest.NoDigest))
      Serialization.freeze(graph2, sDataFreeze2, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
      val graph3 = Serialization.acquire(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))

      graph3.model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ r.name = "333" } } }
      val sDataFreeze3 = SData(Digest.Key.freeze ->
        immutable.Map(folderA.toURI -> Simple("MD2"), folderB.toURI -> Simple("MD5"), folderC.toURI -> Digest.NoDigest))
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
  }
  "Acquire process should fail to load graph if one or more chunks without digest and Digest.Key.acquire -> true" taggedAs (TestDSL.Mark) in {
    withTempFolder { folder ⇒
      import TestDSL._

      test = false
      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ r.name = "111" } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val sDataFreeze = SData(Digest.Key.freeze ->
        immutable.Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> Simple("MD5"), folderC.toURI -> Simple("SHA-512")))
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
        immutable.Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> Digest.NoDigest, folderC.toURI -> Digest.NoDigest))
      Serialization.freeze(graph2, sDataFreeze2, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
      val graph3 = Serialization.acquire(folderB.getAbsoluteFile().toURI(), SData(Digest.Key.acquire -> false))

      graph3.model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ r.name = "333" } } }
      val sDataFreeze3 = SData(Digest.Key.freeze ->
        immutable.Map(folderA.toURI -> Simple("MD2"), folderB.toURI -> Simple("MD5"), folderC.toURI -> Digest.NoDigest))
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
  }
  "Acquire process should load only trusted part of graph if one or more chunks without digest, Digest.Key.acquire -> true and force -> true" in {
    withTempFolder { folder ⇒
      import TestDSL._

      test = true
      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val sDataFreeze = SData(Digest.Key.freeze ->
        immutable.Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> Simple("MD5"), folderC.toURI -> Simple("SHA-512")))
      Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
      val sDataAcquire = SData(Digest.Key.acquire -> true)
      val graph2 = Serialization.acquire(folderB.getAbsoluteFile().toURI(), sDataAcquire.updated(Digest.Key.acquire, false))
      //graph2.
    }
  }
  "Freeze process should use the latest digest configuration by default" in {
    withTempFolder { folder ⇒
      import TestDSL._

      test = true
      // graph
      val graph = Graph[Model]('john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
      val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
      model.takeRecord('baseLevel) { r ⇒ r.takeRecord('level1a) { r ⇒ r.takeRecord('level2a) { r ⇒ } } }

      val folderA = new File(folder, "A")
      val folderB = new File(folder, "B")
      val folderC = new File(folder, "C")
      val sDataFreeze = SData(Digest.Key.freeze ->
        immutable.Map(folderA.toURI -> Digest.NoDigest, folderB.toURI -> Simple("MD5"), folderC.toURI -> Simple("SHA-512")))
      Serialization.freeze(graph, sDataFreeze, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())
      val sDataAcquire = SData(Digest.Key.acquire -> true)
      val graph2 = Serialization.acquire(folderB.getAbsoluteFile().toURI(), sDataAcquire.updated(Digest.Key.acquire, false))
      //graph2.
    }
  }

  override def beforeAll(configMap: org.scalatest.ConfigMap) {
    adjustLoggingBeforeAll(configMap)
    //addFileAppender()
  }

  class TestSimple extends Simple {
    override def afterFreeze(parameters: Mechanism.Parameters, graph: Graph[_ <: Model.Like], transport: Transport, sData: SData) = {
      val result = super.afterFreeze(parameters, graph, transport, sData)
      map1 = sData(Simple.Key.digestMap)
      result
    }
    /** Just invoked after read beginning. */
    override def beforeRead(parameters: Mechanism.Parameters, context: AtomicReference[SoftReference[AnyRef]],
      modified: Element.Timestamp, is: InputStream, uri: URI, transport: Transport, sData: SData): InputStream = {
      val result = super.beforeRead(parameters, context, modified, is, uri, transport, sData)
      val map = getDigestMap(parameters, context, modified, transport, sData)
      val map0 = map1(sData(SData.Key.storageURI))
      if (test)
        map.forall { case (k, v) ⇒ v.deep == map0(k).deep } should be(true)
      result
    }
    override def approve(resourceURI: URI, sData: SData) = super.approve(resourceURI, sData)
    override def refuse(resourceURI: URI, sData: SData) = super.refuse(resourceURI, sData)
  }
}
