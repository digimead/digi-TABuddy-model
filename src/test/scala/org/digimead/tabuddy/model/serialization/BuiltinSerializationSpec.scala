/**
 * TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2012-2014 Alexey Aksenov ezh@ezh.msk.ru
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

import java.io.File
import java.util.UUID
import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.XLoggable
import org.digimead.lib.test.{ LoggingHelper, StorageHelper }
import org.digimead.tabuddy.model.TestDSL
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.element.{ Element, Value }
import org.digimead.tabuddy.model.graph.{ Graph, Node }
import org.digimead.tabuddy.model.serialization.transport.Transport
import org.digimead.tabuddy.model.serialization.yaml.Timestamp
import org.digimead.tabuddy.model.{ Model, Record }
import org.scalatest.{ FunSpec, Matchers }
import scala.collection.immutable

class BuiltinSerializationSpec extends FunSpec with Matchers with StorageHelper with LoggingHelper with XLoggable {
  before { DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false) }

  describe("A SimpleSerialization") {
    it("should fail when storage URI and SData.Key.explicitStorages provided") {
      withTempFolder { folder ⇒
        import TestDSL._

        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val append = Serialization.Storages((graph.storages :+ folder.getAbsoluteFile().toURI).
          map(Serialization.Storages.Simple))
        an[IllegalArgumentException] should be thrownBy Serialization.freeze(graph, SData(SData.Key.explicitStorages -> append), folder.getAbsoluteFile().toURI)
        Serialization.freeze(graph, SData(SData.Key.explicitStorages -> append))
        Serialization.freeze(graph, folder.getAbsoluteFile().toURI)
      }
    }
    it("should provide serialization mechanism for graph") {
      withTempFolder { folder ⇒
        import TestDSL._

        // graph
        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        val record_0 = model.getRecord('baseLevel) { r ⇒
          r.getRecord('level1a) { r ⇒
            r.getRecord('level2a) { r ⇒
              r.name = "record_2a"
            }
            r.name = "record_1a"
          }
          r.getRecord('level1b) { r ⇒
            r.getRecord('level2b) { r ⇒
              r.name = "record_2b"
            }
            r.name = "record_1b"
          }
          r.name = "record_0"
        }.eRelative
        graph.node.safeRead(_.iteratorRecursive.size) should be(5)
        model.eNode.safeRead(_.size) should be(1)
        model.copy(model.eStash.copy(property = model.eStash.property + ('a -> immutable.HashMap(DSLType.classSymbolMap(classOf[String]) -> new Value.Static("123")))))
        log.___glance("Model dump:\n" + model.eDump(false))

        // serialize
        new File(folder, "john1") should not be ('exists)
        graph.retrospective.history should be('empty)
        val before = model.eBox.modified
        val timestamp1 = Serialization.freeze(graph, folder.getAbsoluteFile().toURI)
        val graphX = graph.copy() { g ⇒ }
        graphX.retrospective should be(graph.retrospective)
        Some(timestamp1) should be(graph.retrospective.last)
        timestamp1 should be(graph.node.modified)
        timestamp1 should be(graph.modified)
        val after = model.eBox.modified
        after should be(before)
        graph.retrospective.history should have size (1)
        graph.retrospective.head should be(Some(graph.modified))

        new File(folder, "data") should be('directory)
        new File(folder, "data").length() should be > (0L)
        new File(folder, "descriptor.yaml") should be('file)
        new File(folder, "descriptor.yaml").length() should be > (0L)
        new File(folder, "data/e john1 {0609C486}/node descriptor-%s.yaml".format(Timestamp.dump(model.eNode.modified))) should be('file)
        new File(folder, "data/e john1 {0609C486}/node descriptor-%s.yaml".format(Timestamp.dump(model.eNode.modified))).length() should be > (0L)
        new File(folder, "data/e john1 {0609C486}/e baseLevel {92A93F33}/node descriptor-%s.yaml".
          format(Timestamp.dump((model | RecordLocation('baseLevel)).eNode.modified))) should be('file)
        new File(folder, "data/e john1 {0609C486}/e baseLevel {92A93F33}/node descriptor-%s.yaml".
          format(Timestamp.dump((model | RecordLocation('baseLevel)).eNode.modified))).length() should be > (0L)
        new File(folder, "data/e john1 {0609C486}/e baseLevel {92A93F33}/e level1a {0428D0D4}/node descriptor-%s.yaml".
          format(Timestamp.dump((model | RecordLocation('baseLevel) | RecordLocation('level1a)).eNode.modified))) should be('file)
        new File(folder, "data/e john1 {0609C486}/e baseLevel {92A93F33}/e level1a {0428D0D4}/node descriptor-%s.yaml".
          format(Timestamp.dump((model | RecordLocation('baseLevel) | RecordLocation('level1a)).eNode.modified))).length() should be > (0L)
        new File(folder, "data/e john1 {0609C486}/e baseLevel {92A93F33}/e level1b {0428D0D5}/node descriptor-%s.yaml".
          format(Timestamp.dump((model | RecordLocation('baseLevel) | RecordLocation('level1b)).eNode.modified))) should be('file)
        new File(folder, "data/e john1 {0609C486}/e baseLevel {92A93F33}/e level1b {0428D0D5}/node descriptor-%s.yaml".
          format(Timestamp.dump((model | RecordLocation('baseLevel) | RecordLocation('level1b)).eNode.modified))).length() should be > (0L)
        new File(folder, "data/e john1 {0609C486}/e baseLevel {92A93F33}/e level1a {0428D0D4}/e level2a {0428D0F3}/node descriptor-%s.yaml".
          format(Timestamp.dump((model | RecordLocation('baseLevel) | RecordLocation('level1a) | RecordLocation('level2a)).eNode.modified))) should be('file)
        new File(folder, "data/e john1 {0609C486}/e baseLevel {92A93F33}/e level1a {0428D0D4}/e level2a {0428D0F3}/node descriptor-%s.yaml".
          format(Timestamp.dump((model | RecordLocation('baseLevel) | RecordLocation('level1a) | RecordLocation('level2a)).eNode.modified))).length() should be > (0L)
        new File(folder, "data/e john1 {0609C486}/e baseLevel {92A93F33}/e level1b {0428D0D5}/e level2b {0428D0F4}/node descriptor-%s.yaml".
          format(Timestamp.dump((model | RecordLocation('baseLevel) | RecordLocation('level1b) | RecordLocation('level2b)).eNode.modified))) should be('file)
        new File(folder, "data/e john1 {0609C486}/e baseLevel {92A93F33}/e level1b {0428D0D5}/e level2b {0428D0F4}/node descriptor-%s.yaml".
          format(Timestamp.dump((model | RecordLocation('baseLevel) | RecordLocation('level1b) | RecordLocation('level2b)).eNode.modified))).length() should be > (0L)
        //val testTxtSource = scala.io.Source.fromFile(new File(folder, "data/john1/description"))
        //val str = testTxtSource.getLines.mkString("\n")
        //testTxtSource.close()

        // deserialize
        val graph2 = Serialization.acquire(folder.toURI)
        graph2.retrospective.history should be(graph.retrospective.history)
        graph2.retrospective should be(graph.retrospective)

        /* compare graph */
        graph2 should not be (null)
        graph2 should be(graph)
        graph2.origin.name should be(graph.origin.name)
        graph2.created should be(graph.created)
        graph2.modelType should be(graph.modelType)
        graph2.modified should be(graph.modified)
        graph.node.safeRead { node ⇒
          graph2.node.safeRead { node2 ⇒
            /* compare node */
            node2 should be(node)
            node2.id.name should be(node.id.name)
            node2.unique should be(node.unique)
            node2.children should be(node.children)
            node2.modified should be(node.modified)
            node2.projectionBoxes should be(node.projectionBoxes)
            node2.rootBox.coordinate should be(node.rootBox.coordinate)
            node2.rootBox.elementUniqueId should be(node.rootBox.elementUniqueId)
            node2.rootBox.modified should be(node.rootBox.modified)
            node2.rootBox.serialization should be(node.rootBox.serialization)

            node2.graph should be(graph2)
            node2.parentNodeReference.get should be(None)
            node2.elementType should be(node.elementType)

            /* compare root element box */
            node2.rootBox.coordinate should be(node.rootBox.coordinate)
            node2.rootBox.elementUniqueId should be(node.rootBox.elementUniqueId)
            node2.rootBox.modified should be(node.rootBox.modified)
            node2.rootBox.node should be(node.rootBox.node)
            node2.rootBox.serialization should be(node.rootBox.serialization)

            /* compare root element */
            val element = node.rootBox.e
            val element2 = node2.rootBox.e

            element should not be (null)
            element2 should not be (null)
            node.rootBox.getModified should be(None)
            node2.rootBox.getModified should be(None)
            element.ne(element2) should be(true)
            element2 should be(element)

            node2.iteratorRecursive.toVector should be(node.iteratorRecursive.toVector)

            // check
            // model
            graph2.node.safeRead(_.iteratorRecursive.toSeq) should have size (5)
            // container always point to current active model
            graph2.model.eId.name should be(graph.model.eId.name)
            graph2.model.eUniqueId should be(graph.model.eUniqueId)
            graph2.model.eNode.unique should be(graph.model.eNode.unique)
            graph2.model.modified should be(graph.model.modified)
            graph2.model.eModel should be(graph2.model)
            graph2.model.eModel should be(graph.model)
            graph2.model.eStash.property should be(graph.model.eStash.property)
            graph2.model.eStash.property('AAAKey)(DSLType.classSymbolMap(classOf[String])).get should be("AAA")
            graph2.model.e(graph2.model.eReference) should not be ('empty)
            // record
            graph2.node.safeRead(_.head).eq(graph.node.safeRead(_.head)) should be(false)
            graph2.node.safeRead(_.head).safeRead(_.rootBox.e).eModel should be(graph2.model)
            graph2.model.e(graph2.node.safeRead(_.head).safeRead(_.rootBox.e).eReference) should not be ('empty)
          }
        }

        val graphModification = graph.modified
        val baseLevel = (graph.model | RecordLocation('baseLevel)).eRelative
        baseLevel.name = "321"
        graph.modified should be > (graphModification)

        Serialization.freeze(graph)
        val timestamp2 = Serialization.freeze(graph)
        Some(timestamp2) should be(graph.retrospective.last)
        timestamp2 should be(graph.node.modified)
        timestamp2 should be(graph.modified)
        graph.retrospective.history should have size (2)
        graph.retrospective.last should be(Some(graph.modified))
        graph.retrospective.head should be(Some(graphModification))

        val graph1x = Serialization.acquire(folder.toURI, graph.retrospective.head)
        Some(graph1x.modified) should be(graph.retrospective.head)
        Some(graph2.modified) should be(graph.retrospective.head)

        var elementCounter = 0
        def loadMonitor(ancestors: Seq[Node.ThreadUnsafe[_ <: Element]], nodeDescriptor: Serialization.Descriptor.Node[Element]) = {
          elementCounter += 1
          elementCounter match {
            case 1 ⇒ ancestors.map(_.id) should be('empty)
            case 2 ⇒ ancestors.map(_.id) should be(Seq('john1))
            case 3 ⇒ ancestors.map(_.id) should be(Seq('john1, 'baseLevel))
            case 4 ⇒ ancestors.map(_.id) should be(Seq('john1, 'baseLevel, 'level1a))
            case 5 ⇒ ancestors.map(_.id) should be(Seq('john1, 'baseLevel))
            case 6 ⇒ ancestors.map(_.id) should be(Seq('john1, 'baseLevel, 'level1b))
          }
          nodeDescriptor
        }
        val graph2x = Serialization.acquire(folder.toURI, graph.retrospective.last, SData(SData.Key.acquireT -> loadMonitor _))
        elementCounter should be(6)
        val size = graph.node.safeRead(_.iteratorRecursive.size)
        val size2 = graph2.node.safeRead(_.iteratorRecursive.size)
        val size1x = graph1x.node.safeRead(_.iteratorRecursive.size)
        val size2x = graph2x.node.safeRead(_.iteratorRecursive.size)

        assert(size === size2)
        assert(size === size1x)
        assert(size === size2x)

        graph2.node.modified should be(graph1x.node.modified)
        graph2.node.safeRead { node ⇒
          graph1x.node.safeRead { node2 ⇒
            node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
          }
        } should be(true)
        graph.node.safeRead { node ⇒
          graph2x.node.safeRead { node2 ⇒
            node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
          }
        } should be(true)
        graph1x.node.safeRead { node ⇒
          graph2x.node.safeRead { node2 ⇒
            node.iteratorRecursive.zip(node2.iteratorRecursive).forall {
              case (a, b) ⇒
                a.ne(b) && {
                  if (a.id == 'john1 || a.id == 'baseLevel)
                    a.modified != b.modified
                  else
                    a.modified == b.modified
                }
            }
          }
        } should be(true)
      }
    }
    it("should correctly serialize elements") {
      withTempFolder { folder ⇒
        import TestDSL._

        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        val record_root = model.getRecord('root) { r ⇒
          r.getRecord('level2) { r ⇒
            r.name = "123"
            r.getRecord('level3) { r ⇒
              r.name = "456"
            }
          }
        }
        val record_level2 = record_root & RecordLocation('level2)
        val record_level3 = record_level2 & RecordLocation('level3)

        model.e(record_level3.eReference).get.eq(record_level3) should be(true)
        val note = model.note('note)
        val task = model.task('task)
        model.eNode.safeRead(_.iteratorRecursive.toSeq) should have size (5)

        Serialization.freeze(graph, folder.getAbsoluteFile().toURI)
        val graph2 = Serialization.acquire(folder.toURI)

        graph.node.safeRead { node ⇒
          graph2.node.safeRead { node2 ⇒
            node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
          }
        } should be(true)

        val oldModification = graph.node.modified
        (model & RecordLocation('root) & RecordLocation('level2) & RecordLocation('level3)).name should be("456")
        record_level3.eRelative.name = "789"
        (model & RecordLocation('root) & RecordLocation('level2) & RecordLocation('level3)).name should be("789")
        graph.model.e(record_level3.eReference).map(_.asInstanceOf[Record].name) should be(Some("789"))
        val newModification = graph.node.modified
        newModification should be > (oldModification)
        newModification should be(record_level3.eRelative.modified)
        (model & RecordLocation('root)).eNode.modified should be(newModification)
        (model & RecordLocation('root) & RecordLocation('level2)).eNode.modified should be(newModification)
        (model & RecordLocation('root) & RecordLocation('level2) & RecordLocation('level3)).eNode.modified should be(newModification)

        val record_level3_rel = graph2.model.e(record_level3.eReference).flatMap(_.eAs[Record]).get.eRelative
        val record_level2_node = record_level3_rel.eNode.parent.get
        record_level2_node.id.name should be("level2")
        record_level2_node.safeRead(_.children) should have size (1)
        record_level3_rel.eModel.eq(graph2.model) should be(true)
        val record_level2_rel = record_level2_node.rootBox.e.eAs[Record].get.eRelative
        record_level2_rel.name should be("123")
        record_level2_node.safeRead(_.children) should have size (1)
        record_level2_node.safeRead(_.children.head.rootBox.e) should be(record_level3_rel.absolute)
        record_level3_rel.name should be("456")
        record_level3_rel.eNode.safeRead(_.children) should be('empty)

        record_level3_rel.eReference should be(record_level3.eReference)
        record_level3_rel.eReference.model.hashCode() should be(record_level3.eReference.model.hashCode())
        record_level3_rel.eReference.node.hashCode() should be(record_level3.eReference.node.hashCode())
        record_level3_rel.eReference.origin.hashCode() should be(record_level3.eReference.origin.hashCode())
        record_level3_rel.eReference.coordinate.hashCode() should be(record_level3.eReference.coordinate.hashCode())
        record_level3_rel.eReference.hashCode() should be(record_level3.eReference.hashCode())

        graph.model.e(record_level2.eReference) should be(Some(record_level2))
        graph.model.e(record_level2_rel.eReference) should be(Some(record_level2))

        record_level2.eRelative.name = "111"

        Serialization.freeze(graph)
        val graph3 = Serialization.acquire(graph.storages.head)

        graph2.model.e(record_level2.eReference).flatMap(_.eAs[Record]).get.name should be("123")
        graph.model.e(record_level2.eReference).flatMap(_.eAs[Record]).get.name should be("111")
        graph3.model.e(record_level2.eReference).flatMap(_.eAs[Record]).get.name should be("111")

        graph.node.safeRead { node ⇒
          graph3.node.safeRead { node3 ⇒
            node.iteratorRecursive.corresponds(node3.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
          }
        } should be(true)
      }
    }
    it("should filter elements on save/load") {
      withTempFolder { folder ⇒
        import TestDSL._

        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        val record_root = model.getRecord('root) { r ⇒
          r.getRecord('level2) { r ⇒
            r.name = "123"
            r.getRecord('level3) { r ⇒
              r.name = "456"
            }
          }
        }
        val record_level2 = record_root & RecordLocation('level2)
        val record_level3 = record_level2 & RecordLocation('level3)

        model.e(record_level3.eReference).get.eq(record_level3) should be(true)
        val note = model.note('note)
        val task = model.task('task)
        model.eNode.safeRead(_.iteratorRecursive.toSeq) should have size (5)

        // save

        info("test filter 'remove all children' on save.")
        val fFilterSave1 = (node: Node.ThreadUnsafe[Element]) ⇒
          Node(node.id, node.unique, node.state.copy(children = Seq()), node.modified)(node.elementType)
        Serialization.freeze(graph, SData(SData.Key.freezeT -> fFilterSave1), folder.getAbsoluteFile().toURI())
        graph.retrospective.history should have size (1)

        val graph2 = Serialization.acquire(folder.toURI)
        graph2.node.safeRead(_.children) should be('empty)
        graph2.node.safeRead(_.iteratorRecursive.size) should be(0)

        graph2.modified should be(graph.modified)

        info("test filter 'reset timestamp to (0, 0)' on save.")
        @volatile var x = 0
        val fFilterSave2 = (node: Node.ThreadUnsafe[Element]) ⇒ {
          x += 1
          Node(node.id, node.unique, node.state, Element.timestamp(0, 0))(node.elementType)
        }
        Serialization.freeze(graph, SData(SData.Key.freezeT -> fFilterSave2))
        graph.retrospective.history should have size (2)
        val graph3 = Serialization.acquire(folder.toURI, Some(Element.timestamp(0, 0)))
        graph3.node.safeRead(_.iteratorRecursive.size) should be(5)
        graph3.modified should be(Element.timestamp(0, 0))
        graph3.node.modified should be(Element.timestamp(0, 0))
        graph3.node.safeRead(_.iteratorRecursive.forall(_.modified == Element.timestamp(0, 0))) should be(true)
        x should be(11) // model + all elements + children

        info("test filter 'add x before nodes id' on save.")
        // We MUST copy element boxes since
        // 1. some of original element boxes are unmodified and will not be saved
        // 2. element box container is changed (node id)
        //  so copies will be saved as modified
        val fFilterSave3 = (node: Node.ThreadUnsafe[Element]) ⇒
          Node(Symbol("x" + node.id.name), node.unique, node.state.copy(projectionBoxes =
            immutable.HashMap(node.state.projectionBoxes.map { case (k, v) ⇒ k -> v.copy() }.toSeq: _*)), node.modified)(node.elementType)
        Serialization.freeze(graph, SData(SData.Key.freezeT -> fFilterSave3))
        graph.retrospective.history should have size (2)

        val graph4 = Serialization.acquire(folder.toURI)
        graph.node.safeRead { node ⇒
          graph4.node.safeRead { node4 ⇒
            node.iteratorRecursive.corresponds(node4.iteratorRecursive) { (a, b) ⇒
              a.ne(b) && a.modified == b.modified && ("x" + a.id.name) == b.id.name
            }
          }
        } should be(true)

        // load

        info("test filter 'remove all children' on load.")
        val fFilterLoad1 = (ancestors: Seq[Node.ThreadUnsafe[_ <: Element]], nodeDescriptor: Serialization.Descriptor.Node[Element]) ⇒
          nodeDescriptor.copy(children = Seq())
        val graph5 = Serialization.acquire(folder.toURI, SData(SData.Key.acquireT -> fFilterLoad1))
        graph5.node.safeRead(_.children) should be('empty)
        graph5.node.safeRead(_.iteratorRecursive.size) should be(0)
      }
    }
    it("should have a simple API") {
      withTempFolder { folder ⇒
        import TestDSL._

        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒ }
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        val record_root = model.getRecord('root) { r ⇒
          r.getRecord('level2) { r ⇒
            r.name = "123"
            r.getRecord('level3) { r ⇒
              r.name = "456"
            }
          }
        }

        Serialization.freeze(graph, folder.getAbsoluteFile().toURI())
        val graph1x = Serialization.acquire(folder.toURI)

        graph.node.safeRead { node ⇒
          graph1x.node.safeRead { node2 ⇒
            node.iteratorRecursive.corresponds(node2.iteratorRecursive) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
          }
        } should be(true)
      }
    }
  }

  override def beforeAll(configMap: org.scalatest.ConfigMap) { adjustLoggingBeforeAll(configMap) }
}
