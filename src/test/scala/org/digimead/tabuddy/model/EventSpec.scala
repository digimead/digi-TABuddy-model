/**
 * TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2012-2013 Alexey Aksenov ezh@ezh.msk.ru
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

package org.digimead.tabuddy.model

import java.util.UUID

import scala.collection.mutable
import scala.collection.mutable.Publisher
import scala.collection.mutable.Subscriber

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.LoggingHelper
import org.digimead.lib.test.StorageHelper
import org.digimead.tabuddy.model.element.Value.string2someValue
import org.digimead.tabuddy.model.graph.Event
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Graph.graph2interface
import org.digimead.tabuddy.model.serialization.BuiltinSerialization
import org.digimead.tabuddy.model.serialization.Serialization
import org.scalatest.FunSpec
import org.scalatest.Matchers

import TestDSL._

class EventSpec extends FunSpec with Matchers with StorageHelper with LoggingHelper with Loggable {
  lazy val diConfig = org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default
  after { adjustLoggingAfter }
  before {
    DependencyInjection(diConfig, false)
    adjustLoggingBefore
  }

  describe("An Event") {
    it("should be fired while graph is changed") {
      withTempFolder { folder ⇒
        import TestDSL._

        val events = mutable.ListBuffer[Event]()
        val graph = Graph[Model]('john1, Model.scope, BuiltinSerialization.Identifier, UUID.randomUUID()) { g ⇒
          g.storages = g.storages :+ folder.getAbsoluteFile().toURI()
          g.subscribe(new g.Sub {
            def notify(pub: g.Pub, event: Event) = event match {
              case Event.NodeChange(a, b, c) ⇒ if (b == c) fail("State is the same: " + b)
              case event: Event.GraphChange ⇒ events += event
              case _ ⇒
            }
          })
        }
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
        graph.nodes.size should be(6)
        testCreation(graph, events)

        val graphCopy = graph.copy(origin = 'john2)(g ⇒ g.subscribe(new g.Sub {
          def notify(pub: g.Pub, event: Event) = event match {
            case Event.NodeChange(a, b, c) ⇒ if (b == c) fail("State is the same: " + b)
            case event: Event.GraphChange ⇒ events += event
            case _ ⇒
          }
        }))
        graphCopy.nodes.size should be(6)
        testCreation(graphCopy, events)

        val timestamp = Serialization.freeze(graph)
        val graph2Loader = Serialization.acquireLoaders(graph.origin, folder.toURI).last
        graph2Loader.graph.subscribe(new Subscriber[Event, Publisher[Event]] {
            def notify(pub: Publisher[Event], event: Event) = event match {
              case Event.NodeChange(a, b, c) ⇒ if (b == c) fail("State is the same: " + b)
              case event: Event.GraphChange ⇒ events += event
              case _ ⇒
            }
          })
        val graph2 = graph2Loader.load
        graph2.nodes.size should be(6)
        testCreation(graphCopy, events)
      }
    }
    /*
    (events.remove(0): @unchecked) match {
      case Event.NodeChange(a, b, c) ⇒
        a should be(graph.model.eNode)
        b.projectionBoxes should be('empty)
        c.projectionBoxes should not be ('empty)
        c.children should be('empty)
    }

    (events.remove(0): @unchecked) match {
      case Event.NodeChange(a, b, c) ⇒
        a should be(graph.model.eNode)
        b.children should be('empty)
        c.children should not be ('empty)
    }

    (events.remove(0): @unchecked) match {
      case Event.NodeChange(a, b, c) ⇒
        a should be((graph.model & RecordLocation('baseLevel)).eNode)
        b.projectionBoxes should be('empty)
        c.projectionBoxes should not be ('empty)
        c.children should be('empty)
    }

    (events.remove(0): @unchecked) match {
      case Event.NodeChange(a, b, c) ⇒
        a should be((graph.model & RecordLocation('baseLevel)).eNode)
        b.children should be('empty)
        c.children should not be ('empty)
    }

    (events.remove(0): @unchecked) match {
      case Event.NodeChange(a, b, c) ⇒
        a should be((graph.model & RecordLocation('baseLevel) & RecordLocation('level1a)).eNode)
        b.projectionBoxes should be('empty)
        c.projectionBoxes should not be ('empty)
        c.children should be('empty)
    }

    (events.remove(0): @unchecked) match {
      case Event.NodeChange(a, b, c) ⇒
        a should be((graph.model & RecordLocation('baseLevel) & RecordLocation('level1a)).eNode)
        b.children should be('empty)
        c.children should not be ('empty)
    }
    */
  }
  def testCreation(graph: Graph[_ <: Model.Like], events: mutable.ListBuffer[Event]) {
    import TestDSL._

    /*
     * model
     */

    (events.remove(0): @unchecked) match {
      case Event.GraphChange(a, b, c) ⇒
        a should be(graph.model.eNode)
        b should be(null)
        c should be(graph.model.eNode)
    }

    /*
     * model -> baseLevel
     */

    (events.remove(0): @unchecked) match {
      case ev @ Event.GraphChange(a, b, c) ⇒
        a should be(graph.model.eNode)
        ev.getOldValue() should be(null)
        ev.getNewValue() should be((graph.model & RecordLocation('baseLevel)).eNode)
    }

    /*
     * model -> baseLevel -> level1a
     */

    (events.remove(0): @unchecked) match {
      case ev @ Event.GraphChange(a, b, c) ⇒
        a should be((graph.model & RecordLocation('baseLevel)).eNode)
        ev.getOldValue() should be(null)
        ev.getNewValue() should be((graph.model & RecordLocation('baseLevel) & RecordLocation('level1a)).eNode)
    }

    Option(events.remove(0)).map { ev ⇒
      ev.getPropertyName() should be("level1a")
      ev.getSource() should be((graph.model & RecordLocation('baseLevel) & RecordLocation('level1a)).eNode)
      ev.getOldValue() should be(null)
      ev.getNewValue() should be((graph.model & RecordLocation('baseLevel) & RecordLocation('level1a) & RecordLocation('level2a)).eNode)
    } getOrElse { fail("Event not found.") }

    Option(events.remove(0)).map { ev ⇒
      ev.getPropertyName() should be("baseLevel")
      ev.getSource() should be((graph.model & RecordLocation('baseLevel)).eNode)
      ev.getOldValue() should be(null)
      ev.getNewValue() should be((graph.model & RecordLocation('baseLevel) & RecordLocation('level1b)).eNode)
    } getOrElse { fail("Event not found.") }

    Option(events.remove(0)).map { ev ⇒
      ev.getPropertyName() should be("level1b")
      ev.getSource() should be((graph.model & RecordLocation('baseLevel) & RecordLocation('level1b)).eNode)
      ev.getOldValue() should be(null)
      ev.getNewValue() should be((graph.model & RecordLocation('baseLevel) & RecordLocation('level1b) & RecordLocation('level2b)).eNode)
    } getOrElse { fail("Event not found.") }

    events should be('empty)
  }
  override def beforeAll(configMap: org.scalatest.ConfigMap) { adjustLoggingBeforeAll(configMap) }
}
