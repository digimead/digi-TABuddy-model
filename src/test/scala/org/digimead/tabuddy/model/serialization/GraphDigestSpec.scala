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

import java.io.File
import java.util.UUID
import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.{ LoggingHelper, StorageHelper }
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.TestDSL
import org.digimead.tabuddy.model.graph.Graph
import org.scalatest.{ FreeSpec, Matchers }
import scala.collection.mutable
import java.net.URI

class GraphDigestSpec extends FreeSpec with Matchers with StorageHelper with LoggingHelper with Loggable {
  before { DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false) }

  "Complex test for Raw, MD5, SHA512 digest" in {
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

      val digestArgorithm = new mutable.HashMap[Option[URI], String] with mutable.SynchronizedMap[Option[URI], String]
      digestArgorithm(Some(folderB.toURI())) = "MD5"
      digestArgorithm(Some(folderC.toURI())) = "SHA-512"
      val sData = SData(SData.Key.digestAlgorithm -> digestArgorithm)
      Serialization.freeze(graph, sData, folderA.getAbsoluteFile().toURI(), folderB.getAbsoluteFile().toURI(), folderC.getAbsoluteFile().toURI())

      digestArgorithm(Some(folderB.toURI())) = "?"
      digestArgorithm(Some(folderC.toURI())) = "?"
      Serialization.acquire(folderB.getAbsoluteFile().toURI(), sData)
      assert(true)
    }
  }

  override def beforeAll(configMap: org.scalatest.ConfigMap) { adjustLoggingBeforeAll(configMap) }
}
