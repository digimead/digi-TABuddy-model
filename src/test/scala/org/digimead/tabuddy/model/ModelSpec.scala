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

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.LoggingHelper
import org.digimead.tabuddy.model.Model.model2implementation
import org.digimead.tabuddy.model.TestDSL.element2rich
import org.digimead.tabuddy.model.TestDSL.model2rich
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Stash
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import com.escalatesoft.subcut.inject.NewBindingModule

class ModelSpec extends FunSpec with ShouldMatchers with LoggingHelper with Loggable {
  val custom = new NewBindingModule(module => {
    module.bind[Model.Interface[Model.Stash]] toProvider { new MyModel(new Model.Stash('Model, UUID.randomUUID())) }
  })

  after { adjustLoggingAfter }
  before {
    DependencyInjection(custom ~ org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
    adjustLoggingBefore
  }

  describe("A Model") {
    it("should be reseted in a right way") {
      Model.reset()
      Model.inner.isInstanceOf[MyModel] should be(true)
      val mymodel = Model.inner.asInstanceOf[MyModel]
      var record2: Element.Generic = null
      val record = Model.record('root) { r =>
        record2 = r.record('b) { r => }
      }
      val note = Model.note('note) { n => }
      val task = Model.task('task) { t => }

      // before reset
      Model.eChildren should have size (3)
      Model.eFilter(_ => true) should have size (4)
      record.eChildren should not be ('empty)
      mymodel.getIndex should not be ('empty)

      Model.reset()

      // after reset
      mymodel.getIndex should have size (1)
      record.eStash.model should be(None)
      record.eChildren should be('empty)
      record2.eStash.asInstanceOf[Stash].model should be(None)
      record2.eChildren should be('empty)
      note.eStash.model should be(None)
      note.eChildren should be('empty)
      task.eStash.model should be(None)
      task.eChildren should be('empty)
    }
    it("should attach and detach element") {
      Model.reset()

      var save: Record[Record.Stash] = null
      val record = Model.record('root) { r =>
        save = r.record('level2) { r =>
          r.record('level3) { r =>
          }
        }
      }
      val modelCopy = Model.eCopy()
      modelCopy.eStash.model should equal(Some(modelCopy))
      Model.eModel.eq(Model.inner) should be(true)
      modelCopy.eModel.eq(modelCopy) should be(true)
      val recordCopy = modelCopy.eChildren.head
      recordCopy.eModel.eq(modelCopy) should be(true)
      recordCopy.eId.name should be("root")
      record.eModel.eq(Model.inner) should be(true)
      record.eChildren should have size (1)
      Model.eFilter(_ => true) should have size (3)
      Model.eDetach(save)
      /*Model.eFilter(_ => true) should have size (1)
        save.eFilter(_ => true) should have size (1)
        record.eChildren should be('empty)*/
    }
  }

  override def beforeAll(configMap: Map[String, Any]) { adjustLoggingBeforeAll(configMap) }

  class MyModel(s: Model.Stash, v: Int) extends Model(s) {
    def this(s: Model.Stash) = this(s, 1)
    val getIndex = index
  }
}