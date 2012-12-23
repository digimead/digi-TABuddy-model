/**
 * This file is part of the TABuddy project.
 * Copyright (c) 2012 Alexey Aksenov ezh@ezh.msk.ru
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Global License version 3
 * as published by the Free Software Foundation with the addition of the
 * following permission added to Section 15 as permitted in Section 7(a):
 * FOR ANY PART OF THE COVERED WORK IN WHICH THE COPYRIGHT IS OWNED
 * BY Limited Liability Company «MEZHGALAKTICHESKIJ TORGOVYJ ALIANS»,
 * Limited Liability Company «MEZHGALAKTICHESKIJ TORGOVYJ ALIANS» DISCLAIMS
 * THE WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Global License for more details.
 * You should have received a copy of the GNU Affero General Global License
 * along with this program; if not, see http://www.gnu.org/licenses or write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA, 02110-1301 USA, or download the license from the following URL:
 * http://www.gnu.org/licenses/agpl.html
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Global License.
 *
 * In accordance with Section 7(b) of the GNU Affero General Global License,
 * you must retain the producer line in every report, form or document
 * that is created or manipulated using TABuddy.
 *
 * You can be released from the requirements of the license by purchasing
 * a commercial license. Buying such a license is mandatory as soon as you
 * develop commercial activities involving the TABuddy software without
 * disclosing the source code of your own applications.
 * These activities include: offering paid services to customers,
 * serving files in a web or/and network application,
 * shipping TABuddy with a closed source product.
 *
 * For more information, please contact Digimead Team at this
 * address: ezh@ezh.msk.ru
 */

package org.digimead.tabuddy.model.serialization

import scala.collection.mutable

import org.digimead.digi.lib.DependencyInjection
import org.digimead.lib.test.TestHelperLogging
import org.digimead.tabuddy.model.Element
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.Model.model2implementation
import org.digimead.tabuddy.model.Record
import org.digimead.tabuddy.model.Value
import org.scalatest.fixture.FunSpec
import org.scalatest.matchers.ShouldMatchers

import com.escalatesoft.subcut.inject.NewBindingModule

import org.digimead.tabuddy.model.TestDSL._

class BuiltinSerializationSpec_j1 extends FunSpec with ShouldMatchers with TestHelperLogging {
  type FixtureParam = Map[String, Any]

  override def withFixture(test: OneArgTest) {
    DependencyInjection.get.foreach(_ => DependencyInjection.clear)
    DependencyInjection.set(defaultConfig(test.configMap) ~ org.digimead.tabuddy.model.default)
    withLogging(test.configMap) {
      test(test.configMap)
    }
  }

  def resetConfig(newConfig: NewBindingModule = new NewBindingModule(module => {})) = DependencyInjection.reset(newConfig ~ DependencyInjection())

  describe("A SimpleSerialization") {
    it("should provide serialization mechanism for Model") {
      config =>
        val record = Model.record('root) { r => }
        val note = Model.note('note) { n => }
        val task = Model.task('task) { t => }
        Model.elementChildren should have size (3)

        Model.eStash.property(classOf[String].getName) = mutable.HashMap('a -> new Value.Static("123", Element.virtualContext(Model.inner.asInstanceOf[Element.Generic])))
        // serialize
        val s = new BuiltinSerialization
        val frozen = s.freeze(Model)
        frozen should not be (null)
        // deserialize
        val deserializedModel = s.acquire[Model[Model.Stash]](frozen).get

        // check
        // model
        deserializedModel.elementChildren should have size (3)
        // container always point to current active model
        deserializedModel.eStash.context.container should be(Model.eReference)
        deserializedModel.eStash.id.name should be(Model.eStash.id.name)
        deserializedModel.eStash.unique should be(Model.eStash.unique)
        deserializedModel.eStash.modified should be(Model.eStash.modified)
        deserializedModel.eStash.property should be(Model.eStash.property)
        deserializedModel.eStash.property(classOf[String].getName)('a).get should be("123")
        // record
        deserializedModel.elementChildren(0).eq(Model.elementChildren(0)) should be(false)
        deserializedModel.e(deserializedModel.eReference) should not be ('empty)
        deserializedModel.e(deserializedModel.elementChildren.head.eReference) should not be ('empty)
    }
    it("should provide serialization mechanism for Element") {
      config =>
        Model.reset()
        var save: Record[Record.Stash] = null
        val record = Model.record('root) { r =>
          save = r.record('level2) { r =>
            r.description = "123"
            r.record('level3) { r =>
              r.description = "456"
            }
          }
        }
        Model.e(save.eReference) should be(Some(save))
        val note = Model.note('note) { n => }
        val task = Model.task('task) { t => }
        Model.eFilter(_ => true) should have size (5)
        // serialize
        val s = new BuiltinSerialization
        val frozen = s.freeze(save)
        frozen should not be (null)
        // deserialize
        val dl2 = s.acquire[Record[Record.Stash]](frozen).get
        dl2.eId.name should be("level2")
        dl2.elementChildren should have size (1)
        dl2.eStash.model should be(None)
        dl2.eStash.context.container should be(record.eReference)
        dl2.description should be("123")
        val dl3 = dl2.elementChildren.head.asInstanceOf[Record[Record.Stash]]
        dl3.eId.name should be("level3")
        dl3.elementChildren should be('empty)
        dl3.eStash.model should be(None)
        dl3.eStash.context.container should be(dl2.eReference)
        dl3.description should be("456")
        dl2.description = "789"
        dl3.description = "098"
        save.description should be("123")
        save.elementChildren.head.asInstanceOf[Record[Record.Stash]].description should be("456")
        dl2.eReference should be(save.eReference)
        dl2.eReference.unique.hashCode() should be(save.eReference.unique.hashCode())
        dl2.eReference.origin.hashCode() should be(save.eReference.origin.hashCode())
        dl2.eReference.coordinate.hashCode() should be(save.eReference.coordinate.hashCode())
        dl2.eReference.hashCode() should be(save.eReference.hashCode())
        Model.e(save.eReference) should be(Some(save))
        Model.e(dl2.eReference) should be(Some(save))
        evaluating { Model.eAttach(Model, dl2) } should produce[AssertionError]
    }
  }
}
