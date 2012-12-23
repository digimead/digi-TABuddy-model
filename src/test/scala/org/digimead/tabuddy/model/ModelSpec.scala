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

package org.digimead.tabuddy.model

import java.util.UUID

import org.digimead.digi.lib.DependencyInjection
import org.digimead.lib.test.TestHelperLogging
import org.digimead.tabuddy.model.Model.model2implementation
import org.scalatest.fixture.FunSpec
import org.scalatest.matchers.ShouldMatchers

import com.escalatesoft.subcut.inject.NewBindingModule

import org.digimead.tabuddy.model.TestDSL._

class ModelSpec_j1 extends FunSpec with ShouldMatchers with TestHelperLogging {
  type FixtureParam = Map[String, Any]
  val custom = new NewBindingModule(module => {
    module.bind[Model.Interface[Model.Stash]] toProvider { new MyModel(new Model.Stash('Model, UUID.randomUUID())) }
  })

  override def withFixture(test: OneArgTest) {
    DependencyInjection.get.foreach(_ => DependencyInjection.clear)
    DependencyInjection.set(custom ~ defaultConfig(test.configMap) ~ org.digimead.tabuddy.model.default)
    withLogging(test.configMap) {
      test(test.configMap)
    }
  }

  def resetConfig(newConfig: NewBindingModule = new NewBindingModule(module => {})) = DependencyInjection.reset(newConfig ~ DependencyInjection())

  describe("A Model") {
    it("should be reseted in a right way") {
      config =>
        Model.reset()
        Model.inner.isInstanceOf[MyModel[_]] should be(true)
        val mymodel = Model.inner.asInstanceOf[MyModel[_]]
        var record2: Element.Generic = null
        val record = Model.record('root) { r =>
          record2 = r.record('b) { r => }
        }
        val note = Model.note('note) { n => }
        val task = Model.task('task) { t => }

        // before reset
        Model.elementChildren should have size (3)
        Model.eFilter(_ => true) should have size (4)
        record.elementChildren should not be ('empty)
        mymodel.getIndex should not be ('empty)

        Model.reset()

        // after reset
        mymodel.getIndex should have size (1)
        record.eStash.model should be(None)
        record.elementChildren should be('empty)
        record2.eStash.asInstanceOf[Stash].model should be(None)
        record2.elementChildren should be('empty)
        note.eStash.model should be(None)
        note.elementChildren should be('empty)
        task.eStash.model should be(None)
        task.elementChildren should be('empty)
    }
    it("should attach and detach element") {
      config =>
        Model.reset()
        var save: Record[Record.Stash] = null
        val record = Model.record('root) { r =>
          save = r.record('level2) { r =>
            r.record('level3) { r =>
            }
          }
        }
        record.elementChildren should have size (1)
        Model.eFilter(_ => true) should have size (3)
        val detached = Model.eDetach(save)
        Model.eFilter(_ => true) should have size (1)
        detached.eFilter(_ => true) should have size (1)
        record.elementChildren should be('empty)
    }
  }

  class MyModel[A <: Model.Stash](s: A) extends Model(s) {
    val getIndex = index
  }
}