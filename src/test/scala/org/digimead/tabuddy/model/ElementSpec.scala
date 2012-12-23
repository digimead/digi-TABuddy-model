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
import org.digimead.tabuddy.model.predef.Note
import org.digimead.tabuddy.model.predef.Task
import org.scalatest.fixture.FunSpec
import org.scalatest.matchers.ShouldMatchers

import com.escalatesoft.subcut.inject.NewBindingModule

import org.digimead.tabuddy.model.TestDSL._

class ElementSpec_j1 extends FunSpec with ShouldMatchers with TestHelperLogging {
  type FixtureParam = Map[String, Any]

  override def withFixture(test: OneArgTest) {
    DependencyInjection.get.foreach(_ => DependencyInjection.clear)
    DependencyInjection.set(defaultConfig(test.configMap) ~ org.digimead.tabuddy.model.default)
    withLogging(test.configMap) {
      test(test.configMap)
    }
  }

  def resetConfig(newConfig: NewBindingModule = new NewBindingModule(module => {})) = DependencyInjection.reset(newConfig ~ DependencyInjection())

  describe("An Element") {
    it("should have proper equality") {
      config =>
        val record = Model.record('root) { r => }
        val note = Model.note('note) { n => }
        val task = Model.task('task) { t => }

        // common test that demonstrate JVM architecture cripple
        classOf[Record[Record.Stash]] should be(classOf[Record[Record.Stash]])
        // JVM erasure pitfall
        classOf[Record[Record.Stash]] should be(classOf[Record[Note.Stash]])

        /* actual tests */
        record.canEqual(classOf[Record[Int]], classOf[Record.Stash]) should be(true)
        record.canEqual(classOf[Record[Any]], classOf[Note.Stash]) should be(false)
        record.canEqual(classOf[Record[String]], classOf[Task.Stash]) should be(false)
        record.canEqual(classOf[Note[Int]], classOf[Record.Stash]) should be(false)

        note.canEqual(classOf[Note[String]], classOf[Record.Stash]) should be(false)
        note.canEqual(classOf[Note[Any]], classOf[Note.Stash]) should be(true)
        note.canEqual(classOf[Note[Boolean]], classOf[Task.Stash]) should be(false)
        note.canEqual(classOf[Task[Int]], classOf[Note.Stash]) should be(false)

        task.canEqual(classOf[Task[Double]], classOf[Record.Stash]) should be(false)
        task.canEqual(classOf[Task[BigInt]], classOf[Note.Stash]) should be(false)
        task.canEqual(classOf[Task[Nothing]], classOf[Task.Stash]) should be(true)
        task.canEqual(classOf[Record[Int]], classOf[Task.Stash]) should be(false)
    }
    it("should have proper constraints") {
      config =>
        val r1 = Model.record('a) { r => }
        val rctx = r1.eStash.context
        val rcoord = r1.eStash.coordinate
        val rid = r1.eStash.id
        val runique = r1.eStash.unique
        // create element projection at different coordinate
       /* Model.eAttach(Model, new Record[Record.Stash](new Record.Stash(rctx, Element.Coordinate(('a, 1)), rid, (System.currentTimeMillis, Element.nanoShift), runique, Some(Model))))
        // create element with same coordinate
        evaluating { Model.eAttach(Model, new Record[Record.Stash](new Record.Stash(rctx, rcoord, rid, (System.currentTimeMillis, Element.nanoShift), runique, Some(Model)))) } should produce[AssertionError]
        // create element with same id and different unique
        evaluating { Model.eAttach(Model, new Record[Record.Stash](new Record.Stash(rctx, Element.Coordinate(('a, 1)), rid, (System.currentTimeMillis, Element.nanoShift), UUID.randomUUID(), Some(Model)))) } should produce[AssertionError]
        // create element with different type
        evaluating { Model.eAttach(Model, new Note[Note.Stash](new Note.Stash(rctx, Element.Coordinate(('a, 1)), rid, (System.currentTimeMillis, Element.nanoShift), runique, Some(Model)))) } should produce[AssertionError]*/
    }
    it("should register elements in model") {
      config =>
        val r1 = Model.record('a) { r => }
        r1.eStash.context.container should not be (null)
        Model.e(r1.eStash.context.container)
      //should be (Some(Model.inner))
    }
    it("should have proper copy constructor") {
      config =>
        Model.reset()
        var save: Record[Record.Stash] = null
        val record = Model.record('root) { r =>
          r.description = "root"
          save = r.record('level2) { r =>
            r.description = "level2"
            r.record('level3) { r =>
              r.description = "level3"
            }
          }
        }
        val copy = save.eCopy()
        copy.eId.name should be("level2")
        copy.description should be("level2")
        copy.elementChildren.head.asInstanceOf[Record[Record.Stash]].description should be("level3")
    }
  }
}
