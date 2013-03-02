/**
 * This file is part of the TABuddy project.
 * Copyright (c) 2012-2013 Alexey Aksenov ezh@ezh.msk.ru
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
import org.digimead.tabuddy.model.element.Axis.intToAxis
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
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
        Model.eAttach(new Record[Record.Stash](new Record.Stash(rctx, Coordinate(('a, 1)), Element.timestamp(), rid, Record.scope, runique, new org.digimead.tabuddy.model.element.Stash.Data)))
        // create element with same coordinate
        evaluating { Model.eChildren += new Record[Record.Stash](new Record.Stash(rctx, rcoord, Element.timestamp(), rid, Record.scope, runique, new org.digimead.tabuddy.model.element.Stash.Data)) } should produce[AssertionError]
        // create element with same id and different unique
        evaluating { Model.eChildren += new Record[Record.Stash](new Record.Stash(rctx, Coordinate(('a, 1)), Element.timestamp(), rid, Record.scope, UUID.randomUUID(), new org.digimead.tabuddy.model.element.Stash.Data)) } should produce[AssertionError]
        // create element with different type
        evaluating { Model.eChildren += new Note[Note.Stash](new Note.Stash(rctx, Coordinate(('a, 1)), Element.timestamp(), rid, Record.scope, runique, new org.digimead.tabuddy.model.element.Stash.Data)) } should produce[AssertionError]
    }
    it("should register elements in model") {
      config =>
        val r1 = Model.record('a) { r => }
        r1.eStash.context.container should not be (null)
        Model.e(r1.eStash.context.container) should be(Some(Model.inner))
    }
    it("should have proper copy constructor") {
      config =>
        Model.reset()
        var save: Record[Record.Stash] = null
        val record = Model.record('root) { r =>
          r.label = "root"
          save = r.record('level2) { r =>
            r.label = "level2"
            r.record('level3) { r =>
              r.label = "level3"
            }
          }
        }
        save.eSet[Integer]('test, 123)
        val saveValue = save.eGet[Integer]('test).get
        saveValue.context.container.unique should be(save.eReference.unique)
        val copy = save.eCopy()
        copy.eId.name should be("level2")
        copy.label should be("level2")
        copy.eChildren.head.asInstanceOf[Record[Record.Stash]].label should be("level3")
        val copyValue = copy.eGet[Integer]('test).get
        copyValue.context.container.unique should be(copy.eReference.unique)
        record.eReference.unique should not be (copy.eReference.unique)
        record.eSet('test, copyValue)
        val recordValue = record.eGet[Integer]('test).get
        recordValue.get should be(copyValue.get)
        recordValue.context.container.unique should be(record.eReference.unique)
        record.eChildren -= save
        Model.e(save.eReference) should be(None)
        save.eStash.model should be(None)
        record.eChildren += save
        val newRecord = save.eCopy(save.eStash.copy(id = 'new))
        save.eUnique should be(newRecord.eUnique)
        save.eReference should be(newRecord.eReference)
        save.eStash.model should be(Some(Model.inner))
        Model.e(newRecord.eReference) should not be ('empty)
        record.eChildren -= save
        Model.e(save.eReference) should be(None)
        record.eChildren += newRecord
    }
    it("should determinate ancestors") {
      config =>
        Model.reset()
        var recordL2: Record[Record.Stash] = null
        var recordL3: Record[Record.Stash] = null
        val recordL1 = Model.record('level1) { r =>
          recordL2 = r.record('level2) { r =>
            recordL3 = r.record('level3) { r =>
            }
          }
        }
        Model.eAncestors should be('empty)
        Model.eAncestorReferences should be('empty)
        recordL1.eAncestors should be(Seq(Model.inner))
        recordL1.eAncestorReferences should be(Seq(Model.inner.eReference))
        recordL2.eAncestors should be(Seq(recordL1, Model.inner))
        recordL2.eAncestorReferences should be(Seq(recordL1.eReference, Model.inner.eReference))
        recordL3.eAncestors should be(Seq(recordL2, recordL1, Model.inner))
        recordL3.eAncestorReferences should be(Seq(recordL2.eReference, recordL1.eReference, Model.inner.eReference))
    }
    it("should provide the convertation ability") {
      config =>
        Model.reset()
        val note = Model.note('root) { n => }
        note should not be (null)
        note.getClass should be(classOf[Note[_]])
        val element = note.asInstanceOf[Element.Generic]
        // Element.Generic -> Record -> Note -> Task
        // unable - lack of required fields
        element.eAs[Task[Task.Stash], Task.Stash].nonEmpty should be(false)
        // ok
        element.eAs[Note[Note.Stash], Note.Stash].nonEmpty should be(true)
        // unable - lack of required fields in stash
        element.eAs[Note[Task.Stash], Task.Stash].nonEmpty should be(false)
        // ok
        element.eAs[Record[Record.Stash], Record.Stash].nonEmpty should be(true)
    }
    it("should provide recursive iterator") {
      config =>
        Model.reset()
        var e2: Record[Record.Stash] = null
        var e3: Record[Record.Stash] = null
        val e1 = Model.record('root) { r =>
          r.label = "root"
          e2 = r.record('level2) { r =>
            r.label = "level2"
            e3 = r.record('level3) { r =>
              r.label = "level3"
            }
          }
        }
        val modelIterator = Model.eChildren.iteratorRecursive
        assert(modelIterator.length === 3) // Model + 3 children
        val modelChildren = Model.eChildren.iteratorRecursive.foldLeft(Seq[Element.Generic]())((acc, e) => acc :+ e).sortBy(_.eUnique)
        Seq(e1, e2, e3).sortBy(_.eUnique).sameElements(modelChildren)
        assert(e2.eChildren.iteratorRecursive.length === 1)
        assert(e3.eChildren.iteratorRecursive.length === 0)
    }
  }
}
