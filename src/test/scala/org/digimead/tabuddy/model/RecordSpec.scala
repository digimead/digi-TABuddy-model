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

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.aop.log
import org.digimead.lib.test.TestHelperLogging
import org.digimead.tabuddy.model.Model.model2implementation
import org.scalatest.fixture.FunSpec
import org.scalatest.matchers.ShouldMatchers

import com.escalatesoft.subcut.inject.NewBindingModule

import org.digimead.tabuddy.model.TestDSL._

class RecordSpec_j1 extends FunSpec with ShouldMatchers with TestHelperLogging {
  type FixtureParam = Map[String, Any]

  override def withFixture(test: OneArgTest) {
    DependencyInjection.get.foreach(_ => DependencyInjection.clear)
    DependencyInjection.set(defaultConfig(test.configMap) ~ org.digimead.tabuddy.model.default)
    withLogging(test.configMap) {
      test(test.configMap)
    }
  }

  def resetConfig(newConfig: NewBindingModule = new NewBindingModule(module => {})) = DependencyInjection.reset(newConfig ~ DependencyInjection())

  describe("A Record") {
    it("should create new instance with apply()") {
      config =>
              implicit val snapshot = Element.Snapshot(0)  
        val record1 = Record.apply(classOf[Record[Record.Stash]], classOf[Record.Stash], Model, 'test1, Element.Coordinate.root.coordinate, (n: Record[Record.Stash]) => { "" })
        val record2 = Record.apply(Model, 'test2, Element.Coordinate.root.coordinate, (n: Record[Record.Stash]) => { "" })
        val record2a = Record.apply(Model, 'test2, Element.Coordinate.root.coordinate, (n: Record[Record.Stash]) => { "" })
        assert(record2a eq record2)
    }
    it("should support nested elements") {
      config =>
                implicit val snapshot = Element.Snapshot(0)
        var record_1a: Record[Record.Stash] = null
        var record_2a: Record[Record.Stash] = null
        var record_1b: Record[Record.Stash] = null
        var record_2b: Record[Record.Stash] = null
        // define record
        val record_0 = Model.record('baseLevel) { r =>
          record_1a = r.record('level1a) { r =>
            record_2a = r.record('level2a) { r =>
              r.description = "record_2a"
            }
            r.description = "record_1a"
          }
          record_1b = r.record('level1b) { r =>
            record_2b = r.record('level2b) { r =>
              r.description = "record_2b"
            }
            r.description = "record_1b"
          }
          r.description = "record_0"
        }
        // check description
        record_0.description should be("record_0")
        record_1a.description should be("record_1a")
        record_2a.description should be("record_2a")
        record_1b.description should be("record_1b")
        record_2b.description should be("record_2b")
        // check child elements
        record_0.stash.children should equal(List(record_1a, record_1b))
        record_1a.stash.children should equal(List(record_2a))
        record_1b.stash.children should equal(List(record_2b))
        // check elements with same id
        val treeA = Model.record('test) { r =>
          r.record('test) { r =>
            r.record('test) { _.description = "ok" }
          }
        }
        //find[Note[Note.Stash]](treeA, 'test, 'test, 'test).map(_.description) should be(Some("ok"))
    }
  }
}
