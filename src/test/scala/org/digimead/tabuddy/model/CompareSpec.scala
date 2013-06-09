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

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.LoggingHelper
import org.digimead.tabuddy.model.Model.model2implementation
import org.digimead.tabuddy.model.TestDSL._
import org.digimead.tabuddy.model.element.compare.CompareByTimespamp
import org.digimead.tabuddy.model.element.compare.CompareByTimestampAndContent
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.digimead.tabuddy.model.element.compare.CompareByTimespamp

class CompareSpec extends FunSpec with ShouldMatchers with LoggingHelper with Loggable {
  after { adjustLoggingAfter }
  before {
    DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
    adjustLoggingBefore
  }

  describe("A Compare By Modification") {
    it("should provide proper comparison") {
      Model.reset()
      var save: Record[Record.Stash] = null
      val record = Model.record('root) { r =>
        save = r.record('level2) { r =>
          r.name = "123"
        }
      }
      CompareByTimespamp.doWith {
        val model1 = Model.eCopy()
        Model.eModel.eq(Model.inner) should be(true)
        model1.eModel.eq(model1) should be(true)
        model1.compare(Model) should be(0)
        model1.name = "111"
        model1.compare(Model) should be(1) // model1 modified after Model
        (model1 | RecordLocation('root)).eModel should be(model1)
        // parent modified after child
        record.compare(record) should be(0)
        save.compare(record) should be(-1) // save modified before record
        Model.compare(record) should be(1) // Model modified after record
        Model.compare(save) should be(1) // Model modified after save
        Model.name = "123"
        Model.compare(record) should be(1) // Model modified after record
        Model.compare(save) should be(1) // Model modified after record
        save.name = "321"
        Model.compare(record) should be(1) // Model modified after record
        Model.compare(save) should be(1) // Model modified after save
        record.compare(save) should be(1) // record modified after save
        (model1 | RecordLocation('root)).compare(record) should be(-1)
      }
    }
  }
  describe("A Compare By Content") {
    it("should provide proper comparison") {
      Model.reset()
      var save: Record[Record.Stash] = null
      val record = Model.record('root) { r =>
        save = r.record('level2) { r =>
          r.name = "123"
        }
      }
      CompareByTimestampAndContent.doWith {
        val model1 = Model.eCopy()
        Model.eModel.eq(Model.inner) should be(true)
        model1.eModel.eq(model1) should be(true)
        model1.compare(Model) should be(0)
        model1.name = "111"
        model1.compare(Model) should be(1) // model1 modified after Model
        (model1 | RecordLocation('root)).eModel should be(model1)
        model1.name = Model.name
        model1.compare(Model) should be(0)
        // parent modified after child
        record.compare(record) should be(0)
        save.compare(record) should be(-1) // save modified before record
        Model.compare(record) should be(1) // Model modified after record
        Model.compare(save) should be(1) // Model modified after save
        Model.name = "123"
        Model.compare(record) should be(1) // Model modified after record
        Model.compare(save) should be(1) // Model modified after record
        save.name = "321"
        Model.compare(record) should be(1) // Model modified after record
        Model.compare(save) should be(1) // Model modified after save
        record.compare(save) should be(1) // record modified after save
        model1.compare(Model) should be(0) // model1 modified before Model BUT model1 content and Model content are the same
        Model.name = "333"
        model1.compare(Model) should be(-1) // model1 modified before Model
        model1.name = "444"
        model1.compare(Model) should be(1) // model1 modified before Model
        Model.name = "444"
        //(model1 | RecordLocation('root) | RecordLocation('level2)).name = "321"
        model1.compare(Model) should be(0) // model1 modified after Model but content is equal
      }
    }
  }

  override def beforeAll(configMap: Map[String, Any]) { adjustLoggingBeforeAll(configMap) }
}
