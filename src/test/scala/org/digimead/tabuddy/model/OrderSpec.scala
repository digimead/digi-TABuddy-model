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
import org.digimead.tabuddy.model.Model.Stash
import org.digimead.tabuddy.model.Model.model2implementation
import org.digimead.tabuddy.model.TestDSL._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class OrderSpec extends FunSpec with ShouldMatchers with LoggingHelper with Loggable {
  after { adjustLoggingAfter }
  before {
    DependencyInjection(org.digimead.digi.lib.default ~ org.digimead.tabuddy.model.default, false)
    adjustLoggingBefore
  }

  describe("An Element") {
    it("should be comparable") {
      Model.reset()
      // create
      val record1 = Model | RecordLocation('record1)
      val record2 = Model | RecordLocation('record2)
      log.___glance("compare %s %d#%d against %s %d#%d".format(record1, record1.eStash.modified.milliseconds, record1.eStash.modified.nanoShift,
        record2, record2.eStash.modified.milliseconds, record2.eStash.modified.nanoShift))
      Seq(record2, record1).sorted should be(Seq(record1, record2))
      record1 < record2 should be(true)
      // modify property
      val record1copy = record1.eCopy()
      record1copy eq record1 should be(false)
      record1copy.eStash eq record1.eStash should be(false)
      record1copy.canEqual(record1.getClass(), record1.eStash.getClass()) should be(true)
      record1copy.canEqual(record1.getClass(), classOf[Model.Stash]) should be(false)
      assert(record1copy.eStash.context === record1.eStash.context)
      assert(record1copy.eStash.coordinate === record1.eStash.coordinate)
      assert(record1copy.eStash.created === record1.eStash.created)
      assert(record1copy.eStash.id === record1.eStash.id)
      assert(record1copy.eStash.scope === record1.eStash.scope)
      assert(record1copy.eStash.unique === record1.eStash.unique)
      assert(record1copy.eStash === record1.eStash)
      assert(record1copy === record1)
      record1.name = "123"
      record1 > record2 should be(true)
      // record1 changed
      assert(record1copy.eStash != record1.eStash)
    }
  }

  override def beforeAll(configMap: Map[String, Any]) { adjustLoggingBeforeAll(configMap) }
}
