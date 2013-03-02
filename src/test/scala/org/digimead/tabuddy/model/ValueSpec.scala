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

import scala.ref.WeakReference
import org.digimead.digi.lib.DependencyInjection
import org.digimead.lib.test.TestHelperLogging
import org.scalatest.fixture.FunSpec
import org.scalatest.matchers.ShouldMatchers
import com.escalatesoft.subcut.inject.NewBindingModule
import org.digimead.tabuddy.model.TestDSL._
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Value
import org.digimead.tabuddy.model.element.Context
import java.util.UUID

class ValueSpec_j1 extends FunSpec with ShouldMatchers with TestHelperLogging {
  type FixtureParam = Map[String, Any]

  override def withFixture(test: OneArgTest) {
    DependencyInjection.get.foreach(_ => DependencyInjection.clear)
    DependencyInjection.set(defaultConfig(test.configMap) ~ org.digimead.tabuddy.model.default)
    withLogging(test.configMap) {
      test(test.configMap)
    }
  }

  def resetConfig(newConfig: NewBindingModule = new NewBindingModule(module => {})) = DependencyInjection.reset(newConfig ~ DependencyInjection())

  describe("A Value") {
    it("should have a proper equality") {
      config =>
        val container = Model.record('test) { record => }
        new Value.Static("123", Context.virtual(container)) === new Value.Static("123", Context.virtual(container)) should be(true)
        new Value.Static("123", Context.virtual(container)) == new Value.Static("123", Context.virtual(container)) should be(true)
        new Value.Static("123", Context.virtual(container)) === new Value.Static("123", Context(container.eReference, None, Some(0), None)) should be(false)
        new Value.Static("123", Context.virtual(container)) == new Value.Static("123", Context(container.eReference, None, Some(0), None)) should be(true)
        new Value.Static("123", Context.virtual(container)) === new Value.Static(Int.box(123), Context.virtual(container)) should be(false)
        new Value.Static("123", Context.virtual(container)) === new Value.Dynamic(() => "123", Context.virtual(container)) should be(false)
        new Value.Static("123", Context.virtual(container)) == new Value.Dynamic(() => "123", Context.virtual(container)) should be(true)
        new Value.Static("123", Context.virtual(container)) == new Value.Dynamic(() => Int.box(123), Context.virtual(container)) should be(false)
    }
    it("should should be affected by implicit conversions") {
      config =>
        val rootWorkspace = Model.record('test) { record => }
        implicit val container = rootWorkspace
        // x2value: convert T -> Option[Value[T]]
        val context = Context.virtual(rootWorkspace)
        var test: Option[Value[String]] = None
        test = "123"
        test.get.context.container.unique should not be (UUID.fromString("00000000-0000-0000-0000-000000000000"))
        test.get.context.container.unique should be(rootWorkspace.eReference.unique)
        test.get === new Value.Static("123", test.get.context) should be(true)
        test.get == new Value.Static("123", test.get.context) should be(true)
        // value2x: convert Value[T] -> T
        val a: String = test.get
        a should be("123")
    }
    it("should should may have empty container") {
      config =>
        val rootWorkspace = Model.record('test) { record => }
        // x2value: convert T -> Option[Value[T]]
        val context = Context.virtual(rootWorkspace)
        var test: Option[Value[String]] = None
        test = "123"
        test.get.context.container.unique should be(UUID.fromString("00000000-0000-0000-0000-000000000000"))
        test.get === new Value.Static("123", test.get.context) should be(true)
        test.get == new Value.Static("123", test.get.context) should be(true)
        // value2x: convert Value[T] -> T
        val a: String = test.get
        a should be("123")
    }
    it("should search default value at root node") {
      config =>
        val rootWorkspace = Model.record('test) { record => }
        rootWorkspace.eStash.coordinate.isRoot should be(true)
        rootWorkspace.label should be("")
        val otherWorkspace = Model.record('test, ('a, 0)) { record => }
        // child of same root with same id MUST have same unique values
        rootWorkspace.eId.name should be(otherWorkspace.eId.name)
        rootWorkspace.eUnique should be(otherWorkspace.eUnique)
        //rootWorkspace.unique
        otherWorkspace.eStash.coordinate.isRoot should be(false)
        otherWorkspace.label should be("")
        rootWorkspace.label = "test"
        rootWorkspace.label should be("test")
        // default value from root element
        otherWorkspace.label should be("test")
        otherWorkspace.label = "testOther"
        otherWorkspace.label should be("testOther")
        rootWorkspace.label should be("test")
    }
  }
  describe("A Value.Context") {
    it("should have a proper equality") {
      config =>
        val rootWorkspace = Model.record('test) { record => }
        Context(rootWorkspace.eReference, None, None, None) should be(Context(rootWorkspace.eReference, None, None, None))
        Context(null, None, None, None) should not be (Context(rootWorkspace.eReference, None, None, None))
        Context(rootWorkspace.eReference, Some(null), None, None) should not be (Context(rootWorkspace.eReference, None, None, None))
        Context(rootWorkspace.eReference, None, Some(0), None) should not be (Context(rootWorkspace.eReference, None, None, None))
        Context(rootWorkspace.eReference, None, None, Some(null)) should not be (Context(rootWorkspace.eReference, None, None, None))
    }
  }
}