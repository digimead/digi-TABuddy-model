package org.digimead.tabuddy.model

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.aop.log
import org.digimead.lib.test.TestHelperLogging
import org.digimead.tabuddy.model.Model.model2implementation
import org.scalatest.fixture.FunSpec
import org.scalatest.matchers.ShouldMatchers

import com.escalatesoft.subcut.inject.NewBindingModule

import org.digimead.tabuddy.model.TestDSL._

class OrderSpec_j1 extends FunSpec with ShouldMatchers with TestHelperLogging {
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
    it("should be comparable") {
      config =>
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
        record1copy.canEqual(record1.getClass(), record1.eStash.getClass()) should be (true)
        record1copy.canEqual(record1.getClass(), classOf[Model.Stash]) should be (false)
        assert(record1copy.eStash.context === record1.eStash.context)
        assert(record1copy.eStash.coordinate === record1.eStash.coordinate)
        assert(record1copy.eStash.created === record1.eStash.created)
        assert(record1copy.eStash.id === record1.eStash.id)
        assert(record1copy.eStash.scope === record1.eStash.scope)
        assert(record1copy.eStash.unique === record1.eStash.unique)
        assert(record1copy.eStash === record1.eStash)
        assert(record1copy === record1)
        record1.description = "123"
        record1 > record2 should be(true)
        // record1 changed
        assert(record1copy.eStash != record1.eStash)
    }
  }
}