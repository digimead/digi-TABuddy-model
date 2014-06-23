/**
 * TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2012-2014 Alexey Aksenov ezh@ezh.msk.ru
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

import com.escalatesoft.subcut.inject.NewBindingModule
import org.digimead.digi.lib.DependencyInjection
import org.digimead.tabuddy.model.dsl.{ BasicDSLTypes, ComplexDSLTypes, DSLType }

package object dsl {
  lazy val default = new NewBindingModule(module ⇒ {
    module.bind[DSLType] identifiedBy ("DSLType.Basic") toSingle { new BasicDSLTypes }
    module.bind[DSLType] identifiedBy ("DSLType.Complex") toSingle { new ComplexDSLTypes }
    module.bind[DSLType.Interface] toSingle { new DSLType.Interface {} }
  })
  DependencyInjection.setPersistentInjectable("org.digimead.tabuddy.model.dsl.DSLType$DI$")
}
