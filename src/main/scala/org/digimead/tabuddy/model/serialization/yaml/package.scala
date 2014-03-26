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

package org.digimead.tabuddy.model.serialization

import com.escalatesoft.subcut.inject.NewBindingModule
import org.digimead.digi.lib.DependencyInjection
import org.yaml.snakeyaml.constructor.Construct
import org.yaml.snakeyaml.representer.Represent

package object yaml {
  lazy val default = new NewBindingModule(module ⇒ {
    module.bind[Construct] identifiedBy ("YAML.Construct.Axis") toSingle { new Axis.Construct }
    module.bind[Represent] identifiedBy ("YAML.Represent.Axis") toSingle { new Axis.Represent }
    module.bind[Construct] identifiedBy ("YAML.Construct.Coordinate") toSingle { new Coordinate.Construct }
    module.bind[Represent] identifiedBy ("YAML.Represent.Coordinate") toSingle { new Coordinate.Represent }
    module.bind[Construct] identifiedBy ("YAML.Construct.Optional") toSingle { new Optional.Construct }
    module.bind[Represent] identifiedBy ("YAML.Represent.Optional") toSingle { new Optional.Represent }
    module.bind[Construct] identifiedBy ("YAML.Construct.Property") toSingle { new Property.Construct }
    module.bind[Represent] identifiedBy ("YAML.Represent.Property") toSingle { new Property.Represent }
    module.bind[Construct] identifiedBy ("YAML.Construct.Reference") toSingle { new Reference.Construct }
    module.bind[Represent] identifiedBy ("YAML.Represent.Reference") toSingle { new Reference.Represent }
    module.bind[Construct] identifiedBy ("YAML.Construct.Scope") toSingle { new Scope.Construct }
    module.bind[Represent] identifiedBy ("YAML.Represent.Scope") toSingle { new Scope.Represent }
    module.bind[Construct] identifiedBy ("YAML.Construct.Stash") toSingle { new Stash.Construct }
    module.bind[Represent] identifiedBy ("YAML.Represent.Stash") toSingle { new Stash.Represent }
    module.bind[Construct] identifiedBy ("YAML.Construct.Symbol") toSingle { new Symbol.Construct }
    module.bind[Represent] identifiedBy ("YAML.Represent.Symbol") toSingle { new Symbol.Represent }
    module.bind[Construct] identifiedBy ("YAML.Construct.Timestamp") toSingle { new Timestamp.Construct }
    module.bind[Represent] identifiedBy ("YAML.Represent.Timestamp") toSingle { new Timestamp.Represent }
    module.bind[Construct] identifiedBy ("YAML.Construct.UUID") toSingle { new UUID.Construct }
    module.bind[Represent] identifiedBy ("YAML.Represent.UUID") toSingle { new UUID.Represent }
  })
  DependencyInjection.setPersistentInjectable("org.digimead.tabuddy.model.serialization.yaml.YAML$DI$")
}
