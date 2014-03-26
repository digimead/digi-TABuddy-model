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
import org.digimead.tabuddy.model.serialization.transport.{ Local, Transport }
import org.yaml.snakeyaml.constructor.Construct

package object serialization {
  lazy val default = new NewBindingModule(module ⇒ {
    module.bind[Construct] identifiedBy ("YAML.Construct.Serialization.Descriptor.Element") toSingle { new Serialization.Descriptor.Element.Construct }
    module.bind[Construct] identifiedBy ("YAML.Construct.Serialization.Descriptor.Graph") toSingle { new Serialization.Descriptor.Graph.Construct }
    module.bind[Construct] identifiedBy ("YAML.Construct.Serialization.Descriptor.Node") toSingle { new Serialization.Descriptor.Node.Construct }
    module.bind[Construct] identifiedBy ("YAML.Construct.Serialization.Descriptor.Record") toSingle { new Serialization.Descriptor.Record.Construct }
    module.bind[Construct] identifiedBy ("YAML.Construct.Serialization.Descriptor.RecordResources") toSingle { new Serialization.Descriptor.RecordResources.Construct }
    module.bind[Mechanism] identifiedBy ("Serialization.Mechanism.BuiltinSerialization") toSingle { new BuiltinSerialization }
    module.bind[Mechanism] identifiedBy ("Serialization.Mechanism.YAMLSerialization") toSingle { new YAMLSerialization }
    module.bind[Transport] identifiedBy ("Serialization.Transport.Local") toSingle { new Local }
  })
  DependencyInjection.setPersistentInjectable("org.digimead.tabuddy.model.serialization.Serialization$DI$")
}
