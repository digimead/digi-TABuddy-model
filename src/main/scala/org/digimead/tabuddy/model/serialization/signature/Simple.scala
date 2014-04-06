/**
 * TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2014 Alexey Aksenov ezh@ezh.msk.ru
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

package org.digimead.tabuddy.model.serialization.signature

import java.net.URI
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.serialization.SData
import scala.collection.{ mutable, immutable }
import scala.reflect.runtime.universe

class Simple extends Mechanism with Loggable {
  /** Identifier of the digest. */
  val identifier = Simple.Identifier

  /** Get simple mechanism parameters. */
  def apply(algorithmName: String, args: String*): Mechanism.Parameters = {
    if (args.nonEmpty)
      throw new IllegalArgumentException("Unknown parameters: " + args.mkString(", "))
    null
  }
}

object Simple {
  /** Get simple mechanism parameters. */
  def apply(algorithm: String): Mechanism.Parameters = Signature.perIdentifier.get(Identifier) match {
    case Some(signature: Simple) ⇒ signature(algorithm)
    case _ ⇒ throw new IllegalStateException("Simple signature is not available.")
  }

  /**
   * Simple mechanism identifier.
   */
  object Identifier extends Mechanism.Identifier { val name = "simple" }
  /**
   * Predefined keys of a generic process.
   */
  object Key {
    /** Digest map with graph hash sums: storageURI -> Map[Relative URI, digest] */
    val digestMap = SData.key[immutable.Map[URI, mutable.Map[URI, Array[Byte]] with mutable.SynchronizedMap[URI, Array[Byte]]]]("digest")
  }
}
