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

/**
 * Signature mechanism interface.
 */
trait Mechanism {
  /** Identifier of the signature mechanism. */
  val identifier: Mechanism.Identifier

  /** Get mechanism parameters. */
  def apply(algorithmName: String, args: String*): Mechanism.Parameters
}

object Mechanism {
  /**
   * Identifier that is associated with signature mechanism.
   */
  trait Identifier extends Equals with java.io.Serializable {
    val name: String

    override def canEqual(that: Any) = that.isInstanceOf[Identifier]
    override def equals(that: Any): Boolean = that match {
      case that: Identifier ⇒ that.canEqual(this) && that.name.equals(this.name)
      case _ ⇒ false
    }
    override def hashCode = name.##

    override def toString = s"Mechanism.Identifier(${name})"
  }
  /**
   * Mechanism parameters.
   */
  trait Parameters {
    /** Signature algorithm name. */
    val algorithm: String
    /** Mechanism instance. */
    val mechanism: Mechanism
  }
}
