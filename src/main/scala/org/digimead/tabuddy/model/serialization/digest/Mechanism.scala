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

package org.digimead.tabuddy.model.serialization.digest

import java.io.{ InputStream, OutputStream }
import java.net.URI
import java.util.concurrent.atomic.AtomicReference
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.SData
import org.digimead.tabuddy.model.serialization.transport.Transport
import scala.ref.SoftReference

/**
 * Digest mechanism interface.
 */
trait Mechanism {
  /** Identifier of the digest mechanism. */
  val identifier: Mechanism.Identifier

  /** Get mechanism parameters. */
  def apply(algorithmName: String, args: String*): Mechanism.Parameters
  /** Just invoked before freeze completion. */
  def afterFreeze(parameters: Mechanism.Parameters, graph: Graph[_ <: Model.Like], transport: Transport, sData: SData)
  /** Initialize SData for acquire process. */
  def initAcquire(sData: SData): SData
  /** Initialize SData for freeze process. */
  def initFreeze(sData: SData): SData
  /** Just invoked after read beginning. */
  def readFilter(parameters: Mechanism.Parameters, context: AtomicReference[SoftReference[AnyRef]],
    modified: Element.Timestamp, is: InputStream, uri: URI, transport: Transport, sData: SData): InputStream
  /** Just invoked after write beginning. */
  def writeFilter(parameters: Mechanism.Parameters, os: OutputStream,
    uri: URI, transport: Transport, sData: SData): OutputStream
}

object Mechanism {
  /**
   * Identifier that is associated with digest mechanism.
   */
  trait Identifier extends Equals with java.io.Serializable {
    /** Mechanism name. */
    val name: String
    /** Mechanism description. */
    val description: String

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
  trait Parameters extends Product with java.io.Serializable {
    /** Digest algorithm name. */
    def algorithm: String
    /** Digest parameters as sequence of strings. */
    def arguments: Seq[String]
    /** Mechanism instance. */
    def mechanism: Mechanism
  }
}
