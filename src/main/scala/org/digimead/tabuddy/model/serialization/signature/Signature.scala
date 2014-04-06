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

import java.io.{ InputStream, OutputStream }
import java.net.URI
import java.util.concurrent.atomic.AtomicReference
import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.transport.Transport
import org.digimead.tabuddy.model.serialization.yaml.Timestamp
import org.digimead.tabuddy.model.serialization.{ SData, Serialization }
import scala.Option.option2Iterable
import scala.collection.immutable
import scala.language.implicitConversions
import scala.ref.SoftReference

class Signature {
  /** Initialize SData for acquire process. */
  def initAcquire(sData: SData): SData = sData
  /** Initialize SData for freeze process. */
  def initFreeze(sData: SData): SData = sData
}

object Signature extends Loggable {
  implicit def digest2implementation(d: Signature.type): Signature = d.inner

  /** Get signature implementation. */
  def inner = DI.implementation
  /** Map of all available signature implementations. */
  def perIdentifier = DI.perIdentifier

  /**
   * No mechanism parameter.
   */
  case object NoSignature extends Mechanism.Parameters {
    /** Signature algorithm name. */
    val algorithm: String = ""
    /** Mechanism instance. */
    val mechanism: Mechanism = null
  }
  /**
   * Dependency injection routines.
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    /** Default signature algorithm. */
    lazy val default = injectOptional[Mechanism.Parameters]("Signature.Default") getOrElse NoSignature
    /** Signature container name. */
    lazy val containerName = injectOptional[String]("Signature.ContainerName") getOrElse "signature"
    /** Signature implementation. */
    lazy val implementation = injectOptional[Signature] getOrElse new Signature
    /**
     * Per identifier signature mechanisms map.
     *
     * Each collected mechanism must be:
     *  1. an instance of Mechanism
     *  2. has name that starts with "Signature.Mechanism."
     */
    lazy val perIdentifier: immutable.HashMap[Mechanism.Identifier, Mechanism] = {
      val mechanisms = bindingModule.bindings.filter {
        case (key, value) ⇒ classOf[Mechanism].isAssignableFrom(key.m.runtimeClass)
      }.map {
        case (key, value) ⇒
          key.name match {
            case Some(name) if name.startsWith("Signature.Mechanism.") ⇒
              log.debug(s"'${name}' loaded.")
              bindingModule.injectOptional(key).asInstanceOf[Option[Mechanism]]
            case _ ⇒
              log.debug(s"'${key.name.getOrElse("Unnamed")}' signature mechanism skipped.")
              None
          }
      }.flatten.toSeq
      assert(mechanisms.distinct.size == mechanisms.size, "signature mechanisms contain duplicated entities in " + mechanisms)
      immutable.HashMap(mechanisms.map(m ⇒ m.identifier -> m): _*)
    }
    /** Signature file name with signature type. */
    lazy val typeName = injectOptional[String]("Signature.TypeName") getOrElse "type"
  }
}
