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

package org.digimead.tabuddy.model.serialization

import java.net.URI
import org.digimead.digi.lib.api.XDependencyInjection
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.serialization.digest.Digest
import org.digimead.tabuddy.model.serialization.transport.Transport

/**
 * Source with copy of graph.
 */
case class Source[A <: Model.Like, B <: Element](val storageURI: URI, val transport: Transport,
  val graphDescriptor: Serialization.Descriptor.Graph[A],
  val modelDescriptor: Serialization.Descriptor.Node[B]) {
  /** Higher is better. */
  @volatile var weigth: Int = 0
}

object Source {
  /** Get digest algorithm weights. */
  def digestAlgorithmWeight = DI.digestAlgorithmWeight
  /** Recalculate and update mutable weight value for the source. */
  def recalculate(source: Source[_ <: Model.Like, _ <: Element], bootstrap: Boolean, sData: SData) = DI.calculator(source, bootstrap, sData)

  class DefaultCalculator extends WeightCalculator {
    /** Recalculate and update mutable weight value for the source. */
    def apply(source: Source[_ <: Model.Like, _ <: Element], bootstrap: Boolean, sData: SData) {
      // Add digest algorithm weight.
      sData.get(Digest.historyPerURI).foreach {
        _.get(source.storageURI).foreach {
          _.get(source.graphDescriptor.modified) match {
            case Some((latestDigestParameters, context)) if latestDigestParameters.mechanism != null ⇒
              val add = if (latestDigestParameters.algorithm != null && latestDigestParameters.algorithm.nonEmpty)
                digestAlgorithmWeight.get(latestDigestParameters.algorithm) orElse digestAlgorithmWeight.get("?") getOrElse 0
              else
                0
              source.weigth += add
            case _ ⇒
          }
        }
      }
      // Add transport algorithm weight.
      source.transport match {
        case local if local.scheme == "file" ⇒
          source.weigth += 1000
        case _ ⇒
      }
      // Add bootstrap weight
      if (bootstrap)
        source.weigth += 1000
    }
  }
  /**
   * WeightCalculator function.
   */
  trait WeightCalculator extends Function3[Source[_ <: Model.Like, _ <: Element], Boolean, SData, Unit]
  /**
   * Dependency injection routines.
   */
  // ? - Any unknown.
  private object DI extends XDependencyInjection.PersistentInjectable {
    /** Default digest algorithm. */
    lazy val calculator = injectOptional[WeightCalculator] getOrElse new DefaultCalculator
    /** Digest algorithm weights. */
    lazy val digestAlgorithmWeight = injectOptional[Map[String, Int]]("Digest.Priority") getOrElse
      Map("SHA-512" -> 300,
        "SHA-384" -> 250,
        "Tiger" -> 220,
        "Whirlpool" -> 215,
        "SHA-256" -> 210,
        "?" -> 200,
        "GOST3411" -> 190,
        "RIPEMD320" -> 185,
        "RIPEMD256" -> 180,
        "SHA-224" -> 175,
        "RIPEMD160" -> 160,
        "RIPEMD128" -> 155,
        "MD5" -> 150,
        "SHA-1" -> 150,
        "SHA1" -> 150,
        "SHA" -> 150,
        "MD4" -> 110,
        "MD2" -> 100,
        "CRC32" -> 90)
    lazy val transportWeight = 0
  }
}
