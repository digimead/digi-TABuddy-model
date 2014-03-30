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

import java.io.{ InputStream, OutputStream }
import java.net.URI
import org.digimead.digi.lib.NotNothing
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.transport.Transport
import scala.collection.{ GenTraversableOnce, IterableLike, immutable, mutable }
import scala.language.implicitConversions
import scala.reflect.runtime.universe.TypeTag

/**
 * Serialization data with options, arguments, parameters and side effects of the (de)serialization process.
 */
class SData(val underlying: immutable.Map[SData.Key[_], Any] = immutable.Map()) extends IterableLike[(SData.Key[_], Any), SData] {
  def ++(xs: GenTraversableOnce[(SData.Key[_], Any)]): SData = new SData(underlying ++ xs)
  def +[A](elem1: (SData.Key[A], A), elem2: (SData.Key[A], A), elems: (SData.Key[A], A)*): SData = new SData(underlying + (elem1, elem2, elems: _*))
  def +[A](kv: (SData.Key[A], A)): SData = new SData(underlying + kv)
  def -(elem1: SData.Key[_], elem2: SData.Key[_], elems: SData.Key[_]*): SData = new SData(underlying - (elem1, elem2, elems: _*))
  def -(elem: SData.Key[_]): SData = new SData(underlying - elem)
  def --(xs: GenTraversableOnce[SData.Key[_]]): SData = new SData(underlying -- xs)
  def apply[A](key: SData.Key[A]): A = underlying(key).asInstanceOf[A]
  def empty: SData = SData.empty
  def filterKeys(p: Any ⇒ Boolean): SData = new SData(underlying.filterKeys(p))
  def get[A](key: SData.Key[A]): Option[A] = underlying.get(key).asInstanceOf[Option[A]]
  def getOrElse[B >: A, A](key: SData.Key[A], default: ⇒ B): B = get(key) match {
    case Some(v) ⇒ v
    case None ⇒ default
  }
  def iterator: Iterator[(SData.Key[_], Any)] = underlying.iterator
  def seq: TraversableOnce[(SData.Key[_], Any)] = underlying.seq
  def updated[A](key: SData.Key[A], value: A): SData = new SData(underlying.updated(key, value))
  def withDefault(d: SData.Key[_] ⇒ Any): SData = new SData(underlying.withDefault(d))
  def withDefaultValue(d: Any): SData = new SData(underlying.withDefaultValue(d))

  protected[this] def newBuilder: mutable.Builder[(SData.Key[_], Any), SData] = new SData.Builder

  /** A method that should be called from every well-designed equals method. */
  override def canEqual(that: Any) = that.isInstanceOf[SData]
  /** Compares two maps. */
  override def equals(that: Any): Boolean = that match {
    case that: SData ⇒ (this eq that) || (that canEqual this) && this.underlying == that.underlying
    case _ ⇒ false
  }
  override def hashCode = underlying.hashCode()
}

object SData {
  implicit def state2underlying(s: SData): immutable.Map[SData.Key[_], Any] = s.underlying

  def apply(elems: KeyValue[_]*): SData = new SData(immutable.Map(elems: _*))
  def empty: SData = Nil

  /** Get key for transport parameter. */
  def key[T: NotNothing](implicit tag: TypeTag[T]) = Key[T](tag, None)
  /** Get key for transport parameter. */
  def key[T: NotNothing](name: String)(implicit tag: TypeTag[T]) = Key[T](tag, Some(name))
  /** Get key for transport parameter. */
  def key[T: NotNothing](name: Option[String])(implicit tag: TypeTag[T]) = Key[T](tag, name)

  /**
   * The implementation of a SData builder.
   */
  class Builder() extends mutable.Builder[(SData.Key[_], Any), SData] {
    protected var elems = Map.empty[SData.Key[_], Any]
    def +=(elem: (SData.Key[_], Any)): this.type = { elems += elem; this }
    def clear(): Unit = elems = Map.empty[SData.Key[_], Any]
    def result(): SData = new SData(elems)
  }
  /**
   * The binding key, used to uniquely identify the parameter using the class and an optional name.
   */
  case class Key[A](t: TypeTag[A], name: Option[String]) {
    @inline def ->(y: A): KeyValue[A] = new KeyValue[A](this, y)
    def →(y: A): KeyValue[A] = ->(y)
  }
  /**
   * Predefined keys of a generic transport.
   */
  object Key {
    /** Acquire transformation f(x). */
    val acquireT = SData.key[Serialization.AcquireTransformation]("transform")
    /** Decode file content. */
    val decodeFilter = SData.key[(InputStream, URI, SData) ⇒ InputStream]("decode")
    /** Encode file content. */
    val encodeFilter = SData.key[(OutputStream, URI, SData) ⇒ OutputStream]("encode")
    /** Encode URI path parts. */
    val encodeURI = SData.key[(String, SData) ⇒ String]("encodeURI")
    /** Explicit storages. */
    val explicitStorages = SData.key[Serialization.ExplicitStorages]("storages")
    /** Skip broken nodes on load/overwrite everything. */
    val force = SData.key[Boolean]("force")
    /** Freeze transformation f(x) ⇒ xˈ. */
    val freezeT = SData.key[Serialization.FreezeTransformation]("transform")
    /** Just invoked before acquire completion. */
    val onAcquire = SData.key[(Graph[_ <: Model.Like], Transport, SData) ⇒ _]("onAcquire")
    /** Just invoked before freeze completion. */
    val onFreeze = SData.key[(Graph[_ <: Model.Like], Transport, SData) ⇒ _]("onFreeze")
    /** Just invoked before read completion. */
    val onRead = SData.key[(URI, Array[Byte], SData) ⇒ _]("onRead")
    /** Just invoked before write completion. */
    val onWrite = SData.key[(URI, Array[Byte], SData) ⇒ _]("onWrite")
    /** Storage base URI. */
    val storageURI = SData.key[URI]("storage")
  }
  /**
   * Key value class that provides type safe state pairs.
   */
  class KeyValue[A](key: Key[A], value: A) extends Tuple2[Key[A], A](key, value)
  /**
   * Empty state.
   */
  private case object Nil extends SData(immutable.Map())
}
