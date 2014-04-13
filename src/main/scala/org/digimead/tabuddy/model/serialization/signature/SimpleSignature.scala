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

import java.io.{ BufferedInputStream, BufferedOutputStream, BufferedReader, InputStream, InputStreamReader, OutputStream, PrintStream }
import java.net.URI
import java.security.spec.{ PKCS8EncodedKeySpec, X509EncodedKeySpec }
import java.security.{ KeyFactory, PrivateKey, PublicKey }
import java.util.concurrent.atomic.AtomicReference
import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.serialization.digest.Digest
import org.digimead.tabuddy.model.serialization.transport.Transport
import org.digimead.tabuddy.model.serialization.{ SData, Serialization }
import scala.collection.immutable
import scala.ref.SoftReference
import sun.misc.{ BASE64Decoder, BASE64Encoder }

class SimpleSignature extends Mechanism with Loggable {
  /** Identifier of the digest. */
  val identifier = SimpleSignature.Identifier

  /** Get simple mechanism parameters. */
  def apply(algorithmName: String, args: String*): Mechanism.Parameters = args match {
    case Seq(publicKey, sAlgorithm, sProvider) ⇒
      // acquire parameters
      SimpleSignatureParameters(loadPublicKey(algorithmName, publicKey), None, Option(sAlgorithm), Option(sProvider))
    case Seq(publicKey, privateKey, sAlgorithm, sProvider) ⇒
      // freeze parameters
      SimpleSignatureParameters(loadPublicKey(algorithmName, publicKey),
        Some(loadPrivateKey(algorithmName, privateKey)), Option(sAlgorithm), Option(sProvider))
    case Nil ⇒
      throw new IllegalArgumentException("A cryptographic key is not defined.")
    case _ ⇒
      throw new IllegalArgumentException("Unknown parameters: " + args.drop(2).mkString(", "))
  }
  /** Just invoked before freeze completion. */
  def afterFreeze(parameters: Mechanism.Parameters, graph: Graph[_ <: Model.Like], transport: Transport, sData: SData) =
    parameters match {
      case SimpleSignatureParameters(publicKey, Some(privateKey), algorithm, provider) ⇒
        val storageURI = sData(SData.Key.storageURI)
        val algorithmName = publicKey.getAlgorithm()
        val signatureInstance = SimpleSignature.getInstance(privateKey, algorithm, provider)
        log.debug(s"Save ${algorithmName} signature data for storage ${storageURI}")
        // Write type info.
        val signatureTypeURI = Signature.signatureURI(storageURI, transport, graph.modified, Signature.typeName)
        if (!transport.exists(Serialization.inner.encode(signatureTypeURI, sData), sData) ||
          sData.get(SData.Key.force) == Some(true)) {
          val os = transport.openWrite(Serialization.inner.encode(signatureTypeURI, sData), sData, true)
          val pos = new PrintStream(new BufferedOutputStream(os))
          try {
            pos.println(SimpleSignature.Identifier.name)
            pos.println()
            pos.println(algorithmName)
            pos.println()
            pos.println(savePublicKey(publicKey))
            pos.println()
            pos.println(signatureInstance.getAlgorithm())
            pos.println()
            pos.println(signatureInstance.getProvider().getName())
            pos.flush()
          } finally try pos.close() catch {
            case e: SecurityException ⇒ throw e
            case e: Throwable ⇒ log.error(e.getMessage, e)
          }
        }
        // Write payload.
        sData.get(SimpleSignature.printStream).foreach(streamContainer ⇒
          streamContainer.synchronized {
            Option(streamContainer.get).foreach { stream ⇒
              log.debug(s"Close container with signatures.")
              stream.flush()
              stream.close()
              streamContainer.set(null)
            }
          })
      case unexpected ⇒
        throw new IllegalArgumentException("Unexpected parameters " + unexpected)
    }
  /** Initialize SData for acquire process. */
  def initAcquire(sData: SData): SData = sData
  /** Initialize SData for freeze process. */
  def initFreeze(sData: SData): SData = sData.
    updated(SimpleSignature.printStream, new AtomicReference[PrintStream]())
  /** Just invoked after read beginning. */
  def readFilter(parameters: Mechanism.Parameters, context: AtomicReference[SoftReference[AnyRef]],
    modified: Element.Timestamp, is: InputStream, uri: URI, transport: Transport, sData: SData): InputStream = parameters match {
    case SimpleSignatureParameters(publicKey, _, Some(sAlgorithm), sProvider) ⇒
      val verifier = sProvider match {
        case Some(provider) ⇒
          java.security.Signature.getInstance(sAlgorithm, provider)
        case None ⇒
          java.security.Signature.getInstance(sAlgorithm)
      }
      verifier.initVerify(publicKey)
      new Signature.SignatureInputStream(is, verifier, verifier ⇒
        checkSignature(publicKey, verifier, context, modified, uri, transport, sData))
    case unexpected ⇒
      throw new IllegalArgumentException("Unexpected parameters " + unexpected)
  }
  /** Just invoked after write beginning. */
  def writeFilter(parameters: Mechanism.Parameters, os: OutputStream, uri: URI,
    transport: Transport, sData: SData): OutputStream = parameters match {
    case SimpleSignatureParameters(publicKey, privateKeyOption, sAlgorithm, sProvider) ⇒
      val privateKey = privateKeyOption getOrElse { throw new IllegalStateException("Unable to sign without private key.") }
      new Signature.SignatureOutputStream(os, SimpleSignature.getInstance(privateKey, sAlgorithm, sProvider), signature ⇒
        writeSignature(signature, uri, transport, sData))
    case unexpected ⇒
      throw new IllegalArgumentException("Unexpected parameters " + unexpected)
  }

  /** Convert text to private key. */
  def loadPrivateKey(algorithm: String, privateKey: String): PrivateKey = {
    val sb = new StringBuilder()
    val begin = privateKey.indexOf(SimpleSignature.privateBegin)
    if (begin < 0)
      throw new IllegalStateException("Unable to find the begining of the private key.")
    val end = privateKey.indexOf(SimpleSignature.privateEnd, begin)
    if (end < 0)
      throw new IllegalStateException("Unable to find the ending of the private key.")
    val keyLines = privateKey.substring(begin, end + 1).split("\n").drop(1).dropRight(1)
    val encoded = new BASE64Decoder().decodeBuffer(keyLines.mkString("\n"))
    val keySpec = new PKCS8EncodedKeySpec(encoded)
    val kf = KeyFactory.getInstance(algorithm)
    kf.generatePrivate(keySpec)
  }
  /** Convert text to public key. */
  def loadPublicKey(algorithm: String, publicKey: String): PublicKey = {
    val sb = new StringBuilder()
    val begin = publicKey.indexOf(SimpleSignature.publicBegin)
    if (begin < 0)
      throw new IllegalStateException("Unable to find the begining of the public key.")
    val end = publicKey.indexOf(SimpleSignature.publicEnd, begin)
    if (end < 0)
      throw new IllegalStateException("Unable to find the ending of the public key.")
    val keyLines = publicKey.substring(begin, end + 1).split("\n").drop(1).dropRight(1)
    val encoded = new BASE64Decoder().decodeBuffer(keyLines.mkString("\n"))
    val keySpec = new X509EncodedKeySpec(encoded)
    val kf = KeyFactory.getInstance(algorithm)
    kf.generatePublic(keySpec)
  }
  /** Convert private key to text. */
  // openssl ?sa -in key -text
  def savePrivateKey(privateKey: PrivateKey): String = {
    val sb = new StringBuilder()
    val pkcs8EncodedKeySpec = new PKCS8EncodedKeySpec(privateKey.getEncoded())
    sb.append(SimpleSignature.privateBegin + "\n")
    sb.append(new BASE64Encoder().encode(pkcs8EncodedKeySpec.getEncoded()))
    sb.append("\n" + SimpleSignature.privateEnd)
    sb.toString
  }
  /** Convert public key to text. */
  // openssl ?sa -in key -text
  def savePublicKey(publicKey: PublicKey): String = {
    val sb = new StringBuilder()
    val x509EncodedKeySpec = new X509EncodedKeySpec(publicKey.getEncoded())
    sb.append(SimpleSignature.publicBegin + "\n")
    sb.append(new BASE64Encoder().encode(x509EncodedKeySpec.getEncoded()))
    sb.append("\n" + SimpleSignature.publicEnd)
    sb.toString
  }

  /** Approve resource check sum. */
  protected def approve(resourceURI: URI, sData: SData) =
    log.debug("Approve signature for .../" + sData(SData.Key.storageURI).relativize(resourceURI))
  /** Check signature data. */
  @throws[SecurityException]("if verification is failed")
  protected def checkSignature(publicKey: PublicKey, verifier: java.security.Signature,
    context: AtomicReference[SoftReference[AnyRef]], modified: Element.Timestamp, uri: URI, transport: Transport, sData: SData) {
    val storageURI = sData(SData.Key.storageURI)
    val map = getSignatureMap(context, modified, transport, sData)
    map.get(uri) match {
      case Some(originalValue) ⇒
        // throws SecurityException
        sData(Signature.Key.acquire)(Digest.digestURI(storageURI, transport, modified).resolve(uri),
          modified, Some(publicKey, verifier.verify(originalValue)), sData)
      case None ⇒
        throw new IllegalStateException("Unable to find signature for " + uri)
    }
  }
  /** Get loaded or load new signature map. */
  protected def getSignatureMap(context: AtomicReference[SoftReference[AnyRef]],
    modified: Element.Timestamp, transport: Transport, sData: SData): immutable.Map[URI, Array[Byte]] = {
    context.get().get match {
      case Some(map: immutable.Map[_, _]) ⇒ return map.asInstanceOf[immutable.Map[URI, Array[Byte]]]
      case _ ⇒ context.get().clear
    }
    val storageURI = sData(SData.Key.storageURI)
    log.debug(s"Load signature data from storage ${storageURI}")
    val builder = immutable.Map.newBuilder[URI, Array[Byte]]
    val digestSumURI = Signature.signatureURI(storageURI, transport, modified, Signature.containerName)
    val digestStream = transport.openRead(Serialization.inner.encode(digestSumURI, sData), sData)
    val reader = new BufferedReader(new InputStreamReader(new BufferedInputStream(digestStream)))
    try {
      var line = reader.readLine()
      while (line != null) {
        val hash = line.takeWhile(_ != ' ')
        val uri = new URI(line.drop(hash.length() + 1))
        builder += uri -> Serialization.hexStringToByteArray(hash)
        line = reader.readLine()
      }
    } finally try reader.close() catch {
      case e: SecurityException ⇒ throw e
      case e: Throwable ⇒ log.error(e.getMessage, e)
    }
    val map = builder.result
    context.set(new SoftReference(map))
    map
  }
  /** Refuse resource check sum. */
  protected def refuse(resourceURI: URI, sData: SData) =
    throw new IllegalStateException("Incorrect signature for " + resourceURI)
  /** Write signature data to file. */
  protected def writeSignature(signature: java.security.Signature, uri: URI, transport: Transport, sData: SData): Unit = synchronized {
    sData.get(SimpleSignature.printStream).foreach(streamContainer ⇒ streamContainer.synchronized {
      val modified = sData(SData.Key.modified)
      val storageURI = sData(SData.Key.storageURI)
      val printStream = Option(streamContainer.get) getOrElse {
        val signatureDataURI = Signature.signatureURI(storageURI, transport, modified, Signature.containerName)
        if (!transport.exists(Serialization.inner.encode(signatureDataURI, sData), sData) ||
          sData.get(SData.Key.force) == Some(true)) {
          log.debug(s"Open container with signatures for ${modified} at ${signatureDataURI}")
          val stream = transport.openWrite(Serialization.inner.encode(signatureDataURI, sData), sData, true)
          val printStream = new PrintStream(new BufferedOutputStream(stream))
          streamContainer.set(printStream)
          printStream
        } else {
          log.debug(s"Skip signature data ${modified} for storage ${storageURI}")
          return
        }
      }
      log.debug(s"Append signature for '${Digest.digestURI(storageURI, transport, modified).resolve(uri)}'.")
      printStream.println(Serialization.byteArrayToHexString(signature.sign()) + " " + uri)
    })
  }

  /**
   * SimpleSignature parameters.
   *
   * @param publicKey public key
   * @param privateKey private key
   * @param sAlgorithm signature instance algorithm
   * @param sProvider signature instance provider
   */
  case class SimpleSignatureParameters(val publicKey: PublicKey, privateKey: Option[PrivateKey],
    val sAlgorithm: Option[String], sProvider: Option[String]) extends Mechanism.Parameters {
    privateKey.foreach(privateKey ⇒ if (publicKey.getAlgorithm() != privateKey.getAlgorithm())
      throw new IllegalArgumentException(s"Public key algorithm ${publicKey.getAlgorithm()} and private key algorithm ${privateKey.getAlgorithm()} are different."))
    /** Signature algorithm name. */
    val algorithm = publicKey.getAlgorithm()
    /** SimpleSignature mechanism instance. */
    val mechanism = SimpleSignature.this

    override def toString() = privateKey match {
      case Some(privateKey) ⇒
        s"SimpleSignatureParameters(public ${publicKey.getFormat()} ${publicKey.getAlgorithm()} key, private ${publicKey.getAlgorithm()} key, ${sAlgorithm}, ${sProvider})"
      case None ⇒
        s"SimpleSignatureParameters(public ${publicKey.getFormat()} ${publicKey} key, ${sAlgorithm}, ${sProvider})"
    }
  }
}

object SimpleSignature extends Loggable {
  /** Begin of a public text key. */
  val publicBegin = "-----BEGIN PUBLIC KEY-----"
  /** End of a public text key. */
  val publicEnd = "-----END PUBLIC KEY-----"
  /** Begin of a private text key. */
  val privateBegin = "-----BEGIN PRIVATE KEY-----"
  /** End of a private text key. */
  val privateEnd = "-----END PRIVATE KEY-----"
  /** Simple signature print stream. */
  val printStream = SData.key[AtomicReference[PrintStream]]("simpleSignaturePrintStream")

  /** Get SimpleSignature mechanism parameters. */
  def apply(publicKey: PublicKey, privateKey: PrivateKey): Mechanism.Parameters =
    apply(publicKey, privateKey, None, None)
  /** Get SimpleSignature mechanism parameters. */
  def apply(publicKey: PublicKey, privateKey: PrivateKey, sAlgorithm: Option[String]): Mechanism.Parameters =
    apply(publicKey, privateKey, sAlgorithm, None)
  /** Get SimpleSignature mechanism parameters. */
  def apply(publicKey: PublicKey, privateKey: PrivateKey, sAlgorithm: Option[String], sProvider: Option[String]): Mechanism.Parameters =
    Signature.perIdentifier.get(Identifier) match {
      case Some(signature: SimpleSignature) ⇒
        signature(publicKey.getAlgorithm(),
          signature.savePublicKey(publicKey),
          signature.savePrivateKey(privateKey),
          sAlgorithm.getOrElse(null), sProvider.getOrElse(null))
      case _ ⇒ throw new IllegalStateException("Simple signature is not available.")
    }
  /** Get signature object for the specified private key. */
  def getInstance(privateKey: PrivateKey, sAlgorithm: Option[String], sProvider: Option[String]) =
    DI.factory(privateKey, sAlgorithm, sProvider)
  /** Get signature object for the specified private key. */
  def getInstance(privateKey: PrivateKey, sAlgorithm: String, sProvider: Option[String]) =
    DI.factory(privateKey, Option(sAlgorithm), sProvider)
  /** Get signature object for the specified private key. */
  def getInstance(privateKey: PrivateKey, sAlgorithm: String, sProvider: String = null) =
    DI.factory(privateKey, Option(sAlgorithm), Option(sProvider))

  /**
   * SimpleSignature mechanism identifier.
   */
  object Identifier extends Mechanism.Identifier { val name = "simple" }
  /*
   * Signature.MD2withRSA
   * Signature.MD5andSHA1withRSA
   * Signature.MD5withRSA
   * Signature.NONEwithDSA
   * Signature.SHA1withDSA
   * Signature.SHA1withRSA
   * Signature.SHA256withRSA
   * Signature.SHA384withRSA
   * Signature.SHA512withRSA
   */
  /**
   * Default factory for signature object.
   */
  class DefaultFactory extends Factory {
    def apply(privateKey: PrivateKey, algorithm: Option[String], provider: Option[String]): java.security.Signature = {
      log.debug(s"Get signature instance for ${privateKey.getAlgorithm()} key with algorithm ${algorithm} and provider ${provider}.")
      val signatureProvider = provider orElse {
        privateKey.getAlgorithm() match {
          case "DSA" ⇒ Some("SUN")
          case "RSA" ⇒ Some("SunRsaSign")
          case unsupported ⇒ None
        }
      }
      val signatureAlgorith = algorithm getOrElse {
        privateKey.getAlgorithm() match {
          case "DSA" ⇒ "SHA1withDSA"
          case "RSA" ⇒ "SHA1withRSA"
          case unsupported ⇒
            throw new IllegalArgumentException("Unable to process private key with unsupported algorithm " + unsupported)
        }
      }
      val sInstance = signatureProvider match {
        case Some(signatureProvider) ⇒
          java.security.Signature.getInstance(signatureAlgorith, signatureProvider)
        case None ⇒
          java.security.Signature.getInstance(signatureAlgorith)
      }
      sInstance.initSign(privateKey)
      sInstance
    }
  }
  /**
   * Factory for signature object that implements the specified signature algorithm.
   */
  trait Factory extends Function3[PrivateKey, Option[String], Option[String], java.security.Signature]
  /**
   * Dependency injection routines.
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    /** Default DSA signing algorithm. */
    lazy val factory = injectOptional[Factory] getOrElse new DefaultFactory
  }
}
