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

import java.io.{ BufferedInputStream, BufferedOutputStream, BufferedReader, InputStream, InputStreamReader, IOException, OutputStream, PrintStream }
import java.net.URI
import java.security.spec.{ PKCS8EncodedKeySpec, X509EncodedKeySpec }
import java.security.{ KeyFactory, PrivateKey, PublicKey, SignatureException }
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
    case Seq(publicKeyArg, sAlgorithmArg, sProviderArg) ⇒
      // acquire parameters
      val publicKey = loadPublicKey(algorithmName, publicKeyArg)
      val privateKey = None
      val sAlgorithm = if (sAlgorithmArg == "") None else Option(sAlgorithmArg)
      val sProvider = if (sProviderArg == "") None else Option(sProviderArg)
      SimpleSignatureParameters(publicKey, privateKey, sAlgorithm, sProvider)
    case Seq(publicKeyArg, privateKeyArg, sAlgorithmArg, sProviderArg) ⇒
      // freeze parameters
      val publicKey = loadPublicKey(algorithmName, publicKeyArg)
      val privateKey = if (privateKeyArg == "") None else Some(loadPrivateKey(algorithmName, privateKeyArg))
      val sAlgorithm = if (sAlgorithmArg == "") None else Option(sAlgorithmArg)
      val sProvider = if (sProviderArg == "") None else Option(sProviderArg)
      SimpleSignatureParameters(publicKey, privateKey, sAlgorithm, sProvider)
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
        log.debug(s"Save type information.")
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
        log.debug(s"Save signatures map.")
        sData.get(SimpleSignature.printStream).foreach(streamContainer ⇒
          streamContainer.synchronized {
            Option(streamContainer.get).foreach {
              case (printStream, allEntitiesSignatureStream) ⇒
                log.debug(s"Close container with signatures.")
                printStream.println(Serialization.byteArrayToHexString(allEntitiesSignatureStream.signature.sign()))
                printStream.flush()
                printStream.close()
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
    updated(SimpleSignature.printStream, new AtomicReference[(PrintStream, Signature.SignatureOutputStream[Unit])]())
  /** Just invoked after read beginning. */
  def readFilter(parameters: Mechanism.Parameters, context: AtomicReference[SoftReference[AnyRef]],
    modified: Element.Timestamp, is: InputStream, uri: URI, transport: Transport, sData: SData): InputStream = parameters match {
    case SimpleSignatureParameters(publicKey, _, Some(sAlgorithm), sProvider) ⇒
      if (sData(Signature.Key.acquire)(None)) {
        val storageURI = sData(SData.Key.storageURI)
        Signature.approve(Digest.digestURI(storageURI, transport, modified).resolve(uri), sData)
        // Signature isn't required for this URI. Return the original stream.
        is
      } else {
        // check map
        val map = getSignatureMap(context, modified, transport, publicKey, sAlgorithm, sProvider, sData)
        if (map.isDefinedAt(uri)) {
          val verifier = sProvider match {
            case Some(provider) ⇒
              java.security.Signature.getInstance(sAlgorithm, provider)
            case None ⇒
              java.security.Signature.getInstance(sAlgorithm)
          }
          verifier.initVerify(publicKey)
          new Signature.SignatureInputStream(is, verifier, verifier ⇒
            checkSignature(publicKey, sAlgorithm, sProvider, verifier, context, modified, uri, transport, sData))
        } else {
          val storageURI = sData(SData.Key.storageURI)
          Signature.refuse(Digest.digestURI(storageURI, transport, modified).resolve(uri), sData)
          // There is no signature for this URI. Return the original stream.
          is
        }
      }
    case unexpected ⇒
      throw new IllegalArgumentException("Unexpected parameters " + unexpected)
  }
  /** Just invoked after write beginning. */
  def writeFilter(parameters: Mechanism.Parameters, os: OutputStream, uri: URI,
    transport: Transport, sData: SData): OutputStream = parameters match {
    case SimpleSignatureParameters(publicKey, privateKeyOption, sAlgorithm, sProvider) ⇒
      val privateKey = privateKeyOption getOrElse { throw new IllegalStateException("Unable to sign without private key.") }
      // Initialize SimpleSignature.printStream if needed (only once).
      sData.get(SimpleSignature.printStream).foreach(streamContainer ⇒ streamContainer.synchronized {
        Option(streamContainer.get) getOrElse {
          val modified = sData(SData.Key.modified)
          val storageURI = sData(SData.Key.storageURI)
          val signatureDataURI = Signature.signatureURI(storageURI, transport, modified, Signature.containerName)
          if (!transport.exists(Serialization.inner.encode(signatureDataURI, sData), sData) ||
            sData.get(SData.Key.force) == Some(true)) {
            log.debug(s"Open container with signatures for ${modified} at ${signatureDataURI}")
            val stream = transport.openWrite(Serialization.inner.encode(signatureDataURI, sData), sData, true)
            val allEntitiesSignatureStream = new Signature.SignatureOutputStream(new BufferedOutputStream(stream),
              SimpleSignature.getInstance(privateKey, sAlgorithm, sProvider), _ ⇒ {})
            val printStream = new PrintStream(allEntitiesSignatureStream)
            streamContainer.set((printStream, allEntitiesSignatureStream))
          } else {
            log.debug(s"Skip signature data ${modified} for storage ${storageURI}")
          }
        }
      })
      // Calculate signature inside Signature.SignatureOutputStream.
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

  /** Check signature data. */
  @throws[SecurityException]("if verification is failed")
  protected def checkSignature(publicKey: PublicKey, sAlgorithm: String, sProvider: Option[String],
    verifier: java.security.Signature, context: AtomicReference[SoftReference[AnyRef]],
    modified: Element.Timestamp, uri: URI, transport: Transport, sData: SData) {
    val storageURI = sData(SData.Key.storageURI)
    try {
      val map = getSignatureMap(context, modified, transport, publicKey, sAlgorithm, sProvider, sData)
      map.get(uri) match {
        case Some(originalValue) ⇒
          // throws SecurityException
          if (verifier.verify(originalValue)) {
            if (sData(Signature.Key.acquire)(Some(publicKey)))
              Signature.approve(Digest.digestURI(storageURI, transport, modified).resolve(uri), sData)
            else
              Signature.refuse(Digest.digestURI(storageURI, transport, modified).resolve(uri), sData)
          } else {
            Signature.refuse(Digest.digestURI(storageURI, transport, modified).resolve(uri), sData)
          }
        case None ⇒
          Signature.refuse(Digest.digestURI(storageURI, transport, modified).resolve(uri), sData)
      }
    } catch {
      case e: SecurityException ⇒
        log.warn(e.getMessage)
        Signature.refuse(Digest.digestURI(storageURI, transport, modified).resolve(uri), sData)
      case e: IOException ⇒
        log.warn(e.getMessage)
        Signature.refuse(Digest.digestURI(storageURI, transport, modified).resolve(uri), sData)
      case e: SignatureException ⇒
        log.warn(e.getMessage)
        Signature.refuse(Digest.digestURI(storageURI, transport, modified).resolve(uri), sData)
      case e: Throwable ⇒
        log.fatal(e.getMessage, e)
        Signature.refuse(Digest.digestURI(storageURI, transport, modified).resolve(uri), sData)
    }
  }
  /** Get loaded or load new signature map. */
  protected def getSignatureMap(context: AtomicReference[SoftReference[AnyRef]],
    modified: Element.Timestamp, transport: Transport,
    publicKey: PublicKey, sAlgorithm: String, sProvider: Option[String],
    sData: SData): Map[URI, Array[Byte]] = {
    context.get().get match {
      case Some(map: Map[_, _]) ⇒ return map.asInstanceOf[Map[URI, Array[Byte]]]
      case _ ⇒ context.get().clear
    }
    val storageURI = sData(SData.Key.storageURI)
    var map: Option[Map[URI, Array[Byte]]] = None
    try {
      log.debug(s"Load signature data from storage ${storageURI}")
      val builder = Map.newBuilder[URI, Array[Byte]]
      val digestSumURI = Signature.signatureURI(storageURI, transport, modified, Signature.containerName)
      val digestStream = transport.openRead(Serialization.inner.encode(digestSumURI, sData), sData)
      val verifier = sProvider match {
        case Some(provider) ⇒
          java.security.Signature.getInstance(sAlgorithm, provider)
        case None ⇒
          java.security.Signature.getInstance(sAlgorithm)
      }
      verifier.initVerify(publicKey)
      val reader = new BufferedReader(new InputStreamReader(new BufferedInputStream(digestStream)))
      var validated = false
      try {
        var line = reader.readLine()
        while (line != null) {
          val hash = line.takeWhile(_ != ' ')
          if (line.length() > hash.length()) {
            verifier.update(line.getBytes() ++ "\n".getBytes())
            val uri = new URI(line.drop(hash.length() + 1))
            builder += uri -> Serialization.hexStringToByteArray(hash)
            line = reader.readLine()
          } else {
            try {
              // Last line with container signature
              val containerSignature = Serialization.hexStringToByteArray(hash)
              if (verifier.verify(containerSignature))
                validated = true
              else
                log.warn("Validation failed for container with signatures.")
            } catch {
              case e: Throwable ⇒
                log.warn("Unable to validate container with signatures: " + e.getMessage())
                builder.clear
            }
            line = null
          }
        }
      } finally try reader.close() catch { case e: Throwable ⇒ log.fatal(e.getMessage, e) }
      if (validated)
        map = Option(builder.result)
    } catch {
      case e: IOException ⇒
        log.error("Unable to load signatures map: " + e.getMessage())
      case e: Throwable ⇒
        log.error("Unable to load signatures map: " + e.getMessage(), e)
    }
    context.set(new SoftReference(map.getOrElse(Map())))
    map.getOrElse(Map())
  }
  /** Write signature data to file. */
  protected def writeSignature(signature: java.security.Signature, uri: URI, transport: Transport, sData: SData) =
    sData.get(SimpleSignature.printStream).foreach(streamContainer ⇒ streamContainer.synchronized {
      Option(streamContainer.get).foreach {
        case (printStream, allEntitiesSignatureStream) ⇒
          val modified = sData(SData.Key.modified)
          val storageURI = sData(SData.Key.storageURI)
          log.debug(s"Append signature for '${Digest.digestURI(storageURI, transport, modified).resolve(uri)}'.")
          printStream.println(Serialization.byteArrayToHexString(signature.sign()) + " " + uri)
      }
    })

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
    /** Signature parameters as sequence of strings. */
    val arguments: Seq[String] =
      Seq(mechanism.savePublicKey(publicKey), privateKey.map(mechanism.savePrivateKey).getOrElse(""), sAlgorithm.getOrElse(""), sProvider.getOrElse(""))
    /** SimpleSignature mechanism instance. */
    lazy val mechanism = SimpleSignature.this

    override def toString() = privateKey match {
      case Some(privateKey) ⇒
        s"SimpleSignatureParameters(public ${publicKey.getFormat()} ${publicKey.getAlgorithm()} key, private ${publicKey.getAlgorithm()} key, ${sAlgorithm}, ${sProvider})"
      case None ⇒
        s"SimpleSignatureParameters(public ${publicKey.getFormat()} ${publicKey.getAlgorithm()} key, ${sAlgorithm}, ${sProvider})"
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
  val printStream = SData.key[AtomicReference[(PrintStream, Signature.SignatureOutputStream[Unit])]]("simpleSignaturePrintStream")

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
      log.debug(s"Get signature instance for ${privateKey.getAlgorithm()} key with algorithm ${algorithm.getOrElse("'default'")} and provider ${provider.getOrElse("'default'")}.")
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
