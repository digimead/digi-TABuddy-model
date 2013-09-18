/**
 * TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2013 Alexey Aksenov ezh@ezh.msk.ru
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

package org.digimead.tabuddy.model.element

import java.io.File
import java.net.URI
import java.net.URISyntaxException
import java.net.URL
import java.util.Collections
import java.util.Set

case class IRI(val scheme: String, val schemeSpecificPart: String, val fragment: String) extends CharSequence {
  override def charAt(index: Int): Char = {
    if (index < 0)
      throw new IndexOutOfBoundsException(Integer.toString(index))
    if (index >= length())
      throw new IndexOutOfBoundsException(Integer.toString(index))
    if (index < scheme.length())
      return scheme.charAt(index)
    schemeSpecificPart.charAt(index - scheme.length())
  }
  /**
   * Gets the fragment of the IRI.
   *
   * @return The IRI fragment, or <code>null</code> if the IRI does not have a fragment
   */
  def getFragment(): String = schemeSpecificPart
  /** @return the prefix. Can be null. */
  def getNamespace(): String = scheme
  /** @return the IRI scheme, e.g., http, urn... can be null */
  def getScheme(): String = {
    val colonIndex = scheme.indexOf(':')
    if (colonIndex == -1)
      return null
    scheme.substring(0, colonIndex)
  }
  /**
   * Determines if this IRI is absolute
   *
   * @return <code>true</code> if this IRI is absolute or <code>false</code>
   *         if this IRI is not absolute
   */
  def isAbsolute(): Boolean = {
    val colonIndex = scheme.indexOf(':');
    if (colonIndex == -1) {
      return false
    }
    for (i <- 0 until colonIndex) {
      val ch = scheme.charAt(i)
      if (!Character.isLetter(ch) && !Character.isDigit(ch) && ch != '.' && ch != '+' && ch != '-')
        return false;
    }
    true
  }

  override def length(): Int =
    scheme.length() + (if (schemeSpecificPart == null) 0 else schemeSpecificPart.length())
  /**
   * Resolve IRI.
   *
   * @param s the IRI string to be resolved
   * @return s resolved against this IRI (with the URI::resolve() method, unless this IRI is opaque)
   */
  def resolve(s: String): IRI = {
    // shortcut: checking absolute and opaque here saves the creation of an
    // extra URI object
    val uri = URI.create(s)
    //        if (uri.isAbsolute() || uri.isOpaque()) {
    //            return IRI.create(uri)
    //        }
    //        return IRI.create(toURI().resolve(uri).toString())
    null
  }
  override def subSequence(start: Int, end: Int): CharSequence = {
    val sb = new StringBuilder()
    sb.append(scheme)
    sb.append(schemeSpecificPart)
    sb.subSequence(start, end)
  }
  /**
   * Obtained this IRI surrounded by angled brackets
   *
   * @return This IRI surrounded by &lt; and &gt;
   */
  def toQuotedString(): String = {
    val sb = new StringBuilder()
    sb.append("<")
    sb.append(scheme)
    if (schemeSpecificPart != null)
      sb.append(schemeSpecificPart)
    sb.append(">")
    sb.toString()
  }
  /**
   * Obtains this IRI as a URI. Note that Java URIs handle unicode characters,
   * so there is no loss during this translation.
   *
   * @return The URI
   */
  def toURI(): URI = if (schemeSpecificPart != null) {
    val sb = new StringBuilder()
    sb.append(scheme)
    sb.append(schemeSpecificPart)
    URI.create(sb.toString())
  } else
    URI.create(scheme)
  override def equals(obj: Any): Boolean = {
    if (obj == null)
      return false
    if (!(obj.isInstanceOf[IRI]))
      return false
    val other = obj.asInstanceOf[IRI]
    if (other.eq(this))
      return true
    val otherRemainder = other.schemeSpecificPart
    if (schemeSpecificPart == null)
      otherRemainder == null && scheme.equals(other.scheme)
    else
      otherRemainder != null && schemeSpecificPart.equals(otherRemainder) && other.scheme.equals(scheme)
  }
  override def hashCode() = lazyHashCode
  protected lazy val lazyHashCode = java.util.Arrays.hashCode(Array[AnyRef](scheme, schemeSpecificPart))
  override def toString() = {
    if (schemeSpecificPart != null) {
      val sb = new StringBuilder()
      sb.append(scheme)
      sb.append(schemeSpecificPart)
      sb.toString()
    } else
      scheme
  }
}

object IRI {
  /**
   * Creates an IRI from the specified String.
   *
   * @param str The String that specifies the IRI. Cannot be null.
   * @return The IRI that has the specified string representation.
   */
  //  def create(str: String): IRI = {
  // protected IRI(URI uri) {
  /*/** Constructs an IRI which is built from the concatenation of the specified
     * prefix and suffix.
     *
     * @param prefix
     *            The prefix.
     * @param fragment
     *            The suffix. */
    protected IRI(String prefix, String fragment) {
    protected IRI(String s) {
     /** @param file
     *            the file to create the IRI from. Cannot be null.
     * @return file.toURI() IRI */
    public static IRI create(File file) {
    /** @param uri
     *            the uri to create the IRI from. Cannot be null
     * @return the IRI wrapping the uri */
    public static IRI create(URI uri)
    /** @param url
     *            the url to create the IRI from. Cannot be null.
     * @return an IRI wraopping url.toURI()
     * @throws URISyntaxException
     *             if the URL is ill formed */
    public static IRI create(URL url) throws URISyntaxExcepti
    *
    * Creates an IRI by concatenating two strings. The full IRI is an IRI that
     * contains the characters in prefix + suffix
     * @param prefix
     *            The first string. May be <code>null</code>.
     * @param suffix
     *            The second string. May be <code>null</code>.
     * @return An IRI whose characters consist of prefix + suffix.
    IRI create(String prefix, String suffix)
*/
}
