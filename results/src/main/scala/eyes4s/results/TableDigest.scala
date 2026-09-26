/*
 * Copyright 2026 canardlapin
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

package eyes4s.results

import scala.collection.mutable.ArrayBuffer

/** The SHA-256 identity of a result table: of its schema, family, context,
  * columns and cells, in the length-framed canonical text
  * [[ResultTable.of]] builds. The digest is pure Scala, identical on the JVM
  * and Scala.js, and equal to `eyes4s.io.Sha256.ofUtf8` of the same text.
  */
final class TableDigest private (private val digest: IArray[Byte]):
  /** 64 lowercase hexadecimal digits. */
  lazy val hex: String =
    val digits = "0123456789abcdef"
    val out    = new java.lang.StringBuilder(64)
    var index  = 0
    while index < digest.length do
      val value = digest(index) & 0xff
      out.append(digits.charAt(value >>> 4))
      out.append(digits.charAt(value & 0x0f))
      index += 1
    out.toString

  override def equals(other: Any): Boolean = other match
    case that: TableDigest => hex == that.hex
    case _                 => false
  override def hashCode: Int    = hex.hashCode
  override def toString: String = hex

object TableDigest:
  given CanEqual[TableDigest, TableDigest] = CanEqual.derived

  /** The digest of the UTF-8 bytes of `value`, lone surrogates encoded as
    * `eyes4s.io.Sha256.ofUtf8` has always encoded them.
    */
  def ofUtf8(value: String): TableDigest = new TableDigest(
    Sha256Core.digest(Utf8Lenient(value))
  )

/** UTF-8 with a lone surrogate replaced by U+FFFD, except a high surrogate
  * that ends the text, which is encoded as it stands.
  */
private[eyes4s] object Utf8Lenient:
  def apply(value: String): IArray[Byte] =
    val bytes = ArrayBuffer.empty[Byte]
    var index = 0
    while index < value.length do
      val first     = value.charAt(index).toInt
      val codePoint =
        if first >= 0xd800 && first <= 0xdbff && index + 1 < value.length then
          val second = value.charAt(index + 1).toInt
          if second >= 0xdc00 && second <= 0xdfff then
            index += 1
            0x10000 + ((first - 0xd800) << 10) + (second - 0xdc00)
          else 0xfffd
        // A high surrogate that ends the text is encoded as it stands, as
        // `eyes4s.io.Sha256.ofUtf8` has always encoded it.
        else if first >= 0xdc00 && first <= 0xdfff then 0xfffd
        else first
      if codePoint <= 0x7f then bytes += codePoint.toByte
      else if codePoint <= 0x7ff then
        bytes += (0xc0 | (codePoint >>> 6)).toByte
        bytes += (0x80 | (codePoint & 0x3f)).toByte
      else if codePoint <= 0xffff then
        bytes += (0xe0 | (codePoint >>> 12)).toByte
        bytes += (0x80 | ((codePoint >>> 6) & 0x3f)).toByte
        bytes += (0x80 | (codePoint & 0x3f)).toByte
      else
        bytes += (0xf0 | (codePoint >>> 18)).toByte
        bytes += (0x80 | ((codePoint >>> 12) & 0x3f)).toByte
        bytes += (0x80 | ((codePoint >>> 6) & 0x3f)).toByte
        bytes += (0x80 | (codePoint & 0x3f)).toByte
      index += 1
    IArray.from(bytes)
