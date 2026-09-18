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

package eyes4s.io

import scala.collection.mutable.ArrayBuffer

/** Persistent SHA-256 identity of imported source bytes.
  *
  * This is deliberately separate from the kernel's fast [[eyes4s.kernel.ContentHash]]:
  * the latter is a cache key, while this value is suitable for recording in a
  * durable scientific artifact. The implementation is pure Scala so the same
  * UTF-8 source has exactly the same digest on the JVM and Scala.js; it is
  * [[eyes4s.codec.ByteDigest]]'s, so a source digest and a manifest entry
  * digest over the same bytes are equal.
  */
final class Sha256 private (private val digestBytes: IArray[Byte]) derives CanEqual:
  lazy val hex: String =
    val digits = "0123456789abcdef"
    val out    = new java.lang.StringBuilder(digestBytes.length * 2)
    var index  = 0
    while index < digestBytes.length do
      val value = digestBytes(index) & 0xff
      out.append(digits.charAt(value >>> 4))
      out.append(digits.charAt(value & 0x0f))
      index += 1
    out.toString

  override def equals(other: Any): Boolean = other match
    case digest: Sha256 => hex == digest.hex
    case _              => false

  override def hashCode: Int    = hex.hashCode
  override def toString: String = hex

object Sha256:
  def ofUtf8(value: String): Sha256 = ofBytes(utf8(value))

  /** Parse a canonical lower- or upper-case hexadecimal SHA-256 digest.
    *
    * The operand is carried into every failure so an application can identify
    * which source, converter, or artifact digest was malformed.
    */
  def fromHex(operand: String, value: String): Either[Sha256Error, Sha256] =
    if value.length != 64 then Left(Sha256Error.WrongLength(operand, value.length))
    else
      val output                     = Array.ofDim[Byte](32)
      var index                      = 0
      var error: Option[Sha256Error] = None
      while index < output.length && error.isEmpty do
        val highIndex = index * 2
        val lowIndex  = highIndex + 1
        (hexDigit(value.charAt(highIndex)), hexDigit(value.charAt(lowIndex))) match
          case (Some(high), Some(low)) => output(index) = ((high << 4) | low).toByte
          case (None, _)               =>
            error = Some(
              Sha256Error.InvalidCharacter(operand, highIndex, value.charAt(highIndex))
            )
          case (_, None) =>
            error = Some(
              Sha256Error.InvalidCharacter(operand, lowIndex, value.charAt(lowIndex))
            )
        index += 1
      error.toLeft(new Sha256(IArray.from(output)))

  /** The digest of exactly these bytes, computed by the one pure-Scala
    * SHA-256 implementation shared with manifest verification,
    * [[eyes4s.codec.ByteDigest.sha256]].
    */
  def ofBytes(input: IArray[Byte]): Sha256 =
    new Sha256(eyes4s.codec.ByteDigest.sha256(input).bytes)

  private def utf8(value: String): IArray[Byte] =
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
        else if first >= 0xdc00 && first <= 0xdfff then 0xfffd
        else first
      appendUtf8(bytes, codePoint)
      index += 1
    IArray.from(bytes)

  private def appendUtf8(bytes: ArrayBuffer[Byte], codePoint: Int): Unit =
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

  private def hexDigit(value: Char): Option[Int] =
    if value >= '0' && value <= '9' then Some(value - '0')
    else if value >= 'a' && value <= 'f' then Some(value - 'a' + 10)
    else if value >= 'A' && value <= 'F' then Some(value - 'A' + 10)
    else None

end Sha256

enum Sha256Error derives CanEqual:
  case WrongLength(operand: String, actual: Int)
  case InvalidCharacter(operand: String, index: Int, value: Char)

  def message: String = this match
    case WrongLength(operand, actual) =>
      s"SHA-256 operand='$operand' has hexadecimal length=$actual; expected=64."
    case InvalidCharacter(operand, index, value) =>
      s"SHA-256 operand='$operand' has non-hexadecimal character='$value' at index=$index."

end Sha256Error
