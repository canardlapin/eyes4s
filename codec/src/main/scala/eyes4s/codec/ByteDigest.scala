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

package eyes4s.codec

/** The SHA-256 digest of an exact byte sequence: the integrity checksum of a
  * stored artifact.
  *
  * It is deliberately separate from the kernel's portable
  * [[eyes4s.kernel.ContentHash]], which identifies scientific content (an
  * input's trials, a recording's samples) and survives a change of JSON
  * formatting. A byte digest changes with every byte, so it answers "are these
  * the bytes that were written?" and nothing else. The implementation is pure
  * Scala over `Int` arithmetic, so the same bytes have the same digest on the
  * JVM and Scala.js; `eyes4s.io.Sha256` delegates to it.
  */
final class ByteDigest private (private val digest: IArray[Byte]):
  /** The 32 digest bytes, in the big-endian order SHA-256 specifies. */
  def bytes: IArray[Byte] = digest

  /** Canonical rendering: 64 lowercase hexadecimal digits. */
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
    case that: ByteDigest => hex == that.hex
    case _                => false
  override def hashCode: Int    = hex.hashCode
  override def toString: String = hex

object ByteDigest:
  given CanEqual[ByteDigest, ByteDigest] = CanEqual.derived

  /** The digest of exactly these bytes. */
  def sha256(input: IArray[Byte]): ByteDigest = new ByteDigest(
    eyes4s.results.Sha256Core.digest(input)
  )

  /** The digest of everything `hasher` was given. */
  private[codec] def finished(hasher: eyes4s.results.Sha256Core.Hasher): ByteDigest =
    new ByteDigest(hasher.finish())

  /** Parse the canonical rendering: exactly 64 lowercase hexadecimal digits.
    * Upper-case digits are refused so that one digest has one spelling.
    */
  def parse(value: String): Either[ByteDigestError, ByteDigest] =
    if value.length != 64 then Left(ByteDigestError.WrongLength(value, value.length))
    else
      value.zipWithIndex.collectFirst {
        case (c, i) if !((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) =>
          ByteDigestError.InvalidCharacter(value, i, c)
      } match
        case Some(error) => Left(error)
        case None        =>
          val out = Array.ofDim[Byte](32)
          var i   = 0
          while i < 32 do
            out(i) =
              ((hexValue(value.charAt(2 * i)) << 4) | hexValue(value.charAt(2 * i + 1))).toByte
            i += 1
          Right(new ByteDigest(IArray.unsafeFromArray(out)))

  private def hexValue(c: Char): Int = if c <= '9' then c - '0' else c - 'a' + 10

/** A malformed rendering of a byte digest, carrying the offending text. */
enum ByteDigestError derives CanEqual:
  case WrongLength(value: String, length: Int)
  case InvalidCharacter(value: String, index: Int, character: Char)

  def message: String = this match
    case WrongLength(value, length) =>
      s"SHA-256 digest '$value' has $length characters; expected 64 lowercase hexadecimal digits."
    case InvalidCharacter(value, index, character) =>
      s"SHA-256 digest '$value' has '$character' at index $index; expected lowercase hexadecimal."

/** Strict UTF-8, identical on the JVM and Scala.js. Encoding refuses a lone
  * surrogate and decoding refuses malformed, overlong, surrogate or
  * out-of-range sequences, so one text has exactly one byte form.
  */
private[codec] object Utf8:
  /** The encoded bytes, or the index of the first lone surrogate. */
  def encode(value: String): Either[Int, IArray[Byte]] =
    val out   = scala.collection.mutable.ArrayBuilder.make[Byte]
    var index = 0
    var error = -1
    while index < value.length && error < 0 do
      val first = value.charAt(index).toInt
      if first < 0x80 then
        out += first.toByte
        index += 1
      else if first < 0x800 then
        out += (0xc0 | (first >>> 6)).toByte
        out += (0x80 | (first & 0x3f)).toByte
        index += 1
      else if first >= 0xd800 && first <= 0xdbff then
        val second =
          if index + 1 < value.length then value.charAt(index + 1).toInt else -1
        if second >= 0xdc00 && second <= 0xdfff then
          val codePoint = 0x10000 + ((first - 0xd800) << 10) + (second - 0xdc00)
          out += (0xf0 | (codePoint >>> 18)).toByte
          out += (0x80 | ((codePoint >>> 12) & 0x3f)).toByte
          out += (0x80 | ((codePoint >>> 6) & 0x3f)).toByte
          out += (0x80 | (codePoint & 0x3f)).toByte
          index += 2
        else error = index
      else if first >= 0xdc00 && first <= 0xdfff then error = index
      else
        out += (0xe0 | (first >>> 12)).toByte
        out += (0x80 | ((first >>> 6) & 0x3f)).toByte
        out += (0x80 | (first & 0x3f)).toByte
        index += 1
    if error >= 0 then Left(error) else Right(IArray.unsafeFromArray(out.result()))

  /** The decoded text, or the byte offset of the first malformed sequence. */
  def decode(bytes: IArray[Byte]): Either[Int, String] =
    val out                        = new java.lang.StringBuilder(bytes.length)
    var index                      = 0
    var error                      = -1
    def continuation(at: Int): Int =
      if at < bytes.length && (bytes(at) & 0xc0) == 0x80 then bytes(at) & 0x3f else -1
    while index < bytes.length && error < 0 do
      val first = bytes(index) & 0xff
      if first < 0x80 then
        out.append(first.toChar)
        index += 1
      else if first >= 0xc2 && first <= 0xdf then
        val b1 = continuation(index + 1)
        if b1 < 0 then error = index
        else
          out.append((((first & 0x1f) << 6) | b1).toChar)
          index += 2
      else if first >= 0xe0 && first <= 0xef then
        val b1        = continuation(index + 1)
        val b2        = continuation(index + 2)
        val codePoint = ((first & 0x0f) << 12) | (b1 << 6) | b2
        if b1 < 0 || b2 < 0 || codePoint < 0x800 || (codePoint >= 0xd800 && codePoint <= 0xdfff)
        then error = index
        else
          out.append(codePoint.toChar)
          index += 3
      else if first >= 0xf0 && first <= 0xf4 then
        val b1        = continuation(index + 1)
        val b2        = continuation(index + 2)
        val b3        = continuation(index + 3)
        val codePoint = ((first & 0x07) << 18) | (b1 << 12) | (b2 << 6) | b3
        if b1 < 0 || b2 < 0 || b3 < 0 || codePoint < 0x10000 || codePoint > 0x10ffff then
          error = index
        else
          val shifted = codePoint - 0x10000
          out.append((0xd800 + (shifted >>> 10)).toChar)
          out.append((0xdc00 + (shifted & 0x3ff)).toChar)
          index += 4
      else error = index
    if error >= 0 then Left(error) else Right(out.toString)
