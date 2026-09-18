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
  def sha256(input: IArray[Byte]): ByteDigest = new ByteDigest(Sha256Core.digest(input))

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

/** FIPS 180-4 SHA-256 over `Int` words; exact on the JVM and Scala.js. */
private[eyes4s] object Sha256Core:
  private val Initial = IArray(
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab,
    0x5be0cd19
  )

  private val RoundConstants = IArray(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4,
    0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe,
    0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f,
    0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc,
    0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
    0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116,
    0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
    0xc67178f2
  )

  /** The 32-byte digest. Padding is applied block by block, so the input is
    * never copied as a whole.
    */
  def digest(input: IArray[Byte]): IArray[Byte] =
    val length      = input.length
    val bitLength   = length.toLong * 8L
    val totalBlocks = ((length.toLong + 9L + 63L) / 64L).toInt
    val block       = Array.ofDim[Byte](64)
    val hash        = Array.tabulate(Initial.length)(Initial(_))
    val words       = Array.ofDim[Int](64)
    var blockIndex  = 0
    while blockIndex < totalBlocks do
      val start = blockIndex * 64
      var i     = 0
      while i < 64 do
        val at = start + i
        block(i) =
          if at < length then input(at)
          else if at == length then 0x80.toByte
          else 0
        i += 1
      if blockIndex == totalBlocks - 1 then
        i = 0
        while i < 8 do
          block(63 - i) = ((bitLength >>> (i * 8)) & 0xff).toByte
          i += 1
      compress(block, hash, words)
      blockIndex += 1
    val output = Array.ofDim[Byte](32)
    var index  = 0
    while index < hash.length do
      output(index * 4) = (hash(index) >>> 24).toByte
      output(index * 4 + 1) = (hash(index) >>> 16).toByte
      output(index * 4 + 2) = (hash(index) >>> 8).toByte
      output(index * 4 + 3) = hash(index).toByte
      index += 1
    IArray.unsafeFromArray(output)

  private def compress(block: Array[Byte], hash: Array[Int], words: Array[Int]): Unit =
    var index = 0
    while index < 16 do
      val at = index * 4
      words(index) = ((block(at) & 0xff) << 24) |
        ((block(at + 1) & 0xff) << 16) |
        ((block(at + 2) & 0xff) << 8) |
        (block(at + 3) & 0xff)
      index += 1
    while index < 64 do
      val s0 = rotateRight(words(index - 15), 7) ^
        rotateRight(words(index - 15), 18) ^
        (words(index - 15) >>> 3)
      val s1 = rotateRight(words(index - 2), 17) ^
        rotateRight(words(index - 2), 19) ^
        (words(index - 2) >>> 10)
      words(index) = words(index - 16) + s0 + words(index - 7) + s1
      index += 1

    var a = hash(0)
    var b = hash(1)
    var c = hash(2)
    var d = hash(3)
    var e = hash(4)
    var f = hash(5)
    var g = hash(6)
    var h = hash(7)
    index = 0
    while index < 64 do
      val sum1       = rotateRight(e, 6) ^ rotateRight(e, 11) ^ rotateRight(e, 25)
      val choose     = (e & f) ^ ((~e) & g)
      val temporary1 = h + sum1 + choose + RoundConstants(index) + words(index)
      val sum0       = rotateRight(a, 2) ^ rotateRight(a, 13) ^ rotateRight(a, 22)
      val majority   = (a & b) ^ (a & c) ^ (b & c)
      val temporary2 = sum0 + majority
      h = g
      g = f
      f = e
      e = d + temporary1
      d = c
      c = b
      b = a
      a = temporary1 + temporary2
      index += 1

    hash(0) += a
    hash(1) += b
    hash(2) += c
    hash(3) += d
    hash(4) += e
    hash(5) += f
    hash(6) += g
    hash(7) += h

  private def rotateRight(value: Int, distance: Int): Int =
    (value >>> distance) | (value << (32 - distance))

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
