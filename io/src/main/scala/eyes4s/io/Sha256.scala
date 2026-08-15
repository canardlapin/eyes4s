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
  * UTF-8 source has exactly the same digest on the JVM and Scala.js.
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

  def ofUtf8(value: String): Sha256 = ofBytes(utf8(value))

  def ofBytes(input: IArray[Byte]): Sha256 =
    val bitLength   = input.length.toLong * 8L
    val paddedBytes = ((input.length + 9 + 63) / 64) * 64
    val message     = Array.fill[Byte](paddedBytes)(0)
    var index       = 0
    while index < input.length do
      message(index) = input(index)
      index += 1
    message(input.length) = 0x80.toByte
    index = 0
    while index < 8 do
      message(paddedBytes - 1 - index) = ((bitLength >>> (index * 8)) & 0xff).toByte
      index += 1

    val hash  = Array.tabulate(Initial.length)(Initial(_))
    val words = Array.ofDim[Int](64)
    var block = 0
    while block < paddedBytes do
      index = 0
      while index < 16 do
        val at = block + index * 4
        words(index) = ((message(at) & 0xff) << 24) |
          ((message(at + 1) & 0xff) << 16) |
          ((message(at + 2) & 0xff) << 8) |
          (message(at + 3) & 0xff)
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
      block += 64

    val output = Array.ofDim[Byte](32)
    index = 0
    while index < hash.length do
      output(index * 4) = (hash(index) >>> 24).toByte
      output(index * 4 + 1) = (hash(index) >>> 16).toByte
      output(index * 4 + 2) = (hash(index) >>> 8).toByte
      output(index * 4 + 3) = hash(index).toByte
      index += 1
    new Sha256(IArray.from(output))

  private def rotateRight(value: Int, distance: Int): Int =
    (value >>> distance) | (value << (32 - distance))

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

end Sha256
