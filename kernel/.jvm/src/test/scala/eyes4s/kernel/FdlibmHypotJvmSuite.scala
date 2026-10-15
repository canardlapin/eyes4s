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

package eyes4s.kernel

class FdlibmHypotJvmSuite extends munit.FunSuite:
  private def bits(value: Double): Long = java.lang.Double.doubleToLongBits(value)
  test(
    "shared fdlibm agrees bit for bit with StrictMath over100000 raw pairs and20000 close-exponent pairs"
  ) {
    var state        = 0x5eed1234abcdefL
    def next(): Long =
      state ^= state << 13
      state ^= state >>> 7
      state ^= state << 17
      state
    def check(a: Double, b: Double, index: Int): Unit =
      val expected = bits(java.lang.StrictMath.hypot(a, b))
      assertEquals(
        bits(FdlibmHypot(a, b)),
        expected,
        s"pair=$index a=${java.lang.Long.toHexString(bits(a))} b=${java.lang.Long.toHexString(bits(b))}"
      )
      assertEquals(bits(java.lang.Math.hypot(a, b)), expected)
    var index = 0
    while index < 100000 do
      check(
        java.lang.Double.longBitsToDouble(next()),
        java.lang.Double.longBitsToDouble(next()),
        index
      )
      index += 1
    index = 0
    while index < 20000 do
      val exponent = (index % 2047).toLong << 52
      val a = java.lang.Double.longBitsToDouble(exponent | (next() & 0x000fffffffffffffL))
      val b = java.lang.Double.longBitsToDouble(exponent | (next() & 0x000fffffffffffffL))
      check(a, b, 100000 + index)
      index += 1
  }
