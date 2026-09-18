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

class CsvNumberSuite extends munit.FunSuite:
  private def bits(value: Double): Long = java.lang.Double.doubleToLongBits(value)

  // Literal decimal oracle, not runtime Double.toString output. Both backends must
  // produce these exact ASCII bytes, including exponent style and signed zero.
  private val pinned = Vector(
    0.0                                                    -> "0",
    -0.0                                                   -> "-0",
    1.0                                                    -> "1",
    -1.0                                                   -> "-1",
    1000.0                                                 -> "1E+3",
    0.1                                                    -> "0.1",
    0.5                                                    -> "0.5",
    2.5                                                    -> "2.5",
    0.000001                                               -> "0.000001",
    0.0000001                                              -> "1E-7",
    1e23                                                   -> "1E+23",
    -1e23                                                  -> "-1E+23",
    9007199254740992.0                                     -> "9007199254740992",
    java.lang.Double.longBitsToDouble(0x3fefffffffffffffL) -> "0.9999999999999999",
    java.lang.Double.longBitsToDouble(0x3ff0000000000001L) -> "1.0000000000000002",
    java.lang.Double.longBitsToDouble(1L)                  -> "5E-324",
    java.lang.Double.longBitsToDouble(0x000fffffffffffffL) -> "2.225073858507201E-308",
    java.lang.Double.longBitsToDouble(0x0010000000000000L) -> "2.2250738585072014E-308",
    Double.MaxValue                                        -> "1.7976931348623157E+308"
  )

  test("pinned decimal CSV bytes are identical on JVM and Scala.js") {
    assertEquals(
      Rfc4180.encode(pinned.map((value, _) => Vector(CsvNumber.render(value)))),
      pinned.map((_, expected) => expected + "\r\n").mkString
    )
    pinned.foreach { (value, expected) =>
      assertEquals(bits(expected.toDouble), bits(value), expected)
    }
  }

  test("every binary exponent, both signs and adversarial mantissas round-trip exactly") {
    val mantissas = Vector(0L, 1L, 0x0008000000000000L, 0x000fffffffffffffL)
    val rendered  = new StringBuilder
    for
      exponent <- 0 until 2047
      mantissa <- mantissas
      sign     <- Vector(0L, Long.MinValue)
    do
      val raw  = sign | (exponent.toLong << 52) | mantissa
      val text = CsvNumber.render(java.lang.Double.longBitsToDouble(raw))
      assertEquals(bits(text.toDouble), raw, text)
      assertEquals(CsvNumber.render(text.toDouble), text)
      rendered.append(text).append('\n')
    // Independently reproduced with Python Decimal.from_float, ROUND_HALF_EVEN at
    // precisions 1..17, float round-trip selection, then normalize at precision 1100.
    // This pins every output byte across backends, not just the parsed values.
    assertEquals(
      Sha256.ofUtf8(rendered.toString).hex,
      "af7055f3005ce812e93e638b7f8c2e9129c16163898c5e1514de8bec708027f7"
    )
  }

  test("non-finite helper inputs have explicit spellings rather than throwing") {
    assertEquals(CsvNumber.render(Double.NaN), "NaN")
    assertEquals(CsvNumber.render(Double.PositiveInfinity), "Infinity")
    assertEquals(CsvNumber.render(Double.NegativeInfinity), "-Infinity")
  }
