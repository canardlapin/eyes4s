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

import java.math.{BigDecimal, MathContext, RoundingMode}

/** Runtime-independent decimal cells for contrast and template-training exports.
  *
  * Start from the exact binary value, round HALF_EVEN to successive significant-digit
  * counts (1 through 17), and choose the first decimal that parses to the original bits.
  * Strip trailing decimal zeros and use BigDecimal's canonical spelling (uppercase E,
  * explicit positive exponent). This is a rounded round-trip contract, not a claim to
  * implement every runtime's shortest-decimal convention. Signed zero is preserved.
  *
  * Non-finite spellings make this helper total; scientific exporters admit only finite
  * values. Existing tidy AOI proportions retain their fixed-12-decimal contract, and
  * hexadecimal evidence cells are not decimal display values.
  */
private[io] object CsvNumber:
  private val contexts = (1 to 17).map(p => new MathContext(p, RoundingMode.HALF_EVEN))

  def render(value: Double): String =
    if value.isNaN then "NaN"
    else if value == Double.PositiveInfinity then "Infinity"
    else if value == Double.NegativeInfinity then "-Infinity"
    else if value == 0.0 then
      if java.lang.Double.doubleToLongBits(value) < 0L then "-0" else "0"
    else
      val exact     = exactDecimal(value)
      var precision = 0
      var candidate = exact.round(contexts(precision)).stripTrailingZeros.toString
      while precision < 16 && candidate.toDouble != value do
        precision += 1
        candidate = exact.round(contexts(precision)).stripTrailingZeros.toString
      candidate

  // Do not use BigDecimal(Double): the Scala.js conversion can corrupt large negative
  // values (including -1e23). Integer construction never delegates to runtime formatting
  // or floating-point arithmetic: significand * 2^e = significand * 5^-e / 10^-e for e < 0.
  private def exactDecimal(value: Double): BigDecimal =
    val raw             = java.lang.Double.doubleToLongBits(value)
    val encodedExponent = ((raw >>> 52) & 0x7ffL).toInt
    val fraction        = raw & 0x000fffffffffffffL
    val significand     = if encodedExponent == 0 then fraction else fraction | (1L << 52)
    val signed          = BigInt(if raw < 0L then -significand else significand)
    val exponent        = if encodedExponent == 0 then -1074 else encodedExponent - 1075
    if exponent >= 0 then new BigDecimal((signed << exponent).bigInteger)
    else new BigDecimal((signed * BigInt(5).pow(-exponent)).bigInteger, -exponent)
