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

package eyes4s.studio.app.text

import scala.math.BigDecimal.RoundingMode

/** Number formatting in the boards' conventions, identical on the JVM and
  * Scala.js (no platform locale is consulted).
  */
object Format:

  /** U+2212 MINUS SIGN: every negative number shown uses it, never '-'. */
  val Minus: String = "−"

  /** "21,400": digits grouped in threes; negatives take [[Minus]]. */
  def count(value: Long): String =
    val digits =
      if value == Long.MinValue then "9223372036854775808" else math.abs(value).toString
    val grouped = digits.reverse.grouped(3).mkString(",").reverse
    if value < 0 then Minus + grouped else grouped

  /** `value` rounded half-up to `decimals` places ("0.38", "−0.08"). Not a
    * finite number renders as "—".
    */
  def decimal(value: Double, decimals: Int): String =
    if value.isNaN || value.isInfinite then "—"
    else
      val places  = decimals.max(0)
      val rounded = BigDecimal(value).setScale(places, RoundingMode.HALF_UP)
      val text    = rounded.abs.bigDecimal.toPlainString
      if rounded.signum < 0 then Minus + text else text

  /** A difference with an explicit sign: "+0.38", "−0.08", "0.00". */
  def signed(value: Double, decimals: Int): String =
    val text = decimal(value, decimals)
    if text.startsWith(Minus) || text == "—" || text.forall(c => c == '0' || c == '.') then text
    else "+" + text

  /** A fraction in [0, 1] as a whole percentage ("48%"); clamped. */
  def percent(fraction: Double): String =
    val clamped = if fraction.isNaN then 0.0 else fraction.max(0.0).min(1.0)
    s"${BigDecimal(clamped * 100).setScale(0, RoundingMode.HALF_UP).toInt}%"

  /** "10:24". */
  def clock(hour: Int, minute: Int): String =
    f"$hour%02d:$minute%02d"
