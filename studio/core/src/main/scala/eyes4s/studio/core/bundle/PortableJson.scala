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

package eyes4s.studio.core.bundle

import eyes4s.studio.core.document.CanonicalJson
import io.circe.{Json, JsonNumber}

/** The text a bundle file holds: canonical JSON (members in key order) whose
  * numbers are written the same way on the JVM and Scala.js, so one document
  * is one set of bundle bytes on every platform.
  *
  * The platforms print some doubles differently (`35.0` and `35`, `1.0E-7`
  * and `1e-7`, `4.9E-324` and `5e-324`). A number that is exactly a 64-bit
  * integer is therefore written as that integer, `-0.0` as itself, and any
  * other as the ECMAScript `Number.prototype.toString` form of its shortest
  * round-trip digits. Reading gives the same values back, bit for bit.
  */
object PortableJson:
  def print(json: Json): String = numbers(CanonicalJson(json)).noSpaces

  private def numbers(json: Json): Json =
    json.fold(
      json,
      _ => json,
      n => Json.fromJsonNumber(JsonNumber.fromDecimalStringUnsafe(render(n))),
      _ => json,
      items => Json.fromValues(items.map(numbers)),
      members => Json.fromJsonObject(members.mapValues(numbers))
    )

  private[bundle] def render(n: JsonNumber): String =
    val d = n.toDouble
    if d == 0.0 && 1.0 / d < 0 then "-0.0" // -0.0 keeps its sign, which digests see
    else
      n.toLong match
        case Some(l) => l.toString
        case None    => ecmascript(d)

  /** `d`'s shortest round-trip digits in ECMAScript's layout: positional for
    * decimal exponents in (-7, 21], scientific (`1.5e-7`, `2e+21`) otherwise.
    */
  private[bundle] def ecmascript(d: Double): String =
    if d.isNaN || d.isInfinite then d.toString
    else if d == 0.0 then "0"
    else
      val text                 = java.lang.Double.toString(math.abs(d))
      val (mantissa, exponent) = text.indexWhere(c => c == 'E' || c == 'e') match
        case -1 => (text, 0)
        case i  => (text.take(i), text.drop(i + 1).stripPrefix("+").toInt)
      val (whole, fraction) = mantissa.indexOf('.') match
        case -1 => (mantissa, "")
        case i  => (mantissa.take(i), mantissa.drop(i + 1))
      val all     = whole + fraction
      val leading = all.takeWhile(_ == '0').length
      // The value is 0.<digits> × 10^point.
      val (digits, point) =
        shortest(math.abs(d), trimmed(all.drop(leading)), whole.length + exponent - leading)
      val k    = digits.length
      val body =
        if k <= point && point <= 21 then digits + "0" * (point - k)
        else if 0 < point && point <= 21 then digits.take(point) + "." + digits.drop(point)
        else if -6 < point && point <= 0 then "0." + "0" * -point + digits
        else
          val e    = point - 1
          val sign = if e < 0 then "-" else "+"
          val head = if k == 1 then digits else digits.take(1) + "." + digits.drop(1)
          s"${head}e$sign${math.abs(e)}"
      if d < 0 then "-" + body else body

  private def trimmed(digits: String): String = digits.reverse.dropWhile(_ == '0').reverse

  /** The fewest significant digits that read back as `d` (positive), and of
    * those the nearest to `d`. The JVM may print one digit more than needed
    * (`4.9E-324` for `5e-324`), since it always writes a fractional digit.
    */
  private def shortest(d: Double, digits: String, point: Int): (String, Int) =
    val exact                          = new java.math.BigDecimal(d)
    def distance(text: String, e: Int) =
      new java.math.BigDecimal(new java.math.BigInteger(text), -e).subtract(exact).abs
    val found = (1 until digits.length).iterator.map { k =>
      val prefix = BigInt(digits.take(k))
      Vector(prefix, prefix + 1)
        .map { p =>
          val text = p.toString
          val e    = point - k
          (text, e, java.lang.Double.parseDouble(s"${text}e$e"))
        }
        .filter(_._3 == d)
        .sortWith((a, b) => distance(a._1, a._2).compareTo(distance(b._1, b._2)) < 0)
        .headOption
        .map((text, e, _) => (trimmed(text), e + text.length))
    }
    found.collectFirst { case Some(result) => result }.getOrElse((digits, point))
