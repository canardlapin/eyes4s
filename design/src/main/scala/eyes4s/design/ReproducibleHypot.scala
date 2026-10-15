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

/* Adapted from fdlibm e_hypot.c 1.3 (1995-01-18):
 * https://www.netlib.org/fdlibm/e_hypot.c
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 * Developed at SunSoft, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

package eyes4s.design

/** Shared fdlibm operation order, with no FMA or runtime hypot delegation. */
private[design] object ReproducibleHypot:
  private def high(value: Double): Int =
    (java.lang.Double.doubleToRawLongBits(value) >>> 32).toInt
  private def low(value: Double): Int = java.lang.Double.doubleToRawLongBits(value).toInt
  private def withHigh(value: Double, word: Int): Double = java.lang.Double.longBitsToDouble(
    (word.toLong << 32) | (java.lang.Double.doubleToRawLongBits(value) & 0xffffffffL)
  )

  def apply(x: Double, y: Double): Double =
    var a  = x; var b = y
    var ha = high(x) & 0x7fffffff
    var hb = high(y) & 0x7fffffff
    if hb > ha then
      a = y; b = x
      val previous = ha; ha = hb; hb = previous
    a = withHigh(a, ha); b = withHigh(b, hb)
    if ha - hb > 0x03c00000 then return a + b
    var scale = 0
    if ha > 0x5f300000 then
      if ha >= 0x7ff00000 then
        var result = a + b
        if ((ha & 0x000fffff) | low(a)) == 0 then result = a
        if ((hb ^ 0x7ff00000) | low(b)) == 0 then result = b
        return result
      ha -= 0x25800000; hb -= 0x25800000; scale += 600
      a = withHigh(a, ha); b = withHigh(b, hb)
    if hb < 0x20b00000 then
      if hb <= 0x000fffff then
        if (hb | low(b)) == 0 then return a
        val factor = withHigh(0.0, 0x7fd00000)
        b *= factor; a *= factor; scale -= 1022
      else
        ha += 0x25800000; hb += 0x25800000; scale -= 600
        a = withHigh(a, ha); b = withHigh(b, hb)
    val difference = a - b
    val result     = if difference > b then
      val first  = withHigh(0.0, ha)
      val second = a - first
      math.sqrt(first * first - (b * (-b) - second * (a + first)))
    else
      a = a + a
      val firstY  = withHigh(0.0, hb)
      val secondY = b - firstY
      val first   = withHigh(0.0, ha + 0x00100000)
      val second  = a - first
      math.sqrt(first * firstY - (difference * (-difference) - (first * secondY + second * b)))
    if scale == 0 then result else withHigh(1.0, 0x3ff00000 + (scale << 20)) * result
