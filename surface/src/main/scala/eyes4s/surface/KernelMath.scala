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

/* The fdlibm 5.3 exp and fixed-domain pow algorithms are adapted from
 * https://www.netlib.org/fdlibm/e_exp.c and e_pow.c. Their notice is preserved:
 * ====================================================
 * Copyright (C) 2004 by Sun Microsystems, Inc. All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

package eyes4s.surface

/** Shared elementary operations with fdlibm 5.3 arithmetic order. */
private[surface] object KernelMath:
  private val Ln2Hi        = 6.93147180369123816490e-01
  private val Ln2Lo        = 1.90821492927058770002e-10
  private val InvLn2       = 1.44269504088896338700e+00
  private val P1           = 1.66666666666666019037e-01
  private val P2           = -2.77777777770155933842e-03
  private val P3           = 6.61375632143793436117e-05
  private val P4           = -1.65339022054652515390e-06
  private val P5           = 4.13813679705723846039e-08
  private val TwoMinus1000 = 9.33263618503218878990e-302

  private def high(value: Double): Int =
    (java.lang.Double.doubleToRawLongBits(value) >>> 32).toInt
  private def withHigh(value: Double, word: Int): Double =
    java.lang.Double.longBitsToDouble(
      (word.toLong << 32) | (java.lang.Double.doubleToRawLongBits(value) & 0xffffffffL)
    )
  private def withoutLow(value: Double): Double =
    java.lang.Double.longBitsToDouble(
      java.lang.Double.doubleToRawLongBits(value) & 0xffffffff00000000L
    )

  def fdlibmExp(argument: Double): Double =
    val word      = high(argument)
    val magnitude = word & 0x7fffffff
    val negative  = word < 0
    if argument.isNaN then return argument + argument
    if argument == Double.PositiveInfinity then return argument
    if argument == Double.NegativeInfinity then return 0.0
    if argument > 7.09782712893383973096e+02 then return Double.PositiveInfinity
    if argument < -7.45133219101941108420e+02 then return 0.0
    var reduced = argument
    var hi      = 0.0
    var lo      = 0.0
    var k       = 0
    if magnitude > 0x3fd62e42 then
      if magnitude < 0x3ff0a2b2 then
        hi = argument - (if negative then -Ln2Hi else Ln2Hi)
        lo = if negative then -Ln2Lo else Ln2Lo
        k = if negative then -1 else 1
      else
        k = (InvLn2 * argument + (if negative then -0.5 else 0.5)).toInt
        hi = argument - k * Ln2Hi
        lo = k * Ln2Lo
      reduced = hi - lo
    else if magnitude < 0x3e300000 then return 1.0 + argument
    val t = reduced * reduced
    val c = reduced - t * (P1 + t * (P2 + t * (P3 + t * (P4 + t * P5))))
    if k == 0 then 1.0 - ((reduced * c) / (c - 2.0) - reduced)
    else
      val y = 1.0 - ((lo - (reduced * c) / (2.0 - c)) - hi)
      if k >= -1021 then withHigh(y, high(y) + (k << 20))
      else withHigh(y, high(y) + ((k + 1000) << 20)) * TwoMinus1000

  /** fdlibm pow(count, -1.0/6.0), for positive Int-sized observation counts.
    * Public bandwidth callers reject counts below two before reaching this seam.
    */
  def fdlibmInverseSixthPower(count: Int): Double =
    if count == 1 then return 1.0
    val x            = count.toDouble
    val y            = -1.0 / 6.0
    val originalWord = high(x)
    var exponent     = (originalWord >>> 20) - 0x3ff
    val fraction     = originalWord & 0x000fffff
    var word         = fraction | 0x3ff00000
    val interval     =
      if fraction <= 0x3988e then 0
      else if fraction < 0xbb67a then 1
      else
        exponent += 1
        word -= 0x00100000
        0
    val base          = if interval == 0 then 1.0 else 1.5
    val dpHi          = if interval == 0 then 0.0 else 5.84962487220764160156e-01
    val dpLo          = if interval == 0 then 0.0 else 1.35003920212974897128e-08
    val normalized    = withHigh(x, word)
    val u             = normalized - base
    val v             = 1.0 / (normalized + base)
    val ss            = u * v
    val sHi           = withoutLow(ss)
    val denominatorHi =
      withHigh(0.0, ((word >>> 1) | 0x20000000) + 0x00080000 + (interval << 18))
    val denominatorLo = normalized - (denominatorHi - base)
    val sLo           = v * ((u - sHi * denominatorHi) - sHi * denominatorLo)
    val s2            = ss * ss
    val l1            = 5.99999999999994648725e-01
    val l2            = 4.28571428578550184252e-01
    val l3            = 3.33333329818377432918e-01
    val l4            = 2.72728123808534006489e-01
    val l5            = 2.30660745775561754067e-01
    val l6            = 2.06975017800338417784e-01
    val r             =
      s2 * s2 * (l1 + s2 * (l2 + s2 * (l3 + s2 * (l4 + s2 * (l5 + s2 * l6))))) + sLo * (sHi + ss)
    val squareHi  = sHi * sHi
    val tHi       = withoutLow(3.0 + squareHi + r)
    val tLo       = r - ((tHi - 3.0) - squareHi)
    val productHi = withoutLow(sHi * tHi + (sLo * tHi + tLo * ss))
    val productLo = (sLo * tHi + tLo * ss) - (productHi - sHi * tHi)
    val zHi       = 9.61796700954437255859e-01 * productHi
    val zLo       =
      -7.02846165095275826516e-09 * productHi + productLo * 9.61796693925975554329e-01 + dpLo
    val logHi     = withoutLow(((zHi + zLo) + dpHi) + exponent)
    val logLo     = zLo - (((logHi - exponent) - dpHi) - zHi)
    val yHi       = withoutLow(y)
    var powerHi   = yHi * logHi
    val powerLo   = (y - yHi) * logHi + y * logLo
    val power     = powerLo + powerHi
    val powerWord = high(power)
    val absWord   = powerWord & 0x7fffffff
    var scale     = 0
    if absWord > 0x3fe00000 then
      val powerExponent   = (absWord >>> 20) - 0x3ff
      val roundedWord     = powerWord + (0x00100000 >>> (powerExponent + 1))
      val roundedExponent = ((roundedWord & 0x7fffffff) >>> 20) - 0x3ff
      val integral        = withHigh(0.0, roundedWord & ~(0x000fffff >>> roundedExponent))
      scale = ((roundedWord & 0x000fffff) | 0x00100000) >>> (20 - roundedExponent)
      if powerWord < 0 then scale = -scale
      powerHi -= integral
    val t     = withoutLow(powerLo + powerHi)
    val expHi = t * 6.93147182464599609375e-01
    val expLo =
      (powerLo - (t - powerHi)) * 6.93147180559945286227e-01 + t * -1.90465429995776804525e-09
    val z          = expHi + expLo
    val w          = expLo - (z - expHi)
    val square     = z * z
    val polynomial =
      z - square * (P1 + square * (P2 + square * (P3 + square * (P4 + square * P5))))
    val correction = (z * polynomial) / (polynomial - 2.0) - (w + z * w)
    val result     = 1.0 - (correction - z)
    withHigh(result, high(result) + (scale << 20))
