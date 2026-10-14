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

package eyes4s.compare

/** Exact univariate biased estimator, using sorted partial sums rather than pairs.
  * Chaudhuri and Hu, arXiv:1810.11332, equation 3. Equal x values are queried before
  * their batch is inserted; strict y prefixes consequently handle ties exactly.
  */
private[compare] object UnivariateDistanceCorrelation:
  // Double-double arithmetic protects the centred-moment subtraction on sparse maps.
  // Dekker splitting uses the same elementary operations on both supported runtimes.
  private final case class Wide(hi: Double, lo: Double = 0.0):
    def +(b: Wide): Wide =
      val s = hi + b.hi
      val v = s - hi
      val e = (hi - (s - v)) + (b.hi - v) + lo + b.lo
      val h = s + e
      Wide(h, e - (h - s))
    def unary_- : Wide   = Wide(-hi, -lo)
    def -(b: Wide): Wide = this + -b
    def *(b: Wide): Wide =
      val p  = hi * b.hi
      val ca = 134217729.0 * hi
      val cb = 134217729.0 * b.hi
      val ah = ca - (ca - hi)
      val bh = cb - (cb - b.hi)
      val al = hi - ah
      val bl = b.hi - bh
      val e  = ((ah * bh - p) + ah * bl + al * bh) + al * bl + hi * b.lo + lo * b.hi + lo * b.lo
      val h  = p + e
      Wide(h, e - (h - p))
    def /(b: Double): Wide =
      val q = hi / b
      Wide(q) + Wide(((this - Wide(q) * Wide(b)).value) / b)
    def value: Double = hi + lo

  private val Zero = Wide(0.0)

  private final class Accumulator:
    var hi            = 0.0
    var lo            = 0.0
    def reset(): Unit =
      hi = 0.0
      lo = 0.0
    def add(bHi: Double, bLo: Double): Unit =
      val s = hi + bHi
      val v = s - hi
      val e = (hi - (s - v)) + (bHi - v) + lo + bLo
      hi = s + e
      lo = e - (hi - s)
    def value: Wide = Wide(hi, lo)

  private final class WideBuffer(size: Int):
    val hi                                = new Array[Double](size)
    val lo                                = new Array[Double](size)
    def apply(i: Int): Wide               = Wide(hi(i), lo(i))
    def update(i: Int, value: Wide): Unit =
      hi(i) = value.hi
      lo(i) = value.lo
    def add(i: Int, value: Wide): Unit =
      val a = hi(i)
      val s = a + value.hi
      val v = s - a
      val e = (a - (s - v)) + (value.hi - v) + lo(i) + value.lo
      hi(i) = s + e
      lo(i) = e - (hi(i) - s)
    def sum: Wide =
      val accumulator = new Accumulator
      var i           = 0
      while i < size do
        accumulator.add(hi(i), lo(i))
        i += 1
      accumulator.value

  private def sum(values: Array[Wide]): Wide =
    var result = Zero
    var i      = 0
    while i < values.length do
      result = result + values(i)
      i += 1
    result

  private def scaled(values: IArray[Double]): Array[Double] =
    var low  = values(0)
    var high = low
    var i    = 1
    while i < values.length do
      low = math.min(low, values(i))
      high = math.max(high, values(i))
      i += 1
    val width = high - low
    Array.tabulate(values.length)(i => (values(i) - low) / width)

  private def order(values: Array[Double]): Array[Int] =
    val n           = values.length
    var source      = Array.tabulate(n)(identity)
    var destination = new Array[Int](n)
    var width       = 1
    while width < n do
      var start = 0
      while start < n do
        val middle = math.min(start.toLong + width, n.toLong).toInt
        val end    = math.min(start.toLong + 2L * width, n.toLong).toInt
        var left   = start
        var right  = middle
        var out    = start
        while out < end do
          if left < middle && (right == end || values(source(left)) <= values(source(right)))
          then
            destination(out) = source(left)
            left += 1
          else
            destination(out) = source(right)
            right += 1
          out += 1
        start = end
      val old = source
      source = destination
      destination = old
      width = if width > n / 2 then n else 2 * width
    source

  private def rows(values: Array[Double], indices: Array[Int]): WideBuffer =
    val n     = values.length
    var total = Zero
    var i     = 0
    while i < n do
      total = total + Wide(values(i))
      i += 1
    var prefix = Zero
    val result = new WideBuffer(n)
    i = 0
    while i < n do
      val index = indices(i)
      val value = Wide(values(index))
      result(index) = value * Wide(2.0 * i - n) - prefix * Wide(2.0) + total
      prefix = prefix + value
      i += 1
    result

  private def dot(a: WideBuffer, b: WideBuffer): Wide =
    val accumulator = new Accumulator
    var i           = 0
    while i < a.hi.length do
      val product = a(i) * b(i)
      accumulator.add(product.hi, product.lo)
      i += 1
    accumulator.value

  private def dot(a: Array[Wide], b: Array[Wide]): Wide =
    var result = Zero
    var i      = 0
    while i < a.length do
      result = result + a(i) * b(i)
      i += 1
    result

  /** Preconditions: equally sized nonconstant finite nonnegative vectors. */
  def correlation(left: IArray[Double], right: IArray[Double]): Double =
    val a = scaled(left)
    val b = scaled(right)
    // Canonical operand order makes a reversed call perform the identical reductions.
    var firstDifference = 0
    while firstDifference < a.length && a(firstDifference) == b(firstDifference) do
      firstDifference += 1
    val swap  = firstDifference < a.length && a(firstDifference) > b(firstDifference)
    val x     = if swap then b else a
    val y     = if swap then a else b
    val n     = x.length
    val ox    = order(x)
    val oy    = order(y)
    val ranks = new Array[Int](n)
    var rank  = 0
    var i     = 0
    while i < n do
      if i == 0 || y(oy(i)) != y(oy(i - 1)) then rank += 1
      ranks(oy(i)) = rank
      i += 1
    val counts     = new Array[Int](rank + 1)
    val sumsX      = new WideBuffer(rank + 1)
    val sumsY      = new WideBuffer(rank + 1)
    val sumsXY     = new WideBuffer(rank + 1)
    val sxPrefix   = new Accumulator
    val syPrefix   = new Accumulator
    val sxyPrefix  = new Accumulator
    var concordant = Zero
    var start      = 0
    while start < n do
      var end = start + 1
      while end < n && x(ox(end)) == x(ox(start)) do end += 1
      i = start
      while i < end do
        val index = ox(i)
        val xi    = Wide(x(index))
        val yi    = Wide(y(index))
        var count = 0
        sxPrefix.reset()
        syPrefix.reset()
        sxyPrefix.reset()
        var k = ranks(index) - 1
        while k > 0 do
          count += counts(k)
          sxPrefix.add(sumsX.hi(k), sumsX.lo(k))
          syPrefix.add(sumsY.hi(k), sumsY.lo(k))
          sxyPrefix.add(sumsXY.hi(k), sumsXY.lo(k))
          k -= k & -k
        concordant = concordant + xi * yi * Wide(
          count.toDouble
        ) - xi * syPrefix.value - yi * sxPrefix.value + sxyPrefix.value
        i += 1
      i = start
      while i < end do
        val index   = ox(i)
        val xi      = Wide(x(index))
        val yi      = Wide(y(index))
        val product = xi * yi
        var k       = ranks(index)
        while k > 0 && k <= rank do
          counts(k) += 1
          sumsX.add(k, xi)
          sumsY.add(k, yi)
          sumsXY.add(k, product)
          k += k & -k
        i += 1
      start = end
    val wx    = x.map(Wide(_))
    val wy    = y.map(Wide(_))
    val sx    = sum(wx)
    val sy    = sum(wy)
    val dn    = Wide(n.toDouble)
    val rawXX = Wide(2.0) * (dn * dot(wx, wx) - sx * sx)
    val rawYY = Wide(2.0) * (dn * dot(wy, wy) - sy * sy)
    val rawXY = Wide(4.0) * concordant - Wide(2.0) * (dn * dot(wx, wy) - sx * sy)
    val rx    = rows(x, ox)
    val ry    = rows(y, oy)
    val gx    = rx.sum
    val gy    = ry.sum
    def centred(raw: Wide, rowsDot: Wide, grandProduct: Wide): Double =
      (raw - (Wide(2.0) * rowsDot) / n + (grandProduct / n) / n).value
    val xx = centred(rawXX, dot(rx, rx), gx * gx)
    val yy = centred(rawYY, dot(ry, ry), gy * gy)
    val xy = centred(rawXY, dot(rx, ry), gx * gy)
    math.sqrt(math.max(0.0, math.min(1.0, xy / math.sqrt(xx) / math.sqrt(yy))))
