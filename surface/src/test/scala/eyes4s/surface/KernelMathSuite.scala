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

package eyes4s.surface

class KernelMathSuite extends munit.FunSuite:
  private val Seed                                  = 0xcbf29ce484222325L
  private def bits(value: Double): Long             = java.lang.Double.doubleToLongBits(value)
  private def fold(hash: Long, value: Double): Long = (hash ^ bits(value)) * 1099511628211L

  test("exponential boundary witnesses retain their exact StrictMath bits") {
    KernelMathReference.exponential.foreach { (argument, expected) =>
      assertEquals(
        bits(KernelMath.fdlibmExp(java.lang.Double.longBitsToDouble(argument))),
        expected,
        clue(argument)
      )
    }
  }

  test("positive-count power witnesses retain exact bits at interval and exponent boundaries") {
    KernelMathReference.power.foreach { (count, expected) =>
      assertEquals(bits(KernelMath.fdlibmInverseSixthPower(count)), expected, clue(count))
    }
  }

  test("634949 kernel taps match the independently generated StrictMath digest") {
    var hash              = Seed
    var taps              = 0
    var nativeDifferences = 0
    var index             = 0
    while index <= 6400 do
      val sigma  = 0.2 + index * 0.005
      val radius = math.ceil(3 * sigma).toInt
      var tap    = -radius
      while tap <= radius do
        val argument = -0.5 * tap.toDouble * tap / (sigma * sigma)
        val actual   = KernelMath.fdlibmExp(argument)
        hash = fold(hash, actual)
        if bits(actual) != bits(math.exp(argument)) then nativeDifferences += 1
        taps += 1
        tap += 1
      index += 1
    assertEquals(taps, KernelMathReference.kernelTapCount)
    assertEquals(hash, KernelMathReference.kernelTapHash)
    println(s"KERNEL_EXP_NATIVE_DIFFERENCES=$nativeDifferences")
  }

  test(
    "observation counts one through100000 match the independent fixed-exponent power digest"
  ) {
    var hash              = Seed
    var nativeDifferences = 0
    var count             = 1
    while count <= 100000 do
      val actual = KernelMath.fdlibmInverseSixthPower(count)
      hash = fold(hash, actual)
      if bits(actual) != bits(math.pow(count.toDouble, -1.0 / 6.0)) then nativeDifferences += 1
      count += 1
    assertEquals(hash, KernelMathReference.observationCountHash)
    println(s"BANDWIDTH_POWER_NATIVE_DIFFERENCES=$nativeDifferences")
  }
