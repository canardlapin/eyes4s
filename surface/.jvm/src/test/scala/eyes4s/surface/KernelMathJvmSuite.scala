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

class KernelMathJvmSuite extends munit.FunSuite:
  private def bits(value: Double): Long = java.lang.Double.doubleToLongBits(value)

  test("shared exponential agrees exactly with StrictMath on kernel taps and extremes") {
    var index = 0
    while index <= 6400 do
      val sigma  = 0.2 + index * 0.005
      val radius = math.ceil(3 * sigma).toInt
      var tap    = -radius
      while tap <= radius do
        val argument = -0.5 * tap.toDouble * tap / (sigma * sigma)
        assertEquals(
          bits(KernelMath.fdlibmExp(argument)),
          bits(java.lang.StrictMath.exp(argument)),
          clue((sigma, tap))
        )
        tap += 1
      index += 1
    Vector(
      -746.0,
      -745.1332191019411,
      -744.0,
      -710.0,
      -708.0,
      -1.0,
      -0.0,
      0.0,
      1.0,
      708.0,
      709.782712893384,
      710.0,
      Double.NegativeInfinity,
      Double.PositiveInfinity,
      Double.NaN
    ).foreach { argument =>
      assertEquals(
        bits(KernelMath.fdlibmExp(argument)),
        bits(java.lang.StrictMath.exp(argument)),
        clue(argument)
      )
    }
    println(s"KERNEL_MATH_JVM_ARCH=${System.getProperty("os.arch")}")
  }

  test("shared fixed-exponent power agrees exactly with StrictMath for observation counts") {
    var count = 1
    while count <= 100000 do
      assertEquals(
        bits(KernelMath.fdlibmInverseSixthPower(count)),
        bits(java.lang.StrictMath.pow(count.toDouble, -1.0 / 6.0)),
        clue(count)
      )
      count += 1
    val edges = (1 to 30).flatMap { power =>
      val value = 1 << power
      Vector(value - 1, value, value + 1)
    } ++ Vector(Int.MaxValue - 1, Int.MaxValue)
    edges.foreach { n =>
      assertEquals(
        bits(KernelMath.fdlibmInverseSixthPower(n)),
        bits(java.lang.StrictMath.pow(n.toDouble, -1.0 / 6.0)),
        clue(n)
      )
    }
  }
