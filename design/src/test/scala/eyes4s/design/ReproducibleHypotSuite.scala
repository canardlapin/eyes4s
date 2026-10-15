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

package eyes4s.design

class ReproducibleHypotSuite extends munit.FunSuite:
  private def word(value: String): Long = java.lang.Long.parseUnsignedLong(value, 16)
  private val cases                     = HypotReference.records.linesIterator
    .filter(_.nonEmpty)
    .map { line =>
      val fields = line.split(" ")
      (word(fields(0)), word(fields(1)), word(fields(2)))
    }
    .toVector
  test("1024 independent binary cases preserve fdlibm hypot on this runtime") {
    assertEquals(cases.size, 1024)
    cases.zipWithIndex.foreach { case ((x, y, expected), index) =>
      val actual = ReproducibleHypot(
        java.lang.Double.longBitsToDouble(x),
        java.lang.Double.longBitsToDouble(y)
      )
      assertEquals(java.lang.Double.doubleToLongBits(actual), expected, s"binary case=$index")
    }
  }
