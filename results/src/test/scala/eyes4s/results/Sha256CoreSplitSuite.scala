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

package eyes4s.results

/** The hasher against independently computed (Python hashlib) digests,
  * with each message split at every point and, up to 129 bytes, every pair
  * of points.
  */
class Sha256CoreSplitSuite extends munit.FunSuite:
  private def hex(bytes: IArray[Byte]): String = bytes.map(b => f"${b & 0xff}%02x").mkString
  private val expected                         = Vector(
    55   -> "8aa994584139d128848eeebc4e815639ba5ab6e6e39574195a63ac4f14f7c43b",
    56   -> "ad574708f75c044c9b85de64cb568ee7711ff4f36448c6242f053ba8f6cc2b63",
    57   -> "5b46e502092be01b1100193e089fdda95638c12e19a1d24f308eb2c3d3ae849d",
    63   -> "280ed3e8ff1df845b2e7dfe6ac6cee817bef20e783cc65abc41b818b4d2fe076",
    64   -> "c6ab9724ade5b6a7a1edfffb12f3aa9181351355af8fd08c919952ad211339dd",
    65   -> "788367c73c7ddf4c53f65e68cc0d943e6227ab55b0e78ba63ace822b1c6301c0",
    111  -> "dd1413178fb627f9abbc041ffe39c44aa7aaa0e2e6d2ca5c4528ac7073a2da45",
    119  -> "3d610547d68216dedf7435a4fb6260353911f6b3fd3f18805ddb8be285d726fe",
    120  -> "1f80156a804cb7862ad113e8200e9d74499723e7c7854d5f48776d3148e09656",
    127  -> "192409cd280e14b743642ad1343fbd3e82d9305de72c078117745a679210cc3d",
    128  -> "cc548ca2dec1f6fe4f58b2e27aa9c7521607df1130d140b55a4dad0665302356",
    129  -> "81e89a7b2911aaa7795f9e3d4910cb47d6cd2b00d83b8399481527261a1a7519",
    1000 -> "5097e7d587352f5097062ae679f37bda5802d9f875aba14c8cb4d1a188ada179"
  )
  test("python hashlib vectors at every two-way and three-way split") {
    expected.foreach { (n, want) =>
      val bytes = IArray.tabulate[Byte](n)(i => (i * 31 + 7).toByte)
      assertEquals(hex(Sha256Core.digest(bytes)), want, s"one-shot $n")
      val h = new Sha256Core.Hasher
      (0 to n).foreach { a =>
        h.update(bytes.slice(0, a)); h.update(bytes.slice(a, n))
        assertEquals(hex(h.finish()), want, s"$n split $a")
      }
      if n <= 129 then
        for a <- 0 to n; b <- a to n do
          bytes.slice(0, a).foreach(h.update(_)); h.update(bytes.slice(a, b))
          bytes.slice(b, n).foreach(h.update(_))
          assertEquals(hex(h.finish()), want, s"$n split $a/$b")
    }
  }
