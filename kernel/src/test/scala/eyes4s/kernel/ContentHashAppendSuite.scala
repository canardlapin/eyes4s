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

package eyes4s.kernel

class ContentHashAppendSuite extends munit.FunSuite:
  // Pinned before extraction with an independent byte-at-a-time FNV-1a
  // reference: each UTF-16 unit contributes its eight little-endian bytes.
  // Expected values do not come from either production hashing route.
  private val vectors = Vector(
    ""             -> "cbf29ce484222325",
    "a"            -> "6926124a7b1433c4",
    "Eyes4s"       -> "49a2c824372ede68",
    "\u0000"       -> "a8c7f832281a39c5",
    "A\u0000B"     -> "8458b159f758dce6",
    "\u03b1\u6f22" -> "684602c6d7f1b98e",
    "\ud83d\ude42" -> "58786a97d4bd9778",
    "\ud800"       -> "2b2fbb7d1d6050fd",
    "\udfff"       -> "34ad06ca4db9d26b"
  )
  private def append(hash: ContentHash, text: String): ContentHash =
    text.foldLeft(hash)(ContentHash.appendCodeUnit)

  test("whole and incremental text hashing retain independent pre-change digest vectors") {
    vectors.foreach { (text, expected) =>
      assertEquals(ContentHash.ofString(text).render, expected)
      assertEquals(append(ContentHash.empty, text).render, expected)
    }
  }

  test("every three-part partition resumes the exact hash, including split surrogates") {
    vectors.foreach { (text, expected) =>
      for first <- 0 to text.length; second <- first to text.length do
        val a = append(ContentHash.empty, text.substring(0, first))
        val b = append(a, text.substring(first, second))
        assertEquals(append(b, text.substring(second)).render, expected)
    }
  }
