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

/** The incremental hasher is SHA-256: the FIPS 180-4 example vectors, given
  * whole, byte by byte and in uneven runs, and the one-shot digest of every
  * length that crosses the padding boundaries.
  */
class Sha256CoreSuite extends munit.FunSuite:
  private def hex(bytes: IArray[Byte]): String = bytes.map(b => f"${b & 0xff}%02x").mkString
  private def ascii(s: String): IArray[Byte]   = IArray.from(s.getBytes("US-ASCII"))

  private def hashed(input: IArray[Byte], run: Int): IArray[Byte] =
    val hasher = new Sha256Core.Hasher
    input
      .grouped(run)
      .foreach(chunk => if run == 1 then hasher.update(chunk(0)) else hasher.update(chunk))
    hasher.finish()

  private val vectors = Vector(
    ""    -> "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
    "abc" -> "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad",
    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" ->
      "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1",
    ("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnop" +
      "jklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu") ->
      "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1"
  )

  test("FIPS 180-4 example vectors, whole, byte by byte and in runs of 7") {
    vectors.foreach { (message, expected) =>
      val bytes = ascii(message)
      assertEquals(hex(Sha256Core.digest(bytes)), expected, message)
      Vector(1, 7, 64, 1000).foreach(run =>
        assertEquals(hex(hashed(bytes, run)), expected, run)
      )
    }
  }

  test("one million 'a', fed in runs, never held") {
    val hasher = new Sha256Core.Hasher
    val run    = IArray.fill[Byte](1000)('a'.toByte)
    (1 to 1000).foreach(_ => hasher.update(run))
    assertEquals(
      hex(hasher.finish()),
      "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"
    )
  }

  test("every length across the padding boundaries agrees with the one-shot digest") {
    (0 to 200).foreach { n =>
      val bytes = IArray.tabulate[Byte](n)(i => (i * 31 + 7).toByte)
      assertEquals(hex(hashed(bytes, 1)), hex(Sha256Core.digest(bytes)), n)
      assertEquals(hex(hashed(bytes, 13)), hex(Sha256Core.digest(bytes)), n)
    }
  }

  test("finish resets the hasher, so it digests the next message from the start") {
    val hasher = new Sha256Core.Hasher
    hasher.update(ascii("abc"))
    assertEquals(hex(hasher.finish()), vectors(1)._2)
    assertEquals(hex(hasher.finish()), vectors(0)._2)
    hasher.update(ascii("abc"))
    assertEquals(hex(hasher.finish()), vectors(1)._2)
  }
