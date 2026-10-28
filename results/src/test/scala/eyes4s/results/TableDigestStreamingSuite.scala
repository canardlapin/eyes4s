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

class TableDigestStreamingSuite extends munit.FunSuite:
  private def hex(bytes: IArray[Byte]): String =
    bytes.iterator.map(b => f"${b & 0xff}%02x").mkString

  test("streamed table UTF-8 preserves boundaries and historical surrogate bytes") {
    val cases = Vector(
      ""                         -> Vector.empty[Int],
      "\u0000\u007f"             -> Vector(0, 127),
      "\u0080\u07ff"             -> Vector(0xc2, 0x80, 0xdf, 0xbf),
      "\u0800\ud7ff\ue000\uffff" -> Vector(0xe0, 0xa0, 0x80, 0xed, 0x9f, 0xbf, 0xee, 0x80, 0x80,
        0xef, 0xbf, 0xbf),
      "\ud800\udc00"               -> Vector(0xf0, 0x90, 0x80, 0x80),
      "\udbff\udfff"               -> Vector(0xf4, 0x8f, 0xbf, 0xbf),
      "\ud800A"                    -> Vector(0xef, 0xbf, 0xbd, 0x41),
      "\udc00"                     -> Vector(0xef, 0xbf, 0xbd),
      "\ud800"                     -> Vector(0xed, 0xa0, 0x80),
      "\ud800\ud800"               -> Vector(0xef, 0xbf, 0xbd, 0xed, 0xa0, 0x80),
      "A\u00b0\u20ac\ud83d\ude00Z" -> Vector(0x41, 0xc2, 0xb0, 0xe2, 0x82, 0xac, 0xf0, 0x9f,
        0x98, 0x80, 0x5a)
    )
    cases.foreach { (text, expected) =>
      val emitted = Vector.newBuilder[Byte]
      Utf8Lenient.foreach(text)(b => { emitted += b; () })
      assertEquals(emitted.result().map(_ & 0xff), expected)
      val bytes = IArray.from(expected.map(_.toByte))
      assertEquals(TableDigest.ofUtf8(text).hex, hex(Sha256Core.digest(bytes)))
    }
  }

  test("table digest retains SHA-256 vectors and block-boundary identities") {
    assertEquals(
      TableDigest.ofUtf8("").hex,
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    )
    assertEquals(
      TableDigest.ofUtf8("abc").hex,
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    )
    Vector(1, 55, 56, 63, 64, 65, 127, 128, 129, 100000).foreach { size =>
      val text  = "A" * size
      val bytes = IArray.unsafeFromArray(Array.fill[Byte](size)(65.toByte))
      assertEquals(TableDigest.ofUtf8(text).hex, hex(Sha256Core.digest(bytes)))
    }
  }
