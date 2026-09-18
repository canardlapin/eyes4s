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

package eyes4s.codec

/** SHA-256 against independently known vectors (FIPS 180-2 examples and
  * `shasum -a 256` at every padding boundary), and strict UTF-8, on the JVM
  * and Scala.js.
  */
class ByteDigestSuite extends munit.FunSuite:
  private def ascii(s: String): IArray[Byte] = IArray.from(s.map(_.toByte))
  private def digest(s: String): String      = ByteDigest.sha256(ascii(s)).hex

  test("FIPS 180-2 example messages digest to their published values") {
    assertEquals(digest(""), "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
    assertEquals(
      digest("abc"),
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    )
    assertEquals(
      digest("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"),
      "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
    )
    assertEquals(
      ByteDigest.sha256(IArray.fill(1000000)('a'.toByte)).hex,
      "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"
    )
  }

  test("messages at every padding boundary match shasum") {
    val expected = Vector(
      55  -> "9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318",
      56  -> "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a",
      63  -> "7d3e74a05d7db15bce4ad9ec0658ea98e3f06eeecf16b4c6fff2da457ddc2f34",
      64  -> "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb",
      65  -> "635361c48bb9eab14198e76ea8ab7f1a41685d6ad62aa9146d301d4f17eb0ae0",
      119 -> "31eba51c313a5c08226adf18d4a359cfdfd8d2e816b13f4af952f7ea6584dcfb",
      120 -> "2f3d335432c70b580af0e8e1b3674a7c020d683aa5f73aaaedfdc55af904c21c"
    )
    expected.foreach { case (n, hex) => assertEquals(digest("a" * n), hex, s"length $n") }
  }

  test("a digest has one canonical spelling and parses back to itself") {
    val value = ByteDigest.sha256(ascii("abc"))
    assertEquals(ByteDigest.parse(value.hex), Right(value))
    assertEquals(value.bytes.length, 32)
    assertEquals(
      ByteDigest.parse(value.hex.toUpperCase),
      Left(ByteDigestError.InvalidCharacter(value.hex.toUpperCase, 0, 'B'))
    )
    assertEquals(ByteDigest.parse("abc"), Left(ByteDigestError.WrongLength("abc", 3)))
  }

  test("strict UTF-8 refuses lone surrogates, overlong forms and out-of-range code points") {
    val text = "aé€😀"
    val utf8 = Utf8.encode(text).fold(i => fail(s"offset $i"), identity)
    assertEquals(
      utf8.toVector.map(_ & 0xff),
      Vector(0x61, 0xc3, 0xa9, 0xe2, 0x82, 0xac, 0xf0, 0x9f, 0x98, 0x80)
    )
    assertEquals(Utf8.decode(utf8), Right(text))
    assertEquals(Utf8.encode("x\ud800y"), Left(1))
    assertEquals(Utf8.encode("\udc00"), Left(0))
    def bytes(values: Int*): IArray[Byte] = IArray.from(values.map(_.toByte))
    assertEquals(Utf8.decode(bytes(0x61, 0xc0, 0xaf)), Left(1))
    assertEquals(Utf8.decode(bytes(0xe0, 0x80, 0xaf)), Left(0))
    assertEquals(Utf8.decode(bytes(0xed, 0xa0, 0x80)), Left(0))
    assertEquals(Utf8.decode(bytes(0xf4, 0x90, 0x80, 0x80)), Left(0))
    assertEquals(Utf8.decode(bytes(0x61, 0x62, 0xe2, 0x82)), Left(2))
    assertEquals(Utf8.decode(bytes(0x80)), Left(0))
  }
