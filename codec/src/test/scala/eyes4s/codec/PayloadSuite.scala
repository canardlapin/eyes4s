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

import io.circe.Json
import org.scalacheck.{Gen, Prop}

/** The pinned little-endian payload layout: exact bytes, exact values on both
  * platforms, and typed refusals.
  */
class PayloadSuite extends munit.ScalaCheckSuite:
  private def get[E, A](e: Either[E, A]): A    = e.fold(x => fail(s"$x"), identity)
  private def hex(bytes: IArray[Byte]): String =
    bytes.map(b => f"${b & 0xff}%02x").mkString

  test("int64, int32 and float64 payloads have pinned little-endian bytes and digests") {
    val longs = get(
      PackedArrays.pack(
        get(PayloadLayout.column(ElementKind.Int64, 2)),
        IArray(0x0102030405060708L, -1L)
      )
    )
    assertEquals(hex(longs.bytes), "0807060504030201ffffffffffffffff")
    assertEquals(
      longs.ref.sha256.hex,
      "f4efafa66b241b95ac95d0c2c936a687b8d3df6e89799649553086e4e79a7438"
    )
    val doubles = get(
      PackedArrays.pack(get(PayloadLayout.column(ElementKind.Float64, 2)), IArray(1.0, -0.0))
    )
    assertEquals(hex(doubles.bytes), "000000000000f03f0000000000000080")
    assertEquals(
      doubles.ref.sha256.hex,
      "5e9d905ef08923718da5998eb8ae14dc75a714656224d3753b679523d5a268d9"
    )
    val ints = get(
      PackedArrays.pack(get(PayloadLayout.column(ElementKind.Int32, 2)), IArray(0x01020304, -2))
    )
    assertEquals(hex(ints.bytes), "04030201feffffff")
    assertEquals(
      ints.ref.sha256.hex,
      "a800b1d9e887974b7c040f97f60db9fdc6b721b8d83ac53619c317b76135b09e"
    )
  }

  test("64-bit integers and doubles decode without precision loss on either platform") {
    val longs = IArray(9007199254740993L, Long.MinValue, Long.MaxValue, -9007199254740993L, 0L)
    val packedLongs =
      get(PackedArrays.pack(get(PayloadLayout.column(ElementKind.Int64, longs.length)), longs))
    assertEquals(get(PackedArrays.unpack[Long](packedLongs)).toVector, longs.toVector)
    val doubles = IArray(
      0.1,
      -0.0,
      Double.MinPositiveValue,
      Double.MaxValue,
      Double.PositiveInfinity,
      Double.NegativeInfinity,
      1.0 / 3.0
    )
    val packed =
      get(
        PackedArrays.pack(
          get(PayloadLayout.column(ElementKind.Float64, doubles.length)),
          doubles
        )
      )
    assertEquals(
      get(PackedArrays.unpack[Double](packed)).toVector.map(java.lang.Double.doubleToLongBits),
      doubles.toVector.map(java.lang.Double.doubleToLongBits)
    )
  }

  property("every non-NaN double and every long round-trips through its bits") {
    val doubles = Gen.oneOf(
      Gen.choose(Long.MinValue, Long.MaxValue).map(java.lang.Double.longBitsToDouble),
      Gen.oneOf(0.0, -0.0, Double.MinPositiveValue, Double.PositiveInfinity)
    )
    Prop.forAll(Gen.listOf(doubles), Gen.listOf(Gen.choose(Long.MinValue, Long.MaxValue))) {
      (ds, ls) =>
        val finiteOrInfinite = IArray.from(ds.filterNot(_.isNaN))
        val d                = get(
          PackedArrays.pack(
            get(PayloadLayout.column(ElementKind.Float64, finiteOrInfinite.length)),
            finiteOrInfinite
          )
        )
        val l = get(
          PackedArrays.pack(
            get(PayloadLayout.column(ElementKind.Int64, ls.size)),
            IArray.from(ls)
          )
        )
        get(PackedArrays.unpack[Double](d)).toVector.map(java.lang.Double.doubleToLongBits) ==
          finiteOrInfinite.toVector.map(java.lang.Double.doubleToLongBits) &&
          get(PackedArrays.unpack[Long](l)).toVector == ls.toVector
    }
  }

  test("NaN is refused on packing and on unpacking, naming the element") {
    val layout = get(PayloadLayout.column(ElementKind.Float64, 2))
    assertEquals(
      PackedArrays.pack(layout, IArray(1.0, Double.NaN)),
      Left(PayloadError.NotANumber(1))
    )
    val bytes = IArray.from(Array.fill[Byte](8)(0) ++ Array[Byte](0, 0, 0, 0, 0, 0, -8, 127))
    val ref   = PayloadRef(ByteDigest.sha256(bytes), layout)
    val nan   = get(VerifiedPayload.verify(ref, bytes))
    assertEquals(PackedArrays.unpack[Double](nan), Left(PayloadError.NotANumber(1)))
  }

  test(
    "layouts refuse empty shapes, negative extents, oversized arrays and mismatched values"
  ) {
    assertEquals(
      PayloadLayout.of(ElementKind.Int64, Vector.empty, ArrayOrder.RowMajor),
      Left(PayloadError.EmptyShape)
    )
    assertEquals(
      PayloadLayout.of(ElementKind.Int64, Vector(3, -1), ArrayOrder.RowMajor),
      Left(PayloadError.NegativeExtent(1, -1))
    )
    assertEquals(
      PayloadLayout
        .of(ElementKind.Float64, Vector(1 << 20, 1 << 10, 3), ArrayOrder.ColumnMajor),
      Left(
        PayloadError.TooLarge(
          ElementKind.Float64,
          Vector(1 << 20, 1 << 10, 3),
          Int.MaxValue - 8
        )
      )
    )
    // The inline recording bound is not a payload bound.
    val columns = get(
      PayloadLayout.of(
        ElementKind.Float64,
        Vector(RecordingInputCodecs.maximumSamples + 1, 3),
        ArrayOrder.ColumnMajor
      )
    )
    assertEquals(columns.byteLength, (RecordingInputCodecs.maximumSamples + 1) * 24)
    val two = get(PayloadLayout.column(ElementKind.Int64, 2))
    assertEquals(PackedArrays.pack(two, IArray(1L)), Left(PayloadError.Count(two, 1)))
    assertEquals(
      PackedArrays.pack(two, IArray(1, 2)),
      Left(PayloadError.Element(ElementKind.Int64, ElementKind.Int32))
    )
    val longs = get(PackedArrays.pack(two, IArray(1L, 2L)))
    assertEquals(
      PackedArrays.unpack[Double](longs),
      Left(PayloadError.Element(ElementKind.Int64, ElementKind.Float64))
    )
  }

  test("verification checks length and digest and keeps a private copy of the bytes") {
    val layout   = get(PayloadLayout.column(ElementKind.UInt8, 4))
    val source   = Array[Byte](1, 2, 3, 4)
    val ref      = PayloadRef(ByteDigest.sha256(IArray.from(source)), layout)
    val caller   = IArray.unsafeFromArray(source)
    val admitted = get(VerifiedPayload.verify(ref, caller))
    source(0) = 9
    assertEquals(admitted.bytes.toVector, Vector[Byte](1, 2, 3, 4))
    assertEquals(get(PackedArrays.unpack[Byte](admitted)).toVector, Vector[Byte](1, 2, 3, 4))
    assertEquals(
      VerifiedPayload.verify(ref, IArray[Byte](1, 2, 3)),
      Left(PayloadError.Length(4, 3))
    )
    val changed = IArray[Byte](1, 2, 3, 5)
    assertEquals(
      VerifiedPayload.verify(ref, changed),
      Left(PayloadError.Digest(ref.sha256, ByteDigest.sha256(changed)))
    )
  }

  test("a payload reference round-trips through JSON and refuses a big-endian declaration") {
    val ref =
      get(
        PackedArrays.pack(
          get(PayloadLayout.of(ElementKind.Float64, Vector(2, 3), ArrayOrder.ColumnMajor)),
          IArray(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
        )
      ).ref
    val json = PayloadRef.json(ref)
    assertEquals(
      json.hcursor.downField("layout").focus,
      Some(
        Json.obj(
          "element"   -> Json.fromString("float64"),
          "shape"     -> Json.arr(Json.fromInt(2), Json.fromInt(3)),
          "order"     -> Json.fromString("column-major"),
          "byteOrder" -> Json.fromString("little-endian")
        )
      )
    )
    assertEquals(PayloadRef.read(json), Right(ref))
    val bigEndian = json.hcursor
      .downField("layout")
      .downField("byteOrder")
      .withFocus(_ => Json.fromString("big-endian"))
      .top
      .get
    assert(PayloadRef.read(bigEndian).left.exists {
      case CodecError.Field("byteOrder", _, reason) => reason.contains("big-endian")
      case _                                        => false
    })
  }
