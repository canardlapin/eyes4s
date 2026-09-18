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

import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import io.circe.Json

/** The packed recording: S3's escape hatch for sample columns, with pinned
  * payload bytes and exact reconstruction on the JVM and Scala.js.
  */
class PackedRecordingSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)
  private val codec                         = PackedRecordingCodecs.recording[Px]
  private val monocular                     = InputPayloadFixtures.monocular

  private def lookup(packed: PackedRecording): PayloadRef => Option[VerifiedPayload] =
    ref => packed.payloads.find(_.ref == ref)

  private def samplesOf(document: Json): Json =
    get(
      document.hcursor.downField("value").downField("recording").downField("samples").as[Json]
    )

  private def withSamples(document: Json)(change: Json => Json): Json =
    document.hcursor
      .downField("value")
      .downField("recording")
      .downField("samples")
      .withFocus(change)
      .top
      .get

  test(
    "every support category and lineage packs into pinned columns and reconstructs exactly"
  ) {
    val packed = get(codec.encode(monocular))
    val refs   = packed.payloads.map(_.ref)
    assertEquals(
      refs.map(_.sha256.hex),
      Vector(
        "b0eb08db23502a251b675c9f0683108455c0f38c549a2c12ec249f7daf21d7b5",
        "9d92b6855fd8a4ccfd53bf41a75cb97381a420422c00ce381eb309304a533001",
        "b376730a4db09f72de102f105a7db3904683a5e3f384d047c0b3855a91d9f145",
        "4ead6bd01976f4f532fac378ff25513d11cb2258a3c2ef475086f9df5e03a795"
      )
    )
    assertEquals(
      refs.map(r => (r.layout.element, r.layout.shape, r.layout.order)),
      Vector(
        (ElementKind.Int64, Vector(8), ArrayOrder.RowMajor),
        (ElementKind.UInt8, Vector(8), ArrayOrder.RowMajor),
        (ElementKind.UInt8, Vector(8), ArrayOrder.RowMajor),
        (ElementKind.Float64, Vector(8, 3), ArrayOrder.ColumnMajor)
      )
    )
    assertEquals(
      get(samplesOf(packed.document).hcursor.get[Vector[String]]("lineages")),
      Vector("measured", "measured>smoothed", "interpolated", "interpolated>smoothed")
    )
    assertEquals(PackedRecordingCodecs.references(packed.document), Right(refs))
    val decoded = get(codec.decode(packed.document, lookup(packed)))
    assertEquals(decoded.contentHash, monocular.contentHash)
    assertEquals(decoded.samples.toVector, monocular.samples.toVector)
    assertEquals(get(codec.encode(decoded)).document, packed.document)
  }

  test("timestamps beyond JavaScript's exact integer range reconstruct exactly") {
    val base    = 9007199254740993L
    val samples = IArray.tabulate(3)(i =>
      Sample(Instant.micros(base + i * 1000L), Gaze.Tracked(Pt[Px](1.0 + i, 2.0), None))
    )
    val recording = get(
      Recording.of(
        InputPayloadFixtures.display,
        InputPayloadFixtures.tracker,
        Rate.Irregular,
        Eye.Right,
        None,
        samples,
        get(SamplingTolerance.of(Span.micros(1)))
      )
    )
    val packed  = get(codec.encode(recording))
    val decoded = get(codec.decode(packed.document, lookup(packed)))
    assertEquals(
      decoded.samples.map(_.t.toMicros).toVector,
      Vector(base, base + 1000, base + 2000)
    )
    assertEquals(decoded.contentHash, recording.contentHash)
  }

  test("decoding never loads a payload itself: a missing or foreign payload is refused") {
    val packed = get(codec.encode(monocular))
    val times  = packed.payloads.head.ref
    assertEquals(
      codec.decode(packed.document, ref => if ref == times then None else lookup(packed)(ref)),
      Left(CodecError.Entry("recording", CodecError.MissingPayload("samples.tMicros", times)))
    )
    // A lookup that answers with another payload is not trusted.
    assertEquals(
      codec.decode(packed.document, _ => packed.payloads.lift(1)),
      Left(CodecError.Entry("recording", CodecError.MissingPayload("samples.tMicros", times)))
    )
  }

  test("declared layouts, support codes, fillers and lineage order are checked") {
    val packed      = get(codec.encode(monocular))
    val wrongLayout = withSamples(packed.document)(samples =>
      samples.mapObject(o => o.add("tMicros", get(o("support").toRight("support"))))
    )
    assert(codec.decode(wrongLayout, lookup(packed)).left.exists {
      case CodecError.Entry("recording", CodecError.Field("samples.tMicros", _, reason)) =>
        reason.contains("int64[8]")
      case _ => false
    })

    def replacing(
        index: Int,
        values: IArray[Byte]
    ): (Json, PayloadRef => Option[VerifiedPayload]) =
      val old    = packed.payloads(index)
      val next   = get(PackedArrays.pack(old.ref.layout, values))
      val field  = Vector("tMicros", "support", "lineage", "values")(index)
      val edited =
        withSamples(packed.document)(_.mapObject(_.add(field, PayloadRef.json(next.ref))))
      edited -> (ref => if ref == next.ref then Some(next) else lookup(packed)(ref))

    // A pupil bit on a blink is not a support code.
    val (badCode, badCodes) = replacing(1, IArray[Byte](4, 4, 5, 2, 0, 3, 4, 4))
    assertEquals(
      codec.decode(badCode, badCodes),
      Left(
        CodecError.Entry(
          "recording.samples[2]",
          CodecError.Field("support", Json.fromInt(5), "unknown support code")
        )
      )
    )
    // Lineage indices must follow first appearance.
    val (badOrder, badOrders) = replacing(2, IArray[Byte](0, 2, 0, 0, 1, 0, 3, 0))
    assert(codec.decode(badOrder, badOrders).left.exists {
      case CodecError.Entry("recording", CodecError.Field("samples.lineage[1]", _, _)) => true
      case _                                                                           => false
    })
    // A blink's absent position must be +0.0 exactly, not -0.0.
    val valuesIndex = 3
    val doubles     =
      get(PackedArrays.unpack[Double](packed.payloads(valuesIndex))).toVector.toArray
    doubles(2) = -0.0
    val next =
      get(PackedArrays.pack(packed.payloads(valuesIndex).ref.layout, IArray.from(doubles)))
    val edited =
      withSamples(packed.document)(_.mapObject(_.add("values", PayloadRef.json(next.ref))))
    assert(
      codec
        .decode(edited, ref => if ref == next.ref then Some(next) else lookup(packed)(ref))
        .left
        .exists {
          case CodecError.Entry("recording.samples[2]", CodecError.Field("x", _, reason)) =>
            reason.contains("+0.0")
          case _ => false
        }
    )
  }

  test("the declared content hash is compared with the reconstruction") {
    val packed = get(codec.encode(monocular))
    val times  = get(PackedArrays.unpack[Long](packed.payloads.head)).toVector.toArray
    times(7) = times(7) + 1
    val next   = get(PackedArrays.pack(packed.payloads.head.ref.layout, IArray.from(times)))
    val edited =
      withSamples(packed.document)(_.mapObject(_.add("tMicros", PayloadRef.json(next.ref))))
    assert(
      codec
        .decode(edited, ref => if ref == next.ref then Some(next) else lookup(packed)(ref))
        .left
        .exists {
          case CodecError.Entry("recording", CodecError.InputIdentity(declared, _)) =>
            declared == monocular.contentHash.render
          case _ => false
        }
    )
  }
