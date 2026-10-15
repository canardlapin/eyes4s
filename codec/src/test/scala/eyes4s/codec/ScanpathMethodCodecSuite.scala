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

import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Px, Deg}
import eyes4s.plan.*
import io.circe.Json

private[codec] object ScanpathMethodCodecExamples:
  private def get[E, A](value: Either[E, A]): A = value.toOption.get
  val codec: VersionedCodec[ScanpathMethod[Px]] = new ScanpathMethodCodec[Px].codec
  val multiMatch: ScanpathMethod[Px]            = ScanpathMethod.MultiMatch(
    MultiMatchCardinality.BaselineThreeFixations,
    get(AlignmentCellLimit.of(1234567))
  )
  val overlap: ScanpathMethod[Px] = ScanpathMethod.Overlap(
    get(
      FixationOverlapSpec.of(
        ClockId("exact queries"),
        Vector(Long.MaxValue, Long.MinValue, 9007199254740993L, Long.MaxValue)
          .map(Instant.micros),
        get(OverlapThreshold.of[Px](3.5)),
        FixationGroundDistance.Manhattan,
        TrajectoryEndpoint.OnsetRange,
        MissingOverlapPolicy.RequireComplete
      )
    )
  )
  val transport: ScanpathMethod[Px] = ScanpathMethod.Transport(
    get(
      FixationTransportConfig.of[Px](
        12.5,
        2.25,
        Span.micros(9007199254740993L),
        0.25,
        0.125,
        321,
        1e-8,
        1234,
        Long.MaxValue
      )
    )
  )
  val all: Vector[ScanpathMethod[Px]] = Vector(multiMatch, overlap, transport)

class ScanpathMethodCodecSuite extends munit.FunSuite:
  import ScanpathMethodCodecExamples.*
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def alter(document: Json, name: String, value: Json): Json =
    document.mapObject(
      _.add("value", get(document.hcursor.get[Json]("value")).mapObject(_.add(name, value)))
    )
  private def parameters(document: Json): Json = get(
    document.hcursor.downField("value").get[Json]("parameters")
  )

  test("every registered method restores typed parameters, identity and declared properties") {
    all.foreach { method =>
      val saved = get(codec.encode(method)); val restored = get(codec.decode(saved))
      assertEquals(restored.id, method.id)
      assertEquals(get(method.inspect).description, method.description)
      assert(get(method.inspect).fields.forall(_.info.meaning.nonEmpty))
      assertEquals(restored.parameters, method.parameters)
      assertEquals(restored.parameterHash, method.parameterHash)
      assertEquals(restored.properties, method.properties)
      assertEquals(codec.encode(restored), Right(saved))
    }
  }
  test("signed64 query order, multiplicity, time scale and work limits survive exactly") {
    get(codec.decode(get(codec.encode(overlap)))) match
      case ScanpathMethod.Overlap(spec) =>
        assertEquals(
          spec.queries.map(_.toMicros),
          Vector(Long.MaxValue, Long.MinValue, 9007199254740993L, Long.MaxValue)
        )
      case other => fail(s"$other")
    get(codec.decode(get(codec.encode(transport)))) match
      case ScanpathMethod.Transport(config) =>
        assertEquals(config.timeScale.toMicros, 9007199254740993L)
        assertEquals(config.maximumWork, Long.MaxValue)
        assertEquals(config.maximumIterations, 321)
        assertEquals(config.maximumCosts, 1234)
      case other => fail(s"$other")
  }
  test("alignment tie convention and method version cannot change during restore") {
    val saved = get(codec.encode(multiMatch))
    assert(
      codec
        .decode(
          alter(
            saved,
            "parameters",
            parameters(saved)
              .mapObject(_.add("alignmentConvention", Json.fromString("another-path")))
          )
        )
        .isLeft
    )
    val method = get(saved.hcursor.downField("value").get[Json]("method"))
    assert(
      codec
        .decode(alter(saved, "method", method.mapObject(_.add("version", Json.fromInt(2)))))
        .isLeft
    )
    val overlapSaved = get(codec.encode(overlap))
    assert(
      codec
        .decode(
          alter(
            overlapSaved,
            "parameters",
            parameters(overlapSaved)
              .mapObject(_.add("normConvention", Json.fromString("runtime-native")))
          )
        )
        .isLeft
    )
  }
  test("missing, extra and invalid method settings are named refusals") {
    val saved = get(codec.encode(multiMatch))
    assert(
      codec
        .decode(
          alter(
            saved,
            "parameters",
            parameters(saved).mapObject(_.remove("maximumAlignmentCells"))
          )
        )
        .isLeft
    )
    assert(
      codec
        .decode(
          alter(
            saved,
            "parameters",
            parameters(saved).mapObject(_.add("maximumAlignmentCells", Json.fromString("0")))
          )
        )
        .isLeft
    )
    assert(
      codec
        .decode(
          alter(
            saved,
            "parameters",
            parameters(saved).mapObject(_.add("threshold", Json.fromInt(5)))
          )
        )
        .isLeft
    )
    val sampled = get(codec.encode(overlap))
    assert(
      codec
        .decode(
          alter(
            sampled,
            "parameters",
            parameters(sampled).mapObject(_.add("queries", Json.arr()))
          )
        )
        .isLeft
    )
    assert(
      codec
        .decode(
          alter(
            sampled,
            "parameters",
            parameters(sampled)
              .mapObject(_.add("queries", Json.arr(Json.fromString("9223372036854775808"))))
          )
        )
        .isLeft
    )
    val moved = get(codec.encode(transport))
    assert(
      codec
        .decode(
          alter(
            moved,
            "parameters",
            parameters(moved).mapObject(_.add("lambda", Json.fromInt(0)))
          )
        )
        .isLeft
    )
  }
  test("spatial scales and thresholds retain their static unit at the wire boundary") {
    val saved = get(codec.encode(overlap))
    assert(new ScanpathMethodCodec[Deg].codec.decode(saved).isLeft)
  }

  test("all three pinned method documents restore on this runtime") {
    val documents = Vector(
      ScanpathMethodFixtures.multiMatch,
      ScanpathMethodFixtures.overlap,
      ScanpathMethodFixtures.transport
    )
    all.zip(documents).foreach { (method, text) =>
      val saved = get(io.circe.parser.parse(text))
      assertEquals(codec.encode(method), Right(saved))
      val restored = get(codec.decode(saved))
      assertEquals(restored.parameters, method.parameters)
      assertEquals(codec.encode(restored), Right(saved))
    }
  }
