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

import cats.instances.string.*
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

class EpochComponentCodecSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val markerSchema                      = get(DefinitionId.of("example.marker", 1))
  private def markers(schema: DefinitionId)     =
    VersionedCodec.checked[String](schema)(s => Right(Json.fromString(s)))(j =>
      j.asString.toRight(CodecError.Field("marker", j, "expected text"))
    )
  private val codec  = EpochCodecs.of(markers(markerSchema))
  private val clock  = ClockId("observed")
  private val budget = get(NonNegativeLong.of(100))
  private def plan(
      occurrence: Occurrence = Occurrence.First,
      from: Long = -5,
      until: Long = 8,
      width: Long = 4,
      finalBin: FinalBin = FinalBin.IncludeShortFinal
  ) =
    get(
      EpochPlan.of(
        MarkSelector("start", occurrence),
        get(Window.of(Span.micros(from), Span.micros(until))),
        get(PositiveSpan.of(Span.micros(width))),
        finalBin
      )
    )
  private def same(a: EpochPlan[String], b: EpochPlan[String]): Unit =
    assertEquals(a.anchor, b.anchor)
    assertEquals(a.window, b.window)
    assertEquals(a.binWidth, b.binWidth)
    assertEquals(a.finalBin, b.finalBin)
  private def change(document: Json, name: String, value: Json): Json =
    val payload = get(document.hcursor.get[Json]("value"))
    document.mapObject(_.add("value", payload.mapObject(_.add(name, value))))

  test("the built-in envelope preserves the existing conditional version-one payload") {
    val value =
      plan(Occurrence.Nth(get(NonNegativeLong.of(1))), finalBin = FinalBin.ExcludeAndReport)
    val legacy =
      EpochCodecs.plan(get(DefinitionId.of("example.custom-epoch", 1)), markers(markerSchema))
    val expected = get(legacy.encode(value)).hcursor.get[Json]("value")
    val actual   = get(codec.encode(value)).hcursor.get[Json]("value")
    assertEquals(actual, expected)
    assertEquals(
      get(actual).hcursor.downField("occurrence").get[String]("kind"),
      Right("nth-zero-based")
    )
  }

  test("all occurrence and final-bin combinations round-trip canonically") {
    Vector(
      Occurrence.RequireUnique,
      Occurrence.First,
      Occurrence.Last,
      Occurrence.Nth(get(NonNegativeLong.of(Long.MaxValue)))
    ).foreach { selected =>
      FinalBin.values.foreach { policy =>
        val value    = plan(selected, finalBin = policy)
        val document = get(codec.encode(value))
        val decoded  = get(codec.decode(document))
        same(value, decoded)
        assertEquals(get(codec.encode(decoded)), document)
      }
    }
  }

  test("integer microseconds and occurrence indices are exact beyond binary64") {
    val n        = 9007199254740993L
    val value    = plan(Occurrence.Nth(get(NonNegativeLong.of(n))), n, n + 7, n)
    val document = get(codec.encode(value))
    assertEquals(get(document.hcursor.downField("value").get[String]("fromMicros")), n.toString)
    assertEquals(
      get(document.hcursor.downField("value").get[String]("binWidthMicros")),
      n.toString
    )
    same(value, get(codec.decode(document)))
  }

  test("a decoded recipe resolves observed bins identically without storing a timeline") {
    val timeline = get(ObservedTimeline.of(clock, Vector(Mark(Instant.micros(100), "start"))))
    val value    = plan(Occurrence.RequireUnique)
    val document = get(codec.encode(value))
    assert(!document.noSpaces.contains("observed"))
    val decoded = get(codec.decode(document))
    val a       = get(value.resolve("trial", timeline, clock, budget))
    val b       = get(decoded.resolve("trial", timeline, clock, budget))
    assertEquals(b.anchor, a.anchor)
    assertEquals(b.interval, a.interval)
    assertEquals(b.bins, a.bins)
    assertEquals(b.excludedTail, a.excludedTail)
  }

  test("the caller's exact mark schema and the recipe schema are checked") {
    val document = get(codec.encode(plan()))
    val next     = get(DefinitionId.of(markerSchema.name, 2))
    assert(EpochCodecs.of(markers(next)).decode(document) match
      case Left(CodecError.Schema(`next`, `markerSchema`)) => true
      case _                                               => false)
    val wrong = document.mapObject(
      _.add(
        "schema",
        Json.obj(
          "name"    -> Json.fromString(EpochCodecDefinitions.plan.name),
          "version" -> Json.fromInt(2)
        )
      )
    )
    assert(codec.decode(wrong).isLeft)
  }

  test("invalid widths windows occurrences and policies are values with operands") {
    val document = get(codec.encode(plan()))
    val invalid  = Vector(
      change(document, "binWidthMicros", Json.fromString("0")),
      change(document, "fromMicros", Json.fromString("9")),
      change(
        document,
        "occurrence",
        Json.obj("kind" -> Json.fromString("nth-zero-based"), "index" -> Json.fromString("-1"))
      ),
      change(document, "occurrence", Json.obj("kind" -> Json.fromString("Unknown"))),
      change(document, "finalBin", Json.fromString("Unknown"))
    )
    invalid.foreach(j => assert(codec.decode(j).left.toOption.exists(_.message.nonEmpty)))
  }

  test("empty half-open windows and every declared remainder policy remain legal recipes") {
    FinalBin.values.foreach { policy =>
      val value = plan(from = 0, until = 0, width = 1, finalBin = policy)
      same(value, get(codec.decode(get(codec.encode(value)))))
    }
  }
