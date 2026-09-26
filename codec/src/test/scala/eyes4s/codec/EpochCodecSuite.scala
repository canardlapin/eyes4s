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

class EpochCodecSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)
  private val schema                        = get(DefinitionId.of("test.epoch", 1))
  private val keys   = VersionedCodec.string(get(DefinitionId.of("test.mark", 1)))
  private val codec  = EpochCodecs.plan(schema, keys)
  private val pinned =
    """{"schema":{"name":"test.epoch","version":1},"value":{"anchor":{"schema":{"name":"test.mark","version":1},"value":"start"},"occurrence":{"kind":"nth-zero-based","index":"1"},"fromMicros":"-5","untilMicros":"6","binWidthMicros":"4","finalBin":"ExcludeAndReport"}}"""

  test("pinned v1 recipe retains scientific meaning, time encoding and execution") {
    val p = get(codec.parse(pinned))
    assertEquals(get(codec.encode(p)), get(io.circe.parser.parse(pinned)))
    assertEquals(p.anchor, MarkSelector("start", Occurrence.Nth(get(NonNegativeLong.of(1)))))
    val clock = ClockId("timeline")
    val marks = ObservedTimeline.from(
      get(
        Timeline.of(
          clock,
          Vector(Mark(Instant.micros(0), "start"), Mark(Instant.micros(100), "start"))
        )
      )
    )
    val bins = get(p.resolve("trial", marks, clock, get(NonNegativeLong.of(10))))
    assertEquals(bins.anchor.toMicros, 100L)
    assertEquals(bins.bins.map(_.duration.toMicros), Vector(4L, 4L))
    assertEquals(bins.excludedTail.map(_.duration.toMicros), Some(3L))
  }

  test("all policies roundtrip independently supplied recipe and key schemas") {
    for
      occurrence <- Vector(
        Occurrence.RequireUnique,
        Occurrence.First,
        Occurrence.Last,
        Occurrence.Nth(get(NonNegativeLong.of(Long.MaxValue)))
      )
      finalBin <- FinalBin.values
    do
      val p = get(
        EpochPlan.of(
          MarkSelector("start", occurrence),
          get(Window.of(Span.micros(-9007199254740995L), Span.micros(9007199254740995L))),
          get(PositiveSpan.of(Span.micros(9007199254740995L))),
          finalBin
        )
      )
      val encoded = get(codec.encode(p))
      val decoded = get(codec.decode(encoded))
      assertEquals(decoded.anchor, p.anchor)
      assertEquals(decoded.window, p.window)
      assertEquals(decoded.binWidth.toMicros, p.binWidth.toMicros)
      assertEquals(decoded.finalBin, p.finalBin)
      assertEquals(get(codec.encode(decoded)), encoded)
  }

  test("malformed policies, indices, widths and versions fail as values") {
    val json    = get(io.circe.parser.parse(pinned))
    val payload = get(json.hcursor.downField("value").as[Json])
    def replace(field: String, value: Json): Json =
      json.mapObject(_.add("value", payload.mapObject(_.add(field, value))))
    Vector(
      replace("occurrence", Json.obj("kind" -> Json.fromString("predicate"))),
      replace(
        "occurrence",
        Json.obj("kind" -> Json.fromString("nth-zero-based"), "index" -> Json.fromString("-1"))
      ),
      replace(
        "occurrence",
        Json.obj("kind" -> Json.fromString("nth-zero-based"), "index" -> Json.fromLong(1))
      ),
      replace("binWidthMicros", Json.fromString("0")),
      replace("binWidthMicros", Json.fromString("-1")),
      replace("untilMicros", Json.fromString("-6")),
      replace("finalBin", Json.fromString("truncate")),
      json.mapObject(_.add("schema", Wire.id(get(DefinitionId.of("test.epoch", 2)))))
    ).foreach(j => assert(codec.decode(j).isLeft))
  }
