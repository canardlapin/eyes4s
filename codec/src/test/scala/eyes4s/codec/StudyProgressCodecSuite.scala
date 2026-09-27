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

import eyes4s.plan.*
import io.circe.Json

class StudyProgressCodecSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val codec                             = StudyProgressCodec.codec[String, Int]
  private val pinned                            =
    """{"schema":{"name":"eyes4s.study-progress","version":1},"value":{"stamp":{"plan":"0000000000000000000000000000000000000000000000000000000000000000","input":"1111111111111111111111111111111111111111111111111111111111111111"},"pairQuantum":1,"comparisonQuantum":1,"step":"9007199254740993","stage":{"kind":"contrasting","scale":0,"meter":{"unit":"rows","done":"9223372036854775807","total":{"kind":"at-most","value":"9223372036854775807"}}},"stepUnits":1,"segmentUnits":"9007199254740993","segmentTotal":{"kind":"unknown"},"totalUnits":"9223372036854775807"}}"""
  private val document = get(io.circe.parser.parse(pinned))
  private val value    = get(codec.decode(document))
  private def changed(field: String, replacement: Json): Json =
    document.mapObject(o => o.add("value", o("value").get.mapObject(_.add(field, replacement))))

  test("version one text preserves counters above 2^53 and Long.MaxValue") {
    assertEquals(value.step, 9007199254740993L)
    assertEquals(value.segmentUnits, 9007199254740993L)
    assertEquals(value.totalUnits, Long.MaxValue)
    value.stage match
      case StudyRunStage.Running(StudyStage.Contrasting(0), meter) =>
        assertEquals(meter.done, Long.MaxValue)
        assertEquals(meter.total, SegmentTotal.AtMost(Long.MaxValue))
      case other => fail(s"unexpected stage $other")
    assertEquals(get(codec.encode(value)), document)
    assertEquals(get(codec.encode(get(codec.parse(pinned)))), document)
  }

  test("numeric, noncanonical, overflowing and invalid counters refuse") {
    Vector(
      Json.fromLong(1),
      Json.fromString("01"),
      Json.fromString("+1"),
      Json.fromString("-0"),
      Json.fromString("9223372036854775808"),
      Json.fromString("0"),
      Json.fromString("-1")
    ).foreach { invalid =>
      assert(codec.decode(changed("step", invalid)).isLeft, clue(invalid))
    }
    assert(codec.decode(changed("stepUnits", Json.fromInt(-1))).isLeft)
    assert(codec.decode(changed("segmentUnits", Json.fromString("0"))).isLeft)
    assert(codec.decode(changed("totalUnits", Json.fromString("0"))).isLeft)
    assert(codec.decode(changed("pairQuantum", Json.fromInt(0))).isLeft)
    assert(codec.decode(changed("comparisonQuantum", Json.fromInt(0))).isLeft)
  }

  test("running requires its meter and matching scientific count unit") {
    val stage = get(document.hcursor.downField("value").downField("stage").as[Json])
    assert(codec.decode(changed("stage", stage.mapObject(_.remove("meter")))).isLeft)
    val meter = get(stage.hcursor.downField("meter").as[Json])
    val wrong =
      stage.mapObject(_.add("meter", meter.mapObject(_.add("unit", Json.fromString("maps")))))
    assert(codec.decode(changed("stage", wrong)).isLeft)
    val negative =
      stage.mapObject(_.add("meter", meter.mapObject(_.add("done", Json.fromString("-1")))))
    assert(codec.decode(changed("stage", negative)).isLeft)
    assert(
      codec.decode(changed("stage", stage.mapObject(_.add("scale", Json.fromInt(-1))))).isLeft
    )
  }

  test("counting is distinct from unknown and carries no fabricated scientific meter") {
    val stage = Json.obj(
      "kind"    -> Json.fromString("counting"),
      "design"  -> Json.fromString("matched"),
      "visited" -> Json.fromString("9007199254740993")
    )
    val counting = changed("stage", stage).mapObject(o =>
      o.add(
        "value",
        o("value").get.mapObject(
          _.add("segmentTotal", Json.obj("kind" -> Json.fromString("counting")))
        )
      )
    )
    val decoded = get(codec.decode(counting))
    assertEquals(decoded.stage, StudyRunStage.Counting(StudyDesign.Matched, 9007199254740993L))
    assertEquals(decoded.segmentTotal, SegmentTotal.Counting)
    assertEquals(get(codec.encode(decoded)), counting)
    assert(codec.decode(changed("stage", stage)).isLeft)
    assert(
      codec
        .decode(changed("segmentTotal", Json.obj("kind" -> Json.fromString("counting"))))
        .isLeft
    )
  }

  test("unsupported progress schema versions are refused explicitly") {
    val future = document.mapObject(o =>
      o.add("schema", o("schema").get.mapObject(_.add("version", Json.fromInt(2))))
    )
    assert(codec.decode(future).left.exists {
      case CodecError.Schema(expected, found) =>
        expected == StudyProgressDefinitions.progress && found.version == 2
      case _ => false
    })
  }
