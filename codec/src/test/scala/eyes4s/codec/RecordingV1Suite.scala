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
import eyes4s.kernel.*
import io.circe.Json

class RecordingV1Suite extends munit.FunSuite:
  private def checked[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)
  private def id(name: String): DefinitionId    = checked(DefinitionId.of(name, 1))
  private val persistence                       = RecordingCodecs.ivt(
    id("eyes4s.recording-plan"),
    id("eyes4s.recording.ivt"),
    id("eyes4s.ivt-parameters")
  )
  // Mirrors resources/eyes4s/recording-v1.json for portable JVM/JS tests.
  private val fixture =
    """{"schema":{"name":"eyes4s.recording-plan","version":1},"value":{"method":{"name":"eyes4s.recording.ivt","version":1},"parameters":{"schema":{"name":"eyes4s.ivt-parameters","version":1},"value":{"threshold":30,"minimumMicros":"60000"}},"input":"0123456789abcdef","source":"recording-v1-oracle","display":{"id":"screen-v1","unit":"px","xMin":0,"yMin":0,"xMax":800,"yMax":600,"yAxis":"Down"},"trackerClock":"tracker-v1","analysisClock":"analysis-v1","angularFrame":"angular-v1","viewing":{"distanceMm":600,"widthMm":400,"heightMm":300},"syncModel":"OffsetOnly","marks":[{"id":"trigger-v1","sourceMicros":"9007199254740993","targetMicros":"9007199254741003"}],"residualLimitMicros":"500","interpolationGapMicros":"75000","areas":[{"id":"image","label":"Image","xMin":100,"yMin":100,"xMax":700,"yMax":500}]}}"""

  test(
    "frozen recording v1 independently fixes units, nominal IDs, parameters and exact times"
  ) {
    val plan = checked(persistence.codec.parse(fixture))
    assertEquals(plan.input.digest, "0123456789abcdef")
    assertEquals(plan.display.id, FrameId("screen-v1"))
    assertEquals(plan.display.yAxis, YAxis.Down)
    assertEquals(plan.display.bounds.xMax, 800.0)
    assertEquals(plan.trackerClock, ClockId("tracker-v1"))
    assertEquals(plan.analysisClock, ClockId("analysis-v1"))
    assertEquals(plan.angularFrameId, FrameId("angular-v1"))
    assertEquals(plan.viewing.map(_.perspective.distance.toMm), Some(600.0))
    assertEquals(plan.marks.map(_.onSource.toMicros), Vector(9007199254740993L))
    assertEquals(plan.marks.map(_.onTarget.toMicros), Vector(9007199254741003L))
    assertEquals(plan.parameters.threshold.velocity.value, 30.0)
    assertEquals(plan.parameters.minimumDuration.span.toMicros, 60000L)
    assertEquals(plan.interpolationGap.span.toMicros, 75000L)
    assertEquals(plan.residualLimit.map(_.span.toMicros), Some(500L))
    assertEquals(
      plan.areas.map(a => (a.id, a.label, a.bounds.xMin)),
      Vector(("image", "Image", 100.0))
    )
    val encoded = checked(persistence.codec.encode(plan))
    assertEquals(encoded, checked(io.circe.parser.parse(fixture)))
    assertEquals(checked(persistence.codec.parse(encoded.spaces2)).diff(plan), Vector.empty)
  }

  test(
    "v1 fixture exposes changes in frame identity, axis and time unit without relying on roundtrip"
  ) {
    val original                                 = checked(persistence.codec.parse(fixture))
    val json                                     = checked(io.circe.parser.parse(fixture))
    def changed(value: Json): Vector[PlanChange] =
      checked(persistence.codec.decode(json.mapObject(_.add("value", value)))).diff(original)
    val value   = checked(json.hcursor.get[Json]("value"))
    val display = checked(value.hcursor.get[Json]("display"))
    Vector("id" -> "different", "yAxis" -> "Up").foreach { case (key, text) =>
      assert(
        changed(
          value.mapObject(
            _.add("display", display.mapObject(_.add(key, Json.fromString(text))))
          )
        ).nonEmpty
      )
    }
    val mark = checked(value.hcursor.get[Vector[Json]]("marks")).head
    assert(
      changed(
        value.mapObject(
          _.add(
            "marks",
            Json.arr(mark.mapObject(_.add("sourceMicros", Json.fromString("9007199254740"))))
          )
        )
      ).nonEmpty
    )
  }
