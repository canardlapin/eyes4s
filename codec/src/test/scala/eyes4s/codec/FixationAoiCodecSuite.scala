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

import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.DefinitionId
import io.circe.Json

class FixationAoiCodecSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val schema                            = get(DefinitionId.of("test.fixation-aoi", 1))
  private val codec                             = FixationAoiCodecs.assignment[Px](schema)
  private val frame                             = get(Frame.screen("aoi-codec", 100, 100))
  private val clock                             = ClockId("aoi-codec")
  private def area(id: String, lo: Double, hi: Double): Aoi[Px] =
    get(Aoi.of(id, id, frame, get(Region.rect(Pt[Px](lo, 0), Pt[Px](hi, 60)))))
  private val disjoint = get(AoiSet.of(Vector(area("left", 0, 50), area("right", 50, 100))))
  private val overlap  = get(AoiSet.of(Vector(area("wide", 0, 80), area("small", 0, 30))))
  private val onset    = 9007199254740993L
  private def fixation(
      start: Long,
      duration: Long,
      x: Double,
      y: Double = 10
  ): Event.Fixation[Px] =
    get(
      Event.Fixation.withoutDispersion(
        get(Interval.of(clock, Instant.micros(start), Instant.micros(start + duration))),
        Pt[Px](x, y),
        1
      )
    )
  private val path = get(
    Scanpath.of(
      frame,
      clock,
      IArray(
        fixation(onset, 17, 10),
        fixation(onset + 30, 29, 20),
        fixation(onset + 100, 31, 70),
        fixation(onset + 140, 7, 90, 90),
        fixation(onset + 160, 13, 110),
        fixation(onset + 180, 19, 10)
      )
    )
  )
  private val assignment = get(disjoint.assign(path, MembershipPolicy.Multiple))
  private def rewrite(json: Json, field: String)(f: Json => Json): Json =
    val changed = get(json.hcursor.downField("value").downField(field).as[Json])
    json.mapObject(
      _.add(
        "value",
        get(json.hcursor.downField("value").as[Json]).mapObject(_.add(field, f(changed)))
      )
    )

  test("all spatial policies roundtrip complete assignment, measures and exact 64-bit times") {
    Vector(
      MembershipPolicy.Multiple,
      MembershipPolicy.ExclusiveByPriority,
      MembershipPolicy.SmallestContaining(get(AoiResolution.of(10, 10))),
      MembershipPolicy.RejectOverlap
    ).foreach { policy =>
      val set      = if policy == MembershipPolicy.RejectOverlap then disjoint else overlap
      val original = get(set.assign(path, policy))
      val json     = get(codec.encode(original))
      val restored = get(codec.decode(json))
      assertEquals(get(codec.encode(restored)), json)
      assertEquals(restored.scanpath.first.span.onset.toMicros, onset)
      assertEquals(restored.toVector, original.toVector)
      assertEquals(restored.policy, policy)
      assert(restored.accountingHolds)
      assertEquals(restored.aoiSet.frame.spec, frame.spec)
      assertEquals(restored.scanpath.clock, clock)
      assertEquals(
        get(json.hcursor.downField("schema").downField("name").as[String]),
        "test.fixation-aoi"
      )
      assert(json.noSpaces.contains(onset.toString))
      assertEquals(
        get(
          json.hcursor
            .downField("value")
            .downField("derived")
            .downField("report")
            .downField("aoiUnionMicros")
            .as[String]
        ),
        original.report.aoiUnionTime.toMicros.toString
      )
    }
  }

  test("replay refuses each altered derived field rather than trusting stored metrics") {
    val json = get(codec.encode(assignment))
    Vector("memberships", "report", "metrics", "transitions").foreach { field =>
      val changed = rewrite(json, "derived")(_.mapObject(_.add(field, Json.arr())))
      codec.decode(changed) match
        case Left(CodecError.Field(name, _, reason)) =>
          assertEquals(name, s"derived.$field")
          assert(reason.contains("replay"))
        case other => fail(s"unexpected $other")
    }
  }

  test("well-formed but false visit counts and duration ledgers are rejected") {
    val json   = get(codec.encode(assignment))
    val visits = json.hcursor
      .downField("value")
      .downField("derived")
      .downField("metrics")
      .downArray
      .downField("visitCount")
      .withFocus(_ => Json.fromInt(99))
      .top
      .get
    val duration = json.hcursor
      .downField("value")
      .downField("derived")
      .downField("report")
      .downField("interFixationMicros")
      .withFocus(_ => Json.fromString("999"))
      .top
      .get
    assert(codec.decode(visits).left.exists(_.message.contains("derived.metrics")))
    assert(codec.decode(duration).left.exists(_.message.contains("derived.report")))
  }

  test(
    "unknown versions, missing schemas, invalid policies and duplicate area ids fail explicitly"
  ) {
    val json = get(codec.encode(assignment))
    assert(
      codec
        .decode(
          json.mapObject(_.add("schema", Wire.id(get(DefinitionId.of("test.fixation-aoi", 2)))))
        )
        .isLeft
    )
    assert(codec.decode(json.mapObject(_.remove("schema"))).isLeft)
    assert(
      codec
        .decode(rewrite(json, "policy")(_ => Json.obj("kind" -> Json.fromString("unknown"))))
        .isLeft
    )
    assert(
      codec
        .decode(
          rewrite(json, "policy")(_ =>
            Json.obj(
              "kind" -> Json.fromString("smallestContaining"),
              "nx"   -> Json.fromInt(0),
              "ny"   -> Json.fromInt(10)
            )
          )
        )
        .isLeft
    )
    val duplicate =
      rewrite(json, "areas")(raw => Json.arr(raw.asArray.get.head, raw.asArray.get.head))
    assert(codec.decode(duplicate).left.exists(_.message.contains("duplicate")))
    val overlapRefused = get(codec.encode(get(overlap.assign(path, MembershipPolicy.Multiple))))
    assert(
      codec
        .decode(
          rewrite(overlapRefused, "policy")(_ =>
            Json.obj("kind" -> Json.fromString("rejectOverlap"))
          )
        )
        .left
        .exists(_.message.contains("fixation[0]"))
    )
  }

  test("source-supported scanpaths retain samples and rederive summaries on decode") {
    val recording = get(
      Recording.of(
        frame,
        clock,
        Rate.Fixed(get(Hz(1000))),
        Eye.Left,
        None,
        IArray(
          Sample(Instant.millis(0), Gaze.Tracked(Pt[Px](10, 10), None)),
          Sample(Instant.millis(1), Gaze.Tracked(Pt[Px](20, 10), None))
        )
      )
    )
    val ref   = RecordingRef("fixation-aoi-source")
    val event = get(
      Event.Fixation.of(
        get(Interval.of(clock, Instant.millis(0), Instant.millis(2))),
        Pt[Px](15, 10),
        5,
        DispersionMethod.RmsRadius,
        2
      )
    )
    val supported = get(
      Scanpath.fromEvents(
        get(EventSeries.of(recording, ref, Vector(event), Vector(get(SampleRange.of(0, 2)))))
      )
    )
    val original = get(disjoint.assign(supported, MembershipPolicy.Multiple))
    val json     = get(codec.encode(original))
    val restored = get(codec.decode(json))
    assertEquals(restored.scanpath.source, Some(ref))
    assertEquals(restored.scanpath.sampleSupport, supported.sampleSupport)
    assertEquals(
      restored.scanpath.sourceRecording.map(_.contentHash),
      Some(recording.contentHash)
    )
    assertEquals(get(codec.encode(restored)), json)
    val wrongCentre = json.hcursor
      .downField("value")
      .downField("scanpath")
      .downField("value")
      .downField("fixations")
      .downArray
      .downField("x")
      .withFocus(_ => Json.fromInt(40))
      .top
      .get
    assert(codec.decode(wrongCentre).isLeft)
  }
