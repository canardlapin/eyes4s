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
import eyes4s.plan.*
import io.circe.Json

/** The frozen v1 recording and temporal payloads pin decoded meaning
  * independently of the encoder, and re-encode to the same JSON value.
  */
class InputPayloadV1Suite extends munit.FunSuite:
  import InputPayloadFixtures.*

  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private val inputs                        = RecordingInputCodecs.input[Px]
  private val studies                       = StudyInputCodecs.study[Px]
  private val temporals                     = TemporalInputCodecs.study[Px]()
  private def parse(text: String): Json     = get(io.circe.parser.parse(text))

  test("frozen recording-input v1 fixes digests, support categories, lineage and sync marks") {
    val value = get(inputs.parse(recordingInputVersionOne))
    assertEquals(value.reference.digest, "d4c0d76ef6fd5169")
    assertEquals(value.source, RecordingRef("synthetic-left-headfixed-500"))
    val recording = value.monocular.get
    assertEquals(recording.contentHash.render, "2c826dc41ae25e67")
    assertEquals(recording.size, 8)
    assertEquals(recording.eye, Eye.Left)
    assertEquals(recording.pupilUnit, Some(PupilUnit.Arbitrary))
    assertEquals(recording.rate, Rate.Fixed(get(Hz(500.0))))
    assertEquals(recording.samplingTolerance.toSpan, Span.micros(2))
    assertEquals(
      recording.samples.map(_.t.toMicros).toVector,
      Vector(0L, 2000L, 4001L, 6000L, 8000L, 10000L, 12000L, 14000L)
    )
    assertEquals(
      recording.samples.map(_.gaze).toVector,
      Vector(
        Gaze.Tracked(Pt[Px](500.0, 500.0), Some(900.0)),
        Gaze.Tracked(Pt[Px](510.25, 500.0), Some(901.0)),
        Gaze.Blink[Px](),
        Gaze.Lost[Px](),
        Gaze.Tracked(Pt[Px](515.0, 502.0), None),
        Gaze.OffScreen[Px](Pt[Px](1200.0, -5.0)),
        Gaze.Tracked(Pt[Px](520.0, 500.0), Some(903.0)),
        Gaze.Tracked(Pt[Px](525.0, 505.0), Some(905.0))
      )
    )
    assertEquals(
      recording.samples.map(_.lineage.render).toVector,
      Vector(
        "Measured",
        "Measured>Smoothed",
        "Measured",
        "Measured",
        "Interpolated",
        "Measured",
        "Interpolated>Smoothed",
        "Measured"
      )
    )
    assertEquals(value.viewing.map(_.perspective.distance.toMm), Some(600.0))
    val sync = value.synchronization.get
    assertEquals(sync.target, ClockId("display"))
    assertEquals(sync.mode, SyncFitMode.OffsetOnly)
    assertEquals(sync.marks.map(_.id), Vector("t1", "t2", "t3", "t4"))
    assertEquals(sync.residualLimit.map(_.span), Some(Span.micros(2000)))
    val evidence = get(value.synchronize.get)
    assertEquals(evidence.rejectedMarks.map(_.mark.id), Vector("t4"))
    assertEquals(evidence.offset, Span.micros(1000010))
    assertEquals(evidence.sync.drift, 0.0)
    assertEquals(value.reference, input.reference)
    assertEquals(get(inputs.encode(value)), parse(recordingInputVersionOne))
  }

  test("frozen binocular-recording-input v1 keeps both eyes paired with a lost sample") {
    val value = get(inputs.parse(binocularInputVersionOne))
    assertEquals(value.reference.digest, "c5e4009b9cb153b0")
    val paired = value.channels match
      case RecordingChannels.Binocular(b) => b
      case other                          => fail(s"unexpected $other")
    assertEquals(paired.contentHash.render, "39e3dafb2de70ca2")
    assertEquals(paired.timestamps.toVector.map(_.toMicros), Vector(300000L, 300500L))
    assertEquals(
      paired.leftGaze.toVector,
      Vector(
        Gaze.Tracked(Pt[Px](1.0, 2.0), Some(3.0)),
        Gaze.Tracked(Pt[Px](4.5, 5.5), Some(6.5))
      )
    )
    assertEquals(
      paired.rightGaze.toVector,
      Vector(Gaze.Tracked(Pt[Px](4.0, 5.0), Some(6.0)), Gaze.Lost[Px]())
    )
    assertEquals(paired.rate, Rate.Fixed(get(Hz(2000.0))))
    assertEquals(paired.disparity.size, 1)
    assertEquals(value.viewing, None)
    assertEquals(value.synchronization, None)
    assertEquals(value.reference, binocularInput.reference)
    assertEquals(get(inputs.encode(value)), parse(binocularInputVersionOne))
  }

  test("frozen source-supported study-input v1 rebuilds summaries from its samples") {
    val value = get(studies.input.parse(sourceSupportedVersionOne))
    assertEquals(value.reference.digest, "9ac7e83e4874dec7")
    val path = value.trials.rows.head.value
    assertEquals(path.source, Some(RecordingRef("synthetic-right-headfixed-1000")))
    assertEquals(
      path.sampleSupport,
      Some(Vector(get(SampleRange.of(0, 5)), get(SampleRange.of(5, 10))))
    )
    assertEquals(path.sourceRecording.map(_.contentHash.render), Some("58960f21c98f4dbc"))
    assertEquals(path.sourceRecording.map(_.samples(2).gaze), Some(Gaze.Blink[Px]()))
    assertEquals(path.first.centre, sourceSupportedScanpath.first.centre)
    assertEquals(path.first.sampleCount, 4)
    assertEquals(path.first.dispersion.map(_.method), Some(DispersionMethod.RmsRadius))
    assertEquals(
      path.first.dispersion.map(_.value),
      sourceSupportedScanpath.first.dispersion.map(_.value)
    )
    assertEquals(path.last.centre, Pt[Px](300.0, 102.0))
    assertEquals(path.last.sampleCount, 5)
    assertEquals(path.last.dispersion, None)
    assertEquals(path.fixations.toVector, sourceSupportedScanpath.fixations.toVector)
    assertEquals(value.reference, sourceSupportedStudy.reference)
    assertEquals(get(studies.input.encode(value)), parse(sourceSupportedVersionOne))
  }

  test(
    "frozen temporal-study-input v1 fixes the digest, missing epoch, gap and extreme anchor"
  ) {
    val value = get(temporals.input.parse(temporalInputVersionOne))
    assertEquals(value.reference.digest, "def40de68e529acf")
    assertEquals(value.study.reference.digest, "4fd98f50693c79ad")
    assertEquals(value.study.trials.rows.size, 18)
    assertEquals(value.epochs.size, TemporalFixtures.coverage.size - 1)
    assert(!value.epochs.contains(missingEpochKey))
    assertEquals(value.epochs(shiftedKey).anchor.toMicros, 9007199254740993L)
    assertEquals(
      value
        .epochs(shiftedKey)
        .coverage
        .intervals
        .map(i => (i.onset.toMicros, i.offset.toMicros)),
      TemporalFixtures.coverage("s1/a/encode").map { case (a, b) => (a + shift, b + shift) }
    )
    assertEquals(
      value
        .epochs(StudyKey("s2", "b", "retest"))
        .coverage
        .intervals
        .map(i => (i.onset.toMicros, i.offset.toMicros)),
      Vector((0L, 350000L), (500000L, 930000L))
    )
    assertEquals(
      value.epochs(StudyKey("s2", "b", "retest")).coverage.clock,
      temporalClock(StudyKey("s2", "b", "retest"))
    )
    assertEquals(value.reference, temporal.reference)
    assertEquals(get(temporals.input.encode(value)), parse(temporalInputVersionOne))
  }

  test("v1 fixtures expose dropped samples and dropped epochs without roundtrip") {
    val recordingJson = parse(recordingInputVersionOne)
    val samples       = recordingJson.hcursor
      .downField("value")
      .downField("channels")
      .downField("recording")
      .downField("samples")
    val shortened = samples
      .withFocus(_.mapObject { fields =>
        fields
          .add("length", Json.fromInt(7))
          .add("tMicros", Json.arr(fields("tMicros").get.asArray.get.init*))
          .add("state", Json.arr(fields("state").get.asArray.get.init*))
          .add("x", Json.arr(fields("x").get.asArray.get.init*))
          .add("y", Json.arr(fields("y").get.asArray.get.init*))
          .add("pupil", Json.arr(fields("pupil").get.asArray.get.init*))
          .add("lineage", Json.arr(fields("lineage").get.asArray.get.init*))
      })
      .top
      .get
    inputs.decode(shortened) match
      case Left(
            CodecError.Entry(
              "channels.recording",
              CodecError.InputIdentity("2c826dc41ae25e67", other)
            )
          ) =>
        assertNotEquals(other, "2c826dc41ae25e67")
      case other => fail(s"unexpected $other")
    val temporalJson = parse(temporalInputVersionOne)
    val fewer        = temporalJson.hcursor
      .downField("value")
      .downField("epochs")
      .withFocus(epochs => Json.arr(epochs.asArray.get.tail*))
      .top
      .get
    temporals.input.decode(fewer) match
      case Left(CodecError.InputIdentity("def40de68e529acf", other)) =>
        assertNotEquals(other, "def40de68e529acf")
      case other => fail(s"unexpected $other")
  }
