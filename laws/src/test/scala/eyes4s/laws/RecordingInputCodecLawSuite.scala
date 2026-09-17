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

package eyes4s.laws

import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json
import org.scalacheck.{Gen, Test}

/** Published codec laws over generated recordings, recording inputs, temporal
  * inputs and timelines, with deliberate mutants that the laws must kill.
  */
class RecordingInputCodecLawSuite extends munit.DisciplineSuite:
  private def get[E, A](value: Either[E, A]): A       = value.fold(e => fail(s"$e"), identity)
  private def lift[E, A](value: Either[E, A]): Gen[A] =
    value.fold(_ => Gen.fail, Gen.const)

  private val display = get(Frame.screen("display", 1000, 1000))
  private val tracker = ClockId("tracker")

  /** Timestamps beyond JavaScript's exact integer range and at the Long floor. */
  private val origins: Gen[Long] =
    Gen.oneOf(0L, 1234567L, 9007199254740993L, Long.MinValue + 1)

  private def gaze(pupil: Boolean): Gen[Gaze[Px]] = Gen.frequency(
    5 -> (for
      x <- Gen.choose(0.0, 999.0)
      y <- Gen.choose(0.0, 999.0)
      p <- if pupil then Gen.option(Gen.choose(0.5, 5000.0)) else Gen.const(None)
    yield Gaze.Tracked(Pt[Px](x, y), p)),
    1 -> Gen.const(Gaze.Blink[Px]()),
    1 -> Gen.const(Gaze.Lost[Px]()),
    1 -> (for
      x <- Gen.choose(1000.5, 2000.0)
      y <- Gen.choose(-100.0, 999.0)
    yield Gaze.OffScreen[Px](Pt[Px](x, y)))
  )

  private val lineages: Gen[SampleLineage] = for
    basis <- Gen.oneOf(SampleLineage.measured, SampleLineage.interpolated)
    steps <- Gen
      .choose(0, 2)
      .flatMap(n => Gen.listOfN(n, Gen.oneOf(SampleOrigin.Smoothed, SampleOrigin.Projected)))
  yield steps.foldLeft(basis) { (acc, step) =>
    if step == SampleOrigin.Smoothed then acc.smoothed else acc.projected
  }

  private def timestamps(n: Int, fixed: Boolean): Gen[Vector[Long]] = for
    origin <- origins
    gaps   <- Gen.listOfN(n, if fixed then Gen.choose(-1L, 1L) else Gen.choose(1L, 5000L))
  yield
    if fixed then
      gaps.toVector.zipWithIndex.map { case (jitter, i) => origin + i * 1000L + jitter }
    else gaps.toVector.scanLeft(origin)(_ + _).tail

  private def samples(
      n: Int,
      fixed: Boolean,
      pupil: Boolean
  ): Gen[IArray[Sample[Px]]] = for
    times   <- timestamps(n, fixed)
    gazes   <- Gen.listOfN(n, gaze(pupil))
    lineage <- Gen.listOfN(n, lineages)
  yield IArray.from(
    times.indices.map(i => Sample(Instant.micros(times(i)), gazes(i), lineage(i)))
  )

  private val fixedRate = Rate.Fixed(get(Hz(1000.0)))

  private def recordings(minimum: Int): Gen[Recording[Px]] = for
    n     <- Gen.choose(minimum, 25)
    fixed <- Gen.oneOf(true, false)
    eye   <- Gen.oneOf(Eye.Left, Eye.Right, Eye.Cyclopean)
    unit  <- Gen.option(Gen.oneOf(PupilUnit.Area, PupilUnit.Diameter, PupilUnit.Arbitrary))
    rows  <- samples(n, fixed, unit.isDefined)
    value <- lift(
      Recording.of(
        display,
        tracker,
        if fixed then fixedRate else Rate.Irregular,
        eye,
        unit,
        rows,
        get(SamplingTolerance.of(Span.micros(2)))
      )
    )
  yield value

  private val binoculars: Gen[BinocularRecording[Px]] = for
    n     <- Gen.choose(1, 12)
    fixed <- Gen.oneOf(true, false)
    unit  <- Gen.option(Gen.oneOf(PupilUnit.Area, PupilUnit.Diameter, PupilUnit.Arbitrary))
    times <- timestamps(n, fixed)
    left  <- Gen.listOfN(n, gaze(unit.isDefined))
    right <- Gen.listOfN(n, gaze(unit.isDefined))
    value <- lift(
      BinocularRecording.of(
        display,
        tracker,
        if fixed then fixedRate else Rate.Irregular,
        unit,
        IArray.from(times.map(Instant.micros)),
        IArray.from(left),
        IArray.from(right),
        get(SamplingTolerance.of(Span.micros(2)))
      )
    )
  yield value

  private val viewings: Gen[Option[Viewing]] = Gen.option(
    for
      d <- Gen.choose(300.0, 1000.0)
      w <- Gen.choose(200.0, 800.0)
      h <- Gen.choose(200.0, 600.0)
      v <- lift(Viewing.millimetres(d, w, h))
    yield v
  )

  private def synchronizations(minimumMarks: Int): Gen[ObservedSynchronization] = for
    n      <- Gen.choose(minimumMarks, 5)
    mode   <- Gen.oneOf(SyncFitMode.OffsetOnly, SyncFitMode.Affine)
    offset <- Gen.choose(-5000000L, 5000000L)
    noise  <- Gen.listOfN(n, Gen.choose(0L, 40L))
    marks  <- lift(
      (0 until n).toVector.traverseEither(i =>
        SyncMark.of(
          s"m$i",
          Instant.micros(i * 1000000L),
          Instant.micros(i * 1000000L + offset + noise(i))
        )
      )
    )
    limit <- Gen.option(lift(SyncResidualLimit.of(Span.micros(1000))))
  yield ObservedSynchronization(ClockId("display"), mode, marks, limit)

  private def inputs(sync: Gen[Option[ObservedSynchronization]]): Gen[RecordingInput[Px]] =
    for
      channels <- Gen.oneOf(
        recordings(1).map(RecordingChannels.Monocular.apply),
        binoculars.map(RecordingChannels.Binocular.apply)
      )
      viewing <- viewings
      marks   <- sync
      value   <- lift(RecordingInput.of(RecordingRef("generated"), channels, viewing, marks))
    yield value

  private val sameRecording: (Recording[Px], Recording[Px]) => Boolean = (a, b) =>
    a.contentHash == b.contentHash && a.samples.toVector == b.samples.toVector &&
      a.samplingEvidence == b.samplingEvidence
  private val sameBinocular: (BinocularRecording[Px], BinocularRecording[Px]) => Boolean =
    (a, b) => RecordingChannels.binocularHash(a) == RecordingChannels.binocularHash(b)
  private val sameInput: (RecordingInput[Px], RecordingInput[Px]) => Boolean = (a, b) =>
    a.reference == b.reference && a.synchronization == b.synchronization

  checkAll(
    "recording",
    CodecLaws.roundTrip(RecordingInputCodecs.recording[Px], recordings(1), sameRecording)
  )
  checkAll(
    "binocular recording",
    CodecLaws.roundTrip(RecordingInputCodecs.binocular[Px], binoculars, sameBinocular)
  )
  checkAll(
    "recording input",
    CodecLaws.roundTrip(
      RecordingInputCodecs.input[Px],
      inputs(Gen.option(synchronizations(2))),
      sameInput
    )
  )

  // Temporal inputs over a fixed three-trial study with generated epochs.
  private val temporalFrame = get(Frame.screen("temporal", 2, 2))
  private val keys          = Vector(
    StudyKey("s1", "a", "encode"),
    StudyKey("s1", "a", "recall"),
    StudyKey("s2", "\"a\"", "encode")
  )
  private def clock(k: StudyKey): ClockId = ClockId(
    s"trial:${k.participant}/${k.stimulus}/${k.phase}"
  )
  private val study = StudyInput(
    Trials(keys.map { k =>
      val fixation = get(
        Event.Fixation.withoutDispersion(
          get(Interval.of(clock(k), Instant.micros(0), Instant.micros(1000))),
          Pt[Px](0.5, 0.5),
          1
        )
      )
      Trial(k, (), get(Scanpath.of(temporalFrame, clock(k), IArray(fixation))))
    })
  )

  private val temporals: Gen[TemporalStudyInput[StudyKey, Px]] = for
    chosen <- Gen.someOf(keys)
    epochs <- Gen.sequence[Vector[(StudyKey, TrialEpoch)], (StudyKey, TrialEpoch)](
      chosen.toVector.map { k =>
        for
          anchor <- origins
          gaps   <- Gen.choose(0, 3).flatMap(n => Gen.listOfN(n, Gen.choose(1L, 1000L)))
          widths <- Gen.listOfN(gaps.size, Gen.choose(1L, 1000L))
          intervals = gaps
            .zip(widths)
            .scanLeft((anchor, anchor)) { case ((_, end), (gap, width)) =>
              (end + gap, end + gap + width)
            }
            .tail
            .map { case (a, b) =>
              get(Interval.of(clock(k), Instant.micros(a), Instant.micros(b)))
            }
          coverage <- lift(ObservedCoverage.of(clock(k), intervals.toVector))
        yield k -> TrialEpoch(Instant.micros(anchor), coverage)
      }
    )
    value <- lift(TemporalStudyInput.of(study, epochs))
  yield value

  private val sameTemporal
      : (TemporalStudyInput[StudyKey, Px], TemporalStudyInput[StudyKey, Px]) => Boolean =
    (a, b) => a.reference == b.reference && a.epochs.keySet == b.epochs.keySet

  checkAll(
    "temporal input",
    CodecLaws.roundTrip(TemporalInputCodecs.study[Px]().input, temporals, sameTemporal)
  )

  private val messages = VersionedCodec.string(get(DefinitionId.of("message", 1)))
  private val timelines: Gen[Timeline[String]] = for
    n     <- Gen.choose(0, 6)
    marks <- Gen.listOfN(
      n,
      for
        at    <- Gen.oneOf(origins, Gen.choose(-1000000L, 1000000L))
        value <- Gen.oneOf("", "start", "\"quoted\"", "line\nbreak", "tab\there")
      yield Mark(Instant.micros(at), value)
    )
    line <- lift(Timeline.of(ClockId("display"), marks.toVector))
  yield line

  checkAll(
    "timeline",
    CodecLaws.roundTrip(
      TimelineCodecs.timeline(DefinitionId.timeline, messages),
      timelines,
      _ == _
    )
  )

  /** Wrap a codec so that decoding applies a deliberate change to the value. */
  private def mutant[A](codec: VersionedCodec[A])(
      change: A => Either[CodecError, A]
  ): VersionedCodec[A] =
    VersionedCodec.checked[A](codec.schema)(a =>
      codec
        .encode(a)
        .flatMap(j =>
          j.hcursor.get[Json]("value").left.map(e => CodecError.Field("value", j, e.message))
        )
    )(raw =>
      codec
        .decode(
          Json.obj(
            "schema" -> Json.obj(
              "name"    -> Json.fromString(codec.schema.name),
              "version" -> Json.fromInt(codec.schema.version)
            ),
            "value" -> raw
          )
        )
        .flatMap(change)
    )

  private def killed[A](codec: VersionedCodec[A], gen: Gen[A], eq: (A, A) => Boolean): Boolean =
    CodecLaws.roundTrip(codec, gen, eq).all.properties.exists { case (_, prop) =>
      !Test.check(Test.Parameters.default.withMinSuccessfulTests(40), prop).passed
    }

  test("published laws kill a dropped sample, a swapped clock and a dropped sync mark") {
    val dropped = mutant(RecordingInputCodecs.recording[Px]) { r =>
      Recording
        .of(r.frame, r.clock, r.rate, r.eye, r.pupilUnit, r.samples.init, r.samplingTolerance)
        .left
        .map(CodecError.Recording("mutant", _))
    }
    assert(killed(dropped, recordings(2), sameRecording))

    val swapped = mutant(RecordingInputCodecs.recording[Px]) { r =>
      Recording
        .of(
          r.frame,
          ClockId(s"${r.clock.name}-swapped"),
          r.rate,
          r.eye,
          r.pupilUnit,
          r.samples,
          r.samplingTolerance
        )
        .left
        .map(CodecError.Recording("mutant", _))
    }
    assert(killed(swapped, recordings(1), sameRecording))

    val fewerMarks = mutant(RecordingInputCodecs.input[Px]) { value =>
      val sync = value.synchronization.get
      RecordingInput
        .of(
          value.source,
          value.channels,
          value.viewing,
          Some(sync.copy(marks = sync.marks.init, residualLimit = None))
        )
        .left
        .map(CodecError.Input.apply)
    }
    assert(killed(fewerMarks, inputs(synchronizations(3).map(Some(_))), sameInput))

    val movedAnchor = mutant(TemporalInputCodecs.study[Px]().input) { value =>
      TemporalStudyInput
        .of(
          value.study,
          value.epochs.toVector.map { case (k, epoch) =>
            k -> epoch.copy(anchor = Instant.micros(epoch.anchor.toMicros + 1))
          }
        )
        .left
        .map(CodecError.Temporal.apply)
    }
    assert(killed(movedAnchor, temporals.suchThat(_.epochs.nonEmpty), sameTemporal))
  }

  extension [E, A](values: Vector[A])
    private def traverseEither[B](f: A => Either[E, B]): Either[E, Vector[B]] =
      values.foldLeft[Either[E, Vector[B]]](Right(Vector.empty)) { (acc, a) =>
        for
          built <- acc
          b     <- f(a)
        yield built :+ b
      }
