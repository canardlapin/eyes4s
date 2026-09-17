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
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy
import io.circe.{ACursor, Json}

/** Recording and temporal input payloads reproduce their scientific inputs
  * exactly, and refuse mis-shaped or re-identified payloads before execution.
  */
class InputPayloadCodecSuite extends munit.FunSuite:
  import InputPayloadFixtures.*

  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val recordings                        = RecordingInputCodecs.recording[Px]
  private val binoculars                        = RecordingInputCodecs.binocular[Px]
  private val inputs                            = RecordingInputCodecs.input[Px]
  private val studies                           = StudyInputCodecs.study[Px]
  private val temporals                         = TemporalInputCodecs.study[Px]()

  /** Edit the JSON at a field path, for no-roundtrip mutation tests. */
  private def edit(json: Json, path: String*)(f: Json => Json): Json =
    path.foldLeft(json.hcursor: ACursor)(_.downField(_)).withFocus(f).top.get

  private def channel(json: Json, path: String*)(f: Json => Json): Json =
    edit(json, (Vector("value", "channels", "recording") ++ path)*)(f)

  private def dropAt(index: Int)(json: Json): Json =
    Json.arr(json.asArray.get.patch(index, Nil, 1)*)

  private def setAt(index: Int, value: Json)(json: Json): Json =
    Json.arr(json.asArray.get.updated(index, value)*)

  private def coverageOf(epoch: TrialEpoch): (String, Vector[(Long, Long)]) =
    epoch.coverage.clock.name -> epoch.coverage.intervals.map(i =>
      i.onset.toMicros -> i.offset.toMicros
    )

  test("irregular recordings round-trip missing gaze and derived samples with exact identity") {
    val samples = IArray(
      Sample(Instant.micros(Long.MinValue + 1), Gaze.Tracked(Pt[Px](1.0, 1.0), None)),
      Sample(Instant.micros(Long.MinValue + 37), Gaze.Lost[Px]()),
      Sample(Instant.micros(Long.MinValue + 40), Gaze.Blink[Px]()),
      Sample(
        Instant.micros(Long.MinValue + 900),
        Gaze.Tracked(Pt[Px](2.5, 3.5), None),
        SampleLineage.interpolated.smoothed.projected
      ),
      Sample(Instant.micros(9007199254740993L), Gaze.OffScreen[Px](Pt[Px](-1.0, 0.0)))
    )
    val original = get(
      Recording.of(display, tracker, Rate.Irregular, Eye.Cyclopean, None, samples)
    )
    val decoded = get(recordings.decode(get(recordings.encode(original))))
    assertEquals(decoded.contentHash, original.contentHash)
    assertEquals(decoded.samples.toVector, original.samples.toVector)
    assertEquals(decoded.samplingEvidence, original.samplingEvidence)
    assertEquals(decoded.medianInterval, original.medianInterval)
    assertEquals(decoded.rate, Rate.Irregular)
    assertEquals(
      decoded.samples(3).lineage.toVector.map(_.toString),
      Vector("Interpolated", "Smoothed", "Projected")
    )
    assertEquals(decoded.trackedRatio, 0.4)
    val paired = get(binoculars.decode(get(binoculars.encode(binocular))))
    assertEquals(paired.left.contentHash, binocular.left.contentHash)
    assertEquals(paired.right.contentHash, binocular.right.contentHash)
    assertEquals(paired.disparity, binocular.disparity)
    assertEquals(paired.rightGaze(1), Gaze.Lost[Px]())
  }

  test("recording input round-trips geometry and synchronization; plan outputs agree") {
    val decoded = get(inputs.parse(get(inputs.encode(input)).noSpaces))
    assertEquals(decoded.reference, input.reference)
    assertEquals(decoded.viewing, input.viewing)
    assertEquals(decoded.synchronization, input.synchronization)
    val evidence = get(decoded.synchronize.get)
    assertEquals(evidence.rejectedMarks.map(_.mark.id), Vector("t4"))
    assertEquals(evidence.offset, get(input.synchronize.get).offset)
    assertEquals(evidence.sync.drift, get(input.synchronize.get).sync.drift)
    val recording = decoded.monocular.get
    assertEquals(recording.contentHash, monocular.contentHash)
    assertEquals(recording.samplingTolerance.toSpan, Span.micros(2))
    recording.samplingEvidence match
      case fixed: SamplingEvidence.Fixed => assertEquals(fixed.maximumDeviation, Span.micros(1))
      case other                         => fail(s"unexpected $other")

    val method = RecordingMethod.ivt(get(DefinitionId.of("ivt", 1)))
    val plan   = get(
      RecordingPlan.of(
        ArtifactRef.of[Recording[Px]](monocular.contentHash),
        input.source,
        display,
        tracker,
        ClockId("display"),
        FrameId("angular"),
        Some(viewing),
        synchronization.mode,
        synchronization.marks,
        synchronization.residualLimit,
        InterpolationGap.none,
        Vector(get(RecordingArea.of("image", "Image", get(Bounds.of[Px](0, 0, 800, 600))))),
        method,
        IvtParameters(
          get(IvtThreshold.of(get(Velocity.perSecond[Deg](30)))),
          get(MinimumEventDuration.of(Span.micros(2000)))
        )
      )
    )
    assertEquals(plan.prerequisites(Some(recording)), Vector.empty)
    val direct  = get(plan.run(monocular))
    val rebuilt = get(plan.run(recording))
    assertEquals(rebuilt.prepared.contentHash, direct.prepared.contentHash)
    assertEquals(rebuilt.angular.contentHash, direct.angular.contentHash)
    assertEquals(rebuilt.detection.eventSeries.events, direct.detection.eventSeries.events)
    assertEquals(rebuilt.detection.eventSeries.support, direct.detection.eventSeries.support)
    assertEquals(rebuilt.detection.provenance, direct.detection.provenance)
    assertEquals(rebuilt.synchronization.offset, direct.synchronization.offset)

    val paired = get(inputs.decode(get(inputs.encode(binocularInput))))
    assertEquals(paired.reference, binocularInput.reference)
    assertEquals(paired.monocular, None)
    assertEquals(paired.channels.contentHash, binocularInput.channels.contentHash)

    assertEquals(RecordingInput.disagreements(decoded, plan), Vector.empty)
    val extraMark = get(
      RecordingInput.of(
        input.source,
        input.channels,
        input.viewing,
        Some(
          synchronization.copy(marks =
            synchronization.marks :+ get(
              SyncMark.of("t5", Instant.micros(40000000), Instant.micros(41000010))
            )
          )
        )
      )
    )
    val withExtra = get(inputs.decode(get(inputs.encode(extraMark))))
    assertNotEquals(withExtra.reference, input.reference)
    RecordingInput.disagreements(withExtra, plan) match
      case Vector(RecordingInputError.PlanDisagreement("marks", declared, evidence)) =>
        assert(evidence.endsWith("t5@40000000->41000010"))
        assert(!declared.contains("t5"))
      case other => fail(s"unexpected $other")
    val otherViewing = get(
      RecordingInput.of(
        input.source,
        input.channels,
        Some(get(Viewing.millimetres(650, 500, 500))),
        input.synchronization
      )
    )
    assertEquals(
      RecordingInput.disagreements(otherViewing, plan).map {
        case RecordingInputError.PlanDisagreement(field, _, _) => field
        case other                                             => other.toString
      },
      Vector("viewing")
    )
    RecordingInput.disagreements(paired, plan) match
      case Vector(
            RecordingInputError.PlanDisagreement("source", _, _),
            RecordingInputError.PlanDisagreement("analysisClock", _, "none"),
            RecordingInputError.PlanDisagreement("viewing", _, "none"),
            RecordingInputError.PlanDisagreement("synchronizationModel", _, "none"),
            RecordingInputError.PlanDisagreement("marks", _, ""),
            RecordingInputError.PlanDisagreement("residualLimit", _, "none"),
            RecordingInputError.BinocularChannels(_)
          ) =>
        ()
      case other => fail(s"unexpected $other")
  }

  test("source-supported scanpaths carry their recording and sample ranges") {
    val decoded = get(studies.input.decode(get(studies.input.encode(sourceSupportedStudy))))
    val path    = decoded.trials.rows.head.value
    assertEquals(decoded.reference, sourceSupportedStudy.reference)
    assertEquals(path.source, Some(RecordingRef("synthetic-right-headfixed-1000")))
    assertEquals(path.sampleSupport, sourceSupportedScanpath.sampleSupport)
    assertEquals(path.sourceRecording.map(_.contentHash), Some(sourceRecording.contentHash))
    assertEquals(path.fixations.toVector, sourceSupportedScanpath.fixations.toVector)
    assertEquals(path.first.sampleCount, 4)
    assertEquals(path.last.sampleCount, 5)
    path.first.dispersionStatus match
      case DispersionStatus.Available(spread, SummaryEvidence.SourceSupported(_, range)) =>
        assertEquals(range, get(SampleRange.of(0, 5)))
        assertEquals(spread.method, DispersionMethod.RmsRadius)
        assert(spread.value > 0.0)
      case other => fail(s"unexpected $other")
    val json   = get(studies.input.encode(sourceSupportedStudy))
    val spread = json.hcursor
      .downField("value")
      .downField("trials")
      .downField("value")
      .downArray
      .downField("value")
      .downField("value")
      .downField("fixations")
      .downArray
      .get[Json]("dispersion")
    assertEquals(get(spread), Json.obj("method" -> Json.fromString("rmsRadius")))
    val detachedStudy = StudyInput(
      Trials(
        Vector(
          Trial(
            StudyKey("s1", "a", "encode"),
            (),
            get(Scanpath.of(display, trialClock, sourceSupportedScanpath.fixations))
          )
        )
      )
    )
    assertNotEquals(detachedStudy.reference, sourceSupportedStudy.reference)
    studies.input.encode(detachedStudy) match
      case Left(
            CodecError.Entry(
              "trials.rows[0].fixations[0]",
              CodecError.Unsupported("dispersion", _)
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")
    val moved = get(
      sourceSupportedScanpath.warp(
        get(Warp.rescale(display, get(Frame.screen("half", 500, 500))))
      )
    )
    val recomputed = StudyInput(Trials(Vector(Trial(StudyKey("s1", "a", "encode"), (), moved))))
    studies.input.encode(recomputed) match
      case Left(
            CodecError.Entry(
              "trials.rows[0].fixations[0]",
              CodecError.Unsupported("dispersion", _)
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")
  }

  test("source-supported summaries cannot be detached from their samples") {
    val json                       = get(studies.input.encode(sourceSupportedStudy))
    def row(f: Json => Json): Json =
      edit(json, "value", "trials", "value")(rows =>
        Json.arr(edit(rows.asArray.get.head, "value", "value")(f))
      )
    val detached = row(scan =>
      edit(scan, "fixations")(
        setAt(
          0,
          edit(scan.hcursor.get[Json]("fixations").toOption.get.asArray.get.head, "x")(_ =>
            Json.fromDoubleOrNull(150.0)
          )
        )
      )
    )
    studies.input.decode(detached) match
      case Left(
            CodecError.Entry("trials.rows[0].source", CodecError.Field("fixations[0]", _, _))
          ) =>
        ()
      case other => fail(s"unexpected $other")
    val valued = row(scan =>
      edit(scan, "fixations")(fixes =>
        setAt(
          0,
          edit(fixes.asArray.get.head, "dispersion")(
            _.mapObject(_.add("value", Json.fromDoubleOrNull(1.0)))
          )
        )(fixes)
      )
    )
    studies.input.decode(valued) match
      case Left(
            CodecError.Entry("trials.rows[0].fixations[0]", CodecError.Field("value", _, _))
          ) =>
        ()
      case other => fail(s"unexpected $other")
    val unnamed = row(scan => edit(scan, "source", "ref")(_ => Json.fromString(" ")))
    studies.input.decode(unnamed) match
      case Left(CodecError.Entry("trials.rows[0].source", CodecError.Field("ref", _, _))) => ()
      case other => fail(s"unexpected $other")
    val overlapping = row(scan =>
      edit(scan, "source", "support")(
        setAt(1, Json.obj("from" -> Json.fromInt(4), "until" -> Json.fromInt(10)))
      )
    )
    overlapping.pipe(studies.input.decode) match
      case Left(
            CodecError.Entry(
              "trials.rows[0].source",
              CodecError.Support(
                "support",
                DetectionSupportError.OverlappingSampleRanges(_, 1, _, _)
              )
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")
    val truncated = row(scan => edit(scan, "source", "support")(dropAt(1)))
    truncated.pipe(studies.input.decode) match
      case Left(
            CodecError.Entry(
              "trials.rows[0].source",
              CodecError.Support(
                "support",
                DetectionSupportError.EventSupportCountMismatch(_, 2, 1)
              )
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")
    val foreignClock =
      row(scan => edit(scan, "source", "recording", "clock")(_ => Json.fromString("other")))
    foreignClock.pipe(studies.input.decode) match
      case Left(
            CodecError.Entry(
              "trials.rows[0].source.recording",
              CodecError.MissingIdentity("clock", "other")
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")
    // Not "just a distinct blink": changing the support category of a sample
    // is a different recording, so the declared recording digest no longer holds.
    val flipped = row(scan =>
      edit(scan, "source", "recording", "samples", "state")(setAt(2, Json.fromString("lost")))
    )
    flipped.pipe(studies.input.decode) match
      case Left(
            CodecError.Entry(
              "trials.rows[0].source.recording",
              CodecError.InputIdentity(declared, other)
            )
          ) =>
        assertEquals(declared, sourceRecording.contentHash.render)
        assertNotEquals(other, declared)
      case other => fail(s"unexpected $other")
  }

  test("temporal input round-trips missing epochs, gapped coverage and extreme anchors") {
    val decoded = get(temporals.input.parse(get(temporals.input.encode(temporal)).noSpaces))
    assertEquals(decoded.reference, temporal.reference)
    assertEquals(decoded.study.reference, temporalStudy.reference)
    assertEquals(decoded.epochs.keySet, temporal.epochs.keySet)
    assert(!decoded.epochs.contains(missingEpochKey))
    assert(temporalStudy.trials.rows.exists(_.key == missingEpochKey))
    assertEquals(decoded.epochs(shiftedKey).anchor.toMicros, 9007199254740993L)
    assertEquals(
      decoded.epochs.view.mapValues(coverageOf).toMap,
      temporal.epochs.view.mapValues(coverageOf).toMap
    )
    assertEquals(decoded.epochs(StudyKey("s2", "b", "retest")).coverage.intervals.size, 2)
    val extreme = get(
      TemporalStudyInput.of(
        temporalStudy,
        temporalEpochs(complete = true).map { case (k, epoch) =>
          if k == StudyKey("s1", "b", "encode") then
            k -> TrialEpoch(
              Instant.micros(Long.MinValue + 1),
              get(
                ObservedCoverage.of(
                  temporalClock(k),
                  Vector(
                    get(
                      Interval.of(
                        temporalClock(k),
                        Instant.micros(Long.MinValue + 1),
                        Instant.micros(Long.MaxValue)
                      )
                    )
                  )
                )
              )
            )
          else k -> epoch
        }
      )
    )
    val rebuilt = get(temporals.input.parse(get(temporals.input.encode(extreme)).noSpaces))
    assertEquals(rebuilt.reference, extreme.reference)
    assertEquals(
      rebuilt.epochs(StudyKey("s1", "b", "encode")).anchor.toMicros,
      Long.MinValue + 1
    )
  }

  test("temporal plans produce identical cells from the original and decoded inputs") {
    val decoded = get(temporals.input.decode(get(temporals.input.encode(temporalComplete))))
    val grid    = get(Grid.over(temporalFrame, 2, 2))
    val base    = get(
      StudyPlan.cosine(
        temporalStudy.reference,
        grid,
        "recall",
        "encode",
        Weight.Duration,
        Vector(
          StudyEstimate.Binned(),
          StudyEstimate.Gaussian(get(Sigma.px(1)), EdgePolicy.Truncate)
        ),
        FailurePolicy.RequireAll
      )
    )
    val windows = TemporalFixtures.windows.map { case (name, a, b) =>
      get(StudyWindow.of(name, get(Window.of(Span.micros(a), Span.micros(b)))))
    }
    val repeats = TemporalFixtures.repetitions.map { case (name, f, r) =>
      get(RepetitionContrast.withinParticipant(name, f, r))
    }
    val plan = get(
      TemporalStudyPlan.of(
        base,
        temporalComplete.reference,
        windows,
        repeats,
        FixationBoundary.ClipDuration
      )
    )
    assertEquals(plan.prerequisites(Some(decoded)), Vector.empty)
    def cells(value: TemporalStudyInput[StudyKey, Px]) =
      get(plan.run(value)).cells.map { cell =>
        (
          cell.repetition.name,
          cell.window.name,
          cell.occupancy.map { case (k, o) =>
            k -> o.toOption.map(v => (v.observedMicros, v.missingMicros, v.excludedFixations))
          },
          cell.result.scales.map(scale =>
            scale.estimate.name -> get(scale.contrast).rows.map(row =>
              row.key -> row.difference.toOption.map(_.value)
            )
          )
        )
      }
    val direct  = cells(temporalComplete)
    val rebuilt = cells(decoded)
    assertEquals(direct.size, 8)
    assertEquals(rebuilt, direct)
    val outside = direct.filter(_._2 == "outside")
    assert(outside.nonEmpty)
    outside.foreach { case (_, _, occupancy, _) =>
      occupancy.foreach { case (_, value) => assertEquals(value.map(_._1), Some(0L)) }
    }
    // The gapped s2/b/retest coverage straddles fixations: clipped duration
    // retains only the covered part and reports the gap as missing.
    val gapped = direct.collect { case (_, "middle", occupancy, _) =>
      occupancy.collectFirst { case (StudyKey("s2", "b", "retest"), Some(value)) => value }
    }.flatten
    assert(gapped.nonEmpty)
    gapped.foreach { case (observed, missing, _) =>
      assert(observed > 0L)
      assert(missing > 0L)
    }
  }

  test("by-reference temporal payloads resolve the base study or name the missing artifact") {
    val byReference = TemporalInputCodecs.study[Px](
      StudyEmbedding.ByReference,
      ref => Option.when(ref == temporalStudy.reference)(temporalStudy)
    )
    val json = get(byReference.input.encode(temporal))
    assertEquals(
      get(json.hcursor.downField("value").downField("study").get[String]("kind")),
      "reference"
    )
    assertEquals(byReference.input.decode(json).map(_.reference), Right(temporal.reference))
    temporals.input.decode(json) match
      case Left(
            CodecError.Entry("study", CodecError.Definition(PlanError.MissingArtifact(digest)))
          ) =>
        assertEquals(digest, temporalStudy.reference.digest)
      case other => fail(s"unexpected $other")
    val wrong =
      TemporalInputCodecs.study[Px](StudyEmbedding.ByReference, _ => Some(sourceSupportedStudy))
    wrong.input.decode(json) match
      case Left(CodecError.Entry("study", CodecError.InputIdentity(declared, actual))) =>
        assertEquals(declared, temporalStudy.reference.digest)
        assertEquals(actual, sourceSupportedStudy.reference.digest)
      case other => fail(s"unexpected $other")
  }

  test(
    "duplicate, foreign and clock-inconsistent epochs are refused before decoding completes"
  ) {
    val json       = get(temporals.input.encode(temporal))
    val epochs     = json.hcursor.downField("value").get[Vector[Json]]("epochs").toOption.get
    val duplicated = edit(json, "value", "epochs")(_ => Json.arr((epochs :+ epochs.head)*))
    temporals.input.decode(duplicated) match
      case Left(CodecError.Temporal(TemporalStudyError.DuplicateEpochs(digests))) =>
        assertEquals(digests.size, 1)
      case other => fail(s"unexpected $other")
    val foreign = edit(json, "value", "epochs")(
      setAt(0, edit(epochs.head, "key", "value", "participant")(_ => Json.fromString("nobody")))
    )
    temporals.input.decode(foreign) match
      case Left(CodecError.Temporal(TemporalStudyError.UnknownEpochs(digests))) =>
        assertEquals(digests.size, 1)
      case other => fail(s"unexpected $other")
    val otherClock = edit(json, "value", "epochs")(
      setAt(
        0,
        edit(epochs.head, "coverage", "intervals")(intervals =>
          setAt(0, edit(intervals.asArray.get.head, "clock")(_ => Json.fromString("other")))(
            intervals
          )
        )
      )
    )
    temporals.input.decode(otherClock) match
      case Left(CodecError.Entry("epochs[0]", CodecError.Field("coverage", _, _))) => ()
      case other => fail(s"unexpected $other")
    val trialClocks = json.hcursor
      .downField("value")
      .downField("identities")
      .get[Vector[String]]("clocks")
      .toOption
      .get
    val swappedClock = edit(json, "value", "epochs")(
      setAt(
        0,
        edit(epochs.head, "coverage")(coverage =>
          coverage.mapObject(fields =>
            fields
              .add("clock", Json.fromString(trialClocks(1)))
              .add(
                "intervals",
                Json.arr(
                  fields("intervals").get.asArray.get
                    .map(i => edit(i, "clock")(_ => Json.fromString(trialClocks(1))))*
                )
              )
          )
        )
      )
    )
    temporals.input.decode(swappedClock) match
      case Left(CodecError.Entry("epochs[0]", CodecError.Field("coverage", _, reason))) =>
        assert(reason.contains("trial scanpath is on clock"))
      case other => fail(s"unexpected $other")
    val unknownClock = edit(json, "value", "epochs")(
      setAt(0, edit(epochs.head, "coverage", "clock")(_ => Json.fromString("other")))
    )
    temporals.input.decode(unknownClock) match
      case Left(CodecError.Entry("epochs[0]", CodecError.MissingIdentity("clock", "other"))) =>
        ()
      case other => fail(s"unexpected $other")
    val shiftedCoverage = edit(json, "value", "epochs")(
      setAt(
        0,
        edit(epochs.head, "coverage", "intervals")(intervals =>
          setAt(
            0,
            edit(intervals.asArray.get.head, "offsetMicros")(offset =>
              Json.fromString((offset.asString.get.toLong + 1).toString)
            )
          )(intervals)
        )
      )
    )
    temporals.input.decode(shiftedCoverage) match
      case Left(CodecError.InputIdentity(declared, _)) =>
        assertEquals(declared, temporal.reference.digest)
      case other => fail(s"unexpected $other")
    val moved = edit(json, "value", "epochs")(
      setAt(0, edit(epochs.head, "anchorMicros")(_ => Json.fromString("1")))
    )
    temporals.input.decode(moved) match
      case Left(CodecError.InputIdentity(declared, _)) =>
        assertEquals(declared, temporal.reference.digest)
      case other => fail(s"unexpected $other")
    val numeric = edit(json, "value", "epochs")(
      setAt(0, edit(epochs.head, "anchorMicros")(_ => Json.fromLong(9007199254740993L)))
    )
    temporals.input.decode(numeric) match
      case Left(CodecError.Entry("epochs[0]", CodecError.Field("anchorMicros", _, _))) => ()
      case other => fail(s"unexpected $other")
    val degrees = edit(json, "value", "unit")(_ => Json.fromString("deg"))
    temporals.input.decode(degrees) match
      case Left(CodecError.Field("unit", _, _)) => ()
      case other                                => fail(s"unexpected $other")
  }

  test(
    "mis-shaped sample buffers, wrong units and swapped clocks are refused with located errors"
  ) {
    val json = get(inputs.encode(input))
    channel(json, "samples", "x")(dropAt(7)).pipe(inputs.decode) match
      case Left(
            CodecError.Entry("channels.recording", CodecError.Field("samples.x", _, reason))
          ) =>
        assert(reason.contains("7 entries, expected length 8"))
      case other => fail(s"unexpected $other")
    channel(json, "samples", "x")(setAt(0, Json.Null)).pipe(inputs.decode) match
      case Left(
            CodecError.Entry("channels.recording.samples[0]", CodecError.Field("x", _, _))
          ) =>
        ()
      case other => fail(s"unexpected $other")
    channel(json, "samples", "state")(setAt(0, Json.fromString("missing")))
      .pipe(inputs.decode) match
      case Left(
            CodecError.Entry("channels.recording.samples[0]", CodecError.Field("state", _, _))
          ) =>
        ()
      case other => fail(s"unexpected $other")
    channel(json, "samples", "lineage")(setAt(0, Json.fromString("smoothed")))
      .pipe(inputs.decode) match
      case Left(
            CodecError.Entry("channels.recording.samples[0]", CodecError.Field("lineage", _, _))
          ) =>
        ()
      case other => fail(s"unexpected $other")
    channel(json, "samples", "tMicros")(setAt(1, Json.fromLong(2000L)))
      .pipe(inputs.decode) match
      case Left(
            CodecError.Entry("channels.recording", CodecError.Field("samples.tMicros[1]", _, _))
          ) =>
        ()
      case other => fail(s"unexpected $other")
    channel(json, "samples", "tMicros")(setAt(1, Json.fromString("0")))
      .pipe(inputs.decode) match
      case Left(
            CodecError.Entry(
              "channels.recording",
              CodecError.Recording("samples", RecordingError.NonMonotonic(1, 0L, 0L))
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")
    channel(json, "samples", "length")(_ =>
      Json.fromInt(RecordingInputCodecs.maximumSamples + 1)
    ).pipe(inputs.decode) match
      case Left(
            CodecError.Entry(
              "channels.recording",
              CodecError.SampleBound("samples", declared, maximum)
            )
          ) =>
        assertEquals(declared, RecordingInputCodecs.maximumSamples + 1)
        assertEquals(maximum, RecordingInputCodecs.maximumSamples)
      case other => fail(s"unexpected $other")
    channel(json, "pupilUnit")(_ => Json.Null).pipe(inputs.decode) match
      case Left(
            CodecError.Entry(
              "channels.recording",
              CodecError.Recording("samples", RecordingError.UndeclaredPupilUnit(_, 0, 900.0))
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")
    channel(json, "samplingToleranceMicros")(_ => Json.fromString("0"))
      .pipe(inputs.decode) match
      case Left(
            CodecError.Entry(
              "channels.recording",
              CodecError.Recording(
                "samples",
                RecordingError.FixedRateMismatch(2, _, _, _, _, _)
              )
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")
    edit(json, "value", "unit")(_ => Json.fromString("deg")).pipe(inputs.decode) match
      case Left(CodecError.Field("unit", _, _)) => ()
      case other                                => fail(s"unexpected $other")
    RecordingInputCodecs.input[Deg].decode(json) match
      case Left(CodecError.Field("unit", _, _)) => ()
      case other                                => fail(s"unexpected $other")
    channel(json, "clock")(_ => Json.fromString("display")).pipe(inputs.decode) match
      case Left(
            CodecError.Entry("channels.recording", CodecError.InputIdentity(declared, _))
          ) =>
        assertEquals(declared, monocular.contentHash.render)
      case other => fail(s"unexpected $other")
    channel(json, "clock")(_ => Json.fromString("elsewhere")).pipe(inputs.decode) match
      case Left(
            CodecError.Entry(
              "channels.recording",
              CodecError.MissingIdentity("clock", "elsewhere")
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")
  }

  test("missing-support flags, lineage and synchronization marks are identity-bearing") {
    val json = get(inputs.encode(input))
    channel(json, "samples", "state")(setAt(2, Json.fromString("lost")))
      .pipe(inputs.decode) match
      case Left(
            CodecError.Entry("channels.recording", CodecError.InputIdentity(declared, other))
          ) =>
        assertEquals(declared, monocular.contentHash.render)
        assertNotEquals(other, declared)
      case other => fail(s"unexpected $other")
    channel(json, "samples", "lineage")(setAt(4, Json.fromString("measured")))
      .pipe(inputs.decode) match
      case Left(CodecError.Entry("channels.recording", CodecError.InputIdentity(_, _))) => ()
      case other => fail(s"unexpected $other")
    edit(json, "value", "synchronization", "marks")(dropAt(3)).pipe(inputs.decode) match
      case Left(CodecError.InputIdentity(declared, _)) =>
        assertEquals(declared, input.reference.digest)
      case other => fail(s"unexpected $other")
    edit(json, "value", "synchronization", "fitted", "offsetMicros")(_ =>
      Json.fromString("1000001")
    ).pipe(inputs.decode) match
      case Left(
            CodecError.SynchronizationFit(
              "synchronization.fitted",
              1000001L,
              0.0,
              1000010L,
              0.0
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")
    edit(json, "value", "synchronization", "fitted", "drift")(_ =>
      Json.fromDoubleOrNull(1.0e-6)
    )
      .pipe(inputs.decode) match
      case Left(
            CodecError.SynchronizationFit(
              "synchronization.fitted",
              1000010L,
              1.0e-6,
              1000010L,
              0.0
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")
    val pairedJson = get(inputs.encode(binocularInput))
    edit(pairedJson, "value", "channels", "recording", "samples", "left", "x")(dropAt(1))
      .pipe(inputs.decode) match
      case Left(
            CodecError.Entry("channels.recording", CodecError.Field("samples.x", _, reason))
          ) =>
        assert(reason.contains("1 entries, expected length 2"))
      case other => fail(s"unexpected $other")
    edit(pairedJson, "value", "channels", "recording", "samples", "tMicros")(
      setAt(1, Json.fromString("300000"))
    ).pipe(inputs.decode) match
      case Left(
            CodecError.Entry(
              "channels.recording",
              CodecError.Recording("samples", RecordingError.NonMonotonic(1, _, _))
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")
    edit(pairedJson, "value", "channels", "recording", "samples", "right", "state")(
      setAt(1, Json.fromString("blink"))
    ).pipe(inputs.decode) match
      case Left(
            CodecError.Entry("channels.recording", CodecError.InputIdentity(declared, _))
          ) =>
        assertEquals(declared, binocular.contentHash.render)
      case other => fail(s"unexpected $other")
    edit(json, "value", "synchronization", "target")(_ => Json.fromString("tracker"))
      .pipe(inputs.decode) match
      case Left(
            CodecError.Input(
              RecordingInputError.SynchronizationTargetIsSource(ClockId("tracker"))
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")
    edit(json, "value", "viewing", "distanceMm")(_ => Json.fromDoubleOrNull(650.0))
      .pipe(inputs.decode) match
      case Left(CodecError.InputIdentity(_, _)) => ()
      case other                                => fail(s"unexpected $other")
    edit(json, "value", "input")(_ => Json.fromString("0000000000000000"))
      .pipe(inputs.decode) match
      case Left(CodecError.InputIdentity("0000000000000000", actual)) =>
        assertEquals(actual, input.reference.digest)
      case other => fail(s"unexpected $other")
  }

  test("timelines round-trip with equal instants in input order") {
    val values = VersionedCodec.string(get(DefinitionId.of("message", 1)))
    val codec  = TimelineCodecs.timeline(DefinitionId.timeline, values)
    val line   = get(
      Timeline.of(
        ClockId("display"),
        Vector(
          Mark(Instant.micros(5), "later"),
          Mark(Instant.micros(Long.MinValue + 1), "first"),
          Mark(Instant.micros(5), "later again"),
          Mark(Instant.micros(9007199254740993L), "last")
        )
      )
    )
    val decoded = get(codec.parse(get(codec.encode(line)).noSpaces))
    assertEquals(decoded, line)
    assertEquals(decoded.marks.map(_.value), Vector("first", "later", "later again", "last"))
    val planned  = TimelineCodecs.planned(DefinitionId.timeline, values)
    val observed = TimelineCodecs.observed(DefinitionId.timeline, values)
    assertEquals(
      get(planned.decode(get(planned.encode(PlannedTimeline.from(line))))),
      PlannedTimeline.from(line)
    )
    assertEquals(
      get(observed.decode(get(observed.encode(ObservedTimeline.from(line))))),
      ObservedTimeline.from(line)
    )
    observed.decode(get(planned.encode(PlannedTimeline.from(line)))) match
      case Left(CodecError.Field("timing", _, _)) => ()
      case other                                  => fail(s"unexpected $other")
    codec.decode(get(planned.encode(PlannedTimeline.from(line)))) match
      case Left(CodecError.Field("timing", _, reason)) => assert(reason.contains("planned"))
      case other                                       => fail(s"unexpected $other")
    val json = get(codec.encode(line))
    edit(json, "value", "marks")(
      setAt(1, Json.obj("atMicros" -> Json.fromString("x"), "value" -> Json.Null))
    ).pipe(codec.decode) match
      case Left(CodecError.Entry("marks[1]", CodecError.Field("atMicros", _, _))) => ()
      case other => fail(s"unexpected $other")
    assertEquals(get(codec.decode(edit(json, "value", "marks")(_ => Json.arr()))).size, 0)
  }

  extension [A](value: A) private def pipe[B](f: A => B): B = f(value)
