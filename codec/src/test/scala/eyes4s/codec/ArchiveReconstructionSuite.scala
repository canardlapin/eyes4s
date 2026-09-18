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
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg
import eyes4s.plan.*

/** The checked reconstruction APIs behind the recording and temporal
  * archives, exercised directly: a completed result rebuilds from its parts,
  * and each re-derived stage, layout, context and ledger refuses parts that
  * contradict it, naming the operands.
  */
class ArchiveReconstructionSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)

  private lazy val analysis = ArchiveFixtures.recordingAnalysis
  private lazy val result   = ArchiveFixtures.temporalResult

  private def recording(
      angular: Recording[Deg] = analysis.angular,
      prepared: Recording[Deg] = analysis.prepared,
      support: Vector[SampleRange] = analysis.detection.eventSeries.support,
      areas: Vector[Aoi[Deg]] = analysis.assignment.aoiSet.areas
  ) = RecordingAnalysis.reconstruct(
    analysis.plan,
    angular,
    prepared,
    analysis.detection.eventSeries.events,
    support,
    areas
  )

  private def moved(value: Recording[Deg], clock: ClockId, lastShift: Long): Recording[Deg] =
    val last = value.samples.length - 1
    get(
      Recording.of(
        value.frame,
        clock,
        value.rate,
        value.eye,
        value.pupilUnit,
        value.samples.updated(
          last,
          value
            .samples(last)
            .copy(t = Instant.micros(value.samples(last).t.toMicros + lastShift))
        ),
        value.samplingTolerance
      )
    )

  test("a recording analysis rebuilds from its plan, recordings, events and areas") {
    val rebuilt = get(recording())
    val codec   = ArchiveFixtures.recordingResults.codec
    assertEquals(codec.encode(rebuilt), codec.encode(analysis))
  }

  test("a plan area with padded names reconstructs: the run trims them as the AOI does") {
    val plan   = ArchiveFixtures.recordingPlan
    val padded = get(
      RecordingArea.of(" image", "Image ", plan.areas.head.bounds).left.map(_.message)
    )
    val changed = get(
      RecordingPlan
        .of(
          plan.input,
          plan.source,
          plan.display,
          plan.trackerClock,
          plan.analysisClock,
          plan.angularFrameId,
          plan.viewing,
          plan.synchronizationModel,
          plan.marks,
          plan.residualLimit,
          plan.interpolationGap,
          Vector(padded),
          plan.method,
          plan.parameters
        )
        .left
        .map(_.message)
    )
    val run   = get(changed.run(InputPayloadFixtures.monocular).left.map(_.message))
    val codec = ArchiveFixtures.recordingResults.codec
    assertEquals(
      codec.decode(get(codec.encode(run))).map(_.description),
      Right(run.description)
    )
    assertEquals(
      run.assignment.aoiSet.areas.head.attributes("nativeBoundsPixels"),
      "0,0,800,600"
    )
  }

  test(
    "a recording stage that contradicts the plan or the stage before it is refused by name"
  ) {
    assertEquals(
      recording(angular = moved(analysis.angular, ClockId("elsewhere"), 0L)).left.toOption,
      Some(RecordingResultError.Stage("angular", "clock", "display", "elsewhere"))
    )
    // The prepared recording is re-derived from the angular one by the plan's
    // gap interpolation: a moved timestamp or a moved tracked sample is refused.
    assertEquals(
      recording(prepared = moved(analysis.prepared, analysis.prepared.clock, 1L)).left.toOption,
      Some(
        RecordingResultError.Stage(
          "prepared",
          "samples[7]",
          "1014010:tracked(1.193489,-0.238731,pupil=905):Measured>Projected",
          "1014011:tracked(1.193489,-0.238731,pupil=905):Measured>Projected"
        )
      )
    )
    val first       = analysis.prepared.samples(0)
    val movedSample = get(
      Recording.of(
        analysis.prepared.frame,
        analysis.prepared.clock,
        analysis.prepared.rate,
        analysis.prepared.eye,
        analysis.prepared.pupilUnit,
        analysis.prepared.samples.updated(
          0,
          first.copy(gaze = Gaze.Tracked(Pt[Deg](0.25, 0.0), Some(900.0)))
        ),
        analysis.prepared.samplingTolerance
      )
    )
    assertEquals(
      recording(prepared = movedSample).left.toOption,
      Some(
        RecordingResultError.Stage(
          "prepared",
          "samples[0]",
          "1000010:tracked(0,0,pupil=900):Measured>Projected",
          "1000010:tracked(0.25,0,pupil=900):Measured>Projected"
        )
      )
    )
    val shifted =
      analysis.detection.eventSeries.support.map(r => get(SampleRange.of(r.from, r.until - 1)))
    recording(support = shifted).left.toOption match
      case Some(
            RecordingResultError.Plan(
              RecordingPlanError.Detection(
                DetectionResultError.SourceSupport(
                  _,
                  _,
                  DetectionSupportError.EventSampleRangeMismatch(_, 0, _, declared, derived)
                )
              )
            )
          ) =>
        assertEquals(declared.until, derived.until - 1)
      case other => fail(s"unexpected $other")
    val area    = analysis.assignment.aoiSet.areas.head
    val renamed =
      get(Aoi.of(area.id.value, "Picture", area.frame, area.region, area.attributes))
    assertEquals(
      recording(areas = Vector(renamed)).left.toOption,
      Some(
        RecordingResultError.Stage("assignment", "areas", "[image|Image]", "[image|Picture]")
      )
    )
    // Areas compare as (id, label) pairs, so labels with separators cannot
    // make two areas pass for one.
    val split = get(Aoi.of("image", "Image", area.frame, area.region, area.attributes))
    val extra = get(Aoi.of("x", "y", area.frame, area.region, area.attributes))
    assertEquals(
      recording(areas = Vector(split, extra)).left.toOption,
      Some(
        RecordingResultError.Stage("assignment", "areas", "[image|Image]", "[image|Image][x|y]")
      )
    )
    // The recorded pixel bounds are compared as numbers, so either platform's
    // rendering of the same bounds is the plan's; other bounds are not.
    val portable = get(
      Aoi.of(
        area.id.value,
        area.label,
        area.frame,
        area.region,
        Map("nativeFrame" -> "display", "nativeBoundsPixels" -> "0,0,800,600")
      )
    )
    assert(recording(areas = Vector(portable)).isRight)
    val elsewhere = get(
      Aoi.of(
        area.id.value,
        area.label,
        area.frame,
        area.region,
        Map("nativeFrame" -> "display", "nativeBoundsPixels" -> "0,0,800,601")
      )
    )
    recording(areas = Vector(elsewhere)).left.toOption match
      case Some(RecordingResultError.Stage("assignment", "areas[0].attributes", _, found)) =>
        assertEquals(found, "nativeBoundsPixels=0,0,800,601;nativeFrame=display")
      case other => fail(s"unexpected $other")
    val detached = get(Aoi.of(area.id.value, area.label, area.frame, area.region, Map.empty))
    recording(areas = Vector(detached)).left.toOption match
      case Some(RecordingResultError.Stage("assignment", "areas[0].attributes", _, "")) => ()
      case other => fail(s"unexpected $other")
  }

  private def records =
    result.cells.map(c =>
      TemporalCellRecord(c.repetition.name, c.window.name, c.occupancy, c.result)
    )

  private def temporal(
      cells: Vector[TemporalCellRecord[
        StudyKey,
        Unit2D.Px,
        eyes4s.compare.Similarity,
        eyes4s.design.SignedDifference
      ]]
  ) =
    TemporalStudyResult.reconstruct(result.plan, cells)

  test("a temporal result rebuilds from its plan and cells") {
    val codec = ArchiveFixtures.temporalResults.codec
    assertEquals(codec.encode(get(temporal(records))), codec.encode(result))
  }

  test(
    "the cell layout, each cell's plan and each cell's context are re-derived from the plan"
  ) {
    assertEquals(
      temporal(records.dropRight(1)).left.toOption,
      Some(TemporalResultError.CellCount(8, 7))
    )
    val all = records
    assertEquals(
      temporal(all(1) +: all(0) +: all.drop(2)).left.toOption,
      Some(
        TemporalResultError.CellLayout(0, "recall-encode", "early", "recall-encode", "middle")
      )
    )
    // The retest-recall result in a recall-encode cell describes another plan.
    temporal(all.head.copy(result = all(4).result) +: all.tail).left.toOption match
      case Some(
            TemporalResultError.Cell(
              "recall-encode",
              "early",
              TemporalResultError.Plan(changes)
            )
          ) =>
        assertEquals(changes.map(_.field), Vector("phases"))
      case other => fail(s"unexpected $other")
    // The middle window's result in the early cell was evaluated in another context.
    temporal(all.head.copy(result = all(1).result) +: all.tail).left.toOption match
      case Some(
            TemporalResultError.Cell(
              "recall-encode",
              "early",
              TemporalResultError.Result(
                StudyResultError.Scale(
                  0,
                  StudyResultError.SpecificationParameters(_, expected, found)
                )
              )
            )
          ) =>
        assertEquals(expected.toMap.apply("temporal.window"), Provenance.Param.Text("early"))
        assertEquals(found.toMap.apply("temporal.window"), Provenance.Param.Text("middle"))
      case other => fail(s"unexpected $other")
  }

  test("every occupancy ledger is tied to its window, its trial's anchor and its densities") {
    val all   = records
    val early = all.head
    // Every cell lists the input's trials: a trial dropped from one cell is refused there.
    val middleCell = all(1)
    assertEquals(
      temporal(
        all.head +: middleCell.copy(occupancy = middleCell.occupancy.drop(1)) +: all.drop(2)
      ).left.toOption,
      Some(
        TemporalResultError.Cell(
          "recall-encode",
          "middle",
          TemporalResultError.OccupancyKeys(
            early.occupancy.map(_._1),
            early.occupancy.drop(1).map(_._1)
          )
        )
      )
    )
    // Dropped from every ledger, it no longer matches the trials each cell estimated.
    assertEquals(
      temporal(all.map(cell => cell.copy(occupancy = cell.occupancy.drop(1)))).left.toOption,
      Some(
        TemporalResultError.Cell(
          "recall-encode",
          "early",
          TemporalResultError.OccupancyKeys(
            early.occupancy.map(_._1),
            early.occupancy.drop(1).map(_._1)
          )
        )
      )
    )
    // The late window's ledgers in the early cell span 350 ms, not 300 ms.
    val late = all(2)
    temporal(early.copy(occupancy = late.occupancy) +: all.tail).left.toOption match
      case Some(
            TemporalResultError.Cell(
              "recall-encode",
              "early",
              TemporalResultError.Occupancy(_, "widthMicros", "300000", "350000")
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")
    // The middle window's ledgers span 300 ms as well, but the densities were not estimated from them.
    val middle = all(1)
    temporal(early.copy(occupancy = middle.occupancy) +: all.tail).left.toOption match
      case Some(
            TemporalResultError.Cell(
              "recall-encode",
              "early",
              TemporalResultError.Density(_, Some(_), _)
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")
    // A missing epoch recorded as another failure no longer produces the stored estimation failure.
    val overflow = TemporalStudyError.AnchorOverflow("early", 0L, 0L, 300000L)
    val forged   = early.occupancy.map {
      case (k, Left(TemporalStudyError.MissingEpoch(_))) => k -> Left(overflow)
      case other                                         => other
    }
    temporal(early.copy(occupancy = forged) +: all.tail).left.toOption match
      case Some(
            TemporalResultError.Cell(
              "recall-encode",
              "early",
              TemporalResultError.Failure(
                k,
                StudyFailure.Temporal(_, TemporalStudyError.MissingEpoch(_))
              )
            )
          ) =>
        assertEquals(k, InputPayloadFixtures.missingEpochKey)
      case other => fail(s"unexpected $other")
    // One ledger re-anchored by a microsecond disagrees with the trial's other cells.
    val index           = early.occupancy.indexWhere(_._2.isRight)
    val (key, value)    = early.occupancy(index)
    val occupancy       = get(value.left.map(_.message))
    val shiftedInterval = get(
      Interval.of(
        occupancy.interval.clock,
        Instant.micros(occupancy.interval.onset.toMicros + 1),
        Instant.micros(occupancy.interval.offset.toMicros + 1)
      )
    )
    val shifted = get(
      WindowOccupancy.reconstruct(
        shiftedInterval,
        occupancy.boundary,
        occupancy.measure.frame,
        occupancy.measure.positions,
        occupancy.observedMicros,
        occupancy.missingMicros,
        occupancy.fixationTimes
      )
    )
    temporal(
      early.copy(occupancy = early.occupancy.updated(index, key -> Right(shifted))) +: all.tail
    ).left.toOption match
      case Some(
            TemporalResultError.Cell(
              _,
              _,
              TemporalResultError.Occupancy(`key`, "anchor", expected, found)
            )
          ) =>
        assertNotEquals(expected, found)
      case other => fail(s"unexpected $other")
  }
