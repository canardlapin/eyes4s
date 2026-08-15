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

package eyes4s.io

import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg

class TidyResultSuite extends munit.FunSuite:

  private val validity = ValidityCodebook
    .of(tracked = Set("1"), lost = Set("0"))
    .toOption
    .get
  private val schema = DelimitedSchema
    .of(
      Delimiter.Comma,
      HeaderMode.FirstLine,
      TimeColumn("time", TimestampUnit.Milliseconds),
      PositionColumns("x", "y", CoordinateUnit.Degrees),
      ValidityColumn("valid", validity),
      None,
      missingTokens = Set("")
    )
    .toOption
    .get
  private val frame = Frame.of(
    FrameId("result-frame"),
    Bounds.of[Deg](-5.0, -5.0, 5.0, 5.0).toOption.get,
    YAxis.Down
  )
  private val clock  = ClockId("tracker-clock")
  private val source =
    "time,x,y,valid\n" +
      "0,-3,-3,1\n" +
      "10,-2,-3,1\n" +
      "20,2,-3,1\n" +
      "30,3,-3,1\n" +
      "40,0,3,1\n" +
      "50,,,0\n"
  private val imported = Delimited
    .parse("participant-01/trial-quote.csv", source, schema)
    .validate(frame, clock, Rate.Fixed(Hz(100.0).toOption.get), Eye.Left)
  private val recording = imported.recording.get
  private val sourceRef = RecordingRef("participant-01/trial-quote")
  private val threshold = IvtThreshold
    .of(Velocity.degPerSecond(1000.0).toOption.get)
    .toOption
    .get
  private val minimum  = MinimumEventDuration.of(Span.millis(10)).toOption.get
  private val detector = Detectors.ivt(threshold, minimum, clock)
  private val card     = detector.card

  private def rectangle(id: String, label: String, lo: Pt[Deg], hi: Pt[Deg]): Aoi[Deg] =
    Aoi
      .of(id, label, frame, Region.rect(lo, hi).toOption.get)
      .toOption
      .get

  private val areas = AoiSet
    .of(
      Vector(
        rectangle("left", "Left, \"target\"", Pt(-5.0, -5.0), Pt(0.0, 0.0)),
        rectangle("right", "Right target", Pt(0.0, -5.0), Pt(5.0, 0.0)),
        rectangle("unused", "Never\r\nentered", Pt(-5.0, 0.0), Pt(0.0, 5.0))
      )
    )
    .toOption
    .get

  private val event = Event.Fixation
    .of(
      Interval.of(clock, Instant.millis(10), Instant.millis(40)).toOption.get,
      Pt[Deg](1.0, -3.0),
      1.0,
      DispersionMethod.RmsRadius,
      3
    )
    .toOption
    .get

  private val machine = Machine(
    new Detector[Unit, Sample[Deg], DetectionEmission[Deg]]:
      def init: Unit = ()
      def step(
          state: Unit,
          sample: Sample[Deg]
      ): (Unit, Vector[DetectionEmission[Deg]])              = ((), Vector.empty)
      def flush(state: Unit): Vector[DetectionEmission[Deg]] = Vector(Right(event))
  )

  private val detection = Detection
    .run(
      sourceRef,
      recording,
      detector,
      GapPolicy.Break
    )
    .toOption
    .get

  private val assignment = areas
    .assign(recording, MembershipPolicy.ExclusiveByPriority)
    .toOption
    .get
  private val study = StudyTrial
    .of(
      "participant-01",
      "trial-quote",
      Vector("condition" -> "search, \"difficult\"", "block" -> "A\r\nB")
    )
    .toOption
    .get
  private val result = TidyAoiResult
    .from(
      study,
      imported,
      detection,
      assignment,
      extraWarnings = Vector("Review, \"manually\"\r\nif excluded time is surprising.")
    )
    .toOption
    .get

  test("typed tidy rows retain every interpretation and evidence field") {
    assertEquals(result.rows.length, 13)
    assertEquals(
      result.rows.take(4).map(_.estimand),
      Vector(
        AoiEstimand.Dwell,
        AoiEstimand.DwellProportion,
        AoiEstimand.FirstEntryLatency,
        AoiEstimand.RunCount
      )
    )
    assertEquals(result.rows.last.estimand, AoiEstimand.TransitionCount)
    assertEquals(
      result.rows.last.target,
      TidyTarget.Transition(
        AoiId.of("left").toOption.get,
        "Left, \"target\"",
        AoiId.of("right").toOption.get,
        "Right target"
      )
    )
    assert(
      result.rows.exists(row =>
        row.estimand == AoiEstimand.FirstEntryLatency &&
          row.target == TidyTarget.Area(AoiId.of("unused").toOption.get, "Never\r\nentered") &&
          row.value == TidyValue.Missing("the AOI was never entered") &&
          row.unit == ResultUnit.Microseconds
      )
    )
    result.rows.foreach { row =>
      assertEquals(row.study, study)
      assertEquals(row.evidence.frame.id, frame.id)
      assertEquals(row.evidence.frame.specification, frame.spec)
      assertEquals(row.evidence.clock.source.clock, clock)
      assertEquals(row.evidence.clock.analysis, clock)
      assertEquals(row.evidence.clock.synchronization, None)
      assertEquals(row.evidence.detectorCard, card)
      assertEquals(row.evidence.inputIdentity, imported.sourceDigest)
      assertEquals(row.evidence.excludedTime, Span.millis(10))
      assertEquals(row.evidence.sampleOrigins, SampleOriginCounts(6, 6, 0, 0, 0, 0))
      assertEquals(row.evidence.unclassifiedTime, Span.zero)
      assert(
        row.evidence.detectorConfiguration.exists(_._1 == "velocityThresholdDegPerSecond")
      )
      assertEquals(row.evidence.provenance.steps.last.operation, "aoi-measure")
    }
  }

  test("the psychology report and explanation need no kernel vocabulary") {
    val report = result.report
    assertEquals(report.participant, "participant-01")
    assertEquals(report.trial, "trial-quote")
    assertEquals(report.resultRows, 13)
    assertEquals(report.areaRows, 12)
    assertEquals(report.transitionRows, 1)
    assertEquals(report.excludedMilliseconds, 10.0)
    assertEquals(report.measuredSamples, 6)
    assertEquals(report.derivedSamples, 0)
    assertEquals(report.inputSha256, Sha256.ofUtf8(source).hex)
    assert(result.explain.contains("Participant 'participant-01'"))
    assert(result.explain.contains("Input SHA-256"))
  }

  test("RFC-4180 export has fixed order, explicit missingness, and lossless round-trip") {
    val csv      = TidyCsv.encode(result)
    val document = TidyCsv.decode(csv).toOption.get

    assertEquals(
      TidyCsv.header,
      Vector(
        "schema_version",
        "participant_id",
        "trial_id",
        "conditions",
        "aoi_id",
        "aoi_label",
        "transition_to_aoi_id",
        "transition_to_aoi_label",
        "estimand",
        "value_status",
        "value",
        "missing_reason",
        "unit",
        "frame_id",
        "frame_x_min_bits",
        "frame_y_min_bits",
        "frame_x_max_bits",
        "frame_y_max_bits",
        "frame_y_axis",
        "frame_unit_symbol",
        "frame_unit_name",
        "source_clock_id",
        "source_clock_native_unit",
        "source_clock_rounding",
        "analysis_clock_id",
        "synchronization",
        "detector_id",
        "detector_version",
        "detector_name",
        "detector_card",
        "detector_configuration",
        "excluded_microseconds",
        "unclassified_microseconds",
        "origin_sample_count",
        "origin_measured",
        "origin_interpolated",
        "origin_smoothed",
        "origin_projected",
        "origin_derived",
        "warnings",
        "operation_provenance",
        "input_sha256"
      )
    )
    assert(csv.endsWith("\r\n"))
    assert(csv.contains("\"Left, \"\"target\"\"\""))
    assert(csv.contains("\"Never\r\nentered\""))
    assertEquals(document.header, TidyCsv.header)
    assertEquals(document.rows, TidyCsv.document(result).rows)
    assertEquals(document.encode, csv)

    def cell(row: Vector[String], name: String): String = row(TidyCsv.header.indexOf(name))
    assertEquals(
      document.rows.map(row => cell(row, "aoi_id") -> cell(row, "estimand")),
      Vector(
        "left"   -> "dwell",
        "left"   -> "dwell_proportion",
        "left"   -> "first_entry_latency",
        "left"   -> "run_count",
        "right"  -> "dwell",
        "right"  -> "dwell_proportion",
        "right"  -> "first_entry_latency",
        "right"  -> "run_count",
        "unused" -> "dwell",
        "unused" -> "dwell_proportion",
        "unused" -> "first_entry_latency",
        "unused" -> "run_count",
        "left"   -> "transition_count"
      )
    )
    val missing = document.rows
      .find(row =>
        cell(row, "aoi_id") == "unused" && cell(row, "estimand") == "first_entry_latency"
      )
      .get
    assertEquals(cell(missing, "value_status"), "missing")
    assertEquals(cell(missing, "value"), "")
    assertEquals(cell(missing, "missing_reason"), "the AOI was never entered")
    document.rows.foreach { row =>
      assertEquals(cell(row, "schema_version"), TidyCsv.schemaVersion)
      assert(cell(row, "detector_card").nonEmpty)
      assert(cell(row, "detector_configuration").contains("velocityThresholdDegPerSecond"))
      assert(
        cell(row, "warnings").contains(
          "Review, \"manually\"\r\nif excluded time is surprising."
        )
      )
      assert(cell(row, "operation_provenance").contains("aoi-measure"))
      assertEquals(cell(row, "input_sha256"), imported.sourceDigest.hex)
    }
  }

  test("schema and evidence loss are rejected rather than guessed") {
    val document      = TidyCsv.document(result)
    val swappedHeader = new TidyCsvDocument(
      document.header.updated(0, "participant_id").updated(1, "schema_version"),
      document.rows
    ).encode
    assert(
      TidyCsv.decode(swappedHeader).left.exists(_.isInstanceOf[TidyCsvError.UnexpectedHeader])
    )

    val provenanceIndex   = TidyCsv.header.indexOf("operation_provenance")
    val withoutProvenance = new TidyCsvDocument(
      document.header,
      document.rows.updated(0, document.rows.head.updated(provenanceIndex, ""))
    ).encode
    assertEquals(
      TidyCsv.decode(withoutProvenance).left.toOption,
      Some(TidyCsvError.MissingRequiredContext(2, "operation_provenance"))
    )
  }

  test("raw syntax parsing is weaker than scientific decoding") {
    val document = TidyCsv.document(result)

    def mutated(rowIndex: Int, column: String, value: String): String =
      val columnIndex = TidyCsv.header.indexOf(column)
      new TidyCsvDocument(
        document.header,
        document.rows.updated(
          rowIndex,
          document.rows(rowIndex).updated(columnIndex, value)
        )
      ).encode

    val proportionRow = document.rows.indexWhere(
      _(TidyCsv.header.indexOf("estimand")) == AoiEstimand.DwellProportion.label
    )
    val forgeries = Vector(
      mutated(0, "frame_x_min_bits", "not-a-double"),
      mutated(0, "analysis_clock_id", "forged-clock"),
      mutated(0, "detector_id", "forged-detector"),
      mutated(0, "excluded_microseconds", "-1"),
      mutated(0, "origin_measured", "7"),
      mutated(0, "operation_provenance", "garbage"),
      mutated(0, "input_sha256", "ABC"),
      mutated(proportionRow, "value", "1.01")
    )

    forgeries.foreach { csv =>
      assert(TidyCsv.parseRaw(csv).isRight, clue(csv))
      assert(
        TidyCsv.decode(csv).left.exists(_.isInstanceOf[TidyCsvError.InvalidScientificField]),
        clue(TidyCsv.decode(csv))
      )
    }
  }

  test("study keys and custom detector identities cannot be silently substituted") {
    assert(
      StudyTrial
        .of("p", "t", Vector("condition" -> "a", "condition" -> "b"))
        .left
        .exists(_.isInstanceOf[TidyResultError.DuplicateConditionKey])
    )
    assertEquals(detection.identity, DetectorIdentity.Algorithm(card))
    val custom = Detection
      .runCustom(sourceRef, recording, card.detectorRef, GapPolicy.Break, machine)
      .toOption
      .get
    assert(
      TidyAoiResult
        .from(study, imported, custom, assignment)
        .left
        .exists(_.isInstanceOf[TidyResultError.CustomDetectorCannotProduceScientificExport])
    )
  }

  test("tidy export rejects detection and AOI temporal-support disagreement") {
    val alternate = TemporalSupport.ForwardHold(
      MaximumSupportGap.Unlimited,
      EdgeSupport.Censored
    )
    val alternateDetection = Detection
      .run(sourceRef, recording, detector, GapPolicy.Break, alternate)
      .toOption
      .get

    assertEquals(
      TidyAoiResult.from(study, imported, alternateDetection, assignment).left.toOption,
      Some(
        TidyResultError.TemporalSupportMismatch(
          sourceRef.value + ".csv",
          alternate,
          assignment.support.policy
        )
      )
    )
  }

  test("analysis frame and clock may differ only with matching synchronization evidence") {
    val analysisFrame = Frame.of(
      FrameId("analysis-degrees"),
      frame.bounds,
      YAxis.Up
    )
    val stimulusClock     = ClockId("stimulus-clock")
    val analysisRecording = Recording
      .of(
        analysisFrame,
        stimulusClock,
        recording.rate,
        recording.eye,
        recording.pupilUnit,
        recording.samples.map(sample =>
          Sample(
            sample.t + Span.millis(100),
            sample.gaze,
            SampleOrigin.Projected
          )
        )
      )
      .toOption
      .get
    val analysisDetector  = Detectors.ivt(threshold, minimum, stimulusClock)
    val analysisDetection = Detection
      .run(
        sourceRef,
        analysisRecording,
        analysisDetector,
        GapPolicy.Break
      )
      .toOption
      .get
    def analysisArea(id: String, lo: Pt[Deg], hi: Pt[Deg]) =
      Aoi.of(id, id, analysisFrame, Region.rect(lo, hi).toOption.get).toOption.get
    val analysisAreas = AoiSet
      .of(
        Vector(
          analysisArea("left", Pt(-5.0, -5.0), Pt(0.0, 0.0)),
          analysisArea("right", Pt(0.0, -5.0), Pt(5.0, 0.0))
        )
      )
      .toOption
      .get
    val analysisAssignment = analysisAreas
      .assign(analysisRecording, MembershipPolicy.ExclusiveByPriority)
      .toOption
      .get
    val synchronization = SyncEvidence
      .fromCommonMarks(
        clock,
        stimulusClock,
        SyncFitMode.OffsetOnly,
        Vector(
          SyncMark.of("start", Instant.millis(0), Instant.millis(100)).toOption.get,
          SyncMark.of("end", Instant.millis(50), Instant.millis(150)).toOption.get
        )
      )
      .toOption
      .get

    assertEquals(
      TidyAoiResult
        .from(study, imported, analysisDetection, analysisAssignment)
        .left
        .toOption,
      Some(
        TidyResultError.MissingSynchronization(sourceRef.value + ".csv", clock, stimulusClock)
      )
    )
    val transformed = TidyAoiResult
      .from(
        study,
        imported,
        analysisDetection,
        analysisAssignment,
        Some(synchronization),
        Vector(
          Provenance.Step.text("synchronize", "target", stimulusClock.name),
          Provenance.Step.text("warp", "target", analysisFrame.id.name)
        )
      )
      .toOption
      .get
    assertEquals(transformed.evidence.frame.id, analysisFrame.id)
    assertEquals(transformed.evidence.frame.specification, analysisFrame.spec)
    assertEquals(transformed.evidence.clock.source.clock, clock)
    assertEquals(transformed.evidence.clock.analysis, stimulusClock)
    assertEquals(transformed.evidence.sampleOrigins, SampleOriginCounts(6, 6, 0, 0, 6, 6))
    assertEquals(
      transformed.evidence.provenance.steps.map(_.operation),
      Vector("synchronize", "warp", "detect", "aoi-measure")
    )
    val csv      = TidyCsv.decode(TidyCsv.encode(transformed)).toOption.get
    val syncCell = csv.rows.head(TidyCsv.header.indexOf("synchronization"))
    assert(syncCell.contains("start"))
    assert(syncCell.contains("end"))
  }
