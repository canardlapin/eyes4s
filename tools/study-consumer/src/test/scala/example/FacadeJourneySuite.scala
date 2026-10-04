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

package example

import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

/** The psychology facade from a fresh consumer: `PsychologyWorkflow.run` on a
  * real AdSERP tracker excerpt, against the same analysis composed explicitly
  * from the public import, plan and tidy-result APIs, then saved and reloaded
  * through the recording codec. The convenience route and the explicit
  * composition must agree exactly: the same plan, detection, assignment and
  * tidy export. Runs on the JVM and Scala.js.
  */
class FacadeJourneySuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)

  private val source      = "AdSERP/p049-b4-t6/rows-231-320.csv"
  private val participant = "p049"
  private val trialName   = "b4-t6-excerpt"
  private val conditions  = Vector("dataset" -> "AdSERP", "task" -> "SERP")
  private val metadata    = Vector(
    "dataset"            -> "AdSERP",
    "zenodoRecord"       -> "15236546",
    "originalFile"       -> "pupil-data/p049-b4-t6.csv",
    "originalFileSha256" -> "bd5f7e02fb261e7b04415d015cfd880ab475826c630f0d98cfdac5cc27150c11",
    "license"            -> "MIT",
    "doi"                -> "10.1145/3726302.3730325"
  )
  // Laboratory quantities, stated once and used by both routes.
  private val (widthPx, heightPx)             = (1280, 1024)
  private val (distanceMm, widthMm, heightMm) = (600.0, 530.0, 300.0)
  private val (trackerClock, analysisClock)   = ("gazepoint-unix", "trial-relative")
  private val marks                           =
    Vector(("trial-start", 1678716023627L, 0L), ("trial-end", 1678716024223L, 596L))
  private val (gapMs, velocityDegPerS, minimumMs) = (150L, 30.0, 20L)
  private val bands                               = Vector(
    ("left", "Left result band", 200, 0, 320, 180),
    ("right", "Right result band", 320, 0, 500, 180)
  )

  private def facadePlan: AdserpWorkflowPlan = get(
    AdserpWorkflowPlan.of(
      sourceName = source,
      participant = participant,
      trial = trialName,
      conditions = conditions,
      displayWidthPixels = widthPx,
      displayHeightPixels = heightPx,
      viewingDistanceMillimetres = distanceMm,
      displayWidthMillimetres = widthMm,
      displayHeightMillimetres = heightMm,
      trackerClock = trackerClock,
      analysisClock = analysisClock,
      synchronizationModel = SynchronizationModel.OffsetOnly,
      synchronizationMarks =
        marks.map((id, source, analysis) => get(WorkflowSyncMark.of(id, source, analysis))),
      interpolationMaxGapMilliseconds = gapMs,
      velocityThresholdDegreesPerSecond = velocityDegPerS,
      minimumEventDurationMilliseconds = minimumMs,
      areas = bands.map((id, label, x0, y0, x1, y1) =>
        get(WorkflowAoi.of(id, label, x0, y0, x1, y1))
      ),
      sourceMetadata = metadata
    )
  )

  /** The same analysis from the public building blocks the facade documents:
    * the AdSERP schema, delimited import, and a recording plan whose every
    * choice is stated explicitly.
    */
  private final case class Explicit(
      study: StudyTrial,
      imported: DelimitedImport[Px],
      plan: RecordingPlan[IvtParameters]
  )

  private def explicitRoute: Explicit =
    val validity = get(ValidityCodebook.of(tracked = Set("1"), lost = Set("0")))
    val schema   = get(
      DelimitedSchema.of(
        Delimiter.Comma,
        HeaderMode.FirstLine,
        TimeColumn("timestamp", TimestampUnit.Milliseconds),
        PositionColumns("BPOGX", "BPOGY", CoordinateUnit.Pixels),
        ValidityColumn("LPV", validity),
        Some(PupilColumn("LPD", PupilUnit.Diameter)),
        missingTokens = Set("", "NA")
      )
    )
    val display  = get(Frame.screen(source + ":display", widthPx, heightPx))
    val tracker  = ClockId(trackerClock)
    val raw      = Delimited.parse(source, FacadeFixtures.csv, schema, metadata)
    val imported = raw.validate(display, tracker, Rate.Irregular, Eye.Left)
    val native   = imported.recording.getOrElse(fail(s"import: ${imported.diagnostics}"))
    val areas    = bands.map((id, label, x0, y0, x1, y1) =>
      get(RecordingArea.of(id, label, get(Bounds.of[Px](x0, y0, x1, y1))))
    )
    val plan = get(
      RecordingPlan.of(
        ArtifactRef.of[Recording[Px]](native.contentHash),
        RecordingRef(s"$participant/$trialName"),
        display,
        tracker,
        ClockId(analysisClock),
        FrameId(source + ":visual-angle"),
        Some(get(Viewing.millimetres(distanceMm, widthMm, heightMm))),
        SyncFitMode.OffsetOnly,
        marks.map((id, s, a) => get(SyncMark.of(id, Instant.millis(s), Instant.millis(a)))),
        None,
        get(InterpolationGap.of(Span.millis(gapMs))),
        areas,
        RecordingMethod.ivt(get(DefinitionId.of("eyes4s.recording.ivt", 1))),
        IvtParameters(
          get(IvtThreshold.of(get(Velocity.degPerSecond(velocityDegPerS)))),
          get(MinimumEventDuration.of(Span.millis(minimumMs)))
        )
      )
    )
    Explicit(get(StudyTrial.of(participant, trialName, conditions)), imported, plan)

  test("the facade and the explicit composition describe the same analysis") {
    val prepared = get(PsychologyWorkflow.prepare(facadePlan, FacadeFixtures.csv))
    val explicit = explicitRoute
    assertEquals(
      prepared.imported.sourceDigest.hex,
      "57dc3230c1e2a701017011a9652e215fcb09b7f1b63a16fe00ca0ed1ca9639d0"
    )
    assertEquals(explicit.imported.sourceDigest, prepared.imported.sourceDigest)
    assertEquals(explicit.imported.raw.rows, prepared.imported.raw.rows)
    assertEquals(explicit.study, prepared.study)
    assertEquals(explicit.plan.diff(prepared.analysis), Vector.empty)
    assertEquals(explicit.plan.description, prepared.analysis.description)
  }

  test("the convenience route equals the explicit composition, stage by stage") {
    val facade   = get(PsychologyWorkflow.run(facadePlan, FacadeFixtures.csv))
    val explicit = explicitRoute
    val native   = explicit.imported.recording.getOrElse(fail("recording"))
    val analysis = get(explicit.plan.run(native))
    assertEquals(analysis.synchronization.offset, facade.synchronization.offset)
    assertEquals(analysis.angular.contentHash, facade.angularRecording.contentHash)
    assertEquals(analysis.prepared.contentHash, facade.preparedRecording.contentHash)
    assertEquals(analysis.detection.eventSeries.events, facade.detection.eventSeries.events)
    assertEquals(analysis.detection.eventSeries.support, facade.detection.eventSeries.support)
    assertEquals(analysis.assignment.report, facade.assignment.report)
    // The independent check: from the explicit stages, the generic tidy constructor
    // yields the same report and values. It does not carry the facade's upstream
    // provenance steps.
    val generic = get(
      TidyAoiResult.from(
        explicit.study,
        explicit.imported,
        analysis.detection,
        analysis.assignment,
        Some(analysis.synchronization)
      )
    )
    assertEquals(generic.report, facade.tidy.report)
    assertEquals(generic.rows.map(_.value), facade.tidy.rows.map(_.value))
    // Byte equality of the export, provenance included, goes through the facade's own
    // tidy stage and entry point, so it shows the explicit plan and stages are accepted
    // there; it is not independent of the facade's provenance code.
    val tidy = get(PsychologyWorkflow.tidy(explicit.study, explicit.imported, analysis))
    assertEquals(TidyCsv.encode(tidy), facade.csv)
    assertEquals(tidy.evidence.provenance, facade.provenance)
    // The tidy stage refuses an analysis of another recording.
    val moved = Delimited
      .parse(
        source,
        FacadeFixtures.csv.replace("1678716023627,286,37", "1678716023627,287,37"),
        explicit.imported.raw.schema,
        metadata
      )
      .validate(explicit.imported.frame, explicit.imported.clock, Rate.Irregular, Eye.Left)
    assert(
      PsychologyWorkflow
        .tidy(explicit.study, moved, analysis)
        .left
        .exists(_.isInstanceOf[PsychologyWorkflowError.AnalysisInputMismatch])
    )
    val rerun =
      get(PsychologyWorkflow.runAnalysis(explicit.study, explicit.imported, explicit.plan))
    assertEquals(rerun.csv, facade.csv)
    assertEquals(rerun.report, facade.report)
  }

  test("a saved and reloaded plan reruns to the same tidy export; failures stay named") {
    def id(name: String) = get(DefinitionId.of(name, 1))
    val persistence      =
      RecordingCodecs.ivt(
        id("eyes4s.recording-plan"),
        id("eyes4s.recording.ivt"),
        id("eyes4s.ivt-parameters")
      )
    val prepared = get(PsychologyWorkflow.prepare(facadePlan, FacadeFixtures.csv))
    val direct   = get(PsychologyWorkflow.run(facadePlan, FacadeFixtures.csv))
    val json     = get(persistence.codec.encode(prepared.analysis))
    val restored = get(persistence.codec.parse(json.noSpaces))
    assertEquals(restored.diff(prepared.analysis), Vector.empty)
    val rerun = get(PsychologyWorkflow.runAnalysis(prepared.study, prepared.imported, restored))
    assertEquals(rerun.csv, direct.csv)
    assertEquals(rerun.report, direct.report)

    val report = direct.report
    assertEquals(
      (report.sourceRows, report.acceptedRows, report.rejectedRows, report.detectedEvents),
      (90, 90, 0, 9)
    )
    assertEquals((report.projectedSamples, report.interpolatedSamples), (72, 18))
    assertEquals(report.synchronizationRmsMilliseconds, 0.0)
    assertEquals(
      Sha256.ofUtf8(direct.csv).hex,
      "c9a2ec351a09d94533305ef2bdb7743ee4773f41a6c1d2d21c068a13927b9f76"
    )
    assert(
      PsychologyWorkflow
        .run(facadePlan, "timestamp,BPOGX,BPOGY,LPD,LPV,RPD,RPV\nbad,row")
        .left
        .exists(_.isInstanceOf[PsychologyWorkflowError.ImportFailed])
    )
    assert(
      WorkflowAoi
        .of("bad", "Bad", 10, 10, 5, 20)
        .left
        .exists(_.isInstanceOf[PsychologyWorkflowError.DegenerateAoiBounds])
    )

    val runtime = if System.getProperty("java.vm.name") == "Scala.js" then "js" else "jvm"
    println(
      "EYES4S_FACADE_JOURNEY=" + Json
        .obj(
          "runtime"      -> Json.fromString(runtime),
          "source"       -> Json.fromString(direct.imported.sourceDigest.hex),
          "plan"         -> json,
          "csv_sha256"   -> Json.fromString(Sha256.ofUtf8(direct.csv).hex),
          "rows"         -> Json.fromInt(report.result.resultRows),
          "events"       -> Json.fromInt(report.detectedEvents),
          "projected"    -> Json.fromInt(report.projectedSamples),
          "interpolated" -> Json.fromInt(report.interpolatedSamples)
        )
        .noSpaces
    )
  }
