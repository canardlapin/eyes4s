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

package eyes4s.plan

import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.surface.EdgePolicy

class PreflightSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  // ---------------------------------------------------------------- fixation study fixtures

  private val frame                         = get(Frame.screen("study-work", 2, 2))
  private val other                         = get(Frame.screen("different-address", 2, 2))
  private val grid                          = get(Grid.over(frame, 2, 2))
  private val a                             = StudyKey("p1", "a", "recall")
  private val b                             = StudyKey("p1", "b", "recall")
  private val ar                            = StudyKey("p1", "a", "encode")
  private val br                            = StudyKey("p1", "b", "encode")
  private val lone                          = StudyKey("p2", "c", "recall")
  private val extra                         = StudyKey("p2", "c", "excluded")
  private def clock(key: StudyKey): ClockId =
    ClockId(key.participant + "/" + key.stimulus + "/" + key.phase)
  private def trial(key: StudyKey, x: Double, f: Frame[Px] = frame, y: Double = 0.5) =
    val fix = get(
      Event.Fixation.withoutDispersion(
        get(Interval.of(clock(key), Instant.micros(0), Instant.micros(1000))),
        Pt[Px](x, y),
        1
      )
    )
    Trial(key, (), get(Scanpath.of(f, clock(key), IArray(fix))))
  private val rows   = Vector(trial(a, 0.5), trial(b, 1.5), trial(ar, 0.5), trial(br, 1.5))
  private val input  = StudyInput(Trials(rows :+ trial(extra, 0.5)))
  private val cosine = StudyMethod.cosine[Px](DefinitionId.cosine)

  private def plan(
      source: StudyInput[StudyKey, Px] = input,
      scales: Vector[StudyEstimate[Px]] = Vector(StudyEstimate.Binned()),
      method: StudyMethod[Unit, Px, Similarity, SignedDifference] = cosine,
      focal: String = "recall",
      reference: String = "encode"
  ) =
    get(
      StudyPlan.of(
        source.reference,
        StudyKey.layout(DefinitionId.studyLayout),
        grid,
        focal,
        reference,
        Weight.Duration,
        scales,
        FailurePolicy.RequireAll,
        method,
        ()
      )
    )

  /** A method whose factory and comparison both count their invocations. It
    * stays a bounded comparison, so the cosine descriptor's execution claim
    * remains true of it.
    */
  private final class Sentinel:
    var factory                                                     = 0
    var comparisons                                                 = 0
    val method: StudyMethod[Unit, Px, Similarity, SignedDifference] =
      new StudyMethod[Unit, Px, Similarity, SignedDifference](
        DefinitionId.cosine,
        "sentinel",
        _ => Vector.empty,
        MethodExecution.Bounded { _ =>
          factory += 1
          val inner = Distribution.cosine[Px]
          new BoundedCompare[Mass[Px], Mass[Px], Similarity]:
            def info: MeasureInfo                                             = inner.info
            def start(x: Mass[Px], y: Mass[Px]): ComparisonCursor[Similarity] =
              comparisons += 1
              inner.start(x, y)
        },
        Some(ComparisonMethods.cosine.descriptor)
      )

  private val undescribed =
    new StudyMethod[Unit, Px, Similarity, SignedDifference](
      DefinitionId.cosine,
      "undescribed",
      _ => Vector.empty,
      _ => Distribution.cosine[Px]
    )
  private val cosineV2   = get(DefinitionId.of("eyes4s.cosine", 2))
  private val misversion =
    new StudyMethod[Unit, Px, Similarity, SignedDifference](
      cosineV2,
      "misversioned",
      _ => Vector.empty,
      _ => Distribution.cosine[Px],
      Some(ComparisonMethods.cosine.descriptor)
    )

  // ---------------------------------------------------------------- recording fixtures

  private val display                = get(Frame.screen("external-display", 1000, 1000))
  private val trackerClock           = ClockId("external-tracker")
  private val analysisClock          = ClockId("external-analysis")
  private val source                 = RecordingRef("external-recording")
  private def recordingAt(x: Double) = get(
    Recording.of(
      display,
      trackerClock,
      Rate.Fixed(get(Hz(100.0))),
      Eye.Left,
      None,
      IArray.from(
        (0 until 10).map(i =>
          Sample(Instant.millis(i.toLong * 10L), Gaze.Tracked(Pt[Px](x, x), None))
        )
      )
    )
  )
  private val recording = recordingAt(500.0)
  private val marks     = Vector(
    get(SyncMark.of("start", Instant.millis(0), Instant.millis(0))),
    get(SyncMark.of("end", Instant.millis(90), Instant.millis(90)))
  )
  private val area = get(
    RecordingArea.of("centre", "Central area", get(Bounds.of[Px](400.0, 400.0, 600.0, 600.0)))
  )
  private val ivtId     = get(DefinitionId.of("eyes4s.ivt", 1))
  private val ivt       = RecordingMethod.ivt(ivtId)
  private val ivtParams = IvtParameters(
    get(IvtThreshold.of(get(Velocity.perSecond[Deg](30)))),
    get(MinimumEventDuration.of(Span.micros(20000)))
  )
  private def recordingPlan[P](
      input: ArtifactRef[Recording[Px]] = ArtifactRef.of(recording.contentHash),
      frame: Frame[Px] = display,
      tracker: ClockId = trackerClock,
      viewing: Option[Viewing] = Some(get(Viewing.millimetres(600.0, 500.0, 500.0))),
      model: SyncFitMode = SyncFitMode.OffsetOnly,
      syncMarks: Vector[SyncMark] = marks,
      method: RecordingMethod[P] = ivt,
      parameters: P = ivtParams
  ) = get(
    RecordingPlan.of(
      input,
      source,
      frame,
      tracker,
      analysisClock,
      FrameId("external-angular"),
      viewing,
      model,
      syncMarks,
      None,
      InterpolationGap.none,
      Vector(area),
      method,
      parameters
    )
  )

  /** A detector whose machine counts every sample it is fed. */
  private final class DetectorSentinel:
    var factory                                = 0
    var steps                                  = 0
    val method: RecordingMethod[IvtParameters] = new RecordingMethod[IvtParameters](
      ivtId,
      ivt.parameters,
      (p, _) =>
        factory += 1
        EventDetector.of[Deg](
          AlgorithmCards.ivt,
          Machine(new Detector[Unit, Sample[Deg], DetectionEmission[Deg]]:
            def init: Unit                                                            = ()
            def step(s: Unit, i: Sample[Deg]): (Unit, Vector[DetectionEmission[Deg]]) =
              steps += 1
              ((), Vector.empty)
            def flush(s: Unit): Vector[DetectionEmission[Deg]] =
              steps += 1
              Vector.empty),
          ivt.parameters(p)
        )
      ,
      Some(RecordingMethodDescriptor.ivt(ivtId))
    )

  private val brokenDetector: RecordingMethod[IvtParameters] =
    new RecordingMethod[IvtParameters](
      ivtId,
      ivt.parameters,
      (p, _) =>
        Left(DetectorDefinitionError.Configuration(AlgorithmCards.ivt.id, ivt.parameters(p))),
      Some(RecordingMethodDescriptor.ivt(ivtId))
    )
  private val undescribedDetector: RecordingMethod[IvtParameters] =
    new RecordingMethod[IvtParameters](ivtId, ivt.parameters, ivt.detector)
  private val ivtV2 = get(DefinitionId.of("eyes4s.ivt", 2))
  private val misversionedDetector: RecordingMethod[IvtParameters] =
    new RecordingMethod[IvtParameters](
      ivtV2,
      ivt.parameters,
      ivt.detector,
      Some(RecordingMethodDescriptor.ivt(ivtId))
    )

  // ---------------------------------------------------------------- temporal fixtures

  private def interval(key: StudyKey, from: Long, until: Long) =
    get(Interval.of(clock(key), Instant.micros(from), Instant.micros(until)))
  private def epoch(
      key: StudyKey,
      anchor: Long = 0,
      spans: Vector[(Long, Long)] = Vector(0L -> 1000L)
  ) =
    key -> TrialEpoch(
      Instant.micros(anchor),
      get(ObservedCoverage.of(clock(key), spans.map { case (f, u) => interval(key, f, u) }))
    )
  private val study         = StudyInput(Trials(rows))
  private val epochs        = rows.map(t => epoch(t.key))
  private val temporalInput = get(TemporalStudyInput.of(study, epochs))
  private def window(name: String, from: Long, until: Long) =
    get(StudyWindow.of(name, get(Window.of(Span.micros(from), Span.micros(until)))))
  private val windows    = Vector(window("early", 0, 500), window("late", 500, 1000))
  private val repetition = get(
    RepetitionContrast.withinParticipant("recall", "recall", "encode")
  )
  private def temporalPlan(
      available: TemporalStudyInput[StudyKey, Px] = temporalInput,
      base: StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference] = plan(study),
      windows: Vector[StudyWindow] = windows,
      repetitions: Vector[RepetitionContrast] = Vector(repetition)
  ) = get(
    TemporalStudyPlan.of(
      base,
      available.reference,
      windows,
      repetitions,
      FixationBoundary.ClipDuration
    )
  )

  // ================================================================ tests

  test("the shipped recipe families are listable") {
    assertEquals(
      Preflight.families,
      Vector(
        RecipeFamily.FixationStudy,
        RecipeFamily.EventRecording,
        RecipeFamily.TemporalStudy,
        RecipeFamily.Repetition
      )
    )
  }

  test("fixation study table: artifact, frame, budget, method and design findings") {
    type F = StudyFinding[StudyKey, Px]
    val reversed  = StudyInput(Trials(input.trials.rows.reverse))
    val misframed = StudyInput(
      Trials(Vector(trial(a, 0.5, other), trial(b, 1.5), trial(ar, 0.5), trial(br, 1.5)))
    )
    val duplicated =
      StudyInput(Trials(Vector(trial(a, 0.5), trial(a, 0.5), trial(lone, 0.5), trial(ar, 0.5))))
    val budget = get(PairScheduleBudget.of(5, 7L, 10))
    val table: Vector[
      (
          String,
          StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference],
          Option[StudyInput[StudyKey, Px]],
          PairScheduleBudget,
          Vector[F]
      )
    ] = Vector(
      (
        "missing artifact",
        plan(),
        None,
        PairScheduleBudget.default,
        Vector(StudyFinding.MissingArtifact(plan().input))
      ),
      (
        "wrong artifact",
        plan(),
        Some(reversed),
        PairScheduleBudget.default,
        Vector(StudyFinding.ArtifactMismatch(plan().input, reversed.reference))
      ),
      (
        "incompatible frame",
        plan(misframed),
        Some(misframed),
        PairScheduleBudget.default,
        Vector(StudyFinding.FrameMismatch(a, GeometryError.FrameMismatch(frame.id, other.id)))
      ),
      (
        "invalid budget setting",
        plan(),
        Some(input),
        budget,
        Vector(StudyFinding.OverBudget(BudgetError.CandidateVisits(2, 2, 1, 7L)))
      ),
      (
        "unknown method",
        plan(method = undescribed),
        Some(input),
        PairScheduleBudget.default,
        Vector(StudyFinding.UndescribedMethod(DefinitionId.cosine))
      ),
      (
        "unknown method version",
        plan(method = misversion),
        Some(input),
        PairScheduleBudget.default,
        Vector(
          StudyFinding.InconsistentDescriptor(
            cosineV2,
            DescriptorError.MethodIdentity(cosineV2, DefinitionId.cosine)
          )
        )
      ),
      (
        "duplicate and unmatched focal trials",
        plan(duplicated),
        Some(duplicated),
        PairScheduleBudget.default,
        Vector(
          StudyFinding.DuplicateTrial(a, PairingSide.Focal, Vector(0, 1)),
          // Without a match, lone is left out of the control design (bead
          // S0.7b), so it is not also reported as uncontrolled.
          StudyFinding.UnmatchedFocal(lone, UnmatchedKind.Undetermined)
        )
      )
    )
    table.foreach { case (name, p, available, budget, expected) =>
      val report = p.preflight(available, budget)
      assertEquals(report.findings, expected, name)
      assertEquals(report.family, RecipeFamily.FixationStudy, name)
      assertEquals(report.description, p.description, name)
      assertEquals(report.expected, p.input, name)
      assertEquals(report.available, available.map(_.reference), name)
      assertEquals(report.notChecked, Preflight.studyUnchecked, name)
    }
    val expectations: Vector[(F, Severity, FindingClass, Remedy)] = Vector(
      (
        StudyFinding.MissingArtifact(plan().input),
        Severity.Blocker,
        FindingClass.UnavailableInput,
        Remedy.SupplyReferencedArtifact
      ),
      (
        StudyFinding.ArtifactMismatch(plan().input, reversed.reference),
        Severity.Blocker,
        FindingClass.UnavailableInput,
        Remedy.RetargetPlanToAvailableInput
      ),
      (
        StudyFinding.FrameMismatch(a, GeometryError.FrameMismatch(frame.id, other.id)),
        Severity.Warning,
        FindingClass.IncompatibleInput,
        Remedy.AlignFrame
      ),
      (
        StudyFinding.OverBudget(BudgetError.CandidateVisits(2, 2, 1, 7L)),
        Severity.Blocker,
        FindingClass.InvalidSetting,
        Remedy.RaiseBudgetOrReduceStudy
      ),
      (
        StudyFinding.UndescribedMethod(DefinitionId.cosine),
        Severity.Warning,
        FindingClass.InvalidSetting,
        Remedy.RegisterMethodDescriptor
      ),
      (
        StudyFinding.InconsistentDescriptor(
          cosineV2,
          DescriptorError.MethodIdentity(cosineV2, DefinitionId.cosine)
        ),
        Severity.Warning,
        FindingClass.InvalidSetting,
        Remedy.ReconcileMethodDescriptor
      ),
      (
        StudyFinding.DuplicateTrial(a, PairingSide.Focal, Vector(0, 1)),
        Severity.Warning,
        FindingClass.DataDependent,
        Remedy.ResolveDuplicateTrials
      ),
      (
        StudyFinding.UnmatchedFocal(lone, UnmatchedKind.Undetermined),
        Severity.Warning,
        FindingClass.DataDependent,
        Remedy.SupplyMatchedReference
      ),
      (
        StudyFinding.UncontrolledFocal(lone),
        Severity.Warning,
        FindingClass.DataDependent,
        Remedy.SupplyControlReference
      )
    )
    expectations.foreach { case (f, severity, category, remedy) =>
      assertEquals((f.severity, f.category, f.remedy), (severity, category, remedy), f.message)
    }
    assertEquals(plan().preflight(None).availability, Availability.Unavailable)
    assertEquals(plan(misframed).preflight(Some(misframed)).availability, Availability.Ready)
    assertEquals(plan(misframed).preflight(Some(misframed)).affectedTrials, Vector(a))
    assertEquals(plan(method = undescribed).preflight(Some(input)).ready, true)
    assertEquals(plan(method = misversion).preflight(Some(input)).ready, true)
    assertEquals(plan(duplicated).preflight(Some(duplicated)).affectedTrials, Vector(a, lone))
  }

  test("recording table: artifact, frame, clock, viewing, synchronization, settings, method") {
    val wrong        = recordingAt(400.0)
    val otherDisplay = get(Frame.screen("other-display", 1000, 1000))
    val otherClock   = ClockId("other-tracker")
    val oneMark      = marks.take(1)
    val duplicateIds =
      Vector(marks(0), get(SyncMark.of("start", Instant.millis(90), Instant.millis(90))))
    val expectedRef = ArtifactRef.of[Recording[Px]](recording.contentHash)
    val table: Vector[
      (
          String,
          RecordingPlan[IvtParameters],
          Option[Recording[Px]],
          Vector[RecordingFinding],
          Option[RecordingPlanError]
      )
    ] =
      Vector(
        (
          "missing artifact",
          recordingPlan(),
          None,
          Vector(RecordingFinding.MissingArtifact(expectedRef)),
          None
        ),
        (
          "wrong artifact",
          recordingPlan(),
          Some(wrong),
          Vector(
            RecordingFinding.ArtifactMismatch(expectedRef, ArtifactRef.of(wrong.contentHash))
          ),
          Some(
            RecordingPlanError.Input(
              PlanError.ArtifactMismatch(expectedRef.digest, wrong.contentHash.render)
            )
          )
        ),
        (
          "incompatible frame",
          recordingPlan(frame = otherDisplay),
          Some(recording),
          Vector(
            RecordingFinding
              .FrameMismatch(source, GeometryError.FrameMismatch(otherDisplay.id, display.id))
          ),
          Some(
            RecordingPlanError.Geometry(
              GeometryError.FrameMismatch(otherDisplay.id, display.id)
            )
          )
        ),
        (
          "incompatible clock",
          recordingPlan(tracker = otherClock),
          Some(recording),
          Vector(
            RecordingFinding
              .ClockMismatch(source, TimeError.ClockMismatch(otherClock, trackerClock))
          ),
          Some(RecordingPlanError.Time(TimeError.ClockMismatch(otherClock, trackerClock)))
        ),
        (
          "absent viewing",
          recordingPlan(viewing = None),
          Some(recording),
          Vector(RecordingFinding.MissingViewing(source)),
          Some(RecordingPlanError.MissingViewing(source))
        ),
        (
          "no common marks",
          recordingPlan(syncMarks = Vector.empty),
          Some(recording),
          Vector(RecordingFinding.MissingSynchronization(trackerClock, analysisClock)),
          Some(RecordingPlanError.MissingSynchronization(trackerClock, analysisClock))
        ),
        (
          "insufficient sync evidence",
          recordingPlan(model = SyncFitMode.Affine, syncMarks = oneMark),
          Some(recording),
          Vector(
            RecordingFinding.Synchronization(
              SyncEvidenceError.TooFewCommonMarks(trackerClock, analysisClock, 1, 2)
            )
          ),
          Some(
            RecordingPlanError.Synchronization(
              SyncEvidenceError.TooFewCommonMarks(trackerClock, analysisClock, 1, 2)
            )
          )
        ),
        (
          "invalid mark setting",
          recordingPlan(syncMarks = duplicateIds),
          Some(recording),
          Vector(
            RecordingFinding.Synchronization(
              SyncEvidenceError.DuplicateMarkId(trackerClock, analysisClock, "start", 0, 1)
            )
          ),
          Some(
            RecordingPlanError.Synchronization(
              SyncEvidenceError.DuplicateMarkId(trackerClock, analysisClock, "start", 0, 1)
            )
          )
        ),
        (
          "invalid detector setting",
          recordingPlan(method = brokenDetector),
          Some(recording),
          Vector(
            RecordingFinding.DetectorDefinition(
              ivtId,
              DetectorDefinitionError
                .Configuration(AlgorithmCards.ivt.id, ivt.parameters(ivtParams))
            )
          ),
          Some(
            RecordingPlanError.DetectorDefinition(
              DetectorDefinitionError.Configuration(
                AlgorithmCards.ivt.id,
                ivt.parameters(ivtParams)
              )
            )
          )
        ),
        (
          "unknown method",
          recordingPlan(method = undescribedDetector),
          Some(recording),
          Vector(RecordingFinding.UndescribedMethod(ivtId)),
          None
        ),
        (
          "unknown method version",
          recordingPlan(method = misversionedDetector),
          Some(recording),
          Vector(
            RecordingFinding.InconsistentDescriptor(
              ivtV2,
              DescriptorError.MethodIdentity(ivtV2, ivtId)
            )
          ),
          None
        )
      )
    table.foreach { case (name, p, available, expected, refusal) =>
      val report = p.preflight(available)
      assertEquals(report.findings, expected, name)
      assertEquals(report.family, RecipeFamily.EventRecording, name)
      assertEquals(report.notChecked, Preflight.recordingUnchecked, name)
      // Cross-check: every deterministic blocker is exactly what run refuses.
      available.foreach(r => assertEquals(p.run(r).left.toOption, refusal, name))
      assertEquals(report.ready, available.nonEmpty && refusal.isEmpty, name)
    }
    assertEquals(
      recordingPlan().preflight(None).blockers.map(_.category),
      Vector(FindingClass.UnavailableInput)
    )
    assertEquals(
      recordingPlan(model = SyncFitMode.Affine, syncMarks = oneMark)
        .preflight(Some(recording))
        .blockers
        .map(f => (f.category, f.remedy)),
      Vector((FindingClass.InvalidSetting, Remedy.ReviseSynchronizationMarks))
    )
    assertEquals(
      recordingPlan(viewing = None).preflight(Some(recording)).blockers.map(_.remedy),
      Vector(Remedy.SupplyViewingGeometry)
    )
    assertEquals(
      recordingPlan(frame = otherDisplay).preflight(Some(recording)).blockers.map(_.category),
      Vector(FindingClass.IncompatibleInput)
    )
    assertEquals(recordingPlan().preflight(Some(recording)).findings, Vector.empty)
    assert(recordingPlan().run(recording).isRight)
  }

  test("temporal table: artifact, anchors, coverage, windows, method and repetition design") {
    type F = TemporalFinding[StudyKey, Px]
    val p       = temporalPlan()
    val shifted = get(
      TemporalStudyInput.of(
        study,
        epochs.map { case (k, e) => k -> TrialEpoch(Instant.micros(1), e.coverage) }
      )
    )
    val noEpochB = get(TemporalStudyInput.of(study, epochs.filterNot(_._1 == b)))
    val wrongClk = get(
      TemporalStudyInput.of(
        study,
        epochs.map { case (k, e) =>
          if k == a then
            k -> TrialEpoch(e.anchor, get(ObservedCoverage.of(ClockId("wrong"), Vector.empty)))
          else k -> e
        }
      )
    )
    val emptyCov = get(
      TemporalStudyInput.of(
        study,
        epochs.map { case (k, e) => if k == a then epoch(a, spans = Vector.empty) else k -> e }
      )
    )
    val overflow = get(
      TemporalStudyInput.of(
        study,
        epochs.map { case (k, e) =>
          if k == a then epoch(a, anchor = Long.MaxValue) else k -> e
        }
      )
    )
    val after = Vector(window("after", 5000, 6000))
    val table: Vector[
      (
          String,
          TemporalStudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference],
          Option[TemporalStudyInput[StudyKey, Px]],
          Vector[F]
      )
    ] =
      Vector(
        ("missing artifact", p, None, Vector(TemporalFinding.MissingArtifact(p.input))),
        (
          "wrong artifact",
          p,
          Some(shifted),
          Vector(TemporalFinding.ArtifactMismatch(p.input, shifted.reference))
        ),
        (
          "missing anchor",
          temporalPlan(noEpochB),
          Some(noEpochB),
          Vector(TemporalFinding.MissingEpoch(b))
        ),
        (
          "coverage on another clock",
          temporalPlan(wrongClk),
          Some(wrongClk),
          Vector(
            TemporalFinding
              .CoverageClock(a, TimeError.ClockMismatch(clock(a), ClockId("wrong")))
          )
        ),
        (
          "no observed coverage",
          temporalPlan(emptyCov),
          Some(emptyCov),
          Vector(
            TemporalFinding.NoObservedCoverage(a, "early"),
            TemporalFinding.NoObservedCoverage(a, "late")
          )
        ),
        (
          "window outside coverage",
          temporalPlan(windows = after),
          Some(temporalInput),
          rows.map(t => TemporalFinding.NoObservedCoverage(t.key, "after"))
        ),
        (
          "window setting overflows anchor",
          temporalPlan(overflow),
          Some(overflow),
          Vector(
            TemporalFinding.WindowResolution(
              a,
              "early",
              TemporalStudyError.AnchorOverflow("early", Long.MaxValue, 0, 500)
            ),
            TemporalFinding.WindowResolution(
              a,
              "late",
              TemporalStudyError.AnchorOverflow("late", Long.MaxValue, 500, 1000)
            )
          )
        ),
        (
          "unknown method",
          temporalPlan(base = plan(study, method = undescribed)),
          Some(temporalInput),
          Vector(TemporalFinding.Study(StudyFinding.UndescribedMethod(DefinitionId.cosine)))
        ),
        (
          "repetition without references",
          temporalPlan(repetitions =
            Vector(get(RepetitionContrast.withinParticipant("reversed", "encode", "missing")))
          ),
          Some(temporalInput),
          Vector(
            TemporalFinding.Repetition(
              "reversed",
              StudyFinding.UnmatchedFocal(ar, UnmatchedKind.Undetermined)
            ),
            // Unmatched focal trials are left out of the control design (S0.7b).
            TemporalFinding.Repetition(
              "reversed",
              StudyFinding.UnmatchedFocal(br, UnmatchedKind.Undetermined)
            )
          )
        )
      )
    table.foreach { case (name, tp, available, expected) =>
      val report = tp.preflight(available)
      assertEquals(report.findings, expected, name)
      assertEquals(report.family, RecipeFamily.TemporalStudy, name)
      assertEquals(report.notChecked, Preflight.temporalUnchecked, name)
    }
    // Ambiguous anchors and repeated trial keys are refused by the input constructor itself.
    assertEquals(
      TemporalStudyInput.of(study, epochs ++ epochs.take(1)).left.toOption,
      Some(TemporalStudyError.DuplicateEpochs(Vector(KeyDigest[StudyKey].digest(a).render)))
    )
    assertEquals(
      TemporalStudyInput.of(StudyInput(Trials(rows ++ rows.take(1))), epochs).left.toOption,
      Some(TemporalStudyError.DuplicateTrials(Vector(KeyDigest[StudyKey].digest(a).render)))
    )
    val missing = temporalPlan(noEpochB).preflight(Some(noEpochB))
    assertEquals(
      missing.warnings.map(f => (f.category, f.remedy)),
      Vector((FindingClass.UnavailableInput, Remedy.SupplyEpoch))
    )
    assertEquals(missing.affectedTrials, Vector(b))
    assertEquals(missing.diagnostics.map(_.affectedTrials), Vector(Vector(b)))
    assertEquals(
      missing.diagnostics.map(_.code.render),
      Vector("temporal-finding.missing-epoch")
    )
    assertEquals(missing.ready, true)
    assertEquals(
      temporalPlan(overflow)
        .preflight(Some(overflow))
        .warnings
        .map(f => (f.category, f.remedy))
        .distinct,
      Vector((FindingClass.InvalidSetting, Remedy.ReviseWindow))
    )
    assertEquals(
      temporalPlan(emptyCov)
        .preflight(Some(emptyCov))
        .warnings
        .map(f => (f.category, f.remedy))
        .distinct,
      Vector((FindingClass.DataDependent, Remedy.AcceptMissingObservation))
    )
  }

  test(
    "study and temporal findings coincide with constructor/run refusals and trial failures"
  ) {
    val p        = plan()
    val reversed = StudyInput(Trials(input.trials.rows.reverse))
    val budget   = get(PairScheduleBudget.of(5, 7L, 10))
    assertEquals(p.prerequisites(None), Vector(PlanError.MissingArtifact(p.input.digest)))
    assertEquals(
      p.prepare(reversed).left.toOption,
      Some(PlanError.ArtifactMismatch(p.input.digest, reversed.reference.digest))
    )
    assertEquals(
      p.prepare(input, budget).left.toOption,
      Some(PlanError.StudyWorkBudget(2, 2, 1, 7L))
    )
    assertEquals(
      plan(method = misversion).inspect.left.toOption,
      Some(DescriptorError.MethodIdentity(cosineV2, DefinitionId.cosine))
    )

    // A frame warning names exactly the trial that fails at every scale when the study runs.
    val misframed = StudyInput(
      Trials(Vector(trial(a, 0.5, other), trial(b, 1.5), trial(ar, 0.5), trial(br, 1.5)))
    )
    val mp = plan(
      misframed,
      Vector(
        StudyEstimate.Binned(),
        StudyEstimate.Gaussian(get(Sigma.px(0.5)), EdgePolicy.Truncate)
      )
    )
    assertEquals(mp.preflight(Some(misframed)).affectedTrials, Vector(a))
    assertEquals(
      mp.preflight(Some(misframed)).diagnostics.flatMap(_.affectedTrials).distinct,
      Vector(a)
    )
    val result = get(mp.run(misframed))
    result.scales.foreach { scale =>
      assertEquals(scale.estimation.collect { case (k, Left(_)) => k }, Vector(a))
    }

    // Temporal: the missing epoch and the anchor overflow appear as the same trial's occupancy failures.
    val noEpochB = get(TemporalStudyInput.of(study, epochs.filterNot(_._1 == b)))
    val cells    = get(temporalPlan(noEpochB).run(noEpochB)).cells
    cells.foreach { cell =>
      assertEquals(
        cell.occupancy.collect { case (k, Left(e)) => k -> e },
        Vector(b -> TemporalStudyError.MissingEpoch(KeyDigest[StudyKey].digest(b).render))
      )
    }
    val overflow = get(
      TemporalStudyInput.of(
        study,
        epochs.map { case (k, e) =>
          if k == a then epoch(a, anchor = Long.MaxValue) else k -> e
        }
      )
    )
    val ocells = get(temporalPlan(overflow).run(overflow)).cells
    assertEquals(
      ocells.map(c => c.occupancy.collect { case (k, Left(e)) => k -> e }),
      Vector(
        Vector(a -> TemporalStudyError.AnchorOverflow("early", Long.MaxValue, 0, 500)),
        Vector(a -> TemporalStudyError.AnchorOverflow("late", Long.MaxValue, 500, 1000))
      )
    )
    val wrongClk = get(
      TemporalStudyInput.of(
        study,
        epochs.map { case (k, e) =>
          if k == a then
            k -> TrialEpoch(e.anchor, get(ObservedCoverage.of(ClockId("wrong"), Vector.empty)))
          else k -> e
        }
      )
    )
    get(temporalPlan(wrongClk).run(wrongClk)).cells.foreach { cell =>
      assertEquals(
        cell.occupancy.collect { case (k, Left(e)) => k -> e },
        Vector(
          a -> TemporalStudyError.Occupancy(
            WindowOccupancyError.Time(TimeError.ClockMismatch(clock(a), ClockId("wrong")))
          )
        )
      )
    }
  }

  test("sentinel methods prove preflight performs no comparison, smoothing or detection") {
    val sentinel = new Sentinel
    val p        = plan(
      method = sentinel.method,
      scales = Vector(
        StudyEstimate.Binned(),
        StudyEstimate.Gaussian(get(Sigma.px(0.5)), EdgePolicy.Truncate)
      )
    )
    val report = p.preflight(Some(input))
    assertEquals(report.ready, true)
    assertEquals((sentinel.factory, sentinel.comparisons), (0, 0))
    val work = get(report.prepare(p, input))
    assertEquals((sentinel.factory, sentinel.comparisons), (0, 0))
    assert(get(work.run).scales.forall(_.contrast.isRight))
    assertEquals(sentinel.factory, 2)
    assert(sentinel.comparisons > 0)

    val temporalSentinel = new Sentinel
    val tp               = temporalPlan(base = plan(study, method = temporalSentinel.method))
    assertEquals(tp.preflight(Some(temporalInput)).ready, true)
    assertEquals((temporalSentinel.factory, temporalSentinel.comparisons), (0, 0))
    assert(tp.run(temporalInput).isRight)
    assert(temporalSentinel.comparisons > 0)

    val detector = new DetectorSentinel
    val rp       = recordingPlan(method = detector.method)
    assertEquals(rp.preflight(Some(recording)).ready, true)
    assertEquals((detector.factory, detector.steps), (1, 0))
    assert(rp.run(recording).isRight)
    assert(detector.steps > 0)
  }

  test("changed input or plan after preflight is rejected; unchanged readiness prepares") {
    val p      = plan()
    val report = p.preflight(Some(input))
    assertEquals(report.availability, Availability.Ready)
    val revised =
      plan(scales = Vector(StudyEstimate.Gaussian(get(Sigma.px(0.5)), EdgePolicy.Truncate)))
    assertEquals(
      report.prepare(revised, input).left.toOption,
      Some(PreflightError.ChangedPlan(RecipeFamily.FixationStudy, p.diff(revised)))
    )
    val reversed = StudyInput(Trials(input.trials.rows.reverse))
    assertEquals(
      report.prepare(p, reversed).left.toOption,
      Some(
        PreflightError.ChangedInput(
          RecipeFamily.FixationStudy,
          input.reference,
          reversed.reference
        )
      )
    )
    assertEquals(
      p.preflight(None).prepare(p, input).left.toOption,
      Some(
        PreflightError.NotReady(
          RecipeFamily.FixationStudy,
          Vector(StudyFinding.MissingArtifact(p.input))
        )
      )
    )
    val work = get(report.prepare(p, input))
    assertEquals(work.description, p.description)
    assert(work.run.isRight)

    val rp      = recordingPlan()
    val rreport = rp.preflight(Some(recording))
    assertEquals(rreport.confirm(rp, recording), Right(()))
    val rrevised = recordingPlan(model = SyncFitMode.Affine)
    assertEquals(
      rreport.confirm(rrevised, recording).left.toOption,
      Some(PreflightError.ChangedPlan(RecipeFamily.EventRecording, rp.diff(rrevised)))
    )
    val wrong = recordingAt(400.0)
    assertEquals(
      rreport.confirm(rp, wrong).left.toOption,
      Some(
        PreflightError.ChangedInput(
          RecipeFamily.EventRecording,
          ArtifactRef.of(recording.contentHash),
          ArtifactRef.of(wrong.contentHash)
        )
      )
    )
    assertEquals(
      rp.preflight(None).confirm(rp, recording).left.toOption,
      Some(
        PreflightError.NotReady(
          RecipeFamily.EventRecording,
          Vector(RecordingFinding.MissingArtifact(rp.input))
        )
      )
    )

    val tp      = temporalPlan()
    val treport = tp.preflight(Some(temporalInput))
    assertEquals(treport.confirm(tp, temporalInput), Right(()))
    val trevised = temporalPlan(windows = windows.take(1))
    assertEquals(
      treport.confirm(trevised, temporalInput).left.toOption,
      Some(PreflightError.ChangedPlan(RecipeFamily.TemporalStudy, tp.diff(trevised)))
    )
    val shifted = get(
      TemporalStudyInput.of(
        study,
        epochs.map { case (k, e) => k -> TrialEpoch(Instant.micros(1), e.coverage) }
      )
    )
    assertEquals(
      treport.confirm(tp, shifted).left.toOption,
      Some(
        PreflightError.ChangedInput(
          RecipeFamily.TemporalStudy,
          temporalInput.reference,
          shifted.reference
        )
      )
    )
  }

  test("report ordering is deterministic and affected keys follow the layout ordering") {
    val mixed = StudyInput(
      Trials(
        Vector(
          trial(lone, 0.5),
          trial(b, 1.5, other),
          trial(a, 0.5),
          trial(a, 0.5),
          trial(ar, 0.5)
        )
      )
    )
    val p        = plan(mixed, method = undescribed)
    val expected = Vector[StudyFinding[StudyKey, Px]](
      StudyFinding.UndescribedMethod(DefinitionId.cosine),
      StudyFinding.FrameMismatch(b, GeometryError.FrameMismatch(frame.id, other.id)),
      StudyFinding.DuplicateTrial(a, PairingSide.Focal, Vector(2, 3)),
      StudyFinding.UnmatchedFocal(lone, UnmatchedKind.Undetermined),
      // A focal trial without a match is left out of the control design
      // (bead S0.7b), so it is not also reported as uncontrolled.
      StudyFinding.UnmatchedFocal(b, UnmatchedKind.Undetermined)
    )
    val first  = p.preflight(Some(mixed))
    val second = p.preflight(Some(mixed))
    assertEquals(first.findings, expected)
    assertEquals(second.findings, first.findings)
    assertEquals(first.affectedTrials, Vector(a, b, lone))
    assertEquals(first.warnings, expected)
    assertEquals(first.blockers, Vector.empty)
    assertEquals(first.findings.map(_.message), second.findings.map(_.message))
  }

  test("data-dependent estimation failures are not checked here and are listed as such") {
    // A zero-width Gaussian on this grid is refused by the smoother at run time
    // only; preflight lists estimation as not checked rather than guessing.
    val narrow = Vector[StudyEstimate[Px]](
      StudyEstimate.Gaussian(get(Sigma.px(1e-9)), eyes4s.surface.EdgePolicy.Truncate)
    )
    val p      = plan(input, scales = narrow)
    val report = p.preflight(Some(input))
    assertEquals(report.findings, Vector.empty)
    assert(report.notChecked.contains(UncheckedAspect.OccupancyEstimation))
    val scale = get(p.run(input)).scales.head
    assert(scale.estimation.forall(_._2.isLeft))
  }

  test("a trial entirely off the screen is reported before running and fails as off-window") {
    val outside = StudyInput(
      Trials(Vector(trial(a, 5.0, y = 5.0), trial(b, 1.5), trial(ar, 0.5), trial(br, 1.5)))
    )
    val p      = plan(outside)
    val report = p.preflight(Some(outside))
    val tally  = get(WindowTally.screen(frame, outside.trials.rows.head.value))
    assertEquals(tally.outsideScreen, 1)
    assertEquals(tally.outsideWindow, 0)
    assertEquals(report.findings, Vector(StudyFinding.NoFixationInWindow(a, tally)))
    assertEquals(report.blockers, Vector.empty)
    val scale = get(p.run(outside)).scales.head
    assertEquals(
      scale.estimation.collect { case (k, Left(f)) => k -> f },
      Vector(a -> StudyFailure.OffWindow(a, tally))
    )
  }

  test("inconsistent descriptors warn while execution proceeds") {
    val sp      = plan(method = misversion)
    val sreport = sp.preflight(Some(input))
    assertEquals(
      sreport.findings,
      Vector(
        StudyFinding.InconsistentDescriptor(
          cosineV2,
          DescriptorError.MethodIdentity(cosineV2, DefinitionId.cosine)
        )
      )
    )
    assertEquals(sreport.blockers, Vector.empty)
    assertEquals(sreport.availability, Availability.Ready)
    assert(sp.inspect.isLeft)
    assert(sp.run(input).isRight)
    assert(get(sreport.prepare(sp, input)).run.isRight)

    val rp      = recordingPlan(method = misversionedDetector)
    val rreport = rp.preflight(Some(recording))
    val absent  = rp.preflight(None)
    assertEquals(
      absent.diagnostics.map(_.code.render),
      absent.findings.map(Diagnostic.of(_).code.render)
    )
    assert(absent.diagnostics.nonEmpty)
    assert(absent.diagnostics.forall(_.affectedTrials.isEmpty))
    assertEquals(
      rreport.findings,
      Vector(
        RecordingFinding.InconsistentDescriptor(
          ivtV2,
          DescriptorError.MethodIdentity(ivtV2, ivtId)
        )
      )
    )
    assertEquals(rreport.blockers, Vector.empty)
    assert(rp.inspect.isLeft)
    assert(rp.run(recording).isRight)
    assertEquals(rreport.confirm(rp, recording), Right(()))
  }

  test("plan prerequisites and preflight blockers agree on the shared cases") {
    val reversed = StudyInput(Trials(input.trials.rows.reverse))
    val sp       = plan()
    Vector(None, Some(reversed), Some(input)).foreach { available =>
      val report        = sp.preflight(available)
      val prerequisites = sp.prerequisites(available)
      assertEquals(report.blockers.size, prerequisites.size, s"study $available")
      assertEquals(report.blockers.nonEmpty, prerequisites.nonEmpty, s"study $available")
    }

    val wrong = recordingAt(400.0)
    val plans = Vector(
      "default"  -> recordingPlan(),
      "frame"    -> recordingPlan(frame = get(Frame.screen("other-display", 1000, 1000))),
      "clock"    -> recordingPlan(tracker = ClockId("other-tracker")),
      "viewing"  -> recordingPlan(viewing = None),
      "no marks" -> recordingPlan(syncMarks = Vector.empty)
    )
    for
      (name, rp) <- plans
      available  <- Vector(None, Some(wrong), Some(recording))
    do
      val report        = rp.preflight(available)
      val prerequisites = rp.prerequisites(available)
      assertEquals(report.blockers.size, prerequisites.size, s"recording $name $available")
      assertEquals(
        report.blockers.nonEmpty,
        prerequisites.nonEmpty,
        s"recording $name $available"
      )
      available.foreach(r => assertEquals(rp.run(r).isLeft, report.blockers.nonEmpty, name))

    val tp      = temporalPlan()
    val shifted = get(
      TemporalStudyInput.of(
        study,
        epochs.map { case (k, e) => k -> TrialEpoch(Instant.micros(1), e.coverage) }
      )
    )
    Vector(None, Some(shifted), Some(temporalInput)).foreach { available =>
      val report        = tp.preflight(available)
      val prerequisites = tp.prerequisites(available)
      assertEquals(report.blockers.size, prerequisites.size, s"temporal $available")
      assertEquals(report.blockers.nonEmpty, prerequisites.nonEmpty, s"temporal $available")
    }
  }

  test("a selected-pair budget exhausted while paging is reported as an over-budget blocker") {
    val budget = get(PairScheduleBudget.of(100, 1000L, 1))
    val p      = plan()
    val work   = get(p.prepare(input, budget))
    val paged  = work.matched.start.advance(PairQuantum.default).left.toOption
    val error  =
      paged.getOrElse(fail("expected the matched schedule to exceed one selected pair"))
    assertEquals(
      error,
      PairScheduleError.SelectedBudget(work.matched.pairSpace.relation, 2L, 1)
    )
    val report = p.preflight(Some(input), budget)
    assertEquals(report.findings, Vector(StudyFinding.OverBudget(BudgetError.Schedule(error))))
    assertEquals(report.blockers.map(_.remedy), Vector(Remedy.RaiseBudgetOrReduceStudy))
    assertEquals(report.blockers.map(_.category), Vector(FindingClass.InvalidSetting))
    assertEquals(
      report.blockers.collect { case StudyFinding.OverBudget(e) => e.plan },
      Vector(PlanError.Schedule(error))
    )
    assertEquals(work.run.left.toOption, Some(PlanError.Schedule(error)))
    assertEquals(
      report.prepare(p, input, budget).left.toOption,
      Some(PreflightError.NotReady(RecipeFamily.FixationStudy, report.blockers))
    )
  }
