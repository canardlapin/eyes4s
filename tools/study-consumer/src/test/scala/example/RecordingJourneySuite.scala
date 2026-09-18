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

import cats.data.NonEmptyVector
import eyes4s.aoi.SampleMembership
import eyes4s.codec.*
import eyes4s.codec.CodecDiagnostics.given
import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.fs2.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json
import scala.compiletime.testing.typeCheckErrors

/** UI-G1, the recording route: a normalized recording input in; recipe
  * discovery through typed descriptors, preflight, a run through
  * `RecordingExecution` with progress and cancellation, inspection back to
  * the input's samples, save under one manifest with the recording roles,
  * reload through a fresh resolver and a rerun bit for bit, on the JVM and
  * Scala.js. Every test runs for the shipped I-VT and for the consumer's own
  * laboratory detector. The detected events are checked against the pinned
  * I-VT conformance fixture (`tools/detector-conformance/reference.json`):
  * fixations from the pymovements oracle, the saccade from its eyes4s
  * expectation.
  */
class RecordingJourneySuite extends munit.CatsEffectSuite:
  import RecordingFixtures.*

  /** Degrees: the angular warp is trigonometric, its output rounds per platform. */
  private val AngularTolerance = 1e-9

  private def get[E, A](value: Either[E, A]): A  = value.fold(e => fail(s"$e"), identity)
  private def named(value: String): ArtifactName = get(ArtifactName.of(value))

  private def refused(reloaded: Either[JourneyError, ?]): NonEmptyVector[ResolveError] =
    reloaded match
      case Left(JourneyError.Resolve(errors)) => errors
      case other                              => fail(s"expected a refusal: $other")

  private def progressOf[P](events: Vector[RecordingEvent[P]]): Vector[RecordingProgress] =
    events.collect { case RunEvent.Advanced(p) => p }

  private def journey[P](run: RecordingCase[P]): Unit =
    import run.{plan, route}
    val name = route.name

    test(s"$name: the detector is discovered through its typed descriptor and card") {
      assert(Preflight.families.contains(RecipeFamily.EventRecording))
      val descriptor = get(route.method.descriptor.toRight("undescribed detector"))
      assertEquals(descriptor.id, route.method.id)
      assertEquals(descriptor.card, AlgorithmCards.ivt)
      assertEquals(descriptor.execution, ExecutionCapability.SynchronousWholeOperation)
      assertEquals(
        descriptor.parameters.values(route.parameters),
        Vector(
          "thresholdDegPerSecond" -> Provenance.Param.Num(thresholdDegPerSecond),
          "minimumDurationMicros" -> Provenance.Param.Text(minimumMicros.toString)
        )
      )
      val inspection = get(plan.inspect)
      assertEquals(inspection.description, plan.description)
      assertEquals(inspection.execution, ExecutionCapability.SynchronousWholeOperation)
      assertEquals(
        inspection.fields.map(_.info.id),
        Vector(
          "input",
          "source",
          "frame",
          "clocks",
          "angularFrame",
          "viewing",
          "syncModel",
          "residualLimitMicros",
          "interpolationGapMicros",
          "detector",
          "detector.minimumDurationMicros",
          "detector.thresholdDegPerSecond",
          "sync.0",
          "sync.1",
          "area.0",
          "area.1"
        )
      )
      assertEquals(
        inspection.fields
          .find(_.info.id == "detector.thresholdDegPerSecond")
          .map(f => (f.info.units, f.info.allowed, f.values)),
        Some(
          (
            ParameterUnits.PerSecond("deg"),
            ParameterDomain.PositiveFinite,
            Vector(Provenance.Param.Num(thresholdDegPerSecond))
          )
        )
      )
      assertEquals(
        inspection.fields.find(_.info.id == "viewing").map(_.values),
        Some(
          Vector(
            Provenance.Param.Num(600.0),
            Provenance.Param.Num(1000.0),
            Provenance.Param.Num(1000.0)
          )
        )
      )
    }

    test(
      s"$name: preflight names each blocker, reports the recipe ready and refuses stale work"
    ) {
      val absent = plan.preflight(None)
      assertEquals(absent.findings, Vector(RecordingFinding.MissingArtifact(plan.input)))
      assertEquals(absent.availability, Availability.Unavailable)
      val missing = absent.findings.head
      assertEquals(
        (missing.severity, missing.category, missing.remedy),
        (Severity.Blocker, FindingClass.UnavailableInput, Remedy.SupplyReferencedArtifact)
      )
      assertEquals(
        Diagnostics.recordingFinding(missing).code.render,
        "recording-finding.missing-artifact"
      )
      assertEquals(
        absent.confirm(plan, recording),
        Left(PreflightError.NotReady(RecipeFamily.EventRecording, Vector(missing)))
      )
      val report = plan.preflight(Some(recording))
      assertEquals(report.findings, Vector.empty)
      assertEquals(report.availability, Availability.Ready)
      assertEquals(report.notChecked, Preflight.recordingUnchecked)
      assertEquals((report.expected, report.available), (plan.input, Some(plan.input)))
      assertEquals(report.confirm(plan, recording), Right(()))
      // A revision made after preflight: the report names the changed field.
      val revised = get(
        route.plan(
          input,
          angular,
          get(RecipeParameters.interpolationGap.parse(Span.micros(5000))),
          areas
        )
      )
      assertEquals(plan.diff(revised).map(_.field), Vector("interpolationGapMicros"))
      assertEquals(
        report.confirm(revised, recording),
        Left(PreflightError.ChangedPlan(RecipeFamily.EventRecording, plan.diff(revised)))
      )
      // Another recording than the one preflight saw.
      val later = get(
        Recording.of(
          display,
          trackerClock,
          recording.rate,
          recording.eye,
          None,
          IArray.from(
            recording.samples.toVector.map(s => s.copy(t = Instant.micros(s.t.toMicros + 1000)))
          )
        )
      )
      val laterRef = ArtifactRef.of[Recording[Px]](later.contentHash)
      assertEquals(
        report.confirm(plan, later),
        Left(PreflightError.ChangedInput(RecipeFamily.EventRecording, plan.input, laterRef))
      )
      assertEquals(
        Diagnostics.preflight(get(report.confirm(plan, later).swap)).code.render,
        "preflight.changed-input"
      )
      // A plan without viewing geometry is blocked with the remedy to supply it.
      val blind = get(
        RecordingPlan.of(
          plan.input,
          source,
          display,
          trackerClock,
          analysisClock,
          angular,
          None,
          SyncFitMode.OffsetOnly,
          marks,
          None,
          gap,
          areas,
          route.method,
          route.parameters
        )
      )
      val unviewed = blind.preflight(Some(recording))
      assertEquals(unviewed.findings, Vector(RecordingFinding.MissingViewing(source)))
      assertEquals(unviewed.blockers.map(_.remedy), Vector(Remedy.SupplyViewingGeometry))
    }

    test(
      s"$name: a run reports progress by segment with exact totals and ends in the pure analysis"
    ) {
      run.events.map { events =>
        val progress = progressOf(events)
        assert(progress.forall(_.run == run.id))
        assertEquals(progress.map(_.step), (1L to progress.size.toLong).toVector)
        val blocks = progress.foldLeft(Vector.empty[Vector[RecordingProgress]]) { (acc, p) =>
          if acc.lastOption.exists(_.head.segment == p.segment) then acc.init :+ (acc.last :+ p)
          else acc :+ Vector(p)
        }
        // One whole step each for synchronization, the warp and the areas; the
        // two machines are fed two samples per step, all ten samples each.
        assertEquals(
          blocks.map(b => (b.head.segment, b.head.segmentTotal, b.last.segmentUnits, b.size)),
          Vector(
            (RecordingSegment.Synchronizing, SegmentTotal.Exact(1), 1L, 1),
            (RecordingSegment.Warping, SegmentTotal.Exact(1), 1L, 1),
            (RecordingSegment.Interpolating, SegmentTotal.Exact(10), 10L, 5),
            (RecordingSegment.Detecting, SegmentTotal.Exact(10), 10L, 5),
            (RecordingSegment.Assigning, SegmentTotal.Exact(1), 1L, 1)
          )
        )
        assertEquals(
          progress.collect { case p if p.segment == RecordingSegment.Detecting => p.stage },
          Vector(0, 2, 4, 6, 8).map(RecordingStage.Detecting(_))
        )
        events.collect { case RunEvent.Finished(o) => o } match
          case Vector(RunOutcome.Completed(id, last, analysis)) =>
            assertEquals((id, last), (run.id, progress.last))
            assert(run.same(analysis, run.pure), "the streamed analysis is the pure run")
          case other => fail(s"expected one completed outcome: $other")
      }
    }

    test(s"$name: the detected events agree with the pinned I-VT conformance fixture") {
      val analysis = run.pure
      assert(run.matchesOracle(analysis, AngularTolerance), s"${run.detected(analysis)}")
      assertEquals(analysis.detection.identity, DetectorIdentity.Algorithm(AlgorithmCards.ivt))
      assertEquals(
        analysis.detection.labels.toVector,
        Vector.fill(3)(SampleClass.Fixation) ++ Vector.fill(3)(SampleClass.Saccade) ++
          Vector.fill(4)(SampleClass.Fixation)
      )
      // Exclusive assignment: the split at 550 px, whatever the warp rounds to.
      assertEquals(
        analysis.assignment.toVector.map {
          case SampleMembership.Areas(ids) => ids.map(_.value).mkString("+")
          case other                       => other.toString
        },
        areasBySample
      )
      assertEquals(analysis.synchronization.usedMarks.size, 2)
      assertEquals(analysis.prepared.samples.toVector, analysis.angular.samples.toVector)
    }

    test(s"$name: cancelling lands between detection chunks and leaves no analysis") {
      for
        events <- run.events
        progress = progressOf(events)
        cancelled   <- run.cancelAfter(9)
        immediately <- run.cancelAfter(0)
        outcome     <- run.runner.start(plan, recording, quanta).use(_.outcome)
      yield
        val (stopped, observed) = cancelled
        assertEquals(stopped, RunOutcome.Cancelled(run.id, Some(progress(8))): run.Outcome)
        assertEquals(progress(8).segment, RecordingSegment.Detecting)
        assertEquals(progress(9).segment, RecordingSegment.Detecting)
        assertEquals(observed, Vector(progress(8)))
        assertEquals(
          immediately,
          (RunOutcome.Cancelled(run.id, None): run.Outcome) -> Vector.empty[RecordingProgress]
        )
        assert(
          run.same(run.completed(outcome), run.pure),
          "a run after a cancellation completes"
        )
    }

    test(s"$name: each event drills down to its samples of the input recording") {
      val view = get(ResultInspection.recording(plan, run.pure))
      assertEquals(view.description, plan.description)
      val entries = view.events.first(get(PageSize.of(PageSize.maximum))).entries
      assertEquals(
        entries.map(e => (e.ref, e.kind, e.span.onset.toMicros, e.span.offset.toMicros)),
        Vector(
          (ResultRef.Event(0, 3), EventKind.Fixation, 0L, 3000L),
          (ResultRef.Event(3, 6), EventKind.Saccade, 3000L, 6000L),
          (ResultRef.Event(6, 10), EventKind.Fixation, 6000L, 10000L)
        )
      )
      assertEquals(
        entries.map(_.source),
        support.map((from, until) => SourceLink.Samples(source, plan.input, from, until))
      )
      assertEquals(view.events.get(ResultRef.Event(3, 6)).map(_.kind), Some(EventKind.Saccade))
      // An analysis inspected against another plan is refused, naming the change.
      val revised = get(
        route.plan(
          input,
          angular,
          get(RecipeParameters.interpolationGap.parse(Span.micros(5000))),
          areas
        )
      )
      assertEquals(
        ResultInspection.recording(revised, run.pure).map(_.description),
        Left(InspectionError.PlanMismatch(revised.diff(plan)))
      )
    }

    test(s"$name: a saved run reloads through a fresh resolver and reruns bit for bit") {
      for
        (analysis, saved) <- run.saved
        files    = run.storage(saved)
        reloaded = get(run.reload(files))
        rerun <- RecordingJourney.rerun(reloaded, quanta)
      yield
        assertEquals(
          saved.manifest.entries.map(e => e.name.value -> e.role),
          Vector(
            RecordingJourney.inputEntry     -> ArtifactRole.RecordingInput,
            RecordingJourney.recordingEntry -> ArtifactRole.Recording
          ) ++ Vector("tMicros", "support", "lineage", "values").map(c =>
            s"${RecordingJourney.recordingEntry}.$c" -> ArtifactRole.Payload
          ) ++ Vector(
            RecordingJourney.planEntry   -> ArtifactRole.RecordingPlan,
            RecordingJourney.resultEntry -> ArtifactRole.RecordingResult
          )
        )
        assertEquals(
          saved.manifest.relations.map(_.kind),
          Vector("recording-of") ++ Vector.fill(4)("payload-of") ++
            Vector("recording-plan-input", "recording-result-of")
        )
        // Identity reconstruction: the semantic digests and every sample exactly.
        assertEquals(reloaded.input.reference, input.reference)
        assertEquals(reloaded.recording.contentHash, recording.contentHash)
        assertEquals(reloaded.recording.samples.toVector, recording.samples.toVector)
        assertEquals(reloaded.plan.diff(plan), Vector.empty)
        assertEquals(RecordingInput.disagreements(reloaded.input, reloaded.plan), Vector.empty)
        assert(run.same(reloaded.analysis, analysis), "the archive decodes to the analysis")
        val again = run.completed(get(rerun))
        assert(run.same(again, analysis), "the rerun reproduces every double")
        val stored = get(
          saved.artifacts
            .find(_.name.value == RecordingJourney.resultEntry)
            .toRight("no result")
        )
        assertEquals(run.archived(again), stored.entry.sha256)
    }

    test(s"$name: a changed, missing or replaced artifact is refused by name, with a code") {
      run.saved.map { (_, saved) =>
        val files  = run.storage(saved)
        val result = RecordingJourney.resultEntry
        val digest = refused(run.reload(files.updated(result, Forgery.flip(files(result), 40))))
        assertEquals(
          digest,
          NonEmptyVector.one(
            ResolveError.Digest(
              named(result),
              get(saved.manifest.entry(named(result)).toRight("no entry")).sha256,
              ByteDigest.sha256(Forgery.flip(files(result), 40))
            )
          )
        )
        assertEquals(Diagnostic.of(digest.head).code.render, "resolve.digest")
        assertEquals(Diagnostic.of(digest.head).subject, Vector(Locus.Entry(result)))
        val payload = s"${RecordingJourney.recordingEntry}.values"
        val missing = refused(run.reload(files - payload))
        assertEquals(missing, NonEmptyVector.one(ResolveError.Missing(named(payload))))
        assertEquals(Diagnostic.of(missing.head).code.render, "resolve.missing")
        // The input replaced by another session's, digests re-declared consistently:
        // it decodes, but not to the identity the manifest names.
        val other = get(
          RecordingInput.of(
            RecordingRef("my.lab.session-02"),
            RecordingChannels.Monocular(recording),
            Some(viewing),
            Some(synchronization)
          )
        )
        val swapped  = get(StoredArtifact.recordingInput(RecordingJourney.inputEntry, other))
        val replaced = refused(
          run.reload(
            Forgery.redeclare(files, saved, RecordingJourney.inputEntry, swapped.bytes)
          )
        )
        assertEquals(
          replaced,
          NonEmptyVector.one(
            ResolveError.Identity(
              named(RecordingJourney.inputEntry),
              input.reference.digest,
              other.reference.digest
            )
          )
        )
        assertEquals(Diagnostic.of(replaced.head).code.render, "resolve.identity")
      }
    }

    test(
      s"$name: an archive naming an unregistered detector is refused by method, not guessed"
    ) {
      run.saved.map { (_, saved) =>
        val files      = run.storage(saved)
        val address    = saved.address
        val source     = SavedRun.source(files.get)
        val planSchema = route.persistence.schema
        // No recording registrations at all: both entries are unsupported schemas.
        val bare = get(ArtifactDecoders.study[Px])
        assertEquals(
          ArtifactResolver.resolve(address, source, bare).left.map(_.toVector),
          Left(
            Vector(
              ResolveError.Decode(
                named(RecordingJourney.planEntry),
                CodecError.UnsupportedSchema("recording-plan", planSchema, Vector.empty)
              ),
              ResolveError.Decode(
                named(RecordingJourney.resultEntry),
                CodecError.UnsupportedSchema(
                  "recording-result",
                  DefinitionId.recordingResult,
                  Vector.empty
                )
              )
            )
          )
        )
        // The other route's registrations: this plan's schema is not one they
        // declare, and no result codec is registered for this detector.
        val other: RecordingRoute[?] =
          if name == "lab" then RecordingFixtures.ivt else RecordingFixtures.lab
        val foreign =
          ArtifactResolver.resolve(address, source, get(other.decoders)).left.map(_.toVector)
        assertEquals(
          foreign,
          Left(
            Vector(
              ResolveError.Decode(
                named(RecordingJourney.planEntry),
                CodecError.UnsupportedSchema(
                  "recording-plan",
                  planSchema,
                  Vector(other.persistence.schema)
                )
              ),
              ResolveError.Decode(
                named(RecordingJourney.resultEntry),
                CodecError.MissingResultCodec(route.method.id)
              )
            )
          )
        )
        val coded = get(foreign.swap).map(e => Diagnostic.of(e))
        assertEquals(
          coded.map(d => d.code.render -> d.causes.map(_.code.render)),
          Vector(
            "resolve.decode" -> Vector("codec.unsupported-schema"),
            "resolve.decode" -> Vector("codec.missing-result-codec")
          )
        )
      }
    }

    test(
      s"$name: an impossible viewing geometry or display frame is refused before any sample"
    ) {
      // Typed construction refuses a non-positive viewing distance outright.
      assertEquals(
        RecipeParameters.perspective.parse((0.0, 1000.0, 1000.0)).left.map(_.underlying),
        Left(
          RecipeParameterError.Geometry(
            GeometryError.NonPositivePerspective(0.0, 1000.0, 1000.0)
          )
        )
      )
      // A saved plan edited to a zero viewing distance is refused on decoding,
      // located at the plan entry; the archive that embeds the plan is untouched.
      val edited = run.saved.map { (_, saved) =>
        val files = run.storage(saved)
        val json  = get(
          io.circe.parser.parse(JourneySetup.utf8(files(RecordingJourney.planEntry)))
        )
        val zero = get(
          json.hcursor
            .downField("value")
            .downField("viewing")
            .downField("distanceMm")
            .withFocus(_ => Json.fromDoubleOrNull(0.0))
            .top
            .toRight("no viewing distance")
        )
        val bytes = IArray.from(zero.spaces2.getBytes("UTF-8"))
        refused(run.reload(Forgery.redeclare(files, saved, RecordingJourney.planEntry, bytes)))
      }
      // A recording on another display than the plan declares is blocked by
      // preflight and refused by the runner before its first step.
      val wide  = get(Frame.screen("recording-display", 1920, 1080))
      val moved = get(
        Recording.of(wide, trackerClock, recording.rate, recording.eye, None, recording.samples)
      )
      val mismatched = get(
        RecordingPlan.of(
          ArtifactRef.of[Recording[Px]](moved.contentHash),
          source,
          display,
          trackerClock,
          analysisClock,
          angular,
          Some(viewing),
          SyncFitMode.OffsetOnly,
          marks,
          None,
          gap,
          areas,
          route.method,
          route.parameters
        )
      )
      val report = mismatched.preflight(Some(moved))
      val frame  = get(Agreement.frames(display, wide).swap)
      assertEquals(report.findings, Vector(RecordingFinding.FrameMismatch(source, frame)))
      assertEquals(
        report.findings.map(f => (f.severity, f.category, f.remedy)),
        Vector((Severity.Blocker, FindingClass.IncompatibleInput, Remedy.AlignFrame))
      )
      assertEquals(
        report.confirm(mismatched, moved),
        Left(PreflightError.NotReady(RecipeFamily.EventRecording, report.findings))
      )
      for
        errors  <- edited
        outcome <- run.runner.start(mismatched, moved, quanta).use(_.outcome)
      yield
        assertEquals(
          errors.map(_.entryName.map(_.value)),
          NonEmptyVector.one(Some(RecordingJourney.planEntry))
        )
        errors.head match
          case ResolveError.Decode(_, codec) =>
            Forgery.leaf(codec)._2 match
              case CodecError.Field("perspective", _, _) => ()
              case other => fail(s"expected the perspective field: $other")
          case other => fail(s"expected a decoding refusal: $other")
        assertEquals(
          Diagnostic.of(errors.head).causes.map(_.code.render),
          Vector("codec.field")
        )
        outcome match
          case RunOutcome.Failed(id, error, last) =>
            assertEquals((id, last), (RecordingRunId.of(mismatched, quanta), None))
            assertEquals(error, RecordingPlanError.Geometry(frame))
            val coded = Diagnostic.of(error)
            assertEquals(coded.code.render, "recording-plan.geometry")
            // The recording names the plan's display with other bounds.
            assertEquals(
              coded.causes.map(_.code.render),
              Vector("geometry.frame-identity-conflict")
            )
          case other => fail(s"expected a failed outcome before any step: $other")
    }

    test(
      s"$name: marks the plan's residual limit rejects fail synchronization, as coded evidence"
    ) {
      val drifting = Vector(
        get(SyncMark.of("start", Instant.millis(0), Instant.millis(0))),
        get(SyncMark.of("end", Instant.millis(9), Instant.millis(10)))
      )
      val strict = get(
        RecordingPlan.of(
          plan.input,
          source,
          display,
          trackerClock,
          analysisClock,
          angular,
          Some(viewing),
          SyncFitMode.OffsetOnly,
          drifting,
          Some(get(SyncResidualLimit.of(Span.micros(0)))),
          gap,
          areas,
          route.method,
          route.parameters
        )
      )
      val report = strict.preflight(Some(recording))
      // Derived by hand: an offset-only fit of 0 -> 0 ms and 9 -> 10 ms has an
      // offset of 500 us and residuals of -500 and +500 us, so a 0 us limit
      // rejects both marks and none of the one an offset needs is retained.
      val cause = SyncEvidenceError.TooFewRetainedMarks(trackerClock, analysisClock, 2, 0, 1)
      assertEquals(
        Diagnostics.syncEvidence(cause).code.render,
        "sync-evidence.too-few-retained-marks"
      )
      assertEquals(report.findings, Vector(RecordingFinding.Synchronization(cause)))
      assertEquals(report.blockers.map(_.remedy), Vector(Remedy.ReviseSynchronizationMarks))
      // The plan disagrees with the input's evidence, so it could not be saved beside it.
      assertEquals(
        RecordingInput
          .disagreements(input, strict)
          .map(_ match
            case RecordingInputError.PlanDisagreement(field, _, _) => field
            case other                                             => other.toString),
        Vector("marks", "residualLimit")
      )
      run.runner.start(strict, recording, quanta).use(_.outcome).map {
        case RunOutcome.Failed(id, error, last) =>
          assertEquals((id, last), (RecordingRunId.of(strict, quanta), None))
          assertEquals(error, RecordingPlanError.Synchronization(cause))
          assertEquals(Diagnostic.of(error).code.render, "recording-plan.synchronization")
        case other => fail(s"expected a failed outcome: $other")
      }
    }

    test(s"$name: emit portable recording evidence for the JVM and Scala.js comparison") {
      for
        events            <- run.events
        (analysis, saved) <- run.saved
        cancelled         <- run.cancelAfter(9)
      yield
        val progress = progressOf(events)
        val reloaded = get(run.reload(run.storage(saved)))
        val series   = reloaded.analysis.detection.eventSeries
        val runtime  = if System.getProperty("java.vm.name") == "Scala.js" then "js" else "jvm"
        println(
          "EYES4S_RECORDING_JOURNEY=" + Json
            .obj(
              "runtime"   -> Json.fromString(runtime),
              "route"     -> Json.fromString(name),
              "method"    -> Json.fromString(route.method.id.name),
              "input"     -> Json.fromString(reloaded.input.reference.digest),
              "recording" -> Json.fromString(reloaded.recording.contentHash.render),
              "plan"      -> get(route.persistence.codec.encode(reloaded.plan)),
              "events"    -> Json.arr(run.detected(reloaded.analysis).map {
                case Expected.Fixation(on, off, x, y) =>
                  Json.obj(
                    "kind"         -> Json.fromString("fixation"),
                    "onsetMicros"  -> Json.fromLong(on),
                    "offsetMicros" -> Json.fromLong(off),
                    "degrees" -> Json.arr(Json.fromDoubleOrNull(x), Json.fromDoubleOrNull(y))
                  )
                case Expected.Saccade(on, off, from, to) =>
                  Json.obj(
                    "kind"         -> Json.fromString("saccade"),
                    "onsetMicros"  -> Json.fromLong(on),
                    "offsetMicros" -> Json.fromLong(off),
                    "degrees"      -> Json
                      .arr(Json.fromDoubleOrNull(from), Json.fromDoubleOrNull(to))
                  )
              }*),
              "support" -> Json.arr(
                series.support.map(r => Json.arr(Json.fromInt(r.from), Json.fromInt(r.until)))*
              ),
              "labels" -> Json.arr(
                reloaded.analysis.detection.labels.toVector
                  .map(l => Json.fromString(l.toString))*
              ),
              "areas" -> Json.arr(
                reloaded.analysis.assignment.toVector.map {
                  case SampleMembership.Areas(ids) =>
                    Json.fromString(ids.map(_.value).mkString("+"))
                  case other => Json.fromString(other.toString)
                }*
              ),
              "segments" -> Json.arr(
                progress.groupBy(_.segment).toVector.sortBy(_._2.head.step).map {
                  (segment, steps) =>
                    Json.obj(
                      "segment" -> Json.fromString(segment.toString),
                      "total"   -> Json.fromString(steps.head.segmentTotal.toString),
                      "units"   -> Json.fromLong(steps.last.segmentUnits),
                      "steps"   -> Json.fromInt(steps.size)
                    )
                }*
              ),
              "cancelled" -> cancelled._1.progress.fold(Json.Null)(p =>
                Json.obj(
                  "step"    -> Json.fromLong(p.step),
                  "segment" -> Json.fromString(p.segment.toString),
                  "stage"   -> Json.fromString(p.stage.toString)
                )
              ),
              "fingerprint_size" -> Json.fromInt(RecordingJourney.fingerprint(analysis).size),
              "archive_sha256"   -> Json.fromString(run.archived(analysis).hex)
            )
            .noSpaces
        )
    }

  journey(RecordingRun.ivt)
  journey(RecordingRun.lab)

  test(
    "the laboratory detector detects exactly what the shipped I-VT detects, under its own identity"
  ) {
    val (shipped, lab) = (RecordingRun.ivt.pure, RecordingRun.lab.pure)
    assertEquals(lab.detection.eventSeries.events, shipped.detection.eventSeries.events)
    assertEquals(lab.detection.labels.toVector, shipped.detection.labels.toVector)
    assertEquals(lab.assignment.toVector, shipped.assignment.toVector)
    assertEquals(
      PlanChange
        .between(RecordingRun.ivt.plan.description, RecordingRun.lab.plan.description)
        .map(_.field),
      Vector("detector")
    )
  }

  test("the laboratory detector's typed fields refuse bad input with its own errors") {
    val threshold = get(CustomDetector.thresholdField)
    val minimum   = get(CustomDetector.minimumField)
    assert(threshold.parse(-1.0).isLeft)
    assert(minimum.parse(0L).isLeft)
    assertEquals(threshold.parse(2000.0).map(_.velocity.value), Right(2000.0))
    assertEquals(
      threshold.parse(Double.NaN).left.map(_.field.id),
      Left("thresholdDegPerSecond")
    )
    // Each refused for the raw type it was given, not for another reason.
    val text = typeCheckErrors("""
      import example.*
      CustomDetector.thresholdField.toOption.get.construct("fast")
    """)
    assert(
      text.exists(e => e.message.contains("String") && e.message.contains("Double")),
      text.map(_.message)
    )
    val fraction = typeCheckErrors("""
      import example.*
      CustomDetector.minimumField.toOption.get.construct(2.5)
    """)
    assert(
      fraction.exists(e => e.message.contains("Double") && e.message.contains("Long")),
      fraction.map(_.message)
    )
  }

  test("an undescribed laboratory detector still runs; preflight names it as a warning only") {
    val undescribed = CustomDetector.method(get(DefinitionId.of("my.lab.undescribed-ivt", 1)))
    val plan        = get(
      RecordingPlan.of(
        RecordingRun.lab.plan.input,
        source,
        display,
        trackerClock,
        analysisClock,
        angular,
        Some(viewing),
        SyncFitMode.OffsetOnly,
        marks,
        None,
        gap,
        areas,
        undescribed,
        RecordingFixtures.lab.parameters
      )
    )
    val report = plan.preflight(Some(recording))
    assertEquals(report.findings, Vector(RecordingFinding.UndescribedMethod(undescribed.id)))
    assertEquals(report.availability, Availability.Ready)
    assertEquals(report.warnings.map(_.remedy), Vector(Remedy.RegisterMethodDescriptor))
    assertEquals(
      plan.inspect.left.toOption,
      Some(DescriptorError.MissingMethod(undescribed.id))
    )
    assert(RecordingRun.lab.matchesOracle(get(plan.run(recording)), AngularTolerance))
  }
