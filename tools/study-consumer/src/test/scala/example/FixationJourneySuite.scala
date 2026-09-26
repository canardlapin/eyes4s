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
import cats.effect.IO
import cats.syntax.all.*
import eyes4s.codec.*
import eyes4s.codec.CodecDiagnostics.given
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.fs2.*
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.laws.{StudyResultEquivalence, Tolerance}
import eyes4s.plan.*
import eyes4s.surface.EstimateError
import io.circe.Json

import java.nio.charset.StandardCharsets

/** UI-G0: the fixation-only journey an application makes, headless, through
  * packaged eyes4s artifacts only, on the JVM and Scala.js: a fixation table
  * in; recipe discovery, preflight and the pair schedule; a run with
  * progress, cancellation and a completed outcome; save, reload through a
  * fresh resolver and rerun bit for bit; and result inspection back to CSV
  * records, with typed diagnostics for a bad table and a corrupted archive.
  *
  * Every test runs for the shipped cosine route and for the consumer's own
  * scaled cosine with its custom key and score types. Every assertion is on
  * typed values; diagnostic codes are compared as the catalog's stable
  * identities, never messages.
  */
class FixationJourneySuite extends munit.CatsEffectSuite:
  import JourneyFixtures.*
  import JourneySetup.{comparison, gaussian, grid, pairs, quanta, scales, sigmas}

  private val Oracle = Tolerance(absolute = 1e-12, relative = 0)

  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)

  private val cosine = Journey(JourneyCases.cosine)
  private val scaled = Journey(JourneyCases.scaled)

  // ---------------------------------------------------------------------------
  // The journey, for each route
  // ---------------------------------------------------------------------------

  private def journey[K, P, S, D](j: Journey[K, P, S, D]): Unit =
    import j.{c, route}
    val name = c.name

    test(s"$name: a fixation table imports to typed trials and a complete admission ledger") {
      assertEquals(j.imported.rejected, Vector.empty)
      assertEquals(j.ledger.outcome, AdmissionOutcome.Complete)
      assertEquals(j.ledger.records.map(_.record), (2 to 49).toVector)
      assert(j.ledger.records.forall(_.isAdmitted))
      assertEquals(j.ledger.source, FixationEvidence.source("journey.csv", j.imported))
      assertEquals(j.ledger.checkAgainst(j.input), Right(()))
      val expected = for
        phase       <- Vector("encode", "recall")
        participant <- Vector("s1", "s2")
        image       <- Vector("a", "b", "c")
      yield j.key(participant, image, phase)
      assertEquals(
        j.input.trials.rows.map(_.key).sorted(using route.layout.ordering),
        expected
          .sorted(using route.layout.ordering)
      )
      // Integer microseconds keep all 64 bits: onsets beyond 2^53 are exact.
      val late = get(
        j.input.trials.rows.find(_.key == j.key("s2", "c", "recall")).toRight("no late trial")
      )
      assertEquals(
        late.value.fixations.toVector.map(_.span.onset.toMicros),
        onsets("s2", "c", "recall")
      )
      assertEquals(late.value.fixations.head.span.onset.toMicros, JourneyFixtures.late)
    }

    test(s"$name: the recipe is discovered and its typed parameters inspected") {
      assert(Preflight.families.contains(RecipeFamily.FixationStudy))
      val descriptor = get(route.method.descriptor.toRight("undescribed method"))
      assertEquals(descriptor.id, route.method.id)
      assertEquals(descriptor.execution, ExecutionCapability.BoundedComparison)
      assertEquals(route.method.capability, ExecutionCapability.BoundedComparison)
      assertEquals(
        get(descriptor.components(route.parameters)).map(c => c.id -> c.range),
        Vector("value" -> MeasureScale.Bounded(0, c.multiplier))
      )
      // Checked constructors refuse by the domain's own typed error.
      assertEquals(
        RecipeParameters.sigma[Px].parse(-1.0).left.map(_.underlying),
        Left(RecipeParameterError.Geometry(GeometryError.NonPositiveSigma(-1.0)))
      )
      val inspection = get(j.study.inspect)
      assertEquals(inspection.description, j.study.description)
      assertEquals(inspection.execution, ExecutionCapability.BoundedComparison)
      assertEquals(
        inspection.fields.map(_.info.id),
        Vector(
          "input",
          "layout",
          "method",
          "phases",
          "weight",
          "failurePolicy",
          "frame",
          "grid"
        ) ++
          c.methodFields ++ scales.indices.map(i => s"estimate.$i")
      )
      assertEquals(
        inspection.fields.find(_.info.id == "estimate.1").map(_.children.map(ch => ch.info.id)),
        Some(Vector("sigma", "edges"))
      )
      assertEquals(
        inspection.fields
          .find(_.info.id == "estimate.1")
          .flatMap(_.children.headOption)
          .map(ch => ch.info.quantity -> ch.values),
        Some(Quantity.Planar(PlanarUnit.Px) -> Vector(Provenance.Param.Num(0.5)))
      )
      c.methodFields.foreach { field =>
        assertEquals(
          inspection.fields.find(_.info.id == field).map(f => f.info.version -> f.values),
          Some(2 -> Vector(Provenance.Param.Num(c.multiplier)))
        )
      }
    }

    test(s"$name: preflight names each blocker and then reports the recipe ready") {
      val study = j.study
      // Nothing supplied yet: the referenced input is missing, and that blocks.
      val absent = study.preflight(None, pairs)
      assertEquals(absent.availability, Availability.Unavailable)
      assertEquals(absent.findings, Vector(StudyFinding.MissingArtifact(study.input)))
      val missing = absent.findings.head
      assertEquals(
        (missing.severity, missing.category, missing.remedy),
        (Severity.Blocker, FindingClass.UnavailableInput, Remedy.SupplyReferencedArtifact)
      )
      val coded = Diagnostic.of(missing)
      assertEquals(coded.code.render, "study-finding.missing-artifact")
      assertEquals(
        (coded.category, coded.remedy),
        (Some(missing.category), Some(missing.remedy))
      )
      assertEquals(
        absent.prepare(study, j.input, pairs).left.toOption,
        Some(PreflightError.NotReady(RecipeFamily.FixationStudy, Vector(missing)))
      )
      // A candidate budget below the study's conservative count blocks before any pairing.
      val tight = get(PairScheduleBudget.of(64, 256, 64))
      assertEquals(
        study.preflight(Some(j.input), tight).findings,
        Vector(StudyFinding.OverBudget(BudgetError.CandidateVisits(6, 6, 4, 256)))
      )
      assertEquals(
        study.prepare(j.input, tight).left.toOption,
        Some(PlanError.StudyWorkBudget(6, 6, 4, 256))
      )
      // A selected-pair budget the control design exceeds is refused while paging.
      val selected = study.preflight(Some(j.input), get(PairScheduleBudget.of(64, 512, 8)))
      assertEquals(selected.availability, Availability.Unavailable)
      // The matched design selects 6 pairs; the control design's ninth is refused.
      assertEquals(
        selected.findings.collect {
          case StudyFinding.OverBudget(
                BudgetError.Schedule(PairScheduleError.SelectedBudget(_, attempted, maximum))
              ) =>
            attempted -> maximum
        },
        Vector(9L -> 8)
      )
      assertEquals(
        selected.findings.map(Diagnostic.of(_).code.render),
        Vector("study-finding.over-budget")
      )
      // The explicit working budget: ready, with the execution-only aspects named.
      val report = study.preflight(Some(j.input), pairs)
      assertEquals(report.findings, Vector.empty)
      assertEquals(report.availability, Availability.Ready)
      assertEquals(report.notChecked, Preflight.studyUnchecked)
      assertEquals((report.expected, report.available), (study.input, Some(j.input.reference)))
      // A stale report refuses a plan revised since it was made, naming the change.
      val revised = j.plan(j.input, weighting = Weight.Uniform)
      assertEquals(
        report.prepare(revised, j.input, pairs).left.toOption,
        Some(PreflightError.ChangedPlan(RecipeFamily.FixationStudy, study.diff(revised)))
      )
      assertEquals(study.diff(revised).map(_.field), Vector("weight"))
      assert(report.prepare(study, j.input, pairs).isRight)
    }

    test(s"$name: the pair schedule previews both designs, page by page, without scoring") {
      val preview = get(j.work.preview)
      val trials  = j.input.trials.rows.map(_.key)
      val recall  = trials.filter(k => c.label(k).endsWith("/recall"))
      val encode  = trials.filter(k => c.label(k).endsWith("/encode"))
      assertEquals(preview.focalKeys, recall)
      assertEquals(preview.referenceKeys, encode)
      assertEquals(preview.excludedPhases, Vector.empty)
      assertEquals(preview.failurePolicy, FailurePolicy.RequireAll)
      assertEquals(preview.reductionOrientation, ReductionOrientation.ByLeft)
      assertEquals(preview.inputReference, j.input.reference)
      assertEquals(preview.description, j.study.description)
      // An independent edge list: same participant, same or other image.
      def split(k: K)             = c.label(k).split('/').toVector
      def edges(matched: Boolean) = for
        f <- recall
        r <- encode
        if split(f)(0) == split(r)(0) && (split(f)(1) == split(r)(1)) == matched
      yield f -> r
      val quantum = get(PairQuantum.of(4))
      def page(
          cursor: PairCursor[K, K],
          seen: Vector[ScheduledPair[K, K]],
          pages: Int
      ): (Vector[ScheduledPair[K, K]], PairingReport[K, K], Int) =
        get(cursor.advance(quantum)) match
          case PairPage.More(found, _, next)   => page(next, seen ++ found, pages + 1)
          case PairPage.Done(found, _, report) => (seen ++ found, report, pages + 1)
      for (schedule, matched, count) <- Vector(
          (preview.matched, true, 6),
          (preview.controls, false, 12)
        )
      do
        assertEquals(schedule.candidatePairCount, 36L)
        val (found, report, pages) = page(schedule.start, Vector.empty, 0)
        assertEquals(found.map(p => p.left -> p.right), edges(matched))
        assertEquals(
          found.map(p => recall(p.leftIndex) -> encode(p.rightIndex)),
          edges(matched)
        )
        assertEquals(
          (report.eligiblePairCount, report.selectedPairCount),
          (count.toLong, count)
        )
        assertEquals((report.unmatchedLeft, report.ambiguous), (Vector.empty, Vector.empty))
        assert(pages > 1, "the schedule is paged")
      // A preview kept across an edit is refused as stale; the current one passes.
      val revised = j.plan(j.input, weighting = Weight.Uniform)
      assertEquals(
        preview.checkCurrent(revised, j.input),
        Left(PlanError.ChangedPreparedPlan(route.method.id, route.layout.id))
      )
      assertEquals(preview.checkCurrent(j.study, j.input), Right(()))
    }

    test(s"$name: a run reports progress by segment with stated totals") {
      j.events
        .map { events =>
          val progress = events.collect { case StudyEvent.Advanced(p) => p }
          val finished = events.collect { case StudyEvent.Finished(o) => o }
          val id       = StudyRunId.of(j.work, quanta)
          assert(progress.forall(_.run == id))
          assertEquals(progress.map(_.step), (1L to progress.size.toLong).toVector)
          assertEquals(progress.last.totalUnits, progress.map(_.stepUnits.toLong).sum)
          // Segments in order, each one contiguous block stating one total.
          val blocks = progress.foldLeft(Vector.empty[Vector[StudyProgress]]) { (acc, p) =>
            if acc.lastOption.exists(_.head.segment == p.segment) then
              acc.init :+ (acc.last :+ p)
            else acc :+ Vector(p)
          }
          val order = scales.indices.toVector.flatMap { s =>
            Vector(
              StudySegment.Estimating(s),
              StudySegment.Comparing(s, StudyDesign.Matched),
              StudySegment.Reducing(s, StudyDesign.Matched),
              StudySegment.Comparing(s, StudyDesign.Control),
              StudySegment.Reducing(s, StudyDesign.Control),
              StudySegment.Contrasting(s)
            )
          }
          assertEquals(blocks.map(_.head.segment), order)
          assert(blocks.forall(b => b.map(_.segmentTotal).distinct.size == 1))
          assert(
            blocks.forall(b =>
              b.map(_.segmentUnits) == b.map(_.stepUnits.toLong).scanLeft(0L)(_ + _).tail
            )
          )
          // Totals as documented. Estimation: one unit per trial. Comparison: the
          // bound counts every candidate pair (36) twice plus its cells, and each
          // focal and reference key; paging actually visits the references in the
          // focal key's participant block (the relation's first equality), then
          // every reference key for the unmatched report, and each selected pair
          // costs one unit to begin plus one per cell. Reduction: one unit per
          // contribution and per reduced key. Contrast: at most one per focal key.
          def participant(k: K) = c.label(k).takeWhile(_ != '/')
          val trials            = j.input.trials.rows.map(_.key)
          val focal             = trials.filter(c.label(_).endsWith("/recall"))
          val refs              = trials.filter(c.label(_).endsWith("/encode"))
          val blocked  = focal.map(f => refs.count(participant(_) == participant(f))).sum.toLong
          val perPair  = 1L + grid.size
          val bound    = 36L * (2 + grid.size) + focal.size + refs.size
          val expected = scales.indices.toVector.flatMap { _ =>
            Vector(
              SegmentTotal.Exact(12)      -> 12L,
              SegmentTotal.AtMost(bound)  -> (blocked + refs.size + 6 * perPair),
              SegmentTotal.Exact(6 + 6)   -> 12L,
              SegmentTotal.AtMost(bound)  -> (blocked + refs.size + 12 * perPair),
              SegmentTotal.Exact(12 + 12) -> 24L,
              SegmentTotal.AtMost(6)      -> 6L
            )
          }
          assertEquals(blocked, 18L)
          assertEquals(blocks.map(b => b.head.segmentTotal -> b.last.segmentUnits), expected)
          assertEquals(
            StudyExecution.total(j.work, StudySegment.Reducing(0, StudyDesign.Matched)),
            SegmentTotal.Unknown
          )
          finished match
            case Vector(RunOutcome.Completed(run, last, result)) =>
              assertEquals(run, id)
              assertEquals(last, progress.last)
              assert(j.same(result, get(j.work.run)), "the streamed result is the pure run")
            case other => fail(s"expected one completed outcome: $other")
          progress
        }
        .flatMap { progress =>
          // A UI observes a started run through its progress stream. While the
          // run is held between steps 20 and 21, a new observer receives step
          // 20 exactly as the pull sequence states it; released, the run
          // completes and a late observer ends on the last step.
          val held = 20
          for
            (seen, outcome) <- j.observeParked(held)
            late            <- j.runner.start(j.work, comparison, quanta).use { run =>
              run.outcome >> run.progress.compile.toVector
            }
          yield
            assertEquals(seen, Some(progress(held - 1)))
            assertEquals(outcome.progress, progress.lastOption)
            assert(j.same(j.completed(outcome), get(j.work.run)))
            assertEquals(late, Vector(progress.last))
        }
    }

    test(
      s"$name: cancelling a run lands between steps and leaves no result; a rerun completes"
    ) {
      for
        events <- j.events
        progress = events.collect { case StudyEvent.Advanced(p) => p }
        id       = StudyRunId.of(j.work, quanta)
        // Mid-way through the first scale's matched comparison.
        mid = 14
        cancelled   <- j.cancelAfter(mid)
        immediately <- j.cancelAfter(0)
        outcome     <- j.runner.start(j.work, comparison, quanta).use(_.outcome)
        failed <- j.runner.start(j.work, get(ComparisonBudget.of(3)), quanta).use(_.outcome)
        // Eight selected pairs per design: the matched design fits, the
        // control design's ninth pair is refused while the run pages it.
        paged = get(j.study.prepare(j.input, get(PairScheduleBudget.of(64, 512, 8))))
        refused <- j.runner.start(paged, comparison, quanta).use(_.outcome)
      yield
        val (stopped, observed) = cancelled
        assertEquals(stopped, RunOutcome.Cancelled(id, Some(progress(mid - 1))): j.Outcome)
        assertEquals(progress(mid - 1).segment, StudySegment.Comparing(0, StudyDesign.Matched))
        assert(progress(mid).segment == progress(mid - 1).segment, "cancelled inside a segment")
        assertEquals(observed, Vector(progress(mid - 1)))
        assertEquals(
          immediately,
          (RunOutcome.Cancelled(id, None): j.Outcome) -> Vector.empty[StudyProgress]
        )
        assertEquals(outcome.progress, progress.lastOption)
        assert(j.same(j.completed(outcome), get(j.work.run)))
        // A comparison budget below one comparison's cells fails before any step.
        failed match
          case RunOutcome.Failed(run, error, last) =>
            assertEquals((run, last), (id, None))
            assert(error match
              case PlanError.ComparisonWork(ComparisonWorkError.WorkBudget(_, 4L, 3L)) => true
              case _                                                                   => false)
            val coded = Diagnostic.of(error)
            assertEquals(coded.code.render, "plan.comparison-work")
            assertEquals(coded.causes.map(_.code.render), Vector("comparison-work.work-budget"))
          case other => fail(s"expected a failed outcome: $other")
        // A failure mid-run keeps the last committed step as its evidence.
        refused match
          case RunOutcome.Failed(run, error, Some(last)) =>
            assertEquals(run, StudyRunId.of(paged, quanta))
            assertEquals(last, progress(last.step.toInt - 1))
            assertEquals(last.segment, StudySegment.Comparing(0, StudyDesign.Control))
            assert(error match
              case PlanError.Schedule(PairScheduleError.SelectedBudget(_, 9L, 8)) => true
              case _                                                              => false)
            assertEquals(Diagnostic.of(error).code.render, "plan.schedule")
            assertEquals(
              Diagnostic.of(error).causes.map(_.code.render),
              Vector("pair-schedule.selected-budget")
            )
          case other => fail(s"expected a failed outcome after some steps: $other")
    }

    test(s"$name: the run agrees with the rational cosine and decimal Gaussian oracles") {
      j.saved.map { (result, _) =>
        val m = c.multiplier
        assertEquals(result.scales.size, 4)
        val matched = j.reductions(result, 0, StudyDesign.Matched).toMap
        val control = j.reductions(result, 0, StudyDesign.Control).toMap
        val binned  = j.contrasts(result, 0)
        assertEquals(binned.map(_._1), JourneyFixtures.binned.map(_._1))
        JourneyFixtures.binned.foreach { case (label, (mean, controls, difference)) =>
          assert(Oracle.approxEquals(matched(label), m * mean.value), label)
          assert(Oracle.approxEquals(control(label), m * controls.value), label)
          assert(Oracle.approxEquals(binned.toMap.apply(label), m * difference.value), label)
        }
        JourneyFixtures.gaussian.zipWithIndex.foreach { case ((sigma, targets), i) =>
          assertEquals(result.scales(i + 1).estimate, gaussian(sigma))
          val found = j.contrasts(result, i + 1).toMap
          assertEquals(found.keySet, targets.keySet)
          targets.foreach((label, target) =>
            assert(Oracle.approxEquals(found(label), m * target), s"$sigma $label")
          )
        }
        // Every reduction keeps its denominators: one matched, two controls.
        val scale = result.scales.head.analyses
        assert(
          scale
            .reduced(StudyDesign.Matched)
            .entries
            .forall(r => (r.successful, r.failed, r.contributing) == (1, 0, 1))
        )
        assert(
          scale
            .reduced(StudyDesign.Control)
            .entries
            .forall(r => (r.successful, r.failed, r.contributing) == (2, 0, 2))
        )
      }
    }

    test(s"$name: a saved run reloads through a fresh resolver and reruns bit for bit") {
      for
        (result, saved) <- j.saved
        files    = j.storage(saved)
        reloaded = get(j.reload(files))
        rerun <- FixationJourney.rerun(reloaded, pairs, comparison, quanta)
      yield
        // Identity reconstruction: exact keys, 64-bit times and semantic digests.
        assertEquals(reloaded.input.reference, j.input.reference)
        assertEquals(reloaded.input.trials.rows.map(_.key), j.input.trials.rows.map(_.key))
        assertEquals(
          reloaded.input.trials.rows.map(
            _.value.fixations.toVector.map(f => f.span.onset.toMicros -> f.span.offset.toMicros)
          ),
          j.input.trials.rows.map(
            _.value.fixations.toVector.map(f => f.span.onset.toMicros -> f.span.offset.toMicros)
          )
        )
        assertEquals(reloaded.ledger, j.ledger)
        assertEquals(reloaded.plan.description, j.study.description)
        assertEquals(reloaded.plan.diff(j.study), Vector.empty)
        assert(j.same(reloaded.result, result), "the archive decodes to the run's result")
        val work = get(reloaded.plan.prepare(reloaded.input, pairs))
        assertEquals(StudyRunId.of(work, quanta), StudyRunId.of(j.work, quanta))
        val again = j.completed(get(rerun))
        assert(j.same(again, result), "the rerun reproduces every double")
        // The rerun's canonical archive is the stored entry, byte for byte.
        val stored = get(
          saved.artifacts.find(_.name.value == FixationJourney.resultEntry).toRight("no result")
        )
        assertEquals(
          get(StoredArtifact.result("rerun", route.results, again)).entry.sha256,
          stored.entry.sha256
        )
    }

    test(s"$name: a contrast row drills down to the CSV records that supplied it") {
      for (_, saved) <- j.saved
      yield
        val study = get(j.reload(j.storage(saved)))
        val view  =
          get(ResultInspection.study(study.plan, study.result, study.input, Some(study.ledger)))
        val focal      = j.key("s1", "a", "recall")
        val row        = get(view.contrastRow(ResultRef.ContrastRow(0, focal)))
        val difference = get(row.outcome)
        assertEquals(difference.components.map(_.id), Vector("value"))
        assert(Oracle.approxEquals(difference.components.head.value, c.multiplier * 7 / 25))
        assertEquals(row.matched, Some(ResultRef.Reduction(0, StudyDesign.Matched, focal)))
        assertEquals(row.control, Some(ResultRef.Reduction(0, StudyDesign.Control, focal)))
        val matched = get(view.reduction(get(row.matched.toRight("no matched operand"))))
        assertEquals(matched.contributors, Vector(j.key("s1", "a", "encode")))
        assertEquals(
          (matched.selected, matched.successful, matched.failed, matched.contributing),
          (1, 1, 0, 1)
        )
        val control = get(view.reduction(get(row.control.toRight("no control operand"))))
        assertEquals(
          control.contributors,
          Vector(j.key("s1", "b", "encode"), j.key("s1", "c", "encode"))
        )
        val pair = get(view.pair(matched.members.head.pair))
        assertEquals(
          pair.ref,
          ResultRef.PairRow(0, StudyDesign.Matched, focal, j.key("s1", "a", "encode"))
        )
        val located =
          (0 until 4).toVector.map(i => get(view.sources.fixation(pair.reference, i)))
        assertEquals(located.map(_.record), records("s1", "a", "encode"))
        assertEquals(located.map(_.ordinal), Vector(0, 1, 2, 3))
        assertEquals(get(view.trial(focal)).records, records("s1", "a", "recall"))
        assertEquals(
          get(view.trial(j.key("s2", "c", "recall"))).records,
          records("s2", "c", "recall")
        )
        assertEquals(view.failures, Vector.empty)
    }

    test(
      s"$name: a bandwidth the grid cannot express fails every pair of its scale, as evidence"
    ) {
      val fine  = j.plan(j.input, Vector(StudyEstimate.Binned(), gaussian(0.1)))
      val check = fine.preflight(Some(j.input), pairs)
      // Preflight never estimates, so it cannot see this; it says so.
      assertEquals(check.findings, Vector.empty)
      assert(check.notChecked.contains(UncheckedAspect.OccupancyEstimation))
      val work = get(check.prepare(fine, j.input, pairs))
      j.runner.start(work, comparison, quanta).use(_.outcome).flatMap { outcome =>
        val result   = j.completed(outcome)
        val degraded = EstimateError.DegenerateBandwidth(0.1, 1.0)
        val broken   = result.scales(1)
        assertEquals(
          broken.estimation.map(_._2.left.toOption),
          j.input.trials.rows.map(t => Some(StudyFailure.Estimation(t.key, degraded)))
        )
        StudyDesign.values.foreach { design =>
          val selected = if design == StudyDesign.Matched then 1 else 2
          assert(
            broken.analyses
              .source(design)
              .rows
              .forall(r => r.result == Left(StudyFailure.Estimation(r.left, degraded)))
          )
          assert(
            broken.analyses
              .reduced(design)
              .entries
              .forall(r =>
                r.result == Left(
                  ReductionError.FailedScores(r.key, 0, selected)
                ) && r.contributing == 0
              )
          )
        }
        assertEquals(
          get(broken.contrast).rows
            .map(_.difference.left.map(Diagnostic.of(_).code.render)),
          Vector.fill(6)(Left("contrast-row.reduction-failures"))
        )
        assert(
          result.scales(0).analyses.source(StudyDesign.Matched).rows.forall(_.result.isRight)
        )
        // The inspection names every failure, located and linked to its records.
        val view = get(ResultInspection.study(fine, result, j.input, Some(j.ledger)))
        assertEquals(view.failures.size, 12 + 6 + 6 + 12 + 6 + 6)
        assert(view.failures.forall(_.subject.headOption == Some(Locus.Scale(1))))
        val first = view.failures.head
        assertEquals(first.code.render, "study-failure.estimation")
        assertEquals(first.causes.map(_.code.render), Vector("estimate.degenerate-bandwidth"))
        assertEquals(first.sources, j.trialLinks(j.ledger.source, "s1", "a", "encode"))
        val all     = get(PageSize.of(PageSize.maximum))
        val members = get(view.scale(1))
          .reductions(StudyDesign.Control)
          .first(all)
          .entries
          .flatMap(_.members)
        val failedPairs =
          members.map(_.status).collect { case Membership.FailedPair(d) => d.code.render }
        assertEquals(failedPairs, Vector.fill(12)("study-failure.estimation"))
        // The failures are part of the archive and survive a save and reload.
        val saved = get(FixationJourney.save(route, fine, j.input, j.ledger, result))
        val back  = get(j.reload(j.storage(saved)))
        assert(
          StudyResultEquivalence.same(back.result, result)(
            (x, y) => j.schema.score(x).components == j.schema.score(y).components,
            (x, y) => j.schema.difference(x).components == j.schema.difference(y).components
          )
        )
        assertEquals(
          get(FixationJourney.fingerprint(fine, back.result)),
          get(FixationJourney.fingerprint(fine, result))
        )
        // Rerun from the reloaded study: the same failures in the same places.
        FixationJourney.rerun(back, pairs, comparison, quanta).map { rerun =>
          assert(j.same(j.completed(get(rerun)), result), "the rerun reproduces the failures")
        }
      }
    }

    test(s"$name: a table on another display fails every trial by its frame, as evidence") {
      // UI-G1 invalid geometry: a degenerate display is refused by its constructor.
      assertEquals(
        Frame.screen("display", 0, 2).left.map(e => Diagnostic.of(e).code.render),
        Left("geometry.degenerate-bounds")
      )
      // The journey's table read on a display named like the plan's, with other bounds.
      val wide  = get(Frame.screen("display", 4, 4))
      val input = get(
        get(
          FixationCsv.read(
            JourneyFixtures.table,
            JourneySetup.columns,
            route.reader,
            wide,
            TimestampUnit.Microseconds
          )
        ).requireComplete
      )
      val study  = j.plan(input, Vector(StudyEstimate.Binned()))
      val report = study.preflight(Some(input), pairs)
      val keys   = input.trials.rows.map(_.key)
      // Every trial will fail at every scale: a warning per trial, not a blocker.
      assertEquals(report.findings.map(_.keys), keys.map(Vector(_)))
      assertEquals(
        report.findings.map(f => (f.severity, f.category, f.remedy)).distinct,
        Vector((Severity.Warning, FindingClass.IncompatibleInput, Remedy.AlignFrame))
      )
      assertEquals(
        report.findings
          .map(f => Diagnostic.of(f))
          .map(d => d.code.render -> d.causes.map(_.code.render))
          .distinct,
        Vector("study-finding.frame-mismatch" -> Vector("geometry.frame-identity-conflict"))
      )
      assertEquals(report.availability, Availability.Ready)
      j.runner
        .start(get(report.prepare(study, input, pairs)), comparison, quanta)
        .use(_.outcome)
        .map { outcome =>
          val result = j.completed(outcome)
          val scale  = result.scales.head
          assertEquals(
            scale.estimation.map((k, e) => k -> e.left.map(Diagnostic.of(_).code.render)),
            keys.map(k => k -> Left("study-failure.frame"))
          )
          assertEquals(
            get(scale.contrast).rows
              .map(_.difference.left.map(Diagnostic.of(_).code.render)),
            Vector.fill(6)(Left("contrast-row.reduction-failures"))
          )
          // Each failure is located at its scale and trial.
          val view  = get(ResultInspection.study(study, result, input, None))
          val first = get(view.failures.headOption.toRight("no failure"))
          assertEquals(first.code.render, "study-failure.frame")
          assertEquals(first.subject, Vector(Locus.Scale(0), Locus.Trial(keys.head)))
          assertEquals(view.failures.size, 12 + 6 + 6 + 12 + 6 + 6)
        }
    }

    test(s"$name: a bad table row becomes coded, record-linked diagnostics") {
      val broken  = j.read(JourneyFixtures.broken)
      val trial   = j.key("s1", "b", "encode")
      val records = JourneyFixtures.records("s1", "b", "encode")
      assertEquals(broken.rejected.map(_.rowNumber), records)
      assertEquals(broken.requireComplete, Left(FixationImportError.Incomplete(records)))
      val refused =
        get(FixationEvidence.ledger("broken.csv", broken, AdmissionDecision.RequireComplete))
      assertEquals(refused.outcome, AdmissionOutcome.Refused)
      assertEquals(refused.quarantined, Vector(trial))
      // Against the complete input, the refused ledger names the trial it did not admit.
      import j.given
      val refusal = StudySources.of(j.input, refused).left.toOption
      assertEquals(
        refusal.map(r => r.error -> r.trials),
        Some(
          AdmissionError.UnadmittedTrial(
            j.input.trials.rows.indexWhere(_.key == trial)
          ) -> Vector(trial)
        )
      )
      val unadmitted = Diagnostic.of(get(refusal.toRight("accepted")))
      assertEquals(unadmitted.code.render, "admission.unadmitted-trial")
      assertEquals(unadmitted.keys, Vector(trial))
      assertEquals(unadmitted.sources, j.trialLinks(refused.source, "s1", "b", "encode"))
      // The explicit decision to proceed without it.
      val reviewed =
        get(FixationEvidence.ledger("broken.csv", broken, AdmissionDecision.ReviewExclusions))
      assertEquals(reviewed.outcome, AdmissionOutcome.ReviewedExclusions)
      val kept    = StudyInput(broken.accepted)
      val sources = get(StudySources.of(kept, reviewed))
      val bad     = records(2)
      assertEquals(
        sources.rejections.map(d => (d.code.render, d.subject, d.sources)),
        records.map { n =>
          if n == bad then
            (
              "admission-reason.time",
              Vector(Locus.Trial(trial), Locus.Record(n)),
              Vector(SourceLink.Record(reviewed.source, n))
            )
          else
            (
              "admission-reason.quarantined",
              Vector(Locus.Trial(trial), Locus.Record(n), Locus.Records(records)),
              Vector(SourceLink.Record(reviewed.source, n))
            )
        }
      )
      assert(reviewed.records.exists {
        case SourceRecord(
              n,
              Disposition.Rejected(_, Some(k), AdmissionReason.Time(_, "-200000", _, _))
            ) =>
          n == bad && k == trial
        case _ => false
      })
      // The saved plan names the complete input, so the reviewed one blocks it.
      assertEquals(
        j.study.preflight(Some(kept), pairs).findings,
        Vector(StudyFinding.ArtifactMismatch(j.study.input, kept.reference))
      )
      // Replanned on the reviewed input, the focal trial has lost its match.
      val replanned = j.plan(kept, Vector(StudyEstimate.Binned()))
      val report    = replanned.preflight(Some(kept), pairs)
      val focal     = j.key("s1", "b", "recall")
      assertEquals(report.findings, Vector(StudyFinding.UnmatchedFocal(focal)))
      assertEquals(report.availability, Availability.Ready)
      assertEquals(
        report.warnings.map(w => w.severity -> w.category),
        Vector(Severity.Warning -> FindingClass.DataDependent)
      )
      // A report made for the complete input is stale for the reviewed one.
      assertEquals(
        j.study.preflight(Some(j.input), pairs).prepare(j.study, kept, pairs).left.toOption,
        Some(
          PreflightError.ChangedInput(
            RecipeFamily.FixationStudy,
            j.input.reference,
            kept.reference
          )
        )
      )
      j.runner
        .start(get(report.prepare(replanned, kept, pairs)), comparison, quanta)
        .use(_.outcome)
        .flatMap { outcome =>
          val result = j.completed(outcome)
          val view   = get(ResultInspection.study(replanned, result, kept, Some(reviewed)))
          val row    = get(view.contrastRow(ResultRef.ContrastRow(0, focal)))
          val why    = row.outcome match
            case Left(diagnostic) => diagnostic
            case Right(value)     => fail(s"expected a failed contrast row: $value")
          assertEquals(why.code.render, "contrast-row.reduction-failures")
          assertEquals(why.sources, j.trialLinks(reviewed.source, "s1", "b", "recall"))
          val reduction = get(view.reduction(get(row.matched.toRight("no matched operand"))))
          assertEquals(reduction.selected, 0)
          assertEquals(
            reduction.outcome.left.map(_.code.render),
            Left("reduction.no-selected-scores")
          )
          // Each focal trial of s1 has one control left; s2's keep two.
          def counts(design: StudyDesign) =
            result.scales.head.analyses
              .reduced(design)
              .entries
              .map(r => c.label(r.key) -> (r.successful, r.failed, r.contributing))
          assertEquals(
            counts(StudyDesign.Control),
            Vector(
              "s1/a/recall" -> (1, 0, 1),
              "s1/b/recall" -> (2, 0, 2),
              "s1/c/recall" -> (1, 0, 1),
              "s2/a/recall" -> (2, 0, 2),
              "s2/b/recall" -> (2, 0, 2),
              "s2/c/recall" -> (2, 0, 2)
            )
          )
          assertEquals(
            counts(StudyDesign.Matched).map(_._2),
            Vector.fill(5)((1, 0, 1)) :+ (0, 0, 0)
          )
          // The reviewed study, its ledger and its failed row survive a save,
          // a reload and a rerun unchanged.
          val saved = get(FixationJourney.save(route, replanned, kept, reviewed, result))
          val back  = get(j.reload(j.storage(saved)))
          assertEquals(back.ledger, reviewed)
          assertEquals(back.input.reference, kept.reference)
          FixationJourney.rerun(back, pairs, comparison, quanta).map { rerun =>
            assert(j.same(back.result, result), "the archive decodes to the reviewed result")
            assert(j.same(j.completed(get(rerun)), result), "the rerun reproduces it")
          }
        }
    }

    test(s"$name: a changed, replaced or missing artifact is refused by name, with a code") {
      j.saved.map { (_, saved) =>
        val files                = j.storage(saved)
        val entries              = saved.manifest.entries
        def named(value: String) = get(ArtifactName.of(value))
        def refused(stored: Map[String, IArray[Byte]]): NonEmptyVector[ResolveError] =
          j.reload(stored) match
            case Left(JourneyError.Resolve(errors)) => errors
            case other                              => fail(s"expected a refusal: $other")
        // One flipped byte of the archived result: refused before any decoding.
        val original = files(FixationJourney.resultEntry)
        val flipped  = IArray.tabulate(original.length)(i =>
          if i == 40 then (original(i) ^ 1).toByte else original(i)
        )
        val declared = get(
          entries.find(_.name == named(FixationJourney.resultEntry)).toRight("no entry")
        ).sha256
        val digest = refused(files.updated(FixationJourney.resultEntry, flipped))
        assertEquals(
          digest,
          NonEmptyVector.one(
            ResolveError.Digest(
              named(FixationJourney.resultEntry),
              declared,
              ByteDigest.sha256(flipped)
            )
          )
        )
        val coded = Diagnostic.of(digest.head)
        assertEquals(coded.code.render, "resolve.digest")
        assertEquals(coded.subject, Vector(Locus.Entry(FixationJourney.resultEntry)))
        // A missing ledger is named as missing.
        val missing = refused(files - FixationJourney.ledgerEntry)
        assertEquals(
          missing,
          NonEmptyVector.one(ResolveError.Missing(named(FixationJourney.ledgerEntry)))
        )
        assertEquals(Diagnostic.of(missing.head).code.render, "resolve.missing")
        // The input replaced by another admitted input, its digests re-declared
        // consistently: it decodes, but not to the identity the manifest names.
        import j.given
        val other   = StudyInput(j.read(JourneyFixtures.broken).accepted)
        val swapped = get(StoredArtifact.input(FixationJourney.inputEntry, route.inputs, other))
        val forged  = get(
          ScientificManifest.of(
            entries.map(e =>
              if e.name == swapped.name then
                get(
                  ManifestEntry.of(
                    e.name,
                    e.role,
                    e.schema,
                    e.media,
                    swapped.bytes.length.toLong,
                    swapped.entry.sha256,
                    e.identity,
                    e.layout
                  )
                )
              else e
            ),
            saved.manifest.relations
          )
        )
        val bytes    = get(ScientificManifest.bytes(forged))
        val replaced = refused(
          files ++ Map(
            FixationJourney.inputEntry   -> swapped.bytes,
            FixationJourney.manifestFile -> bytes,
            FixationJourney.addressFile  -> IArray.from(
              ByteDigest.sha256(bytes).hex.getBytes(StandardCharsets.UTF_8)
            )
          )
        )
        assertEquals(
          replaced,
          NonEmptyVector.one(
            ResolveError.Identity(
              named(FixationJourney.inputEntry),
              j.input.reference.digest,
              other.reference.digest
            )
          )
        )
        assertEquals(Diagnostic.of(replaced.head).code.render, "resolve.identity")
        assertEquals(
          Diagnostic.of(replaced.head).subject,
          Vector(Locus.Entry(FixationJourney.inputEntry))
        )
      }
    }

    test(s"$name: emit portable journey evidence for the JVM and Scala.js comparison") {
      for
        events          <- j.events
        (result, saved) <- j.saved
        cancelled       <- j.cancelAfter(14)
      yield
        val progress = events.collect { case StudyEvent.Advanced(p) => p }
        // Onsets as they come back from the stored archive, not the fixture.
        val reloaded = get(j.reload(j.storage(saved)))
        val late     = reloaded.input.trials.rows
          .filter(_.key == j.key("s2", "c", "recall"))
          .flatMap(
            _.value.fixations.toVector.map(f => Json.fromString(f.span.onset.toMicros.toString))
          )
        val reductions = StudyDesign.values.toVector.map(design =>
          Json.obj(
            "design" -> Json.fromString(design.toString),
            "values" -> Json.arr(
              j.reductions(result, 0, design)
                .map((label, value) =>
                  Json.obj(
                    "key"   -> Json.fromString(label),
                    "value" -> Json.fromDoubleOrNull(value)
                  )
                )*
            )
          )
        )
        val runtime  = if System.getProperty("java.vm.name") == "Scala.js" then "js" else "jvm"
        val segments =
          progress.groupBy(_.segment).toVector.sortBy(_._2.head.step).map { (segment, steps) =>
            Json.obj(
              "segment" -> Json.fromString(segment.toString),
              "total"   -> Json.fromString(steps.head.segmentTotal.toString),
              "units"   -> Json.fromLong(steps.last.segmentUnits),
              "steps"   -> Json.fromInt(steps.size)
            )
          }
        val binned = get(result.scales.head.contrast).rows.map { row =>
          val value = get(row.difference.map(j.schema.difference(_).components.head.value))
          Json.obj(
            "key"  -> Json.fromString(c.label(row.key)),
            "bits" -> Json.fromString(
              java.lang.Long.toHexString(java.lang.Double.doubleToRawLongBits(value))
            ),
            "value" -> Json.fromDoubleOrNull(value)
          )
        }
        val gaussian = sigmas.indices.toVector.flatMap(i =>
          j.contrasts(result, i + 1)
            .map((label, value) =>
              Json.obj(
                "sigma" -> Json.fromDoubleOrNull(sigmas(i)),
                "key"   -> Json.fromString(label),
                "value" -> Json.fromDoubleOrNull(value)
              )
            )
        )
        val stopped = cancelled._1.progress.map(p =>
          Json.obj(
            "step"    -> Json.fromLong(p.step),
            "segment" -> Json.fromString(p.segment.toString),
            "units"   -> Json.fromLong(p.segmentUnits)
          )
        )
        println(
          "EYES4S_JOURNEY=" + Json
            .obj(
              "runtime"           -> Json.fromString(runtime),
              "route"             -> Json.fromString(c.name),
              "multiplier"        -> Json.fromDoubleOrNull(c.multiplier),
              "input"             -> Json.fromString(j.input.reference.digest),
              "plan"              -> get(route.persistence.codec.encode(j.study)),
              "late"              -> Json.arr(late*),
              "binned_reductions" -> Json.arr(reductions*),
              "binned"            -> Json.arr(binned*),
              "gaussian"          -> Json.arr(gaussian*),
              "segments"          -> Json.arr(segments*),
              "cancelled"         -> stopped.getOrElse(Json.Null),
              "fingerprint_size"  -> Json.fromInt(j.bits(result).size)
            )
            .noSpaces
        )
    }

  journey(cosine)
  journey(scaled)

  test("the extension's binned contrasts are exactly twice the shipped cosine's") {
    // Scaling by two is exact in binary floating point, so every reduced mean
    // and contrast of the extension is the cosine's doubled, bit for bit.
    def doubled(values: Vector[(String, Double)]) =
      values.map((label, value) => label -> java.lang.Double.doubleToRawLongBits(2 * value))
    def raw(values: Vector[(String, Double)]) =
      values.map((label, value) => label -> java.lang.Double.doubleToRawLongBits(value))
    (cosine.saved, scaled.saved).tupled.map { case ((plain, _), (twice, _)) =>
      assertEquals(raw(scaled.contrasts(twice, 0)), doubled(cosine.contrasts(plain, 0)))
      StudyDesign.values.foreach(design =>
        assertEquals(
          raw(scaled.reductions(twice, 0, design)),
          doubled(cosine.reductions(plain, 0, design))
        )
      )
    }
  }
