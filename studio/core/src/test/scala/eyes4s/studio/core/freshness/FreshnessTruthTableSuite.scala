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

package eyes4s.studio.core.freshness

import cats.data.NonEmptyVector
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments
import org.scalacheck.Prop.forAll

/** Every freshness state of the System board (section 05) and the three
  * story moments of FIXTURE.md, derived from documents (ticket S2.7).
  */
class FreshnessTruthTableSuite extends munit.ScalaCheckSuite:
  import StoryMoments.{r2, r3, rev3, rev4, rev5, run5, run7, run8, run8Job}

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100)

  private def ok[E, A](e: Either[E, A]): A = e.fold(m => fail(s"unexpected: $m"), identity)

  private lazy val t1 = ok(StoryMoments.t1)
  private lazy val t2 = ok(StoryMoments.t2)
  private lazy val t3 = ok(StoryMoments.t3)

  private def rebuild(d: StudioDocument)(
      datasets: Vector[DatasetRevisionSpec] = d.datasets,
      analyses: Vector[AnalysisRevisionSpec] = d.analyses,
      draft: Option[Draft] = d.draft,
      runs: Vector[RunRef] = d.runs,
      figures: Vector[FigureSpec] = d.figures,
      shown: Option[RunId] = d.presentation.shownRun,
      jobs: Vector[JobHandle] = d.jobs
  ): StudioDocument =
    val p = d.presentation
    ok(
      PresentationState
        .of(p.perspective, p.theme, p.stage, p.mapOpacity, p.underlay, shown, p.layouts)
        .flatMap(view =>
          StudioDocument.of(datasets, analyses, draft, runs, d.reporting, figures, view, jobs)
        )
    )

  private val admitted = AdmissionDecision.Admitted(CoreBinding.unbound, CoreBinding.unbound)

  private def diagnostic(level: DiagnosticLevel, code: String) =
    StudioDiagnostic(code, level, DiagnosticOrigin.Host, Vector.empty, code)

  private def progress(job: JobId, step: Long, pairs: Long): JobProgress =
    ok(
      for
        meter  <- StageMeter.of(StageKind.Comparing, CountUnit.Pairs, 0L, ProgressTotal.Unknown)
        totals <- RunTotals.of(
          4685L,
          ProgressTotal.Exact(4685L),
          pairs,
          ProgressTotal.Exact(44845L)
        )
        p <- JobProgress
          .of(job, run8, step, Segment.Comparing(2, PairDesign.Matched), meter, totals)
      yield p
    )

  private def ready(d: StudioDocument): SessionFacts =
    SessionFacts.empty.copy(draftCheck = DraftCheck.Checked(d.draft.get, Vector.empty))

  private def text(f: Freshness): String = FreshnessText.badge(f.badge)

  // -------------------------------------------------------------------------
  // Story moments
  // -------------------------------------------------------------------------

  test("t1 Data · verify: run 5 current on r2; r3 pending would make it stale; no draft") {
    val f = Freshness.of(t1, SessionFacts.empty)
    assertEquals(text(f), "Analysis rev 3 · run 5 · data r2 · current")
    assertEquals(f.draft, None)
    assertEquals(f.banner, None)
    assertEquals(f.activity, None)
    assertEquals(FreshnessText.jobs(f.activity), "No jobs")
    assertEquals(f.pending, Vector(PendingDataset(r3, Vector(run5))))
    assertEquals(
      f.pending.flatMap(FreshnessText.pending(_, t1.runs)),
      Vector("Run 5 (rev 3) used r2 · becomes stale when r3 is admitted")
    )
    assertEquals(
      f.figures.map(FreshnessText.figure),
      Vector("Figure 2 · run 5 · rev 3 · data r2 · current")
    )
  }

  test(
    "t2: rev 4 · run 7 · r3 current; draft rev 5 ready; banner in Compare and Figures only"
  ) {
    val f = Freshness.of(t2, ready(t2))
    assertEquals(text(f), "Analysis rev 4 · run 7 · data r3 · current")
    assertEquals(f.badge.tone, BadgeTone.Current)
    assertEquals(f.draft.map(FreshnessText.draft), Some("Draft rev 5 · 1 change · ready"))
    assertEquals(FreshnessText.jobs(f.activity), "No jobs")
    val banner = "Showing run 7 (analysis rev 4). Draft rev 5 adds σ 8° — not run yet."
    assertEquals(f.bannerFor(Perspective.Compare).map(FreshnessText.banner), Some(banner))
    assertEquals(f.bannerFor(Perspective.Figures).map(FreshnessText.banner), Some(banner))
    for p <- Vector(Perspective.Data, Perspective.Explore, Perspective.Analysis) do
      assertEquals(f.bannerFor(p), None)
    assertEquals(f.pending, Vector.empty)
    // Run 6 (rev 4, cancelled) is older than the shown run and changes nothing.
    assertEquals(
      f.standing(StoryMoments.run6),
      Some(RunStanding.Cancelled(Some(StageKind.Comparing)))
    )
  }

  test("t2 figures: Figure 1 on run 7 is current; Figure 2 on run 5 is stale on both counts") {
    val f = Freshness.of(t2, ready(t2))
    assertEquals(
      f.figures.map(FreshnessText.figure),
      Vector(
        "Figure 1 · run 7 · rev 4 · data r3 · current",
        "Figure 2 · run 5 · rev 3 · data r2 · stale"
      )
    )
    val fig2 = f.figures(1)
    assertEquals(
      fig2.standing,
      RunStanding.Stale(
        NonEmptyVector.of(
          StaleReason.AnalysisMoved(rev3, rev4),
          StaleReason.DatasetMoved(r2, r3)
        )
      )
    )
    assertEquals(
      FreshnessText.stale(fig2.run, fig2.standing),
      Some("Stale · run 5 belongs to rev 3")
    )
  }

  test("t3 Compare · summary: run 8 running at 21,400 / 44,845 pairs; view stays on run 7") {
    val f =
      Freshness.of(t3, SessionFacts.empty.copy(progress = Vector(progress(run8Job, 1, 21400L))))
    assertEquals(text(f), "Analysis rev 4 · run 7 · data r3 · running")
    assertEquals(
      FreshnessText.jobs(f.activity),
      "Run 8 · Comparing · 21,400 / 44,845 pairs"
    )
    assertEquals(f.draft, None)
    assertEquals(
      f.bannerFor(Perspective.Compare).map(FreshnessText.banner),
      Some(
        "Run 8 (rev 5, adds σ 8°) is running — results will not replace this view until you " +
          "choose Show."
      )
    )
    // Saving rev 5 moved the analysis: run 7 itself is now stale.
    assertEquals(
      f.standing(run7),
      Some(RunStanding.Stale(NonEmptyVector.of(StaleReason.AnalysisMoved(rev4, rev5))))
    )
  }

  // -------------------------------------------------------------------------
  // System board rows
  // -------------------------------------------------------------------------

  test("Draft with blockers: error diagnostics block, warnings do not") {
    val check = DraftCheck.Checked(
      t2.draft.get,
      Vector(
        diagnostic(DiagnosticLevel.Error, "studio.matched-cardinality"),
        diagnostic(DiagnosticLevel.Warning, "plan.duplicate-trial")
      )
    )
    val f = Freshness.of(t2, SessionFacts.empty.copy(draftCheck = check))
    assertEquals(f.draft.map(_.readiness), Some(DraftReadiness.Blocked(1)))
    assertEquals(f.draft.map(FreshnessText.draft), Some("Draft rev 5 · 1 change · 1 blocker"))
  }

  test("a draft check of another draft value does not apply; the chip says not checked") {
    val other =
      ok(Draft.between(rev5, t2.analysis(rev4).get, ok(StoryMoments.recipe(Vector(1.0)))))
    val f = Freshness.of(
      t2,
      SessionFacts.empty.copy(draftCheck = DraftCheck.Checked(other, Vector.empty))
    )
    assertEquals(f.draft.map(FreshnessText.draft), Some("Draft rev 5 · 1 change · not checked"))
  }

  test("Stale (dataset moved): run 5 on r2 becomes stale once r3 is admitted") {
    val admittedR3 = rebuild(t1)(datasets =
      t1.datasets.map(d => if d.id == r3 then d.copy(decision = admitted) else d)
    )
    val f = Freshness.of(admittedR3, SessionFacts.empty)
    assertEquals(
      f.standing(run5),
      Some(RunStanding.Stale(NonEmptyVector.of(StaleReason.DatasetMoved(r2, r3))))
    )
    assertEquals(text(f), "Analysis rev 3 · run 5 · data r2 · stale")
    assertEquals(
      FreshnessText.stale(t1.run(run5).get, f.standing(run5).get),
      Some("Stale · run 5 used data r2; r3 is admitted")
    )
    assertEquals(f.pending, Vector.empty)
    assert(f.figures.forall(_.isStale))
    assertEquals(
      f.bannerFor(Perspective.Compare).map(FreshnessText.banner),
      Some("Showing run 5 (analysis rev 3, data r2), which is no longer current.")
    )
  }

  test("Stale (analysis moved): rev 4 saved on r2 leaves run 5 of rev 3 stale") {
    val a3    = t1.analysis(rev3).get
    val moved = rebuild(t1)(analyses = Vector(a3, a3.copy(id = rev4)))
    val f     = Freshness.of(moved, SessionFacts.empty)
    assertEquals(
      f.standing(run5),
      Some(RunStanding.Stale(NonEmptyVector.of(StaleReason.AnalysisMoved(rev3, rev4))))
    )
    assertEquals(text(f), "Analysis rev 3 · run 5 · data r2 · stale")
    assertEquals(
      FreshnessText.stale(t1.run(run5).get, f.standing(run5).get),
      Some("Stale · run 5 belongs to rev 3")
    )
  }

  test("Running without a progress report from the run's own job shows no count") {
    val foreign = progress(JobId(99), 1, 30000L)
    val f       = Freshness.of(t3, SessionFacts.empty.copy(progress = Vector(foreign)))
    assertEquals(FreshnessText.jobs(f.activity), "Run 8 · running")
    assertEquals(f.badge.tone, BadgeTone.Running)
  }

  test("the latest report (highest step) of the run's job is the one shown") {
    val reports = Vector(progress(run8Job, 3, 30000L), progress(run8Job, 1, 21400L))
    val f       = Freshness.of(t3, SessionFacts.empty.copy(progress = reports))
    assertEquals(FreshnessText.jobs(f.activity), "Run 8 · Comparing · 30,000 / 44,845 pairs")
  }

  private def finished(state: RunLifecycle): StudioDocument =
    rebuild(t3)(
      runs = t3.runs.map(r => if r.id == run8 then r.copy(state = state) else r),
      jobs = Vector.empty
    )

  test("Failed: run 8 failed with the diagnostics count of its outcome") {
    val diags =
      Vector(diagnostic(DiagnosticLevel.Error, "a"), diagnostic(DiagnosticLevel.Error, "b"))
    val outcome = JobOutcome.Failed(run8Job, run8, diags, None)
    val f       = Freshness.of(
      finished(RunLifecycle.Failed),
      SessionFacts.empty.copy(outcomes = Vector(outcome))
    )
    assertEquals(FreshnessText.jobs(f.activity), "Run 8 failed · 2 diagnostics")
    assertEquals(text(f), "Analysis rev 4 · run 7 · data r3 · failed")
    // Without the outcome (a reopened document) the count is not invented.
    val reopened = Freshness.of(finished(RunLifecycle.Failed), SessionFacts.empty)
    assertEquals(FreshnessText.jobs(reopened.activity), "Run 8 failed")
  }

  test("Cancelled: run 8 cancelled while comparing") {
    val f = Freshness.of(
      finished(RunLifecycle.Cancelled(Some(StageKind.Comparing))),
      SessionFacts.empty
    )
    assertEquals(FreshnessText.jobs(f.activity), "Run 8 cancelled · Comparing")
    assertEquals(text(f), "Analysis rev 4 · run 7 · data r3 · cancelled")
  }

  test("a newer completed run that is not shown gets its own banner") {
    val f = Freshness.of(finished(RunLifecycle.Completed), SessionFacts.empty)
    assertEquals(text(f), "Analysis rev 4 · run 7 · data r3 · stale")
    assertEquals(
      f.banner.map(FreshnessText.banner),
      Some(
        "Showing run 7 (analysis rev 4). Run 8 (rev 5) has finished — choose Show to see it."
      )
    )
    assertEquals(f.standing(run8), Some(RunStanding.Current))
  }

  test("No run: nothing has run on the latest revision; Compare says No run yet") {
    val a4   = t2.analysis(rev4).get
    val none = rebuild(t2)(
      analyses = Vector(a4),
      draft = None,
      runs = Vector.empty,
      figures = Vector.empty,
      shown = None
    )
    val f = Freshness.of(none, SessionFacts.empty)
    assertEquals(f.badge, Badge.NoRun(Some(rev4), Some(r3)))
    assertEquals(text(f), "Analysis rev 4 · no run yet · data r3")
    assertEquals(f.bannerFor(Perspective.Compare), Some(Banner.NoRun(Some(rev4))))
    assertEquals(
      f.bannerFor(Perspective.Compare).map(FreshnessText.banner),
      Some("Compare · No run yet")
    )
  }

  // -------------------------------------------------------------------------
  // Laws over generated documents
  // -------------------------------------------------------------------------

  property("a completed run is current exactly when it names the latest revision and data") {
    forAll(eyes4s.studio.core.document.DocumentGen.document) { (d: StudioDocument) =>
      val f = Freshness.of(d, SessionFacts.empty)
      f.runs.foreach { rf =>
        if rf.run.state == RunLifecycle.Completed then
          val current =
            d.latestAnalysis.forall(_.id == rf.run.analysis) &&
              d.latestAdmitted.forall(_.id == rf.run.dataset)
          assertEquals(rf.standing == RunStanding.Current, current, rf.run.label)
      }
      assertEquals(f.figures.map(_.figure), d.figures.map(_.id))
    }
  }

  property("presentation and job handles never change a run's standing") {
    forAll(eyes4s.studio.core.document.DocumentGen.document) { (d: StudioDocument) =>
      val bare = ok(d.withJobs(Vector.empty)).withPresentation(PresentationState.default)
      assertEquals(
        Freshness.of(ok(bare), SessionFacts.empty).runs,
        Freshness.of(d, SessionFacts.empty).runs
      )
    }
  }
