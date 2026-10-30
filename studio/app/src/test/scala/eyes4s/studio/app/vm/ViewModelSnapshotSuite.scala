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

package eyes4s.studio.app.vm

import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.app.StoryModels.ok
import eyes4s.studio.core.execution.{ExecutionEvent, JobPhase, RunReady}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{Draft, Perspective, PresentationState, StudioDocument}
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.freshness.{DraftCheck, FreshnessText, SessionFacts}

/** Golden text renderings of the shell view-models at the story moments
  * (ticket S1.0). Every slot text is copied character for character from the
  * board named in the test (docs/studio/design/); `*…*` marks the current
  * crumb, `[…]` a disabled button and "(current)" the selected perspective,
  * which the boards draw rather than print. The Analysis family crumb uses the
  * document's current name and nominal identity (S7.7b4), replacing the board's
  * preset-only label.
  */
class ViewModelSnapshotSuite extends munit.FunSuite:

  private def snapshot(model: AppModel): String = ShellText.render(Shell.project(model))

  private val perspectives = "Data | Analysis | Results | Report"

  private def switcher(current: String): String =
    perspectives.replace(current, s"$current (current)")

  test("DataEmpty board: a new project") {
    assertNoDiff(
      snapshot(StoryModels.firstRun),
      s"""|window: Untitled project
          |app.project: Untitled project
          |app.perspectives: ${switcher("Data")}
          |app.state: No run yet | No data | No analysis
          |context.nav: [Back (⌘[)] | [Forward (⌘])]
          |context.trail: *New project*
          |status: Drop files anywhere in this window | Not saved""".stripMargin
    )
  }

  test("t1 · Data board: verifying r3, which would make run 5 stale") {
    assertNoDiff(
      snapshot(StoryModels.t1Data),
      s"""|window: memory-study.eyes
          |app.project: memory-study
          |app.perspectives: ${switcher("Data")}
          |app.state: Run 5 · current | Data r3 · not admitted | Analysis rev 3
          |context.nav: Back (⌘[) | [Forward (⌘])]
          |context.trail: Dataset r3 (draft) / *Admission*
          |status: Mapping and geometry changes apply when r3 is admitted | Saved 10:24""".stripMargin
    )
  }

  test("t2 · Main board: Compare · query, the matched pair of P17 ret_07") {
    assertNoDiff(
      snapshot(StoryModels.t2Compare),
      s"""|window: memory-study.eyes
          |app.project: memory-study
          |app.perspectives: ${switcher("Results")}
          |app.state: Run 7 · current | Data r3 | Analysis rev 5 · not run
          |context.nav: Back (⌘[) | [Forward (⌘])]
          |context.trail: Summary · by retrieval response / Remembered / P17 / ret_07 · beach-042 / *pair ret_07 × enc_03 · matched*
          |status: Prev / Next steps controls · F6 next pane | Saved 10:24""".stripMargin
    )
  }

  test("t2 · Explore board: the trail crosses into Explore and survives the switch") {
    assertNoDiff(
      snapshot(StoryModels.t2Explore),
      s"""|window: memory-study.eyes
          |app.project: memory-study
          |app.perspectives: ${switcher("Results")}
          |app.state: Run 7 · current | Data r3 | Analysis rev 5 · not run
          |context.nav: Back (⌘[) | [Forward (⌘])]
          |context.trail: Summary · by retrieval response / Remembered / P17 / ret_07 · beach-042 / pair ret_07 × enc_03 / enc_03 · fixation 6 / *fixations.csv record 7,214*
          |status: View state only · no analysis changed | Saved 10:24""".stripMargin
    )
  }

  test("t2 · Analysis board: draft rev 5, the Scales field") {
    assertNoDiff(
      snapshot(StoryModels.t2Analysis),
      s"""|window: memory-study.eyes
          |app.project: memory-study
          |app.perspectives: ${switcher("Analysis")}
          |app.state: Run 7 · current | Data r3 | Analysis rev 5 · not run
          |context.nav: Back (⌘[) | [Forward (⌘])]
          |context.trail: Analyses / Encoding → retrieval reinstatement · analysis 1 / *Draft rev 5*
          |status: Edits change the draft only · runs are never relabelled | Saved 10:24""".stripMargin
    )
  }

  test("t2 · Figures board: Figure 1, panel D") {
    assertNoDiff(
      snapshot(StoryModels.t2Figures),
      s"""|window: memory-study.eyes
          |app.project: memory-study
          |app.perspectives: ${switcher("Report")}
          |app.state: Run 7 · current | Data r3 | Analysis rev 5 · not run
          |context.nav: Back (⌘[) | [Forward (⌘])]
          |context.trail: Figures / Figure 1 / *Panel D*
          |status: Arrow keys move between panels · Enter opens the binding | Saved 10:24""".stripMargin
    )
  }

  test("t3 · Results board: Compare · summary while run 8 runs") {
    assertNoDiff(
      snapshot(StoryModels.t3Summary),
      s"""|window: memory-study.eyes
          |app.project: memory-study
          |app.perspectives: ${switcher("Results")}
          |app.state: Showing run 7 | Data r3 | Run 8 · comparing 21,400 / 44,845
          |app.state.cancel: Cancel run 8
          |app.state.progress: 48%
          |context.nav: [Back (⌘[)] | [Forward (⌘])]
          |context.trail: *Summary · by retrieval response*
          |status: Enter on a participant opens its queries · F6 next pane | Saved 10:24""".stripMargin
    )
  }

  test("t3: the jobs chip reads as one line and names itself for assistive technology") {
    val chip = Shell.appBar(StoryModels.t3Summary).jobs
    assertEquals(chip.text, "Run 8 · Comparing · 21,400 / 44,845 pairs")
    assertEquals(chip.title, "Run 8")
    assertEquals(chip.stage, Some("Comparing"))
    assertEquals(chip.count, Some("21,400 / 44,845 pairs"))
    assertEquals(chip.accessible, "Run 8 comparing, 21,400 of 44,845 pairs")
    assertEquals(chip.state, JobsChipState.Running)
    assertEquals(chip.action.map(_.enabled), Some(true))
  }

  // -------------------------------------------------------------------------
  // System board state rows (09 Freshness, Jobs chip states)
  // -------------------------------------------------------------------------

  private def withJob(phase: JobPhase): AppModel =
    AppModel
      .update(StoryModels.t3Summary, Intent.JobsChanged(Vector(StoryModels.run8Job(phase))))
      ._1

  private val diagnostic =
    StudioDiagnostic(
      "studio-execution.lost-job",
      DiagnosticLevel.Error,
      DiagnosticOrigin.Host,
      Vector.empty,
      "lost"
    )

  test("Cancel names its target run and job in both shell locations and the tab order") {
    for model <- Vector(StoryModels.t3Summary, withJob(JobPhase.Queued)) do
      val shell  = Shell.project(model)
      val chip   = shell.appBar.jobs.action.getOrElse(fail("no chip cancel"))
      val mirror = shell.status.job.action.getOrElse(fail("no status cancel"))
      val name   = s"Cancel Run 8 (job ${StoryMoments.run8Job.number})"
      assertEquals(chip.label, "Cancel")
      assertEquals(chip.accessible, name)
      assertEquals(mirror.accessible, name)
      assertEquals(chip.intent, Intent.RequestCancelJob(StoryMoments.run8Job))
      // The tab order reaches the cancel once: the state button's ✕.
      assertEquals(A11y.tabOrder(model).count(_.name == name), 0)
      assertEquals(A11y.tabOrder(model).count(_.name == "Cancel run 8"), 1)
    assertEquals(
      ActionVM("Show", true, Intent.CancelJob(StoryMoments.run8Job)).accessible,
      "Show"
    )
  }

  test("System board · jobs chip: failed, counting, queued, cancelling, ready") {
    val failed =
      Shell.appBar(withJob(JobPhase.Failed(Vector(diagnostic, diagnostic), None))).jobs
    assertEquals(failed.text, "Run 8 failed · 2 diagnostics")
    assertEquals(failed.open, Some(Intent.OpenDiagnostics))
    val counting = withJob(JobPhase.Running(StoryModels.run8Progress(21400L, counting = true)))
    assertEquals(
      Shell.appBar(counting).jobs.text,
      "Run 8 · Comparing · 21,400 / counting… pairs"
    )
    assertEquals(Shell.appBar(counting).jobs.progress, None)
    assertEquals(Shell.context(counting).newer.map(_.text), Some("Rev 5 · run 8 running"))
    assertEquals(Shell.appBar(withJob(JobPhase.Queued)).jobs.text, "Run 8 · Queued")
    val cancelling = Shell.appBar(withJob(JobPhase.Cancelling(None))).jobs
    assertEquals(cancelling.text, "Run 8 · Cancelling…")
    assertEquals(cancelling.action, None)
    for phase <- Vector(JobPhase.Cancelled(None), JobPhase.Superseded(None, None)) do
      assertEquals(Shell.appBar(withJob(phase)).jobs.text, "No jobs", phase)
  }

  test(
    "Show waits for the shelf: NotReady until run 8's notice arrives for the required stamp"
  ) {
    val done = StoryModels.run8Job(JobPhase.Succeeded(StoryModels.run8Progress(44845L)))
    val m0   = withJob(done.phase)
    assertEquals(Shell.appBar(m0).jobs.text, "No jobs")
    assertEquals(
      Shell.banner(m0).map(_.actions.map(a => (a.label, a.enabled))),
      Some(Vector("Show run 8 · when finished" -> false))
    )
    val (refused, effects) = AppModel.update(m0, Intent.ShowRun(StoryMoments.run8))
    assertEquals(effects, Vector.empty)
    assertEquals(
      Shell.project(refused).notice.map(_.text),
      Some("Run 8 is not ready to show; no run is ready.")
    )
    // A notice for another stamp is not the one requested: the shelf ignores it.
    val other = RunReady(done.id, done.run, done.stamp.copy(dataset = DatasetRevision(9)))
    val m1    = AppModel.update(m0, Intent.Execution(ExecutionEvent.Ready(other)))._1
    assertEquals(m1.jobs.ready, None)
    val notice = RunReady(done.id, done.run, done.stamp)
    val ready  = AppModel.update(m0, Intent.Execution(ExecutionEvent.Ready(notice)))._1
    assertEquals(Shell.appBar(ready).jobs.text, "Run 8 ready — Show")
    assertEquals(Shell.status(ready).job.text, "Run 8 ready — Show")
    assertEquals(
      Shell.banner(ready).map(_.actions.map(a => (a.label, a.enabled))),
      Some(Vector("Show run 8" -> true))
    )
    val shown = AppModel.update(ready, Intent.ShowRun(StoryMoments.run8))._1
    assertEquals(shown.document.presentation.shownRun, Some(StoryMoments.run8))
    assertEquals(shown.jobs.ready, None)
    assertEquals(shown.jobs.shelf.shown, Some(StoryMoments.run8))
  }

  test("System board · draft chip with a blocker, and the stale figure note") {
    val t2      = StoryModels.t2
    val p11     = TrialKey("P11", Phase.Retrieval, "ret_05", 1)
    val blocker = StudioDiagnostic(
      "studio.matched-cardinality",
      DiagnosticLevel.Error,
      DiagnosticOrigin.Host,
      Vector(DiagnosticLocus.Trial(p11)),
      "2 matched references for P11 ret_05 (occurrences 1, 2)"
    )
    val draft: Draft = t2.draft.get
    val model        = AppModel
      .update(
        AppModel.open(t2, None),
        Intent.SessionChanged(
          SessionFacts.empty.copy(draftCheck = DraftCheck.Checked(draft, Vector(blocker)))
        )
      )
      ._1
    val chip = Shell.context(model).draft
    assertEquals(chip.map(_.text), Some("Draft rev 5 · 1 change · 1 blocker"))
    assertEquals(chip.map(_.blocked), Some(true))
  }

  test("The badge and draft chip say what S2.7's reference rendering says") {
    for model <- Vector(StoryModels.t2Compare, StoryModels.t3Summary, StoryModels.t2Analysis) do
      val context = Shell.context(model)
      assertEquals(context.freshness.text, FreshnessText.badge(model.freshness.badge))
      assertEquals(context.draft.map(_.text), model.freshness.draft.map(FreshnessText.draft))
      // The newer chip adds the job's percentage to the reference wording.
      assertEquals(
        context.newer.map(_.text.stripSuffix(" · 48%")),
        FreshnessText.newer(model.freshness.badge)
      )
  }

  test("Compare with no completed run: 'Compare · No run yet' and Open Analysis") {
    val t2   = StoryModels.t2
    val none = ok(
      StudioDocument.of(
        t2.datasets,
        t2.analyses,
        t2.draft,
        Vector.empty,
        t2.reporting,
        Vector.empty,
        PresentationState.default,
        Vector.empty
      )
    )
    val model = AppModel
      .update(AppModel.open(none, None), Intent.SwitchPerspective(Perspective.Results))
      ._1
    val banner = Shell.banner(model)
    assertEquals(banner.map(_.lead), Some("Compare · No run yet"))
    assertEquals(
      banner.map(_.actions.map(_.intent)),
      Some(Vector(Intent.SwitchPerspective(Perspective.Analysis)))
    )
    assertEquals(Shell.context(model).freshness.text, "Analysis rev 4 · no run yet · data r3")
  }

  test("The banner shows only in Compare and Figures") {
    val t2 = StoryModels.t2Compare
    for p <- Perspective.values do
      val model = AppModel.update(t2, Intent.SwitchPerspective(p))._1
      val shown = p == Perspective.Results || p == Perspective.Report
      assertEquals(Shell.banner(model).isDefined, shown, p)
  }
