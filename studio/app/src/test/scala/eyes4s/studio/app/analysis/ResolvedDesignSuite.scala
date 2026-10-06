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

package eyes4s.studio.app.analysis

import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.vm.A11yRole
import eyes4s.studio.app.{AppEffect, AppModel, Intent, PreparedDesign, StoryModels}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{CoreBinding, GridSize, RecipeChange, RunLifecycle}
import eyes4s.studio.core.execution.{ExecutionEffect, RunStamp}
import eyes4s.studio.core.fixture.{MockStudy, StoryMoments}
import eyes4s.studio.core.preview.*
import eyes4s.studio.core.selection.{DesignCount, StudioRef}

/** The resolved-design pane's pure model and view-model (ticket S7.5) at
  * t2's draft rev 5: the backend's counts on the chips, counting state,
  * the stamp, filters, the row cursor, every row opening its trial, and a
  * run of the previewed design submitting that prepared design (E2E-05).
  */
class ResolvedDesignSuite extends munit.FunSuite:
  import StoryMoments.{r3, rev5}

  private def ok[E, A](e: Either[E, A]): A = e.fold(err => fail(s"$err"), identity)

  private val model: AppModel = StoryModels.t2Analysis
  private val stamp: RunStamp = AppModel.stampOf(model.document, rev5, r3)
  private val id              = PreviewId(1L)

  private val candidates = ok(PreviewCandidates.of(466, 471, 24, 219486L, 480, 14, None))
  private val counts     = ok(PreviewCounts.of(8969L, 44845L, 457, 9, 0))
  private val ready      = ok(PreviewReady.of(id, stamp, candidates, counts, Vector.empty))

  private val ret07 = MockStudy.key("P17", "ret_07")
  private val ret09 = MockStudy.key("P17", "ret_09")
  private val ret11 = MockStudy.key("P03", "ret_11")
  private val ret01 = MockStudy.key("P03", "ret_01")

  private val noMatch = StudioDiagnostic(
    "study-finding.unmatched-focal",
    DiagnosticLevel.Warning,
    DiagnosticOrigin.EyesCore,
    Vector(DiagnosticLocus.Trial(ret11)),
    "enc_11 quarantined (overlap)"
  )
  private val rows = Vector(
    PreviewRow(
      ret07,
      "beach-042",
      Response("Remembered"),
      MockStudy.key("P17", "enc_03"),
      Some(19),
      Eligibility.Eligible
    ),
    PreviewRow(
      ret09,
      "market-066",
      Response("Remembered"),
      ret09,
      None,
      Eligibility.QueryNotAdmitted(TrialDisposition.Absent)
    ),
    PreviewRow(
      ret11,
      "dog-217",
      Response("Forgotten"),
      ret11,
      None,
      Eligibility.NoMatch(noMatch)
    ),
    PreviewRow(
      ret01,
      "stadium-747",
      Response("Remembered"),
      MockStudy.key("P03", "enc_01"),
      Some(18),
      Eligibility.Eligible
    )
  )

  private def page(from: Int, size: Int): PreviewPage =
    val request = ok(PageRequest.of(from, size))
    val slice   = rows.slice(from, from + size)
    PreviewPage(rev5, PageInfo.of(request, rows.size, slice.size), slice)

  private def step(
      panel: ResolvedDesign,
      intents: DesignIntent*
  ): (ResolvedDesign, Vector[DesignEffect]) =
    intents.foldLeft((panel, Vector.empty[DesignEffect])) { case ((p, effects), i) =>
      val (next, more) = ResolvedDesign.update(p, i)
      (next, effects ++ more)
    }

  private lazy val synced: (ResolvedDesign, Vector[DesignEffect]) =
    ResolvedDesign.sync(ResolvedDesign.empty, model)

  private def counted: ResolvedDesign =
    val g = synced._1.generation
    step(
      synced._1,
      DesignIntent.Previewed(g, PreviewEvent.Initial(id, stamp, candidates)),
      DesignIntent.Previewed(g, PreviewEvent.Ready(ready)),
      DesignIntent.RowsRead(g, Right(page(0, rows.size)))
    )._1

  private def chip(vm: ResolvedDesignVM, f: DesignFilter): ChipVM =
    vm.chips.find(_.filter == f).getOrElse(fail(s"no chip $f"))

  test("t2's draft starts bounded preparation before any row reads") {
    val (panel, effects) = synced
    val target           = panel.target.getOrElse(fail("no target"))
    assertEquals((target.revision, target.dataset, target.stamp), (rev5, r3, stamp))
    assertEquals(target.recipe, model.document.draftRecipe.getOrElse(fail("no draft")))
    assertEquals(
      effects,
      Vector(
        DesignEffect.StartPreview(panel.generation, rev5, ok(PreviewBudget.of(6)))
      )
    )
    assertEquals(ResolvedDesign.sync(panel, model), (panel, Vector.empty))
    val vm = ResolvedDesignVM.of(panel)
    assertEquals(vm.mode, "preparing")
    assertEquals(vm.chips.map(_.count), Vector("…", "…", "…", "…", "…"))
    assertEquals(vm.rowsNote, Some("Reading the resolved design…"))
  }

  test("counting shows the backend's before-paging counts and the participant progress") {
    val g          = synced._1.generation
    val (panel, e) = step(
      synced._1,
      DesignIntent.Previewed(g, PreviewEvent.Initial(id, stamp, candidates)),
      DesignIntent.Previewed(g, PreviewEvent.Counting(id, ok(PreviewProgress.of(18, 24))))
    )
    assertEquals(e, Vector.empty)
    val vm = ResolvedDesignVM.of(panel)
    assertEquals(vm.mode, "paging")
    assert(!vm.exact)
    assertEquals(
      vm.counting.map(c => (c.text, c.refs)),
      Some(
        (
          "counting eligible pairs… 18 of 24 participants",
          Vector(StudioRef.DesignTally(rev5, DesignCount.Participants))
        )
      )
    )
    assertEquals(
      vm.chips.map(c => (c.label, c.count, c.enabled)),
      Vector(
        ("All requested", "480", true),
        ("Eligible", "…", true),
        ("No match", "…", true),
        ("Query not admitted", "14", true),
        ("No study trial, by design", "—", false)
      )
    )
    assertEquals(
      chip(vm, DesignFilter.All).ref,
      Some(StudioRef.DesignTally(rev5, DesignCount.RequestedQueries))
    )
    assertEquals(chip(vm, DesignFilter.Eligible).ref, None)
    assertEquals(chip(vm, DesignFilter.ByDesign).ref, None)
    assertEquals(
      vm.notes.map(_.text),
      Vector(
        "Candidate pairs before paging 219,486 per scale (466 queries × 471 references, " +
          "Cartesian · eyes4s candidatePairCount).",
        "While paging, counts read counting eligible pairs… 18 of 24 participants and the " +
          "run stays disabled.",
        "input digest unbound · plan unbound · rev 5 · data r3"
      )
    )
    // A bounded page ended before the last participant: count the next.
    assertEquals(
      step(panel, DesignIntent.PageEnded(g))._2,
      Vector(DesignEffect.ContinuePreview(g, id, ok(PreviewBudget.of(6))))
    )
  }

  test("the ready receipt shows exact counts and goes to the app as the prepared design") {
    val g      = synced._1.generation
    val (p, e) = step(
      synced._1,
      DesignIntent.Previewed(g, PreviewEvent.Initial(id, stamp, candidates)),
      DesignIntent.Previewed(g, PreviewEvent.Ready(ready))
    )
    val recipe = model.document.draftRecipe.getOrElse(fail("no draft"))
    assertEquals(
      e,
      Vector(
        DesignEffect.App(Intent.DesignPrepared(PreparedDesign(ready, recipe))),
        DesignEffect.ReadRows(g, rev5, ok(PageRequest.first(120)))
      )
    )
    assertEquals(step(p, DesignIntent.PageEnded(g))._2, Vector.empty)
    val vm = ResolvedDesignVM.of(p)
    assertEquals(vm.mode, "paged · exact")
    assertEquals(vm.counting, None)
    assertEquals(vm.chips.map(_.count), Vector("480", "457", "9", "14", "—"))
    assertEquals(
      vm.chips.flatMap(_.ref),
      Vector(
        StudioRef.DesignTally(rev5, DesignCount.RequestedQueries),
        StudioRef.DesignTally(rev5, DesignCount.EligibleQueries),
        StudioRef.DesignTally(rev5, DesignCount.UnmatchedQueries),
        StudioRef.DesignTally(rev5, DesignCount.QueriesNotAdmitted)
      )
    )
    assertEquals(
      vm.notes.map(n => (n.text, n.refs)),
      Vector(
        (
          "Candidate pairs before paging 219,486 per scale (466 queries × 471 references, " +
            "Cartesian · eyes4s candidatePairCount).",
          Vector(
            StudioRef.DesignTally(rev5, DesignCount.CandidatePairsPerScale),
            StudioRef.DesignTally(rev5, DesignCount.FocalTrials),
            StudioRef.DesignTally(rev5, DesignCount.ReferenceTrials)
          )
        ),
        (
          "Exact eligible after paging 8,969 per scale.",
          Vector(StudioRef.DesignTally(rev5, DesignCount.EligiblePairsPerScale))
        ),
        ("Preview and run use this same prepared design.", Vector.empty),
        ("input digest unbound · plan unbound · rev 5 · data r3", Vector.empty)
      )
    )
  }

  test("a by-design category the recipe declares is a chip with its count") {
    val g      = synced._1.generation
    val lures  = ok(PreviewCandidates.of(466, 471, 24, 219486L, 480, 14, Some(0)))
    val (p, _) =
      step(synced._1, DesignIntent.Previewed(g, PreviewEvent.Initial(id, stamp, lures)))
    val byDesign = chip(ResolvedDesignVM.of(p), DesignFilter.ByDesign)
    assertEquals((byDesign.count, byDesign.enabled), ("0", true))
    assertEquals(byDesign.ref, Some(StudioRef.DesignTally(rev5, DesignCount.ByDesignQueries)))
  }

  test("answers of an older generation or another stamp never prepare a design") {
    val g           = synced._1.generation
    val other       = stamp.copy(dataset = DatasetRevision(2))
    val (stale, e1) =
      step(synced._1, DesignIntent.Previewed(g - 1, PreviewEvent.Ready(ready)))
    assertEquals((stale, e1), (synced._1, Vector.empty))
    val (refused, e2) = step(
      synced._1,
      DesignIntent.Previewed(
        g,
        PreviewEvent.Ready(ok(PreviewReady.of(id, other, candidates, counts, Vector.empty)))
      )
    )
    assertEquals(e2, Vector.empty)
    assertEquals(
      ResolvedDesignVM.of(refused).problem,
      Some(
        "The backend did not prepare the design: the backend prepared rev 5 · data r2, " +
          "not rev 5 · data r3"
      )
    )
    val (failed, e3) =
      step(synced._1, DesignIntent.PreviewRefused(g, "No analysis rev 9"))
    assertEquals(e3, Vector.empty)
    assertEquals(
      ResolvedDesignVM.of(failed).problem,
      Some("The backend did not prepare the design: No analysis rev 9")
    )
  }

  test("rows are read page by page and show their status in the board's words") {
    val g       = synced._1.generation
    val (p1, e) = step(synced._1, DesignIntent.RowsRead(g, Right(page(0, 3))))
    assertEquals(e, Vector(DesignEffect.ReadRows(g, rev5, ok(PageRequest.of(3, 120)))))
    assertEquals(p1.rowState, DesignRows.Waiting)
    val (p2, e2) = step(p1, DesignIntent.RowsRead(g, Right(page(3, 120))))
    assertEquals((p2.rowState, e2), (DesignRows.Complete, Vector.empty))
    val vm = ResolvedDesignVM.of(p2)
    assertEquals(
      vm.rows.map(r =>
        (r.participant, r.trial, r.item, r.matched, r.controls, r.status, r.tone)
      ),
      Vector(
        (
          "P17",
          "ret_07",
          "beach-042",
          Some("enc_03"),
          Some("19"),
          "Eligible",
          StatusTone.Plain
        ),
        (
          "P17",
          "ret_09",
          "market-066",
          None,
          None,
          "Query not admitted · absent from fixations.csv",
          StatusTone.Warning
        ),
        (
          "P03",
          "ret_11",
          "dog-217",
          None,
          None,
          "No match · enc_11 quarantined (overlap)",
          StatusTone.Warning
        ),
        (
          "P03",
          "ret_01",
          "stadium-747",
          Some("enc_01"),
          Some("18"),
          "Eligible",
          StatusTone.Plain
        )
      )
    )
    assertEquals(vm.rows.map(_.ref), rows.map(r => StudioRef.Trial(r.query)))
    assertEquals(vm.columns, Vector("P", "Query", "Item", "Matched", "Controls", "Status"))
    val (failed, _) = step(synced._1, DesignIntent.RowsRead(g, Left("backend closed")))
    assertEquals(
      ResolvedDesignVM.of(failed).rowsNote,
      Some("The resolved design could not be read: backend closed")
    )
  }

  test("filter chips show only the queries of their status") {
    def shown(f: DesignFilter) =
      ResolvedDesignVM.of(step(counted, DesignIntent.ChooseFilter(f))._1).rows.map(_.query)
    assertEquals(shown(DesignFilter.All), rows.map(_.query))
    assertEquals(shown(DesignFilter.Eligible), Vector(ret07, ret01))
    assertEquals(shown(DesignFilter.NoMatch), Vector(ret11))
    assertEquals(shown(DesignFilter.NotAdmitted), Vector(ret09))
    // No by-design category: the chip is refused and every row stays shown.
    assertEquals(shown(DesignFilter.ByDesign), rows.map(_.query))
    val g        = synced._1.generation
    val lures    = ok(PreviewCandidates.of(466, 471, 24, 219486L, 480, 14, Some(0)))
    val byDesign = step(
      synced._1,
      DesignIntent.Previewed(g, PreviewEvent.Initial(id, stamp, lures)),
      DesignIntent.RowsRead(g, Right(page(0, rows.size))),
      DesignIntent.ChooseFilter(DesignFilter.ByDesign)
    )._1
    val empty = ResolvedDesignVM.of(byDesign)
    assertEquals(empty.rows, Vector.empty)
    assertEquals(empty.rowsNote, Some("No query has this status."))
    assert(ResolvedDesignVM.of(counted).chips.head.on)
  }

  test("one row cursor: arrow keys, Home/End and Page keys move it within the shown rows") {
    def moves(p: ResolvedDesign, ms: RowMove*) = step(p, ms.map(DesignIntent.Move(_))*)._1
    assertEquals(moves(counted, RowMove.Down).cursor, Some(ret07))
    assertEquals(moves(counted, RowMove.Last).cursor, Some(ret01))
    assertEquals(moves(counted, RowMove.Down, RowMove.Down, RowMove.Up).cursor, Some(ret07))
    assertEquals(moves(counted, RowMove.Down, RowMove.Up).cursor, Some(ret07))
    assertEquals(moves(counted, RowMove.Down, RowMove.PageDown).cursor, Some(ret01))
    assertEquals(moves(counted, RowMove.Last, RowMove.PageUp).cursor, Some(ret07))
    assertEquals(moves(counted, RowMove.Last, RowMove.First).cursor, Some(ret07))
    val vm = ResolvedDesignVM.of(moves(counted, RowMove.Down, RowMove.Down))
    assertEquals(vm.cursor, Some(1))
    assertEquals(vm.rows.map(_.focused), Vector(false, true, false, false))
    // A filter keeps a cursor whose row it shows, and drops one it hides.
    val onRet09 = moves(counted, RowMove.Down, RowMove.Down)
    assertEquals(
      step(onRet09, DesignIntent.ChooseFilter(DesignFilter.NotAdmitted))._1.cursor,
      Some(ret09)
    )
    assertEquals(
      step(onRet09, DesignIntent.ChooseFilter(DesignFilter.Eligible))._1.cursor,
      None
    )
    val vmCounted = ResolvedDesignVM.of(counted)
    assertEquals(
      (vmCounted.accessible, vmCounted.accessibleHelp),
      ("Resolved design, rev 5; arrow keys move the row cursor", "480 requested queries")
    )
    // The table is one stop after the chips that can be chosen.
    assertEquals(
      vmCounted.focusStops.map(s => (s.role, s.name)),
      Vector(
        A11yRole.ToggleButton -> "All requested",
        A11yRole.ToggleButton -> "Eligible",
        A11yRole.ToggleButton -> "No match",
        A11yRole.ToggleButton -> "Query not admitted",
        A11yRole.Table        -> "Resolved design, rev 5; arrow keys move the row cursor"
      )
    )
  }

  test("every status row opens its trial, by Enter or by a click") {
    DesignFilter.values.foreach { f =>
      val filtered = step(counted, DesignIntent.ChooseFilter(f))._1
      filtered.visible.foreach { r =>
        val opened = Vector(
          DesignEffect.App(Intent.Explain(Place.At(StudioRef.Trial(r.query))))
        )
        val (clicked, e1) = step(filtered, DesignIntent.OpenRow(r.query))
        assertEquals((clicked.cursor, e1), (Some(r.query), opened), (f, r.query))
        val (_, e2) = step(filtered, DesignIntent.FocusRow(r.query), DesignIntent.OpenFocused)
        assertEquals(e2, opened, (f, r.query))
      }
    }
    assertEquals(step(counted, DesignIntent.OpenFocused)._2, Vector.empty)
    // A row the filter hides is not opened.
    val eligible = step(counted, DesignIntent.ChooseFilter(DesignFilter.Eligible))._1
    assertEquals(step(eligible, DesignIntent.OpenRow(ret09))._2, Vector.empty)
    // Opening lands on the trial in Explore, with its participant above it.
    val (m, _) = AppModel.update(model, Intent.Explain(Place.At(StudioRef.Trial(ret11))))
    assertEquals(m.perspective, eyes4s.studio.core.document.Perspective.Explore)
    assertEquals(m.location.trail.last, Place.At(StudioRef.Trial(ret11)))
  }

  test("E2E-05: Save & run of the previewed draft submits the prepared design itself") {
    val recipe   = model.document.draftRecipe.getOrElse(fail("no draft"))
    val prepared =
      AppModel.update(model, Intent.DesignPrepared(PreparedDesign(ready, recipe)))._1
    val (ran, effects) = AppModel.update(prepared, Intent.Dispatch(Command.SaveAndRun(None)))
    assert(
      effects.contains(
        AppEffect.Execution(ExecutionEffect.SubmitPreview(ready, Some(RunId(8))))
      ),
      effects
    )
    assert(!effects.exists {
      case AppEffect.Execution(ExecutionEffect.Submit(_)) => true
      case _                                              => false
    })
    // The prepared stamp is the run's own: no separate Require accompanies it.
    assert(!effects.exists {
      case AppEffect.Execution(ExecutionEffect.Require(_)) => true
      case _                                               => false
    })
    assertEquals(ran.jobs.shelf.required, Some(ready.stamp))
    assertEquals(AppModel.stampOf(ran.document, rev5, r3), ready.stamp)
    // A prepared design is submitted once.
    assertEquals(ran.prepared, None)
  }

  test("native artifacts are accepted only with the exact current recipe snapshot") {
    val recipe = model.document.draftRecipe.getOrElse(fail("no draft"))
    val native = stamp.copy(
      plan = CoreBinding.Bound(
        ok(
          eyes4s.codec.CanonicalDigest
            .parse[eyes4s.studio.core.document.StudyPlanArtifact]("1" * 64)
        )
      ),
      input = CoreBinding.Bound(
        ok(
          eyes4s.codec.CanonicalDigest
            .parse[eyes4s.studio.core.execution.StudyInputArtifact]("2" * 64)
        )
      )
    )
    val receipt =
      ok(PreviewReady.of(id, native, candidates, counts, Vector.empty, Some(recipe)))
    val g                   = synced._1.generation
    val (accepted, effects) = step(
      synced._1,
      DesignIntent.Previewed(g, PreviewEvent.Initial(id, native, candidates)),
      DesignIntent.Previewed(g, PreviewEvent.Ready(receipt))
    )
    assertEquals(accepted.preview.receipt, Some(receipt))
    assertEquals(
      effects,
      Vector(
        DesignEffect.App(Intent.DesignPrepared(PreparedDesign(receipt, recipe))),
        DesignEffect.ReadRows(g, rev5, ok(PageRequest.first(120)))
      )
    )
    val waiting = step(
      synced._1,
      DesignIntent.Previewed(g, PreviewEvent.Initial(id, native, candidates))
    )._1
    val missing = ok(PreviewReady.of(id, native, candidates, counts, Vector.empty))
    assert(
      step(waiting, DesignIntent.Previewed(g, PreviewEvent.Ready(missing)))._1.preview
        .isInstanceOf[DesignPreview.Refused]
    )
    val changed = recipe.copy(grid = ok(GridSize.of(32, 24)))
    val stale = ok(PreviewReady.of(id, native, candidates, counts, Vector.empty, Some(changed)))
    assert(
      step(waiting, DesignIntent.Previewed(g, PreviewEvent.Ready(stale)))._1.preview
        .isInstanceOf[DesignPreview.Refused]
    )
    val unsolicited =
      ok(PreviewReady.of(PreviewId(2), native, candidates, counts, Vector.empty, Some(recipe)))
    assert(
      step(waiting, DesignIntent.Previewed(g, PreviewEvent.Ready(unsolicited)))._1.preview
        .isInstanceOf[DesignPreview.Refused]
    )
    assertEquals(
      step(accepted, DesignIntent.Previewed(g, PreviewEvent.Ready(receipt))),
      (accepted, Vector.empty)
    )
    val prepared =
      AppModel.update(model, Intent.DesignPrepared(PreparedDesign(receipt, recipe)))._1
    val (ran, runEffects) = AppModel.update(prepared, Intent.Dispatch(Command.SaveAndRun(None)))
    assert(
      runEffects.contains(
        AppEffect.Execution(ExecutionEffect.SubmitPreview(receipt, Some(RunId(8))))
      )
    )
    assertEquals(ran.jobs.shelf.required, Some(native))
  }

  test("native preview progress names actual work without inventing participant completion") {
    val g          = synced._1.generation
    val work       = PreviewEvent.CountingWork(id, PairDesign.Control, 1024, 3072L)
    val (panel, _) = step(
      synced._1,
      DesignIntent.Previewed(g, PreviewEvent.Initial(id, stamp, candidates)),
      DesignIntent.Previewed(g, work)
    )
    assertEquals(panel.workProgress.map(p => p: PreviewEvent), Some(work))
    assertEquals(panel.preview, DesignPreview.Counting(id, stamp, candidates, None))
    assertEquals(ResolvedDesignVM.of(panel).counting.map(_.refs), Some(Vector.empty))
  }

  test(
    "E2E-05: a refused prepared design settles the requested run without bypassing the refusal"
  ) {
    val recipe   = model.document.draftRecipe.getOrElse(fail("no draft"))
    val prepared =
      AppModel.update(model, Intent.DesignPrepared(PreparedDesign(ready, recipe)))._1
    val (ran, _) = AppModel.update(prepared, Intent.Dispatch(Command.SaveAndRun(None)))
    val running  = ran.document.running.map(_.id)
    assert(running.nonEmpty)
    val error = eyes4s.studio.core.execution.ExecutionError
      .Backend(BackendError.UnknownPreview(ready.id, Vector.empty))
    val (fell, effects) =
      AppModel.update(ran, Intent.PreparedRefused(ready, error, running.headOption))
    assert(!effects.exists(_.isInstanceOf[AppEffect.Execution]))
    assertEquals(fell.prepared, None)
    assertEquals(fell.notice.map(_.message), Some(error.message))
    assertEquals(fell.document.running.map(_.id), Vector.empty)
    running.foreach(id =>
      assertEquals(fell.document.run(id).map(_.state), Some(RunLifecycle.Failed))
    )
  }

  test("E2E-05: a draft edited after its preview runs its own stamp, not the stale design") {
    val recipe   = model.document.draftRecipe.getOrElse(fail("no draft"))
    val prepared =
      AppModel.update(model, Intent.DesignPrepared(PreparedDesign(ready, recipe)))._1
    val edit = Command.ChangeRecipe(RecipeChange.Grid(recipe.grid, ok(GridSize.of(32, 24))))
    val (edited, _) = AppModel.update(prepared, Intent.Dispatch(edit))
    assertNotEquals(edited.document.draftRecipe, Some(recipe))
    val (_, effects) = AppModel.update(edited, Intent.Dispatch(Command.SaveAndRun(None)))
    assert(effects.contains(AppEffect.Execution(ExecutionEffect.Submit(stamp))), effects)
    // The edit is a new target: the pane prepares the edited draft again.
    val (again, restart) = ResolvedDesign.sync(counted, edited)
    assertEquals(again.preview, DesignPreview.Preparing)
    // ...and withdraws the design it had prepared.
    assertEquals(restart.headOption, Some(DesignEffect.App(Intent.DesignWithdrawn)))
    assertEquals(AppModel.update(prepared, Intent.DesignWithdrawn)._1.prepared, None)
    assert(restart.exists {
      case DesignEffect.StartPreview(_, `rev5`, _) => true
      case _                                       => false
    })
    // Without any prepared design, a run submits its stamp as before.
    val (_, plain) = AppModel.update(model, Intent.Dispatch(Command.SaveAndRun(None)))
    assert(plain.contains(AppEffect.Execution(ExecutionEffect.Submit(stamp))), plain)
    assertEquals(stamp.plan, CoreBinding.unbound)
  }

  test("a delayed preview refusal names its exact document run and preserves a newer receipt") {
    val recipe   = model.document.draftRecipe.getOrElse(fail("no draft"))
    val prepared =
      AppModel.update(model, Intent.DesignPrepared(PreparedDesign(ready, recipe)))._1
    val ran      = AppModel.update(prepared, Intent.Dispatch(Command.SaveAndRun(None)))._1
    val d        = ran.document
    val laterRun = eyes4s.studio.core.document.RunRef(
      RunId(99),
      rev5,
      r3,
      RunLifecycle.Running,
      CoreBinding.unbound
    )
    val document = ok(
      eyes4s.studio.core.document.StudioDocument.of(
        d.datasets,
        d.analyses,
        d.draft,
        d.runs :+ laterRun,
        d.reporting,
        d.figures,
        d.presentation,
        d.jobs
      )
    )
    val newerReady = ok(PreviewReady.of(PreviewId(2), stamp, candidates, counts, Vector.empty))
    val newer      = AppModel
      .update(
        AppModel.open(document, ran.project),
        Intent.DesignPrepared(PreparedDesign(newerReady, recipe))
      )
      ._1
    val error = eyes4s.studio.core.execution.ExecutionError
      .Backend(BackendError.UnknownPreview(ready.id, Vector.empty))
    val (settled, effects) =
      AppModel.update(newer, Intent.PreparedRefused(ready, error, Some(RunId(8))))
    assertEquals(settled.document.run(RunId(8)).map(_.state), Some(RunLifecycle.Failed))
    assertEquals(settled.document.run(RunId(99)).map(_.state), Some(RunLifecycle.Running))
    assertEquals(settled.prepared, newer.prepared)
    assert(!effects.exists(_.isInstanceOf[AppEffect.Execution]))
    val (unbound, unknownEffects) = AppModel.update(newer, Intent.PreparedRefused(ready, error))
    assertEquals(unbound.document.runs, newer.document.runs)
    assertEquals(unbound.prepared, newer.prepared)
    assertEquals(unknownEffects, Vector.empty)
  }

  test("nothing is asked of the backend until the Analysis perspective is shown") {
    val elsewhere = StoryModels.t2Compare
    assertNotEquals(elsewhere.perspective, eyes4s.studio.core.document.Perspective.Analysis)
    assertEquals(
      ResolvedDesign.sync(ResolvedDesign.empty, elsewhere),
      (ResolvedDesign.empty, Vector.empty)
    )
    val (shown, effects) = ResolvedDesign.sync(ResolvedDesign.empty, model)
    assert(shown.started)
    assert(effects.exists {
      case DesignEffect.StartPreview(_, `rev5`, _) => true
      case _                                       => false
    })
    // Once started it follows the target from any perspective.
    assertEquals(ResolvedDesign.sync(shown, elsewhere)._1.started, true)
  }

  test("the by-design chip cannot be chosen while the recipe has no such category") {
    val (p, e) = step(counted, DesignIntent.ChooseFilter(DesignFilter.ByDesign))
    assertEquals((p, e), (counted, Vector.empty))
    val g              = synced._1.generation
    val lures          = ok(PreviewCandidates.of(466, 471, 24, 219486L, 480, 14, Some(0)))
    val (withLures, _) =
      step(synced._1, DesignIntent.Previewed(g, PreviewEvent.Initial(id, stamp, lures)))
    assertEquals(
      step(withLures, DesignIntent.ChooseFilter(DesignFilter.ByDesign))._1.filter,
      DesignFilter.ByDesign
    )
  }
