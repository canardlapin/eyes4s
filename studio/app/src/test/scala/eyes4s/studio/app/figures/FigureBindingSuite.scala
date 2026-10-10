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

package eyes4s.studio.app.figures

import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.diff.{LedgerUnavailable, StatusChanges, StatusDiff}
import eyes4s.studio.core.document.{FigureId, ReportingId, ReportingSpec}
import eyes4s.studio.core.fixture.StoryMoments.{r2, r3, run5, run7}

/** The figure binding's behaviour in the Figures perspective, headless
  * (ticket S9.1; Figures.dc.html, left and inspector), on story moment t2:
  * Figure 1 binds run 7 (current), Figure 2 binds run 5 (stale).
  */
class FigureBindingSuite extends munit.FunSuite:

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val figure1      = ok(FigureId.of(1))
  private val figure2      = ok(FigureId.of(2))
  private def t2: AppModel = StoryModels.t2Figures

  private def step(
      b: FigureBinding,
      m: AppModel,
      i: FigureIntent
  ): (FigureBinding, Vector[FigureEffect]) = FigureBinding.update(b, m, i)

  private val unavailable = StatusDiff.Unavailable(
    r2,
    LedgerUnavailable.Refused(
      LedgerReadError.Refused(BackendError.Unavailable(DiagnosticLocus.Dataset(r2)))
    )
  )

  test("the navigator lists each figure with its binding and standing") {
    val vm = FigureBinding.view(FigureBinding.empty, t2)
    assertEquals(vm.header, "2 figures")
    assertEquals(
      vm.rows.map(r => (r.title, r.status, r.binding, r.reporting)),
      Vector(
        ("Figure 1", "current", "run 7 · rev 4 · data r3", "Reporting: By retrieval response"),
        ("Figure 2", "Stale", "run 5 · rev 3 · data r2", "Reporting: By retrieval response")
      )
    )
    assertEquals((vm.notice, vm.binding, vm.dialog), (None, None, None))
  }

  test("selecting stale Figure 2 asks for the r2 → r3 statuses and shows why it is stale") {
    val (selected, effects) = step(FigureBinding.empty, t2, FigureIntent.Select(figure2))
    assertEquals(effects, Vector(FigureEffect.RequestStatus(r2, r3)))
    val before = FigureBinding.view(selected, t2).notice.get
    assert(before.text.contains("dataset r3 changed the units and the mapping"), before.text)
    // The fake serves no ledger for r2: the cause stays the document's.
    val (read, _) = step(selected, t2, FigureIntent.StatusRead(r2, r3, unavailable))
    val notice    = FigureBinding.view(read, t2).notice.get
    assertEquals(
      notice,
      StaleNoticeVM(
        "Bound to run 5 · rev 3 · data r2, which is no longer current: dataset r3 changed " +
          "the units and the mapping; rev 4 (run 7) supersedes it. The figure still shows " +
          "run 5 exactly as it was.",
        "Rebind…",
        "Keep as rev 3"
      )
    )
    // With both ledgers compared (the real backend, S3.7), the board's cause.
    val key                        = TrialKey("P01", Phase.Retrieval, "ret_01", 1)
    def entry(d: TrialDisposition) = LedgerEntry(key, "item", None, d, Vector.empty)
    val compared                   = StatusDiff.Compared(
      ok(
        StatusChanges.between(
          r2,
          Vector(entry(TrialDisposition.Absent)),
          r3,
          Vector(entry(TrialDisposition.Admitted))
        )
      )
    )
    val (counted, _) = step(selected, t2, FigureIntent.StatusRead(r2, r3, compared))
    assert(
      FigureBinding
        .view(counted, t2)
        .notice
        .exists(_.text.contains("dataset r3 changed the admission status of 1 trial")),
      FigureBinding.view(counted, t2).notice
    )
    // A comparison already read is not asked for again.
    assertEquals(step(read, t2, FigureIntent.Select(figure2))._2, Vector.empty)
    // The inspector's binding section.
    assertEquals(
      FigureBinding.view(read, t2).binding.map(b => (b.title, b.lock, b.run, b.unit)),
      Some(
        (
          "Figure 2 binding",
          "Locked · scientific",
          "run 5 · rev 3 · data r2",
          "Participant means, equal weight"
        )
      )
    )
    // Rebinding replaces the binding; no figure version is kept (bead 4AHQ8QE5).
    val note = FigureBinding.view(read, t2).binding.map(_.note).getOrElse("")
    assert(note.contains("Rebinding replaces the binding in place; Undo restores"), note)
    assert(!note.contains("reproducible"), note)
  }

  test("Keep as rev 3 puts the notice away; the figure stays bound and stale") {
    val selected        = step(FigureBinding.empty, t2, FigureIntent.Select(figure2))._1
    val (kept, effects) = step(selected, t2, FigureIntent.Keep(figure2))
    assertEquals(effects, Vector.empty)
    val vm = FigureBinding.view(kept, t2)
    assertEquals(vm.notice, None)
    assertEquals(vm.rows(1).status, "Stale · kept")
    assertEquals(vm.rows(1).binding, "run 5 · rev 3 · data r2")
  }

  test("Rebind… shows the plan and data diff first; only Rebind dispatches BindFigure") {
    val selected             = step(FigureBinding.empty, t2, FigureIntent.Select(figure2))._1
    val (proposing, effects) = step(selected, t2, FigureIntent.Rebind(figure2))
    // r2 → r3 was already asked for on selection.
    assertEquals(effects, Vector.empty)
    assertEquals(
      step(FigureBinding.empty, t2, FigureIntent.Rebind(figure2))._2,
      Vector(FigureEffect.RequestStatus(r2, r3))
    )
    val dialog = FigureBinding.view(proposing, t2).dialog.get
    assertEquals(
      (dialog.title, dialog.plan, dialog.data, dialog.conflicts, dialog.canConfirm),
      (
        "Rebind Figure 2 to run 7?",
        "Plan: rev 3 → rev 4, no change to the analysis",
        "Data: r2 → r3, onset declared ms; occurrence → occurrence",
        Vector.empty,
        true
      )
    )
    // The status answer updates the open dialog's data diff.
    val (answered, _) = step(proposing, t2, FigureIntent.StatusRead(r2, r3, unavailable))
    assertEquals(
      FigureBinding.view(answered, t2).dialog.map(_.data),
      Some(
        "Data: r2 → r3, onset declared ms; occurrence → occurrence; trial status vs r2 unavailable"
      )
    )
    // Cancel changes nothing.
    val (cancelled, none) = step(answered, t2, FigureIntent.CancelRebind)
    assertEquals((FigureBinding.view(cancelled, t2).dialog, none), (None, Vector.empty))
    // Confirm dispatches the rebind, an ordinary undoable edit.
    val (done, confirm) = step(answered, t2, FigureIntent.ConfirmRebind)
    val reporting       = t2.document.figures(1).reporting
    assertEquals(
      confirm,
      Vector(FigureEffect.App(Intent.Dispatch(Command.BindFigure(figure2, run7, reporting))))
    )
    val rebound =
      AppModel.update(t2, Intent.Dispatch(Command.BindFigure(figure2, run7, reporting)))._1
    val vm = FigureBinding.view(done, rebound)
    assertEquals(vm.dialog, None)
    assertEquals(
      vm.rows(1).binding        -> vm.rows(1).status,
      "run 7 · rev 4 · data r3" -> "current"
    )
    assertEquals(vm.notice, None)
    assertEquals(t2.document.figures(1).run, run5)
  }

  test("rebinding to the run a figure already binds is refused, naming it") {
    // Figure 1 already binds run 7, the latest current run.
    val (b, effects) = step(FigureBinding.empty, t2, FigureIntent.Rebind(figure1))
    assertEquals(effects, Vector.empty)
    assertEquals(FigureBinding.view(b, t2).problem, Some("Figure 1 already binds run 7."))
  }

  // --- Review follow-ups ------------------------------------------------------

  test("a rebind confirmed after the figure changed is shown again, not dispatched") {
    val proposing = step(FigureBinding.empty, t2, FigureIntent.Rebind(figure2))._1
    // Elsewhere, Figure 2 is bound to another reporting spec of the same run.
    val other   = ok(ReportingSpec.grouped(ok(ReportingId.of("zz-other")), "Other", None))
    val changed = StoryModels.play(
      t2,
      _ => Intent.Dispatch(Command.PutReporting(other)),
      _ => Intent.Dispatch(Command.BindFigure(figure2, run5, other.id))
    )
    assertEquals(changed.document.figures(1).reporting, other.id)
    val (again, effects) = step(proposing, changed, FigureIntent.ConfirmRebind)
    assertEquals(effects, Vector.empty)
    assertEquals(again.rebind.map(_.reporting), Some(other.id))
    assertEquals(
      FigureBinding.view(again, changed).problem,
      Some("Figure 2 or its runs changed since the rebind was proposed; review it again.")
    )
    // Confirming the refreshed proposal dispatches it.
    assertEquals(
      step(again, changed, FigureIntent.ConfirmRebind)._2,
      Vector(FigureEffect.App(Intent.Dispatch(Command.BindFigure(figure2, run7, other.id))))
    )
    // A proposal the document no longer allows is withdrawn, with why.
    val rebound = StoryModels.play(
      t2,
      _ => Intent.Dispatch(Command.BindFigure(figure2, run7, t2.document.figures(1).reporting))
    )
    val (gone, none) = step(proposing, rebound, FigureIntent.ConfirmRebind)
    assertEquals((gone.rebind, none), (None, Vector.empty))
    assertEquals(gone.problem, Some("Figure 2 already binds run 7."))
  }

  test("a keep is of the run kept: a later stale binding of the figure is not hidden") {
    val kept = step(FigureBinding.empty, t2, FigureIntent.Keep(figure2))._1
    assertEquals(kept.kept, Vector(figure2 -> run5))
    // Figure 2 rebound to run 7, then run 8 (rev 5) completes: run 7 is stale.
    val t3    = StoryModels.t3
    val later = AppModel.open(
      ok(
        eyes4s.studio.core.document.StudioDocument.of(
          t3.datasets,
          t3.analyses,
          t3.draft,
          t3.runs.map(r =>
            if r.id == eyes4s.studio.core.fixture.StoryMoments.run8 then
              r.copy(state = eyes4s.studio.core.document.RunLifecycle.Completed)
            else r
          ),
          t3.reporting,
          t3.figures.map(f =>
            if f.id == figure2 then
              ok(eyes4s.studio.core.document.FigureSpec.of(f.id, run7, f.reporting, f.panels))
            else f
          ),
          t3.presentation,
          Vector.empty
        )
      ),
      None
    )
    val selected = step(kept, later, FigureIntent.Select(figure2))._1
    val vm       = FigureBinding.view(selected, later)
    assertEquals(vm.rows(1).status, "Stale")
    assert(vm.notice.exists(_.text.startsWith("Bound to run 7 · rev 4 · data r3")), vm.notice)
  }

  test("a figure whose binding does not resolve is a row that says why") {
    val figure9 = ok(FigureId.of(9))
    val row     = FigureBinding.row(
      FigureBinding.empty,
      figure9,
      Left(eyes4s.studio.core.figures.FigureError.UnknownRun(figure9, RunId(42))),
      None
    )
    assertEquals(
      (row.title, row.status, row.binding),
      ("Figure 9", "unresolved", "Figure 9 binds run 42, which the document does not have.")
    )
  }

  test("a status answer for other revisions than asked is reported, not used") {
    val selected = step(FigureBinding.empty, t2, FigureIntent.Select(figure2))._1
    val wrong    =
      StatusDiff.Compared(ok(StatusChanges.between(r3, Vector.empty, r2, Vector.empty)))
    val (after, _) = step(selected, t2, FigureIntent.StatusRead(r2, r3, wrong))
    assertEquals(after.status(r2, r3), StatusDiff.NotRead)
    assertEquals(
      after.problem,
      Some("The trial status comparison asked for r2 → r3 answered r3 → r2; it was not used.")
    )
  }
