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
import eyes4s.studio.core.document.FigureId
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
