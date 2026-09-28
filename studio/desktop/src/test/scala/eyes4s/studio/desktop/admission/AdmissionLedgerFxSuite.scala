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

package eyes4s.studio.desktop.admission

import cats.data.NonEmptyVector
import eyes4s.studio.app.admission.{AdmissionLedger, LedgerDecision}
import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.app.layout.StudioLayouts
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.vm.Shell
import eyes4s.studio.app.{AppEffect, StoryModels}
import eyes4s.studio.core.backend.TrialDisposition
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.freshness.{RunStanding, StaleReason}
import eyes4s.studio.core.selection.{InventoryKind, StudioRef}
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.{FxStage, StudioTheme}
import eyes4s.studio.desktop.runtime.EffectProblem
import eyes4s.studio.desktop.shell.ShellFxSuite
import javafx.scene.input.{KeyCode, KeyEvent}

import scala.jdk.CollectionConverters.*

/** The admission ledger hosted in the Data perspective's admission pane
  * (ticket S5.6; Data.dc.html, admission), in a studio window over the fake
  * backend at story moment t1: the fixture's 960 = 937 + 17 (by cause) + 6;
  * every count is a button that opens exactly its trials; Require complete
  * refuses r3 while 17 trials are quarantined; Review exclusions admits it
  * through the window's own verification (the app's `RequestAdmission`,
  * answered by the backend), and run 5, on r2, becomes stale.
  *
  * Every interaction goes through the controls' own events, so no test
  * depends on OS window focus.
  */
class AdmissionLedgerFxSuite extends ShellFxSuite:
  import StoryMoments.{r2, r3, run5}

  /** A window at t1 with the ledger's counts and entries read. */
  private def ready(fx: FxStage): StudioWindow =
    assumeFullStage(fx)
    val w = boot(fx, StoryModels.t1Data, StoryMoment.T1)
    eventually(fx, "the ledger's counts and entries") {
      val s = w.admission.state
      s.counts.toOption.isDefined && s.entries.toOption.isDefined
    }
    w

  private def rows(w: StudioWindow): Vector[(String, String)] = runOnFx {
    w.admission.view.rows.map(r => r.label.getText -> r.count.getText)
  }

  private def fire(fx: FxStage, button: javafx.scene.control.ButtonBase): Unit =
    runOnFx(button.fire())
    fx.awaitLayout()

  private def trail(w: StudioWindow): Vector[String] =
    runOnFx(Shell.context(w.runtime.model).trail.map(_.label))

  fxStage.test("t1: the board's ledger, 960 = 937 + 17 (by cause) + 6") { fx =>
    val w = ready(fx)
    // The ledger is the Data perspective's admission pane.
    val pane = runOnFx(w.host.node(StudioLayouts.admission))
      .getOrElse(fail("the admission pane was not built"))
    assert(runOnFx(pane.getScene eq fx.scene))
    assert(runOnFx(w.admission.node.getScene eq fx.scene))
    val v = w.admission.view
    assertEquals(runOnFx(v.header.getText), "Counted from the trials.csv inventory")
    assertEquals(
      rows(w),
      Vector(
        "Inventory"                                                         -> "960",
        "Admitted"                                                          -> "937",
        "Quarantined — whole trial held back when any record is invalid"    -> "17",
        "duplicate-ordinals"                                                -> "4",
        "no-fixations"                                                      -> "5",
        "overlap"                                                           -> "6",
        "rejected-records"                                                  -> "2",
        "Absent — in trials.csv, no fixation records at all"                -> "6",
        "Outside screen — excluded from maps and reported, not quarantined" -> "0"
      )
    )
    assertEquals(runOnFx(v.equation.getText), "937 + 17 + 6 = 960.")
    assertEquals(
      runOnFx(v.rows(3).count.getAccessibleText),
      "4 trials quarantined for duplicate-ordinals"
    )
    assertEquals(runOnFx(v.decisionTitle.getText), "Admit dataset r3")
    assertEquals(
      runOnFx(LedgerDecision.values.toVector.map(d => v.choices(d).getText)),
      Vector("Require complete", "Review exclusions")
    )
    assertEquals(
      runOnFx(v.choiceNotes(LedgerDecision.ReviewExclusions).getText),
      "Admits 937 trials; 17 quarantined and 6 absent are recorded with their causes in r3."
    )
    assertEquals(
      runOnFx(v.consequence.getText),
      "Admitting creates dataset r3. Run 5 stays on r2 and is marked stale."
    )
    // Require complete, the default, refuses r3 while trials are quarantined.
    assertEquals(runOnFx(v.choices(LedgerDecision.RequireComplete).isSelected), true)
    assertEquals(runOnFx(v.admit.getText), "Admit as r3")
    assertEquals(runOnFx(v.admit.isDisabled), true)
    assertEquals(
      runOnFx(v.admitNote.getText),
      "Require complete refuses r3: 17 trials are quarantined. Review exclusions admits " +
        "937 trials."
    )
    assertEquals(runOnFx(v.countsSource.getText), "Counts: eyes4s admission of r3.")
    // Board-parity evidence: the Data perspective with the ledger.
    fx.snapshot(StudioTheme.Light)
  }

  fxStage.test("every count opens exactly its trials, and opens them again closed") { fx =>
    val w       = ready(fx)
    val entries = runOnFx(w.admission.state.entries.toOption.get)
    val counts  = runOnFx(w.admission.view.rows.map(r => (r.ref, r.count.getText)))
    counts.foreach { (ref, shown) =>
      val row = runOnFx(w.admission.view.rows.find(_.ref == ref).get)
      fire(fx, row.count)
      eventually(fx, s"$ref to open")(AdmissionLedger.opened(w.runtime.model, r3).contains(ref))
      val listed = runOnFx(w.admission.view.trials.getItems.asScala.toVector)
      assertEquals(listed.size.toString, shown, ref)
      assertEquals(listed.map(_.trial).distinct.size, listed.size, ref)
      val expected = ref match
        case StudioRef.InventoryCount(_, InventoryKind.Inventory) => entries
        case StudioRef.InventoryCount(_, InventoryKind.Admitted)  =>
          entries.filter(_.disposition == TrialDisposition.Admitted)
        case StudioRef.InventoryCount(_, InventoryKind.Absent) =>
          entries.filter(_.disposition == TrialDisposition.Absent)
        case StudioRef.InventoryCount(_, InventoryKind.NoFixations) =>
          entries.filter(_.disposition == TrialDisposition.NoFixations)
        case StudioRef.InventoryCount(_, InventoryKind.Quarantined) =>
          entries.filter(e =>
            e.disposition match
              case TrialDisposition.Quarantined(_) | TrialDisposition.NoFixations => true
              case _                                                              => false
          )
        case StudioRef.InventoryCount(_, InventoryKind.Cause(code)) =>
          entries.filter(e =>
            e.disposition match
              case TrialDisposition.Quarantined(c) => c.code == code
              case _                               => false
          )
        case _ => entries.filter(_.outsideFrame.nonEmpty)
      assertEquals(listed.map(_.trial), expected.map(_.trial), ref)
      assert(runOnFx(row.node.getStyleClass.contains("opened")), ref)
      // The trail names the open count.
      assertEquals(trail(w).take(2), Vector("Dataset r3 (draft)", "Admission"))
      fire(fx, row.count)
      eventually(fx, s"$ref to close")(AdmissionLedger.opened(w.runtime.model, r3).isEmpty)
      assertEquals(runOnFx(w.admission.view.trials.getItems.size), 0)
    }
    // A cause sits under the quarantined count.
    val overlap = runOnFx(w.admission.view.rows.find(_.label.getText == "overlap").get)
    fire(fx, overlap.count)
    assertEquals(trail(w), Vector("Dataset r3 (draft)", "Admission", "Quarantined", "overlap"))
    assertEquals(
      runOnFx(w.admission.view.openedTitle.getText),
      "Quarantined · overlap · 6 trials"
    )
    fire(fx, w.admission.view.close)
    assertEquals(trail(w), Vector("Dataset r3 (draft)", "Admission"))
  }

  fxStage.test("a trial of an open count opens in Explore on Enter") { fx =>
    val w      = ready(fx)
    val absent = runOnFx(
      w.admission.view.rows
        .find(_.ref == StudioRef.InventoryCount(r3, InventoryKind.Absent))
        .get
    )
    fire(fx, absent.count)
    val list  = w.admission.view.trials
    val first = runOnFx {
      list.getSelectionModel.select(0)
      list.getSelectionModel.getSelectedItem
    }
    assertEquals(first.detail, "in the inventory, no fixation records")
    runOnFx(
      list.fireEvent(
        KeyEvent(KeyEvent.KEY_PRESSED, "", "", KeyCode.ENTER, false, false, false, false)
      )
    )
    fx.awaitLayout()
    val m = runOnFx(w.runtime.model)
    assertEquals(m.perspective, Perspective.Explore)
    assertEquals(m.location.trail.lastOption, Some(Place.At(StudioRef.Trial(first.trial))))
  }

  fxStage.test("Admit as r3 under Review exclusions admits it; run 5 (r2) becomes stale") {
    fx =>
      val w = ready(fx)
      val v = w.admission.view
      assertEquals(runOnFx(w.runtime.model.freshness.standing(run5)), Some(RunStanding.Current))
      fire(fx, v.choices(LedgerDecision.ReviewExclusions))
      assertEquals(runOnFx(w.admission.state.decision), LedgerDecision.ReviewExclusions)
      assertEquals(runOnFx(v.admit.isDisabled), false)
      fire(fx, v.admit)
      // The window verifies r3 with the backend, then admits exactly that content.
      eventually(fx, "r3 to be admitted") {
        w.runtime.model.document.dataset(r3).exists(_.decision.isAdmitted)
      }
      val m = runOnFx(w.runtime.model)
      assertEquals(
        m.freshness.standing(run5),
        Some(RunStanding.Stale(NonEmptyVector.one(StaleReason.DatasetMoved(r2, r3))))
      )
      eventually(fx, "the admitted status")(v.status.isVisible)
      assertEquals(
        runOnFx(v.status.getText),
        "r3 is admitted. Run 5 (rev 3) used r2 and is now stale. A change to its mapping " +
          "or geometry creates a new dataset revision."
      )
      assertEquals(runOnFx((v.admit.isVisible, v.consequence.isVisible)), (false, false))
      assert(
        runOnFx(LedgerDecision.values.forall(d => v.choices(d).isDisabled)),
        "the decision of an admitted revision is fixed"
      )
      assertEquals(trail(w).head, "Dataset r3")
      // The verification was performed, not left unwired.
      assertEquals(
        runOnFx(w.effects.problems.collect {
          case p @ EffectProblem.NotWired(_: AppEffect.RequestAdmission, _) => p
        }),
        Vector.empty
      )
      assertEquals(runOnFx(w.admission.state.counts).isInstanceOf[Loading.Ready[?]], true)
  }
