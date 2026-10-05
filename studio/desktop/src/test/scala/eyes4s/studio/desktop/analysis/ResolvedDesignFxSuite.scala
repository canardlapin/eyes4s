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

package eyes4s.studio.desktop.analysis

import eyes4s.studio.app.analysis.*
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.{Intent, PreparedDesign, StoryModels}
import eyes4s.studio.core.backend.{Eligibility, TrialKey}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.execution.ExecutionEffect
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.{FxStage, StudioTheme}
import eyes4s.studio.desktop.shell.ShellFxSuite
import javafx.scene.input.KeyCode

/** The resolved-design table on the desktop (ticket S7.5; Analysis.dc.html,
  * resolved design) at t2's draft rev 5, over the fake backend's preview of
  * FIXTURE.md: the chips' counts (480 requested = 457 eligible + 9 no match
  * + 14 not admitted; by design n/a), the exact counts after paging, the
  * stamp, filtering, the row cursor, every status row opening its trial,
  * and a Save & run that submits the previewed design (E2E-05).
  */
class ResolvedDesignFxSuite extends ShellFxSuite:

  private def ready(fx: FxStage, w: StudioWindow): Unit =
    eventually(fx, "the counted preview and every row") {
      val s = w.resolvedDesign.state
      s.preview.receipt.isDefined && s.rowState == DesignRows.Complete
    }

  private def chip(w: StudioWindow, f: DesignFilter) =
    w.resolvedDesign.view.chips.find(_.filter == f).getOrElse(fail(s"no chip $f"))

  private def shownQueries(w: StudioWindow): Vector[TrialKey] =
    runOnFx(w.resolvedDesign.view.rows.flatMap(w.resolvedDesign.view.queryOf))

  private def backToAnalysis(fx: FxStage, w: StudioWindow): Unit =
    dispatch(fx, w, Intent.Back)
    assertEquals(runOnFx(w.runtime.model.perspective), Perspective.Analysis)

  fxStage.test("the chips, counts and stamp are the backend's preview of rev 5") { fx =>
    val w = boot(fx, StoryModels.t2Analysis, StoryMoment.T2)
    ready(fx, w)
    val view = w.resolvedDesign.view
    assertEquals(
      runOnFx(view.chips.map(c => (c.button.getText, c.count.getText, c.button.isDisabled))),
      Vector(
        ("All requested", "480", false),
        ("Eligible", "457", false),
        ("No match", "9", false),
        ("Query not admitted", "14", false),
        ("No study trial, by design", "—", true)
      )
    )
    assertEquals(runOnFx(view.mode.getText), "paged · exact")
    assert(!runOnFx(view.counting.isVisible))
    assertEquals(
      runOnFx(view.noteLabels.map(_.getText)),
      Vector(
        "Candidate pairs before paging 219,486 per scale (466 queries × 471 references, " +
          "Cartesian · eyes4s candidatePairCount).",
        "Exact eligible after paging 8,969 per scale.",
        "Preview and run use this same prepared design.",
        "input digest unbound · plan unbound · rev 5 · data r3"
      )
    )
    // Every requested query is a row, and each chip's count is its rows.
    assertEquals(shownQueries(w).size, 480)
    val counts = Vector(
      DesignFilter.Eligible    -> 457,
      DesignFilter.NoMatch     -> 9,
      DesignFilter.NotAdmitted -> 14,
      DesignFilter.All         -> 480
    )
    counts.foreach { (f, n) =>
      fx.robot.click(runOnFx(chip(w, f).button))
      fx.awaitLayout()
      assert(runOnFx(chip(w, f).button.isSelected), f)
      assertEquals(shownQueries(w).size, n, f)
    }
    val read = runOnFx(w.resolvedDesign.view.rows.map(_.getAccessibleText))
    assert(read.contains("P17 · ret_07 · beach-042 · Eligible"), read.take(3))
    assert(
      read.contains(
        "P17 · ret_09 · market-066 · Query not admitted · absent from fixations.csv"
      )
    )
    // The receipt the pane counted is the app's prepared design.
    val receipt = runOnFx(w.resolvedDesign.state.preview.receipt).getOrElse(fail("no receipt"))
    assertEquals(
      runOnFx(w.runtime.model.prepared).map(_.ready),
      Some(receipt)
    )
    assertEquals(
      receipt.stamp,
      eyes4s.studio.app.AppModel.stampOf(
        runOnFx(w.runtime.model.document),
        StoryMoments.rev5,
        StoryMoments.r3
      )
    )
    fx.snapshot(StudioTheme.Light)
  }

  fxStage.test("every status row opens its trial: a click and the keyboard row cursor") { fx =>
    val w = boot(fx, StoryModels.t2Analysis, StoryMoment.T2)
    ready(fx, w)
    val rows = runOnFx(w.resolvedDesign.state.rows)
    // Click each no-match row (all nine fit in the pane).
    fx.robot.click(runOnFx(chip(w, DesignFilter.NoMatch).button))
    fx.awaitLayout()
    val unmatched = rows.filter(_.eligibility match
      case Eligibility.NoMatch(_) => true
      case _                      => false)
    assertEquals(shownQueries(w), unmatched.map(_.query))
    unmatched.map(_.query).foreach { q =>
      // Bring the row into the viewport, as the row cursor does, then click it.
      runOnFx(w.resolvedDesign.dispatch(DesignIntent.FocusRow(q)))
      fx.awaitLayout()
      val node = runOnFx(
        w.resolvedDesign.view.rows
          .find(n => w.resolvedDesign.view.queryOf(n).contains(q))
          .getOrElse(fail(s"no row $q"))
      )
      fx.robot.click(node)
      fx.awaitLayout()
      assertEquals(runOnFx(w.runtime.model.perspective), Perspective.Explore, q)
      assertEquals(
        runOnFx(w.runtime.model.location.trail.lastOption),
        Some(Place.At(StudioRef.Trial(q))),
        q
      )
      backToAnalysis(fx, w)
    }
    // Home scrolls the table back to its top. (The robot's picking ignores
    // clips, so a row scrolled above the viewport would cover the chips.)
    runOnFx(w.resolvedDesign.view.table.requestFocus())
    fx.robot.press(KeyCode.HOME)
    // Walk the not-admitted rows with the row cursor and open each with Enter.
    fx.robot.click(runOnFx(chip(w, DesignFilter.NotAdmitted).button))
    fx.awaitLayout()
    val notAdmitted = rows.filter(_.eligibility match
      case Eligibility.QueryNotAdmitted(_) => true
      case _                               => false)
    assertEquals(shownQueries(w), notAdmitted.map(_.query))
    notAdmitted.zipWithIndex.foreach { (r, i) =>
      runOnFx(w.resolvedDesign.view.table.requestFocus())
      fx.awaitLayout()
      fx.robot.press(if i == 0 then KeyCode.HOME else KeyCode.DOWN)
      assertEquals(runOnFx(w.resolvedDesign.state.cursor), Some(r.query))
      fx.robot.press(KeyCode.ENTER)
      fx.awaitLayout()
      assertEquals(
        runOnFx(w.runtime.model.location.trail.lastOption),
        Some(Place.At(StudioRef.Trial(r.query))),
        r.query
      )
      backToAnalysis(fx, w)
    }
    // The cursor survives a round trip; End and Page Up move within the rows.
    runOnFx(w.resolvedDesign.view.table.requestFocus())
    fx.robot.press(KeyCode.END)
    assertEquals(runOnFx(w.resolvedDesign.state.cursor), notAdmitted.lastOption.map(_.query))
    fx.robot.press(KeyCode.PAGE_UP)
    assertEquals(
      runOnFx(w.resolvedDesign.state.cursor),
      Some(notAdmitted(notAdmitted.size - 1 - ResolvedDesign.CursorPage).query)
    )
  }

  fxStage.test("E2E-05: Save & run submits the design the preview prepared") { fx =>
    val w = boot(fx, StoryModels.t2Analysis, StoryMoment.T2)
    ready(fx, w)
    val prepared: PreparedDesign =
      runOnFx(w.runtime.model.prepared).getOrElse(fail("no prepared design"))
    dispatch(fx, w, Intent.Dispatch(Command.SaveAndRun(None)))
    eventually(fx, "run 8's job") {
      w.runtime.model.jobs.jobs.exists(_.stamp == prepared.ready.stamp)
    }
    // No execution effect was refused or failed (the journal is not wired
    // without a project).
    assertEquals(
      runOnFx(w.effects.problems).collect {
        case p: eyes4s.studio.desktop.runtime.EffectProblem.Refused => p
        case p: eyes4s.studio.desktop.runtime.EffectProblem.Failed  => p
      },
      Vector.empty
    )
    // The run's stamp is the receipt's: the app submitted the receipt, and
    // the backend ran the snapshot it retained for it (PreviewSnapshotSuite).
    assertEquals(runOnFx(w.runtime.model.jobs.shelf.required), Some(prepared.ready.stamp))
    assert(
      prepared.prepares(runOnFx(w.runtime.model.document), prepared.ready.stamp),
      "the saved revision is the previewed recipe"
    )
    assertEquals(
      ExecutionEffect.submitted(ExecutionEffect.SubmitPreview(prepared.ready)),
      Some(prepared.ready.stamp)
    )
  }
