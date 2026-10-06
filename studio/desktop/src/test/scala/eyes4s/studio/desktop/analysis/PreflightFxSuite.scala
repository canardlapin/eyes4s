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

import eyes4s.kernel.Unit2D
import eyes4s.plan.{Diagnostic, MatchedReferences, StudyFinding}
import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.analysis.DesignIntent
import eyes4s.studio.core.backend.{Phase, StudioDiagnostic, TrialKey}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.preview.{PreviewEvent, PreviewId, PreviewReady}
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.{FxStage, StudioTheme}
import eyes4s.studio.desktop.shell.ShellFxSuite

import scala.concurrent.duration.Duration

/** The preflight pane and run card in the studio window (ticket S7.6;
  * Analysis.dc.html, preflight) at t2: on the fixture draft rev 5 Save & run
  * is enabled for 44,845 pairs, and submits the run; with a blocker it is
  * disabled with its reason, and the blocker's remedy opens exactly its
  * trials in Explore.
  */
class PreflightFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(180, "s")

  private def ready(fx: FxStage, w: StudioWindow): Unit =
    eventually(fx, "the counted preview") {
      w.resolvedDesign.state.preview.receipt.isDefined
    }

  fxStage.test("the fixture draft rev 5: Save & run enabled for 44,845 pairs, and it runs") {
    fx =>
      val w = boot(fx, StoryModels.t2Analysis, StoryMoment.T2)
      ready(fx, w)
      val (label, enabled, said) = runOnFx(w.preflight.runButton)
      assertEquals((label, enabled), ("Save & run rev 5 · 44,845 pairs", true))
      assert(said.startsWith("Ready · 0 blockers"), said)
      assert(runOnFx(w.preflight.vm.card.lines.exists(_.value == "8,969 × 5 scales = 44,845")))
      fx.snapshot(StudioTheme.Light)
      val stamp = runOnFx(w.resolvedDesign.state.preview.receipt.get.stamp)
      runOnFx(w.preflight.pressRun())
      eventually(fx, "the run's job") {
        w.runtime.model.jobs.jobs.exists(_.stamp == stamp)
      }
  }

  fxStage.test(
    "a blocker disables Save & run with its reason; its remedy opens exactly its trials"
  ) { fx =>
    val w = boot(fx, StoryModels.t2Analysis, StoryMoment.T2)
    ready(fx, w)
    val p11  = TrialKey("P11", Phase.Retrieval, "ret_05", 1)
    val refs = Vector(
      TrialKey("P11", Phase.Encoding, "enc_04", 1),
      TrialKey("P11", Phase.Encoding, "enc_04", 2)
    )
    val finding: StudyFinding[TrialKey, Unit2D.Px] =
      StudyFinding.MatchedCardinality(p11, refs, MatchedReferences.RequireOne)
    val blocker = StudioDiagnostic.of(Diagnostic.of(finding), identity)
    // A fresh backend preview cycle carries the System board's blocker.
    runOnFx {
      val s       = w.resolvedDesign.state
      val r       = s.preview.receipt.get
      val id      = PreviewId(r.id.value + 1)
      val blocked = PreviewReady
        .of(id, r.stamp, r.candidates, r.counts, r.diagnostics :+ blocker, r.recipe)
        .fold(e => fail(e.toString), identity)
      w.resolvedDesign.dispatch(
        DesignIntent.Previewed(s.generation, PreviewEvent.Initial(id, r.stamp, r.candidates))
      )
      w.resolvedDesign.dispatch(
        DesignIntent.Previewed(s.generation, PreviewEvent.Ready(blocked))
      )
    }
    fx.awaitLayout()
    val (_, enabled, said) = runOnFx(w.preflight.runButton)
    assertEquals(enabled, false)
    assertEquals(said, "Save & run disabled: 1 blocker")
    val shown = runOnFx(w.preflight.shown._2)
    val card  =
      shown.find(_.code == "study-finding.matched-cardinality").getOrElse(fail(s"$shown"))
    assertEquals(card.title, "Matched cardinality")
    fx.snapshot(StudioTheme.Light)
    runOnFx(w.preflight.remedy(card))
    fx.awaitLayout()
    val m = runOnFx(w.runtime.model)
    assertEquals(m.perspective, Perspective.Explore)
    assertEquals(m.selection.selected, (p11 +: refs).map(StudioRef.Trial(_)))
  }
