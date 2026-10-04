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

import eyes4s.kernel.Unit2D
import eyes4s.plan.{Diagnostic, MatchedReferences, StudyFinding}
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.execution.RunStamp
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.preview.*
import eyes4s.studio.core.selection.{DesignCount, StudioRef}

/** The preflight pane and run card headlessly (ticket S7.6) at t2's draft
  * rev 5: the run card's pair rows are the backend's (8,969 × 5 scales =
  * 44,845), Save & run is enabled with no blocker and disabled with its
  * reason whenever a blocker exists, while checking, after a refusal, and
  * for a saved revision; eyes4s findings and studio checks stay apart.
  */
class PreflightSuite extends munit.FunSuite:
  import StoryMoments.{r3, rev5}

  private def ok[E, A](e: Either[E, A]): A = e.fold(err => fail(s"$err"), identity)

  private val model: AppModel = StoryModels.t2Analysis
  private val stamp: RunStamp = AppModel.stampOf(model.document, rev5, r3)
  private val id              = PreviewId(1L)
  private val candidates      = ok(PreviewCandidates.of(466, 471, 24, 219486L, 480, 14, None))
  private val counts          = ok(PreviewCounts.of(8969L, 44845L, 457, 9, 0))

  private val warning = StudioDiagnostic(
    "study-finding.unmatched-focal",
    DiagnosticLevel.Warning,
    DiagnosticOrigin.EyesCore,
    Vector.empty,
    "9 focal trials",
    Vector.fill(1)(TrialKey("P03", Phase.Retrieval, "ret_11", 1)),
    Some("DataDependent"),
    Some("SupplyMatchedReference")
  )

  private val blocker: StudioDiagnostic =
    val p11                                  = TrialKey("P11", Phase.Retrieval, "ret_05", 1)
    val f: StudyFinding[TrialKey, Unit2D.Px] = StudyFinding.MatchedCardinality(
      p11,
      Vector(
        TrialKey("P11", Phase.Encoding, "enc_04", 1),
        TrialKey("P11", Phase.Encoding, "enc_04", 2)
      ),
      MatchedReferences.RequireOne
    )
    StudioDiagnostic.of(Diagnostic.of(f), identity)

  private def ready(diagnostics: StudioDiagnostic*) =
    ok(PreviewReady.of(id, stamp, candidates, counts, diagnostics.toVector))

  private def checked(diagnostics: StudioDiagnostic*): ResolvedDesign =
    val (synced, _) = ResolvedDesign.sync(ResolvedDesign.empty, model)
    val g           = synced.generation
    Vector(
      DesignIntent.Previewed(g, PreviewEvent.Initial(id, stamp, candidates)),
      DesignIntent.Previewed(g, PreviewEvent.Ready(ready(diagnostics*)))
    ).foldLeft(synced)((p, i) => ResolvedDesign.update(p, i)._1)

  test("the fixture draft rev 5: 8,969 × 5 scales = 44,845 pairs, Save & run enabled") {
    val vm   = Preflight.vm(checked(warning), model)
    val card = vm.card
    assertEquals(card.enabled, true)
    assertEquals(card.reason, None)
    assertEquals(card.button, "Save & run rev 5 · 44,845 pairs")
    assertEquals(card.run, Some(Intent.Dispatch(Command.SaveAndRun(None))))
    assertEquals(
      card.lines.head,
      RunLine(
        "Pair rows",
        "8,969 × 5 scales = 44,845",
        Vector(
          StudioRef.DesignTally(rev5, DesignCount.EligiblePairsPerScale),
          StudioRef.DesignTally(rev5, DesignCount.Scales),
          StudioRef.DesignTally(rev5, DesignCount.EligiblePairs)
        )
      )
    )
    assertEquals(card.lines(1).label, "Change vs rev 4")
    assert(card.lines(1).value.contains("8°"), card.lines(1).value)
    assertEquals(card.verdict, "Ready · 0 blockers · 1 warning reported with the run")
    assertEquals(vm.report, "StudyReport · rev 5")
    assertEquals(
      vm.findings.map(_.eyes4s.map(_.title)),
      Some(Vector("Focal trials without match"))
    )
  }

  test("findings of one kind are one entry naming all their trials, as on the board") {
    val other = warning.copy(affected = Vector(TrialKey("P17", Phase.Retrieval, "ret_02", 1)))
    val vm    = Preflight.vm(checked(warning, other, blocker), model)
    val f     = vm.findings.get
    assertEquals(
      f.eyes4s.map(_.code),
      Vector("study-finding.unmatched-focal", "study-finding.matched-cardinality")
    )
    val focal = f.eyes4s.head
    assertEquals(focal.affected.map(_.trial), Vector("ret_11", "ret_02"))
    assertEquals(focal.remedy.map(_.trials), Some(focal.affected))
    assertEquals(focal.detail, "2 trials (P03, P17)")
    assertEquals((f.blockers, f.warnings), (1, 1))
    // The blocker keeps its own detail.
    assert(f.eyes4s(1).detail.startsWith("2 matched references for P11"), f.eyes4s(1).detail)
  }

  test("the pane's view id is valid") {
    assert(Preflight.viewId.isRight, Preflight.viewId)
  }

  test("any blocker disables Save & run, with its reason") {
    val vm = Preflight.vm(checked(warning, blocker), model)
    assertEquals(vm.card.enabled, false)
    assertEquals(vm.card.run, None)
    assertEquals(vm.card.reason, Some("Save & run disabled: 1 blocker"))
    assertEquals(vm.card.verdict, "Blocked · 1 blocker · 1 warning reported with the run")
    // Running the draft anyway is not offered.
    val f = vm.findings.get
    assertEquals(
      f.eyes4s.map(_.code),
      Vector("study-finding.unmatched-focal", "study-finding.matched-cardinality")
    )
    assertEquals(f.studio, Vector.empty)
  }

  test("disabled while checking, after a refusal, and for a saved revision, each saying why") {
    val (synced, _) = ResolvedDesign.sync(ResolvedDesign.empty, model)
    assertEquals(Preflight.vm(synced, model).card.reason, Some("Checking rev 5…"))
    val counting = ResolvedDesign
      .update(
        synced,
        DesignIntent.Previewed(synced.generation, PreviewEvent.Initial(id, stamp, candidates))
      )
      ._1
    assert(Preflight.vm(counting, model).card.reason.exists(_.startsWith("Checking rev 5")))
    val refused = ResolvedDesign
      .update(synced, DesignIntent.PreviewRefused(synced.generation, "no backend"))
      ._1
    assertEquals(
      Preflight.vm(refused, model).card.reason,
      Some("The check of rev 5 could not be made: no backend")
    )
    // A saved revision is not the draft: Save & run does not apply to it.
    val onRev4 = AppModel
      .run(
        model,
        Vector(
          Intent.Navigate(
            eyes4s.studio.app.nav.Location(
              eyes4s.studio.core.document.Perspective.Analysis,
              Vector(eyes4s.studio.app.nav.Place.Revision(StoryMoments.rev4))
            )
          )
        )
      )
      ._1
    val saved = Preflight.vm(ResolvedDesign.sync(ResolvedDesign.empty, onRev4)._1, onRev4)
    assertEquals(saved.card.enabled, false)
    assertEquals(saved.card.reason, Some("Only a draft can be saved and run; rev 4 is saved"))
    assertEquals(
      Preflight.vm(ResolvedDesign.empty, model).card.reason,
      Some("No analysis revision to check")
    )
  }
