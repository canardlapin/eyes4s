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

package eyes4s.studio.app.diagnostics

import eyes4s.kernel.Unit2D
import eyes4s.plan.{Diagnostic, MatchedReferences, StudyFinding}
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.plot.ViewSelection
import eyes4s.studio.app.{AppModel, StoryModels}
import eyes4s.studio.core.backend.{
  DiagnosticLevel,
  DiagnosticLocus,
  DiagnosticOrigin,
  Phase,
  StudioDiagnostic,
  TrialKey
}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.selection.{StudioRef, ViewId}

/** The diagnostics presenter (ticket S3.5): eyes4s findings by stable code,
  * apart from Studio checks; words from the code, never the message; the
  * System board's blocked example (P11 ret_05, 2 matched references,
  * occurrences 1 and 2) blocks Save & run; and each remedy opens exactly the
  * affected trials.
  */
class DiagnosticsPresenterSuite extends munit.FunSuite:

  private val ret05  = TrialKey("P11", Phase.Retrieval, "ret_05", 1)
  private val enc04  = TrialKey("P11", Phase.Encoding, "enc_04", 1)
  private val enc04b = TrialKey("P11", Phase.Encoding, "enc_04", 2)

  /** eyes4s's own matched-cardinality finding, through the wire's conversion. */
  private val cardinality: StudioDiagnostic =
    val finding: StudyFinding[TrialKey, Unit2D.Px] =
      StudyFinding.MatchedCardinality(
        ret05,
        Vector(enc04, enc04b),
        MatchedReferences.RequireOne
      )
    StudioDiagnostic.of(Diagnostic.of(finding), identity)

  private def diagnostic(
      code: String,
      level: DiagnosticLevel,
      origin: DiagnosticOrigin,
      trials: Vector[TrialKey],
      message: String = "a message"
  ) =
    StudioDiagnostic(code, level, origin, trials.map(DiagnosticLocus.Trial(_)), message, trials)

  test(
    "the System board's blocked example: 2 matched references for P11 ret_05 block Save & run"
  ) {
    assertEquals(cardinality.code, "study-finding.matched-cardinality")
    assertEquals(cardinality.affected, Vector(ret05, enc04, enc04b))
    assertEquals(cardinality.remedy, Some("ChooseMatchedReference"))
    val vm = DiagnosticsPresenter.present(Vector(cardinality))
    assertEquals((vm.blockers, vm.warnings, vm.runnable), (1, 0, false))
    assertEquals(vm.verdict, "Save & run disabled")
    val f = vm.eyes4s.head
    assertEquals(f.severity, FindingSeverity.Blocker)
    assertEquals(f.title, "Matched cardinality")
    assertEquals(f.detail, "2 matched references for P11 · ret_05 (occurrences 1, 2)")
    assertEquals(f.remedy, Some(RemedyVM("Choose occurrence…", Vector(ret05, enc04, enc04b))))
    assertEquals(vm.studio, Vector.empty)
  }

  test("each remedy opens exactly the affected trials, in Explore") {
    val remedy     = DiagnosticsPresenter.present(Vector(cardinality)).eyes4s.head.remedy.get
    val view       = ViewId.of("analysis.preflight").toOption.get
    val m          = StoryModels.t2Analysis
    val (_, go)    = DiagnosticsPresenter.open(remedy, ViewSelection.initial(view, m.selection))
    val (after, _) = AppModel.run(m, go)
    assertEquals(after.perspective, Perspective.Explore)
    assertEquals(after.location.trail.last, Place.At(StudioRef.Trial(ret05)))
    assertEquals(after.selection.selected, Vector(ret05, enc04, enc04b).map(StudioRef.Trial(_)))
    assertEquals(after.notice, None)
  }

  test("eyes4s findings and Studio checks are kept apart, by origin and by code family") {
    val host = diagnostic(
      "studio-check.example",
      DiagnosticLevel.Warning,
      DiagnosticOrigin.Host,
      Vector(ret05)
    )
    val eyes = diagnostic(
      "study-finding.duplicate-trial",
      DiagnosticLevel.Warning,
      DiagnosticOrigin.EyesCore,
      Vector(ret05)
    )
    val vm = DiagnosticsPresenter.present(Vector(host, cardinality, eyes))
    assertEquals(
      vm.eyes4s.map(_.code),
      Vector("study-finding.matched-cardinality", "study-finding.duplicate-trial")
    )
    assertEquals(vm.studio.map(_.code), Vector("studio-check.example"))
    assertEquals((vm.blockers, vm.warnings), (1, 2))
    assert(DiagnosticsPresenter.isStudioCheck(host))
    assert(!DiagnosticsPresenter.isStudioCheck(cardinality))
  }

  test("words come from the code, never from the message; an unknown code says so") {
    val a = diagnostic(
      "study-finding.no-fixation-in-window",
      DiagnosticLevel.Warning,
      DiagnosticOrigin.EyesCore,
      Vector(ret05),
      "one wording"
    )
    val b        = a.copy(message = "another wording entirely")
    val (fa, fb) = (DiagnosticsPresenter.finding(a), DiagnosticsPresenter.finding(b))
    assertEquals((fa.title, fa.detail), (fb.title, fb.detail))
    assertEquals(fa.title, "Empty map in window")
    assertEquals(fa.detail, "1 trials (P11)")
    assert(!fa.title.contains("wording") && !fa.detail.contains("wording"))
    val unknown = DiagnosticsPresenter.finding(
      diagnostic(
        "study-finding.new-thing",
        DiagnosticLevel.Error,
        DiagnosticOrigin.EyesCore,
        Vector(ret05, enc04)
      )
    )
    assertEquals(
      (unknown.title, unknown.detail),
      ("study-finding.new-thing", "2 trials (P11) · no Studio text for this code")
    )
    assertEquals(
      unknown.remedy,
      Some(RemedyVM("Open the affected trials", Vector(ret05, enc04)))
    )
  }

  test("a diagnostic naming no trial has no remedy; with no blocker the run is ready") {
    val budget = StudioDiagnostic(
      "budget.schedule",
      DiagnosticLevel.Warning,
      DiagnosticOrigin.EyesCore,
      Vector.empty,
      "over"
    )
    val vm = DiagnosticsPresenter.present(Vector(budget))
    assertEquals(vm.eyes4s.head.remedy, None)
    assertEquals((vm.runnable, vm.verdict), (true, "Ready"))
    // A diagnostic from before protocol 1.8 names its trials by its subject.
    val old = StudioDiagnostic(
      "study-failure.off-window",
      DiagnosticLevel.Error,
      DiagnosticOrigin.EyesCore,
      Vector(DiagnosticLocus.Trial(ret05)),
      "empty map"
    )
    assertEquals(DiagnosticsPresenter.affected(old), Vector(ret05))
    assertEquals(
      DiagnosticsPresenter.finding(old).title,
      "Trial fails: fixations outside the window"
    )
  }
