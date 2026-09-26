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

package eyes4s.studio.core.fixture

import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, RunId}
import eyes4s.studio.core.document.*

/** The freshness-relevant facts of FIXTURE.md's story moments, read from the
  * documents StoryMoments builds (S2.7 derives the badges from these).
  */
class StoryMomentsSuite extends munit.FunSuite:
  import StoryMoments.*

  private def doc(e: Either[String, StudioDocument]): StudioDocument =
    e.fold(m => fail(m), identity)

  private def scales(d: StudioDocument, rev: AnalysisRevision): Option[Vector[Double]] =
    d.analysis(rev).map(_.recipe.scales.values.map(_.degrees))

  test("t1 Data · verify: r3 is a draft re-import; run 5 is on r2; nothing runs on r3") {
    val t1 = doc(StoryMoments.t1)
    val r3 = t1.dataset(DatasetRevision(3))
    assertEquals(r3.map(_.decision), Some(AdmissionDecision.Pending))
    assertEquals(r3.flatMap(_.parent), Some(DatasetRevision(2)))
    assertEquals(r3.map(_.units.time), Some(Some(TimeUnit.Milliseconds)))
    assertEquals(t1.dataset(DatasetRevision(2)).map(_.units.time), Some(None))
    assertEquals(
      t1.dataset(DatasetRevision(2)).map(_.mapping.column(ColumnRole.Occurrence)),
      Some(None)
    )
    assert(r3.exists(_.mapping.column(ColumnRole.Occurrence).isDefined))
    assertEquals(t1.latestAdmitted.map(_.id), Some(DatasetRevision(2)))
    assertEquals(t1.runs.map(r => (r.id, r.analysis, r.dataset)), Vector((run5, rev3, r2)))
    assertEquals(t1.runsOn(DatasetRevision(3)), Vector.empty)
    assertEquals(t1.draft, None)
    assertEquals(t1.running, Vector.empty)
  }

  test("t2: r3 admitted; rev 4 · run 7 on r3; draft rev 5 adds σ 8° and is ready; no jobs") {
    val t2 = doc(StoryMoments.t2)
    assertEquals(t2.latestAdmitted.map(_.id), Some(r3))
    assertEquals(t2.latestAnalysis.map(a => (a.id, a.dataset)), Some((rev4, r3)))
    assertEquals(
      t2.runs.map(r => (r.id.number, r.analysis.number, r.dataset.number, r.state)),
      Vector(
        (5, 3, 2, RunLifecycle.Completed),
        (6, 4, 3, RunLifecycle.Cancelled(Some(eyes4s.studio.core.backend.StageKind.Comparing))),
        (7, 4, 3, RunLifecycle.Completed)
      )
    )
    val draft = t2.draft
    assertEquals(draft.map(d => (d.id, d.base, d.changeCount)), Some((rev5, rev4, 1)))
    assertEquals(draft.map(_.render), Some("scales +σ 8°"))
    assertEquals(t2.draftRecipe.map(_.scales.values.map(_.degrees)), Some(scalesRev5))
    assertEquals(scales(t2, rev4), Some(scalesRev4))
    assertEquals(t2.running, Vector.empty)
    assertEquals(t2.presentation.shownRun, Some(run7))
    // The matched-cardinality policy is the persisted recipe field.
    assertEquals(t2.analysis(rev4).map(_.recipe.unmatched), Some(UnmatchedChoice.ReportNoMatch))
  }

  test("t2 figures: Figure 1 = run 7 + by retrieval response; Figure 2 stays on run 5") {
    val t2       = doc(StoryMoments.t2)
    val bindings = t2.figures.map(f => (f.id.label, f.run, f.reporting.value))
    assertEquals(
      bindings,
      Vector(
        ("Figure 1", run7, "by-retrieval-response"),
        ("Figure 2", run5, "by-retrieval-response")
      )
    )
    assertEquals(t2.figures(0).panels.map(_.letter.value), Vector("A", "B", "C", "D", "E"))
    assertEquals(t2.reporting.map(_.name), Vector("By retrieval response"))
    assertEquals(t2.reporting.map(_.minimumPerGroup), Vector(None))
    assertEquals(t2.reporting.flatMap(_.groupBy).map(_.label), Vector("response"))
    // Figure 2's run is on r2, which r3 superseded: the stale case of S2.6/S2.7.
    assertEquals(t2.run(run5).map(_.dataset), Some(r2))
  }

  test("t3: rev 5 saved; run 8 runs rev 5 on r3; the view stays on run 7") {
    val t3 = doc(StoryMoments.t3)
    assertEquals(t3.draft, None)
    assertEquals(t3.latestAnalysis.map(a => (a.id, a.dataset)), Some((rev5, r3)))
    assertEquals(scales(t3, rev5), Some(scalesRev5))
    assertEquals(
      t3.running.map(r => (r.id, r.analysis, r.dataset, r.state)),
      Vector((run8, rev5, r3, RunLifecycle.Running(run8Job)))
    )
    assertEquals(t3.presentation.shownRun, Some(RunId(7)))
  }

  test("sources carry the golden fixture's real SHA-256 and no invented identity") {
    val t2      = doc(StoryMoments.t2)
    val sources = t2.datasets.map(_.sources)
    assertEquals(sources.distinct.size, 1)
    assertEquals(
      sources.head.entries.map(_.path.value),
      Vector("inputs/fixations.csv", "inputs/trials.csv")
    )
    assertEquals(sources.head.fixations.map(_.bytes.hex), Some(GoldenInventory.fixationsSha256))
    assertEquals(sources.head.trials.map(_.bytes.hex), Some(GoldenInventory.trialsSha256))
    assert(sources.head.entries.forall(_.semantic.isEmpty))
    assert(t2.analyses.forall(_.plan == CoreBinding.unbound[StudyPlanArtifact]))
  }
