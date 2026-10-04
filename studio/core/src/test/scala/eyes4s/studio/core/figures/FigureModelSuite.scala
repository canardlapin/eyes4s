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

package eyes4s.studio.core.figures

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.codec.ArtifactName
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.bundle.{InMemoryProjectStore, LockOwner}
import eyes4s.studio.core.command.{Command, History}
import eyes4s.studio.core.diff.{StatusChanges, StatusDiff}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.document.DocumentSamples.{t2, t3}
import eyes4s.studio.core.fixture.StoryMoments.{r2, r3, run5, run7, run8}
import eyes4s.studio.core.freshness.{Freshness, SessionFacts, StaleReason}
import eyes4s.studio.core.runs.{RunArchive, RunStore}
import munit.CatsEffectSuite

import java.nio.charset.StandardCharsets.UTF_8

/** The figure model (ticket S9.1): one run and one reporting spec per figure,
  * stale detection, and rebinding that shows its plan and data diff first.
  */
class FigureModelSuite extends CatsEffectSuite:

  private def right[E, A](e: Either[E, A]): A =
    e.fold(err => throw new AssertionError(err.toString), identity)

  private val figure1 = right(FigureId.of(1))
  private val figure2 = right(FigureId.of(2))

  private def freshness(d: StudioDocument): Freshness = Freshness.of(d, SessionFacts.empty)

  /** t3 after run 8 (rev 5, σ 8° added) completed, and the view moved to it:
    * run 8 is the current run and the one shown.
    */
  private val run8Shown: StudioDocument =
    val completed = right(
      StudioDocument.of(
        t3.datasets,
        t3.analyses,
        t3.draft,
        t3.runs.map(r => if r.id == run8 then r.copy(state = RunLifecycle.Completed) else r),
        t3.reporting,
        t3.figures,
        t3.presentation,
        Vector.empty
      )
    )
    right(History.start(completed).apply(Command.ShowRun(Some(run8)))).history.document

  // --- A figure renders from its bound run, never the current one ----------

  test("a figure always renders from its bound run, never the current one") {
    assertEquals(run8Shown.presentation.shownRun, Some(run8))
    assertEquals(
      freshness(run8Shown).standing(run8),
      Some(eyes4s.studio.core.freshness.RunStanding.Current)
    )
    val one = right(FigureSource.of(run8Shown, figure1))
    val two = right(FigureSource.of(run8Shown, figure2))
    assertEquals((one.run.id, two.run.id), (run7, run5))
    assertEquals(FigureText.binding(two.bound), "run 5 · rev 3 · data r2")
    assertEquals(two.reporting.name, "By retrieval response")
    // The same in t2 (run 7 shown) and t3 (run 8 running): the binding alone decides.
    assertEquals(
      Vector(t2, t3).map(d => right(FigureSource.of(d, figure2)).run.id),
      Vector(run5, run5)
    )
  }

  test("the archive a figure renders is its bound run's, with runs 6-8 stored") {
    val name               = right(ArtifactName.of("study-result.json"))
    def archive(r: RunRef) =
      right(
        RunArchive.of(r, Vector(name -> IArray.unsafeFromArray(r.id.label.getBytes(UTF_8))))
      )
    for
      store <- InMemoryProjectStore.create[IO]
      lock  <- store.acquire(right(LockOwner.of("FigureModelSuite"))).map(right)
      runs = RunStore(store)
      _ <- run8Shown.runs
        .filter(_.state == RunLifecycle.Completed)
        .traverse_(r => runs.put(lock, archive(r)).map(right))
      rendered <- runs.figure(run8Shown, figure2).map(right)
    yield
      assertEquals(rendered.run, right(FigureSource.of(run8Shown, figure2)).run.id)
      assertEquals(rendered.bytes(name).map(b => String(Array.from(b), UTF_8)), Some("run 5"))
  }

  test("an unknown figure is refused, naming the known ones") {
    assertEquals(
      FigureSource.of(t2, right(FigureId.of(9))),
      Left(FigureError.UnknownFigure(right(FigureId.of(9)), Vector(figure1, figure2)))
    )
  }

  // --- Stale detection --------------------------------------------------------

  test("Figure 2 is stale on run 5 · rev 3 · data r2 and says why") {
    val stale = freshness(t2).figure(figure2).get
    assert(stale.isStale)
    val reasons = stale.standing match
      case eyes4s.studio.core.freshness.RunStanding.Stale(rs) => rs
      case other                                              => fail(other.toString)
    assertEquals(
      reasons.toVector.toSet,
      Set(StaleReason.Superseded(run7, AnalysisRevision(4)), StaleReason.DatasetMoved(r2, r3))
    )
    val bound = right(FigureSource.of(t2, figure2)).bound
    val diff  = right(
      eyes4s.studio.core.diff.DatasetDiff.fromParent(t2, r3, StatusDiff.NotRead)
    )
    assertEquals(
      FigureText.stale(bound, reasons, Some(diff)),
      "Bound to run 5 · rev 3 · data r2, which is no longer current: dataset r3 changed the " +
        "units and the mapping; rev 4 (run 7) supersedes it. The figure still shows run 5 " +
        "exactly as it was."
    )
    // With both ledgers compared, the board's cause.
    val key                        = TrialKey("P01", Phase.Retrieval, "ret_01", 1)
    def entry(d: TrialDisposition) = LedgerEntry(key, "item", None, d, Vector.empty)
    val changes                    = right(
      StatusChanges.between(
        r2,
        Vector(entry(TrialDisposition.Admitted)),
        r3,
        Vector(entry(TrialDisposition.NoFixations))
      )
    )
    assert(
      FigureText
        .stale(bound, reasons, Some(right(diff.withStatus(StatusDiff.Compared(changes)))))
        .startsWith(
          "Bound to run 5 · rev 3 · data r2, which is no longer current: dataset r3 changed " +
            "the admission status of 1 trial;"
        )
    )
    assertEquals(FigureText.keep(bound), "Keep as rev 3")
    assert(!freshness(t2).figure(figure1).get.isStale)
  }

  // --- Rebind shows the plan/data diff before applying ----------------------

  test("rebind shows the plan and data diff, and its command is the only change") {
    val to = RebindProposal.target(freshness(run8Shown))
    assertEquals(to.map(_.id), Some(run8))
    val proposal = right(RebindProposal.of(run8Shown, figure2, run8, StatusDiff.NotRead))
    assertEquals(FigureText.rebindTitle(proposal), "Rebind Figure 2 to run 8?")
    assertEquals(FigureText.plan(proposal), "Plan: rev 3 → rev 5, adds σ 8°")
    assertEquals(
      FigureText.data(proposal),
      "Data: r2 → r3, onset declared ms; occurrence → occurrence"
    )
    assertEquals(proposal.conflicts, Vector.empty)
    val bind = proposal.command.get
    assertEquals(
      bind,
      Command.BindFigure(
        figure2,
        run8,
        right(FigureSource.of(run8Shown, figure2)).figure.reporting
      )
    )
    // Building the proposal changed nothing; applying its command rebinds.
    assertEquals(right(FigureSource.of(run8Shown, figure2)).run.id, run5)
    val rebound = right(History.start(run8Shown).apply(bind)).history
    assertEquals(right(FigureSource.of(rebound.document, figure2)).run.id, run8)
    assert(!freshness(rebound.document).figure(figure2).get.isStale)
    // Rebinding is an ordinary edit: undo puts run 5 back.
    assertEquals(
      right(FigureSource.of(right(rebound.undo).history.document, figure2)).run.id,
      run5
    )
  }

  test("a rebind on the same data and plan says so") {
    val proposal = right(RebindProposal.of(t2, figure1, run5, StatusDiff.NotRead))
    // Figure 1 (run 7, rev 4, r3) to run 5 (rev 3, r2): same recipe, older data.
    assertEquals(FigureText.plan(proposal), "Plan: rev 4 → rev 3, no change to the analysis")
    val same = right(RebindProposal.of(run8Shown, figure1, run8, StatusDiff.NotRead))
    assertEquals(FigureText.data(same), "Data: r3, unchanged")
  }

  test("a panel whose scale the new run does not compute blocks the rebind") {
    val sigma8 = right(Sigma.of(8.0))
    val panel  = PanelSpec(
      right(PanelLetter.of("A")),
      "σ 8° map",
      PanelScale.At(sigma8),
      PanelSelection.AllQueries
    )
    val withF3 = right(
      History
        .start(run8Shown)
        .apply(Command.CreateFigure(run8, run8Shown.reporting.head.id, Vector(panel)))
    ).history.document
    val figure3  = right(FigureId.of(3))
    val proposal = right(RebindProposal.of(withF3, figure3, run7, StatusDiff.NotRead))
    assertEquals(
      FigureText.conflicts(proposal),
      Vector("Panel A shows σ 8°, which run 7 does not compute.")
    )
    assertEquals(proposal.command, None)
  }

  test("only another completed run can be proposed") {
    assertEquals(
      RebindProposal.of(t3, figure2, run8, StatusDiff.NotRead),
      Left(FigureError.NotCompleted(run8, RunLifecycle.Running))
    )
    assertEquals(
      RebindProposal.of(t2, figure2, run5, StatusDiff.NotRead),
      Left(FigureError.SameRun(figure2, run5))
    )
    // A status comparison of other revisions is refused.
    val proposal = right(RebindProposal.of(run8Shown, figure2, run8, StatusDiff.NotRead))
    val other    =
      StatusDiff.Compared(right(StatusChanges.between(r3, Vector.empty, r2, Vector.empty)))
    assert(proposal.withStatus(other).isLeft)
  }

  test("Rebind… offers the latest of several current completed runs") {
    // t2 with a second completed run of rev 4 on r3: runs 7 and 9 are current.
    val run9 = RunId(9)
    val two  = right(
      StudioDocument.of(
        t2.datasets,
        t2.analyses,
        t2.draft,
        t2.runs :+ RunRef(
          run9,
          AnalysisRevision(4),
          r3,
          RunLifecycle.Completed,
          CoreBinding.unbound
        ),
        t2.reporting,
        t2.figures,
        t2.presentation,
        t2.jobs
      )
    )
    val current = freshness(two).runs.collect {
      case f if f.standing == eyes4s.studio.core.freshness.RunStanding.Current => f.run.id
    }
    assertEquals(current, Vector(run7, run9))
    assertEquals(RebindProposal.target(freshness(two)).map(_.id), Some(run9))
  }

  test("the reducer refuses a rebind whose panel scale the run does not compute") {
    val sigma8 = right(Sigma.of(8.0))
    val panel  = PanelSpec(
      right(PanelLetter.of("A")),
      "σ 8° map",
      PanelScale.At(sigma8),
      PanelSelection.AllQueries
    )
    val withF3 = right(
      History
        .start(run8Shown)
        .apply(Command.CreateFigure(run8, run8Shown.reporting.head.id, Vector(panel)))
    ).history
    val refused =
      withF3.apply(Command.BindFigure(right(FigureId.of(3)), run7, run8Shown.reporting.head.id))
    assert(
      refused.left.exists(_.message.contains("σ 8°")),
      refused.left.map(_.message)
    )
  }
