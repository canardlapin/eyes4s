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

package eyes4s.studio.app.compare

import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.app.{AppEffect, AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.{PageRequest, PairDesign, ReportView, ResultSummary, Response}
import eyes4s.studio.core.command.{ChangeKind, Command}
import eyes4s.studio.core.document.{
  Covariate,
  MinimumPerGroup,
  ReportingContrast,
  ReportingFilter,
  ReportingId,
  ReportingSpec,
  ReportingWeight,
  Share,
  StudioDocument,
  ValueSet
}
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{DesignCount, QueryCount, StudioRef}

import scala.concurrent.{ExecutionContext, Future}

/** Compare's reporting editor headlessly (ticket S8.7; Results.dc.html,
  * reporting) on the fake backend at t2: it shows the board's spec against
  * run 7's served summary; every edit is one `PutReporting` ("Reporting · no
  * rerun"), which starts no job, leaves every run and analysis as it was and
  * asks the backend for no pair score again (AC 1); a hit-only filter leaves
  * the control pool as served (AC 2, E2E-09); the minimum, off by default,
  * names the cells it would drop (E2E-23); Save as… makes and shows a copy.
  */
class ReportingEditorSuite extends munit.FunSuite:
  import StoryModels.*

  private given ExecutionContext = ExecutionContext.global

  private val run7 = StoryMoments.run7

  private def right[E, A](e: Either[E, A]): A =
    e.fold(x => fail(s"unexpected Left: $x"), identity)

  private def withSession[A](f: HeadlessSession => Future[A]): Future[A] =
    HeadlessSession
      .open(StoryMoment.T2)
      .flatMap(s => f(s).transformWith(x => s.close.transform(_ => x)))

  private def summaryOf(s: HeadlessSession): Future[ResultSummary] =
    s.result(run7).map(right)

  private def vmOf(
      m: AppModel,
      sum: Option[ResultSummary],
      ed: ReportingEditor = ReportingEditor.empty,
      report: Option[ReportView] = None
  ) =
    ReportingEditor.vm(ed, m.document, Some(reporting), Some(run7), sum, Some(sigma2), report)

  private def spec(doc: StudioDocument, id: ReportingId = reporting): ReportingSpec =
    doc.reporting.find(_.id == id).getOrElse(fail(s"no spec ${id.value}"))

  private def reportOf(s: HeadlessSession, model: AppModel = t2Compare): Future[ReportView] =
    s.report(run7, spec(model.document), sigma2.value).map(right)

  /** `intent` as the host performs it: the editor's intents through the app. */
  private def perform(
      m: AppModel,
      intent: ReportingIntent,
      ed: ReportingEditor = ReportingEditor.empty
  ): (AppModel, ReportingEditor, Vector[Intent], Vector[AppEffect]) =
    val (next, intents)  = ReportingEditor.update(ed, m.document, Some(reporting), intent)
    val (after, effects) = intents.foldLeft((m, Vector.empty[AppEffect])) {
      case ((mm, es), i) =>
        val (n, e) = AppModel.update(mm, i)
        (n, es ++ e)
    }
    (after, next, intents, effects)

  // --- the board ---------------------------------------------------------------------------

  test("t2: the board's spec against run 7's summary") {
    withSession(s =>
      summaryOf(s).zip(reportOf(s)).map { (sum, report) =>
        val vm = vmOf(t2Compare, Some(sum), report = Some(report))
        assertEquals(vm.status, None)
        assertEquals(vm.title, "By retrieval response")
        assertEquals(vm.kind, "Reporting · no rerun")
        assertEquals(
          vm.reuses,
          ReportingLine(
            "Reuses run 7's 35,876 pair scores. Grouping, filtering and weighting never " +
              "change a pair score or a control set.",
            Vector(StudioRef.DesignTally(sum.revision, DesignCount.EligiblePairs))
          )
        )
        assertEquals(
          vm.groups.map(c => (c.label, c.chosen)),
          Vector("response" -> true, "None" -> false)
        )
        assertEquals(vm.groupValues, "Remembered · Forgotten")
        assertEquals(
          vm.contributing,
          ReportingLine(
            "All contributing retrieval queries (454)",
            Vector(StudioRef.QueryTally(run7, QueryCount.Contributing))
          )
        )
        assertEquals(
          (vm.outside.label, vm.outside.on, vm.outside.note.text),
          (
            "Exclude queries with >25% of fixation duration outside the analysis window",
            false,
            "count shown when applied"
          )
        )
        assertEquals(
          (vm.minimum.label, vm.minimum.on, vm.minimum.note.text),
          (
            "Minimum queries per group: 3",
            false,
            "off · the report will name excluded participant-group cells when applied"
          )
        )
        assertEquals(vm.minimum.note.refs, Vector.empty)
        assertEquals(
          vm.weights.map(c => (c.label, c.chosen)),
          Vector(
            "Equal weight per participant"   -> true,
            "Weight by contributing queries" -> false
          )
        )
        assertEquals(
          vm.estimand,
          "D is the matched-minus-control spatial similarity of duration-weighted fixation " +
            "density. It reflects spatial correspondence, not sequential replay. D does not " +
            "separate participant-specific reinstatement from item-driven salience common to " +
            "all viewers of that image, and may retain residual centre bias."
        )
        val current = vm.saved.find(_.current).getOrElse(fail("no current spec"))
        assertEquals(current.name, "By retrieval response")
        assert(
          current.detail.startsWith("run 7 · current view · used by Figure 1"),
          current.detail
        )
      }
    )
  }

  // --- AC 1: changing reporting reuses run pair scores, no job started ---------------------------

  private val edits: Vector[ReportingIntent] = Vector(
    ReportingIntent.GroupBy(None),
    ReportingIntent.OutsideFilter(true),
    ReportingIntent.Minimum(true),
    ReportingIntent.Weight(ReportingWeight.PooledQueries),
    ReportingIntent.Keep(right(Covariate.of("response")), Vector("Remembered"))
  )

  /** The board's spec with `edit` applied and nothing else. */
  private def expected(edit: ReportingIntent): ReportingSpec =
    val b        = spec(t2Compare.document)
    val response = right(Covariate.of("response"))
    def with_(
        groupBy: Option[Covariate] = b.groupBy,
        filters: Vector[ReportingFilter] = b.filters,
        minimum: Option[MinimumPerGroup] = b.minimumPerGroup,
        weighting: ReportingWeight = b.weighting
    ) = right(ReportingSpec.of(b.id, b.name, groupBy, filters, minimum, weighting))
    edit match
      case ReportingIntent.GroupBy(None)       => with_(groupBy = None)
      case ReportingIntent.OutsideFilter(true) =>
        with_(filters = b.filters :+ ReportingFilter.OutsideWindowAtMost(right(Share.of(0.25))))
      case ReportingIntent.Minimum(true) => with_(minimum = Some(right(MinimumPerGroup.of(3))))
      case ReportingIntent.Weight(w)     => with_(weighting = w)
      case ReportingIntent.Keep(_, _)    =>
        with_(filters =
          b.filters :+ ReportingFilter.Keep(
            response,
            right(ValueSet.of(response, Vector("Remembered")))
          )
        )
      case other => fail(s"no expected spec for $other")

  test("every edit is one PutReporting: no job, no run or analysis change, no score re-read") {
    edits.foreach { edit =>
      val m                            = t2Compare
      val (after, _, intents, effects) = perform(m, edit)
      intents match
        case Vector(Intent.Dispatch(c @ Command.PutReporting(next))) =>
          assertEquals(c.kind, ChangeKind.ReportingNoRerun, edit.toString)
          // Exactly the edited field changes; every other field stands.
          assertEquals(next, expected(edit), edit.toString)
        case other => fail(s"$edit gave $other")
      assertEquals(
        effects.collect { case e @ AppEffect.Execution(_) => e },
        Vector.empty,
        edit.toString
      )
      val (b, a) = (m.document, after.document)
      assertNotEquals(spec(a), spec(b), edit.toString)
      assertEquals(a.runs, b.runs, edit.toString)
      assertEquals(a.analyses, b.analyses, edit.toString)
      assertEquals(a.datasets, b.datasets, edit.toString)
      assertEquals(a.figures, b.figures, edit.toString)
      assertEquals(after.jobs, m.jobs, edit.toString)
      assertEquals(after.freshness.standing(run7), m.freshness.standing(run7), edit.toString)
      // The summary layout already holds run 7: it asks the backend for nothing.
      val held = CompareSummary(Some(run7), Some(reporting), None, None, None)
      assertEquals(CompareSummary.sync(held, after)._2, Vector.empty, edit.toString)
    }
  }

  test(
    "contrast operands are explicit, validated and retained by unrelated edits and Save as"
  ) {
    val forward = right(ReportingContrast.of("Remembered", "Forgotten"))
    val legacy  = vmOf(t2Compare, None)
    assertEquals((legacy.contrast.minuend, legacy.contrast.subtrahend), ("", ""))
    val (selected, _, commands, effects) =
      perform(t2Compare, ReportingIntent.SetContrast(Some(forward)))
    assertEquals(spec(selected.document).contrast, Some(forward))
    assertEquals(commands.size, 1)
    assertEquals(effects.collect { case e @ AppEffect.Execution(_) => e }, Vector.empty)
    for edit <- edits.filterNot(_.isInstanceOf[ReportingIntent.GroupBy]) do
      assertEquals(
        spec(perform(selected, edit)._1.document).contrast,
        Some(forward),
        edit.toString
      )
    val (sameGroup, _, sameCommands, _) =
      perform(selected, ReportingIntent.GroupBy(spec(selected.document).groupBy))
    assertEquals(spec(sameGroup.document).contrast, Some(forward))
    assertEquals(sameCommands, Vector.empty)
    val saved = perform(
      selected,
      ReportingIntent.ConfirmSaveAs,
      ReportingEditor(Some("With explicit contrast"), None)
    )._1
    assertEquals(
      spec(saved.document, right(ReportingId.of("with-explicit-contrast"))).contrast,
      Some(forward)
    )
    val (invalid, invalidState, refused, _) =
      perform(selected, ReportingIntent.ContrastOperands("Remembered", "Remembered"))
    assertEquals(invalid.document, selected.document)
    assertEquals(refused, Vector.empty)
    assert(invalidState.error.exists(_.contains("Remembered")))
    val (reversed, _, _, _) =
      perform(selected, ReportingIntent.ContrastOperands("Forgotten", "Remembered"))
    assertEquals(
      spec(reversed.document).contrast,
      Some(right(ReportingContrast.of("Forgotten", "Remembered")))
    )
    val (ungrouped, _, _, _) = perform(selected, ReportingIntent.GroupBy(None))
    assertEquals(spec(ungrouped.document).contrast, None)
    assert(!vmOf(ungrouped, None).contrast.enabled)
    val (regrouped, _, _, _) =
      perform(ungrouped, ReportingIntent.GroupBy(Some(right(Covariate.of("response")))))
    assertEquals(spec(regrouped.document).contrast, None)
    val (cleared, _, _, _) = perform(selected, ReportingIntent.SetContrast(None))
    assertEquals(spec(cleared.document).contrast, None)
    val undone =
      AppModel.update(selected, Intent.Undo(eyes4s.studio.core.command.HistoryStack.Science))._1
    assertEquals(spec(undone.document).contrast, None)
  }

  test("an edit is undoable and an unchanged edit dispatches nothing") {
    val (after, _, _, _) = perform(t2Compare, ReportingIntent.Minimum(true))
    assert(spec(after.document).minimumPerGroup.isDefined)
    val undone =
      AppModel.update(after, Intent.Undo(eyes4s.studio.core.command.HistoryStack.Science))._1
    assertEquals(spec(undone.document), spec(t2Compare.document))
    assertEquals(perform(t2Compare, ReportingIntent.Minimum(false))._3, Vector.empty)
  }

  // --- AC 2: hit-only filters never change the control pool (E2E-09) ---------------------------------

  // On FakeStudyBackend the control pool cannot depend on the spec: the
  // navigator receives none. What studio guarantees is that a reporting edit
  // touches no analysis (asserted here) and starts no run (AC 1). The pool
  // half of this test must be re-checked on the real backend in S3.7.
  test("a hit-only filter keeps the control pool: same controls, Forgotten items included") {
    withSession { s =>
      val hits = ReportingIntent.Keep(right(Covariate.of("response")), Vector("Remembered"))
      val (after, _, _, _) = perform(t2Compare, hits)
      val filters          = spec(after.document).filters
      assert(
        filters.exists {
          case ReportingFilter.Keep(a, vs) =>
            a.label == "response" && vs.values == Vector("Remembered")
          case _ => false
        },
        filters.toString
      )
      // The run's analysis, which alone decides the pool, is untouched.
      assertEquals(after.document.analyses, t2Compare.document.analyses)
      val page = right(PageRequest.first(PageRequest.MaximumSize))
      for
        pool <- s.navigator.pairs(query, PairDesign.Control, page).map(right)
        rows <- s.queries(run7, page).map(right)
      yield
        val controls = pool.entries.collect { case StudioRef.Pair(_, _, _, _, ref) => ref }
        assertEquals(controls.size, 19)
        // Some controls are the encoding trials of items P17 forgot at retrieval:
        // a hit-only report still compares every query against them.
        val forgotten = rows.rows
          .filter(r => r.query.participant == "P17" && r.response == Response("Forgotten"))
          .flatMap(_.matched)
          .toSet
        assert(controls.exists(forgotten.contains), controls.toString)
        assertEquals(rows.rows.find(_.query == p17ret07).flatMap(_.controls), Some(19))
    }
  }

  // --- the controls --------------------------------------------------------------------------------

  test("the minimum reads native dropped cells when on; off removes it") {
    withSession { session =>
      val (on, _, _, _) = perform(t2Compare, ReportingIntent.Minimum(true))
      assertEquals(spec(on.document).minimumPerGroup.map(_.queries), Some(3))
      summaryOf(session).zip(reportOf(session, on)).map { (summary, report) =>
        val vm = vmOf(on, Some(summary), report = Some(report))
        assertEquals(
          (vm.minimum.on, vm.minimum.note.text),
          (true, "on · drops 2 Forgotten cells (P17 n 2, P21 n 2)")
        )
        assertEquals(vm.minimum.note.refs, report.dropped.map(_.ref))
        val (off, _, _, _) = perform(on, ReportingIntent.Minimum(false))
        assertEquals(spec(off.document).minimumPerGroup, None)
      }
    }
  }

  test(
    "an unevaluated minimum edit waits for native reporting and never estimates exclusions"
  ) {
    withSession { session =>
      summaryOf(session).map { summary =>
        val hits = ReportingIntent.Keep(right(Covariate.of("response")), Vector("Remembered"))
        val (kept, _, _, _) = perform(t2Compare, hits)
        val (on, _, _, _)   = perform(kept, ReportingIntent.Minimum(true))
        assertEquals(
          vmOf(on, Some(summary)).minimum.note,
          ReportingLine(
            "Cells dropped by the minimum appear after report evaluation",
            Vector.empty
          )
        )
        assertEquals(vmOf(kept, Some(summary)).minimum.note.refs, Vector.empty)
        assertEquals(vmOf(t2Compare, Some(summary)).minimum.note.refs, Vector.empty)
      }
    }
  }

  test("Save as… then Undo shows a spec that exists; Redo brings the copy back") {
    val named            = ReportingEditor(Some("Hits only"), None)
    val (saved, _, _, _) = perform(t2Compare, ReportingIntent.ConfirmSaveAs, named)
    val hits             = right(ReportingId.of("hits-only"))
    assertEquals(CompareSummary.reporting(saved), Some(hits))
    val undone =
      AppModel.update(saved, Intent.Undo(eyes4s.studio.core.command.HistoryStack.Science))._1
    assertEquals(undone.document.reporting.map(_.id), Vector(reporting))
    assertEquals(CompareSummary.reporting(undone), Some(reporting))
    val vm = ReportingEditor.vm(
      ReportingEditor.empty,
      undone.document,
      CompareSummary.reporting(undone),
      Some(run7),
      None,
      Some(sigma2)
    )
    assertEquals((vm.status, vm.title), (None, "By retrieval response"))
    val redone =
      AppModel.update(undone, Intent.Redo(eyes4s.studio.core.command.HistoryStack.Science))._1
    assertEquals(redone.document.reporting.map(_.id), Vector(reporting, hits))
    assertEquals(CompareSummary.reporting(redone), Some(hits))
  }

  test("a blank Save as name names the name field; other names give ASCII ids") {
    val (blank, intents) =
      ReportingEditor.update(
        ReportingEditor(Some("   "), None),
        t2Compare.document,
        Some(reporting),
        ReportingIntent.ConfirmSaveAs
      )
    assertEquals(intents, Vector.empty)
    assertEquals(blank.error, Some("Name of the new reporting spec: enter a name."))
    val (_, accented) =
      ReportingEditor.update(
        ReportingEditor(Some("Évité ✓ 2"), None),
        t2Compare.document,
        Some(reporting),
        ReportingIntent.ConfirmSaveAs
      )
    assertEquals(
      accented.lastOption,
      Some(ReportingEditor.show(right(ReportingId.of("vit-2"))))
    )
    val (_, symbols) =
      ReportingEditor.update(
        ReportingEditor(Some("✓✓"), None),
        t2Compare.document,
        Some(reporting),
        ReportingIntent.ConfirmSaveAs
      )
    assertEquals(symbols.lastOption, Some(ReportingEditor.show(right(ReportingId.of("spec")))))
  }

  test("the outside-window filter adds and removes the board's 25% threshold") {
    val (on, _, _, _) = perform(t2Compare, ReportingIntent.OutsideFilter(true))
    assertEquals(
      spec(on.document).filters.collect { case ReportingFilter.OutsideWindowAtMost(s) =>
        s.value
      },
      Vector(0.25)
    )
    val vm = vmOf(on, None)
    assertEquals(
      (vm.outside.on, vm.outside.note.text),
      (true, "applied · the count is not yet served by this backend")
    )
    val (off, _, _, _) = perform(on, ReportingIntent.OutsideFilter(false))
    assertEquals(spec(off.document).filters, Vector.empty)
  }

  test("group by None and pooled weighting; the minimum's preview needs a served grouping") {
    withSession(s =>
      summaryOf(s).map { sum =>
        val (none, _, _, _) = perform(t2Compare, ReportingIntent.GroupBy(None))
        assertEquals(spec(none.document).groupBy, None)
        val vm = vmOf(none, Some(sum))
        assertEquals(vm.groups.find(_.chosen).map(_.label), Some("None"))
        assertEquals(vm.groupValues, "")
        assertEquals(
          vm.minimum.note.text,
          "off · the report will name excluded participant-group cells when applied"
        )
        val (pooled, _, _, _) =
          perform(none, ReportingIntent.Weight(ReportingWeight.PooledQueries))
        assertEquals(spec(pooled.document).weighting, ReportingWeight.PooledQueries)
        assertEquals(
          vmOf(pooled, Some(sum)).weights.find(_.chosen).map(_.label),
          Some("Weight by contributing queries")
        )
      }
    )
  }

  test("Save as… saves a copy under a new name and shows it; Choose shows a saved spec") {
    val (_, opened, i0, _) = perform(t2Compare, ReportingIntent.OpenSaveAs)
    assertEquals(i0, Vector.empty)
    assertEquals(opened.saving, Some("By retrieval response (copy)"))
    val named = ReportingEditor
      .update(opened, t2Compare.document, Some(reporting), ReportingIntent.Name("Hits only"))
      ._1
    val (after, closed, intents, _) = perform(t2Compare, ReportingIntent.ConfirmSaveAs, named)
    val hits                        = right(ReportingId.of("hits-only"))
    assertEquals(closed, ReportingEditor.empty)
    assertEquals(intents.last, ReportingEditor.show(hits))
    val copy = spec(after.document, hits)
    assertEquals(copy.name, "Hits only")
    assertEquals(copy.groupBy, spec(t2Compare.document).groupBy)
    assertEquals(CompareSummary.reporting(after), Some(hits))
    val vm = ReportingEditor.vm(
      ReportingEditor.empty,
      after.document,
      Some(hits),
      Some(run7),
      None,
      Some(sigma2)
    )
    assertEquals(
      vm.saved.map(v => (v.name, v.current)),
      Vector("By retrieval response" -> false, "Hits only" -> true)
    )
    assertEquals(
      vm.saved.find(_.current).map(_.detail),
      Some("run 7 · current view · not used by a figure")
    )
    // Choosing the original shows its summary again.
    val (_, back) = ReportingEditor.update(
      ReportingEditor.empty,
      after.document,
      Some(hits),
      ReportingIntent.Choose(reporting)
    )
    assertEquals(back, Vector(ReportingEditor.show(reporting)))
    assertEquals(
      CompareSummary.reporting(AppModel.update(after, back.head)._1),
      Some(reporting)
    )
    // A name that is already taken gets a fresh id.
    val again = ReportingEditor
      .update(
        ReportingEditor(Some("Hits only"), None),
        after.document,
        Some(hits),
        ReportingIntent.ConfirmSaveAs
      )
      ._2
    assertEquals(again.last, ReportingEditor.show(right(ReportingId.of("hits-only-2"))))
  }

  test("the editor's stops, and no spec says so") {
    val vm = vmOf(t2Compare, None)
    assertEquals(
      ReportingEditor.focusStops(vm),
      Vector(
        FocusStop(A11yRole.RadioButton, "Group by: response"),
        FocusStop(A11yRole.TextField, "First level (minuend)"),
        FocusStop(A11yRole.TextField, "Subtract level (subtrahend)"),
        FocusStop(A11yRole.Button, "Apply contrast"),
        FocusStop(A11yRole.Button, "Clear contrast"),
        FocusStop(A11yRole.CheckBox, vm.outside.label),
        FocusStop(A11yRole.CheckBox, "Minimum queries per group: 3"),
        FocusStop(
          A11yRole.RadioButton,
          "Summary unit and weighting: Equal weight per participant"
        ),
        FocusStop(A11yRole.Button, "Save as…")
      )
    )
    val none = ReportingEditor.vm(
      ReportingEditor.empty,
      t2Compare.document,
      None,
      Some(run7),
      None,
      None
    )
    assertEquals(none.status, Some("No reporting spec yet."))
    assertEquals(ReportingEditor.focusStops(none), Vector.empty)
  }
