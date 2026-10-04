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

import eyes4s.studio.app.maps.LimitsScope
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.{
  AnalysisRevision,
  DatasetRevision,
  LedgerEntry,
  PageRequest,
  PairDesign,
  QueryRow,
  QueryStatus,
  ResultSummary,
  TrialDisposition
}
import eyes4s.studio.core.document.{
  ControlChoice,
  MatchedChoice,
  OccurrencePick,
  Perspective,
  Recipe,
  StageAppearance,
  StudioDocument,
  UnmatchedChoice
}
import eyes4s.studio.core.fixture.{GoldenAssets, MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{StudioRef, TrialGrouping}

import scala.concurrent.{ExecutionContext, Future}

/** Compare's "Why this reference?" inspector headlessly (ticket S8.4;
  * Main.dc.html, inspector), on the fake backend at t2: the explanation is
  * generated from the prepared design (the run's recipe picks a template, the
  * run's query row and the dataset's ledger fill it), outside-window figures
  * are eyes4s's tally of the panels' fixations, the analysis and reporting
  * sections are read-only with Edit in Analysis, and the appearance section
  * dispatches View-only commands.
  */
class WhyReferenceSuite extends munit.FunSuite:
  import StoryModels.*

  private given ExecutionContext = ExecutionContext.global

  private val run7 = StoryMoments.run7
  private val rev4 = AnalysisRevision(4)
  private val r3   = DatasetRevision(3)

  private def right[E, A](e: Either[E, A]): A =
    e.fold(x => fail(s"unexpected Left: $x"), identity)

  private lazy val registry =
    right(GoldenAssets.registry(right(StoryMoments.t2).datasets.last))

  private def withSession[A](f: HeadlessSession => Future[A]): Future[A] =
    HeadlessSession
      .open(StoryMoment.T2)
      .flatMap(s => f(s).transformWith(x => s.close.transform(_ => x)))

  /** Run 7 as Compare reads it: rows, summary and the r3 ledger. */
  private final case class Read(
      shown: ShownRun,
      summary: ResultSummary,
      ledger: Vector[LedgerEntry]
  )

  private def read(s: HeadlessSession): Future[Read] =
    for
      q   <- s.queries(run7, right(PageRequest.first(PageRequest.MaximumSize)))
      sum <- s.result(run7)
      led <- s.wholeLedger(r3)
    yield Read(ShownRun(run7, rev4, right(q).rows), right(sum), right(led))

  /** A trial's content as the window's content source serves it: the
    * backend's placed fixations under the revision.
    */
  private def content(
      s: HeadlessSession,
      key: ContentKey
  ): Future[Either[ContentError, TrialContent]] =
    s.trialFixations(key.revision, key.trial).map {
      case Left(e)   => Left(ContentError.Unreadable(key.trial, e.message))
      case Right(tf) =>
        registry.display(key.trial).toRight(ContentError.NotServed(key.trial)).map { d =>
          TrialContent(
            d,
            registry.screen,
            tf.fixations.map(f =>
              ContentFixation(
                key.trial,
                f.ref.index,
                f.screenX,
                f.screenY,
                math.round(f.onsetMs).toInt,
                math.round(f.durationMs).toInt,
                f.placement
              )
            )
          )
        }
    }

  /** The panels synced on `m`, every effect answered by the fake. */
  private def panels(s: HeadlessSession, m: AppModel, shown: ShownRun): Future[TrialPanels] =
    val (synced, effects) = TrialPanels.sync(TrialPanels.empty, m, Some(shown))
    effects.foldLeft(Future.successful(synced)) {
      case (acc, PanelsEffect.InspectPair(run, address, pair)) =>
        acc.flatMap(p =>
          s.inspect(run, address)
            .map(a =>
              TrialPanels.update(
                p,
                PanelsIntent
                  .PairRead(pair, a.fold(PairAnswer.Refused(_), PairAnswer.Answered(_)))
              )
            )
        )
      case (acc, PanelsEffect.ReadContent(key)) =>
        acc.flatMap(p =>
          content(s, key).map(c => TrialPanels.update(p, PanelsIntent.ContentRead(key, c)))
        )
    }

  /** The inspector with the ledger answered. */
  private def inspector(r: Read): WhyReference =
    val (synced, effects) = WhyReference.sync(WhyReference.empty, Some(r3))
    assertEquals(effects, Vector(WhyEffect.ReadLedger(r3)))
    WhyReference.update(synced, WhyIntent.LedgerRead(r3, LedgerAnswer.Answered(r.ledger)))._1

  private def vmAt(
      s: HeadlessSession,
      m: AppModel,
      r: Read,
      doc: Option[StudioDocument] = None
  ): Future[CompareInspectorVM] =
    panels(s, m, r.shown).map(p =>
      WhyReference.vm(
        inspector(r),
        doc.getOrElse(m.document),
        InspectorInputs(p, Some(r.shown), Some(r.summary), Some(reporting))
      )
    )

  private def facts(vm: CompareInspectorVM): Map[String, String] =
    vm.why.fold(fail("no explanation"))(_.facts.map(f => f.label -> f.value).toMap)

  private def at(m: AppModel, ref: StudioRef): AppModel =
    AppModel.update(m, Intent.Explain(Place.At(ref)))._1

  private val p17enc08 = MockStudy.key("P17", "enc_08")
  private val control  = StudioRef.Pair(run7, sigma2, PairDesign.Control, p17ret07, p17enc08)

  // --- the board's two explanations ------------------------------------------------------

  test("t2's matched reference: the board's sentence and facts, from the prepared design") {
    withSession(s =>
      read(s).flatMap(r => vmAt(s, t2Compare, r)).map { vm =>
        assertEquals(vm.title, "Why this reference?")
        assertEquals(vm.status, None)
        assertEquals(
          vm.why.map(_.explanation),
          Some(
            "Matched reference: the only admitted Encoding trial of P17 whose match item is " +
              "beach-042, the item this retrieval trial cued."
          )
        )
        val f = facts(vm)
        assertEquals(f("Participant"), "P17, same as query")
        assertEquals(f("Phase · occurrence"), "Encoding · 1 of 1")
        assertEquals(f("Item"), "beach-042")
        assertEquals(f("Controls"), "19 used")
        // A tally of the participant and phase, the same for every query of P17.
        assertEquals(f("Not admitted (P17 · Encoding)"), "none of 20 trials")
        // FIXTURE.md: ret_07 1 of 12 fixations · 4% of duration; enc_03 1 of 13 · 3%.
        assertEquals(f("Outside window"), "1 of 13 fix · 3% dur")
        assertEquals(f("Query outside"), "1 of 12 fix · 4% dur")
      }
    )
  }

  test("a control: the board's control sentence, the control's own item and outside figures") {
    withSession(s =>
      read(s).flatMap(r => vmAt(s, at(t2Compare, control), r)).map { vm =>
        assertEquals(
          vm.why.map(_.explanation),
          Some(
            "Eligible control: an admitted Encoding trial of the same participant (P17) " +
              "showing a different item, one per item, chosen as the matched reference is. " +
              "19 such references are used; none are sampled."
          )
        )
        val f = facts(vm)
        assertEquals(f("Item"), "dog-077")
        assertEquals(f("Phase · occurrence"), "Encoding · 1 of 1")
        // The query's own figure does not move with the reference.
        assertEquals(f("Query outside"), "1 of 12 fix · 4% dur")
        assert(f("Outside window").matches("\\d+ of \\d+ fix · \\d+% dur"), f("Outside window"))
      }
    )
  }

  // --- generated from the prepared design, never hand-written per case ----------------------

  /** `m`'s document with the shown run's analysis recipe changed by `f`. */
  private def withRecipe(m: AppModel)(f: Recipe => Recipe): StudioDocument =
    val doc     = m.document
    val changed =
      doc.analyses.map(a => if a.id == rev4 then a.copy(recipe = f(a.recipe)) else a)
    right(
      StudioDocument.of(
        doc.datasets,
        changed,
        doc.draft,
        doc.runs,
        doc.reporting,
        doc.figures,
        doc.presentation,
        doc.jobs
      )
    )

  test("every contributing query of run 7 gets the template filled from its own row") {
    withSession(s =>
      read(s).map { r =>
        val why     = inspector(r)
        val queries = r.shown.rows.filter(_.status match
          case QueryStatus.Contributing(_, _, _) => true
          case _                                 => false)
        assertEquals(queries.size, 454)
        val sentences = queries.map { row =>
          val focus = PanelFocus(run7, sigma2, row.query, None)
          val p     = TrialPanels.empty.copy(focus = Some(focus))
          val vm    = WhyReference.vm(
            why,
            t2Compare.document,
            InspectorInputs(p, Some(r.shown), Some(r.summary), Some(reporting))
          )
          val text = vm.why.fold(fail(s"no explanation for ${row.query}"))(_.explanation)
          assertEquals(
            text,
            s"Matched reference: the only admitted Encoding trial of ${row.query.participant} " +
              s"whose match item is ${row.item}, the item this retrieval trial cued."
          )
          text
        }
        // No two queries of a participant share an item, so no two share a sentence.
        assertEquals(
          sentences.distinct.size,
          queries.map(q => (q.query.participant, q.item)).distinct.size
        )
      }
    )
  }

  test("the recipe's matching and control choices pick the template") {
    withSession(s =>
      read(s).flatMap { r =>
        val select = withRecipe(t2Compare)(
          _.copy(matched = MatchedChoice.Select(OccurrencePick.First))
        )
        val mean = withRecipe(t2Compare)(_.copy(matched = MatchedChoice.MeanOfAll))
        val all  = withRecipe(t2Compare)(_.copy(controls = ControlChoice.AllOccurrences))
        for
          a <- vmAt(s, t2Compare, r, Some(select))
          b <- vmAt(s, t2Compare, r, Some(mean))
          c <- vmAt(s, at(t2Compare, control), r, Some(all))
        yield
          assertEquals(
            a.why.map(_.explanation),
            Some(
              "Matched reference: the first admitted Encoding occurrence of beach-042 for " +
                "P17, the item this retrieval trial cued."
            )
          )
          assertEquals(
            b.why.map(_.explanation),
            Some(
              "Matched reference: an admitted Encoding trial of P17 whose match item is " +
                "beach-042; M is the mean over every such trial."
            )
          )
          assertEquals(
            c.why.map(_.explanation),
            Some(
              "Eligible control: an admitted Encoding trial of the same participant (P17) " +
                "showing a different item, every occurrence of it counted. All 19 such " +
                "trials are used; none are sampled."
            )
          )
          assertEquals(
            facts(c)("Item"),
            "dog-077"
          )
          assertEquals(
            c.analysis.flatMap(_.facts.find(_.label == "Metric · controls")).map(_.value),
            Some("Cosine · all occurrences")
          )
      }
    )
  }

  test("a query with 18 controls says 18 and names the excluded reference trial") {
    withSession(s =>
      read(s).map { r =>
        val row = r.shown.rows
          .find(_.controls.contains(18))
          .getOrElse(fail("the fixture has a query with 18 controls"))
        val p        = row.query.participant
        val excluded = r.ledger.filter(e =>
          e.trial.participant == p && e.trial.phase == row.matched.phase &&
            e.disposition != TrialDisposition.Admitted
        )
        assertEquals(excluded.size, 1)
        val focus = PanelFocus(run7, sigma2, row.query, Some((PairDesign.Control, row.matched)))
        val vm    = WhyReference.vm(
          inspector(r),
          t2Compare.document,
          InspectorInputs(
            TrialPanels.empty.copy(focus = Some(focus)),
            Some(r.shown),
            Some(r.summary),
            Some(reporting)
          )
        )
        val text = vm.why.fold(fail("no explanation"))(_.explanation)
        assert(text.endsWith("18 such references are used; none are sampled."), text)
        val f     = facts(vm)
        val tally = s"Not admitted ($p · Encoding)"
        assertEquals(f("Controls"), "18 used")
        assert(
          f(tally).startsWith(s"1 of 20 trials: ${excluded.head.trial.trial} ("),
          f(tally)
        )
      }
    )
  }

  test("a no-match query is explained by the recipe's unmatched policy") {
    withSession(s =>
      read(s).map { r =>
        val row = r.shown.rows
          .find(_.status match
            case QueryStatus.NoMatch(_) => true
            case _                      => false)
          .getOrElse(fail("the fixture has no-match queries"))
        def text(doc: StudioDocument) =
          WhyReference
            .vm(
              inspector(r),
              doc,
              InspectorInputs(
                TrialPanels.empty.copy(focus = Some(PanelFocus(run7, sigma2, row.query, None))),
                Some(r.shown),
                Some(r.summary),
                Some(reporting)
              )
            )
            .why
            .map(_.explanation)
        val who = (row.query.participant, row.item)
        assertEquals(
          text(t2Compare.document),
          Some(
            s"No matched reference: no admitted Encoding trial of ${who._1} has match item " +
              s"${who._2}. The query is reported as no match."
          )
        )
        assertEquals(
          text(withRecipe(t2Compare)(_.copy(unmatched = UnmatchedChoice.Refuse))),
          Some(
            s"No matched reference: no admitted Encoding trial of ${who._1} has match item " +
              s"${who._2}. The recipe refuses a study with such a query."
          )
        )
      }
    )
  }

  // --- queries without a reference, and failed queries ---------------------------------------

  /** The inspector's view of `row`'s query, its matched reference in focus. */
  private def queryVM(r: Read, row: eyes4s.studio.core.backend.QueryRow): CompareInspectorVM =
    WhyReference.vm(
      inspector(r),
      t2Compare.document,
      InspectorInputs(
        TrialPanels.empty.copy(focus = Some(PanelFocus(run7, sigma2, row.query, None))),
        Some(r.shown),
        Some(r.summary),
        Some(reporting)
      )
    )

  test("a no-match query shows no reference facts, only its would-be match and why") {
    withSession(s =>
      read(s).map { r =>
        val row = r.shown.rows
          .find(q => q.query == MockStudy.key("P03", "ret_11"))
          .getOrElse(fail("P03 ret_11 is a no-match query of the fixture"))
        assert(row.status.isInstanceOf[QueryStatus.NoMatch], row.status.toString)
        val vm     = queryVM(r, row)
        val labels = vm.why.toVector.flatMap(_.facts.map(_.label))
        assertEquals(
          labels,
          Vector(
            "Participant",
            "Would-be match",
            "Not admitted (P03 · Encoding)",
            "Query outside"
          )
        )
        val would = vm.why.toVector.flatMap(_.facts).find(_.label == "Would-be match").get
        val entry = r.ledger
          .find(e =>
            e.trial.participant == "P03" && e.trial.phase == row.matched.phase && e.item == row.item
          )
          .getOrElse(fail(s"the ledger lists P03's ${row.item}"))
        assert(would.value.startsWith(s"${entry.trial.trial} ("), would.value)
        assert(entry.disposition != TrialDisposition.Admitted, entry.toString)
        assertEquals(would.ref, Some(StudioRef.Trial(entry.trial)))
      }
    )
  }

  test("a query that was not admitted shows no reference facts") {
    withSession(s =>
      read(s).map { r =>
        val row = r.shown.rows
          .find(_.status.isInstanceOf[QueryStatus.NotAdmitted])
          .getOrElse(fail("the fixture has not-admitted queries"))
        val vm = queryVM(r, row)
        assert(
          vm.why.exists(_.explanation.endsWith("so it has no reference.")),
          vm.why.toString
        )
        assertEquals(
          vm.why.toVector.flatMap(_.facts.map(_.label)),
          Vector(
            "Participant",
            s"Not admitted (${row.query.participant} · Encoding)",
            "Query outside"
          )
        )
      }
    )
  }

  test("a failed query names its failure and is not counted as scored") {
    withSession(s =>
      read(s).map { r =>
        val failed =
          r.shown.rows.collect { case q @ QueryRow(_, _, _, _, _, QueryStatus.Failed(d)) =>
            (q, d)
          }
        assertEquals(failed.size, 3)
        failed.foreach { (row, diagnostic) =>
          val vm   = queryVM(r, row)
          val text = vm.why.fold(fail("no explanation"))(_.explanation)
          assertEquals(
            text,
            s"The query failed (${diagnostic.message}), so it has no M, B or D. Its reference " +
              "is shown as the design chose it."
          )
          assertEquals(
            facts(vm)("Controls"),
            row.controls.fold("—")(n => s"$n designed · none scored")
          )
        }
      }
    )
  }

  test("'1 of N' counts only the reference's participant: an item shared across participants") {
    withSession(s =>
      read(s).flatMap { r =>
        val shared = r.ledger.find(_.trial == p17enc03).getOrElse(fail("no enc_03 entry"))
        // P01 also saw beach-042 at encoding: not P17's occurrence.
        val other = shared.copy(trial = MockStudy.key("P01", "enc_99"))
        val wider = r.copy(ledger = r.ledger :+ other)
        // A second occurrence of beach-042 for P17 is counted.
        val twice =
          r.copy(ledger = r.ledger :+ shared.copy(trial = MockStudy.key("P17", "enc_99")))
        for
          a <- vmAt(s, t2Compare, wider)
          b <- vmAt(s, t2Compare, twice)
        yield
          assertEquals(facts(a)("Phase · occurrence"), "Encoding · 1 of 1")
          assertEquals(facts(b)("Phase · occurrence"), "Encoding · 1 of 2")
      }
    )
  }

  // --- traceability ----------------------------------------------------------------------------

  test("every number of the explanation traces to a StudioRef") {
    withSession(s =>
      read(s).flatMap(r => vmAt(s, t2Compare, r)).map { vm =>
        val all = vm.why.toVector.flatMap(_.facts)
        all.filter(_.value.exists(_.isDigit)).foreach(f => assert(f.ref.isDefined, f.label))
        val refs = all.map(f => f.label -> f.ref).toMap
        assertEquals(refs("Query outside"), Some(StudioRef.Trial(p17ret07)))
        assertEquals(refs("Outside window"), Some(StudioRef.Trial(p17enc03)))
        assertEquals(refs("Controls"), Some(StudioRef.QueryContrast(run7, sigma2, p17ret07)))
        val group = Some(
          StudioRef.TrialGroup(
            r3,
            TrialGrouping.PhaseOf("P17", MockStudy.key("P17", "enc_03").phase)
          )
        )
        assertEquals(refs("Not admitted (P17 · Encoding)"), group)
        // "of N" counts the ledger's trials of the item: the phase group.
        assertEquals(refs("Phase · occurrence"), group)
      }
    )
  }

  // --- the ledger read ---------------------------------------------------------------------------

  test(
    "the ledger is read once per dataset; a stale answer is dropped; a failure offers Retry"
  ) {
    val (s1, e1) = WhyReference.sync(WhyReference.empty, Some(r3))
    assertEquals(e1, Vector(WhyEffect.ReadLedger(r3)))
    assertEquals(WhyReference.sync(s1, Some(r3)), (s1, Vector.empty))
    // An answer for another dataset is not this one's.
    val stale = WhyReference.update(
      s1,
      WhyIntent.LedgerRead(DatasetRevision(2), LedgerAnswer.Answered(Vector.empty))
    )
    assertEquals(stale, (s1, Vector.empty))
    val (failed, _) =
      WhyReference.update(s1, WhyIntent.LedgerRead(r3, LedgerAnswer.Failed("down")))
    val vm = WhyReference.vm(
      failed,
      t2Compare.document,
      InspectorInputs(TrialPanels.empty, None, None, None)
    )
    assertEquals(vm.retry, Some("Retry"))
    assertEquals(
      WhyReference.update(failed, WhyIntent.Retry),
      (s1, Vector(WhyEffect.ReadLedger(r3)))
    )
    // Retry does nothing while nothing failed.
    assertEquals(WhyReference.update(s1, WhyIntent.Retry), (s1, Vector.empty))
  }

  test("without a focus the inspector asks for a query; the explanation waits for the rows") {
    val none = WhyReference.vm(
      WhyReference.empty,
      t2Compare.document,
      InspectorInputs(TrialPanels.empty, None, None, None)
    )
    assertEquals(none.status, Some("Choose a query in the Queries navigator"))
    assertEquals(none.why, None)
    val focus = PanelFocus(run7, sigma2, p17ret07, None)
    val empty = WhyReference.vm(
      WhyReference.empty,
      t2Compare.document,
      InspectorInputs(
        TrialPanels.empty.copy(focus = Some(focus)),
        Some(ShownRun(run7, rev4, Vector.empty)),
        None,
        Some(reporting)
      )
    )
    assertEquals(empty.status, Some("Reading the run's queries…"))
  }

  // --- analysis, reporting ------------------------------------------------------------------------

  test("the analysis section is rev 4's recipe, read-only, and Edit opens Analysis") {
    withSession(s =>
      read(s).flatMap(r => vmAt(s, t2Compare, r)).map { vm =>
        val a = vm.analysis.getOrElse(fail("no analysis section"))
        assertEquals(a.title, "Analysis rev 4")
        assertEquals(a.kind, "Analysis · rerun")
        assertEquals(
          a.facts.map(f => f.label -> f.value),
          Vector(
            "Representation"    -> "Duration-weighted fixation density",
            "Window"            -> "Image frame 1024×768",
            "Initial fixation"  -> "Keep all",
            "Smoother"          -> "Gaussian, σ 0.5°, 1°, 2°, 4°",
            "Grid"              -> "64×48 · cell 0.46°",
            "Metric · controls" -> "Cosine · all eligible"
          )
        )
        assertEquals(
          vm.edit,
          ("Edit in Analysis ⌘3", Intent.SwitchPerspective(Perspective.Analysis))
        )
        assertEquals(
          AppModel.update(t2Compare, vm.edit._2)._1.perspective,
          Perspective.Analysis
        )
      }
    )
  }

  test("the reporting section is the trail's spec, with the run's per-group n range") {
    withSession(s =>
      read(s).flatMap(r => vmAt(s, t2Compare, r)).map { vm =>
        assertEquals(vm.reporting.title, "Reporting")
        assertEquals(vm.reporting.kind, "Reporting · no rerun")
        assertEquals(
          vm.reporting.facts.map(f => f.label -> f.value),
          Vector(
            "Spec"                  -> "By retrieval response",
            "Group by"              -> "response",
            "Filters"               -> "None",
            "Min queries per group" -> "Off · n per group 2–17",
            "Summary"               -> "Participant means, equal weight"
          )
        )
      }
    )
  }

  // --- appearance ----------------------------------------------------------------------------------

  test("appearance: dark stage, shared limits and opacity 0.6 by default; each control acts") {
    val vm0 = WhyReference.vm(
      WhyReference.empty,
      t2Compare.document,
      InspectorInputs(TrialPanels.empty, None, None, None)
    )
    val a0 = vm0.appearance
    assertEquals(a0.kind, "View only")
    assertEquals(
      a0.stages.map(c => (c.label, c.chosen)),
      Vector("Dark" -> true, "Mid" -> false, "Light" -> false)
    )
    assertEquals(
      a0.limits.map(c => (c.label, c.chosen)),
      Vector("Shared" -> true, "Per panel" -> false)
    )
    assertEquals((a0.opacity, a0.opacityText), (0.6, "0.60"))
    // The stage and the opacity are the document's presentation: View-only commands.
    val mid = AppModel.update(t2Compare, WhyReference.stageIntent(StageAppearance.Mid))._1
    assertEquals(mid.document.presentation.stage, StageAppearance.Mid)
    assertEquals(
      StudioDocument.scienceDigest(mid.document),
      StudioDocument.scienceDigest(t2Compare.document)
    )
    val op    = WhyReference.opacityIntent(0.4, 0.6).getOrElse(fail("0.4 is an opacity"))
    val faded = AppModel.update(mid, op)._1
    val a1    = WhyReference
      .vm(
        WhyReference.empty,
        faded.document,
        InspectorInputs(TrialPanels.empty, None, None, None)
      )
      .appearance
    assertEquals(a1.stages.find(_.chosen).map(_.value), Some(StageAppearance.Mid))
    assertEquals((a1.opacity, a1.opacityText), (0.4, "0.40"))
    assertEquals(WhyReference.opacityIntent(1.5, 0.6), None)
    // A slider released without moving dispatches nothing.
    assertEquals(WhyReference.opacityIntent(0.6, 0.6), None)
    // Colour limits are the inspector's own view state.
    val (per, effects) =
      WhyReference.update(WhyReference.empty, WhyIntent.ChooseLimits(LimitsScope.PerPanel))
    assertEquals(effects, Vector.empty)
    val a2 = WhyReference
      .vm(per, t2Compare.document, InspectorInputs(TrialPanels.empty, None, None, None))
      .appearance
    assertEquals(a2.limits.find(_.chosen).map(_.value), Some(LimitsScope.PerPanel))
  }

  test("the inspector's stops: Edit, the chosen stage and limits, the opacity slider") {
    val vm = WhyReference.vm(
      WhyReference.empty,
      t2Compare.document,
      InspectorInputs(TrialPanels.empty, None, None, None)
    )
    assertEquals(
      WhyReference.focusStops(vm),
      Vector(
        FocusStop(A11yRole.Button, "Edit in Analysis ⌘3"),
        FocusStop(A11yRole.RadioButton, "Stage: Dark"),
        FocusStop(A11yRole.RadioButton, "Colour limits: Shared"),
        FocusStop(A11yRole.Slider, "Map opacity")
      )
    )
  }
