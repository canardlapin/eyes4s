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

import cats.instances.future.*
import eyes4s.studio.app.compare.SummaryAnswer
import eyes4s.studio.app.layout.StudioLayouts
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.{PageRequest, QueryStatus, ReportView, ResultSummary}
import eyes4s.studio.core.document.{FigureId, StudioDocument, FigureMethodsDraft}
import eyes4s.studio.core.figures.{FigureSource, MethodsFacts, MethodsReads}
import eyes4s.studio.core.command.{Command, HistoryStack}
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession

import scala.concurrent.{ExecutionContext, Future}

/** The methods generator (ticket S9.4; Figures.dc.html, methods.md) on the
  * fake backend at story moment t2: Figure 1's methods from run 7, dataset
  * r3's admission and the "By retrieval response" spec.
  */
class MethodsGeneratorSuite extends munit.FunSuite:
  import StoryMoments.{r3, run7}

  private given ExecutionContext = ExecutionContext.global

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private def t2: AppModel = StoryModels.t2Figures
  private val figure1      = ok(FigureId.of(1))
  private def source       = ok(FigureSource.of(t2.document, figure1))

  /** Run 7's result summary and the facts its methods cite. */
  private def read: Future[(ResultSummary, MethodsFacts, ReportView)] =
    for
      session <- HeadlessSession.open(StoryMoment.T2)
      summary <- session.result(run7)
      facts   <- MethodsReads.read[Future](session.admission, session.queries, run7, r3)
      report  <- session.report(
        run7,
        source.reporting,
        ok(FigureComposer.reportingScale(source)).value
      )
      _ <- session.close
    yield (ok(summary), ok(facts), ok(report))

  private def generated: Future[GeneratedMethods] =
    read.map((s, f, r) => ok(MethodsText.generate(source, s, f, r)))

  /** The generated methods.md of Figure 1 at t2: the fixture's golden text. */
  private val Golden: String =
    "Fixations (fixations.csv, 11,520 records; dataset r3) were admitted per trial against " +
      "the trials.csv inventory of 960 trials: 937 admitted, 12 quarantined (overlap 6, " +
      "duplicate-ordinals 4, rejected-records 2), 5 with no admitted fixations and 6 absent " +
      "(no fixation records). Positions were analysed in the 1024 × 768 image frame placed " +
      "in a 1920 × 1080 screen; 543 of the 11,311 fixation records of admitted trials (4.8% " +
      "of their fixation duration), in 409 trials, fell outside the frame and were excluded " +
      "from the maps; they are reported per trial. No fixation record lay outside the screen " +
      "(off-screen policy: exclude the record). Degrees are measured from the image centre " +
      "(x right, y up) using a declared, uncalibrated 35 px/° with linear conversion. The " +
      "initial-fixation policy was Keep all: every fixation, including the first, was kept. " +
      "Each trial was represented as a duration-weighted fixation density map on a 64 × 48 " +
      "grid (cell 0.46°), smoothed with Gaussian kernels at σ = 0.5°, 1°, 2° and 4°, declared " +
      "before the run. Of 480 retrieval queries, 457 were eligible; for each eligible query, " +
      "cosine similarity was computed to the one matched encoding trial (M) and to every " +
      "admitted encoding trial of another item from the same participant (19 per query; 18 " +
      "for 171 queries); their mean is B, and D = M − B. A contrast required all of its " +
      "pairs: 454 contributed, 3 failed (off-window), 9 had no matched trial and 14 " +
      "queries were not admitted. D was averaged within participant, then across " +
      "participants with equal weight, separately by retrieval response (n = 24 each). Per participant, groups held 2–17 queries; no minimum per group was applied " +
      "in this reporting spec (P17 and P21 each have 2 Forgotten queries). Reporting counts above are evaluated at σ 2°. D measures spatial " +
      "correspondence, not sequential replay. D does not separate participant-specific " +
      "reinstatement from item-driven salience common to all viewers of that image, and may " +
      "retain residual centre bias. Analysis rev 4, run 7; eyes4s 0.1."

  // --- Every number generated ----------------------------------------------------------

  test("the generated text is the fixture's golden methods.md") {
    generated.map(g => assertNoDiff(g.text, Golden))
  }

  test("every number in the text is a fact from the run, never fixed wording") {
    generated.map { g =>
      val words = g.tokens.collect { case MethodsToken.Words(w) => w }
      words.foreach(w => assert(!w.exists(_.isDigit), s"a number in fixed wording: “$w”"))
      // The citations the scope names are there, each from its source.
      val slots = g.tokens.collect { case MethodsToken.Fact(slot, _) => slot }.toSet
      Vector(
        MethodsSlot.Dataset,
        MethodsSlot.Eligible,
        MethodsSlot.Queries,
        MethodsSlot.Controls,
        MethodsSlot.OutsideWindow,
        MethodsSlot.OutsideWindowShare,
        MethodsSlot.OffScreenPolicy,
        MethodsSlot.InitialFixations,
        MethodsSlot.PixelsPerDegree,
        MethodsSlot.Cell,
        MethodsSlot.Scales,
        MethodsSlot.Method,
        MethodsSlot.MatchedRule,
        MethodsSlot.ControlRule,
        MethodsSlot.FailurePolicy,
        MethodsSlot.ReportingWeight,
        MethodsSlot.GroupRange
      ).foreach(s => assert(slots.contains(s), s"no $s"))
    }
  }

  test("a changed fact changes the text where it is cited, and nowhere else") {
    read.map { (summary, facts, report) =>
      val before = ok(MethodsText.generate(source, summary, facts, report)).text
      val after  = ok(
        MethodsText.generate(
          source,
          summary.copy(
            eligibleQueries = 456,
            contrasts = summary.contrasts.copy(contributing = 453, failed = 4)
          ),
          facts.copy(
            admission = facts.admission.copy(admitted = 936),
            failures = Vector("study-failure.off-window" -> 3, "study-failure.empty-map" -> 1)
          ),
          report
        )
      ).text
      assertEquals(
        TextDiff(before, after).collect { case DiffLine.Added(s) => s }.map(_.take(40)),
        Vector(
          "Fixations (fixations.csv, 11,520 records",
          "Of 480 retrieval queries, 456 were eligi",
          "A contrast required all of its pairs: 45"
        )
      )
      assert(after.contains("936 admitted"), after)
      assert(after.contains("453 contributed, 4 failed (off-window 3, empty-map 1)"), after)
    }
  }

  test("the outside share is eyes4s's share of fixation duration, unsaid when undefined") {
    read.map { (summary, facts, report) =>
      def text(w: eyes4s.studio.core.backend.WindowTotals) =
        ok(
          MethodsText.generate(
            source,
            summary,
            facts.copy(admission = facts.admission.copy(window = w)),
            report
          )
        ).text
      val w = facts.admission.window
      // A quarter of the duration outside, whatever the counts say.
      val quarter =
        w.copy(outsideWindowMicros = 250, outsideScreenMicros = 0, totalMicros = 1000)
      assert(text(quarter).contains("(25.0% of their fixation duration)"), text(quarter))
      // No duration at all: the share is undefined and not said; the counts stay.
      val none = w.copy(outsideWindowMicros = 0, outsideScreenMicros = 0, totalMicros = 0)
      assert(!text(none).contains("% of their fixation duration"), text(none))
      assert(
        text(none).contains("543 of the 11,311 fixation records of admitted trials, in 409")
      )
    }
  }

  test("no-fixations is its own disposition, never counted as quarantined") {
    read.map { (summary, facts, report) =>
      val g      = ok(MethodsText.generate(source, summary, facts, report))
      val facts1 = g.tokens.collect { case MethodsToken.Fact(slot, shown) => slot -> shown }
      assert(facts1.contains(MethodsSlot.Quarantined -> "12"), facts1)
      assert(facts1.contains(MethodsSlot.NoFixations -> "5"), facts1)
      assertEquals(facts.admission.quarantinedTrials, 12)
      assertEquals(facts.admission.noFixations, 5)
    }
  }

  test("facts of another run or dataset are refused, naming both") {
    read.map { (summary, facts, report) =>
      val run5 = StoryMoments.run5
      assertEquals(
        MethodsText
          .generate(source, summary.copy(run = run5), facts, report)
          .left
          .map(_.message),
        Left("The methods of run 7 were given the result summary of run 5.")
      )
      assertEquals(
        MethodsText
          .generate(source, summary, facts.copy(run = run5), report)
          .left
          .map(_.message),
        Left("The methods of run 7 were given the query facts of run 5.")
      )
      assertEquals(
        MethodsText
          .generate(
            source,
            summary,
            facts.copy(admission = facts.admission.copy(dataset = StoryMoments.r2)),
            report
          )
          .left
          .map(_.message),
        Left("The methods of dataset r3 were given the admission of r2.")
      )
    }
  }

  test("the facts are read page by page and tallied") {
    read.map { (_, facts, _) =>
      assertEquals(facts.queries, 480)
      // Contributing and failed queries: 454 + 3.
      assertEquals(facts.controls.queries, 457)
      assertEquals(facts.controls.mode, Some(19))
      assertEquals(facts.failures, Vector("study-failure.off-window" -> 3))
    }
  }

  test("controls are tallied over the queries compared, never the unmatched or unadmitted") {
    for
      session <- HeadlessSession.open(StoryMoment.T2)
      page    <- session.queries(run7, ok(PageRequest.of(0, PageRequest.MaximumSize)))
      summary <- session.admission(r3)
      _       <- session.close
    yield
      val rows = ok(page).rows
      // A query that was never compared counts no controls, whatever its row says.
      val unscored = rows.map(r =>
        r.status match
          case QueryStatus.NoMatch(_) | QueryStatus.NotAdmitted(_) => r.copy(controls = Some(5))
          case _                                                   => r
      )
      val facts = MethodsReads.of(run7, ok(summary), unscored)
      assertEquals(facts.controls, MethodsReads.of(run7, ok(summary), rows).controls)
      assert(!facts.controls.entries.exists(_._1 == 5), facts.controls)
  }

  test("failure facts retain every scale code once per query") {
    for
      session   <- HeadlessSession.open(StoryMoment.T2)
      page      <- session.queries(run7, ok(PageRequest.of(0, PageRequest.MaximumSize)))
      admission <- session.admission(r3)
      _         <- session.close
    yield
      val failed = ok(page).rows.find(_.status.isFailed).getOrElse(fail("no failed query"))
      val first  = failed.status.diagnosticAt(0).getOrElse(fail("no failure diagnostic"))
      val second =
        first.copy(code = "study-failure.empty-map", message = "different scale failure")
      val rows = Vector(
        failed.copy(status = QueryStatus.FailedAtScales(Vector(first, second, first))),
        failed.copy(status = QueryStatus.Failed(first))
      )
      val facts = MethodsReads.of(run7, ok(admission), rows)
      assertEquals(facts.failures, Vector(first.code -> 2, second.code -> 1))
      assertEquals(facts.controls.queries, 2)
  }

  test("a query listing in small pages reads every page") {
    for
      session <- HeadlessSession.open(StoryMoment.T2)
      whole   <- MethodsReads.read[Future](session.admission, session.queries, run7, r3)
      paged   <- MethodsReads.read[Future](
        session.admission,
        // Answers at most 50 rows a page, whatever the request asks for.
        (run, page) =>
          session
            .queries(run, page)
            .map(_.map { p =>
              val rows = p.rows.take(50)
              p.copy(
                page = p.page
                  .copy(next = Option.when(page.offset + 50 < p.page.total)(page.offset + 50)),
                rows = rows
              )
            }),
        run7,
        r3
      )
      _ <- session.close
    yield assertEquals(paged, whole)
  }

  // --- Edit, regenerate, diff ----------------------------------------------------------

  test("paired counts are cited only from an explicitly declared report contrast") {
    for
      values  <- read
      session <- HeadlessSession.open(StoryMoment.T2)
      spec     = source.reporting
      contrast = ok(eyes4s.studio.core.document.ReportingContrast.of("Remembered", "Forgotten"))
      explicit = ok(
        eyes4s.studio.core.document.ReportingSpec.of(
          spec.id,
          spec.name,
          spec.groupBy,
          spec.filters,
          spec.minimumPerGroup,
          spec.weighting,
          Some(contrast)
        )
      )
      answer <- session.report(run7, explicit, ok(FigureComposer.reportingScale(source)).value)
      _      <- session.close
    yield
      val (summary, facts, meanOnly) = values
      assert(
        !ok(MethodsText.generate(source, summary, facts, meanOnly)).text.contains("paired n")
      )
      val report = ok(answer)
      val paired = report
        .contrast(eyes4s.studio.core.backend.ReportRole.Difference)
        .getOrElse(fail("no declared contrast"))
      val text =
        ok(MethodsText.generate(source.copy(reporting = explicit), summary, facts, report)).text
      assert(text.contains(s"paired n = ${paired.pairedN}"), text)
  }

  private final case class MethodsHarness(composer: FigureComposer, model: AppModel)

  private def update(
      h: MethodsHarness,
      intent: ComposerIntent
  ): (MethodsHarness, Vector[ComposerEffect]) =
    val (next, effects) = FigureComposer.update(h.composer, h.model, intent)
    val model           = effects.foldLeft(h.model) { (m, e) =>
      e match
        case ComposerEffect.App(i) => AppModel.update(m, i)._1
        case _                     => m
    }
    (MethodsHarness(FigureComposer.sync(next, model)._1, model), effects)

  private def composer(
      facts: MethodsFacts,
      summary: ResultSummary,
      report: ReportView
  ): MethodsHarness =
    Vector(
      ComposerIntent.SummaryRead(run7, SummaryAnswer.Answered(summary)),
      ComposerIntent.ReportRead(
        run7,
        source.reporting,
        ok(FigureComposer.reportingScale(source)),
        Right(report)
      ),
      ComposerIntent.Methods(MethodsIntent.FactsRead(run7, Right(facts)))
    ).foldLeft(MethodsHarness(FigureComposer.sync(FigureComposer.empty, t2)._1, t2))((c, i) =>
      update(c, i)._1
    )

  private def methods(c: MethodsHarness): MethodsVM =
    FigureComposer.view(c.composer, c.model).methods.getOrElse(fail("no methods pane"))

  private def act(c: MethodsHarness, i: MethodsIntent): MethodsHarness =
    update(c, ComposerIntent.Methods(i))._1

  private val Replay = "D measures spatial correspondence, not sequential replay."
  private val Edited = "D measures where gaze went, not the order it went there."

  test("the pane asks for the run's facts and shows the generated text") {
    read.map { (summary, facts, report) =>
      val (_, asked) = FigureComposer.sync(FigureComposer.empty, t2)
      assert(asked.contains(ComposerEffect.RequestMethods(run7, r3)), asked)
      val waiting =
        methods(MethodsHarness(FigureComposer.sync(FigureComposer.empty, t2)._1, t2))
      assertEquals(waiting.text, Left("Reading run 7's results for the methods…"))
      val vm = methods(composer(facts, summary, report))
      assertEquals(vm.text, Right(Golden))
      assertEquals(
        vm.heading,
        "Generated from run 7 and reporting spec “By retrieval response”"
      )
      assert(vm.diff.forall(_.isInstanceOf[DiffLine.Same]))
    }
  }

  test("editing one sentence shows it in the diff against the generated text") {
    read.map { (summary, facts, report) =>
      val c = act(
        composer(facts, summary, report),
        MethodsIntent.Edit(Golden.replace(Replay, Edited))
      )
      // Show diff brings the "Diff vs generated" pane forward.
      assertEquals(
        update(c, ComposerIntent.Methods(MethodsIntent.ShowDiff))._2,
        Vector(ComposerEffect.App(Intent.FocusPane(StudioLayouts.methodsDiff)))
      )
      val vm = methods(c)
      assertEquals(vm.text, Right(Golden.replace(Replay, Edited)))
      assertEquals(
        vm.heading,
        "Generated from run 7 and reporting spec “By retrieval response” · 1 sentence edited by you"
      )
      assertEquals(vm.diffCaption, "Generated text → your text")
      assertEquals(
        vm.diff.filterNot(_.isInstanceOf[DiffLine.Same]),
        Vector(DiffLine.Removed(Replay), DiffLine.Added(Edited))
      )
      // Editing back to the generated text leaves nothing edited.
      val back = methods(act(c, MethodsIntent.Edit(Golden)))
      assertEquals(
        back.heading,
        "Generated from run 7 and reporting spec “By retrieval response”"
      )
    }
  }

  test("methods stay unavailable until all facts arrive, in every callback order") {
    read.map { (summary, facts, report) =>
      val replies = Vector(
        ComposerIntent.SummaryRead(run7, SummaryAnswer.Answered(summary)),
        ComposerIntent.ReportRead(
          run7,
          source.reporting,
          ok(FigureComposer.reportingScale(source)),
          Right(report)
        ),
        ComposerIntent.Methods(MethodsIntent.FactsRead(run7, Right(facts)))
      )
      replies.permutations.foreach { order =>
        var state = MethodsHarness(FigureComposer.sync(FigureComposer.empty, t2)._1, t2)
        order.zipWithIndex.foreach { (reply, index) =>
          assertEquals(methods(state).text, Left(MethodsCopy.reading(run7)))
          state = update(state, reply)._1
          if index < order.size - 1 then
            assertEquals(methods(state).text, Left(MethodsCopy.reading(run7)))
        }
        assertEquals(methods(state).text, Right(Golden))
        val authored = Golden.replace(Replay, Edited)
        state = act(state, MethodsIntent.Edit(authored))
        order.foreach { reply =>
          state = update(state, reply)._1
          assertEquals(methods(state).text, Right(authored))
        }
      }
    }
  }

  test("late refused and recovered facts never replace an existing methods edit") {
    read.map { (summary, facts, report) =>
      val authored = Golden.replace(Replay, Edited)
      val edited   = act(composer(facts, summary, report), MethodsIntent.Edit(authored))
      val refused  = act(edited, MethodsIntent.FactsRead(run7, Left("Reading interrupted")))
      assertEquals(methods(refused).text, Right(authored))
      val continuedText = authored + " The authors checked these settings."
      val continued     = act(refused, MethodsIntent.Edit(continuedText))
      assertEquals(methods(continued).text, Right(continuedText))
      val newer     = facts.copy(failures = Vector("study-failure.empty-map" -> 3))
      val recovered = act(continued, MethodsIntent.FactsRead(run7, Right(newer)))
      assertEquals(methods(recovered).text, Right(continuedText))
      val regenerate = methods(act(recovered, MethodsIntent.Regenerate))
      assertEquals(regenerate.text, Right(continuedText))
      assert(regenerate.choice.isDefined)
    }
  }

  test("a regenerate attempted while loading stops saying reading once all callbacks arrive") {
    read.map { (summary, facts, report) =>
      val replies = Vector(
        ComposerIntent.SummaryRead(run7, SummaryAnswer.Answered(summary)),
        ComposerIntent.ReportRead(
          run7,
          source.reporting,
          ok(FigureComposer.reportingScale(source)),
          Right(report)
        ),
        ComposerIntent.Methods(MethodsIntent.FactsRead(run7, Right(facts)))
      )
      replies.permutations.foreach { order =>
        val initial = MethodsHarness(FigureComposer.sync(FigureComposer.empty, t2)._1, t2)
        val waiting = act(initial, MethodsIntent.Regenerate)
        assertEquals(methods(waiting).status, Some(MethodsCopy.reading(run7)))
        val ready =
          order.foldLeft(waiting)((state, reply) => update(state, reply)._1)
        assertEquals(methods(ready).text, Right(Golden))
        assertEquals(methods(ready).status, None)
      }
    }
  }

  test("regenerating over edits shows a diff and waits; it never discards them") {
    read.map { (summary, facts, report) =>
      val edited =
        act(
          composer(facts, summary, report),
          MethodsIntent.Edit(Golden.replace(Replay, Edited))
        )
      // Nothing new to generate: the edits stay, and the author is told.
      val same = act(edited, MethodsIntent.Regenerate)
      assertEquals(methods(same).text, Right(Golden.replace(Replay, Edited)))
      assertEquals(
        methods(same).status,
        Some("The generated text has not changed; your edits are kept.")
      )
      // New facts (one more failure): the edited text stays and the new text waits.
      val newer = facts.copy(failures = Vector("study-failure.empty-map" -> 3))
      val moved = act(edited, MethodsIntent.FactsRead(run7, Right(newer)))
      assert(methods(moved).heading.endsWith("regenerate to compare with new generated text"))
      val (waits, shown) =
        update(moved, ComposerIntent.Methods(MethodsIntent.Regenerate))
      assertEquals(
        shown.collect { case e @ ComposerEffect.App(_: Intent.FocusPane) => e },
        Vector(ComposerEffect.App(Intent.FocusPane(StudioLayouts.methodsDiff)))
      )
      val asked = methods(waits)
      assertEquals(asked.text, Right(Golden.replace(Replay, Edited)))
      assertEquals(asked.choice, Some(("Keep my edits", "Use the generated text")))
      assertEquals(asked.diffCaption, "Your text → newly generated text")
      val changed = asked.diff.filterNot(_.isInstanceOf[DiffLine.Same])
      assertEquals(
        changed.collect { case DiffLine.Removed(s) => s.take(30) },
        Vector("A contrast required all of its", Edited.take(30))
      )
      assertEquals(
        changed.collect { case DiffLine.Added(s) => s.take(30) },
        Vector("A contrast required all of its", Replay.take(30))
      )
      // Keep: the edits stay, now against the new generated text, which the
      // author's text no longer follows in two sentences.
      val kept = methods(act(act(moved, MethodsIntent.Regenerate), MethodsIntent.KeepEdits))
      assertEquals(kept.text, Right(Golden.replace(Replay, Edited)))
      assertEquals(kept.choice, None)
      assertEquals(
        kept.status,
        Some("Your edits are kept; the diff is now against the new generated text.")
      )
      assert(kept.heading.endsWith("· 2 sentences edited by you"), kept.heading)
      assertEquals(
        kept.diff.filterNot(_.isInstanceOf[DiffLine.Same]).map {
          case DiffLine.Removed(s) => "-" + s.take(30)
          case DiffLine.Added(s)   => "+" + s.take(30)
          case DiffLine.Same(s)    => s
        },
        Vector(
          "-A contrast required all of its",
          "+A contrast required all of its",
          "-" + Replay.take(30),
          "+" + Edited.take(30)
        )
      )
      // Use the generated text: only by the author's choice.
      val used = methods(act(act(moved, MethodsIntent.Regenerate), MethodsIntent.UseGenerated))
      assert(used.text.exists(t => t.contains("3 failed (empty-map)") && t.contains(Replay)))
      assertEquals(used.status, Some("Your edits were replaced by the generated text."))
    }
  }

  test("an unedited text follows its facts and regenerates without asking") {
    read.map { (summary, facts, report) =>
      val newer = facts.copy(failures = Vector("study-failure.empty-map" -> 3))
      val c = act(composer(facts, summary, report), MethodsIntent.FactsRead(run7, Right(newer)))
      val vm = methods(act(c, MethodsIntent.Regenerate))
      assert(vm.text.exists(_.contains("3 failed (empty-map)")))
      assertEquals(vm.choice, None)
      assertEquals(
        vm.status,
        Some("Regenerated from run 7 and reporting spec “By retrieval response”.")
      )
    }
  }

  private def reopened(h: MethodsHarness): MethodsHarness =
    val codec    = ok(StudioDocument.codec)
    val document = ok(codec.decode(ok(codec.encode(h.model.document))))
    val fresh    = AppModel
      .update(AppModel.open(document, h.model.project), Intent.Navigate(h.model.location))
      ._1
    MethodsHarness(FigureComposer.sync(FigureComposer.empty, fresh)._1, fresh)

  test("authored methods and exact base survive document save/reopen before callbacks arrive") {
    read.map { (summary, facts, report) =>
      val authored = "  Authored µ text.\n\nKeep exact spacing.  "
      val edited   = act(composer(facts, summary, report), MethodsIntent.Edit(authored))
      val stored   = edited.model.document.figures.head.methods.get
      assertEquals(stored.base, Golden)
      assertEquals(stored.edited, authored)
      assertEquals(stored.pending, None)
      val opened = reopened(edited)
      assertEquals(methods(opened).text, Right(authored))
      assert(methods(opened).diff.exists(_.isInstanceOf[DiffLine.Added]))
      val (refused, refusedEffects) = update(
        opened,
        ComposerIntent.Methods(MethodsIntent.FactsRead(run7, Left("temporary refusal")))
      )
      assertEquals(methods(refused).text, Right(authored))
      assert(!refusedEffects.exists {
        case ComposerEffect.App(_: Intent.Dispatch) => true; case _ => false
      })
      val recovered =
        update(refused, ComposerIntent.Methods(MethodsIntent.FactsRead(run7, Right(facts))))._1
      assertEquals(methods(recovered).text, Right(authored))
      assertEquals(recovered.model.document.figures.head.methods, Some(stored))
    }
  }

  test(
    "methods undo and redo hydrate document authority rather than retaining stale session edits"
  ) {
    read.map { (summary, facts, report) =>
      val c        = composer(facts, summary, report)
      val authored = Golden.replace(Replay, Edited)
      val edited   = act(c, MethodsIntent.Edit(authored))
      assert(edited.model.document.figures.head.methods.isDefined)
      val undoneModel = AppModel.update(edited.model, Intent.Undo(HistoryStack.Science))._1
      val undone      =
        MethodsHarness(FigureComposer.sync(edited.composer, undoneModel)._1, undoneModel)
      assertEquals(methods(undone).text, Right(Golden))
      assertEquals(undone.model.document.figures.head.methods, None)
      val redoneModel = AppModel.update(undone.model, Intent.Redo(HistoryStack.Science))._1
      val redone      =
        MethodsHarness(FigureComposer.sync(undone.composer, redoneModel)._1, redoneModel)
      assertEquals(methods(redone).text, Right(authored))
      assertEquals(
        redone.model.document.figures.head.methods,
        edited.model.document.figures.head.methods
      )
    }
  }

  test(
    "pending regeneration survives reopen and UseGenerated accepts exactly the offered text"
  ) {
    read.map { (summary, facts, report) =>
      val edited = act(
        composer(facts, summary, report),
        MethodsIntent.Edit(Golden.replace(Replay, Edited))
      )
      val newer   = facts.copy(failures = Vector("study-failure.empty-map" -> 3))
      val changed = act(edited, MethodsIntent.FactsRead(run7, Right(newer)))
      val pending = act(changed, MethodsIntent.Regenerate)
      val offered = pending.model.document.figures.head.methods.get.pending.get
      val opened  = reopened(pending)
      assertEquals(methods(opened).choice, Some(("Keep my edits", "Use the generated text")))
      val accepted = act(opened, MethodsIntent.UseGenerated)
      assertEquals(methods(accepted).text, Right(offered))
      val different =
        update(accepted, ComposerIntent.Methods(MethodsIntent.FactsRead(run7, Right(facts))))._1
      assertEquals(methods(different).text, Right(offered))
      assertEquals(
        reopened(different).model.document.figures.head.methods.map(_.edited),
        Some(offered)
      )
    }
  }

  test(
    "separate figure drafts hydrate independently and delete/restore does not resurrect a discarded cache"
  ) {
    read.map { (summary, facts, report) =>
      val c           = act(composer(facts, summary, report), MethodsIntent.Edit("Figure one."))
      val second      = c.model.document.figures(1)
      val secondDraft =
        ok(FigureMethodsDraft.of("Second base.", "Figure two.", Some("Second pending.")))
      val storedModel = AppModel
        .update(
          c.model,
          Intent.Dispatch(Command.SetFigureMethods(second.id, Some(secondDraft)))
        )
        ._1
      val withBoth = FigureComposer.sync(c.composer, storedModel)._1
      assertEquals(withBoth.methods.drafts(second.id).edited, "Figure two.")
      assertEquals(
        withBoth.methods.drafts(c.model.document.figures.head.id).edited,
        "Figure one."
      )
      val deletedModel =
        AppModel.update(storedModel, Intent.Dispatch(Command.DeleteFigure(second.id)))._1
      val deleted = FigureComposer.sync(withBoth, deletedModel)._1
      assert(!deleted.methods.drafts.contains(second.id))
      val restoredModel = AppModel.update(deletedModel, Intent.Undo(HistoryStack.Science))._1
      assertEquals(
        FigureComposer.sync(deleted, restoredModel)._1.methods.drafts(second.id).pending,
        Some("Second pending.")
      )
    }
  }

  test("sentences split at sentence ends only, and a rewrite counts once") {
    assertEquals(
      TextDiff.sentences("Cell 0.46°, σ = 0.5°. D = M − B. (P17) held 2. 18 left."),
      Vector("Cell 0.46°, σ = 0.5°.", "D = M − B.", "(P17) held 2.", "18 left.")
    )
    val diff = TextDiff("A one. B two. C three.", "A one. B 2. New. C three.")
    assertEquals(TextDiff.changed(diff), 2)
    assertEquals(TextDiff.changed(TextDiff("A one. B two.", "A one. B 2.")), 1)
    assertEquals(TextDiff.changed(TextDiff("A one.", "A one.")), 0)
  }
