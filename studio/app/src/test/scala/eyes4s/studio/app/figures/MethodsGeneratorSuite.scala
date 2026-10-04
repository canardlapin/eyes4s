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
import eyes4s.studio.core.backend.{PageRequest, QueryStatus, ResultSummary}
import eyes4s.studio.core.document.FigureId
import eyes4s.studio.core.figures.{FigureSource, MethodsFacts, MethodsReads}
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
  private def read: Future[(ResultSummary, MethodsFacts)] =
    for
      session <- HeadlessSession.open(StoryMoment.T2)
      summary <- session.result(run7)
      facts   <- MethodsReads.read[Future](session.admission, session.queries, run7, r3)
      _       <- session.close
    yield (ok(summary), ok(facts))

  private def generated: Future[GeneratedMethods] =
    read.map((s, f) => ok(MethodsText.generate(source, s, f)))

  /** The generated methods.md of Figure 1 at t2: the fixture's golden text. */
  private val Golden: String =
    "Fixations (fixations.csv, 11,520 records; dataset r3) were admitted per trial against " +
      "the trials.csv inventory of 960 trials: 937 admitted, 17 quarantined (overlap 6, " +
      "no-fixations 5, duplicate-ordinals 4, rejected-records 2) and 6 absent (no fixation " +
      "records). Positions were analysed in the 1024 × 768 image frame placed in a 1920 × " +
      "1080 screen; 543 of the 11,311 fixation records of admitted trials (4.8%), in 409 " +
      "trials, fell outside the frame and were excluded from the maps; they are reported per " +
      "trial. No fixation record lay outside the screen (off-screen policy: exclude the " +
      "record). Degrees are measured from the image centre (x right, y up) using a declared, " +
      "uncalibrated 35 px/° with linear conversion. The initial-fixation policy was Keep all: " +
      "every fixation, including the first, was kept. Each trial was represented as a " +
      "duration-weighted fixation density map on a 64 × 48 grid (cell 0.46°), smoothed with " +
      "Gaussian kernels at σ = 0.5°, 1°, 2° and 4°, declared before the run. Of 480 retrieval " +
      "queries, 457 were eligible; for each eligible query, cosine similarity was computed to " +
      "the one matched encoding trial (M) and to every admitted encoding trial of another " +
      "item from the same participant (19 per query; 18 for 171 queries); their mean is B, " +
      "and D = M − B. A contrast required all of its pairs: 454 contributed, 3 failed " +
      "(off-window), 9 had no admitted matched trial and 14 queries were not admitted. D was " +
      "averaged within participant, then across participants with equal weight, separately " +
      "by retrieval response (n = 24 each; paired n = 24). Per participant, groups held 2–17 " +
      "queries; no minimum per group was applied in this reporting spec (P17 and P21 each " +
      "have 2 Forgotten queries). D measures spatial correspondence, not sequential replay. D " +
      "does not separate participant-specific reinstatement from item-driven salience common " +
      "to all viewers of that image, and may retain residual centre bias. Analysis rev 4, run " +
      "7; eyes4s 0.1."

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
    read.map { (summary, facts) =>
      val before = ok(MethodsText.generate(source, summary, facts)).text
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
          )
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

  test("facts of another run or dataset are refused, naming both") {
    read.map { (summary, facts) =>
      val run5 = StoryMoments.run5
      assertEquals(
        MethodsText.generate(source, summary.copy(run = run5), facts).left.map(_.message),
        Left("The methods of run 7 were given the result summary of run 5.")
      )
      assertEquals(
        MethodsText.generate(source, summary, facts.copy(run = run5)).left.map(_.message),
        Left("The methods of run 7 were given the query facts of run 5.")
      )
      assertEquals(
        MethodsText
          .generate(
            source,
            summary,
            facts.copy(admission = facts.admission.copy(dataset = StoryMoments.r2))
          )
          .left
          .map(_.message),
        Left("The methods of dataset r3 were given the admission of r2.")
      )
    }
  }

  test("the facts are read page by page and tallied") {
    read.map { (_, facts) =>
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

  private def composer(facts: MethodsFacts, summary: ResultSummary): FigureComposer =
    Vector(
      ComposerIntent.SummaryRead(run7, SummaryAnswer.Answered(summary)),
      ComposerIntent.Methods(MethodsIntent.FactsRead(run7, Right(facts)))
    ).foldLeft(FigureComposer.sync(FigureComposer.empty, t2)._1)((c, i) =>
      FigureComposer.update(c, t2, i)._1
    )

  private def methods(c: FigureComposer): MethodsVM =
    FigureComposer.view(c, t2).methods.getOrElse(fail("no methods pane"))

  private def act(c: FigureComposer, i: MethodsIntent): FigureComposer =
    FigureComposer.update(c, t2, ComposerIntent.Methods(i))._1

  private val Replay = "D measures spatial correspondence, not sequential replay."
  private val Edited = "D measures where gaze went, not the order it went there."

  test("the pane asks for the run's facts and shows the generated text") {
    read.map { (summary, facts) =>
      val (_, asked) = FigureComposer.sync(FigureComposer.empty, t2)
      assert(asked.contains(ComposerEffect.RequestMethods(run7, r3)), asked)
      val waiting = methods(FigureComposer.sync(FigureComposer.empty, t2)._1)
      assertEquals(waiting.text, Left("Reading run 7's results for the methods…"))
      val vm = methods(composer(facts, summary))
      assertEquals(vm.text, Right(Golden))
      assertEquals(
        vm.heading,
        "Generated from run 7 and reporting spec “By retrieval response”"
      )
      assert(vm.diff.forall(_.isInstanceOf[DiffLine.Same]))
    }
  }

  test("editing one sentence shows it in the diff against the generated text") {
    read.map { (summary, facts) =>
      val c = act(composer(facts, summary), MethodsIntent.Edit(Golden.replace(Replay, Edited)))
      // Show diff brings the "Diff vs generated" pane forward.
      assertEquals(
        FigureComposer.update(c, t2, ComposerIntent.Methods(MethodsIntent.ShowDiff))._2,
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

  test("regenerating over edits shows a diff and waits; it never discards them") {
    read.map { (summary, facts) =>
      val edited =
        act(composer(facts, summary), MethodsIntent.Edit(Golden.replace(Replay, Edited)))
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
        FigureComposer.update(moved, t2, ComposerIntent.Methods(MethodsIntent.Regenerate))
      assertEquals(
        shown,
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
    read.map { (summary, facts) =>
      val newer = facts.copy(failures = Vector("study-failure.empty-map" -> 3))
      val c     = act(composer(facts, summary), MethodsIntent.FactsRead(run7, Right(newer)))
      val vm    = methods(act(c, MethodsIntent.Regenerate))
      assert(vm.text.exists(_.contains("3 failed (empty-map)")))
      assertEquals(vm.choice, None)
      assertEquals(
        vm.status,
        Some("Regenerated from run 7 and reporting spec “By retrieval response”.")
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
