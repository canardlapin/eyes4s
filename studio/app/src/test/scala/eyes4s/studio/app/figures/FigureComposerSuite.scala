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
import eyes4s.studio.app.explore.DisplaySource
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.figures.{PairScore, ReferenceReads, ReferenceScores}
import eyes4s.studio.core.fixture.{GoldenAssets, MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import scala.concurrent.{ExecutionContext, Future}

/** The figure composer, headless (ticket S9.2a; Figures.dc.html, page), at
  * story moment t2 on the fake backend: Figure 1 binds run 7 with panels
  * A–E, Figure 2 binds run 5.
  */
class FigureComposerSuite extends munit.ScalaCheckSuite:
  import StoryMoments.{r3, run5, run7}

  private given ExecutionContext = ExecutionContext.global

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val figure1           = ok(FigureId.of(1))
  private val figure2           = ok(FigureId.of(2))
  private def letter(l: String) = ok(PanelLetter.of(l))
  private val sigma2            = ok(Sigma.of(2.0))
  private val scale2            = ok(ScaleIndex.of(2))
  private val p17ret07          = MockStudy.key("P17", "ret_07")

  private def t2: AppModel = StoryModels.t2Figures

  /** The model navigated to `figure`'s `panel`. */
  private def at(model: AppModel, figure: FigureId, panel: Option[String]): AppModel =
    AppModel
      .update(
        model,
        Intent.Navigate(
          Location(
            Perspective.Figures,
            Vector(Place.Figures, Place.Figure(figure)) ++
              panel.map(l => Place.At(StudioRef.FigurePanel(figure, letter(l))))
          )
        )
      )
      ._1

  /** The fake backend's answers for t2's Figure 1: run 7's summary and
    * P17 ret_07's reference scores at σ 2°.
    */
  private def served: Future[(ResultSummary, ReferenceScores)] =
    for
      session <- HeadlessSession.open(StoryMoment.T2)
      summary <- session.result(run7)
      scores  <- ReferenceReads.read[Future](
        session.inspect(run7, _),
        session.navigator.pairs,
        run7,
        scale2,
        p17ret07
      )
      _ <- session.close
    yield (ok(summary), ok(scores))

  /** The composer synced to `model` with the fake's answers read. */
  private def loaded(
      model: AppModel,
      answers: (ResultSummary, ReferenceScores)
  ): FigureComposer =
    val (summary, scores) = answers
    val synced            = FigureComposer.sync(FigureComposer.empty, model)._1
    val registry          = model.document.dataset(r3).map(GoldenAssets.registry).map {
      case Right(r)  => Right(DisplaySource.Served(r))
      case Left(why) => Left(why)
    }
    Vector(
      ComposerIntent.SummaryRead(run7, SummaryAnswer.Answered(summary)),
      ComposerIntent.ReferencesRead(run7, scale2, p17ret07, Right(scores)),
      ComposerIntent.DisplaysRead(r3, registry.getOrElse(Left("no r3")))
    ).foldLeft(synced)((c, i) => FigureComposer.update(c, model, i)._1)

  private def panel(vm: ComposerVM, l: String): PanelVM =
    vm.page.flatMap(_.panels.find(_.letter == letter(l))).getOrElse(fail(s"no panel $l"))

  // --- Templates and reads ----------------------------------------------------------

  test("Figure 1's panels are the board's templates A–E") {
    val panels = t2.document.figures.head.panels
    assertEquals(
      panels.map(PanelTemplate.of),
      Vector(
        PanelTemplate.Gaze(MockStudy.key("P17", "enc_03")),
        PanelTemplate.Gaze(p17ret07),
        PanelTemplate.DensityMaps(sigma2, p17ret07),
        PanelTemplate.ParticipantD(sigma2),
        PanelTemplate.ScaleProfile
      )
    )
  }

  test(
    "the composer reads the bound run once: its summary, its methods facts, panel C's " +
      "references, the displays"
  ) {
    val (synced, effects) = FigureComposer.sync(FigureComposer.empty, t2)
    assertEquals(
      effects,
      Vector(
        ComposerEffect.RequestSummary(run7),
        ComposerEffect.RequestMethods(run7, r3),
        ComposerEffect.RequestDisplays(t2.document.dataset(r3).get),
        ComposerEffect.RequestReferences(run7, scale2, p17ret07)
      )
    )
    assertEquals(FigureComposer.sync(synced, t2)._2, Vector.empty)
    // Figure 2 binds run 5: its reads are of run 5, whatever run is shown.
    val two = at(t2, figure2, None)
    assertEquals(two.document.presentation.shownRun, Some(run7))
    val (_, second) = FigureComposer.sync(synced, two)
    assert(second.contains(ComposerEffect.RequestSummary(run5)), second)
    assert(second.contains(ComposerEffect.RequestMethods(run5, StoryMoments.r2)), second)
    assert(!second.contains(ComposerEffect.RequestSummary(run7)), second)
    // The binding follows the shown figure: stale Figure 2 asks for r2 → r3.
    assert(
      second.contains(
        ComposerEffect.Binding(FigureEffect.RequestStatus(StoryMoments.r2, r3))
      ),
      second
    )
  }

  // --- Panel C: never a single control score where it could be read as B ---------

  test("panel C labels the matched pair, the highest of 19 controls and the control mean B") {
    served.map { answers =>
      val vm = FigureComposer.view(loaded(t2, answers), t2)
      val c  = panel(vm, "C").body match
        case PanelBody.Maps(maps) => maps
        case other                => fail(other.toString)
      assertEquals(
        c.tiles.map(t => (t.title, t.label)),
        Vector(
          "Query ret_07"                        -> "P17 · σ 2°",
          "Matched enc_03"                      -> "0.73",
          "Highest of 19 controls · street-112" -> "0.61"
        )
      )
      assertEquals(
        c.caption,
        "Matched 0.73 · highest of 19 controls street-112 0.61 · control mean B 0.35 · D +0.38"
      )
      // B is said only as the control mean; the control's score only with its rank.
      val said = c.tiles.flatMap(t => Vector(t.title, t.label)) :+ c.caption
      assert(said.forall(s => !s.contains("0.35") || s.contains("control mean B 0.35")), said)
      assert(c.tiles.forall(t => !t.label.contains("0.35")))
    }
  }

  property(
    "panel C: a control's score is always shown with its rank, B only as the control mean"
  ) {
    val pair = for
      trial <- Gen.choose(1, 40).map(i => MockStudy.key("P01", f"enc_$i%02d"))
      item  <- Gen.alphaLowerStr.map(s => "item-" + s.take(6))
      score <- Gen.choose(-1.0, 1.0)
    yield PairScore(trial, item, score)
    val scores = for
      m        <- Gen.choose(-1.0, 1.0)
      b        <- Gen.choose(-1.0, 1.0)
      matched  <- pair
      controls <- Gen.listOf(pair).map(_.toVector.distinctBy(_.reference))
      members  <- Gen.choose(controls.size, controls.size + 5)
    yield ReferenceScores(m, b, m - b, members, matched, controls)
    forAll(scores) { s =>
      val vm      = FigurePanels.densityMaps(run7, scale2, sigma2, p17ret07, s)
      val control = vm.tiles.filter(_.ref match
        case StudioRef.Pair(_, _, PairDesign.Control, _, _) => true
        case _                                              => false)
      assert(control.size <= 1)
      // The rank says how many were scored when the backend scored fewer than B is over.
      val rank =
        if s.controls.size == s.controlMembers then s"highest of ${s.controlMembers} controls"
        else s"highest of ${s.controls.size} scored of ${s.controlMembers} controls"
      assert(control.forall(_.title.startsWith(rank.capitalize)), control)
      // No tile is the control mean; B is in the caption, named.
      assert(vm.tiles.forall(t => !t.title.toLowerCase.contains("mean")), vm.tiles)
      assert(
        vm.caption.contains(s"control mean B ${eyes4s.studio.app.text.Format.decimal(s.b, 2)}")
      )
      val highest = s.controls.sortBy(c => (-c.score, c.reference.trial)).headOption
      highest.foreach(h =>
        assert(
          vm.caption.contains(
            s"$rank ${h.item} " +
              eyes4s.studio.app.text.Format.decimal(h.score, 2)
          ),
          vm.caption
        )
      )
    }
  }

  test("at a scale whose control pairs the backend does not score, panel C says so") {
    val scale0 = ok(ScaleIndex.of(0))
    for
      session <- HeadlessSession.open(StoryMoment.T2)
      scores  <- ReferenceReads.read[Future](
        session.inspect(run7, _),
        session.navigator.pairs,
        run7,
        scale0,
        p17ret07
      )
      _ <- session.close
    yield
      val read = ok(scores)
      assertEquals((read.controls, read.controlMembers), (Vector.empty, 19))
      val vm = FigurePanels.densityMaps(run7, scale0, ok(Sigma.of(0.5)), p17ret07, read)
      assertEquals(vm.tiles.size, 2)
      assert(vm.caption.contains("no control pair score served of 19 controls"), vm.caption)
  }

  // --- Panel D: all 24 participants and the per-group n range ---------------------

  test("panel D draws all 24 participants in each group and the per-group n range") {
    served.map { answers =>
      val vm = FigureComposer.view(loaded(t2, answers), t2)
      val d  = panel(vm, "D").body match
        case PanelBody.Plot(plot) => plot
        case other                => fail(other.toString)
      val participants = d.source.rows.collect { case PlotRowParticipant(group, p) =>
        (group, p)
      }
      assertEquals(
        participants.groupMap(_._1)(_._2).view.mapValues(_.distinct.size).toMap,
        Map("Remembered" -> 24, "Forgotten" -> 24)
      )
      assertEquals(
        d.notes,
        Vector(
          "n = 24 each · paired n = 24",
          "bars: grand mean of participant means, equal weight",
          "Each pair of dots is one participant; per participant, 2–17 queries per group. " +
            "Descriptive only: no intervals or tests."
        )
      )
      val e = panel(vm, "E").body match
        case PanelBody.Plot(plot) => plot
        case other                => fail(other.toString)
      assertEquals(e.notes.head, "n = 24 each · paired n = 24")
    }
  }

  /** A participant row of the participant plot's source: its group and participant. */
  private object PlotRowParticipant:
    def unapply(row: eyes4s.studio.app.plot.PlotRow): Option[(String, String)] = row.ref match
      case StudioRef.ParticipantSummary(_, _, _, Some(group), participant) =>
        Some((group.label, participant))
      case _ => None

  // --- Panels A and B --------------------------------------------------------------

  test("panels A and B say what the screen displayed, and that the gaze is not served") {
    served.map { answers =>
      val vm              = FigureComposer.view(loaded(t2, answers), t2)
      def gaze(l: String) = panel(vm, l).body match
        case PanelBody.Gaze(g) => g
        case other             => fail(other.toString)
      assertEquals(
        (gaze("A").heading, gaze("A").displayed),
        ("Encoding · enc_03 · beach-042", "Displayed: image.")
      )
      assertEquals(
        (gaze("B").heading, gaze("B").displayed),
        (
          "Retrieval · ret_07",
          "Displayed: blank + fixation cross. The remembered image was not shown."
        )
      )
      // Each empty drawing area names the follow-up that fills it.
      assertEquals(
        gaze("A").gaze,
        "No gaze yet: fixations are drawn when the backend serves the trial-fixations view (S6.2)."
      )
      panel(vm, "C").body match
        case PanelBody.Maps(maps) =>
          assertEquals(
            maps.maps,
            "No density maps yet: they are drawn when the backend serves density grids (UI-E). " +
              "The tiles show their scores."
          )
        case other => fail(other.toString)
      assertEquals(
        FigureComposer
          .view(loaded(at(t2, figure1, Some("A")), answers), at(t2, figure1, Some("A")))
          .page
          .flatMap(_.table),
        Some(
          Left(
            "Panel A has no table yet: its fixations are listed when the backend serves the " +
              "trial-fixations view (S6.2)."
          )
        )
      )
    }
  }

  // --- Page, zoom, Table tab ----------------------------------------------------------

  test("the page is 183 mm two-column at 100%; widths and zoom change it") {
    val c  = FigureComposer.sync(FigureComposer.empty, t2)._1
    val vm = FigureComposer.view(c, t2).page.get
    assertEquals(
      (vm.widthLabel, vm.zoom, vm.pxPerMm),
      ("Page 183 mm · two-column", "100% · 3.5 px/mm", 3.5)
    )
    assertEquals(vm.panels.map(_.widthMm).distinct, Vector(89))
    val one = FigureComposer.update(c, t2, ComposerIntent.SetWidth(PageWidth.SingleColumn))._1
    assertEquals(FigureComposer.view(one, t2).page.get.widthLabel, "Page 89 mm · one-column")
    val in = FigureComposer.update(c, t2, ComposerIntent.ZoomIn)._1
    assertEquals(FigureComposer.view(in, t2).page.get.zoom, "150% · 5.3 px/mm")
    val out =
      (1 to 5).foldLeft(c)((x, _) => FigureComposer.update(x, t2, ComposerIntent.ZoomOut)._1)
    assertEquals(FigureComposer.view(out, t2).page.get.zoom, "50% · 1.8 px/mm")
  }

  test("selecting a panel navigates the Figures trail; its Table tab shows its values") {
    served.map { answers =>
      val c = loaded(t2, answers)
      assertEquals(
        FigureComposer.update(c, t2, ComposerIntent.SelectPanel(figure1, letter("C")))._2,
        Vector(
          ComposerEffect.App(
            Intent.Navigate(
              Location(
                Perspective.Figures,
                Vector(
                  Place.Figures,
                  Place.Figure(figure1),
                  Place.At(StudioRef.FigurePanel(figure1, letter("C")))
                )
              )
            )
          )
        )
      )
      val onC   = at(t2, figure1, Some("C"))
      val table = FigureComposer.view(c, onC).page.flatMap(_.table)
      assert(table.exists(_.isRight), table)
      val rows = table.get.toOption.get.rows
      assertEquals(rows.size, 2 + answers._2.controls.size)
      assert(
        rows.exists(
          _.ref == StudioRef.Pair(
            run7,
            scale2,
            PairDesign.Control,
            p17ret07,
            MockStudy.key("P17", "enc_01")
          )
        )
      )
      // Panel D's table is the participant plot's values; panel A has none.
      assertEquals(
        FigureComposer
          .view(c, at(t2, figure1, Some("D")))
          .page
          .flatMap(_.table)
          .map(_.map(_.rows.size)),
        Some(Right(50))
      )
      assert(
        FigureComposer
          .view(c, at(t2, figure1, Some("A")))
          .page
          .flatMap(_.table)
          .exists(_.isLeft)
      )
    }
  }

  // --- New figure and Open in Compare -----------------------------------------------

  test("New figure binds the latest current run and the first reporting spec") {
    val (_, effects) = FigureComposer.update(FigureComposer.empty, t2, ComposerIntent.NewFigure)
    val rep          = t2.document.reporting.head.id
    val half         = ok(Sigma.of(0.5))
    val create       = Command.CreateFigure(
      run7,
      rep,
      Vector(
        PanelSpec(
          letter("A"),
          "Participant D, σ 0.5°",
          PanelScale.At(half),
          PanelSelection.AllQueries
        ),
        PanelSpec(letter("B"), "Scale profile", PanelScale.AllScales, PanelSelection.AllQueries)
      )
    )
    val figure3 = ok(FigureId.of(3))
    assertEquals(
      effects,
      Vector(
        ComposerEffect.App(Intent.Dispatch(create)),
        ComposerEffect.App(
          Intent.Navigate(
            Location(Perspective.Figures, Vector(Place.Figures, Place.Figure(figure3)))
          )
        )
      )
    )
    val made = effects.foldLeft(t2) {
      case (m, ComposerEffect.App(i)) => AppModel.update(m, i)._1
      case (m, _)                     => m
    }
    val page =
      FigureComposer.view(FigureComposer.sync(FigureComposer.empty, made)._1, made).page.get
    assertEquals(
      (page.title, page.panels.map(_.title)),
      ("Figure 3", Vector("Participant D, σ 0.5°", "Scale profile"))
    )
  }

  test("Open in Compare shows the figure's run, at the selected panel") {
    val onC = at(t2, figure1, Some("C"))
    assertEquals(
      FigureComposer.update(FigureComposer.empty, onC, ComposerIntent.OpenInCompare)._2,
      Vector(
        ComposerEffect.App(
          Intent.Explain(Place.At(StudioRef.QueryContrast(run7, scale2, p17ret07)))
        )
      )
    )
    // Figure 2 binds run 5, which is not shown: it is shown first.
    val onTwo = at(t2, figure2, Some("A"))
    val rep   = t2.document.figures(1).reporting
    val two   =
      FigureComposer.update(FigureComposer.empty, onTwo, ComposerIntent.OpenInCompare)._2
    assertEquals(two.head, ComposerEffect.App(Intent.Dispatch(Command.ShowRun(Some(run5)))))
    assertEquals(two.size, 2)
    assertEquals(
      FigureComposer
        .update(FigureComposer.empty, at(t2, figure1, Some("D")), ComposerIntent.OpenInCompare)
        ._2,
      Vector(
        ComposerEffect.App(
          Intent.Navigate(
            Location(
              Perspective.Compare,
              Vector(Place.Summary(t2.document.figures.head.reporting))
            )
          )
        )
      )
    )
    assert(rep.value.nonEmpty)
  }

  // --- Review follow-ups -------------------------------------------------------------

  test("a panel selected in a figure no longer shown is not the shown figure's panel") {
    // Figure 2 is shown with its panel A selected; deleting it falls back to Figure 1.
    val onTwo   = at(t2, figure2, Some("A"))
    val deleted = AppModel.update(onTwo, Intent.Dispatch(Command.DeleteFigure(figure2)))._1
    assertEquals(FigureComposer.shownFigure(deleted), Some(figure1))
    assertEquals(FigureComposer.shownPanel(deleted), None)
    val page = FigureComposer
      .view(FigureComposer.sync(FigureComposer.empty, deleted)._1, deleted)
      .page
      .get
    assertEquals(page.panels.filter(_.selected), Vector.empty)
  }

  test("a failed read is asked again at the next sync; a failed display read says why") {
    val (synced, _) = FigureComposer.sync(FigureComposer.empty, t2)
    val failed      = FigureComposer
      .update(
        synced,
        t2,
        ComposerIntent.SummaryRead(run7, SummaryAnswer.Failed("the backend timed out"))
      )
      ._1
    assertEquals(
      FigureComposer.view(failed, t2).page.get.panels.find(_.letter == letter("D")).map(_.body),
      Some(PanelBody.Unavailable("the backend timed out"))
    )
    assert(FigureComposer.sync(failed, t2)._2.contains(ComposerEffect.RequestSummary(run7)))
    val unread = FigureComposer
      .update(synced, t2, ComposerIntent.DisplaysRead(r3, Left("disk unreadable")))
      ._1
    panel(FigureComposer.view(unread, t2), "B").body match
      case PanelBody.Gaze(g) =>
        assertEquals(
          g.displayed,
          "What the screen displayed could not be read: disk unreadable"
        )
      case other => fail(other.toString)
  }

  test("a narrower page clamps every panel width to itself") {
    val c    = FigureComposer.sync(FigureComposer.empty, onD)._1
    val wide = FigureComposer.update(c, onD, ComposerIntent.SetPanelWidth(letter("D"), 150))._1
    assertEquals(
      FigureComposer.view(wide, onD).page.get.appearance.panelWidthMm.map(_._2),
      Some(150)
    )
    val single =
      FigureComposer.update(wide, onD, ComposerIntent.SetWidth(PageWidth.SingleColumn))._1
    assertEquals(
      FigureComposer.view(single, onD).page.get.appearance.panelWidthMm.map(_._2),
      Some(89)
    )
    assertEquals(single.appearanceOf(figure1).widthsMm.get(letter("D")), Some(89))
  }

  private def onD: AppModel = at(t2, figure1, Some("D"))

  // --- Export (S9.3) -------------------------------------------------------------------

  test("export: choose a format, then Export asks for the page as a file") {
    val c0    = FigureComposer.sync(FigureComposer.empty, t2)._1
    val page0 = FigureComposer.view(c0, t2).page.get
    assertEquals(
      page0.exporting.formats.map((_, l, chosen) => (l, chosen)),
      Vector(("SVG", true), ("PDF", false), ("PNG", false))
    )
    val c1 = FigureComposer.update(c0, t2, ComposerIntent.ChooseFormat(ExportFormat.Pdf))._1
    val (c2, asked) = FigureComposer.update(c1, t2, ComposerIntent.Export)
    val page        = FigureComposer.view(c1, t2).page.get
    assertEquals(
      asked,
      Vector(ComposerEffect.ExportFigure(ExportFormat.Pdf, page, "figure-1.pdf"))
    )
    val done =
      FigureComposer.update(c2, t2, ComposerIntent.Exported(Right("/tmp/figure-1.pdf")))._1
    assertEquals(
      FigureComposer.view(done, t2).page.get.exporting.status,
      Some("Exported to /tmp/figure-1.pdf.")
    )
    val failed =
      FigureComposer.update(c2, t2, ComposerIntent.Exported(Left("no file was chosen")))._1
    assertEquals(
      FigureComposer.view(failed, t2).page.get.exporting.status,
      Some("The figure was not exported: no file was chosen")
    )
  }

  test("the greyscale check is a view of the page; it changes nothing exported") {
    val c0 = FigureComposer.sync(FigureComposer.empty, t2)._1
    val on = FigureComposer.update(c0, t2, ComposerIntent.SetGreyscale(true))._1
    assertEquals(FigureComposer.view(on, t2).page.map(_.greyscale), Some(true))
    val page = FigureComposer.view(on, t2).page.get
    assertEquals(page.copy(greyscale = false), FigureComposer.view(c0, t2).page.get)
  }
