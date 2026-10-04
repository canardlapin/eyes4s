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

package eyes4s.studio.desktop.figures

import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.figures.{ComposerIntent, FigureIntent, PageWidth, PanelBody}
import eyes4s.studio.core.document.{FigureId, PanelLetter}
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.FxStage
import eyes4s.studio.desktop.plot.PlotTwinStatus
import eyes4s.studio.desktop.shell.ShellFxSuite
import javafx.scene.Node
import javafx.scene.control.Label
import javafx.scene.layout.VBox

import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

/** The Figures perspective in the studio window (ticket S9.2a;
  * Figures.dc.html, page) at story moment t2 on the fake backend: Figure 1
  * binds run 7 with panels A–E, Figure 2 binds run 5.
  */
class FigureComposerFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(180, "s")

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private val figure1                      = ok(FigureId.of(1))
  private val figure2                      = ok(FigureId.of(2))
  private def letter(l: String)            = ok(PanelLetter.of(l))

  private def texts(node: Node): Vector[String] =
    runOnFx(node.lookupAll(".label").asScala.toVector.collect { case l: Label => l.getText })

  private def panelNode(w: StudioWindow, l: String): VBox =
    runOnFx(w.figures.paper.getChildren.asScala.collectFirst {
      case v: VBox if v.getId == s"figures-panel-$l" => v
    }).getOrElse(fail(s"no panel $l on the page"))

  private def loaded(fx: FxStage, w: StudioWindow): Unit =
    eventually(fx, "Figure 1's panels are read") {
      val page = w.figures.vm.page
      page.exists(_.panels.forall(p => !p.body.isInstanceOf[PanelBody.Waiting])) &&
      w.figures
        .plot(figure1, letter("D"))
        .exists(_.status.get.isInstanceOf[PlotTwinStatus.Shown])
    }

  fxStage.test("panel D draws all 24 participants in each group and the per-group n range") {
    fx =>
      val w = boot(fx, StoryModels.t2Figures, StoryMoment.T2)
      loaded(fx, w)
      val plot =
        runOnFx(w.figures.plot(figure1, letter("D")).flatMap(_.plot)).getOrElse(fail("no plot"))
      val marks        = plot.marks.map(_.ref)
      val participants = marks.collect {
        case StudioRef.ParticipantSummary(_, _, _, Some(g), p) => (g.label, p)
      }
      assertEquals(
        participants.groupMap(_._1)(_._2).view.mapValues(_.distinct.size).toMap,
        Map("Remembered" -> 24, "Forgotten" -> 24)
      )
      assertEquals(marks.count(_.isInstanceOf[StudioRef.GroupCell]), 2)
      val said = texts(panelNode(w, "D"))
      Vector("n = 24 each · paired n = 24", "per participant, 2–17 queries per group").foreach(
        t => assert(said.contains(t), s"'$t' not in panel D: $said")
      )
  }

  fxStage.test("panel C never places a single control score where it could be read as B") {
    fx =>
      val w = boot(fx, StoryModels.t2Figures, StoryMoment.T2)
      loaded(fx, w)
      val said = texts(panelNode(w, "C"))
      assert(said.contains("Highest of 19 controls · street-112"), said)
      assert(
        said.contains(
          "Matched 0.73 · highest of 19 controls street-112 0.61 · control mean B 0.35 · D +0.38"
        ),
        said
      )
      // B appears only as the control mean; the control's 0.61 only beside its rank.
      assert(said.filter(_.contains("0.35")).forall(_.contains("control mean B 0.35")), said)
      val tiles = runOnFx(
        panelNode(w, "C").lookupAll(".figures-tile").asScala.toVector.map(_.getAccessibleText)
      )
      assertEquals(
        tiles,
        Vector(
          "Query ret_07, P17 · σ 2°",
          "Matched enc_03, 0.73",
          "Highest of 19 controls · street-112, 0.61"
        )
      )
  }

  fxStage.test("the page, its widths and zoom, the Table tab and the panels A and B") { fx =>
    val w = boot(fx, StoryModels.t2Figures, StoryMoment.T2)
    loaded(fx, w)
    val page = texts(w.figures.pageNode)
    Vector("Figure 1", "Page 183 mm · two-column", "100% · 3.5 px/mm").foreach(t =>
      assert(page.contains(t), s"'$t' not on the page: $page")
    )
    assertEquals(runOnFx(panelNode(w, "D").getPrefWidth), 89 * 3.5)
    runOnFx(w.figures.dispatch(ComposerIntent.SetWidth(PageWidth.SingleColumn)))
    runOnFx(w.figures.dispatch(ComposerIntent.ZoomIn))
    assert(texts(w.figures.pageNode).contains("Page 89 mm · one-column"))
    assert(texts(w.figures.pageNode).contains("150% · 5.3 px/mm"))
    assertEquals(runOnFx(panelNode(w, "D").getPrefWidth), 89 * 5.25)
    // t2Figures has panel D selected: the Table tab shows its 50 values.
    eventually(fx, "panel D's table is shown")(
      runOnFx(w.figures.table.source.exists(_.rows.size == 50))
    )
    // A and B say what their screens displayed, and that no gaze is served.
    val a = texts(panelNode(w, "A"))
    val b = texts(panelNode(w, "B"))
    assert(a.contains("Encoding · enc_03 · beach-042") && a.contains("Displayed: image."), a)
    assert(
      b.contains("Displayed: blank + fixation cross. The remembered image was not shown."),
      b
    )
  }

  fxStage.test("the navigator: Figure 2 is stale and says why; New figure adds Figure 3") {
    fx =>
      val w = boot(fx, StoryModels.t2Figures, StoryMoment.T2)
      loaded(fx, w)
      val nav = texts(w.figures.navigatorNode)
      Vector("2 figures", "Figure 1", "current", "Figure 2", "Stale", "run 5 · rev 3 · data r2")
        .foreach(t => assert(nav.contains(t), s"'$t' not in the navigator: $nav"))
      runOnFx(w.figures.dispatch(ComposerIntent.Binding(FigureIntent.Select(figure2))))
      // The r2 → r3 status comparison is performed: the fake serves no r2 ledger.
      eventually(fx, "Figure 2's stale notice is shown") {
        texts(w.figures.navigatorNode).exists(
          _.startsWith(
            "Bound to run 5 · rev 3 · data r2, which is no longer current: dataset r3"
          )
        )
      }
      runOnFx(w.figures.dispatch(ComposerIntent.NewFigure))
      eventually(fx, "Figure 3 is shown")(texts(w.figures.pageNode).contains("Figure 3"))
      assert(texts(w.figures.navigatorNode).contains("3 figures"))
  }
