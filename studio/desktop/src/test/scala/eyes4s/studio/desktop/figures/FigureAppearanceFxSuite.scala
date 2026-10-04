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
import eyes4s.studio.app.figures.{FigureType, PanelBody}
import eyes4s.studio.core.document.{FigureId, PanelLetter}
import eyes4s.studio.core.engine.StudioBuild
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.FxStage
import eyes4s.studio.desktop.plot.PlotTwinStatus
import eyes4s.studio.desktop.shell.ShellFxSuite
import intaglio.Grob
import javafx.scene.Node
import javafx.scene.control.{Button, Label, Labeled}
import javafx.scene.layout.VBox

import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

/** The figure's appearance, captions and provenance stamp in the studio
  * window (ticket S9.2b; Figures.dc.html, inspector and page) at story
  * moment t2: Figure 1 binds run 7.
  */
class FigureAppearanceFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(180, "s")

  /** JavaFX keeps a font's size as a float: compare within its precision. */
  private val FontSizeTolerance: Double = 1e-5

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private val figure1                      = ok(FigureId.of(1))
  private def letter(l: String)            = ok(PanelLetter.of(l))

  private def texts(node: Node): Vector[String] =
    runOnFx(node.lookupAll(".label").asScala.toVector.collect { case l: Label => l.getText })

  private def panelNode(w: StudioWindow, l: String): VBox =
    runOnFx(w.figures.paper.getChildren.asScala.collectFirst {
      case v: VBox if v.getId == s"figures-panel-$l" => v
    }).getOrElse(fail(s"no panel $l on the page"))

  private def labelled(node: Node, text: String): Labeled =
    def all(n: Node): Vector[Node] = n +: (n match
      case p: javafx.scene.Parent => p.getChildrenUnmodifiable.asScala.toVector.flatMap(all)
      case _                      => Vector.empty)
    runOnFx(all(node).collectFirst { case l: Labeled if l.getText == text => l })
      .getOrElse(fail(s"no '$text'"))

  /** The inspector's button named `name`. */
  private def press(w: StudioWindow, name: String): Unit =
    runOnFx(
      w.figures.inspectorNode
        .lookupAll(".button")
        .asScala
        .toVector
        .collectFirst {
          case b: Button if b.getAccessibleText == name => b
        }
        .getOrElse(fail(s"no button '$name'"))
        .fire()
    )

  private def loaded(fx: FxStage, w: StudioWindow): Unit =
    eventually(fx, "Figure 1's panels are read") {
      w.figures.vm.page.exists(_.panels.forall(p => !p.body.isInstanceOf[PanelBody.Waiting])) &&
      w.figures
        .plot(figure1, letter("D"))
        .exists(_.status.get.isInstanceOf[PlotTwinStatus.Shown])
    }

  private def joins(w: StudioWindow): Int =
    runOnFx(w.figures.plot(figure1, letter("D")).flatMap(_.plot)).fold(fail("no plot")) { p =>
      def all(g: Grob): Vector[Grob] = g +: g.children.flatMap(all)
      p.plot.scene.grobs.flatMap(all).count(_.isInstanceOf[Grob.Segments])
    }

  fxStage.test("caption numbers generated, not typed; the stamp shows eyes4s, run and spec") {
    fx =>
      val w = boot(fx, StoryModels.t2Figures, StoryMoment.T2)
      loaded(fx, w)
      val page = texts(w.figures.pageNode)
      Vector(
        "Figure 1. Matched-minus-control spatial similarity of retrieval gaze (dataset r3, " +
          "analysis rev 4, run 7). Spatial correspondence, not sequential replay.",
        "Analysis rev 4 · run 7 (archive unbound) · data r3 · reporting “By retrieval " +
          "response” (sha256:3a0e…5dd) · studio build eyes4s " +
          StudioBuild.eyes4sBaseVersion,
        "Each pair of dots is one participant; per participant, 2–17 queries per group. " +
          "Descriptive only: no intervals or tests."
      ).foreach(t => assert(page.contains(t), s"'$t' not on the page: $page"))
  }

  fxStage.test("the page is set in the figure typography; Text size changes it") { fx =>
    val w = boot(fx, StoryModels.t2Figures, StoryMoment.T2)
    loaded(fx, w)
    def font(n: Labeled) = runOnFx((n.getFont.getFamily, n.getFont.getSize))
    val title            = labelled(panelNode(w, "D"), "Participant D by response")
    assertEquals(font(title)._1, FigureType.BodyFace.javaFxFamily)
    assertEqualsDouble(font(title)._2, FigureType.px(7, 3.5), FontSizeTolerance)
    val d = labelled(panelNode(w, "D"), "D")
    assertEquals(font(d)._1, FigureType.LetterFace.javaFxFamily)
    assertEqualsDouble(font(d)._2, FigureType.px(FigureType.LetterPt, 3.5), FontSizeTolerance)
    press(w, "Text size 8 pt")
    val larger = labelled(panelNode(w, "D"), "Participant D by response")
    assertEqualsDouble(font(larger)._2, FigureType.px(8, 3.5), FontSizeTolerance)
    assert(
      runOnFx(
        w.figures.inspectorNode
          .lookupAll(".button")
          .asScala
          .exists(n => n.getAccessibleText == "Text size 8 pt, selected")
      )
    )
  }

  fxStage.test("participant lines, panel width and the images export option") { fx =>
    val w = boot(fx, StoryModels.t2Figures, StoryMoment.T2)
    loaded(fx, w)
    val shown = joins(w)
    press(w, "Participant lines Hide")
    eventually(fx, "the participant plot is redrawn without its lines")(joins(w) == shown - 1)
    assert(texts(panelNode(w, "D")).exists(_.startsWith("Each dot is one participant's mean;")))
    // t2Figures has panel D selected: its width is 89 mm, one more is 90.
    press(w, "Wider + (panel width 89 mm)")
    assertEquals(runOnFx(panelNode(w, "D").getPrefWidth), 90 * 3.5)
    press(w, "project snapshot includes images, on")
    assert(
      runOnFx(
        w.figures.inspectorNode
          .lookupAll(".button")
          .asScala
          .exists(n => n.getAccessibleText == "project snapshot includes images, off")
      )
    )
  }
