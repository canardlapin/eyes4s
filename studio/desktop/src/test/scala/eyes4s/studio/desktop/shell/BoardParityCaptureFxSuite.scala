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

package eyes4s.studio.desktop.shell

import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.app.appearance.Appearance
import eyes4s.studio.core.document.{FigureId, PanelLetter}
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.harness.StudioTheme
import eyes4s.studio.desktop.plot.PlotTwinStatus
import eyes4s.studio.desktop.trial.TrialViewStatus
import eyes4s.studio.viz.trial.FrameArt
import eyes4s.studio.app.explore.TimelineIntent
import eyes4s.studio.app.plot.HalfOpenSpan
import javafx.scene.control.{Labeled, ScrollPane}

import java.nio.file.Files
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

/** Native inputs for the whole-board review. These captures complement,
  * rather than replace, the human comparison with the pinned HTML boards.
  * Each board boots its own story moment and waits for its served content.
  */
class BoardParityCaptureFxSuite extends StoryCaptureFixture:

  override val munitTimeout: Duration = Duration(180, "s")

  private final case class Board(
      name: String,
      model: AppModel,
      moment: StoryMoment,
      theme: StudioTheme,
      focus: Option[RestingFocus.Board]
  )

  private val boards = Vector(
    Board(
      "Main",
      StoryModels.t2Compare,
      StoryMoment.T2,
      StudioTheme.Light,
      Some(RestingFocus.Board.Main)
    ),
    Board(
      "MainDark",
      at(StoryModels.t2Compare, Intent.SetAppearance(Appearance.Dark)),
      StoryMoment.T2,
      StudioTheme.Dark,
      None
    ),
    Board("Data", StoryModels.t1Data, StoryMoment.T1, StudioTheme.Light, None),
    Board(
      "Analysis",
      StoryModels.t2Analysis,
      StoryMoment.T2,
      StudioTheme.Light,
      Some(RestingFocus.Board.Analysis)
    ),
    Board(
      "Explore",
      StoryModels.t2Explore,
      StoryMoment.T2,
      StudioTheme.Light,
      Some(RestingFocus.Board.Explore)
    ),
    Board(
      "Results",
      StoryModels.t3Summary,
      StoryMoment.T3,
      StudioTheme.Light,
      Some(RestingFocus.Board.Results)
    ),
    Board(
      "Figures",
      StoryModels.t2Figures,
      StoryMoment.T2,
      StudioTheme.Light,
      Some(RestingFocus.Board.Figures)
    )
  )

  boards.foreach { board =>
    fxStage.test(board.name) { fx =>
      assumeFullStage(fx)
      val w =
        boot(fx, board.model, board.moment, panels = golden, project = Some(fixtureProject))
      board.name match
        case "Main" | "MainDark" =>
          eventually(fx, "both native trial stages") {
            w.summary.panels.queryView.status.get.isInstanceOf[TrialViewStatus.Shown] &&
            w.summary.panels.referenceView.status.get.isInstanceOf[TrialViewStatus.Shown]
          }
          assertEquals(
            runOnFx(w.summary.panels.shown.map(_._2)),
            Vector("P17 · ret_07 · beach-042", "P17 · enc_03 · beach-042")
          )
          eventually(fx, "query navigation at the selected scale") {
            w.summary.summary.shown.contains(StoryModels.sigma2)
          }
          assertEquals(runOnFx(w.summary.summary.shown), Some(StoryModels.sigma2))
          eventually(fx, "both result maps and the stored reference image") {
            val views = Vector(w.summary.panels.queryView, w.summary.panels.referenceView)
            views.forall(_.status.get match
              case TrialViewStatus.Shown(s) => s.map.nonEmpty
              case _ => false) && (w.summary.panels.referenceView.status.get match
              case TrialViewStatus.Shown(s) => s.frameArt.isInstanceOf[FrameArt.Picture]
              case _                        => false)
          }
        case "Data" =>
          eventually(fx, "r3 geometry thumbnails") {
            w.geometry.shownPictures.exists(
              _.key.dataset == eyes4s.studio.core.fixture.StoryMoments.r3
            ) &&
            w.geometry.view.thumbnails.exists(_.count.getText.nonEmpty)
          }
          eventually(fx, "the project's source bytes") { w.columnMapping.loaded.isDone }
          runOnFx {
            val choices = w.root.lookupAll(".radio-button").asScala.collect {
              case r: javafx.scene.control.RadioButton if r.getText == "Review exclusions" => r
            }
            assertEquals(choices.size, 1)
            choices.head.fire()
          }
        case "Analysis" =>
          eventually(fx, "the counted preview") {
            w.resolvedDesign.state.preview.receipt.isDefined
          }
        case "Explore" =>
          eventually(fx, "the preview map, stored image and timeline") {
            (w.explore.trialView.status.get match
              case TrialViewStatus.Shown(s) =>
                s.map.nonEmpty && s.frameArt.isInstanceOf[FrameArt.Picture]
              case _ => false
            ) && w.timeline.vm.enabled
          }
          runOnFx {
            w.timeline.dispatch(TimelineIntent.Play)
            // Capture the board at its absolute raw onset, regardless of playback coverage.
            w.timeline.dispatch(TimelineIntent.Tick(2160.0 - w.timeline.timeline.playheadMs))
            w.timeline.dispatch(TimelineIntent.Pause)
            w.timeline.dispatch(TimelineIntent.Brushed(HalfOpenSpan.between(1200.0, 2800.0)))
          }
          assertEquals(runOnFx(w.timeline.vm.playheadMs), Some(2160.0))
        case "Figures" =>
          val id = FigureId.of(1).fold(e => fail(e.toString), identity)
          val d  = PanelLetter.of("D").fold(e => fail(e.toString), identity)
          eventually(fx, "panel D's participant plot") {
            w.figures.plot(id, d).exists(_.status.get.isInstanceOf[PlotTwinStatus.Shown])
          }
          eventually(fx, "the gaze panels' stored image and blank display") {
            Vector("A", "B").forall { name =>
              val letter = PanelLetter.of(name).fold(e => fail(e.toString), identity)
              w.figures
                .gaze(id, letter)
                .exists(_.status.get match
                  case TrialViewStatus.Shown(s) =>
                    if name == "A" then s.frameArt.isInstanceOf[FrameArt.Picture]
                    else s.frameArt == FrameArt.ScreenWithCross
                  case _ => false)
            }
          }
        case "Results" =>
          eventually(fx, "the participant plot") {
            w.summary.participantPlot.status.get.isInstanceOf[PlotTwinStatus.Shown]
          }
          assertEquals(runOnFx(w.summary.summary.shown), Some(StoryModels.sigma2))
          assertEquals(runOnFx(w.summary.vm.paired.flatMap(_.value)), Some("+0.18"))
        case _ => ()
      // On a native glass, focus can arrive after the first pulse. Do not
      // silently skip a board's review before that event has had a chance.
      runOnFx(fx.stage.requestFocus())
      eventually(fx, "the review stage holds keyboard focus") { fx.stage.isFocused }
      board.focus match
        case Some(focus) => RestingFocus.show(fx, w, focus)
        case None        => RestingFocus.clear(fx, w)
      fx.awaitLayout()
      val evidence = runOnFx {
        val heights = Vector(
          "appBar"       -> w.shell.appBar.node.getLayoutBounds.getHeight,
          "contextStrip" -> w.shell.contextStrip.node.getLayoutBounds.getHeight,
          "statusBar"    -> w.shell.statusBar.node.getLayoutBounds.getHeight
        )
        val wanted = Map("appBar" -> 44.0, "contextStrip" -> 32.0, "statusBar" -> 24.0)
        heights.foreach((name, h) => assertEqualsDouble(h, wanted(name), 1))
        val headers = w.root.lookupAll(".dock-header").asScala.toVector.filter(_.isVisible)
        assert(headers.nonEmpty)
        headers.foreach(h => assertEqualsDouble(h.getLayoutBounds.getHeight, 28, 1))
        val labels = w.root.lookupAll("*").asScala.toVector.collect {
          case l: Labeled if l.isVisible && Option(l.getText).exists(_.nonEmpty) =>
            val b = l.localToScene(l.getBoundsInLocal)
            ujson.Obj(
              "text"   -> drawn(l),
              "x"      -> b.getMinX,
              "y"      -> b.getMinY,
              "width"  -> b.getWidth,
              "height" -> b.getHeight
            )
        }
        ujson.Obj(
          "board"                  -> board.name,
          "moment"                 -> board.moment.toString,
          "width"                  -> fx.scene.getWidth,
          "height"                 -> fx.scene.getHeight,
          "heights"                -> ujson.Obj.from(heights.map((k, v) => k -> ujson.Num(v))),
          "tabHeaders"             -> ujson.Arr.from(headers.map(_.getLayoutBounds.getHeight)),
          "toleranceLogicalPixels" -> 1,
          "nativeOutputScaleX"     -> fx.stage.getOutputScaleX,
          "nativeOutputScaleY"     -> fx.stage.getOutputScaleY,
          "scale2Capture" -> "snapshot transform; native output scale recorded separately",
          "shellText"     -> ujson.Arr.from(texts(w)),
          "labels"        -> ujson.Arr.from(labels)
        )
      }
      assertEquals(fx.sceneSize, (1440.0, 900.0))
      val captures = fx.snapshot(board.theme)
      Files.writeString(
        captures.head.getParent.resolve("metrics.json"),
        ujson.write(evidence, indent = 2)
      )
      val originals                      = captures.map(p => p -> Files.readAllBytes(p))
      def supplement(name: String): Unit =
        fx.awaitLayout()
        fx.snapshot(board.theme)
          .foreach(p =>
            Files.move(
              p,
              p.resolveSibling(s"$name-${p.getFileName}"),
              java.nio.file.StandardCopyOption.REPLACE_EXISTING
            )
          )
      board.name match
        case "Data" =>
          runOnFx {
            w.geometry.view.node
              .lookupAll(".scroll-pane")
              .asScala
              .collect { case s: ScrollPane =>
                s
              }
              .foreach(_.setVvalue(1.0))
          }
          supplement("geometry-bottom")
          runOnFx {
            w.columnMapping.node
              .lookupAll(".scroll-pane")
              .asScala
              .collect { case s: ScrollPane =>
                s
              }
              .foreach(_.setVvalue(1.0))
          }
          supplement("mapping-bottom")
        case "Figures" =>
          runOnFx {
            w.figures.pageNode
              .lookupAll(".scroll-pane")
              .asScala
              .collect { case s: ScrollPane =>
                s
              }
              .foreach(_.setVvalue(1.0))
          }
          supplement("page-bottom")
          runOnFx(
            w.figures.dispatch(
              eyes4s.studio.app.figures.ComposerIntent.Binding(
                eyes4s.studio.app.figures.FigureIntent
                  .Select(FigureId.of(2).fold(e => fail(e.toString), identity))
              )
            )
          )
          eventually(fx, "the selected stale figure's reason") {
            w.figures.navigatorNode
              .lookupAll(".label")
              .asScala
              .collect { case l: Labeled =>
                l.getText
              }
              .exists(_.startsWith("Bound to run 5"))
          }
          supplement("stale-figure")
        case _ => ()
      originals.foreach((path, bytes) => Files.write(path, bytes))
      ()
    }
  }
