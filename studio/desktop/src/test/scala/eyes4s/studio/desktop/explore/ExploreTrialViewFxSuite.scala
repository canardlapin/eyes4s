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

package eyes4s.studio.desktop.explore

import eyes4s.plan.{MapPlacement, OffWindowPolicy}
import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.explore.TrialToggle
import eyes4s.studio.app.layout.StudioLayouts
import eyes4s.studio.app.maps.MapId
import eyes4s.studio.core.backend.AnalysisRevision
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.FxStage
import eyes4s.studio.desktop.shell.ShellFxSuite
import eyes4s.studio.desktop.plot.PlotHostStatus
import eyes4s.studio.desktop.trial.{MapLegendPlacement, TrialViewStatus}
import eyes4s.studio.viz.maps.MapLegendScene
import eyes4s.studio.viz.trial.{MarkStyle, TrialScene}
import intaglio.{Grob, value}
import javafx.scene.Node
import javafx.scene.control.Labeled

import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

/** Explore's trial view hosted in its pane (ticket S6.2; Explore.dc.html,
  * centre), in a studio window over the fake backend at story moment t2:
  * P17 enc_03 under rev 4 with neutral marks, fixation 10 flagged outside the
  * analysis window, the preview map tagged "Map is a preview, not a
  * result" in a dashed frame, the legend, and no score anywhere in Explore.
  *
  * Every interaction goes through the controls' own events, so no test
  * depends on OS window focus.
  */
class ExploreTrialViewFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(120, "s")

  private val rev4  = AnalysisRevision(4)
  private val enc03 = StoryModels.p17enc03

  /** A window at t2 in Explore with the trial drawn and its preview mapped. */
  private def ready(fx: FxStage): StudioWindow =
    assumeFullStage(fx)
    val w = boot(fx, StoryModels.t2Explore, StoryMoment.T2)
    eventually(fx, "the trial's fixations, preview and display") {
      val s = w.explore.state
      s.fixations.toOption.isDefined && s.preview.toOption.isDefined &&
      s.displays.toOption.isDefined
    }
    eventually(fx, "the trial view's scene with its preview map")(
      scene(w).exists(_.map.isDefined)
    )
    w

  private def scene(w: StudioWindow): Option[TrialScene] =
    w.explore.trialView.status.get match
      case TrialViewStatus.Shown(s) => Some(s)
      case _                        => None

  private def fire(fx: FxStage, button: javafx.scene.control.ButtonBase): Unit =
    runOnFx(button.fire())
    fx.awaitLayout()

  private def all(n: Node): Vector[Node] = n match
    case p: javafx.scene.Parent => n +: p.getChildrenUnmodifiable.asScala.toVector.flatMap(all)
    case _                      => Vector(n)

  private def names(g: Grob): Vector[String] = g match
    case Grob.Group(children, _, name) => name.map(_.value).toVector ++ children.flatMap(names)
    case other                         => other.name.map(_.value).toVector

  fxStage.test("t2: P17 enc_03 with neutral marks and the dashed preview label") { fx =>
    val w    = ready(fx)
    val pane = runOnFx(w.host.node(StudioLayouts.trialView))
      .getOrElse(fail("the trial view pane was not built"))
    assert(runOnFx(pane.getScene eq fx.scene))
    val p = w.explore.pane
    assertEquals(runOnFx(p.title.getText), "P17 · enc_03")
    assertEquals(
      runOnFx(
        TrialToggle.values.toVector.map(t => (p.toggles(t).getText, p.toggles(t).isSelected))
      ),
      Vector(("Points", true), ("Order", true), ("Map", true))
    )
    // The preview's tag in the pane header (overhaul v4).
    assertEquals(runOnFx(p.mapLabel.getText), "Map is a preview, not a result")
    assert(runOnFx(p.mapLabel.isVisible))
    // The scene: the matched reference's marks (the drill is below its pair),
    // the preview (not a run's map), fixation 10 flagged.
    val s = scene(w).get
    assertEquals(
      runOnFx(w.explore.trialView.input.map(_.marks)),
      Some(MarkStyle.Role(eyes4s.studio.viz.trial.TrialRole.Matched))
    )
    assertEquals(s.map, Some(MapId.Preview(rev4, enc03)))
    assertEquals(s.marks.size, 13)
    assertEquals(
      s.marks
        .filterNot(_.placement == MapPlacement.InWindow)
        .map(m => (m.ref.index.value, m.placement)),
      Vector((10, MapPlacement.OutsideWindow(OffWindowPolicy.Exclude)))
    )
    assertEquals(
      runOnFx(p.legend.lookupAll(".pair-legend-label").asScala.toVector.collect {
        case l: Labeled => l.getText
      }),
      Vector(
        "fixation, area shows duration",
        "selected",
        "outside window",
        "order, not saccades",
        "0",
        "max density · photo in grey",
        "50 · 90% of mass"
      )
    )
  }

  fxStage.test("no score node exists in Explore's scene graph") { fx =>
    val w                           = ready(fx)
    val nodes                       = runOnFx(all(w.root))
    def mentionsScore(text: String) = text.toLowerCase.contains("score")
    val scored                      = runOnFx(nodes.filter { n =>
      n.getStyleClass.asScala.exists(mentionsScore) ||
      Option(n.getId).exists(mentionsScore) ||
      Option(n.getAccessibleText).exists(mentionsScore) ||
      (n match
        case l: Labeled => Option(l.getText).exists(mentionsScore)
        case _          => false)
    })
    assertEquals(runOnFx(scored.map(_.toString)), Vector.empty[String])
    // Nor in the trial's drawn scene.
    val grobs = scene(w).get.plot.scene.grobs.flatMap(names)
    assert(grobs.nonEmpty)
    assert(!grobs.exists(mentionsScore), grobs)
    assert(grobs.contains(TrialScene.MapName), grobs)
  }

  fxStage.test("the toggles turn the marks, the order lines and the preview map off") { fx =>
    val w = ready(fx)
    val p = w.explore.pane
    fire(fx, p.toggles(TrialToggle.Map))
    assertEquals(runOnFx(w.explore.state.map), false)
    assert(!runOnFx(p.mapLabel.isVisible))
    eventually(fx, "the scene without its map")(scene(w).exists(_.map.isEmpty))
    fire(fx, p.toggles(TrialToggle.Points))
    eventually(fx, "the scene without its points")(
      w.explore.trialView.input.exists(i => !i.options.points)
    )
    fire(fx, p.toggles(TrialToggle.Order))
    assertEquals(
      runOnFx(w.explore.trialView.input.map(i => (i.options.points, i.options.order))),
      Some((false, false))
    )
    // Map comes back with its label.
    fire(fx, p.toggles(TrialToggle.Map))
    assertEquals(runOnFx(p.mapLabel.getText), "Map is a preview, not a result")
    eventually(fx, "the preview map again")(scene(w).exists(_.map.isDefined))
  }

  fxStage.test("the preview map's legend keys are in the legend row, none under the stage") {
    fx =>
      val w     = ready(fx)
      val p     = w.explore.pane
      val trial = w.explore.trialView
      // The map has its legend (its description ends the trial's), placed in
      // the host's row: the view draws none and keeps no room for one.
      eventually(fx, "the preview map's legend")(runOnFx(trial.legend).isDefined)
      assertEquals(runOnFx(trial.legendPlacement), MapLegendPlacement.InHostLegend)
      assert(!runOnFx(trial.legendPlotHost.isVisible), "a legend is drawn under the stage")
      assert(!runOnFx(trial.legendUnderTrial))
      // The keys stand for the preview map's entry, after order (overhaul v4).
      val row = runOnFx(p.legend.getChildren.asScala.toVector)
      val at  = row.indexWhere(n => n eq p.rampItem)
      assert(at > 0, "no ramp key in the legend row")
      assertEquals(row.lift(at + 1).map(_ eq p.isolineItem), Some(true))
      assertEquals(
        runOnFx(all(row(at - 1)).collect { case l: Labeled => l.getText }),
        Vector("order, not saccades")
      )
      // Each key draws the legend's own pieces, at the key's size.
      eventually(fx, "the keys drawn")(runOnFx {
        Vector(p.rampKey, p.isolineKey).forall(_.status.get match
          case PlotHostStatus.Drawn(_) => true
          case _                       => false)
      })
      assertEquals(
        runOnFx((p.rampKey.getWidth, p.rampKey.getHeight)),
        (MapLegendScene.RampKeyWidthPx, MapLegendScene.RampKeyHeightPx)
      )
      assertEquals(
        runOnFx((p.isolineKey.getWidth, p.isolineKey.getHeight)),
        (MapLegendScene.IsolineKeyWidthPx, MapLegendScene.IsolineKeyHeightPx)
      )
      // Where the map fades out is said to assistive technology (and shown as
      // the key's tooltip), as the board's title says it.
      val said = runOnFx(Option(p.rampItem.getAccessibleText))
      assert(said.exists(_.contains("fading out below 25 percent of max")), said)
      // Map off: no keys.
      fire(fx, p.toggles(TrialToggle.Map))
      eventually(fx, "the scene without its map")(scene(w).exists(_.map.isEmpty))
      val bare = runOnFx(p.legend.getChildren.asScala.toVector)
      assert(!bare.exists(n => (n eq p.rampItem) || (n eq p.isolineItem)), bare)
  }
