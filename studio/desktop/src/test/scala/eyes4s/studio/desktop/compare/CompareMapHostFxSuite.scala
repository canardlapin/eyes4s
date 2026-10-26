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

package eyes4s.studio.desktop.compare

import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.compare.*
import eyes4s.studio.app.maps.{ColourLimits, LimitsScope, MapId, MapRaster, RasterBudget}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.selection.ScaleIndex
import eyes4s.studio.desktop.shell.ShellFxSuite
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.maps.MapRasterStore
import eyes4s.studio.desktop.trial.{GoldenTrials, StimulusSource, TrialViewStatus}
import javafx.scene.layout.HBox
import javafx.scene.control.Label
import scala.jdk.CollectionConverters.*

class CompareMapHostFxSuite extends ShellFxSuite:
  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private val run                          = StoryMoments.run7
  private val q                            = StoryModels.p17ret07
  private val r                            = StoryModels.p17enc03
  private val two                          = StoryModels.sigma2
  private val one                          = ok(ScaleIndex.of(1))
  private val stimuli                      = StimulusSource.directory(GoldenTrials.stimuli)

  private def grid(trial: TrialKey, scale: ScaleIndex, peak: Double): DensityGrid =
    ok(
      DensityGrid.of(
        run,
        scale.value,
        trial,
        2.0,
        ok(ScreenRegion.of(trial, 448, 156, 1472, 924)),
        2,
        2,
        RowOrder.BottomFirst,
        None,
        Vector(0.0, peak / 4, peak / 2, peak),
        Vector(DensityLevel(0.5, peak / 2))
      )
    )

  private final class Held extends SummaryInputs:
    val reads = scala.collection.mutable.ArrayBuffer
      .empty[(MapId.Result, Either[String, DensityGrid] => Unit)]
    override def mapGrid(id: MapId.Result, done: Either[String, DensityGrid] => Unit): Unit =
      reads += ((id, done))
    def summary(run: RunId, done: SummaryAnswer => Unit): Unit = fail("unexpected summary read")
    def queries(run: RunId, done: QueriesAnswer => Unit): Unit = fail("unexpected query read")
    def inspect(run: RunId, address: ResultAddress, done: PairAnswer => Unit): Unit = fail(
      "unexpected pair read"
    )
    def pairs(run: RunId, scale: ScaleIndex, done: PairsAnswer => Unit): Unit = fail(
      "unexpected pairs read"
    )
    def ladder(
        run: RunId,
        query: TrialKey,
        scales: Vector[String],
        done: LadderAnswer => Unit
    ): Unit                                                = fail("unexpected ladder read")
    def answer(id: MapId.Result, value: DensityGrid): Unit =
      reads.reverseIterator.find(_._1 == id).getOrElse(fail(s"no read $id"))._2(Right(value))

  fxStage.test(
    "Compare uses served cells, row order, region and thresholds; styles and toggles change no score"
  ) { fx =>
    val w = boot(
      fx,
      StoryModels.t2Compare,
      panels = PanelSources(GoldenTrials.contentSource, stimuli)
    )
    eventually(fx, "both result map rasters") {
      Vector(w.summary.panels.queryView, w.summary.panels.referenceView)
        .forall(_.status.get match
          case TrialViewStatus.Shown(scene) => scene.map.nonEmpty
          case _                            => false)
    }
    eventually(fx, "the inspected pair score is served") {
      w.summary.inspectorInputs.panels.inspected.exists(_._2.exists {
        case PairAnswer.Answered(_) => true
        case _                      => false
      })
    }
    val score    = runOnFx(w.summary.panels.shown)
    val original = runOnFx(w.summary.panels.queryView.requestedMap.get.grid)
    runOnFx(w.summary.panels.queryToggles(2).fire())
    assertEquals(runOnFx(w.summary.panels.queryView.requestedMap), None)
    runOnFx(w.summary.panels.queryToggles(2).fire())
    assertEquals(runOnFx(w.summary.panels.queryView.requestedMap.map(_.grid)), Some(original))
    runOnFx { w.summary.panels.queryToggles(0).fire(); w.summary.panels.queryToggles(1).fire() }
    assertEquals(
      runOnFx(w.summary.panels.queryView.input.map(i => (i.options.points, i.options.order))),
      Some((false, false))
    )
    runOnFx { w.summary.panels.queryToggles(0).fire(); w.summary.panels.queryToggles(1).fire() }
    assertEquals(runOnFx(w.summary.panels.shown), score)
    val panels = runOnFx(TrialPanelsView(_ => (), _ => (), () => (), stimuli))
    val held   = Held()
    val host   = runOnFx(CompareMapHost(held, panels))
    val vm     = runOnFx(w.summary.panelsVM)
    val focus  = TrialPanels.focusOf(StoryModels.t2Compare)
    fx.show(runOnFx {
      val root = HBox(panels.queryNode, panels.referenceNode)
      root.getStyleClass.add("studio-shell")
      root.getStylesheets.setAll(ok(StudioStyles.stylesheets(Theme.Light))*)
      root
    })
    try
      runOnFx {
        panels.render(vm, Theme.Light)
        host.sync(focus, vm, 0.6)
        held.answer(MapId.Result(run, q, two), grid(q, two, 1.0))
        held.answer(MapId.Result(run, r, two), grid(r, two, 2.0))
      }
      eventually(fx, "two served maps") {
        panels.queryView.requestedMap.nonEmpty && panels.referenceView.requestedMap.nonEmpty
      }
      val before = runOnFx(panels.queryView.requestedMap.get)
      assertEquals(before.grid.cells, Vector(Some(0.0), Some(0.25), Some(0.5), Some(1.0)))
      assertEquals(before.grid.order, eyes4s.studio.app.maps.RowOrder.BottomFirst)
      assertEquals(before.grid.levels, Vector(0.5))
      assertEquals(before.style.limits, ok(ColourLimits.sequential(0.0, 2.0)))
      runOnFx { host.chooseLimits(LimitsScope.PerPanel); host.sync(focus, vm, 0.25) }
      val after = runOnFx(panels.queryView.requestedMap.get)
      assertEquals(after.grid, before.grid)
      assertEquals(after.style.limits, ok(ColourLimits.sequential(0.0, 1.0)))
      assertEquals(after.opacity.value, 0.25)
      assertEquals(runOnFx(w.summary.panels.shown), score)
    finally runOnFx { host.dispose(); panels.dispose() }
  }

  fxStage.test(
    "late and wrong-identity map answers cannot replace the selected scale; refusals retry"
  ) { fx =>
    val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
    eventually(fx, "trial identities") {
      w.summary.panelsVM.query.nonEmpty && w.summary.panelsVM.reference.nonEmpty
    }
    val vm     = runOnFx(w.summary.panelsVM)
    val panels = runOnFx(TrialPanelsView(_ => (), _ => (), () => (), stimuli))
    val held   = Held()
    val host   = runOnFx(CompareMapHost(held, panels))
    val focus  = TrialPanels.focusOf(StoryModels.t2Compare).get
    try
      runOnFx {
        host.sync(Some(focus), vm, 0.6)
        host.sync(Some(focus.copy(scale = one)), vm, 0.6)
        held.answer(MapId.Result(run, q, two), grid(q, two, 1.0))
        held.answer(MapId.Result(run, r, two), grid(r, two, 2.0))
      }
      fx.awaitLayout()
      assertEquals(runOnFx(panels.queryView.requestedMap), None)
      runOnFx {
        held.answer(MapId.Result(run, q, one), grid(q, one, 1.0))
        held.answer(MapId.Result(run, r, one), grid(r, two, 2.0))
      }
      eventually(fx, "wrong scale refused visibly") { panels.retryOffered }
      assertEquals(runOnFx(panels.referenceView.requestedMap), None)
      assertEquals(
        runOnFx(panels.queryView.requestedMap.map(_.grid.map)),
        Some(MapId(run, q, one))
      )
      val reads = held.reads.size
      runOnFx { host.retry(); host.sync(Some(focus.copy(scale = one)), vm, 0.6) }
      assertEquals(held.reads.size, reads + 1)
      runOnFx(held.answer(MapId.Result(run, r, one), grid(r, one, 2.0)))
      eventually(fx, "retried result map") { panels.referenceView.requestedMap.nonEmpty }
      runOnFx {
        host.sync(None, vm, 0.6); host.dispose();
        held.answer(MapId.Result(run, r, one), grid(r, one, 2.0))
      }
      fx.awaitLayout()
      assertEquals(runOnFx(panels.referenceView.requestedMap), None)
    finally runOnFx { host.dispose(); panels.dispose() }
  }

  fxStage.test("a raster failure is visible and Retry redraws the same served cells") { fx =>
    val w = boot(
      fx,
      StoryModels.t2Compare,
      panels = PanelSources(GoldenTrials.contentSource, stimuli)
    )
    eventually(fx, "both served trial contents") {
      Vector(w.summary.panelsVM.query, w.summary.panelsVM.reference)
        .forall(_.exists(_.content.isInstanceOf[PanelContent.Shown]))
    }
    var broken = true
    val store  = MapRasterStore.on(
      RasterBudget.Default,
      _.run(),
      _.run(),
      (g, s) =>
        if broken then throw IllegalStateException("intentional raster failure")
        else MapRaster.render(g, s)
    )
    val vm     = runOnFx(w.summary.panelsVM)
    val panels = runOnFx(TrialPanelsView(_ => (), _ => (), () => (), stimuli, maps = store))
    val held   = Held()
    val host   = runOnFx(CompareMapHost(held, panels))
    fx.show(runOnFx {
      val root = HBox(panels.queryNode, panels.referenceNode)
      root.getStyleClass.add("studio-shell")
      root.getStylesheets.setAll(ok(StudioStyles.stylesheets(Theme.Light))*)
      root
    })
    def failures = Vector(panels.queryNode, panels.referenceNode).flatMap(
      _.lookupAll(".label").asScala.collect {
        case l: Label if l.isVisible && l.getText.contains("intentional raster failure") =>
          l.getText
      }
    )
    try
      runOnFx {
        panels.render(vm, Theme.Light)
        host.sync(TrialPanels.focusOf(StoryModels.t2Compare), vm, 0.6)
        held.answer(MapId.Result(run, q, two), grid(q, two, 1.0))
        held.answer(MapId.Result(run, r, two), grid(r, two, 2.0))
      }
      eventually(fx, "both refusals have visible reasons and a Retry control") {
        failures.size == 2 && panels.retryOffered
      }
      val before = runOnFx(
        Vector(
          panels.queryView.requestedMap.get.grid,
          panels.referenceView.requestedMap.get.grid
        )
      )
      val renderedBeforeRetry = store.renderCount
      runOnFx { broken = false; panels.pressRetry() }
      eventually(fx, "both raster retries draw") {
        Vector(panels.queryView, panels.referenceView).forall(_.status.get match
          case TrialViewStatus.Shown(scene) => scene.map.nonEmpty
          case _ => false) && failures.isEmpty && !panels.retryOffered
      }
      assertEquals(held.reads.size, 2)
      assertEquals(
        runOnFx(
          Vector(
            panels.queryView.requestedMap.get.grid,
            panels.referenceView.requestedMap.get.grid
          )
        ),
        before
      )
      assertEquals(store.renderCount, renderedBeforeRetry + 2L)
    finally runOnFx { host.dispose(); panels.dispose(); store.close() }
  }
