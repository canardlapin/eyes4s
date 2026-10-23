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

import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.maps.{ColourLimits, LimitsScope, MapId, MapOpacity}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.selection.ScaleIndex

class CompareMapsSuite extends munit.FunSuite:
  private def ok[E, A](value: Either[E, A]): A = value.fold(x => fail(x.toString), identity)
  private val q: MapId.Result                  =
    MapId.Result(StoryMoments.run7, StoryModels.p17ret07, StoryModels.sigma2)
  private val r: MapId.Result =
    MapId.Result(StoryMoments.run7, StoryModels.p17enc03, StoryModels.sigma2)
  private def grid(id: MapId.Result, peak: Double = 1.0): DensityGrid =
    ok(
      DensityGrid.of(
        id.run,
        id.scale.value,
        id.trial,
        2.0,
        ok(ScreenRegion.of(id.trial, 448, 156, 1472, 924)),
        2,
        2,
        RowOrder.BottomFirst,
        None,
        Vector(0.0, peak / 4, peak / 2, peak),
        Vector(DensityLevel(0.5, peak / 2))
      )
    )
  private def drawn(state: CompareMaps): Vector[CompareMapPanel.Drawn] =
    state.panels.collect { case p: CompareMapPanel.Drawn => p }

  test("one read per identity, with both displayed panels retained") {
    val (pending, reads) =
      CompareMaps.follow(CompareMaps.empty, Vector(q, q), MapOpacity.Default)
    assertEquals(reads, Vector(q))
    assertEquals(pending.panels.size, 2)
    assertEquals(CompareMaps.follow(pending, Vector(q, q), MapOpacity.Default)._2, Vector.empty)
    val ready = CompareMaps.read(pending, q, Right(grid(q)))
    assertEquals(drawn(ready).map(_.id), Vector(q, q))
  }

  test("cells, row order, coverage and contour thresholds survive all display styles") {
    val pending = CompareMaps.follow(CompareMaps.empty, Vector(q, r), MapOpacity.Default)._1
    val ready   =
      CompareMaps.read(CompareMaps.read(pending, q, Right(grid(q))), r, Right(grid(r, 2)))
    val before = drawn(ready)
    assertEquals(before.map(_.style.limits), Vector.fill(2)(ok(ColourLimits.sequential(0, 2))))
    val changed = CompareMaps
      .follow(
        CompareMaps.chooseLimits(ready, LimitsScope.PerPanel),
        Vector(q, r),
        MapOpacity.of(0.25).get
      )
      ._1
    val after = drawn(changed)
    assertEquals(after.map(_.grid), before.map(_.grid))
    assertEquals(after.map(_.region), before.map(_.region))
    assertEquals(
      after.map(_.style.limits),
      Vector(ok(ColourLimits.sequential(0, 1)), ok(ColourLimits.sequential(0, 2)))
    )
    assertEquals(after.head.grid.cells, Vector(Some(0.0), Some(0.25), Some(0.5), Some(1.0)))
    assertEquals(after.head.grid.order, eyes4s.studio.app.maps.RowOrder.BottomFirst)
    assertEquals(after.head.grid.levels, Vector(0.5))
    assertEquals(after.map(_.opacity.value), Vector(0.25, 0.25))
  }

  test(
    "another run, scale or trial is refused under the requested identity and can be retried"
  ) {
    val others: Vector[MapId.Result] = Vector(
      q.copy(run = RunId(8)),
      q.copy(scale = ok(ScaleIndex.of(1))),
      q.copy(trial = r.trial)
    )
    others.foreach { wrong =>
      val pending = CompareMaps.follow(CompareMaps.empty, Vector(q, r), MapOpacity.Default)._1
      val failed  = CompareMaps.read(pending, q, Right(grid(wrong)))
      assert(failed.failed)
      assertEquals(drawn(failed), Vector.empty)
      assert(
        failed.panels.head.isInstanceOf[CompareMapPanel.Unavailable],
        "mismatched grid was shown"
      )
      val (again, reads) = CompareMaps.retry(failed)
      assertEquals(reads, Vector(q))
      assert(!again.failed)
      assertEquals(drawn(CompareMaps.read(again, q, Right(grid(q)))).map(_.id), Vector(q))
    }
  }

  test("a failed transport stays distinct from an empty map and from a zero cell") {
    val pending = CompareMaps.follow(CompareMaps.empty, Vector(q), MapOpacity.Default)._1
    val failed  = CompareMaps.read(pending, q, Left("read refused"))
    assertEquals(failed.panels, Vector(CompareMapPanel.Unavailable(q, "read refused")))
    val ready = CompareMaps.read(CompareMaps.retry(failed)._1, q, Right(grid(q)))
    assertEquals(drawn(ready).head.grid.cells.head, Some(0.0))
  }

  test("late replies cannot replace the current pair and navigation keeps only wanted grids") {
    val initial = CompareMaps.follow(CompareMaps.empty, Vector(q, r), MapOpacity.Default)._1
    val nextQ   = q.copy(scale = ok(ScaleIndex.of(1)))
    val current = CompareMaps.follow(initial, Vector(nextQ), MapOpacity.Default)._1
    assertEquals(CompareMaps.read(current, q, Right(grid(q))), current)
    val loaded = CompareMaps.read(current, nextQ, Right(grid(nextQ)))
    assertEquals(loaded.answers.keySet, Set(nextQ))
    val empty = CompareMaps.follow(loaded, Vector.empty, MapOpacity.Default)._1
    assertEquals(empty.panels, Vector.empty)
    assertEquals(empty.answers, Map.empty)
    assertEquals(CompareMaps.read(empty, nextQ, Right(grid(nextQ))), empty)
  }

  test(
    "overlay choices are independent portable view state and trigger no new scientific reads"
  ) {
    val model   = StoryModels.t2Compare
    val shown   = Some(ShownRun(StoryMoments.run7, AnalysisRevision(4), Vector.empty))
    val initial = TrialPanels.sync(TrialPanels.empty, model, shown)._1
    val before  = TrialPanels.vm(initial, shown, Vector("0.5°", "1°", "2°", "4°"))
    for side <- PanelSide.values; overlay <- PanelOverlay.values do
      val changed = TrialPanels.update(initial, PanelsIntent.Overlay(side, overlay, false))
      val (synced, reads) = TrialPanels.sync(changed, model, shown)
      assertEquals(reads, Vector.empty)
      val vm = TrialPanels.vm(synced, shown, Vector("0.5°", "1°", "2°", "4°"))
      assertEquals(vm.query.map(_.readout), before.query.map(_.readout))
      assertEquals(vm.reference.map(_.readout), before.reference.map(_.readout))
      val picked = if side == PanelSide.Query then vm.query else vm.reference
      assertEquals(picked.map(_.overlays), Some(PanelOverlays().changed(overlay, false)))
      val other = if side == PanelSide.Query then vm.reference else vm.query
      assertEquals(other.map(_.overlays), Some(PanelOverlays()))
      assertEquals(
        TrialPanels.update(synced, PanelsIntent.Overlay(side, overlay, true)),
        initial
      )
  }
