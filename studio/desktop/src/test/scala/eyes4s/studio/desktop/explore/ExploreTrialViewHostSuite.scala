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

import eyes4s.plan.MapPlacement
import eyes4s.studio.app.explore.{MarkVM, PreviewMapVM, ShownTrialVM}
import eyes4s.studio.app.maps.{MapGrid, MapId, RowOrder}
import eyes4s.studio.app.tokens.{StageVariant, Theme}
import eyes4s.studio.core.backend.{AnalysisRevision, ScreenRegion}
import eyes4s.studio.core.selection.FixationIndex
import eyes4s.studio.desktop.trial.GoldenTrials
import eyes4s.studio.viz.trial.{MapCoverage, MarkStyle, ScreenRect}

/** Explore's trial view host, headless (ticket S6.2): a preview is drawn
  * over the screen region its grid covers, which need not be the image; the
  * scene keeps neutral marks and the document's appearance.
  */
class ExploreTrialViewHostSuite extends munit.FunSuite:

  private def right[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val enc03   = GoldenTrials.key("P17", "enc_03")
  private val display = GoldenTrials.display("P17", "enc_03")

  /** A window that is not the image frame: the left half of the screen. */
  private val region = right(ScreenRegion.of(enc03, 0.0, 0.0, 960.0, 1080.0))

  private val grid = right(
    MapGrid.of(
      MapId.Preview(AnalysisRevision(4), enc03),
      4,
      3,
      RowOrder.TopFirst,
      Vector.tabulate(12)(i => Some(i / 66.0)),
      Vector(0.1)
    )
  )

  private def shown(map: Option[PreviewMapVM]) = ShownTrialVM(
    enc03,
    display,
    GoldenTrials.screen,
    Vector(MarkVM(right(FixationIndex.of(1)), 600.5, 323.5, 330, MapPlacement.InWindow)),
    true,
    false,
    map,
    Theme.Dark,
    StageVariant.Mid
  )

  test("the preview is requested over the region it covers, at the document's opacity") {
    val request = ExploreTrialViewHost.mapRequest(shown(Some(PreviewMapVM(grid, region))), 0.6)
    assertEquals(request.map(_.grid), Some(grid))
    assertEquals(
      request.map(_.covers),
      Some(MapCoverage.Region(right(ScreenRect.of(0.0, 0.0, 960.0, 1080.0))))
    )
    assert(display.placement.left != 0 || display.placement.width != 960, display.placement)
    assertEquals(request.map(_.opacity.value), Some(0.6))
    assertEquals(ExploreTrialViewHost.mapRequest(shown(None), 0.6), None)
  }

  test("the scene has neutral marks, the toolbar's choices and the document's appearance") {
    val (input, refused) = ExploreTrialViewHost.sceneInput(shown(None))
    assertEquals(refused, Vector.empty)
    assertEquals(input.marks, MarkStyle.Neutral)
    assertEquals((input.options.points, input.options.order), (true, false))
    assertEquals((input.theme, input.stage), (Theme.Dark, StageVariant.Mid))
    assertEquals(input.fixations.map(_.durationMs), Vector(330))
  }
