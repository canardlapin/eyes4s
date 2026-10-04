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

package eyes4s.studio.desktop.geometry

import eyes4s.studio.app.geometry.{GeometryPictures, PicturesKey, PlacementKey}
import eyes4s.studio.core.fixture.{FakePlacement, StoryMoments}

/** The geometry panel's 250 ms redraw (ticket S5.5), timed headless: r3's
  * 11,520 golden records placed by the backend (the fake's eyes4s
  * placement, as the panel asks for it on every geometry change), then the
  * pictures derived from the preview. The FX suite measures the whole
  * change-to-canvas receipt against a generous bound; this one holds the
  * computation to the ticket's own 250 ms, as the median of repeated runs
  * after warm-up, so one slow run on a loaded machine does not decide it.
  */
class GeometryPicturesTimingSuite extends munit.FunSuite:

  /** The ticket's redraw target. */
  val RedrawTarget: Double = 250.0

  test(
    "placing and picturing the 11,520 golden records takes at most 250 ms (median of 9 runs)"
  ) {
    val spec = StoryMoments.t2
      .flatMap(_.dataset(StoryMoments.r3).toRight("no r3"))
      .fold(fail(_), identity)
    val key = PicturesKey(
      spec.id,
      spec.geometry,
      spec.admission,
      PlacementKey.of(spec).getOrElse(fail("no fixation source")),
      None,
      None,
      None
    )
    def once(): Double =
      val start   = System.nanoTime()
      val preview = FakePlacement.of(spec).fold(e => fail(e.message), identity)
      assertEquals(preview.records.size, 11520)
      GeometryPictures.of(key, spec, preview)
      (System.nanoTime() - start) / 1e6
    (1 to 3).foreach(_ => once())
    val runs   = Vector.fill(9)(once()).sorted
    val median = runs(runs.size / 2)
    println(
      f"placement and pictures of 11,520 records: median $median%.1f ms (${runs.map(r => f"$r%.0f").mkString(", ")})"
    )
    assert(
      median <= RedrawTarget,
      s"median $median ms > $RedrawTarget ms: ${runs.mkString(", ")}"
    )
  }
