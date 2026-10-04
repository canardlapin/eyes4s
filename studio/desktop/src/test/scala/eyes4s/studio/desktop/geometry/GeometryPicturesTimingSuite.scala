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

import eyes4s.codec.ByteDigest
import eyes4s.studio.app.geometry.{GeometryPictures, PicturesKey, PositionsKey}
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.geometry.SourcePositions
import eyes4s.studio.desktop.trial.GoldenTrials

import java.nio.file.Files

/** The geometry panel's 250 ms redraw (ticket S5.5), timed headless: the
  * pictures of r3 over the golden fixture's 11,520 records, every record
  * placed by eyes4s, the density and the representative trials. The FX
  * suite measures the whole change-to-canvas receipt against a generous
  * bound; this one holds the computation to the ticket's own 250 ms, as
  * the median of repeated runs after warm-up, so one slow run on a loaded
  * machine does not decide it.
  */
class GeometryPicturesTimingSuite extends munit.FunSuite:

  /** The ticket's redraw target. */
  val RedrawTarget: Double = 250.0

  test("the pictures of the 11,520 golden records take at most 250 ms (median of 9 runs)") {
    val spec = StoryMoments.t2
      .flatMap(_.dataset(StoryMoments.r3).toRight("no r3"))
      .fold(fail(_), identity)
    val bytes =
      IArray.unsafeFromArray(Files.readAllBytes(GoldenTrials.golden.resolve("fixations.csv")))
    val positions = SourcePositions.read(spec, bytes).fold(p => fail(p.message), identity)
    assertEquals(positions.positions.size, 11520)
    val key = PicturesKey(
      spec.id,
      spec.geometry,
      spec.admission,
      PositionsKey(spec.id, ByteDigest.sha256(bytes), spec.mapping),
      None,
      None,
      None
    )
    def once(): Double =
      val start = System.nanoTime()
      GeometryPictures.of(key, spec, positions).fold(p => fail(p.message), identity)
      (System.nanoTime() - start) / 1e6
    (1 to 3).foreach(_ => once())
    val runs   = Vector.fill(9)(once()).sorted
    val median = runs(runs.size / 2)
    println(
      f"GeometryPictures.of over 11,520 records: median $median%.1f ms (${runs.map(r => f"$r%.0f").mkString(", ")})"
    )
    assert(
      median <= RedrawTarget,
      s"median $median ms > $RedrawTarget ms: ${runs.mkString(", ")}"
    )
  }
