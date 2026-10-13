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

package eyes4s.studio.app.geometry

import eyes4s.studio.core.backend.PlacementDensityGrid
import eyes4s.studio.core.fixture.{FakePlacement, StoryMoments}

/** The placement density's levels (DESIGN_SPEC section 15.1, review ticket
  * 23): every non-empty cell is drawn; the levels start at 1, s, 4s and 16s
  * records, with the step s anchored at the median non-empty cell; the
  * levels are computed once from the revision's whole grid. The golden
  * fixture's numbers are the worked example of section 15.1.
  */
class DensityLevelsSuite extends munit.FunSuite:

  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private def grid(columns: Int, rows: Int, counts: Vector[Double]): PlacementDensityGrid =
    get(PlacementDensityGrid.of(columns, rows, counts, counts.sum.toInt))

  test("the step is the largest power of 4, at least 4, whose half the median reaches") {
    val steps =
      Vector(1L, 2L, 7L, 8L, 31L, 32L, 300L).map(m => m -> DensitySteps.anchored(m).step)
    assertEquals(
      steps,
      Vector(1L -> 4L, 2L -> 4L, 7L -> 4L, 8L -> 16L, 31L -> 16L, 32L -> 64L, 300L -> 256L)
    )
    assertEquals(DensitySteps.anchored(300L).starts, Vector(1L, 256L, 1024L, 4096L))
  }

  test("the median is the lower middle of the non-empty cells; empty cells do not count") {
    // Non-empty counts 1, 8, 8, 9: the lower middle is 8, so s = 16. With 1,
    // 2, 8, 9 the lower middle (2) gives 4, where the upper (8) would give 16.
    assertEquals(DensitySteps.of(Vector(0L, 9L, 1L, 0L, 8L, 8L)).step, 16L)
    assertEquals(DensitySteps.of(Vector(9L, 0L, 2L, 1L, 8L)).step, 4L)
    assertEquals(DensitySteps.of(Vector(0L, 0L)), DensitySteps.Default)
  }

  test("a cell's level is the highest level whose start it reaches; empty is 0") {
    val s4 = DensitySteps.Default
    assertEquals(
      Vector(0L, 1L, 3L, 4L, 15L, 16L, 63L, 64L, 1000L).map(s4.level),
      Vector(0, 1, 1, 2, 2, 3, 3, 4, 4)
    )
  }

  test("counts are whole records: rounded half up, and at least 1 above zero") {
    assertEquals(
      Vector(0.0, 0.2, 0.5, 1.49, 1.5, 3.0).map(DensityPicture.count),
      Vector(0L, 1L, 1L, 1L, 2L, 3L)
    )
  }

  test("no record on screen: the default steps and no cell drawn") {
    val picture = DensityPicture.of(grid(4, 2, Vector.fill(8)(0.0)))
    assertEquals(picture.steps, DensitySteps.Default)
    assertEquals(picture.steps.starts, Vector(1L, 4L, 16L, 64L))
    assertEquals(picture.levels, Vector.fill(8)(0))
  }

  test("one non-empty cell anchors the steps: one record is level 1, 300 records level 2") {
    val one = DensityPicture.of(grid(2, 1, Vector(0.0, 1.0)))
    assertEquals((one.steps.step, one.levels), (4L, Vector(0, 1)))
    val many = DensityPicture.of(grid(2, 1, Vector(300.0, 0.0)))
    assertEquals((many.steps.step, many.levels), (256L, Vector(2, 0)))
  }

  test("a singleton is never hidden, however full the fullest cell") {
    // The share floor (5% of the fullest cell) hid this singleton.
    val picture = DensityPicture.of(grid(3, 1, Vector(1.0, 100.0, 4.0)))
    assertEquals(picture.steps.step, 4L)
    assertEquals(picture.levels, Vector(1, 4, 2))
  }

  // --- the golden fixture (DESIGN_SPEC section 15.1, worked example) ----------

  private lazy val goldenGrid: PlacementDensityGrid =
    val r3 = get(
      StoryMoments.t1.flatMap(_.dataset(StoryMoments.r3).toRight("the story has no r3"))
    )
    // As imported: no recorded corrections.
    val spec = r3.copy(admission = r3.admission.copy(corrections = Vector.empty))
    get(FakePlacement.of(spec)).density

  private lazy val golden: DensityPicture = DensityPicture.of(goldenGrid)

  /** The index of the cell whose origin is (`x`, `y`) screen px. */
  private def cellAt(x: Int, y: Int): Int =
    val size = 1920 / golden.columns
    (y / size) * golden.columns + x / size

  /** The cell's (records, level). */
  private def at(x: Int, y: Int): (Long, Int) =
    val i = cellAt(x, y)
    (DensityPicture.count(goldenGrid.counts(i)), golden.levels(i))

  test("golden: 1,287 cells drawn, anchored at m = 4, so the levels start at 1, 4, 16, 64") {
    assertEquals((golden.columns, golden.rows, golden.records), (64, 36, 11520))
    assertEquals(golden.steps.starts, Vector(1L, 4L, 16L, 64L))
    val perLevel = (1 to DensityPicture.Levels).map(l => golden.levels.count(_ == l))
    assertEquals(perLevel.toVector, Vector(618, 364, 296, 9))
  }

  test("golden: the boundary cells fall on either side of each level start") {
    assertEquals(at(900, 210), (3L, 1))
    assertEquals(at(510, 450), (4L, 2))
    assertEquals(at(870, 360), (15L, 2))
    assertEquals(at(600, 390), (16L, 3))
    assertEquals(at(900, 540), (64L, 4))
    assertEquals(at(930, 540), (100L, 4)) // the fullest cell
    assertEquals(at(1140, 450), (8L, 2))  // record 7,214's cell
  }
