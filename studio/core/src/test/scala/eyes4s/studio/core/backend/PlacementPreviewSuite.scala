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

package eyes4s.studio.core.backend

import io.circe.Json
import io.circe.syntax.*

/** The placement preview's parts are refused when they disagree with
  * themselves (review-s55 C3): a tally's counts, a density's shape and
  * counts, and a preview whose tallies are not its records'. A frame that
  * decodes to such a value is refused too, so a bad backend answer is never
  * drawn.
  */
class PlacementPreviewSuite extends munit.FunSuite:
  import ProtocolSamples.{placementPreview, query}

  private val r3    = DatasetRevision(3)
  private val other = TrialKey("P05", Phase.Retrieval, "ret_04", 1)

  private def right[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  test("a tally counts no more records outside than it has, none negative") {
    assert(TrialPlacement.of(query, 2, 1, 1).isRight)
    assertEquals(
      TrialPlacement.of(query, 2, 2, 1),
      Left(PlacementError.TallyCounts(query, 2, 2, 1))
    )
    assertEquals(
      TrialPlacement.of(query, -1, 0, 0),
      Left(PlacementError.TallyCounts(query, -1, 0, 0))
    )
    assertEquals(
      PlacementError.TallyCounts(query, 2, 2, 1).message,
      "The tally of P17 · ret_07 counts 2 records, 2 outside the image frame and 1 outside " +
        "the screen."
    )
  }

  test("a density has columns × rows finite, non-negative counts") {
    assert(PlacementDensityGrid.of(2, 1, Vector(1.5, 0.5), 2).isRight)
    assertEquals(
      PlacementDensityGrid.of(0, 1, Vector.empty, 0),
      Left(PlacementError.EmptyGrid(0, 1))
    )
    assertEquals(
      PlacementDensityGrid.of(2, 1, Vector(1.0), 1),
      Left(PlacementError.CellCount(2, 1, 1))
    )
    assertEquals(
      PlacementDensityGrid.of(2, 1, Vector(1.0, -1.0), 1),
      Left(PlacementError.CellNotCount(1, -1.0))
    )
    assertEquals(
      PlacementDensityGrid.of(2, 1, Vector(1.0, Double.NaN), 1).left.map(_.productPrefix),
      Left("CellNotCount")
    )
  }

  test("a preview's tallies are its records', one per trial, and its density bins them all") {
    val p       = placementPreview
    val grid    = p.density
    val tallied = p.trials.head
    def of(
        trials: Vector[TrialPlacement] = p.trials,
        density: PlacementDensityGrid = grid,
        records: Vector[PlacedRecord] = p.records
    ) = PlacementPreview.of(r3, records, p.unplaced, trials, density)
    assertEquals(of(), Right(p))
    // The record off the screen counted as outside the frame instead.
    val wrong = right(TrialPlacement.of(query, 2, 1, 0))
    assertEquals(
      of(trials = Vector(wrong)),
      Left(PlacementError.TallyMismatch(r3, wrong, 2, 0, 1))
    )
    assertEquals(
      of(trials = Vector(tallied, tallied)),
      Left(PlacementError.TallyRepeated(r3, query))
    )
    assertEquals(of(trials = Vector.empty), Left(PlacementError.TallyMissing(r3, query)))
    // A trial with no records has a tally of nothing.
    val empty = right(TrialPlacement.of(other, 0, 0, 0))
    assert(of(trials = p.trials :+ empty).isRight)
    val fewer = right(PlacementDensityGrid.of(2, 1, Vector(1.5, 0.5), 1))
    assertEquals(of(density = fewer), Left(PlacementError.PlacedCount(r3, 1, 2)))
    assertEquals(
      PlacementError.TallyMismatch(r3, wrong, 2, 0, 1).message,
      "The placement of r3 tallies P17 · ret_07 as 2 records (1 outside the image frame, 0 " +
        "outside the screen); its records are 2 (0, 1)."
    )
  }

  test("a frame that decodes to a preview disagreeing with itself is refused") {
    val wire = placementPreview.asJson
    assertEquals(wire.as[PlacementPreview], Right(placementPreview))
    def patched(f: Json => Json) = f(wire).as[PlacementPreview]
    val badTally                 = patched(
      _.hcursor
        .downField("trials")
        .downArray
        .downField("outsideWindow")
        .withFocus(_ => 1.asJson)
        .top
        .get
    )
    assert(badTally.left.exists(_.getMessage.contains("tallies P17 · ret_07")), badTally)
    val badGrid = patched(
      _.hcursor.downField("density").downField("columns").withFocus(_ => 3.asJson).top.get
    )
    assert(badGrid.left.exists(_.getMessage.contains("2 counts for 3 × 1 cells")), badGrid)
    val badCounts = patched(
      _.hcursor
        .downField("trials")
        .downArray
        .downField("records")
        .withFocus(_ => (-1).asJson)
        .top
        .get
    )
    assert(badCounts.left.exists(_.getMessage.contains("counts -1 records")), badCounts)
  }
