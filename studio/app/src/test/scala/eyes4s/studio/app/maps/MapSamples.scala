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

package eyes4s.studio.app.maps

import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.selection.ScaleIndex

/** Map grids for the map tests (ticket S4.4): test values, not results. */
object MapSamples:

  /** The fixture recipe's density grid: 64 × 48 cells. */
  val Columns: Int = 64
  val Rows: Int    = 48

  def id(trial: String, scale: Int, occurrence: Int = 1): MapId =
    MapId(
      TrialKey("P17", Phase.Retrieval, trial, occurrence),
      ScaleIndex.of(scale).fold(e => throw new AssertionError(e.message), identity)
    )

  def grid(map: MapId, cells: Vector[Option[Double]], levels: Vector[Double] = Vector.empty) =
    MapGrid
      .of(map, Columns, Rows, cells, levels)
      .fold(e => throw new AssertionError(e.message), identity)

  /** A smooth bump of mass peaking at 1 near the middle, missing in one
    * corner cell and exactly zero along the left edge.
    */
  val bump: Vector[Option[Double]] =
    Vector.tabulate(Columns * Rows) { i =>
      val x = i % Columns
      val y = i / Columns
      if x == Columns - 1 && y == 0 then None
      else if x == 0 then Some(0.0)
      else
        val dx = (x - 32.0) / 12.0
        val dy = (y - 24.0) / 9.0
        Some(math.exp(-(dx * dx + dy * dy) / 2.0))
    }

  /** A difference map: −0.4 at the left, +0.6 at the right, zero in the middle column. */
  val difference: Vector[Option[Double]] =
    Vector.tabulate(Columns * Rows) { i =>
      val x = i % Columns
      if x == 32 then Some(0.0)
      else Some(if x < 32 then -0.4 * (32 - x) / 32.0 else 0.6 * (x - 32) / 31.0)
    }
