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

package eyes4s.plan

import eyes4s.compare.*
import eyes4s.kernel.*

/** Location families' samples, kept beside their API so central catalog
  * enrollment cannot reduce their coverage to message-only checks.
  */
object LocationDiagnosticSamples:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => throw AssertionError(error), identity)
  private val trial     = get(LocationTrialId.of("trial"))
  private val other     = get(LocationTrialId.of("other"))
  private val source    = get(LocationSourceId.of("source"))
  private val frame     = get(Frame.screen("location-diagnostic", 2, 2))
  private val grid      = get(Grid.over(frame, 1, 1))
  private val otherGrid = get(
    Grid.over(get(Frame.screen("other-location-diagnostic", 2, 2)), 1, 1)
  )

  val all: Vector[FamilySamples] = Vector(
    DiagnosticSamples.family[LocationInputError]("LocationInputError")(
      LocationInputError.EmptyTrialId(""),
      LocationInputError.EmptySourceId(""),
      LocationInputError.EmptyInput,
      LocationInputError.EmptyTrial(trial),
      LocationInputError.NonPositiveSourceOrdinal(trial, source, 0),
      LocationInputError.DuplicateTrial(trial),
      LocationInputError.NonFinitePosition(trial, source, 1, Double.PositiveInfinity, 0),
      LocationInputError.PositionOutsideFrame(trial, source, 1, 2, 0, frame.id),
      LocationInputError.DuplicateProvenance(source, 1)
    ),
    DiagnosticSamples.family[LocationQueryError]("LocationQueryError")(
      LocationQueryError.EmptyQueryId(""),
      LocationQueryError.SameTrial("query", "focal", trial, "matched", trial)
    ),
    DiagnosticSamples.family[LocationPlanError]("LocationPlanError")(
      LocationPlanError.Grid(GeometryError.DegenerateGrid(0, 1)),
      LocationPlanError.EmptyInput,
      LocationPlanError.EmptyQueries,
      LocationPlanError.DuplicateQuery("query"),
      LocationPlanError.MissingTrial("query", "control", other)
    ),
    DiagnosticSamples.family[LocationMapError]("LocationMapError")(
      LocationMapError.Measure(trial, SurfaceError.DegenerateTotal(0)),
      LocationMapError.Binning(trial, GeometryError.DegenerateGrid(0, 1)),
      LocationMapError.Surface(trial, SurfaceError.DegenerateTotal(0))
    ),
    DiagnosticSamples.family[LocationRunError]("LocationRunError")(
      LocationRunError.Map(
        trial.value,
        "focal",
        trial,
        LocationMapError.Measure(trial, SurfaceError.DegenerateTotal(0))
      ),
      LocationRunError.Comparison(
        "query",
        "matched",
        trial,
        other,
        CompareError.Grids(SurfaceError.GridMismatch(grid.id, otherGrid.id))
      )
    ),
    DiagnosticSamples.family[LocationRestoreError]("LocationRestoreError")(
      LocationRestoreError.MapTrialSet(Vector(trial), Vector(other)),
      LocationRestoreError.InputHash(ContentHash.empty, ContentHash.ofString("input")),
      LocationRestoreError.PlanHash(ContentHash.empty, ContentHash.ofString("plan")),
      LocationRestoreError.MapFailureIdentity(trial, other),
      LocationRestoreError.BinLength(trial, 2, 1),
      LocationRestoreError.BinValue(trial, 0, Double.NaN),
      LocationRestoreError.BinTotal(trial, 2, 1),
      LocationRestoreError.RebuildMap(trial, SurfaceError.DegenerateTotal(0)),
      LocationRestoreError.OutcomeQuerySet(Vector("query"), Vector("other")),
      LocationRestoreError.ScoreWithoutMap("query", "matched", trial),
      LocationRestoreError.InvalidScore("query", "matched", 2),
      LocationRestoreError.NonFiniteDifference("query", Double.PositiveInfinity, 0),
      LocationRestoreError.FailureOperands(
        "query",
        "matched",
        LocationRunError.Map(
          "other",
          "control",
          other,
          LocationMapError.Measure(other, SurfaceError.DegenerateTotal(0))
        )
      )
    )
  )
