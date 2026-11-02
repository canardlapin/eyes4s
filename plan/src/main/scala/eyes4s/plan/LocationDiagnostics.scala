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

/** Diagnostic families for the location-only input and its explicit queries. */
object LocationDiagnostics:
  val input: DiagnosticFamily = DiagnosticFamily.error("location-input")(
    "EmptyTrialId",
    "EmptySourceId",
    "EmptyInput",
    "EmptyTrial",
    "NonPositiveSourceOrdinal",
    "DuplicateTrial",
    "NonFinitePosition",
    "PositionOutsideFrame",
    "DuplicateProvenance"
  )

  val query: DiagnosticFamily = DiagnosticFamily.error("location-query")(
    "EmptyQueryId",
    "SameTrial"
  )

  val plan: DiagnosticFamily = DiagnosticFamily.error("location-plan")(
    "Grid",
    "EmptyInput",
    "EmptyQueries",
    "DuplicateQuery",
    "MissingTrial"
  )

  val map: DiagnosticFamily = DiagnosticFamily.error("location-map")(
    "Measure",
    "Binning",
    "Surface"
  )

  val run: DiagnosticFamily = DiagnosticFamily.error("location-run")(
    "Map",
    "Comparison"
  )

  val restore: DiagnosticFamily = DiagnosticFamily.error("location-restore")(
    "MapTrialSet",
    "InputHash",
    "PlanHash",
    "MapFailureIdentity",
    "BinLength",
    "BinValue",
    "BinTotal",
    "RebuildMap",
    "OutcomeQuerySet",
    "ScoreWithoutMap",
    "InvalidScore",
    "NonFiniteDifference",
    "FailureOperands"
  )
