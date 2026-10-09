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

package eyes4s.laws

import eyes4s.plan.*

/** The code table of the laws module's validation errors: detector
  * validation metrics, synthetic recording generation and the scientific
  * validation artifact. Codes are unique across every catalog.
  */
object LawsDiagnosticCatalog:
  import DiagnosticFamily.error

  val detectorValidation: DiagnosticFamily = error("detector-validation")(
    "LabelCountMismatch",
    "EmptyTruthDenominator",
    "EmptyPredictionDenominator",
    "NoMatchedEvents",
    "NoCentredEventPairs",
    "NoPeakVelocityPairs",
    "EventClockMismatch",
    "InvalidNumericMetric"
  )
  val syntheticGeneration: DiagnosticFamily = error("synthetic-generation")(
    "InvalidClockScale",
    "Frame",
    "Recording",
    "Geometry",
    "Support",
    "Time"
  )
  val validationArtifact: DiagnosticFamily = error("validation-artifact")(
    "EmptyBuildField",
    "InvalidBuildField",
    "InvalidSourceRevision",
    "EmptyOracleContent",
    "MissingOracleSchema",
    "Synthetic",
    "Metrics"
  )

  /** Every family, in the order documented. */
  val families: Vector[DiagnosticFamily] =
    Vector(detectorValidation, syntheticGeneration, validationArtifact)

  /** Every stable code, in catalog order. */
  val codes: Vector[DiagnosticCode] = families.flatMap(_.codes)

/** The laws module's [[eyes4s.plan.Diagnose Diagnose]] instances. Import `LawsDiagnostics.given`
  * for `Diagnostic.of` over these families.
  */
object LawsDiagnostics:
  import LawsDiagnosticCatalog as C

  given detectorValidation: Diagnose[DetectorValidationError, Nothing] =
    Diagnose.derived[DetectorValidationError, Nothing](C.detectorValidation)(_.message)
  given syntheticGeneration: Diagnose[SyntheticGenerationError, Nothing] =
    Diagnose.derived[SyntheticGenerationError, Nothing](C.syntheticGeneration)(_.message)
  given validationArtifact: Diagnose[ValidationArtifactError, Nothing] =
    Diagnose.derived[ValidationArtifactError, Nothing](C.validationArtifact)(_.message)
