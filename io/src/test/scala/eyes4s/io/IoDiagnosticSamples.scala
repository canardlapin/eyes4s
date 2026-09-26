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

package eyes4s.io

import eyes4s.laws.*
import eyes4s.plan.*
import eyes4s.plan.DiagnosticSamples.generated

/** One generated value of every case of every io and laws error family,
  * projected through their public `Diagnose` instances, in catalog order.
  */
object IoDiagnosticSamples:
  import IoDiagnostics.given
  import LawsDiagnostics.given

  given DiagnosticExample[Sha256] = DiagnosticExample.of(seed => Sha256.ofUtf8(s"digest-$seed"))

  // Nested errors, one explicit case each (see DiagnosticExample).
  given DiagnosticExample[TidyCsvError] =
    DiagnosticExample.of(seed => TidyCsvError.WrongColumnCount(seed, seed + 1, seed + 2))
  given DiagnosticExample[TidyResultError] =
    DiagnosticExample.of(seed => TidyResultError.BlankParticipant(s" $seed"))
  given DiagnosticExample[DelimitedSchemaError] =
    DiagnosticExample.of(seed => DelimitedSchemaError.BlankColumnName(seed))
  given DiagnosticExample[AscSourceLineError] =
    DiagnosticExample.of(seed => AscSourceLineError.NonPositiveLineLimit(seed))
  given DiagnosticExample[ContrastExportError] = DiagnosticExample.of(seed =>
    ContrastExportError.Components(Vector(s"a$seed"), Vector(s"b$seed"))
  )
  given DiagnosticExample[eyes4s.codec.CodecError] = DiagnosticExample.of(seed =>
    eyes4s.codec.CodecError.Reconstruction(
      eyes4s.design.ReconstructionError
        .Denominator(DiagnosticSamples.k1, seed, seed + 1, seed + 2)
    )
  )
  given DiagnosticExample[DetectorValidationError] =
    DiagnosticExample.of(seed => DetectorValidationError.NoMatchedEvents(seed, seed + 1))
  given DiagnosticExample[SyntheticGenerationError] =
    DiagnosticExample.of(seed => SyntheticGenerationError.InvalidClockScale(seed + 0.5))

  val io: Vector[FamilySamples] = Vector(
    generated[FixationImportError]("FixationImportError"),
    generated[FixationRowError]("FixationRowError"),
    generated[TidyCsvError]("TidyCsvError"),
    generated[TidyResultError]("TidyResultError"),
    generated[ContrastExportError]("ContrastExportError"),
    generated[ResultExportError]("ResultExportError"),
    generated[TemplateCsvError]("TemplateCsvError"),
    generated[DelimitedSchemaError]("DelimitedSchemaError"),
    generated[PsychologyWorkflowError]("PsychologyWorkflowError"),
    generated[Sha256Error]("Sha256Error"),
    generated[Edf2AscProvenanceError]("Edf2AscProvenanceError"),
    generated[AscSampleMaterializationError]("AscSampleMaterializationError"),
    generated[AscSourceLineError]("AscSourceLineError"),
    generated[AscStreamConfigurationError]("AscStreamConfigurationError"),
    generated[AscNativeTimelineError]("AscNativeTimelineError"),
    generated[AscPerformanceValidationError]("AscPerformanceValidationError"),
    generated[EyeLinkAscSessionConfigError]("EyeLinkAscSessionConfigError"),
    generated[EyeLinkOracleError]("EyeLinkOracleError"),
    generated[EyeLinkConformanceError]("EyeLinkConformanceError"),
    generated[EyeLinkCorpusError]("EyeLinkCorpusError")
  )

  val laws: Vector[FamilySamples] = Vector(
    generated[DetectorValidationError]("DetectorValidationError"),
    generated[SyntheticGenerationError]("SyntheticGenerationError"),
    generated[ValidationArtifactError]("ValidationArtifactError")
  )

  /** Every portable family of every catalog, in documented order. */
  val all: Vector[FamilySamples] =
    DiagnosticSamples.all ++ eyes4s.codec.CodecDiagnosticSamples.all ++ laws ++ io
