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

package eyes4s.examples

import eyes4s.codec.*
import eyes4s.compare.Similarity
import eyes4s.core.Weight
import eyes4s.design.*
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** Exact source for docs/REPETITION_STUDIES.md, compiled on JVM and Scala.js.
  * Admission is separate so rejected rows can be inspected before requiring a
  * complete study. Geometry is supplied by the caller, not guessed from samples.
  */
object RepetitionGuide:
  type Error = FixationImportError | PlanError | CodecError | ContrastExportError

  final case class Output(
      input: StudyInput[StudyKey, Px],
      savedPlan: String,
      result: StudyResult[StudyKey, Px, Similarity, SignedDifference],
      table: TidyCsvDocument
  )

  def admit(
      csv: String,
      frame: Frame[Px]
  ): Either[FixationImportError, FixationImport[StudyKey, Px]] =
    for
      columns <- FixationColumns.of(
        "fixation",
        "x_px",
        "y_px",
        "onset_us",
        "duration_us",
        "sample_count"
      )
      keys     <- FixationKeyReader.study("participant", "image", "phase")
      imported <- FixationCsv.read(csv, columns, keys, frame, TimestampUnit.Microseconds)
    yield imported

  /** A single directed phase contrast, persisted with the existing StudyCodec.
    * The saved route uses exhaustive controls. Finite-cap all-occasion designs
    * use RepetitionDesign directly and are not silently saved as this plan.
    */
  def run(
      imported: FixationImport[StudyKey, Px],
      grid: Grid[Px],
      focalPhase: String,
      referencePhase: String
  ): Either[Error, Output] =
    for
      input <- imported.requireComplete
      plan  <- StudyPlan.cosine(
        input.reference,
        grid,
        focalPhase,
        referencePhase,
        Weight.Duration,
        Vector(StudyEstimate.Binned()),
        FailurePolicy.RequireAll
      )
      persistence = StudyCodecs.cosine[Px]
      json     <- persistence.codec.encode(plan)
      restored <- persistence.codec.parse(json.noSpaces)
      result   <- restored.run(input)
      table    <- ContrastCsv.document(restored, result, persistence, ScoreColumns.similarity)
    yield Output(input, json.spaces2, result, table)

  def message(error: Error): String = error match
    case e: FixationImportError => e.message
    case e: PlanError           => e.message
    case e: CodecError          => e.message
    case e: ContrastExportError => e.message
