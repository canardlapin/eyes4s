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
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** This exact guide compiles and runs in the JVM and Scala.js test matrix. */
object StudyGuide:
  type Error = GeometryError | FixationImportError | PlanError | CodecError |
    ContrastExportError
  final case class Output(savedPlan: String, contrasts: TidyCsvDocument)

  def run(
      csv: String,
      estimates: Vector[StudyEstimate[Px]] = Vector(StudyEstimate.Binned())
  ): Either[Error, Output] =
    for
      frame   <- Frame.screen("matched-control-display", 2, 2)
      grid    <- Grid.over(frame, 2, 2)
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
      input    <- imported.requireComplete
      plan     <- StudyPlan.cosine(
        input.reference,
        grid,
        "recall",
        "encode",
        Weight.Duration,
        estimates,
        FailurePolicy.RequireAll
      )
      persistence = StudyCodecs.cosine[Px]
      saved    <- persistence.codec.encode(plan)
      restored <- persistence.codec.parse(saved.noSpaces)
      result   <- restored.run(input)
      table    <- ContrastCsv.document(restored, result, persistence, ScoreColumns.similarity)
    yield Output(saved.spaces2, table)

  def message(error: Error): String = error match
    case e: GeometryError       => e.message
    case e: FixationImportError => e.message
    case e: PlanError           => e.message
    case e: CodecError          => e.message
    case e: ContrastExportError => e.message
