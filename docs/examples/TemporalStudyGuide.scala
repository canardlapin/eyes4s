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

import cats.syntax.all.*
import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy

/** Existing fixation input plus measured trial anchors/support; no origin is guessed. */
object TemporalStudyGuide:
  type Error = PlanError | TemporalStudyError | CodecError | ContrastExportError |
    GeometryError | TimeError
  final case class Output(savedPlan: String, tables: TemporalTables)
  def run(
      input: StudyInput[StudyKey, Px],
      epochs: Vector[(StudyKey, TrialEpoch)],
      grid: Grid[Px]
  ): Either[Error, Output] = for
    temporal <- TemporalStudyInput.of(input, epochs)
    sigma    <- Sigma.px(1.0)
    base     <- StudyPlan.cosine(
      input.reference,
      grid,
      "recall",
      "encode",
      Weight.Duration,
      Vector(StudyEstimate.Binned(), StudyEstimate.Gaussian(sigma, EdgePolicy.Truncate)),
      FailurePolicy.RequireAll
    )
    windows <- Vector(
      ("early", 0L, 300000L),
      ("middle", 300000L, 600000L),
      ("late", 600000L, 950000L),
      ("outside", 950000L, 1100000L)
    )
      .traverse { case (name, start, end) =>
        for
          relative <- Window.of(Span.micros(start), Span.micros(end)).left.map[Error](identity)
          window   <- StudyWindow.of(name, relative).left.map[Error](identity)
        yield window
      }
    first  <- RepetitionContrast.withinParticipant("recall-encode", "recall", "encode")
    second <- RepetitionContrast.withinParticipant("retest-recall", "retest", "recall")
    plan   <- TemporalStudyPlan.of(
      base,
      temporal.reference,
      windows,
      Vector(first, second),
      FixationBoundary.ClipDuration
    )
    schema <- DefinitionId.of("eyes4s.temporal-study", 1)
    persistence = new TemporalStudyCodec(schema, StudyCodecs.cosine[Px])
    saved    <- persistence.codec.encode(plan)
    restored <- persistence.codec.parse(saved.noSpaces)
    result   <- restored.run(temporal)
    tables   <- TemporalContrastCsv.document(
      restored,
      result,
      persistence,
      ScoreColumns.similarity
    )
  yield Output(saved.spaces2, tables)
