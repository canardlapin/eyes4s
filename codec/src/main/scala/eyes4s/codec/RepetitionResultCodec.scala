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

package eyes4s.codec

import cats.syntax.all.*
import eyes4s.compare.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

/** A completed repetition run with the plan that produced it. */
final case class RepetitionRun[K, U <: Unit2D](
    plan: RepetitionPlan[K, U],
    result: RepetitionPlanResult[K]
)

/** Versioned archive of one completed repetition run (CR4 S3), under the
  * caller's result schema: the plan document, through the plan codec, and
  * the matched and control analyses as their pair rows (scores or
  * comparison failures), pairing report, provenance and evaluation.
  *
  * The plan carries its supplied maps, so the run's input identity is the
  * plan's own: decoding rebuilds both analyses through
  * `DirectedPairwiseAnalysis.reconstruct` and the result through
  * `RepetitionPlanResult.reconstruct`, which refuses analyses another plan
  * or input computed. The archive carries a `RunStamp` whose plan and input
  * digests are both the plan's canonical digest (the plan carries its maps);
  * decoding refuses a stamp other than the embedded plan's. Stored under the
  * generic `analysis-result` role, the run's relation is
  * `AnalysisInputs.EmbeddedInPlan` ([[registration]]).
  */
final class RepetitionResultCodec[K, U <: Unit2D: UnitLabel](
    val schema: DefinitionId,
    val plans: VersionedCodec[RepetitionPlan[K, U]],
    keys: VersionedCodec[K]
):
  private val scores = StudyResultCodecs.similarity()

  val codec: VersionedCodec[RepetitionRun[K, U]] =
    VersionedCodec.checked[RepetitionRun[K, U]](schema)(write)(read)

  private def analysis(
      value: DirectedPairwiseAnalysis[K, K, CompareError, Similarity]
  ): Either[CodecError, Json] = for
    rows <- value.rows.zipWithIndex.traverse { case (row, index) =>
      (for
        left   <- keys.encode(row.left)
        right  <- keys.encode(row.right)
        result <- row.result match
          case Right(score) =>
            ResultWire.payload(scores, score).map(s => ResultWire.tagged("score", "score" -> s))
          case Left(error) =>
            Right(ResultWire.tagged("failure", "failure" -> ResultWire.compareError(error)))
      yield Json.obj("left" -> left, "right" -> right, "result" -> result)).left
        .map(Wire.at(s"rows[$index]"))
    }
    diagnostics <- ResultWire
      .pairingReport(keys)(value.diagnostics)
      .left
      .map(Wire.at("diagnostics"))
    evaluation <- ResultWire.evaluationInfo[U](value.evaluation).left.map(Wire.at("evaluation"))
  yield Json.obj(
    "rows"        -> Json.arr(rows*),
    "diagnostics" -> diagnostics,
    "provenance"  -> ResultWire.provenance(value.provenance),
    "evaluation"  -> evaluation
  )

  private def readAnalysis(
      json: Json
  ): Either[CodecError, DirectedPairwiseAnalysis[K, K, CompareError, Similarity]] = for
    entries <- Wire.field[Vector[Json]](json, "rows")
    rows    <- entries.zipWithIndex.traverse { case (entry, index) =>
      (for
        left    <- ResultWire.keyed(keys, entry, "left")
        right   <- ResultWire.keyed(keys, entry, "right")
        outcome <- Wire.field[Json](entry, "result")
        kind    <- ResultWire.kind(outcome)
        result  <- kind match
          case "score" =>
            Wire
              .field[Json](outcome, "score")
              .flatMap(ResultWire.unwrap(scores, _))
              .map(Right(_))
          case "failure" =>
            Wire
              .field[Json](outcome, "failure")
              .flatMap(ResultWire.readCompareError)
              .map(Left(_))
          case other => Left(ResultWire.unknown(outcome, "pair outcome", other))
      yield PairScore[K, K, CompareError, Similarity](left, right, result)).left
        .map(Wire.at(s"rows[$index]"))
    }
    diagnostics <- Wire
      .field[Json](json, "diagnostics")
      .flatMap(ResultWire.readPairingReport(keys))
      .left
      .map(Wire.at("diagnostics"))
    provenance <- Wire.field[Json](json, "provenance").flatMap(ResultWire.readProvenance)
    evaluation <- Wire
      .field[Json](json, "evaluation")
      .flatMap(ResultWire.readEvaluationInfo[U])
      .left
      .map(Wire.at("evaluation"))
    analysis <- DirectedPairwiseAnalysis
      .reconstruct(rows, diagnostics, provenance, evaluation)
      .left
      .map(e => CodecError.Reconstruction(e))
  yield analysis

  /** The run's stamp: the plan's canonical digest, which is also its input's,
    * since the plan carries its maps.
    */
  def stamp(
      plan: RepetitionPlan[K, U]
  ): Either[CodecError, RunStamp[RepetitionPlan[K, U], RepetitionPlan[K, U]]] =
    RunStamp.of(plan, plan, plans, plans)

  private def write(run: RepetitionRun[K, U]): Either[CodecError, Json] = for
    stamped  <- stamp(run.plan)
    plan     <- plans.encode(run.plan)
    matched  <- analysis(run.result.matched).left.map(Wire.at("matched"))
    controls <- analysis(run.result.controls).left.map(Wire.at("controls"))
  yield Json.obj(
    "runStamp" -> RunStampWire.write(stamped),
    "plan"     -> plan,
    "matched"  -> matched,
    "controls" -> controls
  )

  private def read(json: Json): Either[CodecError, RepetitionRun[K, U]] = for
    claim <- Wire
      .field[Json](json, "runStamp")
      .flatMap(RunStampWire.read[RepetitionPlan[K, U], RepetitionPlan[K, U]])
      .left
      .map(Wire.at("runStamp"))
    plan    <- Wire.field[Json](json, "plan").flatMap(plans.decode).left.map(Wire.at("plan"))
    current <- stamp(plan)
    _       <- claim
      .check(current, Vector.empty)
      .left
      .map(CodecError.Stamp(_))
    matched <- Wire
      .field[Json](json, "matched")
      .flatMap(readAnalysis)
      .left
      .map(Wire.at("matched"))
    controls <- Wire
      .field[Json](json, "controls")
      .flatMap(readAnalysis)
      .left
      .map(Wire.at("controls"))
    result <- RepetitionPlanResult
      .reconstruct(plan, matched, controls)
      .left
      .map(CodecError.RepetitionResult(_))
  yield RepetitionRun(plan, result)

  /** The plan as a stored analysis plan; it embeds its maps, whose identity
    * is its input hash.
    */
  final class LoadedPlan(val plan: RepetitionPlan[K, U]) extends LoadedAnalysisPlan:
    val schema: DefinitionId    = plans.schema
    def description             = plan.description
    def encode                  = plans.encode(plan)
    override def embeddedInputs = Vector(plan.inputHash)

  /** The run as a stored analysis result, computed on its plan's maps. */
  final class LoadedRun(val run: RepetitionRun[K, U]) extends LoadedAnalysisResult:
    val schema: DefinitionId = RepetitionResultCodec.this.schema
    def description          = run.plan.description
    def inputs               = Vector(run.plan.inputHash)
    def encode               = codec.encode(run)

  /** The plan and run archives, for an [[AnalysisRegistry]]: the plan embeds
    * its input, so the run's relation is `AnalysisInputs.EmbeddedInPlan`.
    */
  def registration: AnalysisRegistration = AnalysisRegistration(
    plans.schema,
    schema,
    json => plans.decode(json).map(LoadedPlan(_)),
    json => codec.decode(json).map(LoadedRun(_)),
    embedsInput = true
  )
