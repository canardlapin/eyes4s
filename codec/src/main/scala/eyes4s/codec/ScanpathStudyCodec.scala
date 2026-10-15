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

object ScanpathStudyDefinitions:
  val plan: DefinitionId   = DefinitionId.builtIn("eyes4s.scanpath-study-plan", 1)
  val result: DefinitionId = DefinitionId.builtIn("eyes4s.scanpath-study-result", 1)

final class ScanpathStudyArchive[K, U <: Unit2D] private[codec] (
    val plan: ScanpathStudyPlan[K, U],
    val result: ScanpathStudyResult[K, U]
)
object ScanpathStudyArchive:
  def run[K, U <: Unit2D](
      plan: ScanpathStudyPlan[K, U]
  ): Either[PlanError, ScanpathStudyArchive[K, U]] =
    plan.run.map(new ScanpathStudyArchive(plan, _))
  def completed[K, U <: Unit2D](
      plan: ScanpathStudyPlan[K, U],
      result: ScanpathStudyResult[K, U]
  ): Either[CodecError, ScanpathStudyArchive[K, U]] =
    Either.cond(
      plan.inputHash == result.inputHash && plan.planHash == result.planHash,
      new ScanpathStudyArchive(plan, result),
      CodecError.Field(
        "scanpathStudyResult",
        Json.fromString(result.planHash.render),
        s"Expected input=${plan.inputHash.render} plan=${plan.planHash.render}; found input=${result.inputHash.render} plan=${result.planHash.render}"
      )
    )

/** Complete typed input and method parameters are stamped before result replay.
  * Encoding a completed archive projects its existing ledgers without comparing again.
  */
final class ScanpathStudyCodec[K, U <: Unit2D: UnitLabel](
    val layout: StudyLayout[K],
    val keys: VersionedCodec[K]
):
  private val inputs = new StudyInputCodec[K, U](
    DefinitionId.studyInput,
    DefinitionId.admissionLedger,
    layout,
    keys
  ).input
  private val methods = new ScanpathMethodCodec[U].codec
  private def exact(json: Json, field: String, expected: String): Either[CodecError, Unit] =
    Wire
      .field[String](json, field)
      .flatMap(found =>
        Either.cond(
          found == expected,
          (),
          CodecError.Field(field, json, s"Expected $expected, found=$found")
        )
      )
  val plans: VersionedCodec[ScanpathStudyPlan[K, U]] =
    VersionedCodec.checked[ScanpathStudyPlan[K, U]](ScanpathStudyDefinitions.plan) { plan =>
      for input <- inputs.encode(plan.input); method <- methods.encode(plan.method)
      yield Json.obj(
        "layout"               -> Wire.id(layout.id),
        "unit"                 -> Json.fromString(summon[UnitLabel[U]].symbol),
        "preparationAdmission" -> Json.fromString(
          ScanpathPreparationConvention.ConservativeRawPairsV1.toString
        ),
        "input"          -> input,
        "frame"          -> DomainWire.frame(plan.frame),
        "focalPhase"     -> Json.fromString(plan.focalPhase),
        "referencePhase" -> Json.fromString(plan.referencePhase),
        "method"         -> method,
        "pairing"        -> StudyWire.pairing(plan.pairing),
        "controls"       -> ResultWire.selection(plan.controls),
        "failurePolicy"  -> StudyWire.policy(plan.failurePolicy),
        "budget"         -> Json.obj(
          "sourceRows"     -> Json.fromInt(plan.budget.maxSourceRows),
          "candidatePairs" -> ResultWire.long(plan.budget.maxCandidatePairs),
          "selectedPairs"  -> Json.fromInt(plan.budget.maxSelectedPairs)
        ),
        "inputHash" -> Json.fromString(plan.inputHash.render),
        "planHash"  -> Json.fromString(plan.planHash.render)
      )
    } { json =>
      for
        _ <- Wire.requireId(json, "layout", layout.id)
        _ <- exact(json, "unit", summon[UnitLabel[U]].symbol)
        _ <- exact(
          json,
          "preparationAdmission",
          ScanpathPreparationConvention.ConservativeRawPairsV1.toString
        )
        input      <- Wire.field[Json](json, "input").flatMap(inputs.decode)
        frame      <- Wire.field[Json](json, "frame").flatMap(DomainWire.readFrame[U])
        focal      <- Wire.field[String](json, "focalPhase")
        reference  <- Wire.field[String](json, "referencePhase")
        method     <- Wire.field[Json](json, "method").flatMap(methods.decode)
        pairing    <- Wire.field[Json](json, "pairing").flatMap(StudyWire.readPairing)
        controls   <- Wire.field[Json](json, "controls").flatMap(ResultWire.readSelection)
        policy     <- Wire.field[Json](json, "failurePolicy").flatMap(StudyWire.readPolicy)
        limits     <- Wire.field[Json](json, "budget")
        sourceRows <- Wire.field[Int](limits, "sourceRows")
        candidates <- ResultWire.readLong(limits, "candidatePairs")
        selected   <- Wire.field[Int](limits, "selectedPairs")
        budget     <- PairScheduleBudget
          .of(sourceRows, candidates, selected)
          .left
          .map(e => CodecError.Field("budget", limits, e.message))
        plan <- ScanpathStudyPlan
          .of(layout, input, frame, focal, reference, method, pairing, controls, policy, budget)
          .left
          .map(CodecError.Definition.apply)
        _ <- exact(json, "inputHash", plan.inputHash.render)
        _ <- exact(json, "planHash", plan.planHash.render)
      yield plan
    }
  def stamp(
      plan: ScanpathStudyPlan[K, U]
  ): Either[CodecError, RunStamp[ScanpathStudyPlan[K, U], ScanpathStudyPlan[K, U]]] =
    RunStamp.of(plan, plan, plans, plans)

  private def fixationError(error: FixationComparisonError): Json =
    import ResultWire.{tagged, double, long}
    error match
      case FixationComparisonError.Parameter(name, value) =>
        tagged("parameter", "name" -> Json.fromString(name), "value" -> double(value))
      case FixationComparisonError.Frames(error) =>
        tagged("frames", "error" -> ResultWire.geometryError(error))
      case FixationComparisonError.Clock(error) =>
        tagged("clock", "error" -> ResultWire.timeError(error))
      case FixationComparisonError.EmptyQueries => tagged("emptyQueries")
      case FixationComparisonError.MissingSupport(requested, contributing) =>
        tagged(
          "missingSupport",
          "requested"    -> Json.fromInt(requested),
          "contributing" -> Json.fromInt(contributing)
        )
      case FixationComparisonError.MatrixSize(left, right, maximum) =>
        tagged(
          "matrixSize",
          "left"    -> Json.fromInt(left),
          "right"   -> Json.fromInt(right),
          "maximum" -> Json.fromInt(maximum)
        )
      case FixationComparisonError.Numerical(left, right, value) =>
        tagged(
          "numerical",
          "left"  -> Json.fromInt(left),
          "right" -> Json.fromInt(right),
          "value" -> double(value)
        )
      case FixationComparisonError.NonConvergence(
            left,
            right,
            iterations,
            residual,
            tolerance
          ) =>
        tagged(
          "nonConvergence",
          "left"       -> Json.fromInt(left),
          "right"      -> Json.fromInt(right),
          "iterations" -> Json.fromInt(iterations),
          "residual"   -> double(residual),
          "tolerance"  -> double(tolerance)
        )
      case FixationComparisonError.SolverNumerical(left, right, cost, residual) =>
        tagged(
          "solverNumerical",
          "left"     -> Json.fromInt(left),
          "right"    -> Json.fromInt(right),
          "cost"     -> double(cost),
          "residual" -> double(residual)
        )
      case FixationComparisonError.WorkLimit(left, right, iterations, residual, maximum) =>
        tagged(
          "workLimit",
          "left"       -> Json.fromInt(left),
          "right"      -> Json.fromInt(right),
          "iterations" -> Json.fromInt(iterations),
          "residual"   -> double(residual),
          "maximum"    -> long(maximum)
        )
      case FixationComparisonError.Score(error) =>
        tagged("score", "error" -> ResultWire.comparisonValueError(error))
  private[eyes4s] def error(error: ScanpathComparisonError): Json =
    import ResultWire.{tagged, long}
    error match
      case ScanpathComparisonError.Parameter(name, value) =>
        tagged("parameter", "name" -> Json.fromString(name), "value" -> long(value))
      case ScanpathComparisonError.AlignmentSize(left, right, cells, maximum) =>
        tagged(
          "alignmentSize",
          "left"    -> Json.fromInt(left),
          "right"   -> Json.fromInt(right),
          "cells"   -> long(cells),
          "maximum" -> long(maximum)
        )
      case ScanpathComparisonError.Alignment(underlying) =>
        tagged("alignment", "error" -> ResultWire.compareError(underlying))
      case ScanpathComparisonError.Fixations(underlying) =>
        tagged("fixations", "error" -> fixationError(underlying))
  private[eyes4s] def overlapError(error: OverlapFailure): Json =
    error match
      case OverlapFailure.Missing(left, right) =>
        ResultWire.tagged(
          "missing",
          "left"  -> left.fold(Json.Null)(v => Json.fromString(v.toString)),
          "right" -> right.fold(Json.Null)(v => Json.fromString(v.toString))
        )
      case OverlapFailure.NonfiniteDistance(x, y, u, v) =>
        ResultWire.tagged(
          "nonfiniteDistance",
          "leftX"  -> ResultWire.double(x),
          "leftY"  -> ResultWire.double(y),
          "rightX" -> ResultWire.double(u),
          "rightY" -> ResultWire.double(v)
        )
  private def outcome[E, A](value: Either[E, A])(failure: E => Json, success: A => Json): Json =
    value.fold(
      e => ResultWire.tagged("failure", "error" -> failure(e)),
      a => ResultWire.tagged("success", "value" -> success(a))
    )
  private def output(value: ScanpathMethodOutput): Json = value match
    case ScanpathMethodOutput.MultiMatch(score) =>
      ResultWire.tagged(
        "multiMatch",
        "shape"     -> ResultWire.double(score.shape),
        "direction" -> ResultWire.double(score.direction),
        "length"    -> ResultWire.double(score.length),
        "position"  -> ResultWire.double(score.position),
        "duration"  -> ResultWire.double(score.duration)
      )
    case ScanpathMethodOutput.Overlap(value) =>
      ResultWire.tagged(
        "overlap",
        "policy"       -> Json.fromString(value.policy.toString),
        "requested"    -> Json.fromInt(value.requested),
        "contributing" -> Json.fromInt(value.contributing),
        "overlaps"     -> Json.fromInt(value.overlaps),
        "similarity"   -> outcome(value.similarity)(
          fixationError,
          s => ResultWire.double(s.value)
        ),
        "queries" -> Json.arr(
          value.rows.map(row =>
            Json.obj(
              "time"     -> ResultWire.long(row.time.toMicros),
              "hit"      -> Json.fromBoolean(row.hit),
              "distance" -> outcome(row.distance)(overlapError, ResultWire.double)
            )
          )*
        )
      )
    case ScanpathMethodOutput.Transport(value) =>
      ResultWire.tagged(
        "transport",
        "squaredCost" -> ResultWire.double(value.squaredCost),
        "rootCost"    -> ResultWire.double(value.rootCost),
        "iterations"  -> Json.fromInt(value.iterations),
        "residual"    -> ResultWire.double(value.marginalResidual),
        "leftCount"   -> Json.fromInt(value.leftCount),
        "rightCount"  -> Json.fromInt(value.rightCount),
        "similarity"  -> outcome(value.similarity)(
          fixationError,
          s => ResultWire.double(s.value)
        )
      )

  private def pairs(value: ScanpathPairs[K]): Either[CodecError, Json] = for
    report     <- ResultWire.pairingReport(keys)(value.diagnostics)
    evaluation <- ResultWire.evaluationInfo[U](value.evaluation)
    rows       <- value.rows.traverse(row =>
      for left <- keys.encode(row.left); right <- keys.encode(row.right)
      yield Json
        .obj("left" -> left, "right" -> right, "result" -> outcome(row.result)(error, output))
    )
  yield Json.obj(
    "rows"       -> Json.arr(rows*),
    "pairing"    -> report,
    "provenance" -> ResultWire.provenance(value.provenance),
    "evaluation" -> evaluation
  )

  private def mean(value: ScanpathMean[K]): Either[CodecError, Json] =
    value.result match
      case Left(error) =>
        ResultWire.reductionError(keys)(error).map { error =>
          Json.obj(
            "requested"    -> Json.fromInt(value.requested),
            "successful"   -> Json.fromInt(value.successful),
            "contributing" -> Json.fromInt(value.contributing),
            "policy"       -> StudyWire.policy(value.policy),
            "result"       -> ResultWire.tagged("failure", "error" -> error)
          )
        }
      case Right(score) =>
        Right(
          Json.obj(
            "requested"    -> Json.fromInt(value.requested),
            "successful"   -> Json.fromInt(value.successful),
            "contributing" -> Json.fromInt(value.contributing),
            "policy"       -> StudyWire.policy(value.policy),
            "result"       -> ResultWire.tagged("success", "value" -> ResultWire.double(score))
          )
        )
  private[eyes4s] def resultJson(value: ScanpathStudyResult[K, U]): Either[CodecError, Json] =
    for
      matched   <- pairs(value.matched); controls <- pairs(value.controls)
      contrasts <- value.contrasts.traverse { row =>
        for
          key        <- keys.encode(row.key); matched <- mean(row.matched);
          control    <- mean(row.control)
          difference <- row.difference.fold(
            error =>
              ResultWire
                .reductionError(keys)(error)
                .map(e => ResultWire.tagged("failure", "error" -> e)),
            score => Right(ResultWire.tagged("success", "value" -> ResultWire.double(score)))
          )
        yield Json.obj(
          "key"        -> key,
          "component"  -> Json.fromString(row.component),
          "matched"    -> matched,
          "control"    -> control,
          "difference" -> difference
        )
      }
      excluded <- value.excludedPhases.traverse(keys.encode)
    yield Json.obj(
      "inputHash"      -> Json.fromString(value.inputHash.render),
      "planHash"       -> Json.fromString(value.planHash.render),
      "matched"        -> matched,
      "controls"       -> controls,
      "contrasts"      -> Json.arr(contrasts*),
      "excludedPhases" -> Json.arr(excluded*)
    )

  val results: VersionedCodec[ScanpathStudyArchive[K, U]] =
    VersionedCodec.checked[ScanpathStudyArchive[K, U]](ScanpathStudyDefinitions.result) {
      archive =>
        for
          _      <- ScanpathStudyArchive.completed(archive.plan, archive.result)
          claim  <- stamp(archive.plan); plan <- plans.encode(archive.plan)
          result <- resultJson(archive.result)
        yield Json.obj(
          "runStamp" -> RunStampWire.write(claim),
          "plan"     -> plan,
          "result"   -> result
        )
    } { json =>
      for
        claim <- Wire
          .field[Json](json, "runStamp")
          .flatMap(RunStampWire.read[ScanpathStudyPlan[K, U], ScanpathStudyPlan[K, U]])
        plan     <- Wire.field[Json](json, "plan").flatMap(plans.decode)
        current  <- stamp(plan)
        _        <- claim.check(current, Vector.empty).left.map(CodecError.Stamp.apply)
        expected <- Wire.field[Json](json, "result")
        archive  <- ScanpathStudyArchive
          .run(plan)
          .left
          .map(e => CodecError.Field("scanpathStudyResult", json, e.message))
        actual <- resultJson(archive.result)
        _      <- Either.cond(
          expected == actual,
          (),
          CodecError.Derived("scanpathStudyResult", expected, actual)
        )
      yield archive
    }
  def registration: AnalysisRegistration = AnalysisRegistration.of(plans, results)(
    _.description,
    _.plan.description,
    archive => Vector(archive.plan.inputHash),
    embedded = Some(plan => Vector(plan.inputHash))
  )

  private[eyes4s] def reductionError(error: ReductionError[K]): Either[CodecError, Json] =
    ResultWire.reductionError(keys)(error)
