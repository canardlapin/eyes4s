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

import cats.syntax.all.*
import eyes4s.codec.*
import eyes4s.compare.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.*

enum ContrastExportError derives CanEqual:
  case Codec(underlying: CodecError)
  case PlanMismatch(
      expected: Vector[(String, Vector[Provenance.Param])],
      actual: Vector[(String, Vector[Provenance.Param])]
  )
  case Components(expected: Vector[String], actual: Vector[String])
  case Values(operand: String, components: Vector[String], values: Vector[Double])
  def message: String = this match
    case Codec(e)           => e.message
    case PlanMismatch(e, a) => s"Export plan $e differs from executed plan $a."
    case Components(e, a)   => s"Export components $a differ from method components $e."
    case Values(operand, components, values) =>
      s"$operand must supply one finite value per component $components, got $values."

/** Typed component access for tabular consumers. Custom methods supply their own projections. */
final class ScoreColumns[S, D] private (
    val names: Vector[String],
    score: S => Vector[Double],
    difference: D => Vector[Double]
):
  def scores(value: S, operand: String): Either[ContrastExportError, Vector[Double]] =
    validate(score(value), operand)
  def differences(value: D): Either[ContrastExportError, Vector[Double]] =
    validate(difference(value), "difference")
  private def validate(
      values: Vector[Double],
      operand: String
  ): Either[ContrastExportError, Vector[Double]] =
    Either.cond(
      values.size == names.size && values.forall(_.isFinite),
      values,
      ContrastExportError.Values(operand, names, values)
    )
object ScoreColumns:
  def of[S, D](names: Vector[String])(
      score: S => Vector[Double],
      difference: D => Vector[Double]
  ): Either[ContrastExportError, ScoreColumns[S, D]] =
    if names.isEmpty || names.exists(_.trim.isEmpty) || names.distinct.size != names.size then
      Left(ContrastExportError.Components(Vector("non-empty unique component names"), names))
    else Right(new ScoreColumns(names, score, difference))
  val similarity: ScoreColumns[Similarity, SignedDifference] =
    new ScoreColumns(
      Vector("value"),
      value => Vector(value.value),
      value => Vector(value.value)
    )
  val distance: ScoreColumns[MeasureDistance, SignedDifference] =
    new ScoreColumns(
      Vector("value"),
      value => Vector(value.value),
      value => Vector(value.value)
    )
  val scalar: ScoreColumns[Double, SignedDifference] =
    new ScoreColumns(Vector("value"), value => Vector(value), value => Vector(value.value))
  val multiMatch: ScoreColumns[MultiMatchScore, MultiMatchDifference] =
    new ScoreColumns(
      Vector("shape", "direction", "length", "position", "duration"),
      value =>
        Vector(value.shape, value.direction, value.length, value.position, value.duration),
      value =>
        Vector(
          value.shape.value,
          value.direction.value,
          value.length.value,
          value.position.value,
          value.duration.value
        )
    )

/** Long-form CSV: one row per focal key, scale and component, including failures.
  * Scale-level errors and excluded phase keys have explicit scope/status fields.
  * Empty numeric fields mean missing, with status and reason carried alongside.
  * Decimal cells use the same lossless rounded round-trip spelling on JVM and Scala.js;
  * identical output requires identical input values, not merely numerically close results.
  * Trailing decimal zeros are omitted (1.0 becomes "1"); exponents use uppercase E
  * with an explicit positive sign (1000.0 becomes "1E+3"). Signed zero is preserved.
  */
object ContrastCsv:
  val schemaVersion          = "eyes4s-study-contrast/2"
  val header: Vector[String] = Vector(
    "schema_version",
    "scope",
    "participant",
    "stimulus",
    "phase",
    "key_json",
    "estimator",
    "sigma",
    "edges",
    "component",
    "matched",
    "control",
    "difference",
    "status",
    "reason",
    "matched_selected",
    "matched_successful",
    "matched_failed",
    "matched_contributing",
    "control_selected",
    "control_successful",
    "control_failed",
    "control_contributing",
    "input_digest",
    "frame_id",
    "spatial_unit",
    "method_id",
    "method_version",
    "score_scale",
    "matched_provenance",
    "control_provenance",
    "estimation_failures",
    "plan_json",
    "sigma_x",
    "sigma_y"
  )

  def document[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      result: StudyResult[K, U, S, D],
      persistence: StudyCodec[K, U, P, S, D],
      columns: ScoreColumns[S, D]
  )(using unit: UnitLabel[U]): Either[ContrastExportError, TidyCsvDocument] = for
    _ <- Either.cond(
      result.description == plan.description,
      (),
      ContrastExportError.PlanMismatch(plan.description, result.description)
    )
    _ <- Either.cond(
      columns.names == plan.method.difference.components,
      (),
      ContrastExportError.Components(plan.method.difference.components, columns.names)
    )
    saved <- persistence.codec.encode(plan).left.map(ContrastExportError.Codec.apply)
    rows  <- result.scales.traverse { scale =>
      val estimate = scale.estimate match
        case StudyEstimate.Anisotropic(_, _, edges) => Vector("anisotropic", "", edges.toString)
        case StudyEstimate.Binned()                 => Vector("binned", "", "")
        case StudyEstimate.Gaussian(sigma, edges)   =>
          Vector("gaussian", CsvNumber.render(sigma.value), edges.toString)
      val failures =
        scale.estimation.collect { case (_, Left(error)) => error.message }.mkString(" | ")
      def keyFields(key: K): Either[ContrastExportError, Vector[String]] =
        persistence.keys
          .encode(key)
          .left
          .map(ContrastExportError.Codec.apply)
          .map(json =>
            Vector(
              plan.layout.participant(key),
              plan.layout.stimulus(key),
              plan.layout.phase(key),
              json.noSpaces
            )
          )
      def suffix(matched: String, control: String): Vector[String] = Vector(
        result.input.digest,
        plan.grid.frame.id.name,
        unit.symbol,
        plan.method.id.name,
        plan.method.id.version.toString,
        plan.method.comparison(plan.parameters).scale.render,
        matched,
        control,
        failures,
        saved.noSpaces
      ) ++ (scale.estimate match
        case StudyEstimate.Anisotropic(x, y, _) =>
          Vector(CsvNumber.render(x.value), CsvNumber.render(y.value))
        case StudyEstimate.Gaussian(sigma, _) =>
          Vector(CsvNumber.render(sigma.value), CsvNumber.render(sigma.value))
        case StudyEstimate.Binned() => Vector("", ""))
      val contrastRows = scale.contrast match
        case Left(error) =>
          Right(
            Vector(
              Vector(schemaVersion, "scale") ++ Vector.fill(4)("") ++ estimate ++
                Vector("", "", "", "", "failed", error.message) ++ Vector.fill(8)("") ++ suffix(
                  "",
                  ""
                )
            )
          )
        case Right(contrast) =>
          contrast.rows
            .traverse { row =>
              for
                key     <- keyFields(row.key)
                matched <- row.matched
                  .flatMap(_.result.toOption)
                  .traverse(columns.scores(_, "matched"))
                control <- row.control
                  .flatMap(_.result.toOption)
                  .traverse(columns.scores(_, "control"))
                difference <- row.difference.toOption.traverse(columns.differences)
              yield columns.names.zipWithIndex.map { case (name, index) =>
                def counts(operand: Option[ReductionRow[K, S]]): Vector[String] =
                  operand.fold(Vector.fill(4)(""))(r =>
                    Vector(r.selected, r.successful, r.failed, r.contributing).map(_.toString)
                  )
                Vector(schemaVersion, "contrast") ++ key ++ estimate ++ Vector(
                  name,
                  matched.map(values => CsvNumber.render(values(index))).getOrElse(""),
                  control.map(values => CsvNumber.render(values(index))).getOrElse(""),
                  difference.map(values => CsvNumber.render(values(index))).getOrElse(""),
                  if row.difference.isRight then "ok" else "failed",
                  row.difference.left.toOption.map(_.message).getOrElse("")
                ) ++ counts(row.matched) ++ counts(row.control) ++
                  suffix(contrast.matched.provenance.render, contrast.control.provenance.render)
              }
            }
            .map(_.flatten)
      for
        scored   <- contrastRows
        excluded <- scale.excludedPhases.traverse { key =>
          keyFields(key).map { fields =>
            Vector(schemaVersion, "excluded_trial") ++ fields ++ estimate ++ Vector(
              "",
              "",
              "",
              "",
              "excluded",
              "phase not selected"
            ) ++
              Vector.fill(8)("") ++ suffix("", "")
          }
        }
      yield scored ++ excluded
    }
  yield new TidyCsvDocument(header, rows.flatten)
