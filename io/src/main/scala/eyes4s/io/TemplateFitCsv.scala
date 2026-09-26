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
import eyes4s.design.*

enum TemplateCsvError derives CanEqual:
  case Csv(underlying: TidyCsvError)
  case Header(expected: Vector[String], actual: Vector[String])
  case Row(index: Int, fields: Vector[String], reason: String)
  case Fit(underlying: TemplateError)
  def message: String = this match
    case Csv(e)       => e.message
    case Header(e, a) => s"Template CSV header $a differs from $e."
    case Row(i, f, r) => s"Template CSV row $i ($f): $r."
    case Fit(e)       => e.message

/** Pure text boundary to tools/template-fit/fit.R. No process is launched by this API. */
object TemplateFitCsv:
  val receiptHeader: Vector[String] = Vector(
    "method",
    "training_hash",
    "feature",
    "coefficient",
    "rank",
    "observations",
    "backend",
    "rank_tolerance"
  )

  /** Accepts only the training capability, never the whole split or held-out responses.
    * Transport row indices preserve order; original typed keys remain in the saved recipe.
    * Decimal cells use lossless rounded round-trip spelling, identical on JVM and Scala.js.
    * Trailing decimal zeros are omitted (1.0 becomes "1"); exponents use uppercase E
    * with an explicit positive sign (1000.0 becomes "1E+3"). Signed zero is preserved.
    */
  def training[K](input: TemplateTraining[K, Vector[Double]]): String =
    val header =
      Vector("method", "training_hash", "basis", "response_unit", "row", "fold", "response") ++
        input.design.featureNames.map("feature:" + _)
    val basis = input.design match
      case fixed: TemplateDesign.Fixed => fixed.basis.id
    Rfc4180.encode(header +: input.rows.zipWithIndex.map { (row, index) =>
      Vector(
        TemplateDesign.importedLmMethod,
        input.hash.render,
        basis,
        input.design.responseUnit,
        index.toString,
        row.splitGroup,
        CsvNumber.render(row.response)
      ) ++ row.input.map(CsvNumber.render)
    })

  def importFit[K](
      input: TemplateTraining[K, Vector[Double]],
      csv: String
  ): Either[TemplateCsvError, FittedTemplate[K, Vector[Double]]] =
    for
      all <- Rfc4180.decode(csv).left.map(TemplateCsvError.Csv.apply)
      _   <- Either.cond(
        all.headOption.contains(receiptHeader),
        (),
        TemplateCsvError.Header(receiptHeader, all.headOption.getOrElse(Vector.empty))
      )
      rows = all.drop(1)
      _ <- Either.cond(
        rows.size == input.design.featureNames.size,
        (),
        TemplateCsvError.Row(
          0,
          Vector.empty,
          s"expected ${input.design.featureNames.size} coefficient rows, got ${rows.size}"
        )
      )
      parsed <- rows.zipWithIndex.traverse { (row, index) =>
        def bad(reason: String) = TemplateCsvError.Row(index + 2, row, reason)
        for
          _ <- Either.cond(row.size == receiptHeader.size, (), bad("wrong width"))
          _ <- Either.cond(
            row(0) == TemplateDesign.importedLmMethod,
            (),
            bad("unsupported fitting method")
          )
          coefficient <- row(3).toDoubleOption
            .filter(_.isFinite)
            .toRight(bad("non-finite coefficient"))
          rank  <- row(4).toIntOption.toRight(bad("invalid rank"))
          count <- row(5).toIntOption.toRight(bad("invalid observation count"))
          _     <- Either.cond(
            row(7).toDoubleOption.contains(TemplateDesign.importedLmRankTolerance),
            (),
            bad("unexpected rank tolerance")
          )
        yield (row(1), row(2), coefficient, rank, count, row(6))
      }
      first <- parsed.headOption.toRight(TemplateCsvError.Row(0, Vector.empty, "empty fit"))
      _     <- Either.cond(
        parsed.forall(r =>
          r._1 == first._1 && r._4 == first._4 && r._5 == first._5 && r._6 == first._6
        ),
        (),
        TemplateCsvError.Row(0, Vector.empty, "inconsistent fit metadata")
      )
      fit <- Template
        .importFit(
          input,
          first._1,
          parsed.map(_._2),
          parsed.map(_._3),
          first._4,
          first._5,
          first._6
        )
        .left
        .map(TemplateCsvError.Fit.apply)
    yield fit
