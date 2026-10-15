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
import io.circe.Json

private[io] object ScanpathResultExports:
  import ResultCell.{Text as T, Integer as I, Number as N, Flag as B, Missing as M}
  private def text(name: String, nullable: Boolean = false) = ResultColumn(
    name,
    if name.endsWith("_json") then ResultColumnType.JsonUtf8 else ResultColumnType.Utf8,
    nullable,
    "text",
    name
  )
  private def integer(name: String, unit: String = "count") =
    ResultColumn(name, ResultColumnType.Int64, false, unit, name)
  private def number(name: String, unit: String, nullable: Boolean = false) =
    ResultColumn(name, ResultColumnType.Float64, nullable, unit, name)
  private val status = ResultColumn(
    "status",
    ResultColumnType.Utf8,
    false,
    "label",
    "scientific outcome",
    Vector("success", "failure")
  )
  private val outcome =
    Vector(number("value", "similarity", true), status, text("error_json", true))
  private val pairKeys = Vector(text("role"), text("left_key_json"), text("right_key_json"))
  private def scalar(value: Either[Json, Double]): Vector[ResultCell] = value match
    case Right(value) => Vector(N(value), T("success"), M)
    case Left(error)  => Vector(M, T("failure"), T(ResultTableJson.canonical(error)))
  def apply[K, U <: Unit2D: UnitLabel](
      archive: ScanpathStudyArchive[K, U],
      codec: ScanpathStudyCodec[K, U]
  ): Either[ResultExportError, Vector[ResultTable]] =
    def encoded[A](value: Either[CodecError, A]) = value.left.map(ResultExportError.Codec.apply)
    def key(value: K): Either[ResultExportError, ResultCell] =
      encoded(codec.keys.encode(value)).map(json => T(ResultTableJson.canonical(json)))
    def reduction(
        value: Either[ReductionError[K], Double]
    ): Either[ResultExportError, Either[Json, Double]] =
      value match
        case Right(value) => Right(Right(value))
        case Left(error)  => encoded(codec.reductionError(error)).map(Left.apply)
    for
      saved <- encoded(codec.results.encode(archive))
      context = ResultTableJson.of(
        Json.obj(
          "archive"          -> saved,
          "spatial_unit"     -> Json.fromString(summon[UnitLabel[U]].symbol),
          "difference_sign"  -> Json.fromString("matched minus control"),
          "query_convention" -> Json.fromString(
            "latest onset; ordered queries retain repeated times"
          ),
          "transport_convention" -> Json.fromString(
            "finite entropic transport; no exact EMD or symmetry claim"
          )
        )
      )
      roles = Vector("matched" -> archive.result.matched, "control" -> archive.result.controls)
      keyed <- roles.flatTraverse { (role, analysis) =>
        analysis.rows.traverse { row =>
          for left <- key(row.left); right <- key(row.right)
          yield (Vector[ResultCell](T(role), left, right), row.result)
        }
      }
      pairRows = keyed.flatMap { (prefix, result) =>
        val scores = result.flatMap(_.scores.left.map(ScanpathComparisonError.Fixations.apply))
        archive.plan.method.componentNames.map { name =>
          val value = scores.flatMap(
            _.find(_._1 == name)
              .map(_._2)
              .toRight(ScanpathComparisonError.Parameter(s"missing component $name", 0L))
          )
          prefix ++ Vector(T(name)) ++ scalar(value.left.map(codec.error))
        }
      }
      queryRows = keyed.flatMap { (prefix, result) =>
        result.toOption.toVector.flatMap {
          case ScanpathMethodOutput.Overlap(value) =>
            value.rows.zipWithIndex.map { (row, index) =>
              prefix ++ Vector(I(index.toLong), I(row.time.toMicros), B(row.hit)) ++
                (row.distance match
                  case Right(distance) => Vector(N(distance), T("success"), M)
                  case Left(error)     =>
                    Vector(
                      M,
                      T("failure"),
                      T(ResultTableJson.canonical(codec.overlapError(error)))
                    )) ++
                Vector(T(value.policy.toString))
            }
          case _ => Vector.empty
        }
      }
      transportRows = keyed.flatMap { (prefix, result) =>
        result.toOption.toVector.flatMap {
          case ScanpathMethodOutput.Transport(value) =>
            Vector(
              prefix ++ Vector(
                N(value.squaredCost),
                N(value.rootCost),
                I(value.iterations.toLong),
                N(value.marginalResidual),
                I(value.leftCount.toLong),
                I(value.rightCount.toLong)
              ) ++
                scalar(
                  value.similarity.left
                    .map(error => codec.error(ScanpathComparisonError.Fixations(error)))
                    .map(_.value)
                )
            )
          case _ => Vector.empty
        }
      }
      keyedContrasts <- archive.result.contrasts.traverse { row => key(row.key).map(row -> _) }
      reductionRows  <- keyedContrasts.flatTraverse { (row, key) =>
        Vector("matched" -> row.matched, "control" -> row.control).traverse { (role, mean) =>
          reduction(mean.result).map { result =>
            Vector[ResultCell](
              key,
              T(row.component),
              T(role),
              I(mean.requested.toLong),
              I(mean.successful.toLong),
              I(mean.contributing.toLong),
              T(mean.policy.render)
            ) ++ scalar(result)
          }
        }
      }
      contrastRows <- keyedContrasts.traverse { (row, key) =>
        reduction(row.difference).map { value =>
          Vector[ResultCell](key, T(row.component)) ++ scalar(value)
        }
      }
      excluded <- archive.result.excludedPhases.zipWithIndex.traverse { (value, index) =>
        key(value).map(k => Vector[ResultCell](I(index.toLong), k))
      }
      specifications = Vector(
        (
          ResultFamily.ScanpathPairs,
          pairKeys ++ Vector(text("component")) ++ outcome,
          pairRows
        ),
        (
          ResultFamily.ScanpathQueries,
          pairKeys ++ Vector(
            integer("query_index"),
            integer("query_micros", "microseconds"),
            ResultColumn(
              "hit",
              ResultColumnType.Boolean,
              false,
              "boolean",
              "strict overlap threshold result"
            ),
            number("distance", summon[UnitLabel[U]].symbol, true),
            status,
            text("error_json", true),
            text("missing_policy")
          ),
          queryRows
        ),
        (
          ResultFamily.ScanpathTransportDiagnostics,
          pairKeys ++ Vector(
            number("squared_cost", "normalized squared cost"),
            number("root_cost", "normalized root cost"),
            integer("iterations"),
            number("marginal_residual", "mass"),
            integer("left_fixations"),
            integer("right_fixations")
          ) ++ outcome,
          transportRows
        ),
        (
          ResultFamily.ScanpathReductions,
          Vector(
            text("key_json"),
            text("component"),
            text("role"),
            integer("selected"),
            integer("successful"),
            integer("contributing"),
            text("failure_policy")
          ) ++
            outcome,
          reductionRows
        ),
        (
          ResultFamily.ScanpathContrasts,
          Vector(text("key_json"), text("component")) ++
            Vector(
              number("difference", "matched minus control", true),
              status,
              text("error_json", true)
            ),
          contrastRows
        ),
        (
          ResultFamily.ScanpathExclusions,
          Vector(integer("excluded_index"), text("key_json")),
          excluded
        )
      )
      tables <- specifications.traverse { (family, columns, rows) =>
        eyes4s.results.ResultTable
          .of(family, columns, rows, context)
          .left
          .map(ResultExportError.of)
      }
    yield tables
