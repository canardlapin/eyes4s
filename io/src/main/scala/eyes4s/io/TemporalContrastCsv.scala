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
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

final case class TemporalTables(contrasts: TidyCsvDocument, coverage: TidyCsvDocument)

/** Separate long tables for scientific contrasts and every source trial's temporal ledger. */
object TemporalContrastCsv:
  val contextHeader = Vector("repetition", "window", "from_us", "until_us", "fixation_boundary")
  val coverageHeader = contextHeader ++ Vector(
    "key_json",
    "observed_us",
    "missing_us",
    "retained_us",
    "excluded_fixations",
    "fixation_times_json",
    "status",
    "reason",
    "input_digest",
    "plan_json"
  )
  def document[K, U <: Unit2D: UnitLabel, P, S, D](
      plan: TemporalStudyPlan[K, U, P, S, D],
      result: TemporalStudyResult[K, U, P, S, D],
      persistence: TemporalStudyCodec[K, U, P, S, D],
      columns: ScoreColumns[S, D]
  ): Either[ContrastExportError, TemporalTables] = for
    _ <- Either.cond(
      plan.description == result.description,
      (),
      ContrastExportError.PlanMismatch(plan.description, result.description)
    )
    saved  <- persistence.codec.encode(plan).left.map(ContrastExportError.Codec.apply)
    tables <- result.cells.traverse { cell =>
      val context = Vector(
        cell.repetition.name,
        cell.window.name,
        cell.window.window.from.toMicros.toString,
        cell.window.window.until.toMicros.toString,
        plan.boundary.toString
      )
      for
        contrasts <- ContrastCsv.document(cell.study, cell.result, persistence.study, columns)
        coverage  <- cell.occupancy.traverse { case (key, outcome) =>
          persistence.study.keys.encode(key).left.map(ContrastExportError.Codec.apply).map {
            keyJson =>
              val fields = outcome match
                case Left(error)  => Vector("", "", "", "", "", "failed", error.message)
                case Right(value) =>
                  Vector(
                    value.observedMicros.toString,
                    value.missingMicros.toString,
                    value.retainedMicros.toString,
                    value.excludedFixations.mkString(","),
                    Json
                      .arr(
                        value.fixationTimes.map(t =>
                          Json.obj(
                            "index"       -> Json.fromInt(t.index),
                            "original_us" -> Json.fromString(t.originalMicros.toString),
                            "retained_us" -> Json.fromString(t.retainedMicros.toString)
                          )
                        )*
                      )
                      .noSpaces,
                    "ok",
                    ""
                  )
              context ++ Vector(keyJson.noSpaces) ++ fields ++ Vector(
                plan.input.digest,
                saved.noSpaces
              )
          }
        }
      yield (
        contrasts.rows.map(row =>
          context ++ contrasts.header.zip(row).map {
            case ("plan_json", _)      => saved.noSpaces
            case ("input_digest", _)   => plan.input.digest
            case ("schema_version", _) => "eyes4s-temporal-contrast/1"
            case (_, value)            => value
          }
        ),
        coverage
      )
    }
  yield TemporalTables(
    new TidyCsvDocument(contextHeader ++ ContrastCsv.header, tables.flatMap(_._1)),
    new TidyCsvDocument(coverageHeader, tables.flatMap(_._2))
  )
