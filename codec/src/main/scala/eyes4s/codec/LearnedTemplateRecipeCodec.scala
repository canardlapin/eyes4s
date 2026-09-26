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
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.DefinitionId
import io.circe.Json

/** Complete inputs for deterministic training-only mean-map fitting and held-out evaluation.
  * Decoding reruns admission and exclusion rules, and verifies the training identity.
  * Fitted parameters are reconstructed by LearnedTemplate.fit, never trusted wire values.
  */
object LearnedTemplateRecipeCodec:
  def of[K: KeyDigest, U <: Unit2D: UnitLabel](
      schema: DefinitionId,
      keys: VersionedCodec[K]
  ): VersionedCodec[MapTemplateSplit[K, U]] =
    VersionedCodec.checked[MapTemplateSplit[K, U]](schema) { split =>
      split.rows
        .traverse { row =>
          keys.encode(row.key).map { key =>
            Json.obj(
              "key"        -> key,
              "splitGroup" -> Json.fromString(row.splitGroup),
              "matchGroup" -> Json.fromString(row.matchGroup),
              "response"   -> Json.fromDoubleOrNull(row.response),
              "frame"      -> DomainWire.frame(row.map.grid.frame),
              "grid"       -> Json.obj(
                "id" -> Json.fromString(row.map.grid.id.name),
                "nx" -> Json.fromInt(row.map.grid.nx),
                "ny" -> Json.fromInt(row.map.grid.ny)
              ),
              "values"     -> Json.arr(row.map.values.toVector.map(Json.fromDoubleOrNull)*),
              "provenance" -> ResultWire.provenance(row.map.provenance)
            )
          }
        }
        .map { rows =>
          Json.obj(
            "method"        -> Json.fromString(LearnedTemplate.method),
            "splitUnit"     -> Json.fromString(split.training.splitUnit),
            "responseUnit"  -> Json.fromString(split.training.responseUnit),
            "heldOutGroups" -> Json
              .arr(split.heldOutGroups.toVector.sorted.map(Json.fromString)*),
            "trainingHash" -> Json.fromString(split.training.hash.render),
            "rows"         -> Json.arr(rows*)
          )
        }
    } { json =>
      def domain[A](value: Either[LearnedTemplateError, A]): Either[CodecError, A] =
        value.left.map(e => CodecError.Field("learned template recipe", json, e.message))
      for
        method <- Wire.field[String](json, "method")
        _      <- Either.cond(
          method == LearnedTemplate.method,
          (),
          CodecError.Field("method", json, s"expected ${LearnedTemplate.method}")
        )
        splitUnit    <- Wire.field[String](json, "splitUnit")
        responseUnit <- Wire.field[String](json, "responseUnit")
        held         <- Wire.field[Vector[String]](json, "heldOutGroups")
        _            <- Either.cond(
          held.distinct.size == held.size,
          (),
          CodecError.Field("heldOutGroups", json, "duplicate groups")
        )
        // One canonical wire form: encode sorts, so decode refuses any other order.
        _ <- Either.cond(
          held == held.sorted,
          (),
          CodecError.Field("heldOutGroups", json, "groups must be in ascending order")
        )
        rows <- Wire
          .field[Vector[Json]](json, "rows")
          .flatMap(_.traverse { row =>
            for
              key        <- Wire.field[Json](row, "key").flatMap(keys.decode)
              group      <- Wire.field[String](row, "splitGroup")
              matched    <- Wire.field[String](row, "matchGroup")
              response   <- Wire.field[Double](row, "response")
              frame      <- Wire.field[Json](row, "frame").flatMap(DomainWire.readFrame[U])
              grid       <- Wire.field[Json](row, "grid").flatMap(DomainWire.readGrid(_, frame))
              values     <- Wire.field[Vector[Double]](row, "values")
              provenance <- Wire
                .field[Json](row, "provenance")
                .flatMap(ResultWire.readProvenance)
              mass <- Surface
                .mass(grid, IArray.from(values), provenance)
                .left
                .map(e => CodecError.Field("values", row, e.message))
              result <- domain(MapTemplateObservation.of(key, group, matched, mass, response))
            yield result
          })
        split <- domain(MapTemplateSplit.of(rows, held.toSet, splitUnit, responseUnit))
        hash  <- Wire.field[String](json, "trainingHash")
        _     <- Either.cond(
          hash == split.training.hash.render,
          (),
          CodecError.Field("trainingHash", json, s"expected ${split.training.hash.render}")
        )
      yield split
    }
