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
import eyes4s.plan.DefinitionId
import io.circe.Json

/** A fixed-feature, explicit-fold recipe; no function is reconstructed from a label. */
object TemplateRecipeCodec:
  def of[K: KeyDigest](
      schema: DefinitionId,
      keys: VersionedCodec[K]
  ): VersionedCodec[TemplateSplit[K]] =
    build(schema, keys, FittedTemplate.method, native = false)

  private[codec] def build[K: KeyDigest](
      schema: DefinitionId,
      keys: VersionedCodec[K],
      methodId: String,
      native: Boolean
  ): VersionedCodec[TemplateSplit[K]] =
    VersionedCodec.checked[TemplateSplit[K]](schema) { split =>
      split.rows
        .traverse { row =>
          keys
            .encode(row.key)
            .map(k =>
              Json.obj(
                "key"      -> k,
                "fold"     -> Json.fromString(row.fold),
                "features" -> Json.arr(row.features.map(Json.fromDoubleOrNull)*),
                "response" -> Json.fromDoubleOrNull(row.response)
              )
            )
        }
        .map(rows =>
          Json
            .obj(
              "method"       -> Json.fromString(methodId),
              "basis"        -> Json.fromString(split.basis.id),
              "columns"      -> Json.arr(split.basis.columns.map(Json.fromString)*),
              "responseUnit" -> Json.fromString(split.basis.responseUnit),
              "heldOutFolds" -> Json
                .arr(split.heldOutFolds.toVector.sorted.map(Json.fromString)*),
              "trainingHash" -> Json.fromString(split.training.hash.render),
              "rows"         -> Json.arr(rows*)
            )
            .mapObject(obj =>
              if native then
                obj
                  .add("intercept", Json.fromBoolean(false))
                  .add(
                    "rankTolerance",
                    Json.fromDoubleOrNull(FittedTemplate.nativeRankTolerance)
                  )
              else obj
            )
        )
    } { json =>
      def domain[A](e: Either[TemplateFitError, A]): Either[CodecError, A] =
        e.left.map(e => CodecError.Field("template recipe", json, e.message))
      for
        method <- Wire.field[String](json, "method")
        _      <- Either.cond(
          method == methodId,
          (),
          CodecError.Field("method", json, s"expected ${methodId}, got $method")
        )
        _ <-
          if native then
            for
              intercept <- Wire.field[Boolean](json, "intercept")
              tolerance <- Wire.field[Double](json, "rankTolerance")
              _         <- Either.cond(
                !intercept && tolerance == FittedTemplate.nativeRankTolerance,
                (),
                CodecError.Field(
                  "native fitting convention",
                  json,
                  "expected no intercept and native rank tolerance"
                )
              )
            yield ()
          else Right(())
        id      <- Wire.field[String](json, "basis")
        columns <- Wire.field[Vector[String]](json, "columns")
        unit    <- Wire.field[String](json, "responseUnit")
        basis   <- domain(TemplateBasis.of(id, columns, unit))
        folds   <- Wire.field[Vector[String]](json, "heldOutFolds")
        _       <- Either.cond(
          folds.distinct.size == folds.size,
          (),
          CodecError.Field("heldOutFolds", json, "duplicate folds")
        )
        _    <- Wire.ascending("heldOutFolds", folds.map(f => f -> Json.fromString(f)))
        rows <- Wire
          .field[Vector[Json]](json, "rows")
          .flatMap(_.traverse { row =>
            for
              key    <- Wire.field[Json](row, "key").flatMap(keys.decode)
              fold   <- Wire.field[String](row, "fold")
              x      <- Wire.field[Vector[Double]](row, "features")
              y      <- Wire.field[Double](row, "response")
              result <- domain(TemplateObservation.of(key, fold, x, y))
            yield result
          })
        split <- domain(TemplateSplit.of(basis, rows, folds.toSet))
        hash  <- Wire.field[String](json, "trainingHash")
        _     <- Either.cond(
          hash == split.training.hash.render,
          (),
          CodecError
            .Field("trainingHash", json, s"expected ${split.training.hash.render}, got $hash")
        )
      yield split
    }
