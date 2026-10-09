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

/** The one template recipe codec: the complete inputs of a
  * [[eyes4s.design.TemplateSplit TemplateSplit]] under any [[eyes4s.design.TemplateDesign TemplateDesign]], saved so that reopening
  * reruns admission and exclusion, checks the training identity and refits
  * deterministically. Fitted coefficients are never read back as proof of
  * fitting.
  *
  * ==Versions==
  *
  * The caller supplies the schema identity `S@n`, and the codec is a
  * [[SchemaLadder]] of two versions:
  *
  *   - `S@n` reads and writes the three recipe forms that earlier releases
  *     wrote under a caller schema: native fixed features
  *     (`eyes4s.no-intercept-scaled-householder-qr/2`), the historical
  *     R-labelled fixed features (`eyes4s.no-intercept-r-lm-qr/1`) and the
  *     training-mean map (`eyes4s.training-mean-cosine-response/1`). Each form
  *     is recognised by its `method`, so a recipe saved by an earlier release
  *     under `S@n` decodes here unchanged;
  *   - `S@(n+1)` also expresses fixed-feature rows with match groups. A
  *     fixed-feature split without match groups is still written as `S@n`,
  *     byte for byte as before; the upcast adds nothing to an `S@n` payload.
  */
object TemplateRecipeCodec:
  /** The recipe ladder for inputs `X`, starting at the caller's `schema`. */
  def ladder[K: KeyDigest, X](
      schema: DefinitionId,
      keys: VersionedCodec[K]
  )(using wire: TemplateInputWire[X]): SchemaLadder[TemplateSplit[K, X]] =
    SchemaLadder
      .of[TemplateSplit[K, X]]("template recipe", schema)(wire.write(keys, _, grouped = false))(
        wire.read(keys, _, grouped = false)
      )
      .next(wire.expressedWithoutGroups, identity)(wire.write(keys, _, grouped = true))(
        wire.read(keys, _, grouped = true)
      )

  /** The codec of [[ladder]]: each split is written under the earliest version
    * that expresses it, and both versions are read.
    */
  def of[K: KeyDigest, X](
      schema: DefinitionId,
      keys: VersionedCodec[K]
  )(using TemplateInputWire[X]): VersionedCodec[TemplateSplit[K, X]] =
    ladder[K, X](schema, keys).codec

/** How the inputs of one design kind are written in a template recipe:
  * fixed feature vectors or maps on a nominal grid.
  */
sealed trait TemplateInputWire[X]:
  private[codec] def write[K](
      keys: VersionedCodec[K],
      split: TemplateSplit[K, X],
      grouped: Boolean
  ): Either[CodecError, Json]

  private[codec] def read[K: KeyDigest](
      keys: VersionedCodec[K],
      json: Json,
      grouped: Boolean
  ): Either[CodecError, TemplateSplit[K, X]]

  /** Whether the first version expresses `split`. */
  private[codec] def expressedWithoutGroups[K](split: TemplateSplit[K, X]): Boolean

object TemplateInputWire:
  private def domain[A](json: Json, value: Either[TemplateError, A]): Either[CodecError, A] =
    value.left.map(e => CodecError.Field("template recipe", json, e.message))

  private def checkedHash(json: Json, expected: ContentHash): Either[CodecError, Unit] =
    Wire
      .field[String](json, "trainingHash")
      .flatMap(hash =>
        Either.cond(
          hash == expected.render,
          (),
          CodecError.Field("trainingHash", json, s"expected ${expected.render}, got $hash")
        )
      )

  private def groups(json: Json, field: String): Either[CodecError, Vector[String]] = for
    held <- Wire.field[Vector[String]](json, field)
    _    <- Either.cond(
      held.distinct.size == held.size,
      (),
      CodecError.Field(field, json, "duplicate groups")
    )
    // One canonical wire form: encode sorts, so decode refuses any other order.
    _ <- Wire.ascending(field, held.map(g => g -> Json.fromString(g)))
  yield held

  /** Fixed feature vectors: the native and historical R-labelled methods. */
  given features: TemplateInputWire[Vector[Double]] with
    private[codec] def expressedWithoutGroups[K](
        split: TemplateSplit[K, Vector[Double]]
    ): Boolean = split.rows.forall(_.matchGroup.isEmpty)

    private[codec] def write[K](
        keys: VersionedCodec[K],
        split: TemplateSplit[K, Vector[Double]],
        grouped: Boolean
    ): Either[CodecError, Json] =
      split.design match
        case fixed: TemplateDesign.Fixed =>
          split.rows
            .traverse { row =>
              keys
                .encode(row.key)
                .map(k =>
                  Json.fromFields(
                    Vector("key" -> k, "fold" -> Json.fromString(row.splitGroup)) ++
                      row.matchGroup
                        .filter(_ => grouped)
                        .map(g => "matchGroup" -> Json.fromString(g))
                        .toVector ++ Vector(
                        "features" -> Json.arr(row.input.map(Json.fromDoubleOrNull)*),
                        "response" -> Json.fromDoubleOrNull(row.response)
                      )
                  )
                )
            }
            .map { rows =>
              val base = Json.obj(
                "method"       -> Json.fromString(fixed.method),
                "basis"        -> Json.fromString(fixed.basis.id),
                "columns"      -> Json.arr(fixed.basis.columns.map(Json.fromString)*),
                "responseUnit" -> Json.fromString(fixed.basis.responseUnit),
                "heldOutFolds" -> Json
                  .arr(split.heldOutGroups.toVector.sorted.map(Json.fromString)*),
                "trainingHash" -> Json.fromString(split.training.hash.render),
                "rows"         -> Json.arr(rows*)
              )
              fixed.route match
                case TemplateDesign.FixedRoute.Native =>
                  base.mapObject(
                    _.add("intercept", Json.fromBoolean(false))
                      .add(
                        "rankTolerance",
                        Json.fromDoubleOrNull(TemplateDesign.nativeRankTolerance)
                      )
                  )
                case TemplateDesign.FixedRoute.ImportedLm => base
            }

    private[codec] def read[K: KeyDigest](
        keys: VersionedCodec[K],
        json: Json,
        grouped: Boolean
    ): Either[CodecError, TemplateSplit[K, Vector[Double]]] =
      for
        method <- Wire.field[String](json, "method")
        route  <- method match
          case TemplateDesign.nativeMethod =>
            for
              intercept <- Wire.field[Boolean](json, "intercept")
              tolerance <- Wire.field[Double](json, "rankTolerance")
              _         <- Either.cond(
                !intercept && tolerance == TemplateDesign.nativeRankTolerance,
                (),
                CodecError.Field(
                  "native fitting convention",
                  json,
                  "expected no intercept and native rank tolerance"
                )
              )
            yield TemplateDesign.FixedRoute.Native
          case TemplateDesign.importedLmMethod => Right(TemplateDesign.FixedRoute.ImportedLm)
          case other                           =>
            Left(
              CodecError.Field(
                "method",
                json,
                s"expected ${TemplateDesign.nativeMethod} or " +
                  s"${TemplateDesign.importedLmMethod}, got $other"
              )
            )
        id      <- Wire.field[String](json, "basis")
        columns <- Wire.field[Vector[String]](json, "columns")
        unit    <- Wire.field[String](json, "responseUnit")
        basis   <- domain(json, TemplateBasis.of(id, columns, unit))
        design = route match
          case TemplateDesign.FixedRoute.Native     => TemplateDesign.fixed(basis)
          case TemplateDesign.FixedRoute.ImportedLm => TemplateDesign.importedLm(basis)
        folds <- groups(json, "heldOutFolds")
        rows  <- Wire
          .field[Vector[Json]](json, "rows")
          .flatMap(_.traverse { row =>
            for
              key     <- Wire.field[Json](row, "key").flatMap(keys.decode)
              fold    <- Wire.field[String](row, "fold")
              matched <-
                if grouped then Wire.omittable[String](row, "matchGroup")
                else
                  Either.cond(
                    row.hcursor.downField("matchGroup").focus.isEmpty,
                    None,
                    CodecError.Field("matchGroup", row, "this version has no match groups")
                  )
              x      <- Wire.field[Vector[Double]](row, "features")
              y      <- Wire.field[Double](row, "response")
              result <- domain(row, TemplateObservation.of(key, fold, x, y, matched))
            yield result
          })
        split <- domain(json, TemplateSplit.of(design, rows, folds.toSet))
        _     <- checkedHash(json, split.training.hash)
      yield split

  /** Maps on a nominal grid: the training-mean map method. */
  given maps[U <: Unit2D](using unit: UnitLabel[U]): TemplateInputWire[Mass[U]] with
    private[codec] def expressedWithoutGroups[K](split: TemplateSplit[K, Mass[U]]): Boolean =
      true

    private[codec] def write[K](
        keys: VersionedCodec[K],
        split: TemplateSplit[K, Mass[U]],
        grouped: Boolean
    ): Either[CodecError, Json] =
      split.design match
        case design: TemplateDesign.MeanMap[?] =>
          split.rows
            .traverse { row =>
              keys.encode(row.key).map { key =>
                Json.obj(
                  "key"        -> key,
                  "splitGroup" -> Json.fromString(row.splitGroup),
                  "matchGroup" -> Json.fromString(row.matchGroup.getOrElse("")),
                  "response"   -> Json.fromDoubleOrNull(row.response),
                  "frame"      -> DomainWire.frame(row.input.grid.frame),
                  "grid"       -> Json.obj(
                    "id" -> Json.fromString(row.input.grid.id.name),
                    "nx" -> Json.fromInt(row.input.grid.nx),
                    "ny" -> Json.fromInt(row.input.grid.ny)
                  ),
                  "values" -> Json.arr(row.input.values.toVector.map(Json.fromDoubleOrNull)*),
                  "provenance" -> ResultWire.provenance(row.input.provenance)
                )
              }
            }
            .map { rows =>
              Json.obj(
                "method"        -> Json.fromString(design.method),
                "splitUnit"     -> Json.fromString(design.splitUnit),
                "responseUnit"  -> Json.fromString(design.responseUnit),
                "heldOutGroups" -> Json
                  .arr(split.heldOutGroups.toVector.sorted.map(Json.fromString)*),
                "trainingHash" -> Json.fromString(split.training.hash.render),
                "rows"         -> Json.arr(rows*)
              )
            }

    private[codec] def read[K: KeyDigest](
        keys: VersionedCodec[K],
        json: Json,
        grouped: Boolean
    ): Either[CodecError, TemplateSplit[K, Mass[U]]] =
      for
        method <- Wire.field[String](json, "method")
        _      <- Either.cond(
          method == TemplateDesign.meanMapMethod,
          (),
          CodecError.Field("method", json, s"expected ${TemplateDesign.meanMapMethod}")
        )
        splitUnit    <- Wire.field[String](json, "splitUnit")
        responseUnit <- Wire.field[String](json, "responseUnit")
        design       <- domain(json, TemplateDesign.meanMap[U](splitUnit, responseUnit))
        held         <- groups(json, "heldOutGroups")
        rows         <- Wire
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
              result <- domain(
                row,
                TemplateObservation.of(key, group, mass, response, Some(matched))
              )
            yield result
          })
        split <- domain(json, TemplateSplit.of(design, rows, held.toSet))
        _     <- checkedHash(json, split.training.hash)
      yield split
