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

/** Separate supplied-map recipe schema; historical StudyCodec v1 bytes keep their meaning.
  * A fresh typed registry resolves only named projections, never serialized functions.
  */
object RepetitionPlanCodec:
  def of[K, U <: Unit2D: UnitLabel](
      schema: DefinitionId,
      registry: RepetitionRegistry[K],
      keys: VersionedCodec[K]
  ): VersionedCodec[RepetitionPlan[K, U]] =
    def issue(json: Json)(error: RepetitionPlanError): CodecError =
      CodecError.Field("repetition", json, error.message)
    def ids(layout: RepetitionLayout[K]) =
      Vector(layout.participantId, layout.stimulusId, layout.occasionId)
    def registered(plan: RepetitionPlan[K, U]): Either[CodecError, Unit] =
      registry.resolve(plan.layout.id).left.map(issue(Json.Null)).flatMap { registered =>
        Either.cond(
          ids(registered) == ids(plan.layout),
          (),
          CodecError.Field("projections", Json.Null, "registered projection identities differ")
        )
      }
    VersionedCodec.checked[RepetitionPlan[K, U]](schema) { plan =>
      for
        _    <- registered(plan)
        rows <- plan.trials.rows.traverse { row =>
          keys.encode(row.key).map { key =>
            Json.obj(
              "key"        -> key,
              "values"     -> Json.arr(row.value.values.toVector.map(Json.fromDoubleOrNull)*),
              "provenance" -> ResultWire.provenance(row.value.provenance)
            )
          }
        }
      yield Json.obj(
        "layout"         -> Wire.id(plan.layout.id),
        "keySchema"      -> Wire.id(keys.schema),
        "projections"    -> Json.arr(ids(plan.layout).map(Wire.id)*),
        "method"         -> Json.fromString(plan.method.toString),
        "methodRevision" -> Json.fromInt(1),
        "orientation"    -> Json.fromString("directed"),
        "self"           -> Json.fromString("exclude"),
        "matched"   -> Json.arr(plan.relations.matched.map(x => Json.fromString(x.toString))*),
        "control"   -> Json.arr(plan.relations.controls.map(x => Json.fromString(x.toString))*),
        "selection" -> ResultWire.selection(plan.controls),
        "policy"    -> StudyWire.policy(plan.policy),
        "frame"     -> DomainWire.frame(plan.grid.frame),
        "grid"      -> Json.obj(
          "id" -> Json.fromString(plan.grid.id.name),
          "nx" -> Json.fromInt(plan.grid.nx),
          "ny" -> Json.fromInt(plan.grid.ny)
        ),
        "rows"      -> Json.arr(rows*),
        "inputHash" -> Json.fromString(plan.inputHash.render),
        "planHash"  -> Json.fromString(plan.planHash.render)
      )
    } { json =>
      def requireValue[A](field: String, found: A, expected: A): Either[CodecError, Unit] =
        Either.cond(
          found == expected,
          (),
          CodecError.Field(field, json, s"expected $expected, found $found")
        )
      def rules(field: String): Either[CodecError, Vector[RepetitionRule]] =
        Wire
          .field[Vector[String]](json, field)
          .flatMap(_.traverse { name =>
            RepetitionRule.values
              .find(_.toString == name)
              .toRight(CodecError.Field(field, json, s"unknown rule $name"))
          })
      for
        _           <- Wire.requireId(json, "keySchema", keys.schema)
        id          <- Wire.definition(json, "layout")
        layout      <- registry.resolve(id).left.map(issue(json))
        projections <- Wire
          .field[Vector[Json]](json, "projections")
          .flatMap(_.traverse(x => Wire.definition(Json.obj("id" -> x), "id")))
        _          <- requireValue("projections", projections, ids(layout))
        methodName <- Wire.field[String](json, "method")
        method     <- MapSimilarityMethod.values
          .find(_.toString == methodName)
          .toRight(CodecError.Field("method", json, s"unsupported map method $methodName"))
        revision    <- Wire.field[Int](json, "methodRevision")
        _           <- requireValue("methodRevision", revision, 1)
        orientation <- Wire.field[String](json, "orientation")
        _           <- requireValue("orientation", orientation, "directed")
        self        <- Wire.field[String](json, "self")
        _           <- requireValue("self", self, "exclude")
        matched     <- rules("matched")
        control     <- rules("control")
        relations   <- RepetitionRelations.of(matched, control).left.map(issue(json))
        selection   <- Wire.field[Json](json, "selection").flatMap(ResultWire.readSelection)
        policy      <- Wire.field[Json](json, "policy").flatMap(StudyWire.readPolicy)
        frame       <- Wire.field[Json](json, "frame").flatMap(DomainWire.readFrame[U])
        grid        <- Wire.field[Json](json, "grid").flatMap(DomainWire.readGrid(_, frame))
        rows        <- Wire
          .field[Vector[Json]](json, "rows")
          .flatMap(_.traverse { row =>
            for
              key        <- Wire.field[Json](row, "key").flatMap(keys.decode)
              values     <- Wire.field[Vector[Double]](row, "values")
              provenance <- Wire
                .field[Json](row, "provenance")
                .flatMap(ResultWire.readProvenance)
              mass <- Surface
                .mass(grid, IArray.from(values), provenance)
                .left
                .map(e => CodecError.Field("values", row, e.message))
            yield Trial(key, (), mass)
          })
        plan <- RepetitionPlan
          .of(layout, relations, method, selection, policy, grid, Trials(rows))
          .left
          .map(issue(json))
        inputHash <- Wire.field[String](json, "inputHash")
        _         <- requireValue("inputHash", inputHash, plan.inputHash.render)
        planHash  <- Wire.field[String](json, "planHash")
        _         <- requireValue("planHash", planHash, plan.planHash.render)
      yield plan
    }
