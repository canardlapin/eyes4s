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

import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

/** A decoded plan of an analysis family stored under the generic
  * `analysis-plan` role (CR4): its schema, the description a result run under
  * it must carry, and its own encoding. A family's subclass holds the typed
  * plan, which a host reaches by matching on that subclass.
  */
trait LoadedAnalysisPlan:
  val schema: DefinitionId
  def description: Vector[(String, Vector[Provenance.Param])]
  def encode: Either[CodecError, Json]

/** A decoded result of an analysis family stored under the generic
  * `analysis-result` role: its schema, the plan description it was computed
  * under, the semantic identities of the inputs it was computed on, in order,
  * and its own encoding.
  */
trait LoadedAnalysisResult:
  val schema: DefinitionId
  def description: Vector[(String, Vector[Provenance.Param])]
  def inputs: Vector[ContentHash]
  def encode: Either[CodecError, Json]

/** One analysis family's archives as the resolver sees them: the plan and
  * result schemas it decodes and a decoder for each.
  */
final case class AnalysisRegistration(
    plan: DefinitionId,
    result: DefinitionId,
    decodePlan: Json => Either[CodecError, LoadedAnalysisPlan],
    decodeResult: Json => Either[CodecError, LoadedAnalysisResult]
)

/** The analysis families a resolver decodes under the generic roles, by
  * schema. An entry of a schema no registration names is refused as
  * `CodecError.UnsupportedSchema`, naming the schemas the registry supports.
  */
final class AnalysisRegistry private (val entries: Vector[AnalysisRegistration]):
  /** This registry and `entry`, unless one of its schemas is registered
    * already (`CodecError.DuplicateResultCodec`, naming that schema).
    */
  def register(entry: AnalysisRegistration): Either[CodecError, AnalysisRegistry] =
    val taken = entries.flatMap(e => Vector(e.plan, e.result))
    Vector(entry.plan, entry.result).find(taken.contains) match
      case Some(schema) => Left(CodecError.DuplicateResultCodec(schema))
      case None         =>
        if entry.plan == entry.result then Left(CodecError.DuplicateResultCodec(entry.plan))
        else Right(new AnalysisRegistry(entries :+ entry))

  /** The plan schemas, then the result schemas, in registration order. */
  def planSchemas: Vector[DefinitionId]   = entries.map(_.plan)
  def resultSchemas: Vector[DefinitionId] = entries.map(_.result)

  /** Decode a stored analysis plan of `schema`. */
  def plan(schema: DefinitionId, json: Json): Either[CodecError, LoadedAnalysisPlan] =
    entries
      .find(_.plan == schema)
      .toRight(
        CodecError.UnsupportedSchema(ArtifactRole.AnalysisPlan.wire, schema, planSchemas)
      )
      .flatMap(_.decodePlan(json))

  /** Decode a stored analysis result of `schema`. */
  def result(schema: DefinitionId, json: Json): Either[CodecError, LoadedAnalysisResult] =
    entries
      .find(_.result == schema)
      .toRight(
        CodecError.UnsupportedSchema(ArtifactRole.AnalysisResult.wire, schema, resultSchemas)
      )
      .flatMap(_.decodeResult(json))

object AnalysisRegistry:
  val empty: AnalysisRegistry = new AnalysisRegistry(Vector.empty)

  /** A registry of `entries`, registered in order. */
  def of(entries: Vector[AnalysisRegistration]): Either[CodecError, AnalysisRegistry] =
    entries.foldLeft[Either[CodecError, AnalysisRegistry]](Right(empty))((acc, entry) =>
      acc.flatMap(_.register(entry))
    )
