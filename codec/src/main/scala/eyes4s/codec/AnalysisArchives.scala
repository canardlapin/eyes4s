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

  /** The semantic identities of the input the plan document carries, in
    * order; none for a plan that references its input instead. A result
    * related to this plan with `AnalysisInputs.EmbeddedInPlan` is checked
    * against them.
    */
  def embeddedInputs: Vector[ContentHash] = Vector.empty

object LoadedAnalysisPlan:
  /** A plan `codec` decoded: the family's typed plan with what the resolver
    * reads of it. Built by [[AnalysisRegistration.of]].
    */
  final class Of[P] private[codec] (
      val codec: VersionedCodec[P],
      val plan: P,
      describe: P => Vector[(String, Vector[Provenance.Param])],
      embedded: P => Vector[ContentHash]
  ) extends LoadedAnalysisPlan:
    val schema: DefinitionId                                    = codec.schema
    def description: Vector[(String, Vector[Provenance.Param])] = describe(plan)
    def encode: Either[CodecError, Json]                        = codec.encode(plan)
    override def embeddedInputs: Vector[ContentHash]            = embedded(plan)

  /** The typed plan of `loaded`, when `codec` (this very codec) decoded it. */
  def typed[P](codec: VersionedCodec[P])(loaded: LoadedAnalysisPlan): Option[P] =
    loaded match
      case of: Of[?] if of.codec eq codec => Some(of.plan.asInstanceOf[P])
      case _                              => None

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

object LoadedAnalysisResult:
  /** A result `codec` decoded: the family's typed result with what the
    * resolver reads of it. Built by [[AnalysisRegistration.of]].
    */
  final class Of[R] private[codec] (
      val codec: VersionedCodec[R],
      val result: R,
      describe: R => Vector[(String, Vector[Provenance.Param])],
      identities: R => Vector[ContentHash]
  ) extends LoadedAnalysisResult:
    val schema: DefinitionId                                    = codec.schema
    def description: Vector[(String, Vector[Provenance.Param])] = describe(result)
    def inputs: Vector[ContentHash]                             = identities(result)
    def encode: Either[CodecError, Json]                        = codec.encode(result)

  /** The typed result of `loaded`, when `codec` (this very codec) decoded it. */
  def typed[R](codec: VersionedCodec[R])(loaded: LoadedAnalysisResult): Option[R] =
    loaded match
      case of: Of[?] if of.codec eq codec => Some(of.result.asInstanceOf[R])
      case _                              => None

/** One analysis family's archives as the resolver sees them: the plan and
  * result schemas it decodes, a decoder for each, and whether its plan
  * embeds its input (so a result's relation may say
  * `AnalysisInputs.EmbeddedInPlan`).
  */
final case class AnalysisRegistration(
    plan: DefinitionId,
    result: DefinitionId,
    decodePlan: Json => Either[CodecError, LoadedAnalysisPlan],
    decodeResult: Json => Either[CodecError, LoadedAnalysisResult],
    embedsInput: Boolean = false
)

object AnalysisRegistration:
  /** A family registered from its two codecs and what the resolver reads:
    * each plan's and result's description, each result's input identities,
    * and, for a family whose plan embeds its input, that input's identities
    * (`embedded`; the family then declares `embedsInput`). A host recovers
    * the typed values with `LoadedAnalysisPlan.typed(plans)` and
    * `LoadedAnalysisResult.typed(results)`.
    */
  def of[P, R](
      plans: VersionedCodec[P],
      results: VersionedCodec[R]
  )(
      planDescription: P => Vector[(String, Vector[Provenance.Param])],
      resultDescription: R => Vector[(String, Vector[Provenance.Param])],
      resultInputs: R => Vector[ContentHash],
      embedded: Option[P => Vector[ContentHash]] = None
  ): AnalysisRegistration =
    AnalysisRegistration(
      plans.schema,
      results.schema,
      json =>
        plans
          .decode(json)
          .map(
            LoadedAnalysisPlan
              .Of(plans, _, planDescription, embedded.getOrElse(_ => Vector.empty))
          ),
      json =>
        results
          .decode(json)
          .map(LoadedAnalysisResult.Of(results, _, resultDescription, resultInputs)),
      embedsInput = embedded.isDefined
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

  /** The family whose result schema is `schema`. */
  def forResult(schema: DefinitionId): Option[AnalysisRegistration] =
    entries.find(_.result == schema)

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
