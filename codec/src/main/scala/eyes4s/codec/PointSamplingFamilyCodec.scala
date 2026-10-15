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

/** Built-in identities for the static point-sampling family. Conditional key
  * and layout schemas remain explicit; legacy caller-defined codecs are unchanged.
  */
object PointSamplingFamilyDefinitions:
  val plan: DefinitionId   = DefinitionId.builtIn("eyes4s.point-sampling-plan", 1)
  val result: DefinitionId = DefinitionId.builtIn("eyes4s.point-sampling-result", 1)

/** Saved family route with an embedded complete plan and a canonical RunStamp.
  * Reading verifies the stamp before replaying and checking the stored result.
  * Writing an already-completed archive never reruns sampling.
  */
final class PointSamplingFamilyCodec[K, U <: Unit2D: UnitLabel](
    val layout: StudyLayout[K],
    val keys: VersionedCodec[K]
):
  private val legacy: PointSamplingCodec[K, U] = new PointSamplingCodec(
    PointSamplingFamilyDefinitions.plan,
    PointSamplingFamilyDefinitions.result,
    layout,
    keys
  )
  val plans: VersionedCodec[PointSamplingPlan[K, U]] = legacy.plan
  def stamp(
      plan: PointSamplingPlan[K, U]
  ): Either[CodecError, RunStamp[PointSamplingPlan[K, U], PointSamplingPlan[K, U]]] =
    RunStamp.of(plan, plan, plans, plans)
  val results: VersionedCodec[PointSamplingArchive[K, U]] =
    VersionedCodec.checked[PointSamplingArchive[K, U]](PointSamplingFamilyDefinitions.result) {
      archive =>
        for
          _      <- PointSamplingArchive.completed(archive.plan, archive.result)
          claim  <- stamp(archive.plan)
          plan   <- plans.encode(archive.plan)
          result <- legacy.resultJson(archive.result)
        yield Json.obj(
          "runStamp" -> RunStampWire.write(claim),
          "plan"     -> plan,
          "result"   -> result
        )
    } { json =>
      for
        claim <- Wire
          .field[Json](json, "runStamp")
          .flatMap(RunStampWire.read[PointSamplingPlan[K, U], PointSamplingPlan[K, U]])
          .left
          .map(Wire.at("runStamp"))
        plan <- Wire.field[Json](json, "plan").flatMap(plans.decode).left.map(Wire.at("plan"))
        current  <- stamp(plan)
        _        <- claim.check(current, Vector.empty).left.map(CodecError.Stamp(_))
        expected <- Wire.field[Json](json, "result")
        archive = PointSamplingArchive.run(plan)
        actual <- legacy.resultJson(archive.result)
        _      <- Either.cond(
          actual == expected,
          (),
          CodecError.Derived("pointSamplingResult", expected, actual)
        )
      yield archive
    }
  private[eyes4s] def error(value: PointSamplingError[K]): Json = legacy.error(value)
  def registration: AnalysisRegistration = AnalysisRegistration.of(plans, results)(
    _.description,
    _.plan.description,
    archive => Vector(archive.plan.inputHash),
    embedded = Some(plan => Vector(plan.inputHash))
  )
