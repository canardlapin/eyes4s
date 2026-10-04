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

/** A test analysis family stored under the generic analysis roles: a plan
  * that counts the fixations of one phase of a study input, and its result.
  * It stands in for a family without a role of its own (CR4 S2).
  */
object AnalysisFixtures:
  private def get[E, A](e: Either[E, A]): A = e.fold(x => sys.error(x.toString), identity)

  val planSchema: DefinitionId   = get(DefinitionId.of("test.fixation-count-plan", 1))
  val resultSchema: DefinitionId = get(DefinitionId.of("test.fixation-count-result", 1))

  final case class CountPlan(phase: String) derives CanEqual:
    def description: Vector[(String, Vector[Provenance.Param])] =
      Vector("phase" -> Vector(Provenance.Param.Text(phase)))

  final case class CountResult(plan: CountPlan, input: ContentHash, fixations: Int)
      derives CanEqual

  /** Count the fixations of the plan's phase. */
  def run[K, U <: Unit2D](plan: CountPlan, input: StudyInput[K, U], phase: K => String) =
    CountResult(
      plan,
      input.hash,
      input.trials.rows.filter(t => phase(t.key) == plan.phase).map(_.value.n).sum
    )

  val plans: VersionedCodec[CountPlan] =
    VersionedCodec.of[CountPlan](planSchema)(p =>
      Json.obj("phase" -> Json.fromString(p.phase))
    )(json => Wire.field[String](json, "phase").map(CountPlan(_)))

  val results: VersionedCodec[CountResult] =
    VersionedCodec.of[CountResult](resultSchema)(r =>
      Json.obj(
        "phase"     -> Json.fromString(r.plan.phase),
        "input"     -> Json.fromString(r.input.render),
        "fixations" -> Json.fromInt(r.fixations)
      )
    )(json =>
      for
        phase    <- Wire.field[String](json, "phase")
        rendered <- Wire.field[String](json, "input")
        input    <- ContentHash
          .parse(rendered)
          .toRight(CodecError.Field("input", json, "expected a content hash"))
        fixations <- Wire.field[Int](json, "fixations")
      yield CountResult(CountPlan(phase), input, fixations)
    )

  final class LoadedPlan(val plan: CountPlan) extends LoadedAnalysisPlan:
    val schema: DefinitionId = planSchema
    def description          = plan.description
    def encode               = plans.encode(plan)

  final class LoadedResult(val result: CountResult) extends LoadedAnalysisResult:
    val schema: DefinitionId = resultSchema
    def description          = result.plan.description
    def inputs               = Vector(result.input)
    def encode               = results.encode(result)

  val registration: AnalysisRegistration = AnalysisRegistration(
    planSchema,
    resultSchema,
    json => plans.decode(json).map(LoadedPlan(_)),
    json => results.decode(json).map(LoadedResult(_))
  )

  val registry: AnalysisRegistry = get(AnalysisRegistry.empty.register(registration))
