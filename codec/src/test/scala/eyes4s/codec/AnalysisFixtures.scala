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

  final case class CountResult(plan: CountPlan, inputs: Vector[ContentHash], fixations: Int)
      derives CanEqual

  /** Count the fixations of the plan's phase. */
  def run[K, U <: Unit2D](plan: CountPlan, input: StudyInput[K, U], phase: K => String) =
    CountResult(
      plan,
      Vector(input.hash),
      input.trials.rows.filter(t => phase(t.key) == plan.phase).map(_.value.n).sum
    )

  val plans: VersionedCodec[CountPlan] =
    VersionedCodec.of[CountPlan](planSchema)(p =>
      Json.obj("phase" -> Json.fromString(p.phase))
    )(json => Wire.field[String](json, "phase").map(CountPlan(_)))

  private def writeCount(r: CountResult): Json = Json.obj(
    "phase"     -> Json.fromString(r.plan.phase),
    "inputs"    -> Json.arr(r.inputs.map(h => Json.fromString(h.render))*),
    "fixations" -> Json.fromInt(r.fixations)
  )
  private def readCount(json: Json): Either[CodecError, CountResult] =
    for
      phase    <- Wire.field[String](json, "phase")
      rendered <- Wire.field[Vector[String]](json, "inputs")
      inputs   <- rendered.traverse(r =>
        ContentHash
          .parse(r)
          .toRight(CodecError.Field("inputs", json, "expected a content hash"))
      )
      fixations <- Wire.field[Int](json, "fixations")
    yield CountResult(CountPlan(phase), inputs, fixations)

  val results: VersionedCodec[CountResult] =
    VersionedCodec.of[CountResult](resultSchema)(writeCount)(readCount)

  val registration: AnalysisRegistration =
    AnalysisRegistration.of(plans, results)(_.description, _.plan.description, _.inputs)

  /** A second family whose plan embeds the identity of the input it counts,
    * so its results relate to it with `AnalysisInputs.EmbeddedInPlan`. Its
    * plans describe themselves as count plans do.
    */
  val embeddedPlanSchema: DefinitionId   = get(DefinitionId.of("test.embedded-count-plan", 1))
  val embeddedResultSchema: DefinitionId = get(DefinitionId.of("test.embedded-count-result", 1))

  final case class EmbeddedPlan(phase: String, input: ContentHash) derives CanEqual:
    def count: CountPlan = CountPlan(phase)

  val embeddedPlans: VersionedCodec[EmbeddedPlan] =
    VersionedCodec.of[EmbeddedPlan](embeddedPlanSchema)(p =>
      Json.obj("phase" -> Json.fromString(p.phase), "input" -> Json.fromString(p.input.render))
    )(json =>
      for
        phase    <- Wire.field[String](json, "phase")
        rendered <- Wire.field[String](json, "input")
        input    <- ContentHash
          .parse(rendered)
          .toRight(CodecError.Field("input", json, "expected a content hash"))
      yield EmbeddedPlan(phase, input)
    )

  /** The embedding family's results: count results under their own schema. */
  val embeddedResults: VersionedCodec[CountResult] =
    VersionedCodec.of[CountResult](embeddedResultSchema)(writeCount)(readCount)

  val embeddedRegistration: AnalysisRegistration =
    AnalysisRegistration.of(embeddedPlans, embeddedResults)(
      _.count.description,
      _.plan.description,
      _.inputs,
      embedded = Some(p => Vector(p.input))
    )

  /** Both families. */
  val registry: AnalysisRegistry =
    get(AnalysisRegistry.of(Vector(registration, embeddedRegistration)))

  /** Input entries, in order. */
  def entries(first: ArtifactName, rest: ArtifactName*): AnalysisInputs =
    AnalysisInputs.Entries(cats.data.NonEmptyVector(first, rest.toVector))
