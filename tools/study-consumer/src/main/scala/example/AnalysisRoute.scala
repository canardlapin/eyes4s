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

package example

import eyes4s.codec.*
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** One analysis an application offers on the fixation-only route: how its
  * trial keys are read from a table, which method compares the maps, and the
  * versioned codecs that save and reload every artifact of a run. Everything
  * is typed; no parameter bag, cast or `Any` registry is involved.
  */
final class AnalysisRoute[K, P, S, D](
    val reader: FixationKeyReader[K],
    val persistence: StudyCodec[K, Px, P, S, D],
    val inputs: StudyInputCodec[K, Px],
    val results: StudyResultCodec[K, Px, P, S, D],
    val parameters: P
):
  def method: StudyMethod[P, Px, S, D] = persistence.method
  def layout: StudyLayout[K]           = persistence.layout

  /** The registrations a reader that starts from nothing makes: fresh
    * registries holding only this route's plan, input and result codecs.
    */
  def decoders: Either[CodecError, ArtifactDecoders[K, Px]] = for
    plans    <- StudyRegistry.empty[K, Px].register(persistence.registration)
    admitted <- StudyInputRegistry.empty[K, Px].register(inputs)
    archived <- StudyResultRegistry.empty[K, Px].register(results.registration)
  yield ArtifactDecoders.of(plans, admitted, archived)

  /** The matched/control study of this route over one admitted input. */
  def plan(
      input: ArtifactRef[StudyInput[K, Px]],
      grid: Grid[Px],
      phases: (String, String),
      weight: Weight,
      scales: Vector[StudyEstimate[Px]],
      policy: FailurePolicy
  ): Either[PlanError, StudyPlan[K, Px, P, S, D]] =
    StudyPlan.of(
      input,
      layout,
      grid,
      phases._1,
      phases._2,
      weight,
      scales,
      policy,
      method,
      parameters
    )

/** Why a route could not be assembled. */
enum RouteError derives CanEqual:
  case Definition(error: PlanError)
  case Descriptor(error: DescriptorError)
  case Reader(error: FixationImportError)

object AnalysisRoute:
  /** The shipped route: participant/stimulus/phase keys and cosine similarity. */
  def cosine(
      participant: String,
      stimulus: String,
      phase: String
  ): Either[RouteError, AnalysisRoute[StudyKey, Unit, Similarity, SignedDifference]] =
    FixationKeyReader
      .study(participant, stimulus, phase)
      .left
      .map(RouteError.Reader.apply)
      .map(reader =>
        new AnalysisRoute(
          reader,
          StudyCodecs.cosine[Px],
          StudyInputCodecs.study[Px],
          StudyResultCodecs.cosine[Px],
          ()
        )
      )

  /** The extension route: integer item keys, the consumer's scaled cosine and
    * its own score codec, all registered under the consumer's definitions.
    * `items` maps the table's stimulus labels to item numbers.
    */
  def scaled(
      multiplier: Multiplier,
      participant: String,
      stimulus: String,
      phase: String,
      items: Map[String, Int]
  ): Either[RouteError, AnalysisRoute[TrialKey, Multiplier, ScaledScore, SignedDifference]] =
    def id(name: String) = DefinitionId.of(name, 1).left.map(RouteError.Definition.apply)
    for
      layoutId    <- id("my.lab.trial-layout")
      methodId    <- id("my.lab.scaled-cosine")
      parameterId <- id("my.lab.multiplier")
      keyId       <- id("my.lab.trial-key")
      studyId     <- id("my.lab.study")
      inputId     <- id("my.lab.study-input")
      ledgerId    <- id("my.lab.admission-ledger")
      scoreId     <- id("my.lab.scaled-score")
      method <- CustomMethod.describedMethod(methodId).left.map(RouteError.Descriptor.apply)
      reader <- FixationKeyReader
        .of[TrialKey](Vector(participant, stimulus, phase))(
          fields =>
            for
              subject <- fields.get(participant).toRight(s"missing $participant")
              label   <- fields.get(stimulus).toRight(s"missing $stimulus")
              item    <- items.get(label).toRight(s"unknown $stimulus $label")
              session <- fields.get(phase).toRight(s"missing $phase")
            yield TrialKey(subject, item, session),
          key => ClockId(s"my.lab.trial:${KeyDigest[TrialKey].digest(key).render}")
        )
        .left
        .map(RouteError.Reader.apply)
    yield
      val layout = CustomMethod.layout(layoutId)
      val keys   = CustomMethod.keyCodec(keyId)
      val study  =
        new StudyCodec(studyId, layout, keys, method, CustomMethod.parameterCodec(parameterId))
      new AnalysisRoute(
        reader,
        study,
        new StudyInputCodec[TrialKey, Px](inputId, ledgerId, layout, keys),
        study.results(CustomMethod.scoreCodec(scoreId), StudyResultCodecs.signedDifference()),
        multiplier
      )
