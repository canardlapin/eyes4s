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
import eyes4s.core.*
import eyes4s.design.KeyDigest
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** One repeated temporal analysis an application offers over a fixation
  * route: the same key, method, parameter and score types, with windows
  * resolved against measured trial anchors and repetitions compared within
  * participant. Its temporal input stores the base study input by reference,
  * beside it in the same manifest.
  */
final class TemporalRoute[K, P, S, D](
    val study: AnalysisRoute[K, P, S, D],
    val persistence: TemporalStudyCodec[K, Px, P, S, D]
):
  def name: String = persistence.schema.name

  /** The temporal input codec over the route's own study input codec. */
  val inputs: TemporalInputCodec[K, Px] = new TemporalInputCodec(
    TemporalInputCodecs.input,
    study.inputs,
    StudyEmbedding.ByReference,
    TemporalInputCodecs.unresolved[K, Px]
  )

  /** The `temporal-result@1` archive with the route's own score codecs. */
  val results: TemporalResultCodec[K, Px, P, S, D] =
    persistence.results(study.results.scores, study.results.differences)

  /** The route's fixation registrations plus its temporal plan and result codecs. */
  def decoders: Either[CodecError, ArtifactDecoders[K, Px]] = for
    base     <- study.decoders
    plans    <- TemporalRegistry.empty[K, Px].register(persistence.registration)
    archived <- TemporalResultRegistry.empty[K, Px].register(results.registration)
  yield base.withTemporal(plans, archived)

  /** A temporal input: every listed trial's measured anchor and observed
    * coverage on the trial's own clock. A trial without an epoch stays
    * absent and fails as `MissingEpoch` at run time.
    */
  def input(
      base: StudyInput[K, Px],
      epochs: Vector[(K, TrialEpoch)]
  ): Either[TemporalStudyError, TemporalStudyInput[K, Px]] =
    given KeyDigest[K] = study.layout.digest
    given Ordering[K]  = study.layout.ordering
    TemporalStudyInput.of(base, epochs)

  /** The temporal plan over a base study plan of this route. */
  def plan(
      base: StudyPlan[K, Px, P, S, D],
      input: TemporalStudyInput[K, Px],
      windows: Vector[StudyWindow],
      repetitions: Vector[RepetitionContrast],
      boundary: FixationBoundary
  ): Either[TemporalStudyError, TemporalStudyPlan[K, Px, P, S, D]] =
    TemporalStudyPlan.of(base, input.reference, windows, repetitions, boundary)

object TemporalRoute:
  /** A temporal route over `study`, its plans saved under `schema`. */
  def of[K, P, S, D](
      study: AnalysisRoute[K, P, S, D],
      schema: String
  ): Either[RouteError, TemporalRoute[K, P, S, D]] =
    DefinitionId
      .of(schema, 1)
      .left
      .map(RouteError.Definition.apply)
      .map(id => new TemporalRoute(study, new TemporalStudyCodec(id, study.persistence)))
