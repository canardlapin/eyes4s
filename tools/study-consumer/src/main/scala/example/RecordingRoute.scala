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
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** One recording analysis an application offers: the detector method, its
  * typed parameters and the versioned codecs that save and reload the plan
  * and its completed analysis (`recording-result@1`). The shipped I-VT and
  * the consumer's own laboratory detector are both routes; nothing here is
  * a parameter bag, a cast or an `Any` registry.
  */
final class RecordingRoute[P](
    val name: String,
    val persistence: RecordingPlanCodec[P],
    val parameters: P
):
  def method: RecordingMethod[P]       = persistence.method
  def results: RecordingResultCodec[P] = persistence.results

  /** The registrations a reader that starts from nothing makes: the shipped
    * study codecs a manifest's base decoders need, and this route's plan and
    * result codecs. Recording plans run on display pixels, and the manifest's
    * unit is `Px`, so the pixel witness is found by type, not by a cast.
    */
  def decoders: Either[CodecError, ArtifactDecoders[StudyKey, Px]] = for
    base     <- ArtifactDecoders.study[Px]
    plans    <- RecordingRegistry.empty.register(persistence.registration)
    archived <- RecordingResultRegistry.empty.register(results.registration)
  yield base.withRecordings(plans, archived)

  /** The plan that runs this route's detector on one recording input. It
    * declares exactly the input's evidence (source, clocks, viewing geometry,
    * synchronization marks and residual limit), so
    * `RecordingInput.disagreements` is empty by construction and the plan
    * records no provenance the input does not support.
    */
  def plan(
      input: RecordingInput[Px],
      angular: FrameId,
      gap: InterpolationGap,
      areas: Vector[RecordingArea]
  ): Either[RecordingRouteError, RecordingPlan[P]] =
    for
      recording <- input.monocular.toRight(RecordingRouteError.Binocular(input.source))
      sync <- input.synchronization.toRight(RecordingRouteError.Unsynchronized(input.source))
      plan <- RecordingPlan
        .of(
          ArtifactRef.of[Recording[Px]](recording.contentHash),
          input.source,
          recording.frame,
          recording.clock,
          sync.target,
          angular,
          input.viewing,
          sync.mode,
          sync.marks,
          sync.residualLimit,
          gap,
          areas,
          method,
          parameters
        )
        .left
        .map(RecordingRouteError.Plan.apply)
    yield plan

/** Why a recording route or its plan could not be assembled. */
enum RecordingRouteError derives CanEqual:
  case Definition(error: PlanError)
  case Descriptor(error: DescriptorError)
  case Binocular(source: RecordingRef)
  case Unsynchronized(source: RecordingRef)
  case Plan(error: RecordingPlanError)

object RecordingRoute:
  private def id(name: String) =
    DefinitionId.of(name, 1).left.map(RecordingRouteError.Definition.apply)

  /** The shipped I-VT under the conventional recording-plan schemas. */
  def ivt(
      threshold: IvtThreshold,
      minimum: MinimumEventDuration
  ): Either[RecordingRouteError, RecordingRoute[IvtParameters]] =
    for
      schema     <- id("eyes4s.recording-plan")
      method     <- id("eyes4s.recording.ivt")
      parameters <- id("eyes4s.ivt-parameters")
    yield new RecordingRoute(
      "ivt",
      RecordingCodecs.ivt(schema, method, parameters),
      IvtParameters(threshold, minimum)
    )

  /** The consumer's laboratory detector, registered under its own schemas. */
  def lab(
      parameters: LabIvtParameters
  ): Either[RecordingRouteError, RecordingRoute[LabIvtParameters]] =
    for
      schema    <- id("my.lab.recording-plan")
      method    <- id("my.lab.conservative-ivt")
      parameter <- id("my.lab.conservative-ivt-parameters")
      codec     <- CustomDetector
        .persistence(schema, method, parameter)
        .left
        .map(RecordingRouteError.Descriptor.apply)
    yield new RecordingRoute("lab", codec, parameters)
