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

import eyes4s.compare.Similarity
import eyes4s.core.*
import eyes4s.design.SignedDifference
import eyes4s.detect.{InterpolationGap, MinimumEventDuration}
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*

/** The recording and temporal result archives of the pinned inputs and plans:
  * an I-DT plan whose provenance is exactly recording-input-v1's evidence,
  * run on its monocular channels (its 6 ms interpolation gap fills the blink
  * and the signal loss, so the prepared recording differs from the angular
  * one and the first fixation spans interpolated samples), and the pinned
  * temporal-study-v1 plan run on temporal-study-input-v1.
  */
object ArchiveFixtures:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(e => throw new IllegalStateException(s"$e"), identity)
  private def id(name: String): DefinitionId = get(DefinitionId.of(name, 1))

  /** The I-DT plan family under the conventional recording plan schema. */
  val recordingCodec: RecordingPlanCodec[IdtParameters] = RecordingCodecs.idt(
    id("eyes4s.recording-plan"),
    id("eyes4s.recording.idt"),
    id("eyes4s.idt-parameters")
  )
  val recordingResults: RecordingResultCodec[IdtParameters] = recordingCodec.results

  val temporalCodec: TemporalStudyCodec[StudyKey, Px, Unit, Similarity, SignedDifference] =
    ConventionalPlanFixtures.temporalCodec
  val temporalResults: TemporalResultCodec[StudyKey, Px, Unit, Similarity, SignedDifference] =
    temporalCodec.results(StudyResultCodecs.similarity(), StudyResultCodecs.signedDifference())

  /** An I-DT plan (one degree square, two milliseconds minimum, a 6 ms
    * interpolation gap) whose declared provenance is exactly the input's
    * evidence: the same source, clocks, viewing geometry and observed marks.
    */
  def recordingPlanFor(
      input: RecordingInput[Px]
  ): Either[String, RecordingPlan[IdtParameters]] =
    for
      recording <- input.monocular.toRight("the recording input is not monocular")
      sync      <- input.synchronization.toRight("the recording input has no marks")
      area      <- Bounds
        .of[Px](0, 0, 800, 600)
        .left
        .map(_.message)
        .flatMap(b => RecordingArea.of("image", "Image", b).left.map(_.message))
      extent  <- Extent.of[Deg](1, 1).left.map(_.message)
      minimum <- MinimumEventDuration.of(Span.micros(2000)).left.map(_.message)
      gap     <- InterpolationGap.of(Span.micros(6000)).left.map(_.message)
      plan    <- RecordingPlan
        .of(
          ArtifactRef.of[Recording[Px]](recording.contentHash),
          input.source,
          recording.frame,
          recording.clock,
          sync.target,
          FrameId("angular"),
          input.viewing,
          sync.mode,
          sync.marks,
          sync.residualLimit,
          gap,
          Vector(area),
          recordingCodec.method,
          IdtParameters(extent, minimum)
        )
        .left
        .map(_.message)
      _ <- Either.cond(
        RecordingInput.disagreements(input, plan).isEmpty,
        (),
        s"the plan disagrees with its input: ${RecordingInput.disagreements(input, plan)}"
      )
    yield plan

  lazy val recordingPlan: RecordingPlan[IdtParameters] =
    get(recordingPlanFor(InputPayloadFixtures.input))

  lazy val recordingAnalysis: RecordingAnalysis[IdtParameters] =
    get(recordingPlan.run(InputPayloadFixtures.monocular))

  lazy val temporalResult
      : TemporalStudyResult[StudyKey, Px, Unit, Similarity, SignedDifference] =
    get(ConventionalPlanFixtures.temporalPlan.run(InputPayloadFixtures.temporal))
