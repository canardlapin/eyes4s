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

package eyes4s.plan

import eyes4s.core.*
import eyes4s.kernel.*

/** The gaze channels an importer normalized: one eye, or two eyes whose
  * samples remain paired. A binocular input keeps both eyes so that a later
  * projection (`left`, `right`, `cyclopean`) is an explicit, recorded choice.
  */
enum RecordingChannels[U <: Unit2D]:
  case Monocular(recording: Recording[U])
  case Binocular(recording: BinocularRecording[U])

  def frame: Frame[U] = this match
    case Monocular(r) => r.frame
    case Binocular(b) => b.frame

  def clock: ClockId = this match
    case Monocular(r) => r.clock
    case Binocular(b) => b.clock

  def size: Int = this match
    case Monocular(r) => r.size
    case Binocular(b) => b.size

  /** Semantic identity of the channels; monocular identity is
    * `Recording.contentHash`, so `RecordingPlan.prerequisites` accepts a
    * decoded monocular input by the same digest.
    */
  def contentHash: ContentHash = this match
    case Monocular(r) => r.contentHash
    case Binocular(b) => b.contentHash

/** The observed common marks from which a synchronization is fitted. This is
  * input evidence: the fit itself is deterministic from these operands, so a
  * consumer that holds them can refit and inspect residuals without receiving
  * an unexplained pair of coefficients.
  */
final case class ObservedSynchronization(
    target: ClockId,
    mode: SyncFitMode,
    marks: Vector[SyncMark],
    residualLimit: Option[SyncResidualLimit]
) derives CanEqual:
  def fit(source: ClockId): Either[SyncEvidenceError, SyncEvidence] =
    SyncEvidence.fromCommonMarks(source, target, mode, marks, residualLimit)

/** A normalized recording input: the channels, the bench geometry when it
  * was recorded, and the synchronization marks observed against another
  * clock. Identity covers the source name, the channels, the viewing
  * geometry and every observed mark; `RecordingInput.disagreements` names
  * where a plan's declared provenance departs from this evidence.
  */
final class RecordingInput[U <: Unit2D] private (
    val source: RecordingRef,
    val channels: RecordingChannels[U],
    val viewing: Option[Viewing],
    val synchronization: Option[ObservedSynchronization],
    val hash: ContentHash
):
  def reference: ArtifactRef[RecordingInput[U]] = ArtifactRef.of(hash)
  def frame: Frame[U]                           = channels.frame
  def clock: ClockId                            = channels.clock

  def monocular: Option[Recording[U]] = channels match
    case RecordingChannels.Monocular(r) => Some(r)
    case RecordingChannels.Binocular(_) => None

  /** Refit the observed marks from the recording's own clock. */
  def synchronize: Option[Either[SyncEvidenceError, SyncEvidence]] =
    synchronization.map(_.fit(clock))

object RecordingInput:
  /** Where a recording plan's declared provenance departs from the input's
    * evidence: the source name, clocks, viewing geometry, fit mode, observed
    * marks and residual limit, followed by the plan's own prerequisites over
    * the monocular recording. Empty means the plan may run on this input
    * without changing any provenance it will record.
    */
  def disagreements[P](
      input: RecordingInput[Unit2D.Px],
      plan: RecordingPlan[P]
  ): Vector[RecordingInputError] =
    def differs(
        field: String,
        declared: String,
        evidence: String
    ): Option[RecordingInputError] =
      Option.when(declared != evidence)(
        RecordingInputError.PlanDisagreement(field, declared, evidence)
      )
    val marks = input.synchronization.fold(Vector.empty[SyncMark])(_.marks)
    val named = Vector(
      differs("source", plan.source.value, input.source.value),
      differs("trackerClock", plan.trackerClock.name, input.clock.name),
      differs(
        "analysisClock",
        plan.analysisClock.name,
        input.synchronization.fold("none")(_.target.name)
      ),
      differs(
        "viewing",
        plan.viewing.fold("none")(_.render),
        input.viewing.fold("none")(_.render)
      ),
      differs(
        "synchronizationModel",
        plan.synchronizationModel.render,
        input.synchronization.fold("none")(_.mode.render)
      ),
      differs(
        "marks",
        plan.marks.map(RecordingInput.renderMark).mkString(","),
        marks.map(RecordingInput.renderMark).mkString(",")
      ),
      differs(
        "residualLimit",
        plan.residualLimit.fold("none")(_.span.toMicros.toString),
        input.synchronization.flatMap(_.residualLimit).fold("none")(_.span.toMicros.toString)
      )
    ).flatten
    val channels = input.monocular match
      case Some(recording) =>
        plan.prerequisites(Some(recording)).map(RecordingInputError.Plan.apply)
      case None => Vector(RecordingInputError.BinocularChannels(input.source))
    named ++ channels

  private def renderMark(mark: SyncMark): String =
    s"${mark.id}@${mark.onSource.toMicros}->${mark.onTarget.toMicros}"

  def of[U <: Unit2D](
      source: RecordingRef,
      channels: RecordingChannels[U],
      viewing: Option[Viewing],
      synchronization: Option[ObservedSynchronization]
  ): Either[RecordingInputError, RecordingInput[U]] =
    if source.value.trim.isEmpty then Left(RecordingInputError.EmptySource(source))
    else
      synchronization match
        case Some(sync) if sync.target == channels.clock =>
          Left(RecordingInputError.SynchronizationTargetIsSource(channels.clock))
        case Some(sync) if sync.marks.isEmpty =>
          Left(RecordingInputError.NoSynchronizationMarks(channels.clock, sync.target))
        case Some(sync) =>
          sync
            .fit(channels.clock)
            .left
            .map(RecordingInputError.Synchronization.apply)
            .map(_ => build(source, channels, viewing, synchronization))
        case None => Right(build(source, channels, viewing, synchronization))

  private def build[U <: Unit2D](
      source: RecordingRef,
      channels: RecordingChannels[U],
      viewing: Option[Viewing],
      synchronization: Option[ObservedSynchronization]
  ): RecordingInput[U] =
    val viewingHash = viewing match
      case None    => ContentHash.ofString("viewing:none")
      case Some(v) =>
        ContentHash.combine(
          ContentHash.ofString("viewing:mm"),
          ContentHash.of(
            IArray(
              v.perspective.distance.toMm,
              v.perspective.surfaceWidth.toMm,
              v.perspective.surfaceHeight.toMm
            )
          )
        )
    val syncHash = synchronization match
      case None       => ContentHash.ofString("sync:none")
      case Some(sync) =>
        ContentHash.combineAll(
          Seq(
            ContentHash.ofString("sync:marks"),
            ContentHash.ofString(channels.clock.name),
            ContentHash.ofString(sync.target.name),
            ContentHash.ofString(sync.mode.render),
            sync.residualLimit.fold(ContentHash.ofString("limit:none"))(limit =>
              ContentHash.ofString(limit.span.toMicros.toString)
            )
          ) ++ sync.marks.flatMap(mark =>
            Seq(
              ContentHash.ofString(mark.id),
              ContentHash.ofString(mark.onSource.toMicros.toString),
              ContentHash.ofString(mark.onTarget.toMicros.toString)
            )
          )
        )
    new RecordingInput(
      source,
      channels,
      viewing,
      synchronization,
      ContentHash.combineAll(
        Seq(
          ContentHash.ofString("recording-input:v1"),
          ContentHash.ofString(source.value),
          channels.contentHash,
          viewingHash,
          syncHash
        )
      )
    )

enum RecordingInputError derives CanEqual:
  case EmptySource(source: RecordingRef)
  case SynchronizationTargetIsSource(clock: ClockId)
  case NoSynchronizationMarks(source: ClockId, target: ClockId)
  case Synchronization(underlying: SyncEvidenceError)
  case PlanDisagreement(field: String, plan: String, evidence: String)
  case BinocularChannels(source: RecordingRef)
  case Plan(underlying: RecordingPlanError)

  def message: String = this match
    case EmptySource(source) => s"Recording input source must be named, got '$source'."
    case SynchronizationTargetIsSource(clock) =>
      s"Recording input synchronization must map clock '$clock' to a different clock."
    case NoSynchronizationMarks(source, target) =>
      s"Recording input synchronization from '$source' to '$target' declares no observed marks."
    case Synchronization(e)                      => e.message
    case PlanDisagreement(field, plan, evidence) =>
      s"Recording plan declares $field=$plan but the recording input evidences $field=$evidence."
    case BinocularChannels(source) =>
      s"Recording input '$source' is binocular; project one eye explicitly before running a recording plan."
    case Plan(e) => e.message
