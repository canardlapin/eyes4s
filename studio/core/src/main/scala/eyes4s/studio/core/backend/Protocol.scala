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

package eyes4s.studio.core.backend

import eyes4s.plan.{
  Diagnostic,
  DiagnosticSeverity,
  Locus,
  ResultRef,
  SegmentTotal,
  StudyDesign,
  StudyStage
}
import io.circe.{Codec, Decoder, Encoder}

/** The vocabulary of the [[StudyBackend]] protocol (DESIGN_SPEC section 13,
  * ticket S3.0).
  *
  * Every type here is owned by studio and serializable, so one protocol serves
  * the in-process and the IPC transports. Each is shaped like the eyes4s value
  * the real backend (S3.7) will wrap, and names that value's conversion
  * (`of`) where eyes4s already has it.
  */
private[backend] object ProtocolCodecs:
  /** A single-field wrapper encoded as its field. */
  def wrapper[A, B: Encoder: Decoder](wrap: B => A, unwrap: A => B): Codec[A] =
    Codec.from(Decoder[B].map(wrap), Encoder[B].contramap(unwrap))

  /** A validated value decoded through its smart constructor. */
  def validated[A, B: Encoder: Decoder](
      parse: B => Either[String, A],
      unwrap: A => B
  ): Codec[A] =
    Codec.from(Decoder[B].emap(parse), Encoder[B].contramap(unwrap))

/** A dataset revision, displayed `r3`. */
final case class DatasetRevision(number: Int) derives CanEqual:
  def label: String = s"r$number"

object DatasetRevision:
  given Codec[DatasetRevision] = ProtocolCodecs.wrapper(DatasetRevision(_), _.number)

/** An analysis revision, displayed `rev 4`. */
final case class AnalysisRevision(number: Int) derives CanEqual:
  def label: String = s"rev $number"

object AnalysisRevision:
  given Codec[AnalysisRevision] = ProtocolCodecs.wrapper(AnalysisRevision(_), _.number)

/** A run of one analysis revision on one dataset revision, displayed `run 7`. */
final case class RunId(number: Int) derives CanEqual:
  def label: String = s"run $number"

object RunId:
  given Codec[RunId] = ProtocolCodecs.wrapper(RunId(_), _.number)

/** One submission to the backend; a job produces at most one run. */
final case class JobId(number: Int) derives CanEqual

object JobId:
  given Codec[JobId] = ProtocolCodecs.wrapper(JobId(_), _.number)

enum Phase derives CanEqual, Codec.AsObject:
  case Encoding, Retrieval

/** The full identity of one inventory trial (FIXTURE.md: participant + phase +
  * trial + occurrence). An item is an attribute of a trial, not part of its key.
  */
final case class TrialKey(participant: String, phase: Phase, trial: String, occurrence: Int)
    derives CanEqual,
      Codec.AsObject:
  def label: String = s"$participant · $trial"

/** eyes4s `StudyDesign`: the matched reference or the control pool. */
enum PairDesign derives CanEqual, Codec.AsObject:
  case Matched, Control

object PairDesign:
  def of(design: StudyDesign): PairDesign = design match
    case StudyDesign.Matched => Matched
    case StudyDesign.Control => Control

/** The response a retrieval trial recorded. */
enum Response derives CanEqual, Codec.AsObject:
  case Remembered, Forgotten

/** eyes4s `StudyStage` without its indices: the stage a job reports. */
enum JobStage derives CanEqual, Codec.AsObject:
  case Estimating, Comparing, Reducing, Contrasting

object JobStage:
  def of(stage: StudyStage): JobStage = stage match
    case StudyStage.Estimating(_, _) => Estimating
    case StudyStage.Comparing(_, _)  => Comparing
    case StudyStage.Reducing(_, _)   => Reducing
    case StudyStage.Contrasting(_)   => Contrasting

/** eyes4s `SegmentTotal`: what is known of a stage's total before it ends. */
enum ProgressTotal derives CanEqual, Codec.AsObject:
  case Exact(units: Long)
  case AtMost(units: Long)
  case Unknown

  /** The largest count the stage can reach, when one is stated. */
  def bound: Option[Long] = this match
    case Exact(units)  => Some(units)
    case AtMost(units) => Some(units)
    case Unknown       => None

object ProgressTotal:
  def of(total: SegmentTotal): ProgressTotal = total match
    case SegmentTotal.Exact(units)  => Exact(units)
    case SegmentTotal.AtMost(units) => AtMost(units)
    case SegmentTotal.Unknown       => Unknown

/** Why a progress report was refused. */
enum ProgressError derives CanEqual:
  case NegativeStep(job: JobId, step: Long)
  case NegativeDone(job: JobId, done: Long)
  case BeyondTotal(job: JobId, stage: JobStage, done: Long, total: ProgressTotal)

  def message: String = this match
    case NegativeStep(job, step) => s"Job ${job.number} reports a negative step $step."
    case NegativeDone(job, done) => s"Job ${job.number} reports negative progress $done."
    case BeyondTotal(job, stage, done, total) =>
      s"Job ${job.number} reports $done units of $stage, beyond its stated total $total."

/** One progress report of a job: eyes4s `RunProgress` summed over a stage.
  * `done` never exceeds the stated total.
  */
final case class JobProgress private (
    job: JobId,
    run: RunId,
    step: Long,
    stage: JobStage,
    done: Long,
    total: ProgressTotal
) derives CanEqual

object JobProgress:
  def of(
      job: JobId,
      run: RunId,
      step: Long,
      stage: JobStage,
      done: Long,
      total: ProgressTotal
  ): Either[ProgressError, JobProgress] =
    if step < 0 then Left(ProgressError.NegativeStep(job, step))
    else if done < 0 then Left(ProgressError.NegativeDone(job, done))
    else if total.bound.exists(done > _) then
      Left(ProgressError.BeyondTotal(job, stage, done, total))
    else Right(new JobProgress(job, run, step, stage, done, total))

  given Encoder.AsObject[JobProgress] =
    Encoder.forProduct6("job", "run", "step", "stage", "done", "total")(p =>
      (p.job, p.run, p.step, p.stage, p.done, p.total)
    )

  given Decoder[JobProgress] =
    Decoder
      .forProduct6("job", "run", "step", "stage", "done", "total")(
        (
            job: JobId,
            run: RunId,
            step: Long,
            stage: JobStage,
            done: Long,
            total: ProgressTotal
        ) => of(job, run, step, stage, done, total)
      )
      .emap(_.left.map(_.message))

/** eyes4s `DiagnosticSeverity`. */
enum DiagnosticLevel derives CanEqual, Codec.AsObject:
  case Error, Warning

/** A renderer-neutral diagnostic: eyes4s `Diagnostic` with its subject
  * rendered. `code` is the stable identity (`family.case`); `message` is a
  * default English rendering, never an identity.
  */
final case class StudioDiagnostic(
    code: String,
    level: DiagnosticLevel,
    subject: Vector[String],
    message: String
) derives CanEqual,
      Codec.AsObject

object StudioDiagnostic:
  def of[K](diagnostic: Diagnostic[K], key: K => String): StudioDiagnostic =
    StudioDiagnostic(
      diagnostic.code.render,
      diagnostic.severity match
        case DiagnosticSeverity.Error   => DiagnosticLevel.Error
        case DiagnosticSeverity.Warning => DiagnosticLevel.Warning
      ,
      diagnostic.subject.map {
        case Locus.Trial(k)               => key(k)
        case Locus.Trials(ks)             => ks.map(key).mkString(", ")
        case Locus.Pair(focal, reference) => s"${key(focal)} × ${key(reference)}"
        case other                        => other.toString
      },
      diagnostic.message
    )

/** The end of a job: eyes4s `RunOutcome`. A run exists as a result only after
  * `Completed`; `Cancelled` and `Failed` carry the last progress, if any.
  */
enum JobOutcome derives CanEqual, Codec.AsObject:
  case Completed(job: JobId, run: RunId, last: JobProgress)
  case Cancelled(job: JobId, run: RunId, last: Option[JobProgress])
  case Failed(
      job: JobId,
      run: RunId,
      diagnostics: Vector[StudioDiagnostic],
      last: Option[JobProgress]
  )

  def job: JobId
  def run: RunId

  def progress: Option[JobProgress] = this match
    case Completed(_, _, last) => Some(last)
    case Cancelled(_, _, last) => last
    case Failed(_, _, _, last) => last

/** One element of a job's event stream: eyes4s `RunEvent`. Progress is
  * telemetry and a slow reader sees the latest report; `Finished` is
  * authoritative and always last.
  */
enum JobEvent derives CanEqual, Codec.AsObject:
  case Advanced(progress: JobProgress)
  case Finished(outcome: JobOutcome)

enum JobState derives CanEqual, Codec.AsObject:
  case Queued
  case Running(progress: JobProgress)
  case Finished(outcome: JobOutcome)

final case class JobStatus(
    job: JobId,
    run: RunId,
    revision: AnalysisRevision,
    dataset: DatasetRevision,
    state: JobState
) derives CanEqual,
      Codec.AsObject

/** A typed address of one result item: eyes4s `ResultRef` over [[TrialKey]].
  * Built from scale indices, designs and keys, never from a row position.
  */
enum ResultAddress derives CanEqual, Codec.AsObject:
  case Estimation(scale: Int, key: TrialKey)
  case PairRow(scale: Int, design: PairDesign, focal: TrialKey, reference: TrialKey)
  case Reduction(scale: Int, design: PairDesign, key: TrialKey)
  case ContrastRow(scale: Int, key: TrialKey)

  def scale: Int

object ResultAddress:
  /** The study references; temporal and recording references have no address yet. */
  def of[K](ref: ResultRef[K], key: K => TrialKey): Option[ResultAddress] = ref match
    case ResultRef.Estimation(scale, k)                     => Some(Estimation(scale, key(k)))
    case ResultRef.PairRow(scale, design, focal, reference) =>
      Some(PairRow(scale, PairDesign.of(design), key(focal), key(reference)))
    case ResultRef.Reduction(scale, design, k) =>
      Some(Reduction(scale, PairDesign.of(design), key(k)))
    case ResultRef.ContrastRow(scale, k) => Some(ContrastRow(scale, key(k)))
    case _                               => None

/** One page of a listing: `offset` from the start, at most `size` entries. */
final case class PageRequest private (offset: Int, size: Int) derives CanEqual

object PageRequest:
  /** eyes4s `PageSize`: 1 to 4096 entries. */
  val MaximumSize: Int = 4096

  def of(offset: Int, size: Int): Either[String, PageRequest] =
    if offset < 0 then Left(s"Page offset $offset is negative.")
    else if size < 1 || size > MaximumSize then
      Left(s"Page size $size is outside 1 to $MaximumSize.")
    else Right(new PageRequest(offset, size))

  def first(size: Int): Either[String, PageRequest] = of(0, size)

  given Encoder.AsObject[PageRequest] =
    Encoder.forProduct2("offset", "size")(p => (p.offset, p.size))

  given Decoder[PageRequest] =
    Decoder.forProduct2("offset", "size")((o: Int, s: Int) => of(o, s)).emap(identity)

/** Where a listing's page sits: its offset, the listing's total and the offset
  * the next page starts at, if any.
  */
final case class PageInfo(offset: Int, total: Int, next: Option[Int])
    derives CanEqual,
      Codec.AsObject

object PageInfo:
  def of(request: PageRequest, total: Int, returned: Int): PageInfo =
    val end = request.offset + returned
    PageInfo(request.offset, total, Option.when(end < total)(end))
