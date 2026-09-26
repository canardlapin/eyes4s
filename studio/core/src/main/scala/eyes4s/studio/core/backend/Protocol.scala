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
  DiagnosticSource,
  Locus,
  ResultRef,
  SegmentTotal,
  StudyDesign,
  StudySegment
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

/** A trial's phase: an inventory attribute value, not a closed set. */
final case class Phase(label: String) derives CanEqual

object Phase:
  val Encoding: Phase  = Phase("Encoding")
  val Retrieval: Phase = Phase("Retrieval")
  given Codec[Phase]   = ProtocolCodecs.wrapper(Phase(_), _.label)

/** A reporting group's value of an inventory attribute, such as a retrieval
  * trial's response. Open: labels come from the data.
  */
final case class Response(label: String) derives CanEqual

object Response:
  val Remembered: Response = Response("Remembered")
  val Forgotten: Response  = Response("Forgotten")
  given Codec[Response]    = ProtocolCodecs.wrapper(Response(_), _.label)

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

  def render: String = this match
    case Matched => "matched"
    case Control => "control"

object PairDesign:
  def of(design: StudyDesign): PairDesign = design match
    case StudyDesign.Matched => Matched
    case StudyDesign.Control => Control

// ---------------------------------------------------------------------------
// Progress (eyes4s UI-D: StudySegment, StageMeter, run totals)
// ---------------------------------------------------------------------------

/** The kind of a study stage (UI-D `StageKind`). */
enum StageKind derives CanEqual, Codec.AsObject:
  case Estimating, Comparing, Reducing, Contrasting

/** What a stage counts (UI-D `CountUnit`). */
enum CountUnit derives CanEqual, Codec.AsObject:
  case Maps, Pairs, Keys, Rows

/** eyes4s `StudySegment`: a stage with its scale and, where it has one, its
  * design.
  */
enum Segment derives CanEqual, Codec.AsObject:
  case Estimating(scale: Int)
  case Comparing(scale: Int, design: PairDesign)
  case Reducing(scale: Int, design: PairDesign)
  case Contrasting(scale: Int)

  def scale: Int

  def kind: StageKind = this match
    case Estimating(_)   => StageKind.Estimating
    case Comparing(_, _) => StageKind.Comparing
    case Reducing(_, _)  => StageKind.Reducing
    case Contrasting(_)  => StageKind.Contrasting

  /** The unit each kind counts: maps estimated, pairs compared, keys reduced
    * and contrast rows.
    */
  def unit: CountUnit = kind match
    case StageKind.Estimating  => CountUnit.Maps
    case StageKind.Comparing   => CountUnit.Pairs
    case StageKind.Reducing    => CountUnit.Keys
    case StageKind.Contrasting => CountUnit.Rows

object Segment:
  def of(segment: StudySegment): Segment = segment match
    case StudySegment.Estimating(s)   => Estimating(s)
    case StudySegment.Comparing(s, d) => Comparing(s, PairDesign.of(d))
    case StudySegment.Reducing(s, d)  => Reducing(s, PairDesign.of(d))
    case StudySegment.Contrasting(s)  => Contrasting(s)

/** eyes4s `SegmentTotal`: what is known of a total before it is reached. */
enum ProgressTotal derives CanEqual, Codec.AsObject:
  case Exact(units: Long)
  case AtMost(units: Long)
  case Unknown

  /** The largest count that can be reached, when one is stated. */
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
  case Negative(field: String, value: Long)
  case BeyondTotal(field: String, done: Long, total: ProgressTotal)
  case UnitMismatch(segment: Segment, meter: StageMeter)

  def message: String = this match
    case Negative(field, value)          => s"Progress $field is negative: $value."
    case BeyondTotal(field, done, total) =>
      s"Progress $field reports $done, beyond its stated total $total."
    case UnitMismatch(segment, meter) =>
      s"Segment $segment counts ${segment.unit}, but its meter counts ${meter.unit} of ${meter.kind}."

private[backend] object Counted:
  def check(field: String, done: Long, total: ProgressTotal): Either[ProgressError, Unit] =
    if done < 0 then Left(ProgressError.Negative(field, done))
    else if total.bound.exists(done > _) then
      Left(ProgressError.BeyondTotal(field, done, total))
    else Right(())

/** UI-D `StageMeter`: one segment's count in its own unit. */
final case class StageMeter private (
    kind: StageKind,
    unit: CountUnit,
    done: Long,
    total: ProgressTotal
) derives CanEqual

object StageMeter:
  def of(
      kind: StageKind,
      unit: CountUnit,
      done: Long,
      total: ProgressTotal
  ): Either[ProgressError, StageMeter] =
    Counted.check("meter", done, total).map(_ => new StageMeter(kind, unit, done, total))

  given Encoder.AsObject[StageMeter] =
    Encoder.forProduct4("kind", "unit", "done", "total")(m => (m.kind, m.unit, m.done, m.total))

  given Decoder[StageMeter] =
    Decoder
      .forProduct4("kind", "unit", "done", "total")(of)
      .emap(_.left.map(_.message))

/** Run-cumulative counts: maps estimated and pairs compared over every scale.
  * The chip reads `completedPairs / totalPairs`.
  */
final case class RunTotals private (
    completedMaps: Long,
    totalMaps: ProgressTotal,
    completedPairs: Long,
    totalPairs: ProgressTotal
) derives CanEqual

object RunTotals:
  def of(
      completedMaps: Long,
      totalMaps: ProgressTotal,
      completedPairs: Long,
      totalPairs: ProgressTotal
  ): Either[ProgressError, RunTotals] =
    for
      _ <- Counted.check("completedMaps", completedMaps, totalMaps)
      _ <- Counted.check("completedPairs", completedPairs, totalPairs)
    yield new RunTotals(completedMaps, totalMaps, completedPairs, totalPairs)

  given Encoder.AsObject[RunTotals] =
    Encoder.forProduct4("completedMaps", "totalMaps", "completedPairs", "totalPairs")(t =>
      (t.completedMaps, t.totalMaps, t.completedPairs, t.totalPairs)
    )

  given Decoder[RunTotals] =
    Decoder
      .forProduct4("completedMaps", "totalMaps", "completedPairs", "totalPairs")(of)
      .emap(_.left.map(_.message))

/** One progress report of a job: eyes4s `RunProgress` with the UI-D meter.
  * The meter's kind and unit are the segment's.
  */
final case class JobProgress private (
    job: JobId,
    run: RunId,
    step: Long,
    segment: Segment,
    meter: StageMeter,
    totals: RunTotals
) derives CanEqual

object JobProgress:
  def of(
      job: JobId,
      run: RunId,
      step: Long,
      segment: Segment,
      meter: StageMeter,
      totals: RunTotals
  ): Either[ProgressError, JobProgress] =
    if step < 0 then Left(ProgressError.Negative("step", step))
    else if meter.kind != segment.kind || meter.unit != segment.unit then
      Left(ProgressError.UnitMismatch(segment, meter))
    else Right(new JobProgress(job, run, step, segment, meter, totals))

  given Encoder.AsObject[JobProgress] =
    Encoder.forProduct6("job", "run", "step", "segment", "meter", "totals")(p =>
      (p.job, p.run, p.step, p.segment, p.meter, p.totals)
    )

  given Decoder[JobProgress] =
    Decoder
      .forProduct6("job", "run", "step", "segment", "meter", "totals")(of)
      .emap(_.left.map(_.message))

// ---------------------------------------------------------------------------
// Diagnostics
// ---------------------------------------------------------------------------

/** eyes4s `DiagnosticSeverity`. */
enum DiagnosticLevel derives CanEqual, Codec.AsObject:
  case Error, Warning

/** eyes4s `DiagnosticSource`: the library, or studio's own checks. */
enum DiagnosticOrigin derives CanEqual, Codec.AsObject:
  case EyesCore, Host

/** eyes4s `Locus` over [[TrialKey]], every case kept with its typed operands. */
enum DiagnosticLocus derives CanEqual, Codec.AsObject:
  case Artifact(digest: String)
  case Definition(name: String, version: Int)
  case Field(name: String)
  case Scale(index: Int)
  case Design(design: PairDesign)
  case Repetition(name: String)
  case Window(name: String)
  case Trial(key: TrialKey)
  case Trials(keys: Vector[TrialKey])
  case TrialDigest(digest: String)
  case Pair(focal: TrialKey, reference: TrialKey)
  case Fixation(index: Int)
  case Record(number: Int)
  case Records(numbers: Vector[Int])
  case InputTrial(index: Int)
  case Recording(source: String)
  case Area(id: String)
  case Event(index: Int)
  case Sample(index: Int)
  case Samples(from: Int, until: Int)
  case Entry(name: String)
  case Path(path: String)
  case Relation(kind: String, source: String)
  case Line(source: String, line: Long)

  /** A participant of a report, by the name the study layout projects. */
  case Participant(name: String)

  /** A report group: one level of each grouping term, in grouping order; no
    * levels is the whole report.
    */
  case Group(levels: Vector[GroupLevel])

  // Studio's own subjects.
  case Dataset(dataset: DatasetRevision)
  case Revision(revision: AnalysisRevision)
  case Run(run: RunId)
  case Job(job: JobId)
  case Address(address: ResultAddress)

  /** A short English rendering for messages; never an identity. */
  def render: String = this match
    case Artifact(digest)       => s"artifact $digest"
    case Definition(name, v)    => s"definition $name@$v"
    case Field(name)            => s"field $name"
    case Scale(index)           => s"scale $index"
    case Design(design)         => design.render
    case Repetition(name)       => s"repetition $name"
    case Window(name)           => s"window $name"
    case Trial(key)             => key.label
    case Trials(keys)           => keys.map(_.label).mkString(", ")
    case TrialDigest(digest)    => s"trial $digest"
    case Pair(focal, reference) => s"${focal.label} × ${reference.label}"
    case Fixation(index)        => s"fixation $index"
    case Record(number)         => s"record $number"
    case Records(numbers)       => s"records ${numbers.mkString(", ")}"
    case InputTrial(index)      => s"input trial $index"
    case Recording(source)      => s"recording $source"
    case Area(id)               => s"area $id"
    case Event(index)           => s"event $index"
    case Sample(index)          => s"sample $index"
    case Samples(from, until)   => s"samples [$from, $until)"
    case Entry(name)            => s"entry $name"
    case Path(path)             => s"path $path"
    case Relation(kind, source) => s"$kind relation of $source"
    case Line(source, line)     => s"$source line $line"
    case Participant(name)      => s"participant $name"
    case Group(levels)          =>
      if levels.isEmpty then "whole report"
      else levels.map(l => s"${l.term} = ${l.level}").mkString(" · ")
    case Dataset(dataset)   => s"dataset ${dataset.label}"
    case Revision(revision) => revision.label
    case Run(run)           => run.label
    case Job(job)           => s"job ${job.number}"
    case Address(address)   => address.render

/** One level of a grouping term in a report group locus. */
final case class GroupLevel(term: String, level: String) derives CanEqual, Codec.AsObject

object DiagnosticLocus:
  def of[K](locus: Locus[K], key: K => TrialKey): DiagnosticLocus = locus match
    case Locus.Artifact(digest)       => Artifact(digest)
    case Locus.Definition(id)         => Definition(id.name, id.version)
    case Locus.Field(name)            => Field(name)
    case Locus.Scale(index)           => Scale(index)
    case Locus.Design(design)         => Design(PairDesign.of(design))
    case Locus.Repetition(name)       => Repetition(name)
    case Locus.Window(name)           => Window(name)
    case Locus.Trial(k)               => Trial(key(k))
    case Locus.Trials(ks)             => Trials(ks.map(key))
    case Locus.TrialDigest(digest)    => TrialDigest(digest)
    case Locus.Pair(focal, reference) => Pair(key(focal), key(reference))
    case Locus.Fixation(index)        => Fixation(index)
    case Locus.Record(number)         => Record(number)
    case Locus.Records(numbers)       => Records(numbers)
    case Locus.InputTrial(index)      => InputTrial(index)
    case Locus.Recording(source)      => Recording(source.value)
    case Locus.Area(id)               => Area(id)
    case Locus.Event(index)           => Event(index)
    case Locus.Sample(index)          => Sample(index)
    case Locus.Samples(from, until)   => Samples(from, until)
    case Locus.Entry(name)            => Entry(name)
    case Locus.Path(path)             => Path(path)
    case Locus.Relation(kind, source) => Relation(kind, source)
    case Locus.Line(source, line)     => Line(source, line)
    case Locus.Participant(name)      => Participant(name)
    case Locus.Group(levels) => Group(levels.map((term, level) => GroupLevel(term, level)))

/** A renderer-neutral diagnostic: eyes4s `Diagnostic` with a typed subject.
  * `code` is the stable identity (`family.case`); `message` is a default
  * English rendering, never an identity. Operands stay in eyes4s until S3.7
  * needs them on the wire.
  */
final case class StudioDiagnostic(
    code: String,
    level: DiagnosticLevel,
    origin: DiagnosticOrigin,
    subject: Vector[DiagnosticLocus],
    message: String
) derives CanEqual,
      Codec.AsObject

object StudioDiagnostic:
  def of[K](diagnostic: Diagnostic[K], key: K => TrialKey): StudioDiagnostic =
    StudioDiagnostic(
      diagnostic.code.render,
      diagnostic.severity match
        case DiagnosticSeverity.Error   => DiagnosticLevel.Error
        case DiagnosticSeverity.Warning => DiagnosticLevel.Warning
      ,
      diagnostic.source match
        case DiagnosticSource.EyesCore => DiagnosticOrigin.EyesCore
        case DiagnosticSource.Host     => DiagnosticOrigin.Host
      ,
      diagnostic.subject.map(DiagnosticLocus.of(_, key)),
      diagnostic.message
    )

// ---------------------------------------------------------------------------
// Jobs
// ---------------------------------------------------------------------------

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

/** One frame of a job subscription: eyes4s `RunEvent`. Progress is telemetry
  * and a slow reader sees the latest report; `Finished` is authoritative and
  * always last.
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

// ---------------------------------------------------------------------------
// Addresses and paging
// ---------------------------------------------------------------------------

/** A typed address of one result item: eyes4s `ResultRef` over [[TrialKey]].
  * Built from scale indices, designs and keys, never from a row position.
  */
enum ResultAddress derives CanEqual, Codec.AsObject:
  case Estimation(scale: Int, key: TrialKey)
  case PairRow(scale: Int, design: PairDesign, focal: TrialKey, reference: TrialKey)
  case Reduction(scale: Int, design: PairDesign, key: TrialKey)
  case ContrastRow(scale: Int, key: TrialKey)

  def scale: Int

  /** A short English rendering for messages; never an identity. */
  def render: String = this match
    case Estimation(s, k)          => s"estimation of ${k.label} at scale $s"
    case PairRow(s, d, focal, ref) =>
      s"${d.render} pair ${focal.label} × ${ref.label} at scale $s"
    case Reduction(s, d, k) => s"${d.render} reduction of ${k.label} at scale $s"
    case ContrastRow(s, k)  => s"contrast row of ${k.label} at scale $s"

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

/** Why a page request was refused. */
enum PageError derives CanEqual:
  case NegativeOffset(offset: Int)
  case SizeOutOfRange(size: Int, maximum: Int)

  def message: String = this match
    case NegativeOffset(offset)        => s"Page offset $offset is negative."
    case SizeOutOfRange(size, maximum) => s"Page size $size is outside 1 to $maximum."

/** One page of a listing: `offset` from the start, at most `size` entries. */
final case class PageRequest private (offset: Int, size: Int) derives CanEqual

object PageRequest:
  /** eyes4s `PageSize`: 1 to 4096 entries. */
  val MaximumSize: Int = 4096

  def of(offset: Int, size: Int): Either[PageError, PageRequest] =
    if offset < 0 then Left(PageError.NegativeOffset(offset))
    else if size < 1 || size > MaximumSize then
      Left(PageError.SizeOutOfRange(size, MaximumSize))
    else Right(new PageRequest(offset, size))

  def first(size: Int): Either[PageError, PageRequest] = of(0, size)

  given Encoder.AsObject[PageRequest] =
    Encoder.forProduct2("offset", "size")(p => (p.offset, p.size))

  given Decoder[PageRequest] =
    Decoder.forProduct2("offset", "size")(of).emap(_.left.map(_.message))

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
