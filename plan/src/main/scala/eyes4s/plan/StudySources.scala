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

import eyes4s.core.{RecordingRef, SampleRange}
import eyes4s.design.KeyDigest
import eyes4s.kernel.Unit2D

/** One admitted fixation's source: its trial, its zero-based position in the
  * trial's scanpath, the logical record that supplied it, and the ordinal that
  * record declared. Positions follow ordinal rank, as the ledger defines.
  */
final case class FixationSource[K](key: K, index: Int, record: Int, ordinal: Int)
    derives CanEqual

/** Recording samples behind each fixation of a source-supported scanpath:
  * `ranges(i)` supports fixation `i`. `artifact` is the content identity of
  * the recording the scanpath carries, which the study input encodes with it.
  */
final case class SampleSupport(
    recording: RecordingRef,
    artifact: ArtifactRef[?],
    ranges: Vector[SampleRange]
) derives CanEqual

/** Everything the evidence says about one trial key: its admitted fixations in
  * scanpath order, the records rejected under the same key (a quarantined trial
  * has only these), and its recording support when the scanpath has one.
  */
final case class TrialSources[K](
    key: K,
    admitted: Vector[FixationSource[K]],
    rejected: Vector[SourceRecord[K]],
    support: Option[SampleSupport]
) derives CanEqual:
  /** Every source record naming this key, in record order. */
  def records: Vector[Int] = (admitted.map(_.record) ++ rejected.map(_.record)).sorted

/** Source linkage for one study input.
  *
  * Built from the input and, when the input came through a ledgered import,
  * the admission ledger that produced it; the pair is checked with
  * `AdmissionLedger.checkAgainst`, so a ledger for different data is refused.
  * Every lookup answers with evidence or an explicit [[MissingSource]]: no
  * record number or fixation is guessed from row positions or display text.
  * `input` is the identity of the study input the index describes.
  */
final class StudySources[K] private (
    val input: ArtifactRef[? <: StudyInput[K, ?]],
    val ledger: Option[AdmissionLedger[K]],
    admittedByKey: Map[K, Vector[FixationSource[K]]],
    rejectedByKey: Map[K, Vector[SourceRecord[K]]],
    byRecord: Map[Int, SourceRecord[K]],
    inputKeys: Vector[K],
    occurrences: Map[K, Int],
    fixations: Map[K, Int],
    supports: Map[K, SampleSupport],
    digests: Map[String, Vector[K]]
):
  def source: Option[SourceRef] = ledger.map(_.source)

  /** The ledger entry for one logical record number. */
  def record(number: Int): Option[SourceRecord[K]] = byRecord.get(number)

  /** The trial key a record admitted or rejected, when it named one. */
  def trialOf(number: Int): Option[K] = byRecord
    .get(number)
    .flatMap(_.disposition match
      case Disposition.Admitted(key, _)    => Some(key)
      case Disposition.Rejected(_, key, _) => key)

  /** Evidence for one trial key; unknown to both the input and the ledger is missing. */
  def trial(key: K): Either[MissingSource[K], TrialSources[K]] =
    val admitted = admittedByKey.getOrElse(key, Vector.empty)
    val rejected = rejectedByKey.getOrElse(key, Vector.empty)
    if admitted.isEmpty && rejected.isEmpty && !occurrences.contains(key) then
      Left(MissingSource.UnknownTrial(key))
    else Right(TrialSources(key, admitted, rejected, supports.get(key)))

  /** The record that supplied fixation `index` of this trial. */
  def fixation(key: K, index: Int): Either[MissingSource[K], FixationSource[K]] =
    if ledger.isEmpty then Left(MissingSource.NoLedger)
    else
      admittedByKey.get(key) match
        case None =>
          Left(rejectedByKey.get(key).fold(MissingSource.UnknownTrial(key)) { rejected =>
            MissingSource.NotAdmitted(key, rejected.map(_.record))
          })
        case Some(admitted) =>
          admitted
            .lift(index)
            .toRight(MissingSource.FixationOutOfRange(key, index, admitted.size))

  /** The recording samples that support fixation `index` of this trial. */
  def samples(key: K, index: Int): Either[MissingSource[K], SourceLink[K]] =
    present(key, index).flatMap { _ =>
      supports.get(key) match
        case None          => Left(MissingSource.NotSourceSupported(key))
        case Some(support) =>
          support.ranges
            .lift(index)
            .map(range =>
              SourceLink.Samples(support.recording, support.artifact, range.from, range.until)
            )
            .toRight(MissingSource.FixationOutOfRange(key, index, support.ranges.size))
    }

  /** Record links for every source record of a trial, or an explicit missing link. */
  def links(key: K): Vector[SourceLink[K]] = ledger match
    case None          => Vector(SourceLink.Missing(MissingSource.NoLedger))
    case Some(entries) =>
      trial(key) match
        case Left(missing)                         => Vector(SourceLink.Missing(missing))
        case Right(found) if found.records.isEmpty =>
          Vector(SourceLink.Missing(MissingSource.UnknownTrial(key)))
        case Right(found) => found.records.map(SourceLink.Record(entries.source, _))

  /** Links for one fixation: the fixation itself, its record (or why none),
    * and its recording samples when the scanpath is source-supported. A
    * fixation the input does not have is a single explicit missing link.
    */
  def fixationLinks(key: K, index: Int): Vector[SourceLink[K]] =
    present(key, index) match
      case Left(missing) => Vector(SourceLink.Missing(missing))
      case Right(_)      =>
        val record = fixation(key, index) match
          case Right(found) =>
            ledger.toVector.map(entries => SourceLink.Record(entries.source, found.record))
          case Left(missing) => Vector(SourceLink.Missing(missing))
        val support =
          if supports.contains(key) then
            samples(key, index).fold(missing => Vector(SourceLink.Missing(missing)), Vector(_))
          else Vector.empty
        SourceLink.Fixation(key, index) +: (record ++ support)

  /** The input trial with this key, when exactly one exists, and the fixation
    * position within it.
    */
  private def present(key: K, index: Int): Either[MissingSource[K], Unit] =
    occurrences.get(key) match
      case None =>
        Left(rejectedByKey.get(key).fold(MissingSource.UnknownTrial(key)) { rejected =>
          MissingSource.NotAdmitted(key, rejected.map(_.record))
        })
      case Some(count) if count > 1 => Left(MissingSource.AmbiguousTrial(key, count))
      case Some(_)                  =>
        val count = fixations.getOrElse(key, 0)
        Either.cond(
          index >= 0 && index < count,
          (),
          MissingSource.FixationOutOfRange(key, index, count)
        )

  private def recordLink(number: Int): Vector[SourceLink[K]] = ledger match
    case None          => Vector(SourceLink.Missing(MissingSource.NoLedger))
    case Some(entries) => Vector(SourceLink.Record(entries.source, number))

  /** The trial an input position or retained key digest names, if exactly one. */
  private def resolved(locus: Locus[K]): Either[MissingSource[K], K] = locus match
    case Locus.InputTrial(index) =>
      inputKeys.lift(index).toRight(MissingSource.UnknownInputTrial(index, inputKeys.size))
    case Locus.TrialDigest(digest) =>
      digests.get(digest) match
        case Some(Vector(key)) => Right(key)
        case _                 => Left(MissingSource.UnresolvedDigest(digest))
    case _ => Left(MissingSource.NoLedger)

  /** Source links for the innermost object a subject names: a trial followed
    * by a fixation resolves to that fixation; a trial, pair or trial set to
    * every record of each trial; a record to itself. An input position or a
    * retained key digest resolves to its trial when exactly one input trial
    * has it, and to an explicit missing link otherwise. Coarser loci (a pair
    * around a failing trial, a scale, a window) add no links of their own.
    */
  def locate(subject: Vector[Locus[K]]): Vector[SourceLink[K]] =
    def trialLinks(key: K, i: Int): Vector[SourceLink[K]] =
      subject.lift(i + 1) match
        case Some(Locus.Fixation(index)) => fixationLinks(key, index)
        case _                           => links(key)
    val innermost = subject.zipWithIndex.reverse.collectFirst {
      case (
            locus @ (Locus.Trial(_) | Locus.Pair(_, _) | Locus.Trials(_) | Locus.Record(_) |
            Locus.Records(_) | Locus.InputTrial(_) | Locus.TrialDigest(_)),
            i
          ) =>
        locus -> i
    }
    innermost
      .fold(Vector.empty[SourceLink[K]]) {
        case (Locus.Trial(key), i)             => trialLinks(key, i)
        case (Locus.Pair(focal, reference), _) => links(focal) ++ links(reference)
        case (Locus.Trials(keys), _)           => keys.flatMap(links)
        case (Locus.Record(number), _)         => recordLink(number)
        case (Locus.Records(numbers), _)       => numbers.flatMap(recordLink)
        case (other, i)                        =>
          resolved(other).fold(missing => Vector(SourceLink.Missing(missing)), trialLinks(_, i))
      }
      .distinct

  /** The same diagnostic with the source links of its subject. */
  def link(diagnostic: Diagnostic[K]): Diagnostic[K] =
    diagnostic.copy(sources = (diagnostic.sources ++ locate(diagnostic.subject)).distinct)

  /** Every rejected record as a linked diagnostic naming its record, its trial
    * key when one was read, and the typed admission reason.
    */
  def rejections: Vector[Diagnostic[K]] = ledger.toVector.flatMap { entries =>
    entries.rejected.collect {
      case SourceRecord(number, Disposition.Rejected(_, key, reason)) =>
        val inner = Diagnostics.admissionReason(reason)
        val trial = key.map(k => Locus.Trial(k)).toVector
        inner
          .copy(subject = trial ++ (Locus.Record(number) +: inner.subject))
          .copy(sources = Vector(SourceLink.Record(entries.source, number)))
    }
  }

object StudySources:
  /** Sources of a ledgered import; the ledger must describe exactly this input.
    * The key digest resolves errors that retained only a digest.
    */
  def of[K: KeyDigest, U <: Unit2D](
      input: StudyInput[K, U],
      ledger: AdmissionLedger[K]
  ): Either[AdmissionError, StudySources[K]] =
    ledger.checkAgainst(input).map(_ => build(input, Some(ledger)))

  /** Sources of an input with no ledger: every record lookup is explicitly
    * missing; recording sample support is still available when present.
    */
  def unledgered[K: KeyDigest, U <: Unit2D](input: StudyInput[K, U]): StudySources[K] =
    build(input, None)

  private def build[K: KeyDigest, U <: Unit2D](
      input: StudyInput[K, U],
      ledger: Option[AdmissionLedger[K]]
  ): StudySources[K] =
    val entries  = ledger.fold(Vector.empty[SourceRecord[K]])(_.records)
    val admitted = entries
      .collect { case SourceRecord(number, Disposition.Admitted(key, ordinal)) =>
        (key, number, ordinal)
      }
      .groupBy(_._1)
      .view
      .mapValues(_.sortBy(_._3).zipWithIndex.map { case ((key, number, ordinal), index) =>
        FixationSource(key, index, number, ordinal)
      })
      .toMap
    val rejected = entries
      .collect { case entry @ SourceRecord(_, Disposition.Rejected(_, Some(key), _)) =>
        key -> entry
      }
      .groupMap(_._1)(_._2)
    val rows        = input.trials.rows
    val keys        = rows.map(_.key)
    val occurrences = rows.groupMapReduce(_.key)(_ => 1)(_ + _)
    val fixations   = rows.map(row => row.key -> row.value.n).toMap
    val supports    = rows
      .filter(row => occurrences(row.key) == 1)
      .flatMap { row =>
        val path = row.value
        for
          ref       <- path.source
          recording <- path.sourceRecording
          ranges    <- path.sampleSupport
        yield row.key -> SampleSupport(ref, ArtifactRef.of(recording.contentHash), ranges)
      }
      .toMap
    new StudySources(
      input.reference,
      ledger,
      admitted,
      rejected,
      entries.map(entry => entry.record -> entry).toMap,
      keys,
      occurrences,
      fixations,
      supports,
      keys.distinct.groupBy(key => KeyDigest[K].digest(key).render)
    )
