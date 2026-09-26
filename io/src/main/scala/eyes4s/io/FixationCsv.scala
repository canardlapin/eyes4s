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

package eyes4s.io

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.*

/** Raw fixation table columns. Time units are declared separately, never inferred. */
final case class FixationColumns private (
    ordinal: String,
    x: String,
    y: String,
    onset: String,
    duration: String,
    sampleCount: String
) derives CanEqual:
  def names: Vector[String] = Vector(ordinal, x, y, onset, duration, sampleCount)
object FixationColumns:
  def of(
      ordinal: String,
      x: String,
      y: String,
      onset: String,
      duration: String,
      sampleCount: String
  ): Either[FixationImportError, FixationColumns] =
    val names = Vector(ordinal, x, y, onset, duration, sampleCount)
    if names.exists(_.trim.isEmpty) || names.distinct.size != names.size then
      Left(FixationImportError.Columns(names))
    else Right(new FixationColumns(ordinal, x, y, onset, duration, sampleCount))

/** A typed boundary reader for user keys; only the raw file boundary uses column strings. */
/** An attribute a key carries beside its identity, such as the item a trial
  * is matched on: keys with one identity must carry one value.
  */
private[io] trait KeyAttribute[K]:
  type Identity
  def identity(key: K): Identity
  def item(key: K): String
  def occurrence(key: K): Int

final class FixationKeyReader[K] private (
    val columns: Vector[String],
    val read: Map[String, String] => Either[String, K],
    val clock: K => ClockId,
    val participant: Option[K => String],
    private[io] val attribute: Option[KeyAttribute[K]] = None
)(using val digest: KeyDigest[K], val ordering: Ordering[K])
object FixationKeyReader:
  def of[K: KeyDigest: Ordering](columns: Vector[String])(
      read: Map[String, String] => Either[String, K],
      clock: K => ClockId
  ): Either[FixationImportError, FixationKeyReader[K]] =
    make(columns, read, clock, None)

  /** A reader whose keys name their participant, so participant-scoped
    * corrections can be resolved.
    */
  def withParticipant[K: KeyDigest: Ordering](columns: Vector[String])(
      read: Map[String, String] => Either[String, K],
      clock: K => ClockId,
      participant: K => String
  ): Either[FixationImportError, FixationKeyReader[K]] =
    make(columns, read, clock, Some(participant))

  private def make[K: KeyDigest: Ordering](
      columns: Vector[String],
      read: Map[String, String] => Either[String, K],
      clock: K => ClockId,
      participant: Option[K => String]
  ): Either[FixationImportError, FixationKeyReader[K]] =
    if columns.isEmpty || columns.exists(
        _.trim.isEmpty
      ) || columns.distinct.size != columns.size
    then Left(FixationImportError.Columns(columns))
    else Right(new FixationKeyReader(columns, read, clock, participant))

  /** Trial keys: participant, phase and trial columns, an optional occurrence
    * column (1 when absent) and the item column the trial is matched on. Rows
    * of one trial identity that name different items quarantine the trial
    * with `QuarantineCause.ItemConflict`.
    */
  def trial(
      participant: String,
      phase: String,
      trial: String,
      item: String,
      occurrence: Option[String] = None
  ): Either[FixationImportError, FixationKeyReader[TrialKey]] =
    val columns = Vector(participant, phase, trial, item) ++ occurrence.toVector
    make[TrialKey](
      columns,
      fields =>
        def text(column: String) =
          fields.get(column).filter(_.nonEmpty).toRight(s"Missing column '$column'.")
        for
          p <- text(participant)
          f <- text(phase)
          t <- text(trial)
          i <- text(item)
          n <- occurrence.fold[Either[String, TrialOccurrence]](Right(TrialOccurrence.first))(
            column =>
              text(column).flatMap(raw =>
                raw.toIntOption
                  .toRight(s"Occurrence '$raw' in column '$column' is not an integer.")
                  .flatMap(TrialOccurrence.of(_).left.map(_.message))
              )
          )
          key <- TrialKey.of(p, f, t, n, i).left.map(_.message)
        yield key
      ,
      key => ClockId(s"fixation-trial:${KeyDigest[TrialKey].digest(key).render}"),
      Some(_.participant)
    ).map(reader =>
      new FixationKeyReader(
        reader.columns,
        reader.read,
        reader.clock,
        reader.participant,
        Some(new KeyAttribute[TrialKey]:
          type Identity = (String, String, String)
          def identity(key: TrialKey)   = (key.participant, key.phase, key.trial)
          def item(key: TrialKey)       = key.item
          def occurrence(key: TrialKey) = key.occurrence.value)
      )
    )

  def study(
      participant: String,
      stimulus: String,
      phase: String
  ): Either[FixationImportError, FixationKeyReader[StudyKey]] =
    withParticipant[StudyKey](Vector(participant, stimulus, phase))(
      fields =>
        for
          p <- fields
            .get(participant)
            .filter(_.nonEmpty)
            .toRight(s"Missing participant column '$participant'.")
          s <- fields
            .get(stimulus)
            .filter(_.nonEmpty)
            .toRight(s"Missing stimulus column '$stimulus'.")
          phaseValue <- fields
            .get(phase)
            .filter(_.nonEmpty)
            .toRight(s"Missing phase column '$phase'.")
        yield StudyKey(p, s, phaseValue),
      key => ClockId(s"fixation-trial:${KeyDigest[StudyKey].digest(key).render}"),
      _.participant
    )

/** File-level errors leave the source untouched. Row-level defects live in the report. */
enum FixationImportError derives CanEqual:
  case Csv(underlying: TidyCsvError)
  case Columns(names: Vector[String])
  case Header(found: Vector[String], required: Vector[String])
  case Incomplete(rejectedRows: Vector[Int])
  case ParticipantScope(rules: Vector[Int])
  def message: String = this match
    case Csv(error)     => error.message
    case Columns(names) => s"Fixation columns must be distinct non-empty names: $names."
    case Header(found, required) =>
      s"Fixation header $found must have unique columns and contain $required."
    case Incomplete(rows) =>
      s"Fixation study has rejected source rows $rows; inspect the import report before analysis."
    case ParticipantScope(rules) =>
      s"Correction rules $rules are scoped to a participant, but the key reader names no " +
        "participant; use FixationKeyReader.withParticipant."

enum FixationRowError derives CanEqual:
  case Width(expected: Int, actual: Int)
  case Key(reason: String)
  case Number(column: String, value: String, requirement: String)
  case Time(onset: String, duration: String, unit: TimestampUnit, reason: String)
  case Position(x: Double, y: Double, frame: FrameId)
  case Event(reason: String)
  case Trial(rows: Vector[Int], cause: QuarantineCause)
  def message: String = this match
    case Width(e, a)        => s"Expected $e fields, got $a."
    case Key(reason)        => s"Trial key: $reason"
    case Number(c, v, r)    => s"Column '$c' has '$v'; expected $r."
    case Time(o, d, u, r)   => s"Onset '$o', duration '$d' in $u: $r"
    case Position(x, y, f)  => s"Position ($x,$y) is outside frame $f."
    case Event(reason)      => s"Fixation: $reason"
    case Trial(rows, cause) => s"Trial from rows $rows: ${cause.message}"

final case class RejectedFixationRow[K] private[io] (
    rowNumber: Int,
    raw: Vector[String],
    key: Option[K],
    error: FixationRowError
) derives CanEqual

/** The record/ordinal link of one admitted source row to its typed trial key. */
final case class AdmittedFixationRow[K] private[io] (rowNumber: Int, key: K, ordinal: Int)
    derives CanEqual

/** The default admission refuses incomplete studies. Accepted trial groups are
  * separately available for an analyst who explicitly reviews the exclusions.
  * No trial containing a bad keyed row is partially reconstructed.
  */
final class FixationImport[K, U <: Unit2D] private[io] (
    val header: Vector[String],
    val sourceRows: Vector[Vector[String]],
    val accepted: Trials[K, Unit, Scanpath[U]],
    val admitted: Vector[AdmittedFixationRow[K]],
    val rejected: Vector[RejectedFixationRow[K]],
    val policy: AdmissionPolicy[K],
    val outsideFrame: Vector[OutsideFrame]
)(using KeyDigest[K], UnitLabel[U]):
  def requireComplete: Either[FixationImportError, StudyInput[K, U]] =
    if rejected.nonEmpty then Left(FixationImportError.Incomplete(rejected.map(_.rowNumber)))
    else Right(StudyInput(accepted))

object FixationCsv:
  private final case class Parsed[K, U <: Unit2D](
      row: Int,
      raw: Vector[String],
      key: K,
      ordinal: Int,
      fixation: Event.Fixation[U],
      outside: Option[OutsideFrame],
      conflict: Option[(Int, Int)]
  )

  /** The version-1 admission: an out-of-frame position rejects its record
    * and quarantines its trial, and no correction applies. Equivalent to
    * [[admit]] under `AdmissionPolicy.version1`. This is the route of saved
    * version-1 ledgers and eyesim parity; new imports use [[admit]].
    */
  def read[K, U <: Unit2D](
      contents: String,
      columns: FixationColumns,
      keys: FixationKeyReader[K],
      frame: Frame[U],
      timeUnit: TimestampUnit,
      rounding: TimestampRounding = TimestampRounding.NearestMicrosecond
  )(using UnitLabel[U]): Either[FixationImportError, FixationImport[K, U]] =
    admit(contents, columns, keys, frame, timeUnit, AdmissionPolicy.version1[K], rounding)

  /** Admit a fixation table under an explicit admission policy. The default
    * policy admits a record whose finite position lies outside `frame` and
    * lists it as outside the frame; a study leaves it out of every map and
    * reports it. Each trial's correction, when a rule covers it, is applied
    * to the parsed position before containment is checked; raw fields are
    * kept. A trial two rules cover is quarantined with
    * `QuarantineCause.CorrectionConflict`.
    */
  def admit[K, U <: Unit2D](
      contents: String,
      columns: FixationColumns,
      keys: FixationKeyReader[K],
      frame: Frame[U],
      timeUnit: TimestampUnit,
      policy: AdmissionPolicy[K] = AdmissionPolicy.default[K],
      rounding: TimestampRounding = TimestampRounding.NearestMicrosecond
  )(using UnitLabel[U]): Either[FixationImportError, FixationImport[K, U]] =
    given KeyDigest[K]   = keys.digest
    given Ordering[K]    = keys.ordering
    val participantRules = policy.corrections.zipWithIndex.collect {
      case (AppliedCorrection(CorrectionScope.Participant(_), _), index) => index
    }
    keys.participant match
      case None if participantRules.nonEmpty =>
        Left(FixationImportError.ParticipantScope(participantRules))
      case participant =>
        decodeRows(contents, columns, keys, frame, timeUnit, policy, participant, rounding)

  private def decodeRows[K, U <: Unit2D](
      contents: String,
      columns: FixationColumns,
      keys: FixationKeyReader[K],
      frame: Frame[U],
      timeUnit: TimestampUnit,
      policy: AdmissionPolicy[K],
      participant: Option[K => String],
      rounding: TimestampRounding
  )(using
      UnitLabel[U],
      KeyDigest[K],
      Ordering[K]
  ): Either[FixationImportError, FixationImport[K, U]] =
    // Without a participant projection no participant-scoped rule exists, so
    // the projection is never consulted.
    val owner: K => String = participant.getOrElse(_ => "")
    Rfc4180.decode(contents).left.map(FixationImportError.Csv.apply).flatMap { rows =>
      val header   = rows.headOption.getOrElse(Vector.empty)
      val required = (columns.names ++ keys.columns).distinct
      if header.isEmpty || header.distinct.size != header.size || !required.forall(
          header.contains
        )
      then Left(FixationImportError.Header(header, required))
      else
        val parsed = rows.drop(1).zipWithIndex.map { case (raw, index) =>
          val number = index + 2
          val fields = header.zip(raw).toMap
          val key    = keys.read(fields).left.map(FixationRowError.Key.apply)
          val result = for
            k <- key
            _ <- Either.cond(
              raw.size == header.size,
              (),
              FixationRowError.Width(header.size, raw.size)
            )
            ordinal <- integer(fields, columns.ordinal, positive = false)
            count   <- integer(fields, columns.sampleCount, positive = true)
            x       <- finite(fields, columns.x)
            y       <- finite(fields, columns.y)
            rule = policy.correctionFor(k, owner)
            centre <- rule match
              case Right(Some((_, correction))) =>
                correction
                  .correct(frame, Pt[U](x, y))
                  .toRight(FixationRowError.Position(x, y, frame.id))
              case _ => Right(Pt[U](x, y))
            _ <- Either.cond(
              rule.isLeft || frame.contains(centre) ||
                policy.offScreen == OffScreenPolicy.ExcludeRecord,
              (),
              FixationRowError.Position(centre.x, centre.y, frame.id)
            )
            onset    <- micros(fields(columns.onset), columns.onset, timeUnit, rounding)
            duration <- micros(fields(columns.duration), columns.duration, timeUnit, rounding)
            end = BigInt(onset) + BigInt(duration)
            _ <- Either.cond(
              duration > 0 && end.isValidLong,
              (),
              FixationRowError.Time(
                fields(columns.onset),
                fields(columns.duration),
                timeUnit,
                "duration must be positive and the interval must fit signed microseconds"
              )
            )
            span <- Interval
              .of(keys.clock(k), Instant.micros(onset), Instant.micros(end.toLong))
              .left
              .map(e =>
                FixationRowError
                  .Time(fields(columns.onset), fields(columns.duration), timeUnit, e.message)
              )
            fixation <- Event.Fixation
              .withoutDispersion(span, centre, count)
              .left
              .map(e => FixationRowError.Event(e.message))
          yield Parsed(
            number,
            raw,
            k,
            ordinal,
            fixation,
            Option.when(!frame.contains(centre))(
              OutsideFrame(number, centre.x, centre.y, frame.id)
            ),
            rule.left.toOption
          )
          result.left.map(error => RejectedFixationRow(number, raw, key.toOption, error))
        }
        val invalid = parsed.collect { case Left(error) => error }
        val valid   = parsed.collect { case Right(value) => value }
        // Keys that name one trial but disagree on its item or occurrence:
        // each maps to the trial's first key, all its records and the cause,
        // so the trial is quarantined once.
        val itemConflicts: Map[K, (K, Vector[Int], QuarantineCause)] =
          keys.attribute.fold(Map.empty) { rule =>
            val keyed = valid.map(v => v.key -> v.row) ++
              invalid.flatMap(r => r.key.map(_ -> r.rowNumber))
            keyed
              .map(_._1)
              .distinct
              .groupBy(rule.identity)
              .values
              .collect {
                case group if group.size > 1 =>
                  val items       = group.map(rule.item).distinct.sorted
                  val occurrences = group.map(rule.occurrence).distinct.sorted
                  val cause       =
                    if items.size > 1 then QuarantineCause.ItemConflict(items)
                    else QuarantineCause.OccurrenceConflict(occurrences)
                  val rows = keyed.collect { case (k, r) if group.contains(k) => r }.sorted
                  group.map(_ -> (group.min, rows, cause))
              }
              .flatten
              .toMap
          }
        val groups =
          valid.groupBy(_.key).toVector.sortBy(_._1).map { case (key, observations) =>
            val ordered  = observations.sortBy(_.ordinal)
            val affected = invalid.filter(_.key.contains(key)).map(_.rowNumber)
            val allRows  = (ordered.map(_.row) ++ affected).sorted
            val conflict = ordered.flatMap(_.conflict).headOption
            val path     = (affected.nonEmpty, conflict) match
              case _ if itemConflicts.contains(key) =>
                val (_, rows, cause) = itemConflicts(key)
                Left(FixationRowError.Trial(rows, cause))
              case (true, _) =>
                Left(FixationRowError.Trial(allRows, QuarantineCause.RejectedRecords))
              case (false, Some((first, second))) =>
                Left(
                  FixationRowError.Trial(
                    allRows,
                    QuarantineCause.CorrectionConflict(first, second)
                  )
                )
              case (false, None) if ordered.map(_.ordinal).distinct.size != ordered.size =>
                Left(FixationRowError.Trial(allRows, QuarantineCause.DuplicateOrdinals))
              case (false, None) =>
                Scanpath
                  .of(frame, keys.clock(key), IArray.from(ordered.map(_.fixation)))
                  .left
                  .map(e => FixationRowError.Trial(allRows, QuarantineCause.of(e)))
            path
              .map(value =>
                Trial(key, (), value) ->
                  ordered
                    .map(row => AdmittedFixationRow(row.row, key, row.ordinal) -> row.outside)
              )
              .left
              .map { error =>
                val named = itemConflicts.get(key).fold(key)(_._1)
                ordered.map(row => RejectedFixationRow(row.row, row.raw, Some(named), error))
              }
          }
        val renamed = invalid
          .map(r => r.key.flatMap(itemConflicts.get).fold(r)(c => r.copy(key = Some(c._1))))
        val rejected = (renamed ++ groups.collect { case Left(errors) => errors }.flatten)
          .sortBy(_.rowNumber)
        val links =
          groups.collect { case Right((_, links)) => links }.flatten.sortBy(_._1.rowNumber)
        Right(
          new FixationImport(
            header,
            rows.drop(1),
            Trials(groups.collect { case Right((trial, _)) => trial }),
            links.map(_._1),
            rejected,
            policy,
            links.flatMap(_._2)
          )
        )
    }

  private def integer(
      fields: Map[String, String],
      column: String,
      positive: Boolean
  ): Either[FixationRowError, Int] =
    val raw = fields(column)
    raw.toIntOption
      .filter(n => if positive then n > 0 else n >= 0)
      .toRight(
        FixationRowError.Number(
          column,
          raw,
          if positive then "a positive integer" else "a non-negative integer"
        )
      )

  private def finite(
      fields: Map[String, String],
      column: String
  ): Either[FixationRowError, Double] =
    val raw = fields(column)
    raw.toDoubleOption
      .filter(_.isFinite)
      .toRight(FixationRowError.Number(column, raw, "a finite number"))

  private def micros(
      raw: String,
      column: String,
      unit: TimestampUnit,
      rounding: TimestampRounding
  ): Either[FixationRowError, Long] =
    scala.util
      .Try(BigDecimal(raw))
      .toEither
      .left
      .map(_ => FixationRowError.Number(column, raw, "a finite decimal timestamp"))
      .flatMap { value =>
        val scale = unit match
          case TimestampUnit.Microseconds => BigDecimal(1)
          case TimestampUnit.Milliseconds => BigDecimal(1000)
          case TimestampUnit.Seconds      => BigDecimal(1000000)
        val magnitudeError =
          FixationRowError.Number(column, raw, "a timestamp within signed 64-bit microseconds")
        if value.abs < BigDecimal("0.5") / scale then Right(0L)
        else if value > (BigDecimal(Long.MaxValue) + 1) / scale || value < (BigDecimal(
            Long.MinValue
          ) - 1) / scale
        then Left(magnitudeError)
        else
          scala.util
            .Try {
              rounding match
                case TimestampRounding.NearestMicrosecond =>
                  (value * scale + BigDecimal("0.5")).setScale(0, BigDecimal.RoundingMode.FLOOR)
            }
            .toEither
            .left
            .map(_ => magnitudeError)
            .flatMap { rounded =>
              if rounded.isValidLong then Right(rounded.toLongExact) else Left(magnitudeError)
            }
      }
