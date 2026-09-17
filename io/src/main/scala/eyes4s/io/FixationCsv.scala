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
final class FixationKeyReader[K] private (
    val columns: Vector[String],
    val read: Map[String, String] => Either[String, K],
    val clock: K => ClockId
)(using val digest: KeyDigest[K], val ordering: Ordering[K])
object FixationKeyReader:
  def of[K: KeyDigest: Ordering](columns: Vector[String])(
      read: Map[String, String] => Either[String, K],
      clock: K => ClockId
  ): Either[FixationImportError, FixationKeyReader[K]] =
    if columns.isEmpty || columns.exists(
        _.trim.isEmpty
      ) || columns.distinct.size != columns.size
    then Left(FixationImportError.Columns(columns))
    else Right(new FixationKeyReader(columns, read, clock))

  def study(
      participant: String,
      stimulus: String,
      phase: String
  ): Either[FixationImportError, FixationKeyReader[StudyKey]] =
    of[StudyKey](Vector(participant, stimulus, phase))(
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
      key => ClockId(s"fixation-trial:${KeyDigest[StudyKey].digest(key).render}")
    )

/** File-level errors leave the source untouched. Row-level defects live in the report. */
enum FixationImportError derives CanEqual:
  case Csv(underlying: TidyCsvError)
  case Columns(names: Vector[String])
  case Header(found: Vector[String], required: Vector[String])
  case Incomplete(rejectedRows: Vector[Int])
  def message: String = this match
    case Csv(error)     => error.message
    case Columns(names) => s"Fixation columns must be distinct non-empty names: $names."
    case Header(found, required) =>
      s"Fixation header $found must have unique columns and contain $required."
    case Incomplete(rows) =>
      s"Fixation study has rejected source rows $rows; inspect the import report before analysis."

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
    val rejected: Vector[RejectedFixationRow[K]]
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
      fixation: Event.Fixation[U]
  )

  def read[K, U <: Unit2D](
      contents: String,
      columns: FixationColumns,
      keys: FixationKeyReader[K],
      frame: Frame[U],
      timeUnit: TimestampUnit,
      rounding: TimestampRounding = TimestampRounding.NearestMicrosecond
  )(using UnitLabel[U]): Either[FixationImportError, FixationImport[K, U]] =
    given KeyDigest[K] = keys.digest
    given Ordering[K]  = keys.ordering
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
            _       <- Either.cond(
              frame.contains(Pt[U](x, y)),
              (),
              FixationRowError.Position(x, y, frame.id)
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
              .withoutDispersion(span, Pt[U](x, y), count)
              .left
              .map(e => FixationRowError.Event(e.message))
          yield Parsed(number, raw, k, ordinal, fixation)
          result.left.map(error => RejectedFixationRow(number, raw, key.toOption, error))
        }
        val invalid = parsed.collect { case Left(error) => error }
        val valid   = parsed.collect { case Right(value) => value }
        val groups  =
          valid.groupBy(_.key).toVector.sortBy(_._1).map { case (key, observations) =>
            val ordered  = observations.sortBy(_.ordinal)
            val affected = invalid.filter(_.key.contains(key)).map(_.rowNumber)
            val allRows  = (ordered.map(_.row) ++ affected).sorted
            val path     =
              if affected.nonEmpty then
                Left(FixationRowError.Trial(allRows, QuarantineCause.RejectedRecords))
              else if ordered.map(_.ordinal).distinct.size != ordered.size then
                Left(FixationRowError.Trial(allRows, QuarantineCause.DuplicateOrdinals))
              else
                Scanpath
                  .of(frame, keys.clock(key), IArray.from(ordered.map(_.fixation)))
                  .left
                  .map(e => FixationRowError.Trial(allRows, QuarantineCause.of(e)))
            path
              .map(value =>
                Trial(key, (), value) ->
                  ordered.map(row => AdmittedFixationRow(row.row, key, row.ordinal))
              )
              .left
              .map { error =>
                ordered.map(row => RejectedFixationRow(row.row, row.raw, Some(key), error))
              }
          }
        val rejected = (invalid ++ groups.collect { case Left(errors) => errors }.flatten)
          .sortBy(_.rowNumber)
        Right(
          new FixationImport(
            header,
            rows.drop(1),
            Trials(groups.collect { case Right((trial, _)) => trial }),
            groups.collect { case Right((_, links)) => links }.flatten.sortBy(_.rowNumber),
            rejected
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
