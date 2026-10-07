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

import eyes4s.kernel.{Span, Window}
import eyes4s.studio.core.document.{InventoryDurationColumn, SourcePath}
import io.circe.{Codec, Decoder, DecodingFailure, Encoder}
import ProtocolCodecs.portableLong

/** Inventory evidence names data rows (the header is record 1), never fabricated timing. */
final case class TrialExtentEvidence private (
    trial: TrialKey,
    dataset: DatasetRevision,
    source: SourcePath,
    declaration: InventoryDurationColumn,
    records: Vector[Int]
) derives CanEqual

object TrialExtentEvidence:
  def of(
      trial: TrialKey,
      dataset: DatasetRevision,
      source: SourcePath,
      declaration: InventoryDurationColumn,
      records: Vector[Int]
  ): Either[TrialViewError, TrialExtentEvidence] =
    if records.isEmpty || records
        .exists(_ < 2) || records.zip(records.drop(1)).exists((a, b) => a >= b)
    then Left(TrialViewError.ExtentRecords(trial, source.value, records))
    else Right(new TrialExtentEvidence(trial, dataset, source, declaration, records))

  given Codec.AsObject[TrialExtentEvidence] = Codec.AsObject.from(
    Decoder
      .forProduct5("trial", "dataset", "source", "declaration", "records")(of)
      .emap(_.left.map(_.message)),
    Encoder.forProduct5("trial", "dataset", "source", "declaration", "records")(e =>
      (e.trial, e.dataset, e.source, e.declaration, e.records)
    )
  )

/** A native declared half-open extent anchored at trial start, with its inventory evidence. */
final case class TrialExtentFromStart private (
    window: Window,
    evidence: TrialExtentEvidence
) derives CanEqual:
  def trial: TrialKey                      = evidence.trial
  def dataset: DatasetRevision             = evidence.dataset
  def source: SourcePath                   = evidence.source
  def declaration: InventoryDurationColumn = evidence.declaration
  def records: Vector[Int]                 = evidence.records
  def durationMicros: Long                 = window.width.toMicros

object TrialExtentFromStart:
  def of(
      trial: TrialKey,
      dataset: DatasetRevision,
      window: Window,
      source: SourcePath,
      declaration: InventoryDurationColumn,
      records: Vector[Int]
  ): Either[TrialViewError, TrialExtentFromStart] =
    if window.from.toMicros != 0 || window.until.toMicros <= 0 then
      Left(TrialViewError.ExtentInvalid(trial, window.from.toMicros, window.until.toMicros))
    else
      TrialExtentEvidence
        .of(trial, dataset, source, declaration, records)
        .map(e => new TrialExtentFromStart(window, e))

  given Codec.AsObject[TrialExtentFromStart] = Codec.AsObject.from(
    Decoder.instance { c =>
      for
        trial       <- c.get[TrialKey]("trial")
        dataset     <- c.get[DatasetRevision]("dataset")
        from        <- c.get[Long]("fromMicros")
        until       <- c.get[Long]("untilMicros")
        source      <- c.get[SourcePath]("source")
        declaration <- c.get[InventoryDurationColumn]("declaration")
        records     <- c.get[Vector[Int]]("records")
        window      <- Window
          .of(Span.micros(from), Span.micros(until))
          .left
          .map(e => DecodingFailure(e.message, c.history))
        value <- of(trial, dataset, window, source, declaration, records).left.map(e =>
          DecodingFailure(e.message, c.history)
        )
      yield value
    },
    Encoder.forProduct7(
      "trial",
      "dataset",
      "fromMicros",
      "untilMicros",
      "source",
      "declaration",
      "records"
    )(e =>
      (
        e.trial,
        e.dataset,
        e.window.from.toMicros,
        e.window.until.toMicros,
        e.source,
        e.declaration,
        e.records
      )
    )
  )

/** Availability is explicit; a fixation's final offset never declares a trial end. */
enum TrialTemporalExtent derives CanEqual, Codec.AsObject:
  case FromTrialStart(value: TrialExtentFromStart)
  case Undeclared(owner: TrialKey, dataset: DatasetRevision)
  case Blank(evidence: TrialExtentEvidence)
  case LegacyMissing(owner: TrialKey)

  def trial: TrialKey = this match
    case FromTrialStart(value) => value.trial
    case Undeclared(t, _)      => t
    case Blank(evidence)       => evidence.trial
    case LegacyMissing(t)      => t

  def declaredDataset: Option[DatasetRevision] = this match
    case FromTrialStart(value)  => Some(value.dataset)
    case Undeclared(_, dataset) => Some(dataset)
    case Blank(evidence)        => Some(evidence.dataset)
    case LegacyMissing(_)       => None

  def declared: Option[TrialExtentFromStart] = this match
    case FromTrialStart(value) => Some(value)
    case _                     => None

  def missingMessage: Option[String] = this match
    case FromTrialStart(_) => None
    case Undeclared(t, d)  =>
      Some(s"Trial extent of ${t.label} is not declared in ${d.label}'s inventory.")
    case Blank(e) =>
      Some(
        s"Trial extent of ${e.trial.label} is blank in ${e.source.value} column ${e.declaration.column.value}, records ${e.records.mkString(", ")}."
      )
    case LegacyMissing(t) =>
      Some(s"Trial extent of ${t.label} was not supplied by the legacy payload.")
