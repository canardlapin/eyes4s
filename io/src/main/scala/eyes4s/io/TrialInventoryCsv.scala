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

import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.*

/** How a fixation record's sample support is declared. A fixation is
  * supported by at least one sample, and the rule says where that support
  * comes from.
  */
enum SampleCountRule derives CanEqual:
  /** Counts are read from `column`. A count that is not a positive integer,
    * 0 included, rejects its record with `FixationRowError.Number`; a trial
    * every record of which is rejected so has no fixations.
    */
  case PositiveColumn(column: String)

  /** The table has no count column: a record's count is its duration times
    * `rate`, rounded up, so every record of positive duration has at least
    * one sample.
    */
  case FromDuration(rate: Hz)

  def countColumn: Option[String] = this match
    case PositiveColumn(name) => Some(name)
    case FromDuration(_)      => None

  /** The count a record of `durationMicros` has under [[FromDuration]]. */
  private[io] def derived(durationMicros: Long): Int = this match
    case PositiveColumn(_)  => 1
    case FromDuration(rate) =>
      val samples = (BigDecimal(durationMicros) * BigDecimal(rate.value) / BigDecimal(1000000))
        .setScale(0, BigDecimal.RoundingMode.CEILING)
      if samples > BigDecimal(Int.MaxValue) then Int.MaxValue else math.max(1, samples.toInt)

/** The onset and duration columns and the unit both are written in. The unit
  * is a declaration; it is never inferred from the values.
  */
final case class TimeColumns(onset: String, duration: String, unit: TimestampUnit)
    derives CanEqual

/** The columns that identify a trial: participant, phase, trial label and,
  * optionally, occurrence (1 when no column is named).
  */
final case class TrialColumns private (
    participant: String,
    phase: String,
    trial: String,
    occurrence: Option[String]
) derives CanEqual:
  def names: Vector[String] = Vector(participant, phase, trial) ++ occurrence.toVector

  /** The identity a record declares, or the column, value and requirement it fails. */
  private[io] def identity(
      fields: Map[String, String]
  ): Either[(String, String, String), TrialIdentity] =
    def text(column: String) =
      val value = fields.getOrElse(column, "")
      Either.cond(value.trim.nonEmpty, value, (column, value, "a non-blank value"))
    for
      p <- text(participant)
      f <- text(phase)
      t <- text(trial)
      n <- occurrence.fold[Either[(String, String, String), TrialOccurrence]](
        Right(TrialOccurrence.first)
      )(column =>
        val raw = fields.getOrElse(column, "")
        raw.toIntOption
          .flatMap(TrialOccurrence.of(_).toOption)
          .toRight((column, raw, "a positive integer occurrence"))
      )
      id <- TrialIdentity.of(p, f, t, n).left.map(e => (participant, p, e.message))
    yield id

object TrialColumns:
  def of(
      participant: String,
      phase: String,
      trial: String,
      occurrence: Option[String] = None
  ): Either[FixationImportError, TrialColumns] =
    distinct(Vector(participant, phase, trial) ++ occurrence.toVector)
      .map(_ => new TrialColumns(participant, phase, trial, occurrence))

  private[io] def distinct(names: Vector[String]): Either[FixationImportError, Unit] =
    Either.cond(
      !names.exists(_.trim.isEmpty) && names.distinct.size == names.size,
      (),
      FixationImportError.Columns(names)
    )

/** The declared columns of a fixation table read against a trial inventory:
  * the trial identity, an optional item column, ordinal, position, time
  * columns with their declared unit, the sample-count rule and the record
  * attributes. No other column is read.
  */
final class FixationTable private (
    val trial: TrialColumns,
    val item: Option[String],
    val ordinal: String,
    val x: String,
    val y: String,
    val time: TimeColumns,
    val samples: SampleCountRule,
    val attributes: Vector[AttributeColumn]
):
  def names: Vector[String] =
    trial.names ++ item.toVector ++ spec.names

  private[io] def spec: RowSpec =
    RowSpec(ordinal, x, y, time.onset, time.duration, samples, time.unit, attributes)

object FixationTable:
  def of(
      trial: TrialColumns,
      ordinal: String,
      x: String,
      y: String,
      time: TimeColumns,
      samples: SampleCountRule,
      item: Option[String] = None,
      attributes: Vector[AttributeColumn] = Vector.empty
  ): Either[FixationImportError, FixationTable] =
    val table = new FixationTable(trial, item, ordinal, x, y, time, samples, attributes)
    TrialColumns.distinct(table.names).map(_ => table)

/** The declared columns of a trials table: the trial identity, an optional
  * item column and the attributes. No other column is read.
  */
final class TrialInventoryColumns private (
    val trial: TrialColumns,
    val item: Option[String],
    val attributes: Vector[AttributeColumn]
):
  def names: Vector[String] = trial.names ++ item.toVector ++ attributes.map(_.name)

object TrialInventoryColumns:
  def of(
      trial: TrialColumns,
      item: Option[String] = None,
      attributes: Vector[AttributeColumn] = Vector.empty
  ): Either[FixationImportError, TrialInventoryColumns] =
    val columns = new TrialInventoryColumns(trial, item, attributes)
    TrialColumns.distinct(columns.names).map(_ => columns)

/** One declared trial: its identity, the inventory records that declare it
  * (identical repeats collapse into one trial), its item and attributes.
  */
final case class InventoryRow(
    identity: TrialIdentity,
    records: Vector[Int],
    item: Option[String],
    attributes: Attributes
) derives CanEqual

/** A trials table read under declared columns: every declared trial once, in
  * first-record order. Record numbers count the header as record 1.
  */
final class TrialInventory private (
    val header: Vector[String],
    val rows: Vector[Vector[String]],
    val columns: TrialInventoryColumns,
    val trials: Vector[InventoryRow]
):
  private lazy val byIdentity = trials.map(t => t.identity -> t).toMap

  def trial(identity: TrialIdentity): Option[InventoryRow] = byIdentity.get(identity)

  /** Nominal reference to the decoded inventory records. */
  def source(label: String): SourceRef = SourceRef.of(label, header, rows)

object TrialInventory:
  /** Read a trials table. The inventory is a declaration, so any defective
    * record refuses it: a record of the wrong width, a blank identity field
    * or item, an occurrence that is not a positive integer, an attribute that
    * is not of its declared kind, or two records that declare one trial
    * (participant, phase and trial label) with different values.
    */
  def read(
      contents: String,
      columns: TrialInventoryColumns
  ): Either[FixationImportError, TrialInventory] =
    FixationCsv.table(contents, columns.names).flatMap { (header, rows) =>
      val parsed = rows.zipWithIndex.map { (raw, index) =>
        val number                                                    = index + 2
        val fields                                                    = header.zip(raw).toMap
        def field(column: String, value: String, requirement: String) =
          InventoryError.Field(number, column, value, requirement)
        for
          _ <- Either.cond(
            raw.size == header.size,
            (),
            InventoryError.Width(number, header.size, raw.size)
          )
          identity <- columns.trial.identity(fields).left.map(field.tupled)
          item     <- columns.item.fold[Either[InventoryError, Option[String]]](Right(None)) {
            column =>
              val value = fields(column)
              Either.cond(
                value.trim.nonEmpty,
                Some(value),
                field(column, value, "a non-blank item")
              )
          }
          attributes <- columns.attributes
            .foldLeft[Either[InventoryError, Vector[(String, AttributeValue)]]](
              Right(Vector.empty)
            ) { (acc, column) =>
              acc.flatMap(done =>
                column
                  .parse(fields(column.name))
                  .map(value => done :+ (column.name -> value))
                  .left
                  .map(field(column.name, fields(column.name), _))
              )
            }
            .flatMap(Attributes.of)
        yield (number, raw, identity, item, attributes)
      }
      parsed.collectFirst { case Left(error) => error } match
        case Some(error) => Left(FixationImportError.Inventory(error))
        case None        =>
          val valid    = parsed.collect { case Right(row) => row }
          val declared = columns.names.map(header.indexOf)
          val labels   = valid.groupBy { case (_, _, id, _, _) =>
            (id.participant, id.phase, id.trial)
          }
          val conflict = valid.iterator
            .map { case (_, _, id, _, _) => labels((id.participant, id.phase, id.trial)) }
            .collectFirst {
              case group if group.map(r => declared.map(r._2)).distinct.size > 1 =>
                val (_, _, id, _, _) = group.head
                val differing        = columns.names.zip(declared).collect {
                  case (name, i) if group.map(_._2(i)).distinct.size > 1 => name
                }
                InventoryError.Conflict(
                  id.participant,
                  id.phase,
                  id.trial,
                  group.map(_._1),
                  differing
                )
            }
          conflict match
            case Some(error) => Left(FixationImportError.Inventory(error))
            case None        =>
              val trials = valid
                .groupBy(_._3)
                .values
                .map(group =>
                  val (_, _, id, item, attributes) = group.head
                  InventoryRow(id, group.map(_._1), item, attributes)
                )
                .toVector
                .sortBy(_.records.head)
              Right(new TrialInventory(header, rows, columns, trials))
    }

/** A fixation table admitted against a trial inventory: the fixation import
  * (keyed by `TrialKey`, the item resolved from inventory and records), every
  * inventory trial with exactly one disposition, the trials only the fixation
  * table names, and the declared attributes of admitted records.
  */
final class InventoryImport[U <: Unit2D] private[io] (
    val fixations: FixationImport[TrialKey, U],
    val inventory: TrialInventory,
    val trials: Vector[InventoryTrial],
    val unlisted: Vector[UnlistedTrial],
    val recordAttributes: Vector[RecordAttributes]
):
  /** The inventory attributes of a trial key's trial. */
  def attributes(key: TrialKey): Option[Attributes] =
    trials.find(_.identity == TrialIdentity.of(key)).map(_.attributes)

  def requireComplete: Either[FixationImportError, StudyInput[TrialKey, U]] =
    fixations.requireComplete

private[io] object InventoryAdmission:
  /** Admit a fixation table against a trial inventory. Records are joined to
    * inventory trials by identity (participant, phase, trial label,
    * occurrence). Each inventory trial gets exactly one disposition:
    *
    *   - `Absent` when no record names it;
    *   - `NoFixations` when every one of its records is rejected on its own,
    *     which takes precedence over every quarantine cause;
    *   - `Quarantined(cause)` when its records conflict on the item (among
    *     themselves, `ItemConflict`, or with the inventory,
    *     `InventoryItemConflict`), or on the importer's usual grounds;
    *   - `Admitted` otherwise.
    *
    * Records of a trial the inventory does not declare are quarantined with
    * `QuarantineCause.NotInInventory` and listed as unlisted trials; they are
    * never dropped. The item of a trial is the inventory's when it declares
    * one, otherwise the one item its records name.
    */
  def admit[U <: Unit2D](
      contents: String,
      table: FixationTable,
      inventory: TrialInventory,
      frame: Frame[U],
      policy: AdmissionPolicy[TrialKey],
      rounding: TimestampRounding
  )(using UnitLabel[U]): Either[FixationImportError, InventoryImport[U]] =
    if table.item.isEmpty && inventory.columns.item.isEmpty then
      Left(FixationImportError.NoItemColumn(inventory.columns.names, table.names))
    else
      FixationCsv.table(contents, table.names).map { (header, rows) =>
        join(header, rows, table, inventory, frame, policy, rounding)
      }

  private def join[U <: Unit2D](
      header: Vector[String],
      rows: Vector[Vector[String]],
      table: FixationTable,
      inventory: TrialInventory,
      frame: Frame[U],
      policy: AdmissionPolicy[TrialKey],
      rounding: TimestampRounding
  )(using UnitLabel[U]): InventoryImport[U] =
    // A record's identity and, when the table declares one, its item.
    def read(fields: Map[String, String]): Either[String, (TrialIdentity, Option[String])] =
      for
        id <- table.trial
          .identity(fields)
          .left
          .map((c, v, r) => s"Column '$c' has '$v'; expected $r.")
        item <- table.item.fold[Either[String, Option[String]]](Right(None)) { column =>
          val value = fields.getOrElse(column, "")
          Either.cond(value.trim.nonEmpty, Some(value), s"Missing column '$column'.")
        }
      yield id -> item
    val named = rows.map(raw => read(header.zip(raw).toMap).toOption)
    val items: Map[TrialIdentity, Vector[String]] =
      named.flatten.groupMap(_._1)(_._2).view.mapValues(_.flatten.distinct.sorted).toMap
    def recordItems(id: TrialIdentity) = items.getOrElse(id, Vector.empty)
    def conflict(id: TrialIdentity, declared: Option[String]): Option[QuarantineCause] =
      val named = recordItems(id)
      if named.size > 1 then Some(QuarantineCause.ItemConflict(named))
      else
        declared.flatMap(item =>
          Option.when(named.exists(_ != item))(
            QuarantineCause.InventoryItemConflict(item, named)
          )
        )
    // The key each identity is admitted, or its records reported, under.
    val keys: Map[TrialIdentity, TrialKey] = items.keys.iterator
      .map { id =>
        val declared = inventory.trial(id).flatMap(_.item)
        id -> declared.orElse(recordItems(id).headOption)
      }
      .collect { case (id, Some(item)) => id.withItem(item).toOption.map(id -> _) }
      .flatten
      .toMap
    val forced: Map[TrialIdentity, QuarantineCause] = items.keys.iterator
      .flatMap(id =>
        inventory.trial(id) match
          case None =>
            Some(
              id -> QuarantineCause
                .NotInInventory(id.participant, id.phase, id.trial, id.occurrence.value)
            )
          case Some(row) => conflict(id, row.item).map(id -> _)
      )
      .toMap
    def clock(id: TrialIdentity): ClockId =
      keys.get(id) match
        case Some(key) => ClockId(s"fixation-trial:${KeyDigest[TrialKey].digest(key).render}")
        case None => ClockId(s"fixation-trial:${KeyDigest[TrialIdentity].digest(id).render}")
    val scoped = AdmissionPolicy[TrialIdentity](
      policy.offScreen,
      policy.corrections.map(rule =>
        AppliedCorrection(
          rule.scope match
            case CorrectionScope.AllTrials()    => CorrectionScope.AllTrials[TrialIdentity]()
            case CorrectionScope.Participant(p) => CorrectionScope.Participant[TrialIdentity](p)
            case CorrectionScope.Trial(key)     => CorrectionScope.Trial(TrialIdentity.of(key)),
          rule.correction
        )
      )
    )
    val parsed = FixationCsv.parseRows[TrialIdentity, U](
      header,
      rows,
      table.spec,
      fields => read(fields).map(_._1),
      clock,
      frame,
      scoped,
      Some(_.participant),
      rounding
    )
    val invalid = parsed.collect { case Left(error) => error }
    val valid   = parsed.collect { case Right(value) => value }
    val recordsOf: Map[TrialIdentity, Vector[Int]] =
      (valid.map(v => v.key -> v.row) ++ invalid.flatMap(r => r.key.map(_ -> r.rowNumber)))
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.sorted)
        .toMap
    val overrides =
      forced.map((id, cause) => id -> (id, recordsOf.getOrElse(id, Vector.empty), cause))
    val (imported, attributes) =
      FixationCsv.assemble(header, rows, valid, invalid, frame, clock, overrides, scoped)
    val accepted = imported.accepted.rows.map(_.key).toSet
    val causes   = imported.rejected.collect {
      case RejectedFixationRow(_, _, Some(id), FixationRowError.Trial(_, cause)) => id -> cause
    }.toMap
    val withValid = valid.map(_.key).toSet
    val trials    = inventory.trials.map { row =>
      val records     = recordsOf.getOrElse(row.identity, Vector.empty)
      val disposition =
        if records.isEmpty then TrialDisposition.Absent
        else if !withValid.contains(row.identity) then TrialDisposition.NoFixations
        else if accepted.contains(row.identity) then TrialDisposition.Admitted
        else
          TrialDisposition.Quarantined(
            causes.getOrElse(row.identity, QuarantineCause.RejectedRecords)
          )
      InventoryTrial(
        row.identity,
        row.records,
        row.item,
        row.attributes,
        recordItems(row.identity),
        records,
        disposition
      )
    }
    val unlisted = recordsOf.toVector
      .collect {
        case (id, records) if inventory.trial(id).isEmpty =>
          UnlistedTrial(id, recordItems(id), records)
      }
      .sortBy(_.records.head)
    val fixations = new FixationImport[TrialKey, U](
      imported.header,
      imported.sourceRows,
      Trials(
        imported.accepted.rows.flatMap(t => keys.get(t.key).map(k => Trial(k, (), t.value)))
      ),
      imported.admitted.flatMap(r =>
        keys.get(r.key).map(k => AdmittedFixationRow(r.rowNumber, k, r.ordinal))
      ),
      imported.rejected.map(r =>
        RejectedFixationRow(r.rowNumber, r.raw, r.key.flatMap(keys.get), r.error)
      ),
      policy,
      imported.outsideFrame
    )
    new InventoryImport(
      fixations,
      inventory,
      trials,
      unlisted,
      if table.attributes.isEmpty then Vector.empty
      else attributes.map((record, values) => RecordAttributes(record, values))
    )
