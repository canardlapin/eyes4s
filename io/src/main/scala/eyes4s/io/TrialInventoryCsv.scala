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

import cats.data.NonEmptyVector
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.*

/** The onset and duration columns and the unit both are written in. The unit
  * is a declaration; it is never inferred from the values.
  */
final case class TimeColumns(onset: String, duration: String, unit: TimestampUnit)
    derives CanEqual

/** The columns that identify a trial: participant, phase, trial label and,
  * optionally, occurrence (1 when no column is named). The label (participant,
  * phase and trial) identifies a trial; the occurrence is checked against it.
  */
final case class TrialColumns private (
    participant: String,
    phase: String,
    trial: String,
    occurrence: Option[String]
) derives CanEqual:
  def names: Vector[String] = Vector(participant, phase, trial) ++ occurrence.toVector

  /** The identity a record declares, or every column, value and requirement
    * it fails.
    */
  private[io] def identity(
      fields: Map[String, String]
  ): Either[Vector[(String, String, String)], TrialIdentity] =
    def text(column: String) =
      val value = fields.getOrElse(column, "")
      Either.cond(value.trim.nonEmpty, value, (column, value, "a non-blank value"))
    val p = text(participant)
    val f = text(phase)
    val t = text(trial)
    val n = occurrence.fold[Either[(String, String, String), TrialOccurrence]](
      Right(TrialOccurrence.first)
    )(column =>
      val raw = fields.getOrElse(column, "")
      Option
        .when(raw.matches("[0-9]+"))(raw)
        .flatMap(_.toIntOption)
        .flatMap(TrialOccurrence.of(_).toOption)
        .toRight((column, raw, "a positive integer occurrence"))
    )
    (p, f, t, n) match
      case (Right(pv), Right(fv), Right(tv), Right(nv)) =>
        TrialIdentity.of(pv, fv, tv, nv).left.map(e => Vector((participant, pv, e.message)))
      case _ => Left(Vector(p, f, t, n).collect { case Left(e) => e })

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
    trial.names ++ item.toVector ++ spec(false).names

  private[io] def spec(itemRequired: Boolean): RowSpec =
    RowSpec(
      ordinal,
      x,
      y,
      time.onset,
      time.duration,
      samples,
      time.unit,
      attributes,
      if itemRequired then item.toVector else Vector.empty
    )

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
  * (repeats with equal parsed values collapse into one trial), its item and
  * attributes.
  */
final case class InventoryRow(
    identity: TrialIdentity,
    records: Vector[Int],
    item: Option[String],
    attributes: Attributes
) derives CanEqual

/** A trials table read under declared columns: every declared trial once, in
  * first-record order. Record numbers count the header as record 1. A trial is
  * identified by its label (participant, phase and trial); the occurrence
  * column, when declared, is an attribute every record of the trial must
  * agree with, not part of the join.
  */
final class TrialInventory private (
    val header: Vector[String],
    val rows: Vector[Vector[String]],
    val columns: TrialInventoryColumns,
    val trials: Vector[InventoryRow]
):
  private lazy val byLabel = trials.map(t => TrialInventory.label(t.identity) -> t).toMap

  /** The declared trial with this identity, occurrence included. */
  def trial(identity: TrialIdentity): Option[InventoryRow] =
    labelled(identity).filter(_.identity == identity)

  /** The declared trial with this identity's label, whatever its occurrence. */
  private[io] def labelled(identity: TrialIdentity): Option[InventoryRow] =
    byLabel.get(TrialInventory.label(identity))

  /** Nominal reference to the decoded inventory records. */
  def source(label: String): SourceRef = SourceRef.of(label, header, rows)

object TrialInventory:
  private[io] def label(id: TrialIdentity): (String, String, String) =
    (id.participant, id.phase, id.trial)

  private final case class Row(
      number: Int,
      identity: TrialIdentity,
      item: Option[String],
      attributes: Attributes
  )

  /** Read a trials table. The inventory is a declaration, so any defective
    * record refuses it, and the refusal lists every defect, each naming its
    * record and column: a record of the wrong width, a blank identity field
    * or item, an occurrence that is not a positive integer, an attribute that
    * is not of its declared kind, and every set of records that declare one
    * label with different parsed values (`Conflict`, naming the records and
    * the columns that differ).
    */
  def read(
      contents: String,
      columns: TrialInventoryColumns
  ): Either[FixationImportError, TrialInventory] =
    FixationCsv.table(contents, columns.names).flatMap { (header, rows) =>
      val parsed = rows.zipWithIndex.map { (raw, index) =>
        val number = index + 2
        val fields = header.zip(raw).toMap
        if raw.size != header.size then
          Left(Vector(InventoryError.Width(number, header.size, raw.size)))
        else
          def field(column: String, value: String, requirement: String) =
            InventoryError.Field(number, column, value, requirement)
          val identity = columns.trial.identity(fields).left.map(_.map(field.tupled))
          val item     = columns.item.fold[Either[Vector[InventoryError], Option[String]]](
            Right(None)
          ) { column =>
            val value = fields(column)
            Either.cond(
              value.trim.nonEmpty,
              Some(value),
              Vector(field(column, value, "a non-blank item"))
            )
          }
          val values = columns.attributes.map(column =>
            column
              .parse(fields(column.name))
              .left
              .map(field(column.name, fields(column.name), _))
          )
          val errors = identity.left.toSeq.flatten ++ item.left.toSeq.flatten ++
            values.collect { case Left(e) => e }
          if errors.nonEmpty then Left(errors.toVector)
          else
            for
              id         <- identity
              declared   <- item
              attributes <- Attributes
                .of(columns.attributes.map(_.name).zip(values.collect { case Right(v) => v }))
                .left
                .map(Vector(_))
            yield Row(number, id, declared, attributes)
      }
      val rowErrors = parsed.collect { case Left(errors) => errors }.flatten
      val valid     = parsed.collect { case Right(row) => row }
      val groups = valid.groupBy(r => label(r.identity)).values.toVector.sortBy(_.head.number)
      val conflicts = groups.flatMap { group =>
        val first     = group.head
        val differing =
          columns.trial.occurrence.filter(_ =>
            group.map(_.identity.occurrence.value).distinct.size > 1
          ) ++ columns.item.filter(_ => group.map(_.item).distinct.size > 1) ++
            columns.attributes
              .map(_.name)
              .filter(name => group.map(_.attributes.get(name)).distinct.size > 1)
        Option.when(differing.nonEmpty)(
          InventoryError.Conflict(
            first.identity.participant,
            first.identity.phase,
            first.identity.trial,
            group.map(_.number),
            differing.toVector
          )
        )
      }
      NonEmptyVector.fromVector(rowErrors ++ conflicts) match
        case Some(errors) => Left(FixationImportError.Inventory(errors))
        case None         =>
          val trials = groups.map(group =>
            val first = group.head
            InventoryRow(first.identity, group.map(_.number), first.item, first.attributes)
          )
          Right(new TrialInventory(header, rows, columns, trials))
    }

/** A fixation table admitted against a trial inventory: the fixation import
  * (keyed by `TrialKey`, the item resolved from inventory and records), every
  * inventory trial with exactly one disposition, the trials only the fixation
  * table names, the declared attributes of admitted records, and the
  * declarations the ledger records with them.
  */
final class InventoryImport[U <: Unit2D] private[io] (
    val fixations: FixationImport[TrialKey, U],
    val inventory: TrialInventory,
    val trials: Vector[InventoryTrial],
    val unlisted: Vector[UnlistedTrial],
    val recordAttributeColumns: Vector[AttributeColumn],
    val recordAttributes: Vector[RecordAttributes],
    val sampleCounts: SampleCountRule
):
  /** The inventory attributes of a trial key's trial. */
  def attributes(key: TrialKey): Option[Attributes] =
    trials.find(_.identity == TrialIdentity.of(key)).map(_.attributes)

  def requireComplete: Either[FixationImportError, StudyInput[TrialKey, U]] =
    fixations.requireComplete

private[io] object InventoryAdmission:
  /** Admit a fixation table against a trial inventory. Records join inventory
    * trials by label (participant, phase and trial). Each inventory trial gets
    * exactly one disposition:
    *
    *   - `Absent` when no record names it;
    *   - `NoFixations` when every one of its records is rejected on its own,
    *     which takes precedence over every quarantine cause;
    *   - `Quarantined(cause)` when its records conflict on the item (among
    *     themselves, `ItemConflict`, or with the inventory,
    *     `InventoryItemConflict`), on the occurrence (`OccurrenceConflict`,
    *     naming the inventory's and the records' occurrences), or on the
    *     importer's usual grounds;
    *   - `Admitted` otherwise.
    *
    * Records of a label the inventory does not declare are quarantined with
    * `QuarantineCause.NotInInventory` and listed as unlisted trials; they are
    * never dropped. The item of a trial is the inventory's when it declares
    * one (a blank record item cell then takes it); otherwise the one item its
    * records name, and a blank record item cell rejects that record, which
    * stays with its trial.
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
      FixationCsv.table(contents, table.names).flatMap { (header, rows) =>
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
  )(using UnitLabel[U]): Either[FixationImportError, InventoryImport[U]] =
    def identityOf(fields: Map[String, String]): Either[String, TrialIdentity] =
      table.trial
        .identity(fields)
        .left
        .map(_.map((c, v, r) => s"Column '$c' has '$v'; expected $r.").mkString(" "))
    // A record's identity and the item its cell names, if the cell is not blank.
    val named = rows.map { raw =>
      val fields = header.zip(raw).toMap
      identityOf(fields).toOption.map(id =>
        id -> table.item.flatMap(fields.get).filter(_.trim.nonEmpty)
      )
    }
    // Records join the inventory trial of their label; others stand alone.
    def canonical(id: TrialIdentity): TrialIdentity =
      inventory.labelled(id).fold(id)(_.identity)
    val byTrial                        = named.flatten.groupMap(p => canonical(p._1))(identity)
    def recordItems(id: TrialIdentity) =
      byTrial.getOrElse(id, Vector.empty).flatMap(_._2).distinct.sorted
    def itemConflict(id: TrialIdentity, declared: Option[String]): Option[QuarantineCause] =
      val items = recordItems(id)
      if items.size > 1 then Some(QuarantineCause.ItemConflict(items))
      else
        declared.flatMap(item =>
          Option.when(items.exists(_ != item))(
            QuarantineCause.InventoryItemConflict(item, items)
          )
        )
    // The key each trial is admitted, or its records reported, under.
    val keys: Map[TrialIdentity, TrialKey] = byTrial.keys.iterator.flatMap { id =>
      inventory
        .labelled(id)
        .flatMap(_.item)
        .orElse(recordItems(id).headOption)
        .flatMap(item => id.withItem(item).toOption)
        .map(id -> _)
    }.toMap
    // Every raw identity a forced quarantine applies to, with its trial.
    val forced: Map[TrialIdentity, (TrialIdentity, QuarantineCause)] =
      named.flatten
        .map(_._1)
        .distinct
        .flatMap { id =>
          inventory.labelled(id) match
            case None =>
              Some(
                id -> (id -> QuarantineCause
                  .NotInInventory(id.participant, id.phase, id.trial, id.occurrence.value))
              )
            case Some(row) =>
              val occurrences =
                (row.identity.occurrence.value +: byTrial(row.identity).map(
                  _._1.occurrence.value
                )).distinct.sorted
              itemConflict(row.identity, row.item)
                .orElse(
                  Option.when(occurrences.size > 1)(
                    QuarantineCause.OccurrenceConflict(occurrences)
                  )
                )
                .map(cause => id -> (row.identity -> cause))
        }
        .toMap
    def clock(id: TrialIdentity): ClockId =
      keys.get(canonical(id)) match
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
      table.spec(itemRequired = inventory.columns.item.isEmpty),
      identityOf,
      clock,
      frame,
      scoped,
      Some(_.participant),
      rounding
    )
    val invalid = parsed.collect { case Left(error) => error }
    val valid   = parsed.collect { case Right(value) => value }
    val recordsOf: Map[TrialIdentity, Vector[Int]] =
      (valid.map(v => canonical(v.key) -> v.row) ++
        invalid.flatMap(r => r.key.map(k => canonical(k) -> r.rowNumber)))
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.sorted)
        .toMap
    val overrides = forced.map { case (id, (trial, cause)) =>
      id -> (trial, recordsOf.getOrElse(trial, Vector.empty), cause)
    }
    val (imported, attributes) =
      FixationCsv.assemble(header, rows, valid, invalid, frame, clock, overrides, scoped)
    val accepted = imported.accepted.rows.map(_.key).toSet
    val causes   = imported.rejected.collect {
      case RejectedFixationRow(_, _, Some(id), FixationRowError.Trial(_, cause)) =>
        canonical(id) -> cause
    }.toMap
    val withValid                   = valid.map(v => canonical(v.key)).toSet
    def rejected(e: InventoryError) = FixationImportError.Inventory(NonEmptyVector.one(e))
    val trials                      = inventory.trials.traverseEither { row =>
      val records     = recordsOf.getOrElse(row.identity, Vector.empty)
      val disposition =
        if records.isEmpty then TrialDisposition.Absent
        else if !withValid.contains(row.identity) then TrialDisposition.NoFixations
        else if accepted.contains(row.identity) then TrialDisposition.Admitted
        else
          TrialDisposition.Quarantined(
            causes.getOrElse(row.identity, QuarantineCause.RejectedRecords)
          )
      InventoryTrial.of(
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
        case (id, records) if inventory.labelled(id).isEmpty => (id, records)
      }
      .sortBy(_._2.head)
      .traverseEither((id, records) => UnlistedTrial.of(id, recordItems(id), records))
    val perRecord =
      if table.attributes.isEmpty then Right(Vector.empty)
      else attributes.traverseEither((record, values) => RecordAttributes.of(record, values))
    for
      inventoried <- trials.left.map(rejected)
      others      <- unlisted.left.map(rejected)
      listed      <- perRecord.left.map(rejected)
    yield
      def keyOf(id: TrialIdentity) = keys.get(canonical(id))
      val fixations                = new FixationImport[TrialKey, U](
        imported.header,
        imported.sourceRows,
        Trials(
          imported.accepted.rows.flatMap(t => keyOf(t.key).map(k => Trial(k, (), t.value)))
        ),
        imported.admitted.flatMap(r =>
          keyOf(r.key).map(k => AdmittedFixationRow(r.rowNumber, k, r.ordinal))
        ),
        imported.rejected.map(r =>
          RejectedFixationRow(r.rowNumber, r.raw, r.key.flatMap(keyOf), r.error)
        ),
        policy,
        imported.outsideFrame
      )
      new InventoryImport(
        fixations,
        inventory,
        inventoried,
        others,
        table.attributes,
        listed,
        table.samples
      )

  extension [A](values: Vector[A])
    private def traverseEither[E, B](f: A => Either[E, B]): Either[E, Vector[B]] =
      values.foldLeft[Either[E, Vector[B]]](Right(Vector.empty))((acc, a) =>
        acc.flatMap(done => f(a).map(done :+ _))
      )
