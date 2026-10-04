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

import eyes4s.design.SampleQuantum
import eyes4s.kernel.Unit2D
import eyes4s.plan.*

private[io] enum LedgerEvidenceKey[K]:
  case Study extends LedgerEvidenceKey[StudyKey]
  case Trial extends LedgerEvidenceKey[TrialKey]

  def fields(key: K): Vector[String] = this match
    case Study => Vector(key.participant, key.stimulus, key.phase)
    case Trial => Vector(key.participant, key.phase, key.trial, key.item)

private[io] enum LedgerEvidenceVisit[K]:
  case Source(value: SourceRef)
  case Texts(values: Vector[String], index: Int, dimension: LedgerResource)
  case Header(values: Vector[String])
  case Key(value: K, dimension: LedgerResource)
  case Records(values: Vector[SourceRecord[K]], index: Int)
  case Reason(value: AdmissionReason)
  case Cause(value: QuarantineCause)
  case Integers(values: Vector[Int])
  case Rules(values: Vector[AppliedCorrection[K]], index: Int)
  case Outside(values: Vector[OutsideFrame], index: Int)
  case Inventory(value: InventoryLedger)
  case Definitions(values: Vector[AttributeColumn], index: Int)
  case Attributes(values: Vector[(String, AttributeValue)], index: Int)
  case Trials(values: Vector[InventoryTrial], index: Int)
  case Unlisted(values: Vector[UnlistedTrial], index: Int)
  case RecordAttributes(values: Vector[eyes4s.plan.RecordAttributes], index: Int)

/** Resource preflight of the supplied ledger, before structural equality,
  * hashing or diagnostic rendering. Each unit visits one fixed-shape node,
  * string or vector entry. Primitive integer vectors need only their size:
  * they contain no variable-size operands. This is not semantic verification.
  *
  * The frontier stores collection references and indices. No collection is
  * copied, rendered, sorted or traversed inside a single visit. Retained units
  * count visited nodes, text code units and primitive vector elements, including
  * repeated references: they bound the logical evidence, not allocated bytes.
  */
private[io] final case class LedgerEvidenceCursor[K] private (
    limits: LedgerExecutionLimits,
    keys: LedgerEvidenceKey[K],
    pending: List[(LedgerEvidenceVisit[K], LedgerResourceLocation)],
    retained: Long,
    inventoryRecords: Long
):
  def advance(quantum: SampleQuantum): Either[LedgerResourceError, LedgerEvidenceStep[K]] =
    import LedgerEvidenceVisit.*
    var todo                                 = pending
    var kept                                 = retained
    var inventoryCount                       = inventoryRecords
    var units                                = 0
    var failure: Option[LedgerResourceError] = None
    var at = LedgerResourceLocation(LedgerResourceSource.ExpectedLedger)
    def check(dimension: LedgerResource, value: Long): Boolean =
      limits.check(dimension, value, at) match
        case Left(error) => failure = Some(error); false
        case Right(())   => true
    def keep(amount: Long): Boolean =
      limits.add(LedgerResource.RetainedEvidenceUnits, kept, amount, at) match
        case Left(error)  => failure = Some(error); false
        case Right(value) => kept = value; true
    def inventoryRows(amount: Long): Boolean =
      limits.add(LedgerResource.LogicalRecords, inventoryCount, amount, at) match
        case Left(error)  => failure = Some(error); false
        case Right(value) => inventoryCount = value; true
    def push(value: LedgerEvidenceVisit[K], location: LedgerResourceLocation = at): Unit =
      todo = (value, location) :: todo
    def texts(
        values: Vector[String],
        dimension: LedgerResource = LedgerResource.FieldCodeUnits
    ): Unit =
      if values.nonEmpty then push(Texts(values, 0, dimension))
    def identity(value: TrialIdentity): Unit =
      texts(Vector(value.participant, value.phase, value.trial))
    def next[A](values: Vector[A], index: Int)(visit: Int => LedgerEvidenceVisit[K]): Unit =
      if index + 1 < values.size then push(visit(index + 1))

    while todo.nonEmpty && units < quantum.value && failure.isEmpty do
      val (current, location) = todo.head
      todo = todo.tail
      at = location
      units += 1
      if keep(1) then
        current match
          case Source(value) =>
            value.interpretation match
              case declared: SourceInterpretation.Declared =>
                texts(Vector(declared.parser.name))
              case SourceInterpretation.LegacyUnspecified => ()
            texts(Vector(value.label))
          case Texts(values, index, dimension) =>
            at = at.copy(field = Some(index.toLong + 1))
            val value = values(index)
            if check(LedgerResource.FieldCodeUnits, value.length.toLong) &&
              (dimension == LedgerResource.FieldCodeUnits || check(
                dimension,
                value.length.toLong
              ))
            then
              val _ = keep(value.length.toLong)
            next(values, index)(Texts(values, _, dimension))
          case Header(values) =>
            if check(LedgerResource.Columns, values.size.toLong) then texts(values)
          case Key(value, dimension)  => texts(keys.fields(value), dimension)
          case Records(values, index) =>
            if (index != 0 || check(LedgerResource.LogicalRecords, values.size.toLong + 1)) &&
              index < values.size
            then
              next(values, index)(Records(values, _))
              at = at.copy(record = Some(values(index).record.toLong), field = None)
              values(index).disposition match
                case Disposition.Admitted(key, _) =>
                  push(Key(key, LedgerResource.FieldCodeUnits))
                case Disposition.Rejected(raw, key, reason) =>
                  push(Reason(reason))
                  key.foreach(k => push(Key(k, LedgerResource.FieldCodeUnits)))
                  push(Header(raw))
          case Reason(value) =>
            value match
              case AdmissionReason.Width(_, _)                        => ()
              case AdmissionReason.Key(reason)                        => texts(Vector(reason))
              case AdmissionReason.Number(column, value, requirement) =>
                texts(Vector(column, value, requirement))
              case AdmissionReason.Time(onset, duration, unit, reason) =>
                texts(Vector(onset, duration, unit, reason))
              case AdmissionReason.Position(_, _, frame)       => texts(Vector(frame.name))
              case AdmissionReason.Event(reason)               => texts(Vector(reason))
              case AdmissionReason.Quarantined(records, cause) =>
                push(Cause(cause))
                push(Integers(records))
          case Cause(value) =>
            value match
              case QuarantineCause.RejectedRecords | QuarantineCause.DuplicateOrdinals |
                  QuarantineCause.NoFixations | QuarantineCause.CorrectionConflict(_, _) =>
                ()
              case QuarantineCause.Overlap(_, previous, current) =>
                texts(Vector(previous, current))
              case QuarantineCause.WrongClock(_, expected, actual) =>
                texts(Vector(expected, actual))
              case QuarantineCause.InvalidTransition(_, reason) => texts(Vector(reason))
              case QuarantineCause.InvalidExtent(reason)        => texts(Vector(reason))
              case QuarantineCause.UnmappableFixation(_, from, to, _, _) =>
                texts(Vector(from.name, to.name))
              case QuarantineCause.ItemConflict(items)             => texts(items)
              case QuarantineCause.OccurrenceConflict(occurrences) =>
                push(Integers(occurrences))
              case QuarantineCause.NotInInventory(p, f, t, _) => texts(Vector(p, f, t))
              case QuarantineCause.InventoryItemConflict(inventory, records) =>
                texts(records)
                texts(Vector(inventory))
          case Integers(values) =>
            val _ = keep(values.size.toLong)
          case Rules(values, index) =>
            if (index != 0 || check(
                LedgerResource.CorrectionRules,
                values.size.toLong
              )) && index < values.size
            then
              next(values, index)(Rules(values, _))
              values(index).scope match
                case CorrectionScope.AllTrials()    => ()
                case CorrectionScope.Participant(p) =>
                  texts(Vector(p), LedgerResource.CorrectionOperandCodeUnits)
                case CorrectionScope.Trial(key) =>
                  push(Key(key, LedgerResource.CorrectionOperandCodeUnits))
          case Outside(values, index) =>
            if index < values.size then
              next(values, index)(Outside(values, _))
              at = at.copy(record = Some(values(index).record.toLong), field = None)
              texts(Vector(values(index).frame.name))
          case Inventory(value) =>
            // The inventory has its own header and row count, independent of
            // primary records and of how many rows collapse into each trial.
            if check(LedgerResource.LogicalRecords, inventoryCount) then
              value.sampleCounts match
                case SampleCountRule.PositiveColumn(name)   => texts(Vector(name))
                case SampleCountRule.DerivedFromDuration(_) => ()
              push(RecordAttributes(value.recordAttributes, 0))
              push(Definitions(value.recordAttributeColumns, 0))
              push(Unlisted(value.unlisted, 0))
              push(Trials(value.trials, 0))
              push(Definitions(value.attributeColumns, 0))
              push(Header(value.header))
              push(Source(value.source))
          case Definitions(values, index) =>
            if (index != 0 || check(
                LedgerResource.DeclaredAttributes,
                values.size.toLong
              )) && index < values.size
            then
              next(values, index)(Definitions(values, _))
              texts(Vector(values(index).name))
          case Attributes(values, index) =>
            if (index != 0 || check(
                LedgerResource.DeclaredAttributes,
                values.size.toLong
              )) && index < values.size
            then
              next(values, index)(Attributes(values, _))
              val (name, value) = values(index)
              value match
                case AttributeValue.Text(text) => texts(Vector(text))
                case AttributeValue.Integer(_) | AttributeValue.Number(_) |
                    AttributeValue.Blank =>
                  ()
              texts(Vector(name))
          case Trials(values, index) =>
            if index < values.size then
              val value = values(index)
              at = at.copy(record = value.rows.headOption.map(_.toLong), field = None)
              if inventoryRows(value.rows.size.toLong) then
                next(values, index)(Trials(values, _))
                value.disposition match
                  case TrialDisposition.Quarantined(cause) => push(Cause(cause))
                  case TrialDisposition.Admitted | TrialDisposition.NoFixations |
                      TrialDisposition.Absent =>
                    ()
                push(Integers(value.records))
                texts(value.recordItems)
                push(Attributes(value.attributes.entries, 0))
                texts(value.inventoryItem.toVector)
                push(Integers(value.rows))
                identity(value.identity)
          case Unlisted(values, index) =>
            if index < values.size then
              next(values, index)(Unlisted(values, _))
              val value = values(index)
              at = at.copy(record = value.records.headOption.map(_.toLong), field = None)
              push(Integers(value.records))
              texts(value.recordItems)
              identity(value.identity)
          case RecordAttributes(values, index) =>
            if index < values.size then
              next(values, index)(RecordAttributes(values, _))
              at = at.copy(record = Some(values(index).record.toLong), field = None)
              push(Attributes(values(index).attributes.entries, 0))
    failure.toLeft(
      LedgerEvidenceStep(
        units,
        kept,
        Option.when(todo.nonEmpty)(
          new LedgerEvidenceCursor(limits, keys, todo, kept, inventoryCount)
        )
      )
    )

private[io] object LedgerEvidenceCursor:
  def start[K, U <: Unit2D](
      spec: ImportSpec[K, U],
      ledger: AdmissionLedger[K],
      limits: LedgerExecutionLimits
  ): Either[LedgerExecutionEvidence.Unsupported, LedgerEvidenceCursor[K]] =
    LedgerExecutionEvidence.qualify(spec).flatMap { _ =>
      val keys: Either[LedgerExecutionEvidence.Unsupported, LedgerEvidenceKey[K]] =
        spec.keys match
          case SourceKeyColumns.Study(_, _, _)       => Right(LedgerEvidenceKey.Study)
          case SourceKeyColumns.Trial(_, _, _, _, _) => Right(LedgerEvidenceKey.Trial)
          case SourceKeyColumns.Custom(_, _, _)      =>
            Left(LedgerExecutionEvidence.Unsupported.KeyColumns)
      keys.map { shape =>
        import LedgerEvidenceVisit.*
        val at     = LedgerResourceLocation(LedgerResourceSource.ExpectedLedger)
        val visits = List(
          Source(ledger.source),
          Header(ledger.header),
          Records(ledger.records, 0),
          Rules(ledger.policy.corrections, 0),
          Outside(ledger.outsideFrame, 0)
        ) ++
          ledger.inventory.toList.map(Inventory.apply)
        new LedgerEvidenceCursor(limits, shape, visits.map(_ -> at), 0L, 1L)
      }
    }

private[io] final case class LedgerEvidenceStep[K](
    workUnits: Int,
    retainedUnits: Long,
    next: Option[LedgerEvidenceCursor[K]]
)
