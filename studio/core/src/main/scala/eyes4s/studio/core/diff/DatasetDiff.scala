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

package eyes4s.studio.core.diff

import eyes4s.studio.core.backend.{
  DatasetRevision,
  LedgerEntry,
  LedgerReadError,
  TrialDisposition,
  TrialKey
}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.selection.StudioRef

/** Which of a dataset revision's column mappings a change is in. */
enum MappedSource derives CanEqual:
  case Fixations
  case Inventory

/** One difference between two dataset revisions' own content (ticket
  * S5.8): what the analyst changed, read from the document alone.
  */
enum DatasetChange derives CanEqual:
  /** The source of `role` was added, removed or replaced by other bytes. */
  case SourceBytes(role: SourceRole, from: Option[Source], to: Option[Source])

  /** The same bytes of `role` imported under another name. A source's import
    * name is part of the revision: it is what the analyst sees and what the
    * bundle stores the copy under.
    */
  case SourceRenamed(role: SourceRole, from: SourcePath, to: SourcePath)

  /** The same bytes and name of `role` with another eyes4s semantic
    * identity (bound, unbound or rebound).
    */
  case SourceIdentified(
      role: SourceRole,
      from: Option[SemanticIdentity],
      to: Option[SemanticIdentity]
  )

  /** The column playing `role` changed; `None` is unmapped. */
  case Mapped(
      source: MappedSource,
      role: ColumnRole,
      from: Option[ColumnName],
      to: Option[ColumnName]
  )

  /** An attribute column was declared, dropped, or declared as another kind. */
  case Attribute(
      source: MappedSource,
      column: ColumnName,
      from: Option[AttributeKindChoice],
      to: Option[AttributeKindChoice]
  )

  /** The trial inventory's mapping appeared or disappeared. */
  case InventoryMapped(from: Boolean, to: Boolean)
  case Units(from: DeclaredUnits, to: DeclaredUnits)
  case Screen(from: ScreenSize, to: ScreenSize)
  case Image(from: ImagePlacement, to: ImagePlacement)
  case PixelsPerDegree(from: DeclaredPixelsPerDegree, to: DeclaredPixelsPerDegree)
  case OffScreen(from: OffScreenChoice, to: OffScreenChoice)
  case CorrectionAdded(rule: CorrectionRule)
  case CorrectionRemoved(rule: CorrectionRule)

  /** The same correction rules in another order: eyes4s applies them in
    * order, so the order is content.
    */
  case CorrectionsReordered(from: Vector[CorrectionRule], to: Vector[CorrectionRule])

/** What admission made of one trial, as a status: an eyes4s disposition
  * without its operands, or `Unlisted` for a trial one ledger does not list.
  */
enum TrialStatus derives CanEqual:
  case Admitted

  /** Quarantined with the eyes4s cause `code` (`quarantine.overlap`). */
  case Quarantined(code: String)
  case NoFixations
  case Absent
  case Unlisted

object TrialStatus:
  def of(disposition: TrialDisposition): TrialStatus = disposition match
    case TrialDisposition.Admitted           => Admitted
    case TrialDisposition.Quarantined(cause) => Quarantined(cause.code)
    case TrialDisposition.NoFixations        => NoFixations
    case TrialDisposition.Absent             => Absent

/** One trial whose status differs between two ledgers. */
final case class StatusChange(trial: TrialKey, from: TrialStatus, to: TrialStatus)
    derives CanEqual:
  /** The trial, where the diff's counts lead. */
  def ref: StudioRef = StudioRef.Trial(trial)

/** The trials that went from one status to another. */
final case class Transition(from: TrialStatus, to: TrialStatus, trials: Vector[TrialKey])
    derives CanEqual:
  def count: Int              = trials.size
  def refs: Vector[StudioRef] = trials.map(StudioRef.Trial(_))

/** The trials whose admission status differs between the ledgers of two
  * dataset revisions, joined by trial key. Studio compares the statuses
  * eyes4s recorded; it decides none. A trial key one ledger lists and the
  * other does not (a remapped occurrence, say) is a change from or to
  * [[TrialStatus.Unlisted]].
  */
final case class StatusChanges private (
    from: DatasetRevision,
    to: DatasetRevision,
    changes: Vector[StatusChange]
) derives CanEqual:
  def trials: Int = changes.size

  /** The changes grouped by transition: the largest first, then in the
    * order of their first trial in the later ledger.
    */
  def transitions: Vector[Transition] =
    changes
      .groupBy(c => (c.from, c.to))
      .toVector
      .map { case ((f, t), cs) => Transition(f, t, cs.map(_.trial)) }
      .sortBy(t => (-t.count, changes.indexWhere(c => c.from == t.from && c.to == t.to)))

object StatusChanges:
  /** Compare `fromLedger` (of `from`) with `toLedger` (of `to`), in the
    * later ledger's order, then the trials only the earlier one lists. A
    * ledger listing a trial twice is refused.
    */
  def between(
      from: DatasetRevision,
      fromLedger: Vector[LedgerEntry],
      to: DatasetRevision,
      toLedger: Vector[LedgerEntry]
  ): Either[DiffError, StatusChanges] =
    def index(dataset: DatasetRevision, ledger: Vector[LedgerEntry]) =
      val repeated = ledger.map(_.trial).diff(ledger.map(_.trial).distinct).distinct
      Either.cond(
        repeated.isEmpty,
        ledger.map(e => e.trial -> TrialStatus.of(e.disposition)).toMap,
        DiffError.RepeatedTrials(dataset, repeated)
      )
    for
      before <- index(from, fromLedger)
      after  <- index(to, toLedger)
    yield
      val later = toLedger.map(_.trial).flatMap { k =>
        val was = before.getOrElse(k, TrialStatus.Unlisted)
        Option.when(was != after(k))(StatusChange(k, was, after(k)))
      }
      val dropped = fromLedger.map(_.trial).filterNot(after.contains).map { k =>
        StatusChange(k, before(k), TrialStatus.Unlisted)
      }
      StatusChanges(from, to, later ++ dropped)

/** Why a revision's ledger is not there to compare. */
enum LedgerUnavailable derives CanEqual:
  /** The backend answered, and its answer was no (the fake backend serves
    * no ledger for data r2).
    */
  case Refused(error: LedgerReadError)

  /** The call itself failed; `reason` names what failed. */
  case Failed(reason: String)

  def message: String = this match
    case Refused(error) => error.message
    case Failed(reason) => reason

/** Whether the trial statuses of two revisions were compared. */
enum StatusDiff derives CanEqual:
  case Compared(changes: StatusChanges)

  /** `dataset`'s ledger could not be read. */
  case Unavailable(dataset: DatasetRevision, error: LedgerUnavailable)

  /** The ledgers have not been read yet. */
  case NotRead

/** Why a diff could not be made. Every case names its operands. */
enum DiffError derives CanEqual:
  case UnknownDataset(dataset: DatasetRevision, known: Vector[DatasetRevision])
  case NoParent(dataset: DatasetRevision)
  case RepeatedTrials(dataset: DatasetRevision, trials: Vector[TrialKey])

  /** The status comparison is of other revisions than the diff's. */
  case StatusOfOther(
      expected: (DatasetRevision, DatasetRevision),
      found: (DatasetRevision, DatasetRevision)
  )

  def message: String = this match
    case UnknownDataset(d, known) =>
      s"The document has no dataset ${d.label} (it has ${known.map(_.label).mkString(", ")})."
    case NoParent(d)           => s"Dataset ${d.label} re-imports no earlier revision."
    case RepeatedTrials(d, ks) =>
      s"The ledger of ${d.label} lists ${ks.map(_.label).mkString(", ")} more than once."
    case StatusOfOther((ef, et), (ff, ft)) =>
      s"The status comparison is of ${ff.label} → ${ft.label}, not ${ef.label} → ${et.label}."

/** The difference between two dataset revisions (ticket S5.8): every change
  * to their own content, in the order sources, units, mappings, geometry,
  * admission choices, and the trials whose admission status changed.
  */
final case class DatasetDiff private (
    from: DatasetRevision,
    to: DatasetRevision,
    changes: Vector[DatasetChange],
    status: StatusDiff
) derives CanEqual:
  /** The number of trials whose status changed, once compared. */
  def statusChanges: Option[Int] = status match
    case StatusDiff.Compared(c) => Some(c.trials)
    case _                      => None

  /** The same diff with the statuses compared or found unavailable. */
  def withStatus(next: StatusDiff): Either[DiffError, DatasetDiff] =
    DatasetDiff.checkStatus(from, to, next).map(_ => copy(status = next))

object DatasetDiff:
  /** The diff from `from` to `to`. */
  def of(
      from: DatasetRevisionSpec,
      to: DatasetRevisionSpec,
      status: StatusDiff
  ): Either[DiffError, DatasetDiff] =
    checkStatus(from.id, to.id, status).map(_ =>
      DatasetDiff(from.id, to.id, content(from, to), status)
    )

  /** The diff from `revision`'s parent, the revision it re-imports. */
  def fromParent(
      document: StudioDocument,
      revision: DatasetRevision,
      status: StatusDiff
  ): Either[DiffError, DatasetDiff] =
    def find(d: DatasetRevision) =
      document.dataset(d).toRight(DiffError.UnknownDataset(d, document.datasets.map(_.id)))
    for
      to     <- find(revision)
      parent <- to.parent.toRight(DiffError.NoParent(revision))
      from   <- find(parent)
      diff   <- of(from, to, status)
    yield diff

  private[diff] def checkStatus(
      from: DatasetRevision,
      to: DatasetRevision,
      status: StatusDiff
  ): Either[DiffError, Unit] = status match
    case StatusDiff.Compared(c) if c.from != from || c.to != to =>
      Left(DiffError.StatusOfOther((from, to), (c.from, c.to)))
    case _ => Right(())

  private def content(
      from: DatasetRevisionSpec,
      to: DatasetRevisionSpec
  ): Vector[DatasetChange] =
    import DatasetChange.*
    def when[A](a: A, b: A)(change: => DatasetChange)(using CanEqual[A, A]) =
      Option.when(a != b)(change).toVector
    val sources = SourceRole.values.toVector.flatMap { role =>
      val (f, t) =
        (from.sources.entries.find(_.role == role), to.sources.entries.find(_.role == role))
      (f, t) match
        case (Some(a), Some(b)) if a.bytes == b.bytes =>
          when(a.path, b.path)(SourceRenamed(role, a.path, b.path)) ++
            when(a.semantic, b.semantic)(SourceIdentified(role, a.semantic, b.semantic))
        case _ => when(f.map(_.bytes.hex), t.map(_.bytes.hex))(SourceBytes(role, f, t))
    }
    def roles(source: MappedSource)(
        f: ColumnRole => Option[ColumnName],
        t: ColumnRole => Option[ColumnName]
    ) = ColumnRole.values.toVector.flatMap(r => when(f(r), t(r))(Mapped(source, r, f(r), t(r))))
    def attributes(source: MappedSource)(f: DeclaredAttributes, t: DeclaredAttributes) =
      def kind(a: DeclaredAttributes, c: ColumnName) =
        a.bindings.find(_.column == c).map(_.kind)
      (f.columns ++ t.columns).distinct.flatMap { c =>
        when(kind(f, c), kind(t, c))(Attribute(source, c, kind(f, c), kind(t, c)))
      }
    val inventory = (from.inventory, to.inventory) match
      case (Some(f), Some(t)) =>
        roles(MappedSource.Inventory)(f.column, t.column) ++
          attributes(MappedSource.Inventory)(f.attributes, t.attributes)
      case (f, t) => when(f.isDefined, t.isDefined)(InventoryMapped(f.isDefined, t.isDefined))
    val (fg, tg)         = (from.geometry, to.geometry)
    val (fRules, tRules) = (from.admission.corrections, to.admission.corrections)
    val corrections      =
      if fRules.diff(tRules).isEmpty && tRules.diff(fRules).isEmpty then
        when(fRules, tRules)(CorrectionsReordered(fRules, tRules))
      else
        fRules.diff(tRules).map(CorrectionRemoved(_)) ++ tRules
          .diff(fRules)
          .map(CorrectionAdded(_))
    sources ++
      when(from.units, to.units)(Units(from.units, to.units)) ++
      roles(MappedSource.Fixations)(from.mapping.column, to.mapping.column) ++
      attributes(MappedSource.Fixations)(from.attributes, to.attributes) ++
      inventory ++
      when(fg.screen, tg.screen)(Screen(fg.screen, tg.screen)) ++
      when(fg.image, tg.image)(Image(fg.image, tg.image)) ++
      when(fg.pixelsPerDegree, tg.pixelsPerDegree)(
        PixelsPerDegree(fg.pixelsPerDegree, tg.pixelsPerDegree)
      ) ++
      when(from.admission.offScreen, to.admission.offScreen)(
        OffScreen(from.admission.offScreen, to.admission.offScreen)
      ) ++ corrections
