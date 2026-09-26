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

import eyes4s.core.*
import eyes4s.design.KeyDigest
import eyes4s.kernel.*

import scala.annotation.tailrec

/** A position and the frame it is in. */
final case class FramedPosition[U <: Unit2D](frame: FrameId, position: Pt[U]) derives CanEqual

/** A record's position fields as its source holds them, and the position
  * parsed from them before any correction, in the admission frame. Only
  * `eyes4s-io`, which holds the source text, builds one, after checking that
  * correcting the parsed position gives the admitted position bit for bit.
  */
final case class RecordedPosition[U <: Unit2D] private[eyes4s] (
    xColumn: String,
    yColumn: String,
    xText: String,
    yText: String,
    parsed: FramedPosition[U]
) derives CanEqual

/** The admission policy's correction rule that moved a trial's positions:
  * its index among the policy's rules, and the correction.
  */
final case class CorrectionApplied(rule: Int, correction: Correction) derives CanEqual

/** Where a fixation falls against a study's map. Each fixation falls in
  * exactly one place, decided in this order: the initial-fixation policy
  * drops it; its centre is outside the admission frame (the screen); it is
  * on the screen but outside the analysis window ([[CentrePlacement]] decides
  * both); its trial fails as a whole, so it is in no map; or it is in the
  * map.
  */
enum MapPlacement derives CanEqual:
  case DroppedInitial
  case OutsideScreen

  /** On the screen, outside the window; the policy says what the study does. */
  case OutsideWindow(policy: OffWindowPolicy)

  /** In the window, but the trial fails with `StudyFailure.OffWindow` for the
    * tally of its kept fixations (under `OffWindowPolicy.FailTrial`, a
    * fixation of it lies outside the window), so no map is built from it.
    */
  case TrialFailed(tally: WindowTally)
  case InMap

/** How a study measures degrees of visual angle: from the centre (`origin`)
  * of the `measured` frame (the analysis window, or the admission frame of a
  * whole-frame plan), `unitsPerDegree` frame units to a degree as the plan
  * declares, into the `degrees` frame, whose `x` runs right and `y` up
  * (`degrees.yAxis` is [[YAxis.Up]]). The degrees frame is named after the
  * measured frame, `<measured>/degrees`.
  */
final case class AngularReference[U <: Unit2D] private[plan] (
    measured: FrameId,
    origin: Pt[U],
    unitsPerDegree: Double,
    degrees: Frame[Unit2D.Deg]
) derives CanEqual

/** A position in degrees under its reference. */
final case class AngularPosition[U <: Unit2D](
    reference: AngularReference[U],
    position: Pt[Unit2D.Deg]
) derives CanEqual

/** Every coordinate a fixation's position has, each in a named frame:
  *
  *  - `recorded`: the source's position fields and their parse before
  *    correction, when the source text was read (`eyes4s-io`);
  *  - `correction`: the rule that moved the trial's positions, if any;
  *  - `admitted`: the position the study input holds, in the admission frame;
  *  - `window`: the same position in the analysis window's frame (image
  *    units), for a windowed plan;
  *  - `placement`: where it falls against the map;
  *  - `angular`: degrees from the centre of the window (or of the admission
  *    frame), `x` right and `y` up, when the plan declares units per degree.
  */
final case class CoordinateTrail[U <: Unit2D] private[eyes4s] (
    recorded: Option[RecordedPosition[U]],
    correction: Option[CorrectionApplied],
    admitted: FramedPosition[U],
    window: Option[FramedPosition[U]],
    placement: MapPlacement,
    angular: Option[AngularPosition[U]]
) derives CanEqual:
  private[eyes4s] def withRecorded(position: RecordedPosition[U]): CoordinateTrail[U] =
    copy(recorded = Some(position))

/** One admitted fixation: its trial and scanpath position, its interval, the
  * source record that supplied it (or why none is known) and its coordinates.
  */
final case class FixationProvenance[K, U <: Unit2D](
    key: K,
    position: ScanpathPosition,
    span: Interval,
    source: Option[SourceRef],
    record: Either[MissingSource[K], DataRecord],
    trail: CoordinateTrail[U]
) derives CanEqual

/** Why a fixation's provenance or a page of source records was refused. */
enum ProvenanceError[+K] derives CanEqual:
  /** The plan names another input than the one supplied. */
  case InputMismatch(plan: String, input: String)

  /** The admission ledger does not describe the input. */
  case Ledger(refusal: LedgerRefusal[K])

  /** The plan's angular scale has no degrees frame on its measured frame. */
  case Angular(underlying: GeometryError)
  case UnknownTrial(key: K)

  /** The input repeats this key, so a fixation of it cannot be addressed. */
  case AmbiguousTrial(key: K, occurrences: Int)
  case FixationOutOfRange(key: K, position: ScanpathPosition, fixations: Int)

  /** The trial is in another frame than the plan's admission frame. */
  case TrialFrame(key: K, underlying: GeometryError)

  /** Two correction rules cover the trial. */
  case CorrectionConflict(key: K, first: Int, second: Int)

  /** A position could not be carried into the named frame. */
  case Unmappable(key: K, position: ScanpathPosition, into: FrameId)

  /** A ledger record number that is not a data record. */
  case Identity(underlying: RecordIdentityError)

  /** A page was asked to start after the last listed record. */
  case PageStart(from: DataRecord, records: Int)

  def message: String = this match
    case InputMismatch(plan, input) =>
      s"The plan was made for input $plan; the supplied input is $input."
    case Ledger(refusal)      => refusal.error.message
    case Angular(underlying)  => s"The plan's degrees frame: ${underlying.message}"
    case UnknownTrial(key)    => s"The input has no trial $key."
    case AmbiguousTrial(k, n) => s"The input has $n trials with key $k; none can be addressed."
    case FixationOutOfRange(k, p, n) =>
      s"Trial $k has $n fixations; there is none at scanpath position ${p.value}."
    case TrialFrame(k, e) => s"Trial $k is not in the admission frame: ${e.message}"
    case CorrectionConflict(k, first, second) =>
      s"Correction rules $first and $second both cover trial $k."
    case Unmappable(k, p, frame) =>
      s"Fixation at scanpath position ${p.value} of trial $k cannot be carried into frame ${frame.name}."
    case Identity(underlying)     => underlying.message
    case PageStart(from, records) =>
      s"No source record is listed at or after data record ${from.value}; $records are listed."

object ProvenanceError:
  given diagnose[K]: Diagnose[ProvenanceError[K], K] =
    given DiagnosticOperand[K, K] = DiagnosticOperand.key[K]
    Diagnose.derived[ProvenanceError[K], K](DiagnosticCatalog.coordinateProvenance, subject[K])(
      _.message
    )

  /** The trial, fixation or record a refusal names. */
  private def subject[K](error: ProvenanceError[K]): Vector[Locus[K]] = error match
    case UnknownTrial(key)             => Vector(Locus.Trial(key))
    case AmbiguousTrial(key, _)        => Vector(Locus.Trial(key))
    case FixationOutOfRange(key, p, _) => Vector(Locus.Trial(key), Locus.Fixation(p.value))
    case TrialFrame(key, _)            => Vector(Locus.Trial(key))
    case CorrectionConflict(key, _, _) => Vector(Locus.Trial(key))
    case Unmappable(key, p, _)         => Vector(Locus.Trial(key), Locus.Fixation(p.value))
    case PageStart(from, _)            => Vector(Locus.Record(from.csv.value))
    case _                             => Vector.empty

/** One source record of a ledger and, when it was admitted into the input,
  * the provenance of the fixation it supplied.
  */
final case class SourceRecordView[K, U <: Unit2D](
    record: DataRecord,
    entry: SourceRecord[K],
    fixation: Option[FixationProvenance[K, U]]
) derives CanEqual

/** One page of source records: the entries, how many records the ledger
  * lists in all, and the record the next page starts at.
  */
final case class RecordPage[K, U <: Unit2D](
    entries: Vector[SourceRecordView[K, U]],
    total: Int,
    next: Option[DataRecord]
) derives CanEqual

/** The coordinates of a study input's fixations under a plan, with the
  * sources the admission ledger names. Built once per plan, input and
  * ledger; each fixation's provenance is derived when it is asked for, from
  * the input, the plan and the ledger alone.
  */
final class CoordinateProvenance[K, U <: Unit2D] private (
    plan: StudyPlanGeometry[K, U],
    val sources: StudySources[K],
    paths: Map[K, Vector[Scanpath[U]]],
    val angular: Option[AngularReference[U]],
    toDegrees: Option[Warp[U, Unit2D.Deg]]
):
  /** The plan's admission frame, which admitted positions are in. */
  def admission: Frame[U] = plan.admission

  /** The scanpath positions of a trial's fixations, in order. */
  def positions(key: K): Either[ProvenanceError[K], Vector[ScanpathPosition]] =
    paths.get(key) match
      case None               => Left(ProvenanceError.UnknownTrial(key))
      case Some(Vector(path)) => Right((0 until path.n).toVector.map(new ScanpathPosition(_)))
      case Some(found)        => Left(ProvenanceError.AmbiguousTrial(key, found.size))

  /** The provenance of the fixation at a scanpath position of a trial. */
  def fixation(
      key: K,
      position: ScanpathPosition
  ): Either[ProvenanceError[K], FixationProvenance[K, U]] =
    paths.get(key) match
      case None               => Left(ProvenanceError.UnknownTrial(key))
      case Some(Vector(path)) => provenance(key, path, position)
      case Some(found)        => Left(ProvenanceError.AmbiguousTrial(key, found.size))

  /** Every source record of the ledger, in record order, a page at a time.
    * Its `total` is known before any page is built; with no ledger there are
    * no records.
    */
  val records: SourceRecordListing[K, U] = new SourceRecordListing(this, sources.ledger)

  private def provenance(
      key: K,
      path: Scanpath[U],
      position: ScanpathPosition
  ): Either[ProvenanceError[K], FixationProvenance[K, U]] =
    for
      fixation <- path.fixations
        .lift(position.value)
        .toRight(ProvenanceError.FixationOutOfRange(key, position, path.n))
      _ <- Agreement
        .frames(plan.admission, path.frame)
        .left
        .map(ProvenanceError.TrialFrame(key, _))
      correction <- plan
        .correction(key)
        .left
        .map((first, second) => ProvenanceError.CorrectionConflict(key, first, second))
      centre = fixation.centre
      window <- plan.window.fold(Right(None))(w =>
        (w.locate(centre) match
          case HalfOpenPlacement.Inside(local) => Some(local)
          case HalfOpenPlacement.Outside(_)    => w.enter(centre)
        ).map(p => Some(FramedPosition(w.frame.id, p)))
          .toRight(ProvenanceError.Unmappable(key, position, w.frame.id))
      )
      angular <- toDegrees.fold(Right(None)) { warp =>
        val measured = window.fold(centre)(_.position)
        warp(measured)
          .map(p => angularOf(p))
          .toRight(ProvenanceError.Unmappable(key, position, warp.to.id))
      }
      record <- sources.fixation(key, position.value) match
        case Left(missing) => Right(Left(missing))
        case Right(found)  =>
          CsvRecord
            .of(found.record)
            .flatMap(_.dataRecord)
            .map(Right(_))
            .left
            .map(ProvenanceError.Identity.apply)
    yield FixationProvenance(
      key,
      position,
      fixation.span,
      sources.source,
      record,
      CoordinateTrail(
        None,
        correction.map((rule, c) => CorrectionApplied(rule, c)),
        FramedPosition(path.frame.id, centre),
        window,
        plan.placement(path, position.value, centre),
        angular
      )
    )

  private def angularOf(p: Pt[Unit2D.Deg]): Option[AngularPosition[U]] =
    angular.map(AngularPosition(_, p))

object CoordinateProvenance:
  /** The provenance of an input's fixations under a plan made for it, with
    * the ledger of its import when it has one; the ledger must describe the
    * input.
    */
  def of[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      input: StudyInput[K, U],
      ledger: Option[AdmissionLedger[K]]
  ): Either[ProvenanceError[K], CoordinateProvenance[K, U]] =
    given KeyDigest[K] = plan.layout.digest
    for
      _ <- Either.cond(
        plan.input == input.reference,
        (),
        ProvenanceError.InputMismatch(plan.input.digest, input.reference.digest)
      )
      sources <- ledger.fold(Right(StudySources.unledgered(input)))(l =>
        StudySources.of(input, l).left.map(ProvenanceError.Ledger.apply)
      )
      geometry = StudyPlanGeometry(plan, ledger)
      degrees <- plan.angularScale.fold(Right(None)) { scale =>
        val onMeasured = plan.geometry match
          case StudyGeometry.Windowed(window, _, _) => scale.on(window)
          case StudyGeometry.WholeFrame(_)          => Right(scale)
        onMeasured
          .flatMap { s =>
            val id = FrameId(s"${s.frame.id.name}/degrees")
            for
              frame <- s.angularFrame(id)
              warp  <- s.angular(id)
            yield Some(
              AngularReference(s.frame.id, s.frame.centre, s.unitsPerDegree, frame) -> warp
            )
          }
          .left
          .map(ProvenanceError.Angular.apply)
      }
    yield new CoordinateProvenance(
      geometry,
      sources,
      input.trials.rows.groupMap(_.key)(_.value),
      degrees.map(_._1),
      degrees.map(_._2)
    )

/** What provenance reads from a plan and a ledger: the admission frame, the
  * window, the initial-fixation rule and the correction rules.
  */
private final class StudyPlanGeometry[K, U <: Unit2D](
    geometry: StudyGeometry[U],
    val admission: Frame[U],
    val window: Option[Subframe[U]],
    offWindow: OffWindowPolicy,
    rule: InitialFixationRule[U],
    policy: Option[AdmissionPolicy[K]],
    participant: K => String
):
  def correction(key: K): Either[(Int, Int), Option[(Int, Correction)]] =
    policy.fold(Right(None))(_.correctionFor(key, participant))

  def placement(path: Scanpath[U], index: Int, centre: Pt[U]): MapPlacement =
    val selected = rule.select(path)
    if index < selected.tally.dropped then MapPlacement.DroppedInitial
    else
      CentrePlacement.of(admission, window, centre) match
        case CentrePlacement.OutsideScreen => MapPlacement.OutsideScreen
        case CentrePlacement.OutsideWindow => MapPlacement.OutsideWindow(offWindow)
        case CentrePlacement.Inside        =>
          selected.kept
            .flatMap(kept => StudyWindowing.tally(geometry, kept).toOption)
            .filter(StudyWindowing.fails(geometry, _))
            .fold(MapPlacement.InMap)(MapPlacement.TrialFailed(_))

private object StudyPlanGeometry:
  def apply[K, U <: Unit2D](
      plan: StudyPlan[K, U, ?, ?, ?],
      ledger: Option[AdmissionLedger[K]]
  ): StudyPlanGeometry[K, U] =
    val (window, offWindow) = plan.geometry match
      case StudyGeometry.Windowed(w, _, policy) => (Some(w), policy)
      case StudyGeometry.WholeFrame(_)          => (None, OffWindowPolicy.Exclude)
    new StudyPlanGeometry(
      plan.geometry,
      plan.geometry.admission,
      window,
      offWindow,
      plan.initialFixationRule,
      ledger.map(_.policy),
      plan.layout.participant.apply
    )

/** The source records a ledger lists, in record order, paged by data record.
  * [[total]] is the ledger's record count, known before any page is built;
  * a page builds the provenance of its own admitted records only.
  */
final class SourceRecordListing[K, U <: Unit2D] private[plan] (
    provenance: CoordinateProvenance[K, U],
    ledger: Option[AdmissionLedger[K]]
):
  private val entries: Vector[SourceRecord[K]] = ledger.fold(Vector.empty)(_.records)

  /** The source the ledger's records were read from, when there is a ledger. */
  def source: Option[SourceRef] = ledger.map(_.source)

  /** The admission frame the listed positions are in. */
  def admission: Frame[U] = provenance.admission

  /** How many source records the ledger lists. */
  def total: Int = entries.size

  /** The page of at most `size` records starting at the first listed record
    * at or after `from`.
    */
  def page(from: DataRecord, size: PageSize): Either[ProvenanceError[K], RecordPage[K, U]] =
    val start                                  = from.csv.value
    @tailrec def search(lo: Int, hi: Int): Int =
      if lo >= hi then lo
      else
        val mid = (lo + hi) >>> 1
        if entries(mid).record < start then search(mid + 1, hi) else search(lo, mid)
    val index = search(0, entries.size)
    if index >= entries.size then Left(ProvenanceError.PageStart(from, total))
    else at(index, size)

  /** The first page; empty when the ledger lists no record. */
  def first(size: PageSize): Either[ProvenanceError[K], RecordPage[K, U]] = at(0, size)

  private def at(index: Int, size: PageSize): Either[ProvenanceError[K], RecordPage[K, U]] =
    val slice = entries.slice(index, index + size.value)
    val views = slice.foldLeft[Either[ProvenanceError[K], Vector[SourceRecordView[K, U]]]](
      Right(Vector.empty)
    ) { (done, entry) =>
      done.flatMap(views => view(entry).map(views :+ _))
    }
    for
      built <- views
      next  <- entries
        .lift(index + size.value)
        .fold(Right(None))(e => dataRecord(e).map(Some(_)))
    yield RecordPage(built, total, next)

  private def dataRecord(entry: SourceRecord[K]): Either[ProvenanceError[K], DataRecord] =
    CsvRecord.of(entry.record).flatMap(_.dataRecord).left.map(ProvenanceError.Identity.apply)

  private def view(entry: SourceRecord[K]): Either[ProvenanceError[K], SourceRecordView[K, U]] =
    dataRecord(entry).flatMap { record =>
      entry.disposition match
        case Disposition.Admitted(key, _) =>
          provenance.sources
            .trial(key)
            .toOption
            .flatMap(_.admitted.find(_.record == entry.record))
            .fold(Right(SourceRecordView(record, entry, None))) { source =>
              provenance
                .fixation(key, new ScanpathPosition(source.index))
                .map(p => SourceRecordView(record, entry, Some(p)))
            }
        case Disposition.Rejected(_, _, _) => Right(SourceRecordView(record, entry, None))
    }
