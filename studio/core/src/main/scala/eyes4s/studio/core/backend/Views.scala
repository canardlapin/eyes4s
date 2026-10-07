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

import ProtocolCodecs.portableLong
import eyes4s.studio.core.document.Sigma
import eyes4s.codec.ByteDigest
import ProtocolCodecs.byteDigest

import eyes4s.plan.{QuarantineCause as CoreCause, TrialDisposition as CoreDisposition}
import io.circe.syntax.*
import io.circe.{Codec, Decoder, Encoder, HCursor, Json, JsonObject}

// ---------------------------------------------------------------------------
// Admission and the ledger
// ---------------------------------------------------------------------------

enum DatasetState derives CanEqual, Codec.AsObject:
  case Draft, Admitted

/** eyes4s `QuarantineCause`, every case with its operands. `code` is the
  * eyes4s diagnostic code. A cause this build does not know arrives as
  * [[Other]], so an older studio can read a newer backend's ledger.
  */
enum QuarantineCause derives CanEqual:
  case RejectedRecords
  case DuplicateOrdinals
  case NoFixations
  case Overlap(index: Int, previous: String, current: String)
  case WrongClock(index: Int, expected: String, actual: String)
  case InvalidTransition(index: Int, reason: String)
  case InvalidExtent(reason: String)
  case UnmappableFixation(index: Int, from: String, to: String, x: Double, y: Double)
  case CorrectionConflict(first: Int, second: Int)
  case ItemConflict(items: Vector[String])
  case OccurrenceConflict(occurrences: Vector[Int])
  case NotInInventory(participant: String, phase: String, trial: String, occurrence: Int)
  case InventoryItemConflict(inventory: String, records: Vector[String])

  /** A cause with a code this build does not know. */
  case Other(unknownCode: String, text: String)

  def code: String = this match
    case Other(unknown, _) => unknown
    case _                 => s"quarantine.${QuarantineCause.slug(productPrefix)}"

  def message: String = this match
    case RejectedRecords     => "one or more source rows were rejected"
    case DuplicateOrdinals   => "duplicate fixation ordinals"
    case NoFixations         => "a scanpath needs at least one fixation"
    case Overlap(i, p, c)    => s"fixation $i at $c begins before the previous fixation $p ends"
    case WrongClock(i, e, a) => s"fixation $i is on clock $a, expected $e"
    case InvalidTransition(i, r)               => s"transition into fixation $i: $r"
    case InvalidExtent(r)                      => s"scanpath extent: $r"
    case UnmappableFixation(i, from, to, x, y) =>
      s"fixation $i at ($x, $y) cannot be mapped from $from to $to"
    case CorrectionConflict(a, b) => s"correction rules $a and $b both apply to the trial"
    case ItemConflict(items)      =>
      s"the trial's records name different match items ${items.mkString(", ")}"
    case OccurrenceConflict(os) =>
      s"the trial's records name different occurrences ${os.mkString(", ")}"
    case NotInInventory(p, f, t, o) => s"trial $p/$f/$t#$o is not in the trial inventory"
    case InventoryItemConflict(inventory, records) =>
      s"the trial's records name items ${records.mkString(", ")}, but the inventory declares '$inventory'"
    case Other(_, message) => message

object QuarantineCause:
  private[backend] def slug(label: String): String =
    label.flatMap(c => if c.isUpper then s"-${c.toLower}" else c.toString).stripPrefix("-")

  def of(cause: CoreCause): QuarantineCause = cause match
    case CoreCause.RejectedRecords                   => RejectedRecords
    case CoreCause.DuplicateOrdinals                 => DuplicateOrdinals
    case CoreCause.NoFixations                       => NoFixations
    case CoreCause.Overlap(i, p, c)                  => Overlap(i, p, c)
    case CoreCause.WrongClock(i, e, a)               => WrongClock(i, e, a)
    case CoreCause.InvalidTransition(i, r)           => InvalidTransition(i, r)
    case CoreCause.InvalidExtent(r)                  => InvalidExtent(r)
    case CoreCause.UnmappableFixation(i, f, t, x, y) =>
      UnmappableFixation(i, f.name, t.name, x, y)
    case CoreCause.CorrectionConflict(a, b)    => CorrectionConflict(a, b)
    case CoreCause.ItemConflict(items)         => ItemConflict(items)
    case CoreCause.OccurrenceConflict(os)      => OccurrenceConflict(os)
    case CoreCause.NotInInventory(p, f, t, o)  => NotInInventory(p, f, t, o)
    case CoreCause.InventoryItemConflict(i, r) => InventoryItemConflict(i, r)

  private def operands(cause: QuarantineCause): JsonObject = cause match
    case Overlap(i, p, c) =>
      JsonObject("index" -> i.asJson, "previous" -> p.asJson, "current" -> c.asJson)
    case WrongClock(i, e, a) =>
      JsonObject("index" -> i.asJson, "expected" -> e.asJson, "actual" -> a.asJson)
    case InvalidTransition(i, r) => JsonObject("index" -> i.asJson, "reason" -> r.asJson)
    case InvalidExtent(r)        => JsonObject("reason" -> r.asJson)
    case UnmappableFixation(i, f, t, x, y) =>
      JsonObject(
        "index" -> i.asJson,
        "from"  -> f.asJson,
        "to"    -> t.asJson,
        "x"     -> x.asJson,
        "y"     -> y.asJson
      )
    case CorrectionConflict(a, b)   => JsonObject("first" -> a.asJson, "second" -> b.asJson)
    case ItemConflict(items)        => JsonObject("items" -> items.asJson)
    case OccurrenceConflict(os)     => JsonObject("occurrences" -> os.asJson)
    case NotInInventory(p, f, t, o) =>
      JsonObject(
        "participant" -> p.asJson,
        "phase"       -> f.asJson,
        "trial"       -> t.asJson,
        "occurrence"  -> o.asJson
      )
    case InventoryItemConflict(i, r) =>
      JsonObject("inventory" -> i.asJson, "records" -> r.asJson)
    case _ => JsonObject.empty

  /** `{"code": …, "message": …, <operands>}`: the code selects the case. */
  given Encoder.AsObject[QuarantineCause] = Encoder.AsObject.instance { c =>
    operands(c).+:("message" -> c.message.asJson).+:("code" -> c.code.asJson)
  }

  given Decoder[QuarantineCause] = Decoder.instance { (h: HCursor) =>
    h.get[String]("code").flatMap { code =>
      code.stripPrefix("quarantine.") match
        case "rejected-records"   => Right(RejectedRecords)
        case "duplicate-ordinals" => Right(DuplicateOrdinals)
        case "no-fixations"       => Right(NoFixations)
        case "overlap"            =>
          for
            i <- h.get[Int]("index"); p <- h.get[String]("previous")
            c <- h.get[String]("current")
          yield Overlap(i, p, c)
        case "wrong-clock" =>
          for
            i <- h.get[Int]("index"); e <- h.get[String]("expected")
            a <- h.get[String]("actual")
          yield WrongClock(i, e, a)
        case "invalid-transition" =>
          for i <- h.get[Int]("index"); r <- h.get[String]("reason")
          yield InvalidTransition(i, r)
        case "invalid-extent"      => h.get[String]("reason").map(InvalidExtent(_))
        case "unmappable-fixation" =>
          for
            i <- h.get[Int]("index"); f <- h.get[String]("from"); t <- h.get[String]("to")
            x <- h.get[Double]("x"); y  <- h.get[Double]("y")
          yield UnmappableFixation(i, f, t, x, y)
        case "correction-conflict" =>
          for a <- h.get[Int]("first"); b <- h.get[Int]("second")
          yield CorrectionConflict(a, b)
        case "item-conflict"       => h.get[Vector[String]]("items").map(ItemConflict(_))
        case "occurrence-conflict" =>
          h.get[Vector[Int]]("occurrences").map(OccurrenceConflict(_))
        case "not-in-inventory" =>
          for
            p <- h.get[String]("participant"); f <- h.get[String]("phase")
            t <- h.get[String]("trial"); o       <- h.get[Int]("occurrence")
          yield NotInInventory(p, f, t, o)
        case "inventory-item-conflict" =>
          for i <- h.get[String]("inventory"); r <- h.get[Vector[String]]("records")
          yield InventoryItemConflict(i, r)
        case _ => h.get[String]("message").map(Other(code, _))
    }
  }

/** eyes4s `TrialDisposition` (UI-H): what admission did with one inventory trial. */
enum TrialDisposition derives CanEqual, Codec.AsObject:
  case Admitted
  case Quarantined(cause: QuarantineCause)

  /** Every record was rejected on its own; takes precedence over
    * `Quarantined(RejectedRecords)`.
    */
  case NoFixations

  /** Listed in the inventory with no fixation records. */
  case Absent

object TrialDisposition:
  def of(disposition: CoreDisposition): TrialDisposition = disposition match
    case CoreDisposition.Admitted       => Admitted
    case CoreDisposition.Quarantined(c) => Quarantined(QuarantineCause.of(c))
    case CoreDisposition.NoFixations    => NoFixations
    case CoreDisposition.Absent         => Absent

/** eyes4s `OutsideFrame`: an admitted record whose finite position lies off
  * the admission frame (the screen). Under `ExcludeRecord` it is kept out of
  * every map and reported as "outside screen", but stays a fixation of its
  * trial's scanpath, so later fixations keep their `ScanpathPosition`.
  */
final case class OutsideFrame(record: Int, x: Double, y: Double, frame: String)
    derives CanEqual,
      Codec.AsObject

/** Trials quarantined with one cause code. */
final case class QuarantineCount(code: String, trials: Int) derives CanEqual, Codec.AsObject

final case class MissingImage(item: String, participants: Vector[String], encodingTrials: Int)
    derives CanEqual,
      Codec.AsObject

/** eyes4s `WindowSummary` with the `WindowTally` durations summed: fixations
  * of the tallied (admitted) trials outside the screen and, on the screen,
  * outside the analysis window. `sourceRecords` counts every ledger record.
  */
final case class WindowTotals(
    outsideWindow: Int,
    outsideScreen: Int,
    total: Int,
    trialsOutsideWindow: Int,
    trialsOutsideScreen: Int,
    trials: Int,
    untallied: Int,
    sourceRecords: Option[Int],
    outsideWindowMicros: Long,
    outsideScreenMicros: Long,
    totalMicros: Long
) derives CanEqual,
      Codec.AsObject

/** The backend's inventory accounting, including its authoritative equation check. */
final case class AdmissionEquation(
    admitted: Int,
    quarantined: Int,
    absent: Int,
    inventory: Int,
    balances: Boolean
) derives CanEqual,
      Codec.AsObject

/** What admission of one dataset revision decided, in counts. `quarantined`
  * counts `Quarantined(cause)` trials by cause code; `noFixations` is a
  * disposition of its own, and so is absent, which only an inventory can
  * count ([[InventoryJoin]], S5.4).
  *
  * `history` is deprecated: free text the backend wrote for the dataset's
  * changes, which studio no longer reads. The Data history line is the
  * typed dataset diff (S5.8, `eyes4s.studio.core.diff.DatasetDiff`). The
  * field stays on the wire until its retirement is scheduled.
  */
final case class AdmissionSummary(
    dataset: DatasetRevision,
    state: DatasetState,
    inventory: InventoryJoin,
    admitted: Int,
    quarantined: Vector[QuarantineCount],
    noFixations: Int,
    fixationRecords: Int,
    window: WindowTotals,
    items: Int,
    imagesFound: Int,
    missingImages: Vector[MissingImage],
    history: String,
    quarantinedTotal: Int,
    equation: Option[AdmissionEquation]
) derives CanEqual,
      Codec.AsObject:
  def quarantinedTrials: Int = quarantined.map(_.trials).sum

  /** The inventory's trials, when the dataset declares one. */
  def inventoryTrials: Option[Int] = inventory.trialCount

  /** Inventory trials without fixation records, when there is an inventory. */
  def absent: Option[Int] = inventory.absentCount

final case class LedgerEntry(
    trial: TrialKey,
    item: String,
    response: Option[Response],
    disposition: TrialDisposition,
    outsideFrame: Vector[OutsideFrame]
) derives CanEqual,
      Codec.AsObject

final case class LedgerPage(
    dataset: DatasetRevision,
    page: PageInfo,
    entries: Vector[LedgerEntry]
) derives CanEqual,
      Codec.AsObject

// ---------------------------------------------------------------------------
// Preview of the resolved design (eyes4s StudyPreview)
// ---------------------------------------------------------------------------

/** Counts of one revision's resolved design before execution.
  * `candidatePairsPerScale` is eyes4s `candidatePairCount`, an upper bound;
  * `pairRows` is the exact eligible count (UI-D `StudyCounts`).
  */
final case class PreviewSummary(
    revision: AnalysisRevision,
    dataset: DatasetRevision,
    scales: Vector[String],
    focalTrials: Int,
    referenceTrials: Int,
    requestedQueries: Int,
    eligibleQueries: Int,
    candidatePairsPerScale: Long,
    pairRowsPerScale: Long,
    pairRows: Long
) derives CanEqual,
      Codec.AsObject

/** Whether a query enters the comparisons. */
enum Eligibility derives CanEqual, Codec.AsObject:
  case Eligible

  /** The query trial itself was not admitted. */
  case QueryNotAdmitted(disposition: TrialDisposition)

  /** Admitted, but without a matched reference (`study-finding.unmatched-focal`). */
  case NoMatch(diagnostic: StudioDiagnostic)

final case class PreviewRow(
    query: TrialKey,
    item: String,
    response: Response,
    matched: Option[TrialKey],
    controls: Option[Int],
    eligibility: Eligibility
) derives CanEqual,
      Codec.AsObject

object PreviewRow:
  /** A present reference is meaningful only for a query the design admits. */
  def apply(
      query: TrialKey,
      item: String,
      response: Response,
      matched: TrialKey,
      controls: Option[Int],
      eligibility: Eligibility
  ): PreviewRow =
    new PreviewRow(
      query,
      item,
      response,
      Option.when(eligibility == Eligibility.Eligible)(matched),
      controls,
      eligibility
    )

final case class PreviewPage(
    revision: AnalysisRevision,
    page: PageInfo,
    rows: Vector[PreviewRow]
) derives CanEqual,
      Codec.AsObject

// ---------------------------------------------------------------------------
// Runs and results
// ---------------------------------------------------------------------------

enum RunState derives CanEqual, Codec.AsObject:
  /** The run the project shows; its revision and data are current. */
  case Current

  /** Its data or revision has since changed. */
  case Stale

  case Running(job: JobId)
  case Cancelled(at: Option[StageKind])
  case Failed

  /** Finished and not yet shown. */
  case Completed

  def label: String = this match
    case Current       => "current"
    case Stale         => "stale"
    case Running(job)  => s"running (job ${job.number})"
    case Cancelled(at) => at.fold("cancelled")(k => s"cancelled at ${k.productPrefix}")
    case Failed        => "failed"
    case Completed     => "completed"

final case class RunSummary(
    run: RunId,
    revision: AnalysisRevision,
    dataset: DatasetRevision,
    state: RunState
) derives CanEqual,
      Codec.AsObject

/** A participant's query accounting across the run, independent of reporting
  * spec and scale. Numerical means belong to the requested `ReportView`.
  */
final case class ParticipantCounts(
    participant: String,
    requested: Int,
    contributing: Int,
    failed: Int,
    noMatch: Int,
    notAdmitted: Int
) derives CanEqual,
      Codec.AsObject

/** What became of the requested queries (retrieval trials). */
final case class QueryContrasts(
    requested: Int,
    queryNotAdmitted: Int,
    noMatch: Int,
    failed: Int,
    contributing: Int
) derives CanEqual,
      Codec.AsObject

/** Scale-free run facts. Reporting edits read numerical cells only through
  * `report(run, spec, scale)`, whose references carry the exact context.
  */
final case class ResultSummary(
    run: RunId,
    revision: AnalysisRevision,
    dataset: DatasetRevision,
    scales: Vector[Sigma],
    pairRowsPerScale: Long,
    pairRows: Long,
    eligibleQueries: Int,
    contrasts: QueryContrasts,
    participants: Vector[ParticipantCounts]
) derives CanEqual:
  /** Display/export text only; scale identity is carried by `scales`. */
  def scaleLabels: Vector[String] = scales.map(_.label)

object ResultSummary:
  // Safe integral degrees keep raw wire pins identical on JVM and JS.
  // Document codecs remain unchanged; this codec belongs to protocol 1.18.
  private given Codec[Sigma] = Codec.from(
    Decoder[Double].emap(d => Sigma.of(d).left.map(_.message)),
    Encoder.instance(sigma =>
      val degrees        = sigma.degrees
      val maxSafeInteger = 9007199254740991.0
      if degrees == math.rint(degrees) && degrees <= maxSafeInteger then
        Json.fromLong(degrees.toLong)
      else Json.fromDoubleOrNull(degrees)
    )
  )
  given Codec.AsObject[ResultSummary] = Codec.AsObject.derived

/** The outcome of one query in a run. Scores are indexed by scale. */
enum QueryStatus derives CanEqual, Codec.AsObject:
  case Contributing(m: Vector[Double], b: Vector[Double], d: Vector[Double])
  case Failed(diagnostic: StudioDiagnostic)
  case NoMatch(diagnostic: StudioDiagnostic)
  case NotAdmitted(disposition: TrialDisposition)

  /** Every scale failed; diagnostics retain the run's declared scale order. */
  case FailedAtScales(diagnostics: Vector[StudioDiagnostic])

  def isFailed: Boolean = this match
    case Failed(_) | FailedAtScales(_) => true
    case _                             => false

  /** A uniform fixture failure applies at every scale; native failures
    * retain the diagnostic belonging to the requested scale.
    */
  def diagnosticAt(scale: Int): Option[StudioDiagnostic] = this match
    case Failed(diagnostic) if scale >= 0 => Some(diagnostic)
    case FailedAtScales(diagnostics)      => diagnostics.lift(scale)
    case _                                => None

final case class QueryRow(
    query: TrialKey,
    item: String,
    response: Response,
    matched: Option[TrialKey],
    controls: Option[Int],
    status: QueryStatus
) derives CanEqual,
      Codec.AsObject

object QueryRow:
  /** An unmatched or unadmitted query carries no guessed reference. */
  def apply(
      query: TrialKey,
      item: String,
      response: Response,
      matched: TrialKey,
      controls: Option[Int],
      status: QueryStatus
  ): QueryRow =
    val reference = status match
      case QueryStatus.NoMatch(_) | QueryStatus.NotAdmitted(_) => None
      case _                                                   => Some(matched)
    new QueryRow(query, item, response, reference, controls, status)

final case class QueryPage(run: RunId, page: PageInfo, rows: Vector[QueryRow])
    derives CanEqual,
      Codec.AsObject

// ---------------------------------------------------------------------------
// Inspection and provenance
// ---------------------------------------------------------------------------

/** One inspected result item, as eyes4s `ResultInspection` resolves it. */
enum Inspection derives CanEqual, Codec.AsObject:
  /** A contrast row: the matched and control reductions and D = M − B. */
  case Contrast(address: ResultAddress, m: Double, b: Double, d: Double)

  /** A reduction over `members` selected pairs. */
  case Reduction(address: ResultAddress, value: Double, members: Int)

  /** One directed pair and its score. */
  case Pair(address: ResultAddress, referenceItem: String, score: Double)

  /** The addressed query has no score; `status` says why. */
  case Unscored(address: ResultAddress, status: QueryStatus)

/** One step from a run to the object an address names, coarsest first. */
enum ProvenanceStep derives CanEqual, Codec.AsObject:
  case Run(run: RunId)
  case Recomputed(eyes4sVersion: String)
  case Analysis(revision: AnalysisRevision)
  case Dataset(dataset: DatasetRevision)
  case Scale(index: Int, label: String)
  case Design(design: PairDesign)
  case Trial(key: TrialKey, item: String)
  case Restored(manifest: ByteDigest)

final case class Provenance(address: ResultAddress, trail: Vector[ProvenanceStep])
    derives CanEqual,
      Codec.AsObject
