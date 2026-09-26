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

import io.circe.Codec

// ---------------------------------------------------------------------------
// Admission and the ledger
// ---------------------------------------------------------------------------

enum DatasetState derives CanEqual, Codec.AsObject:
  case Draft, Admitted

/** eyes4s `QuarantineCause`, restricted to the causes an import of fixation
  * CSVs reports. `code` is the eyes4s diagnostic code.
  */
enum QuarantineCause derives CanEqual, Codec.AsObject:
  case DuplicateOrdinals, NoFixations, Overlap, RejectedRecords

  def code: String = this match
    case DuplicateOrdinals => "quarantine.duplicate-ordinals"
    case NoFixations       => "quarantine.no-fixations"
    case Overlap           => "quarantine.overlap"
    case RejectedRecords   => "quarantine.rejected-records"

object QuarantineCause:
  /** The cause whose code slug (the part after `quarantine.`) is `slug`. */
  def fromSlug(slug: String): Option[QuarantineCause] =
    values.find(_.code == s"quarantine.$slug")

final case class QuarantineCount(cause: QuarantineCause, trials: Int)
    derives CanEqual,
      Codec.AsObject

final case class MissingImage(item: String, participants: Vector[String], encodingTrials: Int)
    derives CanEqual,
      Codec.AsObject

/** What admission of one dataset revision decided, in counts. */
final case class AdmissionSummary(
    dataset: DatasetRevision,
    state: DatasetState,
    inventoryTrials: Int,
    admitted: Int,
    quarantined: Vector[QuarantineCount],
    absent: Int,
    fixationRecords: Int,
    outsideWindowRecords: Int,
    outsideWindowTrials: Int,
    items: Int,
    imagesFound: Int,
    missingImages: Vector[MissingImage],
    history: String
) derives CanEqual,
      Codec.AsObject:
  def quarantinedTrials: Int = quarantined.map(_.trials).sum

/** How admission disposed of one inventory trial. */
enum TrialDisposition derives CanEqual, Codec.AsObject:
  case Admitted
  case Quarantined(cause: QuarantineCause)

  /** Listed in the inventory with no fixation records. */
  case Absent

final case class LedgerEntry(
    trial: TrialKey,
    item: String,
    response: Option[Response],
    disposition: TrialDisposition
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
  * `pairRows` is the exact eligible count.
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

/** Whether a query enters the comparisons. `reason` is as eyes4s states it. */
enum Eligibility derives CanEqual, Codec.AsObject:
  case Eligible
  case QueryNotAdmitted(reason: String)
  case NoMatch(reason: String)

final case class PreviewRow(
    query: TrialKey,
    item: String,
    response: Response,
    matched: TrialKey,
    controls: Option[Int],
    eligibility: Eligibility
) derives CanEqual,
      Codec.AsObject

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
  case Cancelled(at: Option[JobStage])
  case Failed

  /** Finished and not yet shown. */
  case Completed

final case class RunSummary(
    run: RunId,
    revision: AnalysisRevision,
    dataset: DatasetRevision,
    state: RunState
) derives CanEqual,
      Codec.AsObject

/** Group means of M (matched), B (control) and D = M − B, as eyes4s reports them. */
final case class GroupMeans(n: Int, m: Double, b: Double, d: Double)
    derives CanEqual,
      Codec.AsObject

final case class ScoreMeans(m: Double, b: Double, d: Double, dByScale: Vector[Double])
    derives CanEqual,
      Codec.AsObject

final case class ParticipantSummary(
    participant: String,
    requested: Int,
    contributing: Int,
    failed: Int,
    noMatch: Int,
    notAdmitted: Int,
    all: ScoreMeans,
    remembered: GroupMeans,
    forgotten: GroupMeans
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

/** Grand means of one reporting group over participant means. */
final case class GroupSummary(n: Int, d: Double, dByScale: Vector[Double])
    derives CanEqual,
      Codec.AsObject

final case class ResultSummary(
    run: RunId,
    revision: AnalysisRevision,
    dataset: DatasetRevision,
    scales: Vector[String],
    pairRowsPerScale: Long,
    pairRows: Long,
    eligibleQueries: Int,
    contrasts: QueryContrasts,
    grandD: Double,
    grandDByScale: Vector[Double],
    remembered: GroupSummary,
    forgotten: GroupSummary,
    pairedN: Int,
    groupNMinimum: Int,
    groupNMaximum: Int,
    participants: Vector[ParticipantSummary]
) derives CanEqual,
      Codec.AsObject

/** The outcome of one query in a run. Scores are indexed by scale. */
enum QueryStatus derives CanEqual, Codec.AsObject:
  case Contributing(m: Vector[Double], b: Vector[Double], d: Vector[Double])
  case Failed(reason: String)
  case NoMatch(reason: String)
  case NotAdmitted(reason: String)

final case class QueryRow(
    query: TrialKey,
    item: String,
    response: Response,
    matched: TrialKey,
    controls: Option[Int],
    status: QueryStatus
) derives CanEqual,
      Codec.AsObject

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
  case Analysis(revision: AnalysisRevision)
  case Dataset(dataset: DatasetRevision)
  case Scale(index: Int, label: String)
  case Design(design: PairDesign)
  case Trial(key: TrialKey, item: String)

final case class Provenance(address: ResultAddress, trail: Vector[ProvenanceStep])
    derives CanEqual,
      Codec.AsObject
