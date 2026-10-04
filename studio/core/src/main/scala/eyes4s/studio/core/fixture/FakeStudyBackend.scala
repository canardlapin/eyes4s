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

package eyes4s.studio.core.fixture

import cats.Eq
import cats.effect.Concurrent
import cats.syntax.all.*
import eyes4s.codec.CanonicalDigest
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.DatasetRevisionSpec
import eyes4s.studio.core.execution.RunStamp
import eyes4s.studio.core.navigation.StudyNavigator
import eyes4s.studio.core.preview.*
import fs2.Stream
import fs2.concurrent.SignallingRef

/** The three boards of the story timeline (FIXTURE.md "Story moments"). */
enum StoryMoment derives CanEqual:
  /** Data · verify: dataset r3 is a draft re-import of r2; run 5 (rev 3, r2)
    * is the only run. No run on r3 yet.
    */
  case T1

  /** r3 admitted; run 5 stale, run 6 cancelled at Comparing, run 7 (rev 4)
    * current; draft rev 5 is ready and not run. No jobs.
    */
  case T2

  /** As T2, with rev 5 submitted: run 8 running, held at Comparing
    * 21,400 / 44,845 pairs.
    */
  case T3

object StoryMoment:
  /** The running chip of every board: "Run 8 · Comparing · 21,400 / 44,845
    * pairs" (DESIGN_SPEC section 12), counted over every scale.
    */
  val RunningPairs: Long = 21400L

/** Why a test's command to a fake job was refused. */
enum FakeControlError derives CanEqual:
  case UnknownJob(job: JobId, known: Vector[JobId])
  case NotRunning(job: JobId, state: JobState)
  case UnscriptedSegment(job: JobId, segment: Segment)

  /** Run-level progress may not move back: `(segment, done)` is ordered by
    * the script position of the segment, then by `done`.
    */
  case Regressed(job: JobId, from: Segment, fromDone: Long, to: Segment, toDone: Long)

  /** No comparison segment of the script ends at or after this run-wide count. */
  case PairsOutOfRange(job: JobId, pairs: Long, total: ProgressTotal)
  case Progress(underlying: ProgressError)

  /** `revision` is already declared on `declared`, not `requested`. */
  case RevisionConflict(
      revision: AnalysisRevision,
      declared: DatasetRevision,
      requested: DatasetRevision
  )
  case MissingSnapshot(job: JobId)

  def message: String = this match
    case UnknownJob(job, known) =>
      s"No job ${job.number}; the fake has ${known.map(_.number).mkString(", ")}."
    case NotRunning(job, state)          => s"Job ${job.number} has finished (state $state)."
    case UnscriptedSegment(job, segment) =>
      s"Job ${job.number} has no segment $segment in its script."
    case Regressed(job, from, fromDone, to, toDone) =>
      s"Job ${job.number} cannot move back from $fromDone of $from to $toDone of $to."
    case PairsOutOfRange(job, pairs, total) =>
      s"Job ${job.number} cannot hold at $pairs pairs of $total."
    case Progress(underlying)                            => underlying.message
    case RevisionConflict(revision, declared, requested) =>
      s"${revision.label} stands on data ${declared.label}, not ${requested.label}."
    case MissingSnapshot(job) => s"Job ${job.number} has no retained prepared snapshot."

/** How the fake answers for a dataset's trial inventory (S5.4, S5.6): the
  * fixture's joined inventory, none declared, or eyes4s's refusal of it.
  */
enum InventoryScenario derives CanEqual:
  case Joined
  case Undeclared
  case Refused(issues: Vector[InventoryIssue])

/** One segment of a fake job's script with its stated total. */
final case class ScriptedSegment(segment: Segment, total: ProgressTotal) derives CanEqual

/** A test double of [[StudyBackend]] that serves docs/studio/fixture/fixture.json
  * exactly, and the inventory of fixtures/studio-golden as the ledger.
  *
  * The ledger's dispositions and window totals are recomputed at build time
  * (project/StudioFixture.scala) from the rules the fixture README states for
  * eyes4s `FixationCsv.read` under `OffScreenPolicy.ExcludeRecord`; eyes4s
  * does not compute them here. `StudioFixtureCountsSuite` checks them against
  * fixture.json trial by trial.
  *
  * Jobs never advance by themselves. A test moves one with [[advanceTo]] or
  * [[advanceToPairs]] and ends it with [[complete]], [[fail]] or `cancel`, so
  * any step of a run can be held deterministically. A job's script orders its
  * segments by scale, then stage, as eyes4s runs them.
  *
  * The fake computes no science: every score it serves is a value of the
  * fixture. Only a completed run of rev 4 on data r3 (run 7 in the story) has
  * scores; the fixture has no numbers for data r2 or for a completed rev 5.
  */
final class FakeStudyBackend[F[_]] private[fixture] (
    val study: MockStudy,
    val moment: StoryMoment,
    state: SignallingRef[F, FakeStudyBackend.State]
)(using F: Concurrent[F])
    extends StudyBackend[F]:
  import FakeStudyBackend.*

  private val summary = study.summary

  /** The dataset every fixture number describes. */
  private val servedDataset = DatasetRevision(3)

  /** The analysis whose scores fixture.json holds: rev 4 on data r3. Every
    * completed run of it serves them (run 7 in the story; a headless journey
    * that saves rev 4 itself gets its own run number, S3.6).
    */
  private val scoredRevision = AnalysisRevision(4)

  private val byKey: Map[TrialKey, MockQuery]      = study.queries.map(q => q.key -> q).toMap
  private val ledgerOf: Map[TrialKey, LedgerEntry] =
    study.inventory.map(e => e.trial -> e).toMap

  private def scalesOf(revision: AnalysisRevision): Vector[String] =
    if revision.number >= 5 then summary.scales :+ Rev5Scale else summary.scales

  private def pairRowsOf(revision: AnalysisRevision): Long =
    if revision.number >= 5 then summary.pairRowsRev5 else summary.pairRowsAllScales

  /** A job's segments in run order, each with its stated total: every
    * admitted trial's map, each eligible query's matched pair and its
    * controls, one key per eligible query and design, and at most one contrast
    * row per eligible query, at every scale.
    */
  def script(revision: AnalysisRevision): Vector[ScriptedSegment] =
    val eligible = summary.eligibleQueries.toLong
    val controls = summary.pairRowsPerScale - eligible
    scalesOf(revision).indices.toVector.flatMap { s =>
      Vector(
        ScriptedSegment(Segment.Estimating(s), ProgressTotal.Exact(summary.admitted.toLong)),
        ScriptedSegment(
          Segment.Comparing(s, PairDesign.Matched),
          ProgressTotal.Exact(eligible)
        ),
        ScriptedSegment(
          Segment.Comparing(s, PairDesign.Control),
          ProgressTotal.Exact(controls)
        ),
        ScriptedSegment(Segment.Reducing(s, PairDesign.Matched), ProgressTotal.Exact(eligible)),
        ScriptedSegment(Segment.Reducing(s, PairDesign.Control), ProgressTotal.Exact(eligible)),
        ScriptedSegment(Segment.Contrasting(s), ProgressTotal.AtMost(eligible))
      )
    }

  private def prepared(r: AnalysisRevision, d: DatasetRevision): FakePreparedSnapshot =
    FakePreparedSnapshot(study, PreviewStamp.fake(r, d), scalesOf(r), script(r))

  private def full(s: ScriptedSegment): Long = s.total.bound.getOrElse(0L)

  private def sumOf(plan: Vector[ScriptedSegment], unit: CountUnit): Long =
    plan.filter(_.segment.unit == unit).map(full).sum

  /** The run-wide pair total of a revision's script. */
  def totalPairs(revision: AnalysisRevision): Long = sumOf(script(revision), CountUnit.Pairs)

  // -------------------------------------------------------------------------
  // Admission and the ledger
  // -------------------------------------------------------------------------

  private def dataset(d: DatasetRevision): F[Either[BackendError, DatasetState]] =
    state.get.map { s =>
      s.datasets.get(d) match
        case None =>
          Left(BackendError.UnknownDataset(d, s.datasets.keys.toVector.sortBy(_.number)))
        case Some(_) if d != servedDataset =>
          Left(BackendError.Unavailable(DiagnosticLocus.Dataset(d)))
        case Some(st) => Right(st)
    }

  /** The dataset's state, or the refusal its inventory scenario makes. */
  private def inventoried(
      d: DatasetRevision
  ): F[Either[BackendError, (DatasetState, InventoryScenario)]] =
    (dataset(d), state.get).mapN { (known, s) =>
      val scenario = s.inventories.getOrElse(d, InventoryScenario.Joined)
      known.flatMap { st =>
        scenario match
          case InventoryScenario.Refused(issues) =>
            Left(BackendError.InventoryRefused(d, issues))
          case _ => Right((st, scenario))
      }
    }

  /** The admission of `d`, verified for `content`: refused when this backend
    * holds other content for `d`. It holds each story revision's own content
    * from the start, and what [[holdContent]] gives it since (a test standing
    * for a project saved with a re-mapped revision); the real backend reads
    * its own stored revisions (S3.7). Content it holds nothing for is refused
    * too: nothing is verified by default.
    */
  def verify(
      d: DatasetRevision,
      content: CanonicalDigest[DatasetRevisionSpec]
  ): F[Either[BackendError, AdmissionSummary]] =
    state.get.flatMap(s =>
      s.contents.get(d) match
        // An unknown dataset is refused as admission refuses it.
        case None if !s.datasets.contains(d) => admission(d)
        case None => Concurrent[F].pure(Left(BackendError.ContentNotHeld(d, content)))
        case Some(held) if held != content =>
          Concurrent[F].pure(Left(BackendError.ContentMismatch(d, content, held)))
        case Some(_) => admission(d)
    )

  def admission(d: DatasetRevision): F[Either[BackendError, AdmissionSummary]] =
    inventoried(d).map(_.map { (st, scenario) =>
      val bySlug = summary.quarantineBySlug.toMap
      AdmissionSummary(
        d,
        st,
        scenario match
          case InventoryScenario.Undeclared => InventoryJoin.Undeclared
          case _ => InventoryJoin.Joined(summary.inventoryTrials, summary.absent)
        ,
        summary.admitted,
        summary.quarantineBySlug.collect {
          case (slug, n) if slug != NoFixationsSlug => QuarantineCount(s"quarantine.$slug", n)
        },
        bySlug.getOrElse(NoFixationsSlug, 0),
        summary.fixationRecords,
        WindowTotals(
          outsideWindow = GoldenInventory.outsideWindow,
          outsideScreen = GoldenInventory.outsideScreen,
          total = GoldenInventory.talliedRecords,
          trialsOutsideWindow = GoldenInventory.trialsOutsideWindow,
          trialsOutsideScreen = GoldenInventory.trialsOutsideScreen,
          trials = GoldenInventory.talliedTrials,
          untallied = 0,
          sourceRecords = Some(GoldenInventory.sourceRecords),
          outsideWindowMicros = GoldenInventory.outsideWindowMicros,
          outsideScreenMicros = GoldenInventory.outsideScreenMicros,
          totalMicros = GoldenInventory.talliedMicros
        ),
        summary.itemsInPool,
        summary.imagesFound,
        summary.missingImages,
        summary.datasetHistory.getOrElse(d.label, "")
      )
    })

  /** Without an inventory, nothing lists the absent trials. */
  def ledger(d: DatasetRevision, page: PageRequest): F[Either[BackendError, LedgerPage]] =
    inventoried(d).map(_.map { (_, scenario) =>
      val all = scenario match
        case InventoryScenario.Undeclared =>
          study.inventory.filterNot(_.disposition == TrialDisposition.Absent)
        case _ => study.inventory
      val entries = slice(all, page)
      LedgerPage(d, PageInfo.of(page, all.size, entries.size), entries)
    })

  // -------------------------------------------------------------------------
  // Preview
  // -------------------------------------------------------------------------

  private def revision(r: AnalysisRevision): F[Either[BackendError, DatasetRevision]] =
    state.get.map { s =>
      s.revisions.get(r) match
        case None =>
          Left(BackendError.UnknownRevision(r, s.revisions.keys.toVector.sortBy(_.number)))
        case Some(d) if d != servedDataset =>
          Left(BackendError.Unavailable(DiagnosticLocus.Revision(r)))
        case Some(d) => Right(d)
    }

  // -------------------------------------------------------------------------
  // Trial views (protocol 1.6, S6.2)
  // -------------------------------------------------------------------------

  def trialFixations(
      r: AnalysisRevision,
      trial: TrialKey
  ): F[Either[BackendError, TrialFixations]] =
    revision(r).map(
      _.flatMap(d => known(d, trial).flatMap(FakeTrialViews.fixations(moment, r, _, trial)))
    )

  def trialPreview(
      r: AnalysisRevision,
      trial: TrialKey
  ): F[Either[BackendError, TrialPreview]] =
    revision(r).map(
      _.flatMap(d => known(d, trial).flatMap(FakeTrialViews.preview(moment, r, _, trial)))
    )

  def placement(spec: DatasetRevisionSpec): F[Either[BackendError, PlacementPreview]] =
    Concurrent[F].pure(FakePlacement.of(spec))

  def sourceRecords(
      r: AnalysisRevision,
      from: Int,
      count: Int
  ): F[Either[BackendError, SourceRecordPage]] =
    revision(r).map(_.flatMap(FakeSourceRecords.page(moment, r, _, from, count)))

  /** `d` when `trial` is in its inventory; a trial outside it is refused. */
  private def known(
      d: DatasetRevision,
      trial: TrialKey
  ): Either[BackendError, DatasetRevision] =
    Either.cond(ledgerOf.contains(trial), d, BackendError.UnknownTrial(d, trial))

  def preview(r: AnalysisRevision): F[Either[BackendError, PreviewSummary]] =
    revision(r).map(_.map { d =>
      PreviewSummary(
        r,
        d,
        scalesOf(r),
        focalTrials = study.queries.size,
        referenceTrials = study.inventory.count(_.trial.phase == Phase.Encoding),
        requestedQueries = summary.contrasts.requested,
        eligibleQueries = summary.eligibleQueries,
        candidatePairsPerScale = summary.candidatePairsPerScale,
        pairRowsPerScale = summary.pairRowsPerScale,
        pairRows = pairRowsOf(r)
      )
    })

  private def dispositionOf(q: MockQuery): TrialDisposition =
    ledgerOf.get(q.key).fold(TrialDisposition.Absent)(_.disposition)

  /** eyes4s `StudyFinding.UnmatchedFocal`, reported as no match by the
    * persisted policy (FIXTURE.md).
    */
  private def unmatched(q: MockQuery): StudioDiagnostic =
    StudioDiagnostic(
      "study-finding.unmatched-focal",
      DiagnosticLevel.Warning,
      DiagnosticOrigin.EyesCore,
      Vector(DiagnosticLocus.Trial(q.key)),
      q.reason.getOrElse("")
    )

  /** eyes4s `StudyFailure.OffWindow`: no fixation of the trial lies in its map. */
  private def offWindow(q: MockQuery): StudioDiagnostic =
    StudioDiagnostic(
      "study-failure.off-window",
      DiagnosticLevel.Error,
      DiagnosticOrigin.EyesCore,
      Vector(DiagnosticLocus.Trial(q.key)),
      q.reason.getOrElse("")
    )

  private def eligibility(q: MockQuery): Eligibility = q.status match
    case QueryNotAdmitted => Eligibility.QueryNotAdmitted(dispositionOf(q))
    case NoMatchStatus    => Eligibility.NoMatch(unmatched(q))
    case _                => Eligibility.Eligible

  def previewRows(
      r: AnalysisRevision,
      page: PageRequest
  ): F[Either[BackendError, PreviewPage]] =
    revision(r).map(_.map { _ =>
      val rows = slice(study.queries, page).map { q =>
        PreviewRow(q.key, q.item, q.response, q.matchedKey, q.controls, eligibility(q))
      }
      PreviewPage(r, PageInfo.of(page, study.queries.size, rows.size), rows)
    })

  /** The fixture's preview counts, refused if the fixture states a negative
    * count. [[FakeStudyBackend.create]] refuses such a fixture as a defect.
    */
  private[fixture] val previewFacts: Either[PreviewError, FakePreviewFacts] =
    def counts(r: AnalysisRevision) =
      PreviewCounts.of(
        summary.pairRowsPerScale,
        pairRowsOf(r),
        summary.eligibleQueries,
        summary.contrasts.noMatch,
        ambiguousMatches = 0
      )
    for
      candidates <- PreviewCandidates.of(
        study.queries.size,
        study.inventory.count(_.trial.phase == Phase.Encoding),
        study.queries.map(_.key.participant).distinct.size,
        summary.candidatePairsPerScale,
        summary.contrasts.requested,
        summary.contrasts.queryNotAdmitted,
        // Enc→Ret has no reference-less category; a recognition recipe does.
        byDesignQueries = None
      )
      allScales <- counts(scoredRevision)
      rev5      <- counts(AnalysisRevision(5))
      _         <- PreviewReady.partition(candidates, allScales)
      _         <- PreviewReady.partition(candidates, rev5)
    yield FakePreviewFacts(candidates, allScales, rev5)

  private def previewDiagnostics: Vector[StudioDiagnostic] =
    study.queries.collect { case q if q.status == NoMatchStatus => unmatched(q) }

  /** Evaluating a fresh stream captures a snapshot before counting begins.
    * Count progress is one participant per page; stopping consumption preserves
    * pages already completed, without promising zero transport read-ahead.
    */
  def previewCounting(
      r: AnalysisRevision,
      budget: PreviewBudget
  ): Stream[F, Either[BackendError, PreviewEvent]] =
    Stream.eval(revision(r)).flatMap {
      case Left(error) => Stream.emit(Left(error))
      case Right(d)    =>
        Stream
          // create refuses a fixture with invalid preview counts, so this
          // fails only for a fake built around it, as a fixture defect.
          .eval(
            previewFacts.fold(
              e =>
                F.raiseError(
                  new IllegalStateException(s"FakeStudyBackend preview: ${e.message}")
                ),
              F.pure
            )
          )
          .flatMap(facts =>
            Stream.eval(state.modify { s =>
              val id = PreviewId(s.previews.keys.map(_.value).maxOption.getOrElse(0L) + 1L)
              val snapshot = prepared(r, d)
              val ready    =
                PreviewReady.of(
                  id,
                  snapshot.stamp,
                  facts.candidates,
                  facts.counts(r),
                  previewDiagnostics
                )
              ready match
                case Left(e)        => (s, Left(e))
                case Right(receipt) =>
                  val retained =
                    RetainedPreview(snapshot, receipt, PreviewProgress.start(facts.candidates))
                  (s.copy(previews = s.previews.updated(id, retained)), Right(retained))
            })
          )
          .flatMap {
            // create refuses a fixture whose counts do not partition.
            case Left(e) =>
              Stream.raiseError[F](
                new IllegalStateException(s"FakeStudyBackend preview: ${e.message}")
              )
            case Right(retained) => page(retained, budget, initial = true).map(Right(_))
          }
    }

  def continuePreview(
      id: PreviewId,
      budget: PreviewBudget
  ): Stream[F, Either[BackendError, PreviewEvent]] =
    Stream.eval(state.get).flatMap { s =>
      s.previews.get(id) match
        case None =>
          Stream.emit(
            Left(BackendError.UnknownPreview(id, s.previews.keys.toVector.sortBy(_.value)))
          )
        case Some(retained) if retained.progress.isComplete =>
          Stream.emit(Right(PreviewEvent.Ready(retained.ready)))
        case Some(retained) => page(retained, budget, initial = false).map(Right(_))
    }

  private def page(
      retained: RetainedPreview,
      budget: PreviewBudget,
      initial: Boolean
  ): Stream[F, PreviewEvent] =
    // `Initial` appears only on creation; resumed pages contain progress or Ready.
    val opening = if initial then
      Stream.emit(
        PreviewEvent.Initial(retained.ready.id, retained.ready.stamp, retained.ready.candidates)
      )
    else Stream.empty
    opening ++ Stream
      .emits(Vector.fill(budget.participants)(()))
      .evalMap { _ =>
        state.modify { s =>
          s.previews.get(retained.ready.id) match
            case Some(current) =>
              current.progress.advance match
                case Some(done) =>
                  val counting = PreviewEvent.Counting(current.ready.id, done)
                  val ready    = Option.when(done.isComplete)(PreviewEvent.Ready(current.ready))
                  (
                    s.copy(previews =
                      s.previews.updated(current.ready.id, current.copy(progress = done))
                    ),
                    Vector(counting) ++ ready.toVector
                  )
                case None => (s, Vector(PreviewEvent.Ready(current.ready)))
            case None => (s, Vector.empty)
        }
      }
      .flatMap(Stream.emits)
      .takeThrough {
        case PreviewEvent.Ready(_) => false
        case _                     => true
      }

  def submitPreview(ready: PreviewReady): F[Either[BackendError, JobStatus]] =
    state.modify { s =>
      s.previews.get(ready.id) match
        case None =>
          (
            s,
            Left(
              BackendError.UnknownPreview(ready.id, s.previews.keys.toVector.sortBy(_.value))
            )
          )
        case Some(retained) if !retained.progress.isComplete =>
          (
            s,
            Left(
              BackendError.PreviewNotReady(
                ready.id,
                retained.progress.completed,
                retained.progress.total
              )
            )
          )
        case Some(retained) if retained.ready != ready =>
          (s, Left(BackendError.TamperedPreview(ready, retained.ready)))
        case Some(retained) =>
          s.revisions.get(retained.ready.stamp.revision) match
            case Some(dataset) if dataset == retained.ready.stamp.dataset =>
              submitSnapshot(s, retained.snapshot)
            case _ =>
              val current = PreviewStamp.fake(
                retained.ready.stamp.revision,
                s.revisions
                  .getOrElse(retained.ready.stamp.revision, retained.ready.stamp.dataset)
              )
              (s, Left(BackendError.StalePreview(ready.id, retained.ready.stamp, current)))
    }

  // -------------------------------------------------------------------------
  // Runs and jobs
  // -------------------------------------------------------------------------

  def runs: F[Vector[RunSummary]] = state.get.map(_.runs)

  def jobs: F[Vector[JobStatus]] = state.get.map(_.jobs)

  def job(id: JobId): F[Either[BackendError, JobStatus]] =
    state.get.map(s => s.job(id).toRight(unknownJob(s, id)))

  private def unknownJob(s: State, id: JobId) = BackendError.UnknownJob(id, s.jobs.map(_.job))

  def submit(r: AnalysisRevision): F[Either[BackendError, JobStatus]] =
    revision(r).flatMap {
      case Left(e)  => F.pure(Left(e))
      case Right(d) =>
        state.modify(s => submitSnapshot(s, prepared(r, d)))
    }

  private def submitSnapshot(
      s: State,
      snapshot: FakePreparedSnapshot
  ): (State, Either[BackendError, JobStatus]) =
    s.jobs.find(j => !isFinished(j.state)) match
      case Some(active) =>
        (s, Left(BackendError.AlreadyRunning(snapshot.stamp.revision, active.job)))
      case None =>
        val job    = JobId(s.jobs.size + 1)
        val run    = RunId(s.runs.map(_.run.number).maxOption.getOrElse(0) + 1)
        val status =
          JobStatus(job, run, snapshot.stamp.revision, snapshot.stamp.dataset, JobState.Queued)
        (
          s.copy(
            runs = s.runs :+ RunSummary(
              run,
              snapshot.stamp.revision,
              snapshot.stamp.dataset,
              RunState.Running(job)
            ),
            jobs = s.jobs :+ status,
            jobSnapshots = s.jobSnapshots.updated(job, snapshot)
          ),
          Right(status)
        )

  def subscribe(id: JobId): F[Either[BackendError, Stream[F, JobEvent]]] =
    state.get.map { s =>
      s.job(id).toRight(unknownJob(s, id)).map { _ =>
        state.discrete
          .map(_.job(id).map(_.state))
          .unNone
          .changes(using Eq.fromUniversalEquals)
          .collect {
            case JobState.Running(p)  => JobEvent.Advanced(p)
            case JobState.Finished(o) => JobEvent.Finished(o)
          }
          .takeThrough {
            case JobEvent.Finished(_) => false
            case JobEvent.Advanced(_) => true
          }
      }
    }

  def outcome(id: JobId): F[Either[BackendError, Option[JobOutcome]]] =
    state.get.map { s =>
      s.job(id)
        .toRight(unknownJob(s, id))
        .map(_.state match
          case JobState.Finished(o) => Some(o)
          case _                    => None)
    }

  def cancel(id: JobId): F[Either[BackendError, JobStatus]] =
    state.modify { s =>
      s.job(id) match
        case None                           => (s, Left(unknownJob(s, id)))
        case Some(j) if isFinished(j.state) => (s, Right(j))
        case Some(j)                        =>
          val last    = progressOf(j.state)
          val outcome = JobOutcome.Cancelled(j.job, j.run, last)
          val next    = s.finish(j, outcome, RunState.Cancelled(last.map(_.segment.kind)))
          (next, next.job(id).toRight(unknownJob(next, id)))
    }

  // -------------------------------------------------------------------------
  // Test control
  // -------------------------------------------------------------------------

  /** The progress of `job` at `done` units of the script's segment `index`. */
  private def progressAt(
      j: JobStatus,
      plan: Vector[ScriptedSegment],
      index: Int,
      done: Long,
      step: Long
  ): Either[FakeControlError, JobProgress] =
    val here                       = plan(index)
    val before                     = plan.take(index)
    def completed(unit: CountUnit) =
      sumOf(before, unit) + (if here.segment.unit == unit then done else 0L)
    (for
      meter  <- StageMeter.of(here.segment.kind, here.segment.unit, done, here.total)
      totals <- RunTotals.of(
        completed(CountUnit.Maps),
        ProgressTotal.Exact(sumOf(plan, CountUnit.Maps)),
        completed(CountUnit.Pairs),
        ProgressTotal.Exact(sumOf(plan, CountUnit.Pairs))
      )
      progress <- JobProgress.of(j.job, j.run, step, here.segment, meter, totals)
    yield progress).leftMap(FakeControlError.Progress(_))

  /** Move to `index`, `done` and hold there; progress never moves back. */
  private def moveTo(
      id: JobId,
      target: (JobStatus, Vector[ScriptedSegment]) => Either[FakeControlError, (Int, Long)]
  ) =
    state.modify { s =>
      val result =
        for
          j             <- active(s, id)
          plan          <- s.snapshot(id).map(_.script)
          (index, done) <- target(j, plan)
          prior = progressOf(j.state)
          _ <- prior.fold(Right(())) { p =>
            val from = plan.indexWhere(_.segment == p.segment)
            if index < from || (index == from && done < p.meter.done) then
              Left(
                FakeControlError
                  .Regressed(id, p.segment, p.meter.done, plan(index).segment, done)
              )
            else Right(())
          }
          progress <- progressAt(j, plan, index, done, prior.fold(1L)(_.step + 1))
        yield (j, progress)
      result match
        case Left(e)              => (s, Left(e))
        case Right((j, progress)) =>
          (s.update(j.copy(state = JobState.Running(progress))), Right(progress))
    }

  /** Move a job to `done` units of `segment` and hold it there. */
  def advanceTo(
      id: JobId,
      segment: Segment,
      done: Long
  ): F[Either[FakeControlError, JobProgress]] =
    moveTo(
      id,
      (_, plan) =>
        val index = plan.indexWhere(_.segment == segment)
        Either.cond(index >= 0, (index, done), FakeControlError.UnscriptedSegment(id, segment))
    )

  /** Move a job to the comparison where `pairs` pairs have been compared over
    * the whole run, and hold it there: the chip's "x / total pairs".
    */
  def advanceToPairs(id: JobId, pairs: Long): F[Either[FakeControlError, JobProgress]] =
    moveTo(
      id,
      (_, plan) =>
        val starts = plan.scanLeft(0L)((acc, s) =>
          acc + (if s.segment.unit == CountUnit.Pairs then full(s) else 0L)
        )
        plan.indices
          .find(i =>
            plan(i).segment.unit == CountUnit.Pairs && pairs > starts(i) &&
              pairs <= starts(i) + full(plan(i))
          )
          .map(i => (i, pairs - starts(i)))
          .toRight(
            FakeControlError
              .PairsOutOfRange(id, pairs, ProgressTotal.Exact(sumOf(plan, CountUnit.Pairs)))
          )
    )

  /** Finish a job successfully: every segment completes and its run becomes
    * `Completed`. The fixture has no scores for the new run.
    */
  def complete(id: JobId): F[Either[FakeControlError, JobOutcome]] =
    state.modify { s =>
      val result =
        for
          j    <- active(s, id)
          plan <- s.snapshot(id).map(_.script)
          last <- progressAt(
            j,
            plan,
            plan.size - 1,
            full(plan.last),
            progressOf(j.state).fold(1L)(_.step + 1)
          )
        yield (j, JobOutcome.Completed(id, j.run, last))
      result match
        case Left(e)             => (s, Left(e))
        case Right((j, outcome)) => (s.finish(j, outcome, RunState.Completed), Right(outcome))
    }

  /** Declare that `revision` stands on `dataset`, as a saved analysis the
    * document now holds (Save & run in a headless journey, S3.6). The real
    * backend learns a revision from its bound plan (S3.7). Declaring a known
    * revision again on another dataset is refused.
    */
  def declare(
      revision: AnalysisRevision,
      dataset: DatasetRevision
  ): F[Either[FakeControlError, Unit]] =
    state.modify { s =>
      s.revisions.get(revision) match
        case Some(d) if d != dataset =>
          (s, Left(FakeControlError.RevisionConflict(revision, d, dataset)))
        case _ => (s.copy(revisions = s.revisions.updated(revision, dataset)), Right(()))
    }

  /** Answer admission and the ledger of `dataset` under `scenario` from now
    * on: a dataset whose trials.csv declares no inventory, or one eyes4s
    * refuses (S5.4). The fixture's inventory is joined otherwise.
    */
  def serveInventory(dataset: DatasetRevision, scenario: InventoryScenario): F[Unit] =
    state.update(s => s.copy(inventories = s.inventories.updated(dataset, scenario)))

  /** Hold `content` for `dataset` from now on, as a backend that stored the
    * revision would: [[verify]] then refuses any other content (S5.6).
    */
  def holdContent(
      dataset: DatasetRevision,
      content: CanonicalDigest[DatasetRevisionSpec]
  ): F[Unit] =
    state.update(s => s.copy(contents = s.contents.updated(dataset, content)))

  /** Hold no content for `dataset` from now on, as a backend whose stored
    * revision is gone: [[verify]] then refuses with ContentNotHeld.
    */
  def forgetContent(dataset: DatasetRevision): F[Unit] =
    state.update(s => s.copy(contents = s.contents - dataset))

  /** Finish a job with diagnostics; its run becomes `Failed`. */
  def fail(
      id: JobId,
      diagnostics: Vector[StudioDiagnostic]
  ): F[Either[FakeControlError, JobOutcome]] =
    state.modify { s =>
      active(s, id) match
        case Left(e)  => (s, Left(e))
        case Right(j) =>
          val outcome = JobOutcome.Failed(id, j.run, diagnostics, progressOf(j.state))
          (s.finish(j, outcome, RunState.Failed), Right(outcome))
    }

  private def active(s: State, id: JobId): Either[FakeControlError, JobStatus] =
    s.job(id) match
      case None => Left(FakeControlError.UnknownJob(id, s.jobs.map(_.job)))
      case Some(j) if isFinished(j.state) => Left(FakeControlError.NotRunning(id, j.state))
      case Some(j)                        => Right(j)

  // -------------------------------------------------------------------------
  // Results, inspection and provenance
  // -------------------------------------------------------------------------

  private def scored(run: RunId): F[Either[BackendError, RunSummary]] =
    state.get.map { s =>
      s.runs.find(_.run == run) match
        case None                     => Left(BackendError.UnknownRun(run, s.runs.map(_.run)))
        case Some(r) if !hasScores(r) => Left(BackendError.NoResult(run, r.state))
        case Some(r)                  => Right(r)
    }

  private def hasScores(r: RunSummary): Boolean =
    r.revision == scoredRevision && r.dataset == servedDataset && (r.state match
      case RunState.Current | RunState.Completed => true
      case _                                     => false)

  /** The provenance chain's down functions over this backend's runs (S3.4). */
  lazy val navigator: StudyNavigator[F] =
    FakeNavigator[F](study, run => scored(run).map(_.void), status)

  def result(run: RunId): F[Either[BackendError, ResultSummary]] =
    scored(run).map(_.map { r =>
      ResultSummary(
        r.run,
        r.revision,
        r.dataset,
        summary.scales,
        summary.pairRowsPerScale,
        summary.pairRowsAllScales,
        summary.eligibleQueries,
        summary.contrasts,
        summary.grandD,
        summary.grandDByScale,
        summary.groups,
        summary.pairedN,
        summary.groupNRange._1,
        summary.groupNRange._2,
        summary.participants
      )
    })

  private def status(q: MockQuery): QueryStatus = q.status match
    case QueryNotAdmitted => QueryStatus.NotAdmitted(dispositionOf(q))
    case NoMatchStatus    => QueryStatus.NoMatch(unmatched(q))
    case FailedStatus     => QueryStatus.Failed(offWindow(q))
    case _                =>
      QueryStatus.Contributing(
        q.m.getOrElse(Vector.empty),
        q.b.getOrElse(Vector.empty),
        q.d.getOrElse(Vector.empty)
      )

  def queries(run: RunId, page: PageRequest): F[Either[BackendError, QueryPage]] =
    scored(run).map(_.map { _ =>
      val rows = slice(study.queries, page).map { q =>
        QueryRow(q.key, q.item, q.response, q.matchedKey, q.controls, status(q))
      }
      QueryPage(run, PageInfo.of(page, study.queries.size, rows.size), rows)
    })

  private def focalOf(address: ResultAddress): TrialKey = address match
    case ResultAddress.Estimation(_, key)      => key
    case ResultAddress.PairRow(_, _, focal, _) => focal
    case ResultAddress.Reduction(_, _, key)    => key
    case ResultAddress.ContrastRow(_, key)     => key

  /** The query an address names, when the scale exists. */
  private def locate(run: RunId, address: ResultAddress): Either[BackendError, MockQuery] =
    byKey
      .get(focalOf(address))
      .filter(_ => summary.scales.indices.contains(address.scale))
      .toRight(BackendError.UnknownReference(run, address))

  private def unavailable(address: ResultAddress) =
    Left(BackendError.Unavailable(DiagnosticLocus.Address(address)))

  def inspect(run: RunId, address: ResultAddress): F[Either[BackendError, Inspection]] =
    scored(run).map(_.flatMap { _ =>
      locate(run, address).flatMap { q =>
        val s = address.scale
        (status(q), address) match
          // The fixture holds no density estimates.
          case (_, ResultAddress.Estimation(_, _)) => unavailable(address)
          case (QueryStatus.Contributing(m, b, d), ResultAddress.ContrastRow(_, _)) =>
            Right(Inspection.Contrast(address, m(s), b(s), d(s)))
          case (
                QueryStatus.Contributing(m, _, _),
                ResultAddress.Reduction(_, PairDesign.Matched, _)
              ) =>
            // FIXTURE.md: every eligible query has exactly one matched reference.
            Right(Inspection.Reduction(address, m(s), 1))
          case (
                QueryStatus.Contributing(_, b, _),
                ResultAddress.Reduction(_, PairDesign.Control, _)
              ) =>
            Right(Inspection.Reduction(address, b(s), q.controls.getOrElse(0)))
          case (
                QueryStatus.Contributing(m, _, _),
                ResultAddress.PairRow(_, PairDesign.Matched, _, ref)
              ) =>
            // The one matched pair is the matched reduction's only member.
            if ref == q.matchedKey then Right(Inspection.Pair(address, q.item, m(s)))
            else Left(BackendError.UnknownReference(run, address))
          case (
                QueryStatus.Contributing(_, _, _),
                ResultAddress.PairRow(_, PairDesign.Control, _, ref)
              ) =>
            // The fixture scores control pairs of P17 ret_07 at 2° only.
            q.controlScores2deg.find(c => MockStudy.key(q.participant, c.trial) == ref) match
              case Some(c) if summary.scales.lift(s).contains(FocusScale) =>
                Right(Inspection.Pair(address, c.item, c.score))
              case _ => unavailable(address)
          case (unscored, _) => Right(Inspection.Unscored(address, unscored))
      }
    })

  /** Protocol 1.9: every compared query's pairs at `scale`, matched first.
    * A contributing query's matched score is its M (its matched reduction's
    * only member); its control scores are the fixture's, which it holds for
    * P17 ret_07 at 2° only, and otherwise not served. A failed query's pairs
    * carry its diagnostic. The rows per scale are the summary's
    * `pairRowsPerScale`.
    */
  def pairRows(
      run: RunId,
      scale: Int,
      page: PageRequest
  ): F[Either[BackendError, PairRowPage]] =
    scored(run).map(_.flatMap { _ =>
      if !summary.scales.indices.contains(scale) then
        Left(BackendError.UnknownScale(run, scale, summary.scales))
      else
        val all = study.queries.flatMap { q =>
          val controls = study.controls(q).getOrElse(Vector.empty)
          status(q) match
            case QueryStatus.Contributing(m, _, _) =>
              val focus = summary.scales.lift(scale).contains(FocusScale)
              PairRowEntry(
                q.key,
                PairDesign.Matched,
                q.matchedKey,
                q.item,
                m.lift(scale).fold(PairScoreState.NotServed)(PairScoreState.Scored(_))
              ) +: controls.map { (key, item) =>
                val score = q.controlScores2deg
                  .find(c => focus && MockStudy.key(q.participant, c.trial) == key)
                  .fold(PairScoreState.NotServed)(c => PairScoreState.Scored(c.score))
                PairRowEntry(q.key, PairDesign.Control, key, item, score)
              }
            case QueryStatus.Failed(d) =>
              PairRowEntry(
                q.key,
                PairDesign.Matched,
                q.matchedKey,
                q.item,
                PairScoreState.Failed(d)
              ) +:
                controls.map((key, item) =>
                  PairRowEntry(q.key, PairDesign.Control, key, item, PairScoreState.Failed(d))
                )
            case _ => Vector.empty
        }
        val rows = slice(all, page)
        Right(PairRowPage(run, scale, PageInfo.of(page, all.size, rows.size), rows))
    })

  def provenance(run: RunId, address: ResultAddress): F[Either[BackendError, Provenance]] =
    scored(run).map(_.flatMap { r =>
      locate(run, address).flatMap { _ =>
        val trial = (k: TrialKey) =>
          ledgerOf
            .get(k)
            .map(e => ProvenanceStep.Trial(k, e.item))
            .toRight(BackendError.UnknownReference(run, address))
        val head = Vector(
          ProvenanceStep.Run(r.run),
          ProvenanceStep.Analysis(r.revision),
          ProvenanceStep.Dataset(r.dataset),
          ProvenanceStep.Scale(address.scale, summary.scales(address.scale))
        )
        val tail = address match
          case ResultAddress.Estimation(_, k)        => trial(k).map(Vector(_))
          case ResultAddress.ContrastRow(_, k)       => trial(k).map(Vector(_))
          case ResultAddress.Reduction(_, design, k) =>
            trial(k).map(t => Vector(ProvenanceStep.Design(design), t))
          case ResultAddress.PairRow(_, design, focal, reference) =>
            (trial(focal), trial(reference)).mapN((f, t) =>
              Vector(ProvenanceStep.Design(design), f, t)
            )
        tail.map(t => Provenance(address, head ++ t))
      }
    })

object FakeStudyBackend:

  /** Rev 5 adds σ 8° (FIXTURE.md "Story moments"). */
  val Rev5Scale: String = "8°"

  /** The scale whose control pair scores fixture.json holds. */
  val FocusScale: String = "2°"

  private val QueryNotAdmitted = "query not admitted"
  private val NoMatchStatus    = "no match"
  private val FailedStatus     = "failed"
  private val NoFixationsSlug  = "no-fixations"

  /** Kept only by the fake backend; no protocol value exposes a snapshot. */
  private[fixture] final case class FakePreparedSnapshot(
      study: MockStudy,
      stamp: RunStamp,
      scales: Vector[String],
      script: Vector[ScriptedSegment]
  )

  /** Kept only by the fake backend; no protocol value exposes a snapshot. */
  private[fixture] final case class RetainedPreview(
      snapshot: FakePreparedSnapshot,
      ready: PreviewReady,
      progress: PreviewProgress
  )

  /** The fixture's validated preview counts: candidates and the counts of an
    * analysis before and after rev 5 adds a scale.
    */
  private[fixture] final case class FakePreviewFacts(
      candidates: PreviewCandidates,
      allScales: PreviewCounts,
      rev5: PreviewCounts
  ):
    def counts(r: AnalysisRevision): PreviewCounts = if r.number >= 5 then rev5 else allScales

  private[fixture] final case class State(
      datasets: Map[DatasetRevision, DatasetState],
      revisions: Map[AnalysisRevision, DatasetRevision],
      runs: Vector[RunSummary],
      jobs: Vector[JobStatus],
      previews: Map[PreviewId, RetainedPreview],
      jobSnapshots: Map[JobId, FakePreparedSnapshot],
      inventories: Map[DatasetRevision, InventoryScenario] = Map.empty,
      contents: Map[DatasetRevision, CanonicalDigest[DatasetRevisionSpec]] = Map.empty
  ):
    def job(id: JobId): Option[JobStatus] = jobs.find(_.job == id)

    def snapshot(id: JobId): Either[FakeControlError, FakePreparedSnapshot] =
      jobSnapshots.get(id).toRight(FakeControlError.MissingSnapshot(id))

    def update(j: JobStatus): State =
      copy(jobs = jobs.map(o => if o.job == j.job then j else o))

    def finish(j: JobStatus, outcome: JobOutcome, run: RunState): State =
      update(j.copy(state = JobState.Finished(outcome))).copy(runs = runs.map { r =>
        if r.run == j.run then r.copy(state = run) else r
      })

  private def isFinished(s: JobState): Boolean = s match
    case JobState.Finished(_) => true
    case _                    => false

  private def progressOf(s: JobState): Option[JobProgress] = s match
    case JobState.Running(p)  => Some(p)
    case JobState.Finished(o) => o.progress
    case JobState.Queued      => None

  private def slice[A](all: Vector[A], page: PageRequest): Vector[A] =
    all.slice(page.offset, page.offset + page.size)

  private val r2   = DatasetRevision(2)
  private val r3   = DatasetRevision(3)
  private val rev3 = AnalysisRevision(3)
  private val rev4 = AnalysisRevision(4)
  private val rev5 = AnalysisRevision(5)

  private def initial(moment: StoryMoment): State = moment match
    case StoryMoment.T1 =>
      State(
        Map(r2   -> DatasetState.Admitted, r3 -> DatasetState.Draft),
        Map(rev3 -> r2),
        Vector(RunSummary(RunId(5), rev3, r2, RunState.Current)),
        Vector.empty,
        Map.empty,
        Map.empty
      )
    case StoryMoment.T2 | StoryMoment.T3 =>
      State(
        Map(r2   -> DatasetState.Admitted, r3 -> DatasetState.Admitted),
        Map(rev3 -> r2, rev4                  -> r3, rev5 -> r3),
        Vector(
          RunSummary(RunId(5), rev3, r2, RunState.Stale),
          RunSummary(RunId(6), rev4, r3, RunState.Cancelled(Some(StageKind.Comparing))),
          RunSummary(RunId(7), rev4, r3, RunState.Current)
        ),
        Vector.empty,
        Map.empty,
        Map.empty
      )

  /** The fake at one story moment. The effect fails only if the embedded
    * fixture does not decode, which is a build defect.
    */
  def create[F[_]](moment: StoryMoment)(using F: Concurrent[F]): F[FakeStudyBackend[F]] =
    def defect[A](message: String): F[A] =
      F.raiseError(new IllegalStateException(s"FakeStudyBackend at $moment: $message"))
    // It holds each story revision's own content (S5.6): a verification of
    // other content is refused, as a backend that stored the revisions would.
    val held = StorySeed
      .document(moment)
      .flatMap(
        _.datasets.traverse(s =>
          DatasetRevisionSpec.contentDigest(s).left.map(_.message).map(s.id -> _)
        )
      )
      .map(_.toMap)
    for
      study    <- MockStudy.load.fold(defect, F.pure)
      contents <- held.fold(defect, F.pure)
      state    <- SignallingRef.of[F, State](initial(moment).copy(contents = contents))
      backend = new FakeStudyBackend[F](study, moment, state)
      _ <- backend.previewFacts.fold(e => defect(e.message), _ => F.unit)
      _ <- moment match
        case StoryMoment.T3 =>
          backend.submit(rev5).flatMap {
            case Left(e)  => defect(e.message)
            case Right(j) =>
              backend
                .advanceToPairs(j.job, StoryMoment.RunningPairs)
                .flatMap(_.fold(e => defect(e.message), _ => F.unit))
          }
        case _ => F.unit
    yield backend
