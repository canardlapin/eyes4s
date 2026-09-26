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
import eyes4s.studio.core.backend.*
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
    * pairs" (DESIGN_SPEC section 12).
    */
  val RunningPairs: Long = 21400L

/** Why a test's command to a fake job was refused. */
enum FakeControlError derives CanEqual:
  case UnknownJob(job: JobId, known: Vector[JobId])
  case NotRunning(job: JobId, state: JobState)
  case NotStarted(job: JobId)
  case UnscriptedStage(job: JobId, stage: JobStage, script: Vector[JobStage])
  case StageRegressed(job: JobId, from: JobStage, to: JobStage)
  case ProgressRegressed(job: JobId, stage: JobStage, from: Long, to: Long)
  case Progress(underlying: ProgressError)

  def message: String = this match
    case UnknownJob(job, known) =>
      s"No job ${job.number}; the fake has ${known.map(_.number).mkString(", ")}."
    case NotRunning(job, state) => s"Job ${job.number} is not running (state $state)."
    case NotStarted(job)        => s"Job ${job.number} has reported no progress yet."
    case UnscriptedStage(job, stage, script) =>
      s"Job ${job.number} has no stage $stage; its script is ${script.mkString(", ")}."
    case StageRegressed(job, from, to) =>
      s"Job ${job.number} cannot move back from $from to $to."
    case ProgressRegressed(job, stage, from, to) =>
      s"Job ${job.number} cannot move $stage progress back from $from to $to."
    case Progress(underlying) => underlying.message

/** A test double of [[StudyBackend]] that serves docs/studio/fixture/fixture.json
  * exactly, and the inventory of fixtures/studio-golden as the ledger.
  *
  * Jobs never advance by themselves. A test moves one with [[advanceTo]] and
  * ends it with [[complete]], [[fail]] or `cancel`, so any step of a run can be
  * held deterministically.
  *
  * The fake computes nothing: every number it serves is a value of the
  * fixture. Only run 7 (rev 4, data r3) has scores; the fixture has no numbers
  * for data r2 or for a completed rev 5.
  */
final class FakeStudyBackend[F[_]] private (
    val study: MockStudy,
    state: SignallingRef[F, FakeStudyBackend.State]
)(using F: Concurrent[F])
    extends StudyBackend[F]:
  import FakeStudyBackend.*

  private val summary = study.summary

  /** The dataset every fixture number describes. */
  private val servedDataset = DatasetRevision(3)

  /** The run whose scores fixture.json holds. */
  private val scoredRun = RunId(7)

  private val byKey: Map[TrialKey, MockQuery] = study.queries.map(q => q.key -> q).toMap
  private val items: Map[TrialKey, String] = study.inventory.map(e => e.trial -> e.item).toMap

  private def scalesOf(revision: AnalysisRevision): Vector[String] =
    if revision.number >= 5 then summary.scales :+ Rev5Scale else summary.scales

  private def pairRowsOf(revision: AnalysisRevision): Long =
    if revision.number >= 5 then summary.pairRowsRev5 else summary.pairRowsAllScales

  /** Each job's stages and their stated totals. */
  def script(revision: AnalysisRevision): Vector[(JobStage, ProgressTotal)] = Vector(
    JobStage.Estimating  -> ProgressTotal.Unknown,
    JobStage.Comparing   -> ProgressTotal.Exact(pairRowsOf(revision)),
    JobStage.Reducing    -> ProgressTotal.Unknown,
    JobStage.Contrasting -> ProgressTotal.AtMost(summary.eligibleQueries.toLong)
  )

  // -------------------------------------------------------------------------
  // Admission and the ledger
  // -------------------------------------------------------------------------

  private def dataset(d: DatasetRevision): F[Either[BackendError, DatasetState]] =
    state.get.map { s =>
      s.datasets.get(d) match
        case None =>
          Left(BackendError.UnknownDataset(d, s.datasets.keys.toVector.sortBy(_.number)))
        case Some(_) if d != servedDataset =>
          Left(BackendError.Unavailable(d.label, "the fixture describes dataset r3 only"))
        case Some(st) => Right(st)
    }

  def admission(d: DatasetRevision): F[Either[BackendError, AdmissionSummary]] =
    dataset(d).map(_.map { st =>
      AdmissionSummary(
        d,
        st,
        summary.inventoryTrials,
        summary.admitted,
        summary.quarantineByCause,
        summary.absent,
        summary.fixationRecords,
        summary.outsideWindowRecords,
        summary.outsideWindowTrials,
        summary.itemsInPool,
        summary.imagesFound,
        summary.missingImages,
        summary.datasetHistory.getOrElse(d.label, "")
      )
    })

  def ledger(d: DatasetRevision, page: PageRequest): F[Either[BackendError, LedgerPage]] =
    dataset(d).map(_.map { _ =>
      val entries = slice(study.inventory, page)
      LedgerPage(d, PageInfo.of(page, study.inventory.size, entries.size), entries)
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
          Left(
            BackendError.Unavailable(r.label, s"it resolves on ${d.label}, not in the fixture")
          )
        case Some(d) => Right(d)
    }

  def preview(r: AnalysisRevision): F[Either[BackendError, PreviewSummary]] =
    revision(r).map(_.map { d =>
      val scales = scalesOf(r)
      PreviewSummary(
        r,
        d,
        scales,
        focalTrials = study.queries.size,
        referenceTrials = study.inventory.count(_.trial.phase == Phase.Encoding),
        requestedQueries = summary.contrasts.requested,
        eligibleQueries = summary.eligibleQueries,
        candidatePairsPerScale = summary.candidatePairsPerScale,
        pairRowsPerScale = summary.pairRowsPerScale,
        pairRows = pairRowsOf(r)
      )
    })

  private def eligibility(q: MockQuery): Eligibility = q.status match
    case QueryNotAdmitted => Eligibility.QueryNotAdmitted(q.reason.getOrElse(""))
    case NoMatchStatus    => Eligibility.NoMatch(q.reason.getOrElse(""))
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
        state.modify { s =>
          s.jobs.find(j => !isFinished(j.state)) match
            case Some(active) => (s, Left(BackendError.AlreadyRunning(r, active.job)))
            case None         =>
              val job    = JobId(s.jobs.size + 1)
              val run    = RunId(s.runs.map(_.run.number).maxOption.getOrElse(0) + 1)
              val status = JobStatus(job, run, r, d, JobState.Queued)
              val next   = s.copy(
                runs = s.runs :+ RunSummary(run, r, d, RunState.Running(job)),
                jobs = s.jobs :+ status
              )
              (next, Right(status))
        }
    }

  def events(id: JobId): F[Either[BackendError, Stream[F, JobEvent]]] =
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

  def outcome(id: JobId): F[Either[BackendError, JobOutcome]] =
    state.get.flatMap { s =>
      s.job(id) match
        case None    => F.pure(Left(unknownJob(s, id)))
        case Some(_) =>
          state.discrete
            .map(_.job(id).map(_.state))
            .collectFirst { case Some(JobState.Finished(o)) => o }
            .compile
            .lastOrError
            .map(Right(_))
    }

  def cancel(id: JobId): F[Either[BackendError, JobOutcome]] =
    state.modify { s =>
      s.job(id) match
        case None                                              => (s, Left(unknownJob(s, id)))
        case Some(JobStatus(_, _, _, _, JobState.Finished(o))) => (s, Right(o))
        case Some(j)                                           =>
          val last    = progressOf(j.state)
          val outcome = JobOutcome.Cancelled(j.job, j.run, last)
          (s.finish(j, outcome, RunState.Cancelled(last.map(_.stage))), Right(outcome))
    }

  // -------------------------------------------------------------------------
  // Test control
  // -------------------------------------------------------------------------

  /** Move a job to `done` units of `stage` and hold it there. Stages follow
    * [[script]]; neither the stage nor its progress may move back.
    */
  def advanceTo(
      id: JobId,
      stage: JobStage,
      done: Long
  ): F[Either[FakeControlError, JobProgress]] =
    state.modify { s =>
      val result =
        for
          j <- active(s, id)
          plan = script(j.revision)
          total <- plan
            .collectFirst { case (`stage`, t) => t }
            .toRight(FakeControlError.UnscriptedStage(id, stage, plan.map(_._1)))
          prior = progressOf(j.state)
          _ <- prior.fold(Right(())) { p =>
            val from = plan.indexWhere(_._1 == p.stage)
            val to   = plan.indexWhere(_._1 == stage)
            if to < from then Left(FakeControlError.StageRegressed(id, p.stage, stage))
            else if to == from && done < p.done then
              Left(FakeControlError.ProgressRegressed(id, stage, p.done, done))
            else Right(())
          }
          step = prior.fold(1L)(_.step + 1)
          progress <- JobProgress
            .of(id, j.run, step, stage, done, total)
            .leftMap(FakeControlError.Progress(_))
        yield (j, progress)
      result match
        case Left(e)              => (s, Left(e))
        case Right((j, progress)) =>
          (s.update(j.copy(state = JobState.Running(progress))), Right(progress))
    }

  /** Finish a job successfully: its last stage completes and its run becomes
    * `Completed`. The fixture has no scores for the new run.
    */
  def complete(id: JobId): F[Either[FakeControlError, JobOutcome]] =
    state.modify { s =>
      val result =
        for
          j <- active(s, id)
          p <- progressOf(j.state).toRight(FakeControlError.NotStarted(id))
          (stage, total) = script(j.revision).last
          last <- JobProgress
            .of(id, j.run, p.step + 1, stage, total.bound.getOrElse(0L), total)
            .leftMap(FakeControlError.Progress(_))
        yield (j, JobOutcome.Completed(id, j.run, last))
      result match
        case Left(e)             => (s, Left(e))
        case Right((j, outcome)) => (s.finish(j, outcome, RunState.Completed), Right(outcome))
    }

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
        case None => Left(BackendError.UnknownRun(run, s.runs.map(_.run)))
        case Some(r) if r.run != scoredRun => Left(BackendError.NoResult(run, r.state))
        case Some(r)                       => Right(r)
    }

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
        summary.remembered,
        summary.forgotten,
        summary.pairedN,
        summary.groupNRange._1,
        summary.groupNRange._2,
        summary.participants
      )
    })

  private def status(q: MockQuery): QueryStatus = q.status match
    case QueryNotAdmitted => QueryStatus.NotAdmitted(q.reason.getOrElse(""))
    case NoMatchStatus    => QueryStatus.NoMatch(q.reason.getOrElse(""))
    case FailedStatus     => QueryStatus.Failed(q.reason.getOrElse(""))
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

  private def unavailable(address: ResultAddress, reason: String) =
    Left(BackendError.Unavailable(address.toString, reason))

  def inspect(run: RunId, address: ResultAddress): F[Either[BackendError, Inspection]] =
    scored(run).map(_.flatMap { _ =>
      locate(run, address).flatMap { q =>
        val s = address.scale
        (status(q), address) match
          case (_, ResultAddress.Estimation(_, _)) =>
            unavailable(address, "the fixture holds no density estimates")
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
            q.controlScores2deg.find(c => MockStudy.key(q.participant, c.trial) == ref) match
              case Some(c) if summary.scales.lift(s).contains(FocusScale) =>
                Right(Inspection.Pair(address, c.item, c.score))
              case _ =>
                unavailable(
                  address,
                  "the fixture scores control pairs of P17 ret_07 at 2° only"
                )
          case (unscored, _) => Right(Inspection.Unscored(address, unscored))
      }
    })

  def provenance(run: RunId, address: ResultAddress): F[Either[BackendError, Provenance]] =
    scored(run).map(_.flatMap { r =>
      locate(run, address).flatMap { _ =>
        val trial = (k: TrialKey) =>
          items
            .get(k)
            .map(ProvenanceStep.Trial(k, _))
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

  private[fixture] final case class State(
      datasets: Map[DatasetRevision, DatasetState],
      revisions: Map[AnalysisRevision, DatasetRevision],
      runs: Vector[RunSummary],
      jobs: Vector[JobStatus]
  ):
    def job(id: JobId): Option[JobStatus] = jobs.find(_.job == id)

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
        Vector.empty
      )
    case StoryMoment.T2 | StoryMoment.T3 =>
      State(
        Map(r2   -> DatasetState.Admitted, r3 -> DatasetState.Admitted),
        Map(rev3 -> r2, rev4                  -> r3, rev5 -> r3),
        Vector(
          RunSummary(RunId(5), rev3, r2, RunState.Stale),
          RunSummary(RunId(6), rev4, r3, RunState.Cancelled(Some(JobStage.Comparing))),
          RunSummary(RunId(7), rev4, r3, RunState.Current)
        ),
        Vector.empty
      )

  /** The fake at one story moment. The effect fails only if the embedded
    * fixture does not decode, which is a build defect.
    */
  def create[F[_]](moment: StoryMoment)(using F: Concurrent[F]): F[FakeStudyBackend[F]] =
    def defect[A](message: String): F[A] =
      F.raiseError(new IllegalStateException(s"FakeStudyBackend at $moment: $message"))
    for
      study <- MockStudy.load.fold(defect, F.pure)
      state <- SignallingRef.of[F, State](initial(moment))
      backend = new FakeStudyBackend[F](study, state)
      _ <- moment match
        case StoryMoment.T3 =>
          backend.submit(rev5).flatMap {
            case Left(e)  => defect(e.message)
            case Right(j) =>
              backend
                .advanceTo(j.job, JobStage.Comparing, StoryMoment.RunningPairs)
                .flatMap(_.fold(e => defect(e.message), _ => F.unit))
          }
        case _ => F.unit
    yield backend
