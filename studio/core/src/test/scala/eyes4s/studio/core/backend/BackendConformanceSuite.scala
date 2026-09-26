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

import cats.effect.IO
import cats.syntax.all.*
import io.circe.syntax.*
import munit.CatsEffectSuite

/** The contract every [[StudyBackend]] meets on the studio acceptance study
  * (FIXTURE.md), whatever computes it: `FakeStudyBackend` now, the real eyes4s
  * backend in S3.7, and each transport of either.
  *
  * Counts are the study design's and hold on every backend. Scores differ
  * between backends (fixture.json on the fake, SCORES.json on the real one),
  * so this suite checks only their consistency across methods.
  */
abstract class BackendConformanceSuite extends CatsEffectSuite:
  import BackendConformanceSuite.*

  /** A fresh backend at story moment t2: data admitted, a current run and a
    * ready draft revision, no jobs.
    */
  def subject: IO[Subject]

  private def ok[A](fa: IO[Either[BackendError, A]]): IO[A] =
    fa.flatMap(e => IO.fromEither(e.leftMap(err => new AssertionError(err.message))))

  private def page(offset: Int, size: Int): PageRequest =
    PageRequest.of(offset, size).fold(m => throw new AssertionError(m), identity)

  /** Every page of a listing, following `next`. */
  private def all[A](size: Int)(get: PageRequest => IO[(PageInfo, Vector[A])]): IO[Vector[A]] =
    def go(offset: Int, acc: Vector[A]): IO[Vector[A]] =
      get(page(offset, size)).flatMap { case (info, items) =>
        assertEquals(info.offset, offset)
        assert(items.size <= size, s"page at $offset has ${items.size} > $size entries")
        info.next match
          case Some(n) => assertEquals(n, offset + items.size); go(n, acc ++ items)
          case None    => IO.pure(acc ++ items)
      }
    go(0, Vector.empty)

  test("admission reports the FIXTURE.md inventory") {
    subject.flatMap(s => ok(s.backend.admission(s.dataset))).map { a =>
      assertEquals(a.state, DatasetState.Admitted)
      assertEquals(a.inventoryTrials, 960)
      assertEquals(a.admitted, 937)
      assertEquals(a.quarantinedTrials, 17)
      assertEquals(a.absent, 6)
      assertEquals(
        a.quarantined.map(q => q.cause -> q.trials).toMap,
        Map(
          QuarantineCause.DuplicateOrdinals -> 4,
          QuarantineCause.NoFixations       -> 5,
          QuarantineCause.Overlap           -> 6,
          QuarantineCause.RejectedRecords   -> 2
        )
      )
      assertEquals(a.fixationRecords, 11520)
      assertEquals((a.items, a.imagesFound), (259, 257))
      assertEquals(a.missingImages.map(_.item).sorted, Vector("forest-044", "kitchen-081"))
      assertEquals((a.outsideWindowRecords, a.outsideWindowTrials), (543, 409))
    }
  }

  test("the ledger pages through every inventory trial once and agrees with admission") {
    for
      s       <- subject
      summary <- ok(s.backend.admission(s.dataset))
      entries <- all(97)(p => ok(s.backend.ledger(s.dataset, p)).map(l => (l.page, l.entries)))
    yield
      assertEquals(entries.size, summary.inventoryTrials)
      assertEquals(entries.map(_.trial).distinct.size, entries.size)
      val dispositions = entries.groupMapReduce(_.disposition)(_ => 1)(_ + _)
      assertEquals(dispositions.getOrElse(TrialDisposition.Admitted, 0), summary.admitted)
      assertEquals(dispositions.getOrElse(TrialDisposition.Absent, 0), summary.absent)
      summary.quarantined.foreach { q =>
        assertEquals(dispositions.getOrElse(TrialDisposition.Quarantined(q.cause), 0), q.trials)
      }
  }

  test("the preview's counts agree with its rows") {
    for
      s       <- subject
      preview <- ok(s.backend.preview(s.draft))
      rows    <- all(128)(p => ok(s.backend.previewRows(s.draft, p)).map(r => (r.page, r.rows)))
    yield
      assertEquals(preview.requestedQueries, 480)
      assertEquals(preview.eligibleQueries, 457)
      assertEquals(preview.candidatePairsPerScale, 230400L)
      assertEquals(rows.size, preview.requestedQueries)
      assertEquals(rows.count(_.eligibility == Eligibility.Eligible), preview.eligibleQueries)
      assertEquals(preview.pairRows, preview.pairRowsPerScale * preview.scales.size)
      assert(rows.forall(_.query.phase == Phase.Retrieval))
  }

  test("the current run's result agrees with its query rows") {
    for
      s      <- subject
      result <- ok(s.backend.result(s.current))
      rows   <- all(100)(p => ok(s.backend.queries(s.current, p)).map(q => (q.page, q.rows)))
    yield
      val c = result.contrasts
      assertEquals(c, QueryContrasts(480, 14, 9, 3, 454))
      assertEquals(result.eligibleQueries, 457)
      assertEquals(result.pairRows, result.pairRowsPerScale * result.scales.size)
      assertEquals(result.participants.size, 24)
      val tally = QueryContrasts(
        requested = rows.size,
        queryNotAdmitted = rows.count(_.status.isInstanceOf[QueryStatus.NotAdmitted]),
        noMatch = rows.count(_.status.isInstanceOf[QueryStatus.NoMatch]),
        failed = rows.count(_.status.isInstanceOf[QueryStatus.Failed]),
        contributing = rows.count(_.status.isInstanceOf[QueryStatus.Contributing])
      )
      assertEquals(tally, c)
      result.participants.foreach { p =>
        val mine = rows.filter(_.query.participant == p.participant)
        assertEquals(mine.size, p.requested, p.participant)
        assertEquals(
          mine.count(_.status.isInstanceOf[QueryStatus.Contributing]),
          p.contributing,
          p.participant
        )
      }
  }

  test("inspecting a contrast row returns the row's scores; unscored queries say why") {
    for
      s       <- subject
      rows    <- ok(s.backend.queries(s.current, page(0, 60))).map(_.rows)
      checked <- rows.traverse { row =>
        val address = ResultAddress.ContrastRow(0, row.query)
        ok(s.backend.inspect(s.current, address)).map { inspection =>
          row.status match
            case QueryStatus.Contributing(m, b, d) =>
              assertEquals(inspection, Inspection.Contrast(address, m(0), b(0), d(0)))
            case other => assertEquals(inspection, Inspection.Unscored(address, other))
        }
      }
    yield assertEquals(checked.size, 60)
  }

  test("provenance runs from the run to the pair's two trials") {
    for
      s   <- subject
      row <- ok(s.backend.queries(s.current, page(0, 1))).map(_.rows.head)
      address = ResultAddress.PairRow(0, PairDesign.Matched, row.query, row.matched)
      trail <- ok(s.backend.provenance(s.current, address)).map(_.trail)
    yield
      assertEquals(trail.take(1), Vector(ProvenanceStep.Run(s.current)))
      assert(trail.exists(_.isInstanceOf[ProvenanceStep.Analysis]), trail)
      assert(trail.exists(_ == ProvenanceStep.Design(PairDesign.Matched)), trail)
      assertEquals(
        trail.collect { case ProvenanceStep.Trial(k, _) => k },
        Vector(row.query, row.matched)
      )
  }

  test("refusals are values with stable codes") {
    for
      s    <- subject
      run  <- s.backend.result(RunId(9999))
      job  <- s.backend.outcome(JobId(9999))
      rev  <- s.backend.preview(AnalysisRevision(9999))
      data <- s.backend.admission(DatasetRevision(9999))
    yield
      assertEquals(run.left.map(_.code), Left("studio-backend.unknown-run"))
      assertEquals(job.left.map(_.code), Left("studio-backend.unknown-job"))
      assertEquals(rev.left.map(_.code), Left("studio-backend.unknown-revision"))
      assertEquals(data.left.map(_.code), Left("studio-backend.unknown-dataset"))
  }

  test("a submitted job reports monotone progress and ends with exactly one Finished") {
    for
      s      <- subject
      status <- ok(s.backend.submit(s.draft))
      stream <- ok(s.backend.events(status.job))
      fiber  <- stream.compile.toVector.start
      _      <- s.finish(status.job)
      events <- fiber.joinWithNever
      out    <- ok(s.backend.outcome(status.job))
      runs   <- s.backend.runs
    yield
      assertEquals(events.lastOption, Some(JobEvent.Finished(out)))
      assertEquals(events.count(_.isInstanceOf[JobEvent.Finished]), 1)
      val progress = events.collect { case JobEvent.Advanced(p) => p } ++ out.progress
      progress.foreach(p => assert(p.total.bound.forall(p.done <= _), p))
      assertEquals(progress.map(_.step), progress.map(_.step).sorted)
      assert(out.isInstanceOf[JobOutcome.Completed], out)
      assert(runs.exists(r => r.run == status.run && r.revision == s.draft), runs)
  }

  test("cancelling a job settles it Cancelled, once") {
    for
      s      <- subject
      status <- ok(s.backend.submit(s.draft))
      first  <- ok(s.backend.cancel(status.job))
      again  <- ok(s.backend.cancel(status.job))
      out    <- ok(s.backend.outcome(status.job))
      after  <- ok(s.backend.job(status.job))
      runs   <- s.backend.runs
    yield
      assert(first.isInstanceOf[JobOutcome.Cancelled], first)
      assertEquals(again, first)
      assertEquals(out, first)
      assertEquals(after.state, JobState.Finished(first))
      assert(
        runs.exists(r => r.run == status.run && r.state.isInstanceOf[RunState.Cancelled]),
        runs
      )
      assert(runs.exists(r => r.run == s.current && r.state == RunState.Current), runs)
  }

  test("the JSON transport answers exactly as the backend does in process") {
    for
      s   <- subject
      row <- ok(s.backend.queries(s.current, page(0, 1))).map(_.rows.head)
      requests = Vector(
        BackendRequest.Admission(s.dataset),
        BackendRequest.Ledger(s.dataset, page(900, 100)),
        BackendRequest.Preview(s.draft),
        BackendRequest.PreviewRows(s.draft, page(0, 5)),
        BackendRequest.Runs,
        BackendRequest.Jobs,
        BackendRequest.Result(s.current),
        BackendRequest.Queries(s.current, page(10, 10)),
        BackendRequest.Inspect(s.current, ResultAddress.ContrastRow(1, row.query)),
        BackendRequest.ProvenanceOf(s.current, ResultAddress.ContrastRow(1, row.query)),
        BackendRequest.Result(RunId(9999))
      )
      direct <- requests.traverse(StudyBackend.serve(s.backend))
      wired  <- requests.traverse { r =>
        IO.fromEither(r.asJson.as[BackendRequest])
          .flatMap(StudyBackend.serve(s.backend))
          .flatMap(a =>
            IO.fromEither(io.circe.parser.decode[BackendResponse](a.asJson.noSpaces))
          )
      }
    yield
      assertEquals(wired, direct)
      assertEquals(direct.count(_.isInstanceOf[BackendResponse.Refused]), 1)
  }

object BackendConformanceSuite:

  /** A backend under test and what the suite needs to know about it.
    *
    * @param finish drives a submitted job to successful completion: a
    *   test-controlled backend steps it; a real backend waits for it.
    */
  final case class Subject(
      backend: StudyBackend[IO],
      dataset: DatasetRevision,
      current: RunId,
      draft: AnalysisRevision,
      finish: JobId => IO[Unit]
  )
