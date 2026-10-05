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
    PageRequest.of(offset, size).fold(e => throw new AssertionError(e.message), identity)

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

  test("admission reports the FIXTURE.md inventory and window totals") {
    subject.flatMap(s => ok(s.backend.admission(s.dataset))).map { a =>
      assertEquals(a.state, DatasetState.Admitted)
      assertEquals((a.inventoryTrials, a.admitted, a.absent), (Some(960), 937, Some(6)))
      // FIXTURE.md counts no-fixations among its 17 quarantined trials.
      assertEquals(a.quarantinedTrials + a.noFixations, 17)
      assertEquals(
        a.quarantined.map(q => q.code -> q.trials).toMap,
        Map(
          "quarantine.duplicate-ordinals" -> 4,
          "quarantine.overlap"            -> 6,
          "quarantine.rejected-records"   -> 2
        )
      )
      assertEquals(a.noFixations, 5)
      assertEquals(a.fixationRecords, 11520)
      assertEquals((a.items, a.imagesFound), (259, 257))
      assertEquals(a.missingImages.map(_.item).sorted, Vector("forest-044", "kitchen-081"))
      val w = a.window
      assertEquals((w.outsideWindow, w.trialsOutsideWindow), (543, 409))
      assertEquals((w.outsideScreen, w.trialsOutsideScreen, w.outsideScreenMicros), (0, 0, 0L))
      assertEquals((w.trials, w.untallied, w.sourceRecords), (937, 0, Some(11520)))
      assert(w.outsideWindowMicros > 0L && w.outsideWindowMicros < w.totalMicros, w)
    }
  }

  test("the ledger pages through every inventory trial once and agrees with admission") {
    for
      s       <- subject
      summary <- ok(s.backend.admission(s.dataset))
      entries <- all(97)(p => ok(s.backend.ledger(s.dataset, p)).map(l => (l.page, l.entries)))
    yield
      assertEquals(Some(entries.size), summary.inventoryTrials)
      assertEquals(entries.map(_.trial).distinct.size, entries.size)
      def count(p: TrialDisposition => Boolean) = entries.count(e => p(e.disposition))
      assertEquals(count(_ == TrialDisposition.Admitted), summary.admitted)
      assertEquals(Some(count(_ == TrialDisposition.Absent)), summary.absent)
      assertEquals(count(_ == TrialDisposition.NoFixations), summary.noFixations)
      val byCode = entries
        .collect { case LedgerEntry(_, _, _, TrialDisposition.Quarantined(c), _) => c.code }
        .groupMapReduce(identity)(_ => 1)(_ + _)
      assertEquals(byCode, summary.quarantined.map(q => q.code -> q.trials).toMap)
      assertEquals(entries.map(_.outsideFrame.size).sum, summary.window.outsideScreen)
  }

  test("the preview's counts agree with its rows") {
    for
      s       <- subject
      preview <- ok(s.backend.preview(s.draft))
      rows    <- all(128)(p => ok(s.backend.previewRows(s.draft, p)).map(r => (r.page, r.rows)))
    yield
      assertEquals(preview.requestedQueries, 480)
      assertEquals(preview.eligibleQueries, 457)
      assertEquals(preview.candidatePairsPerScale, 219486L)
      assertEquals(rows.size, preview.requestedQueries)
      assertEquals(rows.count(_.eligibility == Eligibility.Eligible), preview.eligibleQueries)
      assertEquals(preview.pairRows, preview.pairRowsPerScale * preview.scales.size)
      assert(rows.forall(_.query.phase == Phase.Retrieval))
      rows.foreach { row =>
        row.eligibility match
          case Eligibility.NoMatch(d) =>
            assertEquals(d.code, "study-finding.unmatched-focal")
            assert(d.subject.contains(DiagnosticLocus.Trial(row.query)), d)
          case Eligibility.QueryNotAdmitted(disposition) =>
            assertNotEquals(disposition, TrialDisposition.Admitted)
          case Eligibility.Eligible => ()
      }
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
      // Groups are labelled by the values the inventory attribute takes.
      assertEquals(result.groups.map(_.label).toSet, rows.map(_.response).toSet)
      result.participants.foreach { p =>
        val mine = rows.filter(_.query.participant == p.participant)
        assertEquals(mine.size, p.requested, p.participant)
        assertEquals(
          mine.count(_.status.isInstanceOf[QueryStatus.Contributing]),
          p.contributing,
          p.participant
        )
        assertEquals(p.groups.map(_.label), result.groups.map(_.label), p.participant)
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
      assert(trail.contains(ProvenanceStep.Design(PairDesign.Matched)), trail)
      assertEquals(
        trail.collect { case ProvenanceStep.Trial(k, _) => k },
        Vector(row.query, row.matched)
      )
  }

  test("refusals are values with stable codes and typed subjects") {
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
      assertEquals(
        run.left.map(_.diagnostic.subject),
        Left(Vector(DiagnosticLocus.Run(RunId(9999))))
      )
  }

  test("a subscription reports run-monotone progress and ends with exactly one Finished") {
    for
      s      <- subject
      status <- ok(s.backend.submit(s.draft))
      before <- ok(s.backend.outcome(status.job))
      stream <- ok(s.backend.subscribe(status.job))
      fiber  <- stream.compile.toVector.start
      _      <- s.finish(status.job)
      events <- fiber.joinWithNever
      out    <- ok(s.backend.outcome(status.job))
      runs   <- s.backend.runs
    yield
      assertEquals(before, None)
      assertEquals(events.lastOption, out.map(JobEvent.Finished(_)))
      assertEquals(events.count(_.isInstanceOf[JobEvent.Finished]), 1)
      val progress =
        events.collect { case JobEvent.Advanced(p) => p } ++ out.flatMap(_.progress)
      progress.foreach { p =>
        assertEquals(p.meter.kind, p.segment.kind)
        assertEquals(p.meter.unit, p.segment.unit)
      }
      val pairs = progress.map(_.totals.completedPairs)
      assertEquals(pairs, pairs.sorted)
      assertEquals(
        progress.map(_.totals.completedMaps),
        progress.map(_.totals.completedMaps).sorted
      )
      assertEquals(progress.map(_.step), progress.map(_.step).sorted)
      assert(out.exists(_.isInstanceOf[JobOutcome.Completed]), out)
      assert(runs.exists(r => r.run == status.run && r.revision == s.draft), runs)
  }

  test("cancelling a job settles it Cancelled, once, and the current run stays current") {
    for
      s      <- subject
      status <- ok(s.backend.submit(s.draft))
      first  <- ok(s.backend.cancel(status.job))
      again  <- ok(s.backend.cancel(status.job))
      out    <- ok(s.backend.outcome(status.job))
      runs   <- s.backend.runs
    yield
      assert(out.exists(_.isInstanceOf[JobOutcome.Cancelled]), out)
      assertEquals(first.state, JobState.Finished(out.get))
      assertEquals(again, first)
      assert(
        runs.exists(r => r.run == status.run && r.state.isInstanceOf[RunState.Cancelled]),
        runs
      )
      assert(runs.exists(r => r.run == s.current && r.state == RunState.Current), runs)
  }

  test("a run's pair rows page through every pair at a scale; an unknown scale is refused") {
    for
      s       <- subject
      summary <- ok(s.backend.result(s.current))
      first   <- ok(s.backend.pairRows(s.current, 0, page(0, 50)))
      last    <- ok(s.backend.pairRows(s.current, 0, page(first.page.total - 3, 50)))
      wrong   <- s.backend.pairRows(s.current, summary.scales.size, page(0, 5))
    yield
      assertEquals(first.page.total.toLong, summary.pairRowsPerScale)
      assertEquals(first.rows.size, 50)
      assertEquals(first.page.next, Some(50))
      assertEquals((last.rows.size, last.page.next), (3, None))
      // Each query's matched pair comes first, then its controls.
      assertEquals(first.rows.head.design, PairDesign.Matched)
      assertEquals(
        wrong,
        Left(BackendError.UnknownScale(s.current, summary.scales.size, summary.scales))
      )
  }

  test("the enveloped JSON transport answers exactly as the backend does in process") {
    def wire(request: Envelope[BackendRequest]): IO[Vector[Envelope[ServerFrame]]] =
      for
        s  <- subject
        in <- IO.fromEither(
          io.circe.parser.decode[Envelope[BackendRequest]](request.asJson.noSpaces)
        )
        out  <- StudyBackend.handle(s.backend)(in).compile.toVector
        back <- out.traverse(f =>
          IO.fromEither(io.circe.parser.decode[Envelope[ServerFrame]](f.asJson.noSpaces))
        )
      yield back
    for
      s   <- subject
      row <- ok(s.backend.queries(s.current, page(0, 1))).map(_.rows.head)
      rev <- s.backend.runs.map(_.find(_.run == s.current).map(_.revision).get)
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
        BackendRequest.Result(RunId(9999)),
        BackendRequest.Subscribe(JobId(9999)),
        BackendRequest.PairRowsOf(s.current, 0, page(0, 5)),
        BackendRequest.TrialFixationsOf(rev, row.query),
        BackendRequest.TrialPreviewOf(rev, row.query),
        BackendRequest.SourceRecordsOf(rev, 1, 5)
      ).zipWithIndex.map((r, i) => Envelope(RequestId(i.toLong), r))
      direct <- requests.traverse(r =>
        subject.flatMap(t => StudyBackend.handle(t.backend)(r).compile.toVector)
      )
      wired  <- requests.traverse(wire)
      future <- wire(Envelope(ProtocolVersion(2, 0), RequestId(99), BackendRequest.Runs))
      older  <- wire(Envelope(ProtocolVersion(1, 2), RequestId(98), BackendRequest.Runs))
    yield
      assertEquals(wired, direct)
      assert(direct.forall(_.size == 1), direct)
      assertEquals(direct.map(_.head.id), requests.map(_.id))
      // Protocol 1.7: the revision's first five source records.
      assert(
        direct.last.head.body match
          case ServerFrame.Response(BackendResponse.SourceRecordsOf(p)) =>
            p.revision == rev && p.rows.map(_.record) == (1 to 5).toVector
          case _ => false
        ,
        direct.last
      )
      // Protocol 1.6: the run's query trial has fixations and a preview.
      assert(
        direct.dropRight(1).takeRight(2).map(_.head.body) match
          case Vector(
                ServerFrame.Response(BackendResponse.TrialFixationsOf(f)),
                ServerFrame.Response(BackendResponse.TrialPreviewOf(p))
              ) =>
            f.trial == row.query && p.trial == row.query && f.fixations.nonEmpty
          case _ => false
        ,
        direct.dropRight(1).takeRight(2)
      )
      assertEquals(
        direct.flatten.count {
          case Envelope(_, _, ServerFrame.Response(BackendResponse.Refused(_))) => true
          case _                                                                => false
        },
        2
      )
      // Versions are exact: an older minor is refused as a major is.
      assertEquals(
        older.map(_.body),
        Vector(
          ServerFrame.Response(
            BackendResponse.Refused(
              BackendError.UnsupportedVersion(ProtocolVersion(1, 2), ProtocolVersion.Current)
            )
          )
        )
      )
      assertEquals(
        future.map(_.body),
        Vector(
          ServerFrame.Response(
            BackendResponse.Refused(
              BackendError.UnsupportedVersion(ProtocolVersion(2, 0), ProtocolVersion.Current)
            )
          )
        )
      )
  }

  test("a subscription over the transport streams event frames under its request id") {
    for
      s      <- subject
      status <- ok(s.backend.submit(s.draft))
      fiber  <- StudyBackend
        .handle(s.backend)(Envelope(RequestId(7), BackendRequest.Subscribe(status.job)))
        .compile
        .toVector
        .start
      _      <- s.finish(status.job)
      frames <- fiber.joinWithNever
    yield
      assert(frames.nonEmpty)
      assert(frames.forall(_.id == RequestId(7)), frames)
      frames.last.body match
        case ServerFrame.Event(JobEvent.Finished(o)) => assertEquals(o.job, status.job)
        case other                                   => fail(s"last frame is $other")
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
