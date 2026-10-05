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

package eyes4s.studio.core.real

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{
  DatasetRevisionSpec,
  Source,
  SourceRole,
  Sources,
  StudioDocument
}
import eyes4s.studio.core.fixture.{FakeStudyBackend, GoldenCsv, StoryMoment, StoryMoments}
import eyes4s.studio.core.preview.PreviewBudget
import java.nio.charset.StandardCharsets
import munit.CatsEffectSuite

/** RealStudyBackend slice 1 beyond the conformance contract: the ledger is
  * the one the fake serves from the fixture's stated rules, and every
  * refusal is a typed value naming its subject.
  */
class RealStudyBackendSuite extends CatsEffectSuite:
  // Several tests run the golden study to completion, some more than once.
  override def munitIOTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(5, "min")

  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val t2: StudioDocument = get(StoryMoments.t2)

  private def backend(sources: DatasetSources[IO] = RealBackendConformanceSuite.golden) =
    RealStudyBackend.create[IO](t2, sources)

  private def every(b: StudyBackend[IO], d: DatasetRevision): IO[Vector[LedgerEntry]] =
    LedgerPages.all[IO](b.ledger(d, _)).map(get)

  /** The clock an overlap names, which eyes4s identifies by the trial key's
    * digest and the fake by the key's text; nothing else of an entry differs.
    */
  private def withoutClock(entry: LedgerEntry): LedgerEntry = entry.disposition match
    case TrialDisposition.Quarantined(QuarantineCause.Overlap(index, previous, current)) =>
      def interval(s: String) = s.takeWhile(_ != ')') + ")"
      entry.copy(disposition =
        TrialDisposition.Quarantined(
          QuarantineCause.Overlap(index, interval(previous), interval(current))
        )
      )
    case _ => entry

  test("eyes4s's ledger equals the fake's, trial by trial, item, response and disposition") {
    for
      real  <- backend()
      fake  <- FakeStudyBackend.create[IO](StoryMoment.T2)
      mine  <- every(real, StoryMoments.r3)
      yours <- every(fake, StoryMoments.r3)
    yield
      assertEquals(mine.size, 960)
      assertEquals(mine.map(withoutClock), yours.map(withoutClock))
      // Only the six overlap trials' clock names differ: eyes4s names a
      // trial's clock by its key digest (fixation-trial:<16 hex digits>).
      val differing = mine.zip(yours).collect { case (m, y) if m != y => m }
      assertEquals(differing.size, 6)
      differing.foreach { e =>
        e.disposition match
          case TrialDisposition.Quarantined(QuarantineCause.Overlap(_, previous, _)) =>
            assert(previous.matches(""".* on fixation-trial:[0-9a-f]{16}"""), previous)
          case other => fail(s"${e.trial} differs in $other")
      }
  }

  test("the ledger pages as requested and names the dataset") {
    for
      real <- backend()
      page <- real.ledger(StoryMoments.r3, get(PageRequest.of(950, 97))).map(get)
    yield
      assertEquals(page.dataset, StoryMoments.r3)
      assertEquals((page.page.offset, page.entries.size, page.page.next), (950, 10, None))
  }

  test("a revision without declared time units is not admitted: Unavailable names the field") {
    backend().flatMap(_.admission(StoryMoments.r2)).map { r =>
      assertEquals(r, Left(BackendError.Unavailable(DiagnosticLocus.Field("time units"))))
    }
  }

  test("admission equals the fake's summary of the fixture, window durations included") {
    for
      real  <- backend().flatMap(_.admission(StoryMoments.r3)).map(get)
      fake  <- FakeStudyBackend.create[IO](StoryMoment.T2)
      yours <- fake.admission(StoryMoments.r3).map(get)
    yield assertEquals(real, yours.copy(history = ""))
  }

  private def admitSynthetic(trials: Vector[String], fixations: Vector[String]) =
    val header = "participant,phase,trial,occurrence,item,display_kind,image_file,response"
    val fixes  =
      "participant,phase,trial,occurrence,ordinal,x,y,onset_ms,duration_ms,sample_count"
    val r3 = get(t2.dataset(StoryMoments.r3).toRight("no r3"))
    RealAdmission.admit(
      r3,
      (fixes +: fixations).mkString("", "\n", "\n"),
      (header +: trials).mkString("", "\n", "\n"),
      get(eyes4s.studio.core.fixture.GoldenAssets.registry(r3))
    )

  test("each trial keeps its own outside-screen records, keyed by the full trial key") {
    val admitted = get(
      admitSynthetic(
        Vector(
          "P01,Encoding,enc_01,1,beach-007,image,beach-007.png,",
          "P01,Encoding,enc_02,2,beach-007,image,beach-007.png,"
        ),
        Vector(
          "P01,Encoding,enc_01,1,1,500,500,0,100,50",
          "P01,Encoding,enc_01,1,2,2000,500,200,100,50",
          "P01,Encoding,enc_02,2,1,-10,300,0,100,50",
          "P01,Encoding,enc_02,2,2,600,600,200,100,50"
        )
      )
    )
    val outside =
      admitted.ledger.map(e => (e.trial.trial, e.trial.occurrence) -> e.outsideFrame.map(_.x))
    assertEquals(
      outside,
      Vector(("enc_01", 1) -> Vector(2000.0), ("enc_02", 2) -> Vector(-10.0))
    )
    assertEquals(admitted.summary.window.outsideScreen, 2)
    assertEquals(admitted.summary.window.trialsOutsideScreen, 2)
  }

  test("eyes4s refuses one trial label declared with two occurrences: nothing can merge") {
    val refused = admitSynthetic(
      Vector(
        "P01,Encoding,enc_01,1,beach-007,image,beach-007.png,",
        "P01,Encoding,enc_01,2,beach-007,image,beach-007.png,"
      ),
      Vector(
        "P01,Encoding,enc_01,1,1,2000,500,0,100,50",
        "P01,Encoding,enc_01,2,1,-10,300,0,100,50"
      )
    )
    assertEquals(
      refused,
      Left(
        BackendError.InventoryRefused(
          StoryMoments.r3,
          Vector(
            InventoryIssue.Conflict(
              TrialLabel("P01", "Encoding", "enc_01"),
              Vector(2, 3),
              Vector("occurrence")
            )
          )
        )
      )
    )
  }

  test("bytes the revision did not record are refused, never admitted") {
    val tampered = new DatasetSources[IO]:
      def bytes(d: DatasetRevisionSpec, s: Source) =
        RealBackendConformanceSuite.golden
          .bytes(d, s)
          .map(_.map(b => b.updated(b.length - 2, '9'.toByte)))
      def assets(d: DatasetRevisionSpec) = RealBackendConformanceSuite.golden.assets(d)
    val recorded = get(t2.dataset(StoryMoments.r3).toRight("no r3")).sources.fixations.get
    backend(tampered).flatMap(_.admission(StoryMoments.r3)).map { r =>
      assertEquals(r.left.map(_.code), Left("studio-backend.unavailable"))
      // Until protocol 1.11: the reason names the file and both digests.
      val message = r.left.map(_.message).swap.getOrElse(fail("admitted"))
      assert(message.contains(recorded.path.value), message)
      assert(message.contains(s"recorded sha256 ${recorded.bytes.hex}"), message)
      assert(message.matches(".*read sha256 [0-9a-f]{64}.*"), message)
    }
  }

  // Pending protocol 1.11 (docs/studio/plan/S3.7-slices.md): not served yet,
  // listed so the typed forms are not forgotten.
  test("PENDING 1.11: a digest mismatch is the typed SourceDigestMismatch".ignore) {
    val tampered = new DatasetSources[IO]:
      def bytes(d: DatasetRevisionSpec, s: Source) =
        RealBackendConformanceSuite.golden
          .bytes(d, s)
          .map(_.map(b => b.updated(b.length - 2, '9'.toByte)))
      def assets(d: DatasetRevisionSpec) = RealBackendConformanceSuite.golden.assets(d)
    backend(tampered).flatMap(_.admission(StoryMoments.r3)).map { r =>
      assertEquals(r.left.map(_.code), Left("studio-backend.source-digest-mismatch"))
    }
  }

  test("PENDING 1.11: a non-inventory refusal is the typed AdmissionRefused".ignore) {
    backend().flatMap(_.admission(StoryMoments.r2)).map { r =>
      assertEquals(r.left.map(_.code), Left("studio-backend.admission-refused"))
    }
  }

  test("an inventory eyes4s refuses is InventoryRefused, naming the trial and the column") {
    val lines       = GoldenCsv.trials.linesIterator.toVector
    val conflicting =
      (lines :+ (lines(1).stripSuffix(",") + ",Remembered")).mkString("", "\n", "\n")
    val changed = IArray.from(conflicting.getBytes(StandardCharsets.UTF_8))
    val r3      = get(t2.dataset(StoryMoments.r3).toRight("no r3"))
    val sources = get(
      Sources.of(r3.sources.entries.map {
        case s if s.role == SourceRole.Trials => s.copy(bytes = ByteDigest.sha256(changed))
        case s                                => s
      })
    )
    val document = get(
      StudioDocument.of(
        t2.datasets.map(d => if d.id == StoryMoments.r3 then d.copy(sources = sources) else d),
        t2.analyses,
        t2.draft,
        t2.runs,
        t2.reporting,
        t2.figures,
        t2.presentation,
        t2.jobs
      )
    )
    val host = new DatasetSources[IO]:
      def bytes(d: DatasetRevisionSpec, s: Source) =
        if s.role == SourceRole.Trials then IO.pure(Some(changed))
        else RealBackendConformanceSuite.golden.bytes(d, s)
      def assets(d: DatasetRevisionSpec): IO[Option[AssetRegistry]] =
        RealBackendConformanceSuite.golden.assets(d)
    RealStudyBackend.create[IO](document, host).flatMap(_.admission(StoryMoments.r3)).map {
      case Left(BackendError.InventoryRefused(d, issues)) =>
        assertEquals(d, StoryMoments.r3)
        assertEquals(
          issues,
          Vector(
            InventoryIssue.Conflict(
              TrialLabel("P01", "Encoding", "enc_01"),
              Vector(2, lines.size + 1),
              Vector("response")
            )
          )
        )
      case other => fail(s"expected InventoryRefused, got $other")
    }
  }

  test("operations of later slices are typed refusals, never invented data") {
    for
      real  <- backend()
      known <- real.preview(StoryMoments.rev4)
      draft <- real.trialFixations(
        StoryMoments.rev5,
        TrialKey("P17", Phase.Retrieval, "ret_07", 1)
      )
      unknown <- real.submit(AnalysisRevision(99))
      runs    <- real.runs
      jobs    <- real.jobs
      run7    <- real.result(StoryMoments.run7)
      fake    <- FakeStudyBackend.create[IO](StoryMoment.T2).flatMap(_.runs)
      events  <- real
        .previewCounting(StoryMoments.rev4, get(PreviewBudget.of(1)))
        .compile
        .toVector
    yield
      assertLayoutRefused(known, StoryMoments.rev4)
      assertLayoutRefused(draft, StoryMoments.rev5)
      assertEquals(unknown.left.map(_.code), Left("studio-backend.unknown-revision"))
      // The document's runs, in the states the fake reports at t2.
      assertEquals(runs, fake)
      assertEquals(jobs, Vector.empty)
      assertEquals(run7, Left(BackendError.Unavailable(DiagnosticLocus.Run(StoryMoments.run7))))
      assertEquals(events.map(_.left.map(_.code)), Vector(Left("studio-backend.unavailable")))
  }

  private val trialLayout: StudioDocument = get(RealBackendConformanceSuite.trialLayout)

  /** The story preset's layout refused, naming the revision and the field. */
  private def assertLayoutRefused[A](r: Either[BackendError, A], revision: AnalysisRevision) =
    r match
      case Left(BackendError.Unavailable(DiagnosticLocus.Artifact(why))) =>
        assert(why.startsWith(s"${revision.label} layout: "), why)
      case other => fail(s"expected the layout refused, got $other")

  test("the preview counts are eyes4s's prepared study over the admitted trials") {
    for
      real  <- RealStudyBackend.create[IO](trialLayout, RealBackendConformanceSuite.golden)
      rev4  <- real.preview(StoryMoments.rev4).map(get)
      rev5  <- real.preview(StoryMoments.rev5).map(get)
      story <- backend().flatMap(_.preview(StoryMoments.rev4))
    yield
      assertEquals((rev4.revision, rev4.dataset), (StoryMoments.rev4, StoryMoments.r3))
      assertEquals(rev4.scales, Vector("0.5°", "1°", "2°", "4°"))
      assertEquals(rev5.scales, rev4.scales :+ "8°")
      // Candidates are counted over admitted trials (lead, 2026-10-04): 466 x 471.
      assertEquals((rev4.focalTrials, rev4.referenceTrials), (466, 471))
      assertEquals(rev4.candidatePairsPerScale, 219486L)
      assertEquals(rev4.requestedQueries, 480)
      assert(rev4.eligibleQueries > 0 && rev4.eligibleQueries <= rev4.focalTrials, rev4)
      assertEquals(rev4.pairRows, rev4.pairRowsPerScale * 4)
      assertEquals(rev5.pairRowsPerScale, rev4.pairRowsPerScale)
      assertEquals(rev5.pairRows, rev5.pairRowsPerScale * 5)
      // The story's own layout does not fit an inventory dataset's keys.
      assertLayoutRefused(story, StoryMoments.rev4)
  }

  private def ok[A](fa: IO[Either[BackendError, A]]): IO[A] = fa.map(get)

  test(
    "a completed run did every map and pair its prepared study counted, by eyes4s's meters"
  ) {
    for
      real    <- RealStudyBackend.create[IO](trialLayout, RealBackendConformanceSuite.golden)
      preview <- ok(real.preview(StoryMoments.rev4))
      status  <- ok(real.submit(StoryMoments.rev4))
      again   <- real.submit(StoryMoments.rev5)
      events  <- ok(real.subscribe(status.job)).flatMap(_.compile.toVector)
      out     <- ok(real.outcome(status.job))
      runs    <- real.runs
    yield
      assertEquals((status.job, status.run), (JobId(1), RunId(8)))
      assertEquals(status.state, JobState.Queued)
      assertEquals(again, Left(BackendError.AlreadyRunning(StoryMoments.rev5, status.job)))
      val last = out match
        case Some(JobOutcome.Completed(_, _, p)) => p
        case other                               => fail(s"expected Completed, got $other")
      assertEquals(last.totals.completedPairs, preview.pairRows)
      assertEquals(last.totals.totalPairs, ProgressTotal.Exact(preview.pairRows))
      assertEquals(last.totals.totalMaps, ProgressTotal.Exact(last.totals.completedMaps))
      val advanced = events.collect { case JobEvent.Advanced(p) => p }
      assert(advanced.nonEmpty, events)
      // Every report's meter is eyes4s's, in the unit of its stage, and the
      // run totals carry the latest map and pair meters.
      advanced.foreach { p =>
        p.meter.unit match
          case CountUnit.Maps =>
            assertEquals(p.totals.completedMaps, p.meter.done)
            assertEquals(p.meter.total, last.totals.totalMaps)
          case CountUnit.Pairs =>
            assertEquals(p.totals.completedPairs, p.meter.done)
            assertEquals(p.meter.total, ProgressTotal.Exact(preview.pairRows))
          case _ => ()
      }
      assert(advanced.exists(_.meter.unit == CountUnit.Pairs), advanced.map(_.segment))
      assertEquals(
        runs.last,
        RunSummary(RunId(8), StoryMoments.rev4, StoryMoments.r3, RunState.Completed)
      )
  }

  /** rev4 prepared over the golden data, for the pure execution mapping. */
  private lazy val rev4Prepared: RealPrepared =
    val r3       = get(trialLayout.dataset(StoryMoments.r3).toRight("no r3"))
    val registry = get(eyes4s.studio.core.fixture.GoldenAssets.registry(r3))
    val admitted = get(RealAdmission.admit(r3, GoldenCsv.fixations, GoldenCsv.trials, registry))
    val recipe   = get(trialLayout.analysis(StoryMoments.rev4).toRight("no rev4")).recipe
    get(RealPrepared.of(StoryMoments.rev4, StoryMoments.r3, recipe, admitted))

  test("a run eyes4s refuses settles Failed with eyes4s's study-run diagnostic") {
    import eyes4s.plan.{
      CountUnit as CoreUnit,
      StageKind as CoreKind,
      StageMeterError,
      StudyRunError
    }
    val error =
      StudyRunError.Meter(StageMeterError.Regressed(CoreKind.Comparing, CoreUnit.Pairs, 9, 4))
    val (out, state, result) = RealExecution.settle(
      JobId(1),
      RunId(8),
      Right(eyes4s.fs2.RunOutcome.Failed((JobId(1), RunId(8)), error, None)),
      RealExecution.Carried.none,
      rev4Prepared.counts
    )
    val expected = eyes4s.plan.Diagnostic.of(error)
    out match
      case JobOutcome.Failed(JobId(1), RunId(8), Vector(d), None) =>
        assertEquals(d.code, expected.code.render)
        assert(d.code.startsWith("study-run"), d.code)
        assertEquals(d.origin, DiagnosticOrigin.EyesCore)
        assertEquals(d.message, expected.message)
      case other => fail(s"expected one eyes4s diagnostic, got $other")
    assertEquals((state, result), (RunState.Failed, None))
  }

  test("a run whose end eyes4s cannot report settles Failed, naming the run, with no result") {
    val prepared             = rev4Prepared
    val (out, state, result) = RealExecution.settle(
      JobId(1),
      RunId(8),
      Left(RealExecution.Defect("eyes4s raised boom")),
      RealExecution.Carried.none,
      prepared.counts
    )
    val diagnostics = out match
      case JobOutcome.Failed(JobId(1), RunId(8), ds, None) => ds
      case other => fail(s"expected Failed, got $other")
    assertEquals(
      diagnostics.map(d => (d.code, d.subject, d.message)),
      Vector(
        (
          "studio-backend.unavailable",
          Vector(DiagnosticLocus.Run(RunId(8))),
          "eyes4s raised boom"
        )
      )
    )
    assertEquals(state, RunState.Failed)
    assertEquals(result, None)
  }

  // ------------------------------------------------------------------ recomputation (slice 5)

  /** trialLayout with run 7's archive bound to `digest`. */
  private def bound(digest: String): StudioDocument =
    val archive = eyes4s.studio.core.document.CoreBinding.Bound(
      get(
        eyes4s.codec.CanonicalDigest
          .parse[eyes4s.studio.core.document.ResultArchiveArtifact](digest)
      )
    )
    val t = trialLayout
    get(
      StudioDocument.of(
        t.datasets,
        t.analyses,
        t.draft,
        t.runs.map(r => if r.id == StoryMoments.run7 then r.copy(archive = archive) else r),
        t.reporting,
        t.figures,
        t.presentation,
        t.jobs
      )
    )

  /** Ask for `run`'s result, then wait for the job that recomputes it. */
  private def recompute(real: RealStudyBackend[IO], run: RunId) =
    for
      first <- real.held(run)
      jobs  <- real.jobs
      job = jobs.find(_.run == run).getOrElse(fail(s"no job for $run: $jobs"))
      _     <- ok(real.subscribe(job.job)).flatMap(_.compile.drain)
      out   <- ok(real.outcome(job.job))
      after <- real.held(run)
    yield (first, job, out, after)

  test(
    "a document run is recomputed as a visible job, kept, and marked with the eyes4s release"
  ) {
    for
      real   <- RealStudyBackend.create[IO](trialLayout, RealBackendConformanceSuite.golden)
      before <- real.runs
      (first, job, out, after) <- recompute(real, StoryMoments.run7)
      again                    <- real.held(StoryMoments.run7)
      jobs                     <- real.jobs
      runs                     <- real.runs
    yield
      assertEquals(
        first,
        Left(BackendError.Unavailable(DiagnosticLocus.Run(StoryMoments.run7)))
      )
      assertEquals(
        (job.run, job.revision, job.dataset),
        (StoryMoments.run7, StoryMoments.rev4, StoryMoments.r3)
      )
      assert(out.exists(_.isInstanceOf[JobOutcome.Completed]), out)
      val held = after.getOrElse(fail(s"no result: $after"))
      assertEquals(
        held.origin,
        RealStudyBackend.RunOrigin.Recomputed(
          eyes4s.studio.core.engine.StudioBuild.eyes4sBaseVersion
        )
      )
      assertEquals(held.prepared.revision, StoryMoments.rev4)
      // Kept: no second job, and the run's state is untouched.
      assert(again.isRight, again)
      assertEquals(jobs.map(_.job), Vector(job.job))
      assertEquals(runs, before)
  }

  test("a run without a result is NoResult, and a run whose revision cannot run says why") {
    for
      real      <- RealStudyBackend.create[IO](trialLayout, RealBackendConformanceSuite.golden)
      cancelled <- real.held(StoryMoments.run6)
      r2        <- real.held(StoryMoments.run5)
      unknown   <- real.held(RunId(99))
      jobs      <- real.jobs
    yield
      assertEquals(
        cancelled,
        Left(
          BackendError.NoResult(
            StoryMoments.run6,
            RunState.Cancelled(Some(StageKind.Comparing))
          )
        )
      )
      // run 5 is on r2, which declares no time units.
      assertEquals(r2, Left(BackendError.Unavailable(DiagnosticLocus.Field("time units"))))
      assertEquals(unknown.left.map(_.code), Left("studio-backend.unknown-run"))
      assertEquals(jobs, Vector.empty)
  }

  test(
    "a recomputed result is checked against the run's recorded digest: equal is served, different refused naming both"
  ) {
    val wrong = "ab" * 32
    for
      plain <- RealStudyBackend.create[IO](trialLayout, RealBackendConformanceSuite.golden)
      (_, _, _, served) <- recompute(plain, StoryMoments.run7)
      held   = served.getOrElse(fail(s"no result: $served"))
      digest = get(held.prepared.digest(held.result))
      matching <- RealStudyBackend.create[IO](bound(digest), RealBackendConformanceSuite.golden)
      (_, _, _, same) <- recompute(matching, StoryMoments.run7)
      tampered <- RealStudyBackend.create[IO](bound(wrong), RealBackendConformanceSuite.golden)
      (_, job, out, bad) <- recompute(tampered, StoryMoments.run7)
      again              <- tampered.held(StoryMoments.run7)
      jobs               <- tampered.jobs
    yield
      assert(same.isRight, same)
      val refused = BackendError.Unavailable(
        DiagnosticLocus.Artifact(
          s"run 7 result: recorded sha256 $wrong, recomputed sha256 $digest"
        )
      )
      assertEquals(bad, Left(refused))
      assertEquals(
        out,
        Some(
          JobOutcome.Failed(
            job.job,
            StoryMoments.run7,
            Vector(refused.diagnostic),
            out.flatMap(_.progress)
          )
        )
      )
      // The refusal is kept: no second recomputation.
      assertEquals(again, Left(refused))
      assertEquals(jobs.size, 1)
  }

  // ------------------------------------------------------------------ results against SCORES.json

  /** SCORES.json's queries: key, status, matched trials and M/B/D per scale. */
  private final case class Scored(
      key: TrialKey,
      status: String,
      matched: Vector[String],
      controls: Option[Int],
      scores: Vector[(Option[Double], Option[Double], Option[Double])]
  )

  private lazy val scores: (Vector[Scored], Long) =
    val json   = get(io.circe.parser.parse(eyes4s.studio.core.fixture.GoldenScores.text))
    val c      = json.hcursor
    val sigmas = Vector("0.5", "1", "2", "4")
    val qs     = get(c.get[Vector[io.circe.Json]]("queries")).map { q =>
      val h              = q.hcursor
      def mbd(s: String) =
        val sc = h.downField("scales").downField(s)
        (
          get(sc.get[Option[Double]]("M")),
          get(sc.get[Option[Double]]("B")),
          get(sc.get[Option[Double]]("D"))
        )
      Scored(
        TrialKey(
          get(h.get[String]("participant")),
          Phase.Retrieval,
          get(h.get[String]("trial")),
          get(h.get[Int]("occurrence"))
        ),
        get(h.get[String]("status")),
        get(h.get[Vector[String]]("matched")),
        get(h.get[Option[Int]]("controls")),
        sigmas.map(mbd)
      )
    }
    (qs, get(c.downField("counts").get[Long]("pairRowsPerScale")))

  /** Every pair row of `run` at `scale`, following `next`. */
  private def allPairRows(
      real: RealStudyBackend[IO],
      run: RunId,
      scale: Int
  ): IO[Vector[PairRowEntry]] =
    def go(offset: Int, acc: Vector[PairRowEntry]): IO[Vector[PairRowEntry]] =
      real.pairRows(run, scale, get(PageRequest.of(offset, 4096))).map(get).flatMap { page =>
        page.page.next match
          case Some(n) => go(n, acc ++ page.rows)
          case None    => IO.pure(acc ++ page.rows)
      }
    go(0, Vector.empty)

  /** SCORES.json rounds half-even to 6 places: half a unit in the last
    * place, plus the double's own rounding of the stored decimal.
    */
  private val ScoresTolerance: Double = 5e-7 + 1e-12

  private def near(obtained: Double, stored: Double, what: String): Unit =
    assert(math.abs(obtained - stored) <= ScoresTolerance, s"$what: $obtained vs $stored")

  test("run 7's inspected contrasts and matched pair rows are SCORES.json's, at every scale") {
    val (queries, perScale) = scores
    for
      real <- RealStudyBackend.create[IO](trialLayout, RealBackendConformanceSuite.golden)
      (_, _, out, _) <- recompute(real, StoryMoments.run7)
      inspected      <- queries.traverse(q =>
        (0 until 4).toVector.traverse(i =>
          real.inspect(StoryMoments.run7, ResultAddress.ContrastRow(i, q.key))
        )
      )
      pages  <- (0 until 4).toVector.traverse(i => allPairRows(real, StoryMoments.run7, i))
      beyond <- real.pairRows(StoryMoments.run7, 4, get(PageRequest.of(0, 1)))
      // The reductions and the matched pair of every contributing query at 2°.
      contributing = queries.filter(_.status == "contributing")
      reduced <- contributing.traverse { q =>
        val enc = TrialKey(q.key.participant, Phase.Encoding, q.matched.head, 1)
        (
          real.inspect(
            StoryMoments.run7,
            ResultAddress.Reduction(2, PairDesign.Matched, q.key)
          ),
          real.inspect(
            StoryMoments.run7,
            ResultAddress.Reduction(2, PairDesign.Control, q.key)
          ),
          real.inspect(
            StoryMoments.run7,
            ResultAddress.PairRow(2, PairDesign.Matched, q.key, enc)
          )
        ).tupled
      }
      // The first control pair of every contributing query at 2°.
      firstControls = contributing.flatMap(q =>
        pages(2).find(r => r.query == q.key && r.design == PairDesign.Control)
      )
      controlInspected <- firstControls.traverse(r =>
        real
          .inspect(
            StoryMoments.run7,
            ResultAddress.PairRow(2, PairDesign.Control, r.query, r.reference)
          )
          .map(r -> _)
      )
      ledger <- every(real, StoryMoments.r3)
      items = ledger.map(e => e.trial -> e.item).toMap
    yield
      assert(out.exists(_.isInstanceOf[JobOutcome.Completed]), out)
      assertEquals(pages.map(_.size.toLong), Vector.fill(4)(perScale))
      assertEquals(beyond.left.map(_.code), Left("studio-backend.unknown-scale"))
      queries.zip(inspected).foreach { (q, results) =>
        results.zipWithIndex.foreach { (r, i) =>
          val at = s"${q.key.label} at scale $i"
          (q.status, r) match
            case ("contributing", Right(Inspection.Contrast(_, m, b, d))) =>
              val (sm, sb, sd) = q.scores(i)
              near(m, sm.get, s"$at M"); near(b, sb.get, s"$at B"); near(d, sd.get, s"$at D")
            case (s, Right(Inspection.Unscored(_, QueryStatus.Failed(diag))))
                if s.startsWith("failed:") =>
              assertEquals(diag.code, s.stripPrefix("failed:"), at)
            case ("no-match", Right(Inspection.Unscored(_, QueryStatus.Failed(diag)))) =>
              // eyes4s holds the row with no control operand. Protocol 1.11
              // reports it as NoMatch with eyes4s's unmatched reason instead.
              assertEquals(diag.code, "contrast-row.missing-operands", at)
            case other => fail(s"$at: SCORES ${q.status}, backend $other")
        }
      }
      contributing.zip(reduced).foreach { case (q, (m, b, pair)) =>
        val (sm, sb, _) = q.scores(2)
        (m, b, pair) match
          case (
                Right(Inspection.Reduction(_, mv, 1)),
                Right(Inspection.Reduction(_, bv, controls)),
                Right(Inspection.Pair(_, _, pv))
              ) =>
            near(mv, sm.get, s"${q.key.label} matched reduction")
            near(bv, sb.get, s"${q.key.label} control reduction")
            near(pv, sm.get, s"${q.key.label} matched pair")
            assertEquals(Some(controls), q.controls, s"${q.key.label} controls")
          case other => fail(s"${q.key.label}: $other")
      }
      // Each query's pair rows are contiguous, queries in focal (SCORES) order.
      val order = queries.map(_.key)
      pages.zipWithIndex.foreach { (rows, i) =>
        val runs = rows
          .map(_.query)
          .foldLeft(Vector.empty[TrialKey])((acc, k) =>
            if acc.lastOption.contains(k) then acc else acc :+ k
          )
        assertEquals(runs, runs.distinct, s"scale $i: a query's rows are not contiguous")
        assertEquals(runs, order.filter(runs.toSet), s"scale $i: queries out of focal order")
      }
      // Every contributing query's control count, and a control pair as
      // eyes4s holds it: the reference's item and the row's score.
      val at2 = pages(2).groupBy(_.query)
      contributing.foreach { q =>
        val controls = at2(q.key).filter(_.design == PairDesign.Control)
        assertEquals(Some(controls.size), q.controls, s"${q.key.label} control rows")
      }
      assertEquals(controlInspected.size, contributing.size)
      controlInspected.foreach { (row, inspected) =>
        val score = row.score match
          case PairScoreState.Scored(v) => v
          case other                    => fail(s"$row: $other")
        assertEquals(
          inspected,
          Right(
            Inspection.Pair(
              ResultAddress.PairRow(2, PairDesign.Control, row.query, row.reference),
              items(row.reference),
              score
            )
          )
        )
        assertNotEquals(items(row.reference), items(row.query), row)
      }
      // Each contributing query's matched pair is its stored M, query by query,
      // matched pair first.
      pages.zipWithIndex.foreach { (rows, i) =>
        val firsts = rows.groupBy(_.query).view.mapValues(_.head).toMap
        queries.filter(_.status == "contributing").foreach { q =>
          val row = firsts.getOrElse(q.key, fail(s"${q.key.label}: no pair rows at $i"))
          assertEquals((row.design, row.reference.trial), (PairDesign.Matched, q.matched.head))
          row.score match
            case PairScoreState.Scored(v) =>
              near(v, q.scores(i)._1.get, s"${q.key.label} pair at $i")
            case other => fail(s"${q.key.label} at $i: $other")
        }
      }
  }

  // ------------------------------------------------------------------ trial views (slice 7)

  test("every admitted trial's fixations are eyes4s's placements, equal to the fake's") {
    for
      real   <- RealStudyBackend.create[IO](trialLayout, RealBackendConformanceSuite.golden)
      fake   <- FakeStudyBackend.create[IO](StoryMoment.T2)
      ledger <- every(real, StoryMoments.r3)
      trials = ledger.filter(_.disposition == TrialDisposition.Admitted).map(_.trial)
      mine   <- trials.traverse(t => real.trialFixations(StoryMoments.rev4, t))
      theirs <- trials.traverse(t => fake.trialFixations(StoryMoments.rev4, t))
      absent = ledger.find(_.disposition != TrialDisposition.Admitted).map(_.trial)
      none  <- absent.traverse(t => real.trialFixations(StoryMoments.rev4, t))
      stray <- real.trialFixations(StoryMoments.rev4, TrialKey("P99", Phase.Encoding, "x", 1))
    yield
      assertEquals(trials.size, 937)
      val served = mine.collect { case Right(f) => f.fixations.size }
      assertEquals(served.size, 937, mine.collectFirst { case Left(e) => e })
      // 11,520 records less the outside-screen and rejected ones eyes4s keeps out.
      assert(served.sum > 10000, served.sum)
      val differ = trials.indices.filter(i => mine(i) != theirs(i))
      assert(
        differ.isEmpty,
        s"${differ.size} trials differ; first: ${differ.headOption.map(i => (mine(i), theirs(i)))}"
      )
      assertEquals(
        none.map(_.left.map(_.code)),
        absent.map(_ => Left("studio-backend.unavailable"))
      )
      assertEquals(stray.left.map(_.code), Left("studio-backend.unknown-trial"))
  }

  test("every source record page is eyes4s-io's text and placement, equal to the fake's") {
    val limit                      = SourceRecordPage.Limit
    def pages(b: StudyBackend[IO]) =
      (0 until 24).toVector.traverse(i =>
        b.sourceRecords(StoryMoments.rev4, 1 + i * limit, limit)
      )
    for
      real    <- RealStudyBackend.create[IO](trialLayout, RealBackendConformanceSuite.golden)
      fake    <- FakeStudyBackend.create[IO](StoryMoment.T2)
      mine    <- pages(real)
      theirs  <- pages(fake)
      range   <- real.sourceRecords(StoryMoments.rev4, 0, 10)
      past    <- real.sourceRecords(StoryMoments.rev4, 11521, 1)
      tooMany <- real.sourceRecords(StoryMoments.rev4, 1, limit + 1)
    yield
      val rows = mine.collect { case Right(p) => p.rows.size }
      assertEquals(rows.sum, 11520, mine.collectFirst { case Left(e) => e })
      assert(mine.flatMap(_.toOption).flatMap(_.rows).exists(_.placement.isDefined))
      val differ = mine.indices.filter(i => mine(i) != theirs(i))
      assert(
        differ.isEmpty,
        s"${differ.size} pages differ; first: ${differ.headOption.map { i =>
            val (a, b) = (mine(i).toOption.get.rows, theirs(i).toOption.get.rows)
            a.zip(b).find((x, y) => x != y)
          }}"
      )
      def refusal(r: Either[BackendError, SourceRecordPage]) = r.left.toOption.collect {
        case BackendError.SourceRecordsRefused(_, e) => e
      }
      assertEquals(refusal(range), Some(SourceRecordsError.RangeInvalid(0, 10, limit)))
      assertEquals(refusal(past), Some(SourceRecordsError.PastEnd(11521, 11520)))
      assertEquals(refusal(tooMany), Some(SourceRecordsError.RangeInvalid(1, limit + 1, limit)))
  }

  test(
    "placements under FailTrial and DropFirst are eyes4s's: dropped first, failed trials, outside window"
  ) {
    import eyes4s.plan.{MapPlacement, OffWindowPolicy}
    import eyes4s.studio.core.document.{InitialFixationChoice, OffWindowChoice}
    val t      = trialLayout
    val strict = get(
      StudioDocument.of(
        t.datasets,
        t.analyses.map(a =>
          a.copy(recipe =
            a.recipe.copy(
              offWindow = Some(OffWindowChoice.FailTrial),
              initialFixations = InitialFixationChoice.DropFirst
            )
          )
        ),
        t.draft,
        t.runs,
        t.reporting,
        t.figures,
        t.presentation,
        t.jobs
      )
    )
    for
      real   <- RealStudyBackend.create[IO](strict, RealBackendConformanceSuite.golden)
      ledger <- every(real, StoryMoments.r3)
      trials = ledger.filter(_.disposition == TrialDisposition.Admitted).map(_.trial)
      views <- trials.traverse(tr => real.trialFixations(StoryMoments.rev4, tr).map(get))
    yield
      val placements = views.flatMap(_.fixations.map(_.placement))
      // Every scanpath's first fixation is dropped; none other is.
      views.foreach { v =>
        assertEquals(
          v.fixations.headOption.map(_.placement),
          Some(MapPlacement.DroppedInitial),
          v.trial
        )
        assert(!v.fixations.drop(1).exists(_.placement == MapPlacement.DroppedInitial), v.trial)
      }
      assert(
        placements.exists(_ == MapPlacement.OutsideWindow(OffWindowPolicy.FailTrial)),
        placements.distinct
      )
      assert(placements.exists(_.isInstanceOf[MapPlacement.TrialFailed]), placements.distinct)
      assert(placements.contains(MapPlacement.InWindow), placements.distinct)
      // A failed trial's in-window fixations are all TrialFailed, none InWindow.
      views
        .filter(_.fixations.exists(_.placement.isInstanceOf[MapPlacement.TrialFailed]))
        .foreach { v =>
          assert(!v.fixations.exists(_.placement == MapPlacement.InWindow), v.trial)
        }
  }

  test("provenance runs from the run to the items eyes4s holds, and refuses one it does not") {
    for
      real <- RealStudyBackend.create[IO](trialLayout, RealBackendConformanceSuite.golden)
      _    <- recompute(real, StoryMoments.run7)
      row  <- real
        .pairRows(StoryMoments.run7, 2, get(PageRequest.of(0, 1)))
        .map(get(_).rows.head)
      pair = ResultAddress.PairRow(2, row.design, row.query, row.reference)
      trail   <- real.provenance(StoryMoments.run7, pair).map(get(_).trail)
      map     <- real.provenance(StoryMoments.run7, ResultAddress.Estimation(2, row.reference))
      reduced <- real.provenance(
        StoryMoments.run7,
        ResultAddress.Reduction(2, PairDesign.Control, row.query)
      )
      swapped <- real.provenance(
        StoryMoments.run7,
        ResultAddress.PairRow(2, row.design, row.reference, row.query)
      )
      beyond <- real.provenance(StoryMoments.run7, ResultAddress.ContrastRow(9, row.query))
    yield
      assertEquals(
        trail,
        Vector(
          ProvenanceStep.Run(StoryMoments.run7),
          ProvenanceStep.Analysis(StoryMoments.rev4),
          ProvenanceStep.Dataset(StoryMoments.r3),
          ProvenanceStep.Scale(2, "2°"),
          ProvenanceStep.Design(PairDesign.Matched),
          ProvenanceStep.Trial(
            row.query,
            trail.collect { case ProvenanceStep.Trial(k, i) if k == row.query => i }.head
          ),
          ProvenanceStep.Trial(row.reference, row.referenceItem)
        )
      )
      assertEquals(
        map.map(_.trail.last),
        Right(ProvenanceStep.Trial(row.reference, row.referenceItem))
      )
      assertEquals(
        reduced.map(_.trail.takeRight(2).head),
        Right(ProvenanceStep.Design(PairDesign.Control))
      )
      assertEquals(swapped.left.map(_.code), Left("studio-backend.unknown-reference"))
      assertEquals(beyond.left.map(_.code), Left("studio-backend.unknown-scale"))
  }

  // ------------------------------------------------------------------ review-s37b

  test(
    "a run recorded on another dataset revision than its analysis's is refused, naming both"
  ) {
    val t     = trialLayout
    val moved = get(
      StudioDocument.of(
        t.datasets,
        t.analyses,
        t.draft,
        t.runs.map(r =>
          if r.id == StoryMoments.run7 then r.copy(dataset = StoryMoments.r2) else r
        ),
        t.reporting,
        t.figures,
        t.presentation,
        t.jobs
      )
    )
    for
      real <- RealStudyBackend.create[IO](moved, RealBackendConformanceSuite.golden)
      held <- real.held(StoryMoments.run7)
      jobs <- real.jobs
    yield
      assertEquals(
        held,
        Left(
          BackendError.Unavailable(
            DiagnosticLocus.Artifact("run 7 was computed on data r2; rev 4 is on data r3")
          )
        )
      )
      assertEquals(jobs, Vector.empty)
  }

  test(
    "PENDING S3.7 minor: a recomputed digest mismatch is the typed ResultDigestMismatch".ignore
  ) {
    RealStudyBackend
      .create[IO](bound("ab" * 32), RealBackendConformanceSuite.golden)
      .flatMap(real =>
        recompute(real, StoryMoments.run7).map { (_, _, _, held) =>
          assertEquals(held.left.map(_.code), Left("studio-backend.result-digest-mismatch"))
        }
      )
  }

  test(
    "PENDING S3.7 minor: a run on another dataset revision is the typed RunDatasetMismatch".ignore
  ) {
    RealStudyBackend
      .create[IO](trialLayout, RealBackendConformanceSuite.golden)
      .flatMap(
        _.held(StoryMoments.run7).map(r =>
          assertEquals(r.left.map(_.code), Left("studio-backend.run-dataset-mismatch"))
        )
      )
  }

  test(
    "PENDING S3.7 minor: a recomputed run's provenance says so, with the eyes4s release".ignore
  ) {
    RealStudyBackend
      .create[IO](trialLayout, RealBackendConformanceSuite.golden)
      .flatMap(real =>
        recompute(real, StoryMoments.run7) >>
          real
            .provenance(
              StoryMoments.run7,
              ResultAddress.ContrastRow(2, TrialKey("P17", Phase.Retrieval, "ret_07", 1))
            )
            .map(p =>
              assert(
                p.toOption.exists(_.trail.lift(1).exists(_.toString.contains("Recomputed"))),
                p
              )
            )
      )
  }

  test("releasing the backend cancels its running job promptly") {
    import scala.concurrent.duration.*
    RealStudyBackend
      .resource[IO](trialLayout, RealBackendConformanceSuite.golden)
      .use(real =>
        real
          .submit(StoryMoments.rev4)
          .map(get)
          .flatTap(_ =>
            // Wait until the job reports progress, so a run is in flight.
            real.subscribe(JobId(1)).map(get).flatMap(_.take(1).compile.drain)
          )
      )
      .timeout(20.seconds)
      .map(status => assertEquals(status.job, JobId(1)))
  }
