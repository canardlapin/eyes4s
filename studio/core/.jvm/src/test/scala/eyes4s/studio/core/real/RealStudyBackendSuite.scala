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
      assertEquals(
        known,
        Left(BackendError.Unavailable(DiagnosticLocus.Revision(StoryMoments.rev4)))
      )
      assertEquals(
        draft,
        Left(BackendError.Unavailable(DiagnosticLocus.Revision(StoryMoments.rev5)))
      )
      assertEquals(unknown.left.map(_.code), Left("studio-backend.unknown-revision"))
      // The document's runs, in the states the fake reports at t2.
      assertEquals(runs, fake)
      assertEquals(jobs, Vector.empty)
      assertEquals(run7, Left(BackendError.Unavailable(DiagnosticLocus.Run(StoryMoments.run7))))
      assertEquals(events.map(_.left.map(_.code)), Vector(Left("studio-backend.unavailable")))
  }

  private val trialLayout: StudioDocument = get(RealBackendConformanceSuite.trialLayout)

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
      assertEquals(
        story,
        Left(BackendError.Unavailable(DiagnosticLocus.Revision(StoryMoments.rev4)))
      )
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
