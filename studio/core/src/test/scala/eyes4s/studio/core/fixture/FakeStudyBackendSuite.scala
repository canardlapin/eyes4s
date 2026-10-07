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

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.studio.core.backend.*
import io.circe.{ACursor, Json}
import munit.CatsEffectSuite

import scala.concurrent.duration.*

/** The fake's own promises: the story timeline, step-held jobs, and every
  * fixture.json score, read here straight from the raw JSON rather than
  * through the fake's decoder.
  */
class FakeStudyBackendSuite extends CatsEffectSuite:

  private def ok[E, A](fa: IO[Either[E, A]]): IO[A] =
    fa.flatMap(e => IO.fromEither(e.leftMap(err => new AssertionError(err.toString))))

  private def page(offset: Int, size: Int): PageRequest =
    PageRequest.of(offset, size).fold(e => throw new AssertionError(e.message), identity)

  private val raw: Json =
    io.circe.parser
      .parse(MockStudy.fixtureText)
      .fold(e => throw new AssertionError(e), identity)

  private def get[A: io.circe.Decoder](c: ACursor, field: String): A =
    c.get[A](field).fold(e => throw new AssertionError(s"$field: $e"), identity)

  private val run7 = RunId(7)
  private val rev5 = AnalysisRevision(5)

  private def key(participant: String, trial: String) =
    TrialKey(
      participant,
      if trial.startsWith("enc_") then Phase.Encoding else Phase.Retrieval,
      trial,
      1
    )

  test("t3 holds run 8 at Comparing 21,400 / 44,845 pairs and stays there") {
    for
      fake   <- FakeStudyBackend.create[IO](StoryMoment.T3)
      before <- fake.jobs
      _      <- IO.sleep(50.millis)
      after  <- fake.jobs
      stream <- ok(fake.subscribe(before.head.job))
      first  <- stream.take(1).compile.toVector
    yield
      assertEquals(before, after)
      assertEquals(before.size, 1)
      val job = before.head
      assertEquals((job.run, job.revision, job.dataset), (RunId(8), rev5, DatasetRevision(3)))
      job.state match
        case JobState.Running(p) =>
          assertEquals(p.segment.kind, StageKind.Comparing)
          assertEquals(
            (p.totals.completedPairs, p.totals.totalPairs),
            (21400L, ProgressTotal.Exact(44845L))
          )
          // 21,400 = two whole scales of 8,969 pairs, 457 matched and 3,005 controls.
          assertEquals(p.segment, Segment.Comparing(2, PairDesign.Control))
          assertEquals(
            (p.meter.unit, p.meter.done, p.meter.total),
            (CountUnit.Pairs, 3005L, ProgressTotal.Exact(8512L))
          )
          assertEquals(
            (p.totals.completedMaps, p.totals.totalMaps),
            (3L * 937, ProgressTotal.Exact(5L * 937))
          )
          assertEquals(first, Vector(JobEvent.Advanced(p)))
        case other => fail(s"run 8 is not running: $other")
  }

  test("a test holds a submitted job at any step; run-level progress never moves back") {
    for
      fake   <- FakeStudyBackend.create[IO](StoryMoment.T2)
      status <- ok(fake.submit(rev5))
      queued <- ok(fake.job(status.job))
      p      <- ok(fake.advanceToPairs(status.job, 21400L))
      now    <- ok(fake.job(status.job))
      back   <- fake.advanceTo(status.job, Segment.Estimating(0), 1L)
      less   <- fake.advanceToPairs(status.job, 100L)
      beyond <- fake.advanceToPairs(status.job, 44846L)
      over   <- fake.advanceTo(status.job, Segment.Comparing(2, PairDesign.Control), 9000L)
      again  <- ok(fake.advanceTo(status.job, Segment.Estimating(3), 10L))
      second <- fake.submit(AnalysisRevision(4))
      out    <- ok(fake.cancel(status.job))
      late   <- fake.advanceToPairs(status.job, 30000L)
      runs   <- fake.runs
    yield
      assertEquals(queued.state, JobState.Queued)
      assertEquals((status.run, status.revision), (RunId(8), rev5))
      assertEquals((p.totals.completedPairs, p.step), (21400L, 1L))
      assertEquals(now.state, JobState.Running(p))
      assertEquals(
        back,
        Left(
          FakeControlError.Regressed(status.job, p.segment, 3005L, Segment.Estimating(0), 1L)
        )
      )
      assertEquals(
        less,
        Left(
          FakeControlError.Regressed(
            status.job,
            p.segment,
            3005L,
            Segment.Comparing(0, PairDesign.Matched),
            100L
          )
        )
      )
      assertEquals(
        beyond,
        Left(FakeControlError.PairsOutOfRange(status.job, 44846L, ProgressTotal.Exact(44845L)))
      )
      assertEquals(
        over,
        Left(
          FakeControlError.Progress(
            ProgressError.BeyondTotal("meter", 9000L, ProgressTotal.Exact(8512L))
          )
        )
      )
      // Stages repeat per scale: Estimating again, at scale 3, after Comparing at 2.
      assertEquals((again.segment, again.step), (Segment.Estimating(3), 2L))
      assertEquals(again.totals.completedPairs, 3L * 8969)
      assertEquals(second, Left(BackendError.AlreadyRunning(AnalysisRevision(4), status.job)))
      assertEquals(
        out.state,
        JobState.Finished(JobOutcome.Cancelled(status.job, RunId(8), Some(again)))
      )
      assert(late.isLeft, late)
      assertEquals(
        runs.find(_.run == RunId(8)).map(_.state),
        Some(RunState.Cancelled(Some(StageKind.Estimating)))
      )
  }

  test("a failed job carries its diagnostics and fails its run") {
    val diagnostic = StudioDiagnostic(
      "study-failure.off-window",
      DiagnosticLevel.Error,
      DiagnosticOrigin.EyesCore,
      Vector(DiagnosticLocus.Trial(key("P05", "ret_04"))),
      "empty map"
    )
    for
      fake   <- FakeStudyBackend.create[IO](StoryMoment.T2)
      status <- ok(fake.submit(rev5))
      out    <- ok(fake.fail(status.job, Vector(diagnostic)))
      polled <- ok(fake.outcome(status.job))
      runs   <- fake.runs
      result <- fake.result(status.run)
    yield
      assertEquals(out, JobOutcome.Failed(status.job, status.run, Vector(diagnostic), None))
      assertEquals(polled, Some(out))
      assertEquals(runs.last.state, RunState.Failed)
      assertEquals(result, Left(BackendError.NoResult(status.run, RunState.Failed)))
  }

  test("a completed job reaches every total") {
    for
      fake <- FakeStudyBackend.create[IO](StoryMoment.T3)
      job  <- fake.jobs.map(_.head.job)
      out  <- ok(fake.complete(job))
    yield out match
      case JobOutcome.Completed(_, _, last) =>
        assertEquals(last.segment, Segment.Contrasting(4))
        assertEquals(last.totals.completedPairs, 44845L)
        assertEquals(last.totals.completedMaps, 5L * 937)
      case other => fail(s"not completed: $other")
  }

  test("the story history is runs 5 to 8 of FIXTURE.md") {
    val described = get[Map[String, String]](raw.hcursor.downField("summary"), "runs")
    val Revision  = """rev (\d+)""".r.unanchored
    val Data      = """data r(\d+)""".r.unanchored
    for
      t1  <- FakeStudyBackend.create[IO](StoryMoment.T1)
      t1r <- t1.runs
      t1d <- ok(t1.admission(DatasetRevision(3)))
      t1s <- t1.submit(AnalysisRevision(4))
      t2  <- FakeStudyBackend.create[IO](StoryMoment.T2)
      t2r <- t2.runs
      t2j <- t2.jobs
      t3  <- FakeStudyBackend.create[IO](StoryMoment.T3)
      t3r <- t3.runs
    yield
      assertEquals(
        t1r,
        Vector(RunSummary(RunId(5), AnalysisRevision(3), DatasetRevision(2), RunState.Current))
      )
      assertEquals(t1d.state, DatasetState.Draft)
      assert(t1s.left.exists(_.code == "studio-backend.unknown-revision"), t1s)
      assertEquals(t2j, Vector.empty)
      assertEquals(
        t2r.map(_.state),
        Vector(RunState.Stale, RunState.Cancelled(Some(StageKind.Comparing)), RunState.Current)
      )
      assertEquals(t3r.map(_.run.number), Vector(5, 6, 7, 8))
      assertEquals(t3r.last.state, RunState.Running(JobId(1)))
      t3r.foreach { r =>
        val text = described(r.run.label)
        text match
          case Revision(rev) => assertEquals(rev.toInt, r.revision.number, text)
          case _             => fail(s"no revision in '$text'")
        text match
          case Data(data) => assertEquals(data.toInt, r.dataset.number, text)
          case _          => assertEquals(r.dataset.number, 3, s"'$text' implies data r3")
      }
      assert(described("run 6").contains("cancelled at Comparing"))
      assert(described("run 7").contains("current"))
      assert(described("run 5").contains("stale"))
  }

  test("every fixture.json query and score is served exactly") {
    val participants                        = get[Vector[Json]](raw.hcursor, "participants")
    def slugOf(d: TrialDisposition): String = d match
      case TrialDisposition.Absent         => "absent"
      case TrialDisposition.NoFixations    => "no-fixations"
      case TrialDisposition.Quarantined(c) => c.code.stripPrefix("quarantine.")
      case TrialDisposition.Admitted       => "admitted"
    for
      fake <- FakeStudyBackend.create[IO](StoryMoment.T2)
      rows <- ok(fake.queries(run7, page(0, 4096))).map(_.rows)
      byKey = rows.map(r => r.query -> r).toMap
      checked <- participants.flatTraverse { p =>
        val id = get[String](p.hcursor, "id")
        get[Vector[Json]](p.hcursor, "queries").traverse { q =>
          val c   = q.hcursor
          val k   = key(id, get[String](c, "trial"))
          val row = byKey(k)
          assertEquals(row.item, get[String](c, "item"))
          val expectedMatch = row.status match
            case QueryStatus.NoMatch(_) | QueryStatus.NotAdmitted(_) => None
            case _ => Some(key(id, get[String](c, "match")))
          assertEquals(row.matched, expectedMatch)
          assertEquals(row.response.label, get[String](c, "response"))
          assertEquals(row.controls, get[Option[Int]](c, "controls"))
          def reason = get[String](c, "reason")
          (get[String](c, "status"), row.status) match
            case ("ok", QueryStatus.Contributing(m, b, d)) =>
              val (rm, rb, rd) = (
                get[Vector[Double]](c, "M"),
                get[Vector[Double]](c, "B"),
                get[Vector[Double]](c, "D")
              )
              assertEquals((m, b, d), (rm, rb, rd))
              rm.indices.toVector
                .traverse_ { s =>
                  val contrast = ResultAddress.ContrastRow(s, k)
                  val control  = ResultAddress.Reduction(s, PairDesign.Control, k)
                  val matched = ResultAddress.PairRow(s, PairDesign.Matched, k, row.matched.get)
                  (
                    ok(fake.inspect(run7, contrast)),
                    ok(fake.inspect(run7, control)),
                    ok(fake.inspect(run7, matched))
                  ).mapN { (x, y, z) =>
                    assertEquals(x, Inspection.Contrast(contrast, rm(s), rb(s), rd(s)))
                    assertEquals(
                      y,
                      Inspection.Reduction(control, rb(s), row.controls.getOrElse(-1))
                    )
                    assertEquals(z, Inspection.Pair(matched, row.item, rm(s)))
                  }
                }
                .as(1)
            case ("failed", QueryStatus.Failed(d)) =>
              IO {
                assertEquals((d.code, d.message), ("study-failure.off-window", reason))
                assertEquals(d.subject, Vector(DiagnosticLocus.Trial(k)))
              }.as(1)
            case ("no match", QueryStatus.NoMatch(d)) =>
              IO(assertEquals((d.code, d.message), ("study-finding.unmatched-focal", reason)))
                .as(1)
            case ("query not admitted", QueryStatus.NotAdmitted(disposition)) =>
              IO(assertEquals(slugOf(disposition), reason)).as(1)
            case (s, status) => IO(fail(s"$id ${k.trial}: fixture status $s, served $status"))
        }
      }
    yield
      assertEquals(checked.sum, 480)
      assertEquals(rows.size, 480)
  }

  test("the result summary carries only fixture query accounting and run facts") {
    val s = raw.hcursor.downField("summary")
    for
      fake   <- FakeStudyBackend.create[IO](StoryMoment.T2)
      result <- ok(fake.result(run7))
    yield
      assertEquals(result.scaleLabels, get[Vector[String]](s, "scales"))
      assertEquals((result.pairRowsPerScale, result.pairRows), (8969L, 35876L))
      val participants = get[Vector[Json]](s, "participants")
      assertEquals(result.participants.size, participants.size)
      result.participants.zip(participants).foreach { (p, json) =>
        val c = json.hcursor
        assertEquals(
          p,
          ParticipantCounts(
            get[String](c, "id"),
            get[Int](c, "requested"),
            get[Int](c, "contributing"),
            get[Int](c, "failed"),
            get[Int](c, "no_match"),
            get[Int](c, "not_admitted")
          )
        )
      }
      val encoded = io.circe.Encoder[ResultSummary].apply(result).asObject.get
      assertEquals(
        encoded.keys.toSet,
        Set(
          "run",
          "revision",
          "dataset",
          "scales",
          "pairRowsPerScale",
          "pairRows",
          "eligibleQueries",
          "contrasts",
          "participants"
        )
      )
      assert(result.participants.map(_.participant).distinct.size == result.participants.size)
  }

  test("the focus query: P17 ret_07 · beach-042 against enc_03 and 19 controls at 2°") {
    val focus    = key("P17", "ret_07")
    val matched  = key("P17", "enc_03")
    val control  = ResultAddress.PairRow(2, PairDesign.Control, focus, key("P17", "enc_01"))
    val atScale1 = ResultAddress.PairRow(1, PairDesign.Control, focus, key("P17", "enc_01"))
    val absent   = ResultAddress.ContrastRow(2, key("P17", "ret_09"))
    for
      fake <- FakeStudyBackend.create[IO](StoryMoment.T2)
      rows <- ok(fake.queries(run7, page(0, 4096))).map(_.rows)
      row = rows.find(_.query == focus).get
      pair  <- ok(fake.inspect(run7, control))
      other <- fake.inspect(run7, atScale1)
      gone  <- ok(fake.inspect(run7, absent))
      trail <- ok(
        fake.provenance(run7, ResultAddress.PairRow(2, PairDesign.Matched, focus, matched))
      )
    yield
      assertEquals(
        (row.item, row.response, row.matched),
        ("beach-042", Response.Remembered, Some(matched))
      )
      assertEquals(row.controls, Some(19))
      assertEquals(
        row.status,
        QueryStatus.Contributing(
          Vector(0.41, 0.58, 0.73, 0.86),
          Vector(0.22, 0.29, 0.35, 0.63),
          Vector(0.19, 0.29, 0.38, 0.23)
        )
      )
      assertEquals(pair, Inspection.Pair(control, "street-112", 0.61))
      assertEquals(
        other,
        Left(BackendError.Unavailable(DiagnosticLocus.Address(atScale1)))
      )
      assertEquals(
        gone,
        Inspection.Unscored(absent, QueryStatus.NotAdmitted(TrialDisposition.Absent))
      )
      assertEquals(
        trail.trail,
        Vector(
          ProvenanceStep.Run(run7),
          ProvenanceStep.Analysis(AnalysisRevision(4)),
          ProvenanceStep.Dataset(DatasetRevision(3)),
          ProvenanceStep.Scale(2, "2°"),
          ProvenanceStep.Design(PairDesign.Matched),
          ProvenanceStep.Trial(focus, "beach-042"),
          ProvenanceStep.Trial(matched, "beach-042")
        )
      )
  }

  test("the focus query's 19 control scores: highest street-112 0.61, mean 0.35") {
    val focus    = key("P17", "ret_07")
    val controls = Vector(
      "enc_01",
      "enc_08",
      "enc_15",
      "enc_02",
      "enc_09",
      "enc_16",
      "enc_10",
      "enc_17",
      "enc_04",
      "enc_11",
      "enc_18",
      "enc_05",
      "enc_12",
      "enc_19",
      "enc_06",
      "enc_13",
      "enc_20",
      "enc_07",
      "enc_14"
    )
    for
      fake   <- FakeStudyBackend.create[IO](StoryMoment.T2)
      scores <- controls.traverse { t =>
        ok(
          fake.inspect(run7, ResultAddress.PairRow(2, PairDesign.Control, focus, key("P17", t)))
        )
          .map {
            case Inspection.Pair(_, item, score) => (item, score)
            case other                           => fail(s"not a pair: $other")
          }
      }
    yield
      assertEquals(scores.size, 19)
      assertEquals(scores.maxBy(_._2), ("street-112", 0.61))
      // FIXTURE.md rounds the mean to two places; B at 2° is the same mean.
      assertEqualsDouble(scores.map(_._2).sum / 19, 0.35, 0.005)
  }

  test("only run 7 has scores; data r2 is not in the fixture") {
    for
      fake <- FakeStudyBackend.create[IO](StoryMoment.T3)
      r5   <- fake.result(RunId(5))
      r8   <- fake.queries(RunId(8), page(0, 1))
      r2   <- fake.admission(DatasetRevision(2))
    yield
      assertEquals(r5, Left(BackendError.NoResult(RunId(5), RunState.Stale)))
      assertEquals(r8, Left(BackendError.NoResult(RunId(8), RunState.Running(JobId(1)))))
      assertEquals(
        r2,
        Left(BackendError.Unavailable(DiagnosticLocus.Dataset(DatasetRevision(2))))
      )
  }

  test(
    "a declared rev 4 run on r3 serves the fixture's scores; a conflicting declaration is refused"
  ) {
    val rev4 = AnalysisRevision(4)
    val r2   = DatasetRevision(2)
    val r3   = DatasetRevision(3)
    for
      fake     <- FakeStudyBackend.create[IO](StoryMoment.T1)
      conflict <- fake.declare(AnalysisRevision(3), r3)
      _        <- ok(fake.declare(rev4, r3))
      again    <- fake.declare(rev4, r3)
      status   <- ok(fake.submit(rev4))
      before   <- fake.result(status.run)
      _        <- ok(fake.complete(status.job))
      after    <- ok(fake.result(status.run))
    yield
      assertEquals(
        conflict,
        Left(FakeControlError.RevisionConflict(AnalysisRevision(3), r2, r3))
      )
      assertEquals(again, Right(()))
      assertEquals(status.run, RunId(6))
      assertEquals(before, Left(BackendError.NoResult(RunId(6), RunState.Running(status.job))))
      assertEquals((after.run, after.revision, after.dataset), (RunId(6), rev4, r3))
      assertEquals(after.pairRows, 35876L)
  }

  test("the fake serves an undeclared or a refused inventory when a test asks (S5.4)") {
    val r2    = DatasetRevision(2)
    val r3    = DatasetRevision(3)
    val issue = InventoryIssue.Field(4, "Trial", "", "a trial id")
    for
      fake       <- FakeStudyBackend.create[IO](StoryMoment.T2)
      joined     <- ok(fake.admission(r3))
      _          <- fake.serveInventory(r3, InventoryScenario.Undeclared)
      undeclared <- ok(fake.admission(r3))
      entries    <- ok(fake.ledger(r3, page(0, 4096)))
      _          <- fake.serveInventory(r3, InventoryScenario.Refused(Vector(issue)))
      refused    <- fake.admission(r3)
      noLedger   <- fake.ledger(r3, page(0, 1))
      other      <- fake.admission(r2)
    yield
      assertEquals(joined.inventory, InventoryJoin.Joined(960, 6))
      assertEquals((undeclared.inventory, undeclared.absent), (InventoryJoin.Undeclared, None))
      assertEquals(
        undeclared.copy(inventory = joined.inventory, equation = joined.equation),
        joined
      )
      assertEquals((entries.page.total, entries.entries.size), (954, 954))
      assert(!entries.entries.exists(_.disposition == TrialDisposition.Absent))
      assertEquals(refused, Left(BackendError.InventoryRefused(r3, Vector(issue))))
      assertEquals(noLedger, Left(BackendError.InventoryRefused(r3, Vector(issue))))
      // A scenario is per dataset; r2 is still answered as before.
      assertEquals(other, Left(BackendError.Unavailable(DiagnosticLocus.Dataset(r2))))
  }

  test(
    "verify answers only for content the fake holds: the story's own, or what it is told (S5.6)"
  ) {
    import eyes4s.studio.core.backend.ProtocolSamples.content
    val r3    = DatasetRevision(3)
    val story = StoryMoments.t2
      .flatMap(_.dataset(r3).toRight("no r3"))
      .flatMap(
        eyes4s.studio.core.document.DatasetRevisionSpec.contentDigest(_).left.map(_.message)
      )
      .fold(e => fail(e), identity)
    for
      fake    <- FakeStudyBackend.create[IO](StoryMoment.T2)
      counts  <- ok(fake.admission(r3))
      own     <- ok(fake.verify(r3, story))
      other   <- fake.verify(r3, content("cd"))
      _       <- fake.holdContent(r3, content("cd"))
      told    <- ok(fake.verify(r3, content("cd")))
      _       <- fake.forgetContent(r3)
      unheld  <- fake.verify(r3, story)
      unknown <- fake.verify(DatasetRevision(9999), content("ab"))
    yield
      // The story revision's own content is held from the start.
      assertEquals((own, told), (counts, counts))
      assertEquals(other, Left(BackendError.ContentMismatch(r3, content("cd"), story)))
      assertEquals(
        other.left.map(_.message),
        Left(
          s"Dataset r3 holds content ${story.display}; the request verifies ${content("cd").display}."
        )
      )
      // Nothing is verified by default.
      assertEquals(unheld, Left(BackendError.ContentNotHeld(r3, story)))
      assert(unknown.left.exists(_.isInstanceOf[BackendError.UnknownDataset]), unknown)
  }

  test(
    "placement: every golden record placed by eyes4s, its trials' tallies and density (S5.5)"
  ) {
    val r3 = StoryMoments.t2
      .flatMap(_.dataset(DatasetRevision(3)).toRight("no r3"))
      .fold(e => fail(e), identity)
    for
      fake    <- FakeStudyBackend.create[IO](StoryMoment.T2)
      preview <- ok(fake.placement(r3))
      // A draft with a recorded correction is placed too: P05 shifted right.
      shifted = r3.copy(admission =
        r3.admission.copy(corrections =
          Vector(
            eyes4s.studio.core.document.CorrectionRule(
              eyes4s.studio.core.document.CorrectionTarget.Participant(
                eyes4s.studio.core.document.ParticipantId
                  .of("P05")
                  .fold(e => fail(e.message), identity)
              ),
              eyes4s.studio.core.document.CoordinateCorrection.FlipX
            )
          )
        )
      )
      draft <- ok(fake.placement(shifted.copy(id = DatasetRevision(9))))
      other <- fake.placement(
        r3.copy(
          sources = eyes4s.studio.core.document.Sources
            .of(
              Vector(
                r3.sources.fixations.get
                  .copy(bytes = eyes4s.codec.ByteDigest.sha256(IArray.from("x".getBytes)))
              )
            )
            .fold(e => fail(e.message), identity),
          inventory = None
        )
      )
    yield
      // The fixture's counts: 543 of 11,520 records outside the frame, in 409 trials.
      assertEquals(preview.records.size, 11520)
      assertEquals(preview.records.count(_.placement == RecordPlacement.OutsideWindow), 543)
      assertEquals(preview.trials.count(_.outsideWindow > 0), 409)
      assertEquals(preview.trials.map(_.records).sum, 11520)
      assertEquals(preview.density.placed, 11520)
      // Record 7,214 (fixation 6 of P17 enc_03): image (700, 300), (+5.4°, +2.4°).
      val focus = preview.record(7214).getOrElse(fail("no record 7214"))
      assertEquals((focus.imageX, focus.imageY), (700.0, 300.0))
      assert(
        focus.degrees.exists((x, y) => math.abs(x - 5.4) < 0.05 && math.abs(y - 2.4) < 0.05),
        focus
      )
      // The draft is placed under its own rules: every P05 record corrected by rule 0.
      assertEquals(draft.dataset, DatasetRevision(9))
      val p05 = draft.records.filter(_.trial.participant == "P05")
      assert(p05.nonEmpty && p05.forall(_.rule.contains(0)))
      assert(p05.forall(r => r.correctedX == r3.geometry.screen.width - r.rawX), p05.take(2))
      // Another fixation file is refused, naming both digests.
      assert(
        other.left.exists {
          case BackendError.PlacementRefused(DatasetRevision(3), reason) =>
            reason.contains("this backend holds sha256:")
          case _ => false
        },
        other
      )
  }
