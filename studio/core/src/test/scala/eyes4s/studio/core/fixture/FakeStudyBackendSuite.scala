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
          assertEquals(row.matched, key(id, get[String](c, "match")))
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
                  val matched  = ResultAddress.PairRow(s, PairDesign.Matched, k, row.matched)
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

  test("participant summaries and grand means are fixture.json's, grouped by response") {
    val s = raw.hcursor.downField("summary")
    for
      fake   <- FakeStudyBackend.create[IO](StoryMoment.T2)
      result <- ok(fake.result(run7))
    yield
      assertEquals(result.grandD, get[Double](s, "grand_D_all"))
      assertEquals(result.grandDByScale, get[Vector[Double]](s, "grand_D_by_scale"))
      assertEquals(result.scales, get[Vector[String]](s, "scales"))
      assertEquals((result.pairRowsPerScale, result.pairRows), (8969L, 35876L))
      assertEquals(
        result.groups,
        Vector("Remembered", "Forgotten").map(l =>
          GroupSummary(
            "response",
            Response(l),
            get[Int](s, s"n_$l"),
            get[Double](s, s"grand_D_$l"),
            get[Vector[Double]](s, s"grand_D_by_scale_$l")
          )
        )
      )
      assertEquals((result.pairedN, result.groupNMinimum, result.groupNMaximum), (24, 2, 17))
      val raws = get[Vector[Json]](s, "participants")
      assertEquals(result.participants.size, raws.size)
      result.participants.zip(raws).foreach { (p, j) =>
        val c                   = j.hcursor
        val all                 = c.downField("all")
        def group(name: String) =
          val g = c.downField(name)
          GroupMeans(
            Response(name),
            get[Int](g, "n"),
            get[Double](g, "M"),
            get[Double](g, "B"),
            get[Double](g, "D")
          )
        assertEquals(
          p,
          ParticipantSummary(
            get[String](c, "id"),
            get[Int](c, "requested"),
            get[Int](c, "contributing"),
            get[Int](c, "failed"),
            get[Int](c, "no_match"),
            get[Int](c, "not_admitted"),
            ScoreMeans(
              get[Double](all, "M"),
              get[Double](all, "B"),
              get[Double](all, "D"),
              get[Vector[Double]](all, "D_by_scale")
            ),
            Vector(group("Remembered"), group("Forgotten"))
          )
        )
      }
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
        ("beach-042", Response.Remembered, matched)
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
