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
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.reports.ReportRefusal
import cats.syntax.all.*
import eyes4s.studio.core.selection.{ReportCount, ReportGroup, ScaleIndex, StudioRef}
import munit.CatsEffectSuite

/** Reports (protocol 1.11, bead bd-01M43VP5YV6WB4VVQEW2CJTB9V) on the fake
  * backend at story moment t2: a reporting spec evaluated over run 7 by
  * eyes4s-results, as edited. The story's spec reproduces the fixture's own
  * served means; an edit changes the cells, n, dropped cells and filter
  * counts, each with its ref.
  */
class ReportsSuite extends CatsEffectSuite:
  import StoryMoments.run7

  /** The fixture's means are written to two decimals; eyes4s's are exact. */
  private val FixtureRounding: Double = 0.005 + 1e-9

  private def fake = FakeStudyBackend.create[IO](StoryMoment.T2)

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private def spec(
      groupBy: Option[String] = Some("response"),
      filters: Vector[ReportingFilter] = Vector.empty,
      minimum: Option[Int] = None,
      weighting: ReportingWeight = ReportingWeight.ParticipantMeans
  ): ReportingSpec =
    ok(
      ReportingSpec.of(
        ok(ReportingId.of("by-retrieval-response")),
        "By retrieval response",
        groupBy.map(c => ok(Covariate.of(c))),
        filters,
        minimum.map(m => ok(MinimumPerGroup.of(m))),
        weighting,
        groupBy.map(_ => ok(ReportingContrast.of("Remembered", "Forgotten")))
      )
    )

  private val remembered = Some(Response.Remembered)
  private val forgotten  = Some(Response.Forgotten)
  private val scale2     = 2

  test("the story's spec reproduces the summary's group means and participant n") {
    for
      b <- fake
      summary = ok(MockStudy.load).summary
      report <- b.report(run7, spec(), scale2).map(ok)
    yield
      for g <- summary.groups do
        val cell = report.cell(Some(g.label), ReportRole.Difference).get
        assertEqualsDouble(cell.estimate.get, g.d, FixtureRounding, g.label)
        assertEquals(cell.participants, g.n, g.label)
        assertEquals(
          cell.ref,
          StudioRef.ReportCell(
            run7,
            spec().id,
            ok(ScaleIndex.of(scale2)),
            ReportGroup.Level(g.label),
            ReportRole.Difference
          )
        )
      // M, B and D of one group are three values, so three refs.
      val refs = report.cells.filter(_.group == forgotten).map(_.ref)
      assertEquals(refs.size, 3)
      assertEquals(refs.distinct.size, 3)
      // Each participant's difference is the summary's, with its query count.
      for
        p <- summary.participants
        g <- p.groups
      do
        val mine = report.participant(Some(g.label), ReportRole.Difference, p.participant).get
        assertEquals(mine.queries, g.n, s"${p.participant} ${g.label.label}")
        assertEqualsDouble(
          mine.value.get,
          g.d,
          FixtureRounding,
          s"${p.participant} ${g.label.label}"
        )
      assertEquals(report.dropped, Vector.empty)
  }

  test("every role's participant values are served, each with its own ref") {
    for
      b <- fake
      summary = ok(MockStudy.load).summary
      report <- b.report(run7, spec(), scale2).map(ok)
    yield
      val at = ok(ScaleIndex.of(scale2))
      for
        p    <- summary.participants
        g    <- p.groups
        role <- ReportRole.values
      do
        val mine     = report.participant(Some(g.label), role, p.participant).get
        val expected = role match
          case ReportRole.Matched    => g.m
          case ReportRole.Control    => g.b
          case ReportRole.Difference => g.d
        val what = s"${p.participant} ${g.label.label} $role"
        assertEqualsDouble(mine.value.get, expected, FixtureRounding, what)
        assertEquals(mine.queries, g.n, what)
        assertEquals(
          mine.ref,
          StudioRef.ReportParticipant(
            run7,
            spec().id,
            at,
            ReportGroup.Level(g.label),
            role,
            p.participant
          ),
          what
        )
        assertEquals(
          mine.ref.parent,
          Some(StudioRef.ReportCell(run7, spec().id, at, ReportGroup.Level(g.label), role))
        )
      assertEquals(report.participants.map(_.ref).distinct.size, report.participants.size)
  }

  test("the level contrast is eyes4s's: Remembered − Forgotten, its paired n, with a ref") {
    for
      b <- fake
      summary = ok(MockStudy.load).summary
      report    <- b.report(run7, spec(), scale2).map(ok)
      ungrouped <- b.report(run7, spec(groupBy = None), scale2).map(ok)
    yield
      val c = report.contrast(ReportRole.Difference).get
      assertEquals((c.minuend, c.subtrahend), (Response.Remembered, Response.Forgotten))
      assertEquals(c.pairedN, summary.pairedN)
      assertEquals(c.pairedN, 24)
      assertEquals(c.unpaired, Vector.empty)
      // The mean over participants with both levels of their own difference.
      val diffs = summary.participants.flatMap { p =>
        for
          r <- p.groups.find(_.label == Response.Remembered)
          f <- p.groups.find(_.label == Response.Forgotten)
        yield r.d - f.d
      }
      assertEquals(diffs.size, 24)
      assertEqualsDouble(c.estimate.get, diffs.sum / diffs.size, 2 * FixtureRounding)
      assertEquals(
        c.ref,
        StudioRef.ReportContrast(
          run7,
          spec().id,
          ok(ScaleIndex.of(scale2)),
          ReportRole.Difference,
          Response.Remembered,
          Response.Forgotten
        )
      )
      assertEquals(report.contrasts.map(_.role), ReportRole.values.toVector)
      // No grouping, no contrast.
      assertEquals(ungrouped.contrasts, Vector.empty)
  }

  test("the queries-per-participant range is eyes4s's, the summary's 2 to 17") {
    for
      b <- fake
      summary = ok(MockStudy.load).summary
      report <- b.report(run7, spec(), scale2).map(ok)
    yield
      val d = report.queryRange(ReportRole.Difference).get
      assertEquals((d.fewest, d.most), (summary.groupNRange._1, summary.groupNRange._2))
      assertEquals((d.fewest, d.most), (2, 17))
      assertEquals(
        d.ref,
        StudioRef.ReportQueryRange(
          run7,
          spec().id,
          ok(ScaleIndex.of(scale2)),
          ReportRole.Difference
        )
      )
      // The range is the extremes of the participant query counts served.
      val counts = report.participants.filter(_.role == ReportRole.Difference).map(_.queries)
      assertEquals((counts.min, counts.max), (d.fewest, d.most))
  }

  test("a minimum per group drops the participants below it, named, and changes n") {
    for
      b      <- fake
      report <- b.report(run7, spec(minimum = Some(3)), scale2).map(ok)
    yield
      val dropped = report.dropped.map(d => (d.participant, d.group, d.queries, d.required))
      assertEquals(dropped, Vector(("P17", forgotten, 2, 3), ("P21", forgotten, 2, 3)))
      assertEquals(
        report.dropped.head.ref,
        StudioRef.ReportParticipant(
          run7,
          spec().id,
          ok(ScaleIndex.of(scale2)),
          ReportGroup.Level(Response.Forgotten),
          ReportRole.Difference,
          "P17"
        )
      )
      assertEquals(report.cell(forgotten, ReportRole.Difference).map(_.participants), Some(22))
      assertEquals(report.cell(remembered, ReportRole.Difference).map(_.participants), Some(24))
      assertEquals(report.tally(ReportRole.Difference, ReportCount.BelowMinimum), Some(2))
  }

  test("a keep filter is counted where the queries went; the accounting adds up") {
    val keep = ReportingFilter.Keep(
      ok(Covariate.of("response")),
      ok(ValueSet.of(ok(Covariate.of("response")), Vector("Remembered")))
    )
    for
      b    <- fake
      rows <- b
        .queries(
          run7,
          PageRequest.of(0, PageRequest.MaximumSize).fold(e => fail(e.message), identity)
        )
        .map(ok(_).rows)
      report <- b.report(run7, spec(filters = Vector(keep)), scale2).map(ok)
    yield
      val d = (c: ReportCount) => report.tally(ReportRole.Difference, c).get
      assertEquals(
        d(ReportCount.Eligible),
        d(ReportCount.Kept) + d(ReportCount.FilteredOut) + d(ReportCount.UnknownPredicate) + d(
          ReportCount.Failed
        )
      )
      // Every eligible Forgotten query is filtered out, whatever its outcome.
      val forgottenEligible = rows.count(r =>
        r.response == Response.Forgotten && !r.status.isInstanceOf[QueryStatus.NotAdmitted]
      )
      assertEquals(d(ReportCount.FilteredOut), forgottenEligible)
      assertEquals(report.cell(forgotten, ReportRole.Difference).flatMap(_.estimate), None)
      assertEquals(
        report.cell(forgotten, ReportRole.Difference).flatMap(_.absence),
        Some(ReportAbsence.EmptyGroup)
      )
      assertEquals(
        report.tallies.find(_.count == ReportCount.FilteredOut).map(_.ref.parent),
        Some(
          Some(
            StudioRef.ReportTally(
              run7,
              spec().id,
              ok(ScaleIndex.of(scale2)),
              ReportRole.Matched,
              ReportCount.Eligible
            )
          )
        )
      )
  }

  /** Each eligible query's outside-window share of fixation duration, from
    * the fixations the trial view serves; None when the trial has none.
    */
  private def shares(b: FakeStudyBackend[IO], rows: Vector[QueryRow]) =
    rows
      .filterNot(_.status.isInstanceOf[QueryStatus.NotAdmitted])
      .traverse[IO, (QueryRow, Option[Double])] { r =>
        b.trialFixations(AnalysisRevision(4), r.query).map { v =>
          val f =
            ok(v).fixations.filterNot(_.placement == eyes4s.plan.MapPlacement.DroppedInitial)
          val total   = f.map(_.durationMs).sum
          val outside = f
            .filter(_.placement.isInstanceOf[eyes4s.plan.MapPlacement.OutsideWindow])
            .map(_.durationMs)
            .sum
          (r, Option.when(total > 0)(outside / total))
        }
      }

  test("the outside-window filter's counts are eyes4s's own, failed queries included") {
    val window = ReportingFilter.OutsideWindowAtMost(ok(Share.of(0.05)))
    for
      b    <- fake
      rows <- b
        .queries(
          run7,
          PageRequest.of(0, PageRequest.MaximumSize).fold(e => fail(e.message), identity)
        )
        .map(ok(_).rows)
      shared <- shares(b, rows)
      report <- b.report(run7, spec(filters = Vector(window)), scale2).map(ok)
      plain  <- b.report(run7, spec(), scale2).map(ok)
    yield
      val d = (r: ReportView, c: ReportCount) => r.tally(ReportRole.Difference, c).get
      // Every eligible query whose share exceeds the threshold, whatever its
      // outcome: a failed query outside the window is filtered out too.
      val outside = shared.collect { case (r, Some(share)) if share > 0.05 => r }
      // Outside the window but never kept: failed or with no stored D.
      val unscored = outside.count(r => !r.status.isInstanceOf[QueryStatus.Contributing])
      assertEquals(d(report, ReportCount.OutsideWindowFiltered), outside.size)
      assertEquals(d(report, ReportCount.OutsideWindowFiltered), 194)
      assert(
        outside.exists(_.status.isInstanceOf[QueryStatus.Failed]),
        "the fixture has failed queries outside the window"
      )
      // The difference of kept counts misses them: it is not the count.
      val subtraction = d(plain, ReportCount.Kept) - d(report, ReportCount.Kept)
      assertEquals(unscored, 5)
      assertEquals(subtraction, 194 - unscored)
      assertEquals(d(report, ReportCount.OutsideWindowUnknown), shared.count(_._2.isEmpty))
      assertEquals(d(report, ReportCount.OutsideWindowUnknown), 0)
      // The window filter alone, here the spec's only filter, is the spec.
      assertEquals(d(report, ReportCount.FilteredOut), 194)
      assertEquals(
        report.tallies
          .find(t =>
            t.role == ReportRole.Difference && t.count == ReportCount.OutsideWindowFiltered
          )
          .map(_.ref),
        Some(
          StudioRef.ReportTally(
            run7,
            spec().id,
            ok(ScaleIndex.of(scale2)),
            ReportRole.Difference,
            ReportCount.OutsideWindowFiltered
          )
        )
      )
      assertEquals(plain.tally(ReportRole.Difference, ReportCount.OutsideWindowFiltered), None)
  }

  test("with a keep filter too, the window's counts are its alone") {
    val window = ReportingFilter.OutsideWindowAtMost(ok(Share.of(0.05)))
    val keep   = ReportingFilter.Keep(
      ok(Covariate.of("response")),
      ok(ValueSet.of(ok(Covariate.of("response")), Vector("Remembered")))
    )
    for
      b     <- fake
      both  <- b.report(run7, spec(filters = Vector(keep, window)), scale2).map(ok)
      alone <- b.report(run7, spec(filters = Vector(window)), scale2).map(ok)
    yield
      val d = (r: ReportView, c: ReportCount) => r.tally(ReportRole.Difference, c).get
      assertEquals(d(both, ReportCount.OutsideWindowFiltered), 194)
      assertEquals(
        d(both, ReportCount.OutsideWindowFiltered),
        d(alone, ReportCount.OutsideWindowFiltered)
      )
      assert(d(both, ReportCount.FilteredOut) > 194, both.tallies)
  }

  test("a query whose share is undefined is counted as undecided by the window, not filtered") {
    import eyes4s.results.*
    val window               = ReportingFilter.OutsideWindowAtMost(ok(Share.of(0.05)))
    given Ordering[TrialKey] = Ordering.by(k => (k.participant, k.phase.label, k.trial))
    val key                  = (t: String) => TrialKey("P01", Phase.Retrieval, t, 1)
    def query(t: String, share: Value[Double], d: RoleOutcome) = ok(
      Query.of(
        key(t),
        "P01",
        "beach",
        "Retrieval",
        1,
        Vector.empty,
        Vector.empty,
        Vector(WindowMeasure.OutsideWindowShare -> share),
        d,
        d,
        d
      )
    )
    val scored  = RoleOutcome.Scored(Vector(0.2))
    val queries = Vector(
      query("in", Value.Present(0.01), scored),
      query("out", Value.Present(0.5), scored),
      query("zero", Value.Missing(Absence.Undefined(UndefinedReason.ZeroDuration)), scored),
      query("none", Value.Missing(Absence.NotRecorded), scored)
    )
    val table  = ok(QueryTable.of(0, Vector("value"), CovariateSchema.empty, queries))
    val source =
      ok(eyes4s.codec.ReportSources.tables(io.circe.Encoder[TrialKey])(Vector(table), None))
    val view = ok(
      eyes4s.studio.core.reports.ReportEvaluation
        .evaluate(run7, spec(groupBy = None, filters = Vector(window)), 0, Map.empty, source)
    )
    val d = (c: ReportCount) => view.tally(ReportRole.Difference, c).get
    assertEquals(d(ReportCount.OutsideWindowFiltered), 1)
    assertEquals(d(ReportCount.OutsideWindowUnknown), 2)
    assertEquals(d(ReportCount.Kept), 1)
    // An ungrouped report's cells are the whole report's, one per role.
    assertEquals(
      view.cells.map(_.ref).toSet,
      ReportRole.values.toSet.map(r =>
        StudioRef.ReportCell(run7, spec().id, ok(ScaleIndex.of(0)), ReportGroup.Whole, r)
      )
    )
  }

  test("a stored failure whose code is not a diagnostic code is refused, named") {
    val study   = ok(MockStudy.load)
    val q       = study.queries.head
    val broken  = eyes4s.studio.core.backend.ProtocolSamples.diagnostic.copy(code = "nodot")
    val refused =
      FakeReports.source("r", StoryMoment.T2, Vector(q -> QueryStatus.Failed(broken)), 4)
    assertEquals(
      refused.map(_ => ()),
      Left(ReportRefusal.UnreadableFailure("r", q.key, "nodot"))
    )
    assert(
      ReportRefusal.UnreadableFailure("r", q.key, "nodot").message.contains(q.key.trial),
      "names the query"
    )
  }

  test("stored failed-scale diagnostics retain their exact per-scale operands") {
    val study  = ok(MockStudy.load)
    val query  = study.queries.head
    val first  = eyes4s.studio.core.backend.ProtocolSamples.diagnostic
    val second = first.copy(message = "second scale has different operands")
    val source = ok(
      FakeReports.source(
        "scaled",
        StoryMoment.T2,
        Vector(query -> QueryStatus.FailedAtScales(Vector(first, second))),
        2
      )
    )
    for (scale, diagnostic) <- Vector(first, second).zipWithIndex.map((d, i) => (i, d)) do
      val table = ok(source.queries(scale))
      assertEquals(
        table.queries.head.matched,
        eyes4s.results.RoleOutcome.Failed(
          ok(eyes4s.plan.DiagnosticCode.host("study-failure", "off-window")),
          diagnostic.message
        )
      )
    assert(
      FakeReports
        .source(
          "scaled",
          StoryMoment.T2,
          Vector(query -> QueryStatus.FailedAtScales(Vector(first))),
          2
        )
        .isLeft
    )
  }

  test("a run with no queries is an empty source, not a defect") {
    assert(FakeReports.source("r", StoryMoment.T2, Vector.empty, 4).isRight)
  }

  test("an unknown scale, a run without rows and an undeclared covariate are refused") {
    for
      b <- fake
      summary = ok(MockStudy.load).summary
      scale <- b.report(run7, spec(), summary.scales.size)
      norun <- b.report(StoryMoments.run5, spec(), scale2)
      cov   <- b.report(run7, spec(groupBy = Some("confidence")), scale2)
    yield
      assertEquals(
        scale,
        Left(BackendError.UnknownScale(run7, summary.scales.size, summary.scales))
      )
      assert(norun.isLeft, norun)
      assertEquals(
        cov,
        Left(
          BackendError.ReportRefused(
            run7,
            ReportRefusal.UndeclaredCovariate(
              "by-retrieval-response",
              "confidence",
              Vector("response")
            )
          )
        )
      )
  }

  test("pooled queries weigh every query equally; participant means weigh participants") {
    for
      b    <- fake
      rows <- b
        .queries(
          run7,
          PageRequest.of(0, PageRequest.MaximumSize).fold(e => fail(e.message), identity)
        )
        .map(ok(_).rows)
      pooled <- b
        .report(run7, spec(groupBy = None, weighting = ReportingWeight.PooledQueries), scale2)
        .map(ok)
      means <- b.report(run7, spec(groupBy = None), scale2).map(ok)
    yield
      val ds = rows.collect { case QueryRow(_, _, _, _, _, QueryStatus.Contributing(_, _, d)) =>
        d(scale2)
      }
      val all = pooled.cell(None, ReportRole.Difference).get
      assertEquals(all.queries, ds.size)
      assertEqualsDouble(all.estimate.get, ds.sum / ds.size, 1e-12)
      assertNotEquals(means.cell(None, ReportRole.Difference).get.estimate, all.estimate)
  }
