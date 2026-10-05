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
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.core.figures.{MethodsReadError, MethodsReads}
import munit.CatsEffectSuite

/** The fake backend's pair rows (protocol 1.9, S9.5) at story moment t2: run
  * 7's every compared query, its matched pair scored with its M, its control
  * pairs scored where the fixture holds them (P17 ret_07 at 2°) and not
  * served otherwise, and a failed query's pairs carrying its diagnostic.
  */
class PairRowsSuite extends CatsEffectSuite:
  import StoryMoments.run7

  private def fake = FakeStudyBackend.create[IO](StoryMoment.T2)

  private def ok[A](e: Either[BackendError, A]): A = e.fold(x => fail(x.message), identity)

  private def page(offset: Int): PageRequest =
    PageRequest.of(offset, PageRequest.MaximumSize).fold(e => fail(e.message), identity)

  private def all(b: FakeStudyBackend[IO], scale: Int): IO[Vector[PairRowEntry]] =
    def from(offset: Int, got: Vector[PairRowEntry]): IO[Vector[PairRowEntry]] =
      b.pairRows(run7, scale, page(offset)).flatMap { e =>
        val p = ok(e)
        p.page.next.fold(IO.pure(got ++ p.rows))(from(_, got ++ p.rows))
      }
    from(0, Vector.empty)

  test("every scale has the summary's pair rows, one matched pair per compared query") {
    for
      b       <- fake
      summary <- b.result(run7).map(ok)
      queries <- b.queries(run7, page(0)).map(ok)
      rows    <- summary.scales.indices.toVector.traverse(all(b, _))
    yield
      rows.foreach(r => assertEquals(r.size.toLong, summary.pairRowsPerScale))
      assertEquals(rows.map(_.size.toLong).sum, summary.pairRows)
      val compared = queries.rows.filter(q =>
        q.status match
          case QueryStatus.Contributing(_, _, _) | QueryStatus.Failed(_) => true
          case _                                                         => false
      )
      val matched = rows.head.filter(_.design == PairDesign.Matched)
      assertEquals(
        matched.map(r => (r.query, r.reference)),
        compared.map(q => (q.query, q.matched))
      )
      // Controls per query are the query rows' served counts.
      assertEquals(
        rows.head.filter(_.design == PairDesign.Control).groupMapReduce(_.query)(_ => 1)(_ + _),
        compared.map(q => q.query -> q.controls.get).toMap
      )
  }

  test("scores are the run's: M for the matched pair, the fixture's controls, failures") {
    for
      b    <- fake
      rows <- all(b, 2)
      q    <- b.queries(run7, page(0)).map(ok)
      p17 = MockStudy.key("P17", "ret_07")
      insp <- rows
        .filter(r => r.query == p17)
        .traverse(r =>
          b.inspect(run7, ResultAddress.PairRow(2, r.design, r.query, r.reference))
            .map(r -> _)
        )
    yield
      // P17 ret_07 at 2°: every pair the inspector scores, the rows score alike.
      insp.foreach {
        case (r, Right(Inspection.Pair(_, item, score))) =>
          assertEquals((r.referenceItem, r.score), (item, PairScoreState.Scored(score)))
        case (r, other) => fail(s"$r: $other")
      }
      q.rows.foreach { row =>
        val mine = rows.filter(_.query == row.query)
        row.status match
          case QueryStatus.Contributing(m, _, _) =>
            assertEquals(mine.head.score, PairScoreState.Scored(m(2)))
          case QueryStatus.Failed(d) =>
            assert(mine.nonEmpty && mine.forall(_.score == PairScoreState.Failed(d)), mine)
          case _ => assertEquals(mine, Vector.empty)
      }
      // Elsewhere the fixture holds no control scores, and says so.
      assert(rows.exists(_.score == PairScoreState.NotServed))
  }

  test(
    "the fixture's control scores are P17 ret_07's at 2° only; every other scale is not served"
  ) {
    for
      b    <- fake
      rows <- (0 until 4).toVector.traverse(all(b, _))
    yield
      val p17 = MockStudy.key("P17", "ret_07")
      for (scale, i) <- rows.zipWithIndex do
        val controls = scale.filter(r => r.query == p17 && r.design == PairDesign.Control)
        assert(controls.nonEmpty)
        if i == 2 then
          assert(controls.forall(_.score.isInstanceOf[PairScoreState.Scored]), controls)
        else
          assert(controls.forall(_.score == PairScoreState.NotServed), s"scale $i: $controls")
  }

  test("a page past the end is empty and last; a negative scale is refused") {
    for
      b     <- fake
      first <- b.pairRows(run7, 0, page(0)).map(ok)
      past  <- b.pairRows(run7, 0, page(first.page.total + 10)).map(ok)
      neg   <- b.pairRows(run7, -1, page(0))
      sum   <- b.result(run7).map(ok)
    yield
      assertEquals((past.rows, past.page.next), (Vector.empty, None))
      assertEquals(neg, Left(BackendError.UnknownScale(run7, -1, sum.scales)))
  }

  // --- Reading every page (eyes4s.studio.core.figures.MethodsReads.pairRows) ---------

  private def read(
      b: FakeStudyBackend[IO],
      tweak: (Int, PageRequest, PairRowPage) => PairRowPage
  ): IO[Either[MethodsReadError, Vector[PairRowPage]]] =
    MethodsReads.pairRows[IO](
      (r, s, p) => b.pairRows(r, s, p).map(_.map(tweak(s, p, _))),
      run7,
      1
    )

  /** Pages of at most 1,000 rows, as a backend may serve them. */
  private def small(page: PageRequest, p: PairRowPage): PairRowPage =
    val end = page.offset + 1000
    p.copy(
      rows = p.rows.take(1000),
      page = p.page.copy(next = Option.when(end < p.page.total)(end))
    )

  test("every page of a scale is read as served, its PageInfo kept") {
    for
      b     <- fake
      whole <- read(b, (_, _, p) => p)
      paged <- read(b, (_, req, p) => small(req, p))
    yield
      val pages = paged.fold(e => fail(e.message), identity)
      assertEquals(pages.size, 9)
      assertEquals(pages.map(_.page.offset), (0 until 9).toVector.map(_ * 1000))
      assertEquals(
        pages.flatMap(_.rows),
        whole.fold(e => fail(e.message), identity).flatMap(_.rows)
      )
  }

  test("a read that would drop rows is refused, naming the run, scale and counts") {
    for
      b       <- fake
      short   <- read(b, (_, _, p) => p.copy(rows = p.rows.dropRight(1)))
      stalled <- read(
        b,
        (_, req, p) => small(req, p).copy(page = p.page.copy(next = Some(req.offset)))
      )
      other <- read(b, (s, _, p) => p.copy(scale = s + 1))
      run   <- read(b, (_, _, p) => p.copy(run = StoryMoments.run5))
    yield
      // Three pages of at most 4,096, each one row short.
      assertEquals(short, Left(MethodsReadError.PairsShort(run7, 0, 8966, 8969)))
      assertEquals(stalled, Left(MethodsReadError.PairsStalled(run7, 0, 0, 0)))
      assertEquals(other, Left(MethodsReadError.OtherPairPage(run7, 0, run7, 1)))
      assertEquals(run, Left(MethodsReadError.OtherPairPage(run7, 0, StoryMoments.run5, 0)))
  }

  // --- Window tallies (protocol 1.12, bd-01M44FKT4NFZ0F5KQTKT96DC8E) ------------------

  test("each pair carries its query's and reference's window tally, with the trial's ref") {
    for
      b    <- fake
      rows <- all(b, 2)
    yield
      val p17  = MockStudy.key("P17", "ret_07")
      val enc  = MockStudy.key("P17", "enc_03")
      val pair = rows.find(r => r.query == p17 && r.design == PairDesign.Matched).get
      assertEquals(pair.reference, enc)
      val q = pair.queryWindow.getOrElse(fail("no query tally"))
      val r = pair.referenceWindow.getOrElse(fail("no reference tally"))
      assertEquals((q.trial, r.trial), (StudioRef.Trial(p17), StudioRef.Trial(enc)))
      // FIXTURE.md: ret_07 has 1 of 12 fixations outside (4% of duration),
      // enc_03 1 of 13 (3%).
      assertEquals((q.tally.outsideWindow, q.tally.total), (1, 12))
      assertEquals((r.tally.outsideWindow, r.tally.total), (1, 13))
      assertEquals(q.tally.outsideWindowShare.map(s => math.round(s * 100)), Some(4L))
      assertEquals(r.tally.outsideWindowShare.map(s => math.round(s * 100)), Some(3L))
      // Every compared pair's trials have admitted scanpaths, so a tally.
      assert(rows.forall(r => r.queryWindow.isDefined && r.referenceWindow.isDefined))
      // One tally per trial: a trial's tally is the same in every row it is in.
      assertEquals(
        rows.groupBy(_.reference).values.map(_.map(_.referenceWindow).distinct.size).toSet,
        Set(1)
      )
  }

  test("a pair's tallies count the fixations the trial view serves") {
    for
      b    <- fake
      rows <- all(b, 0)
      view <- b.trialFixations(AnalysisRevision(4), rows.head.reference).map(ok)
    yield
      val t = rows.head.referenceWindow.get.tally
      assertEquals(t.total, view.fixations.size)
      assertEquals(
        t.outsideWindow,
        view.fixations.count(_.placement.isInstanceOf[eyes4s.plan.MapPlacement.OutsideWindow])
      )
  }
