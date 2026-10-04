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
