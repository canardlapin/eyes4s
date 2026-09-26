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

package eyes4s.studio.app.nav

import cats.syntax.all.*
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.app.vm.Shell
import eyes4s.studio.core.backend.{Provenance as _, *}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.{MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.navigation.*
import eyes4s.studio.core.selection.{FixationIndex, ScaleIndex, StudioRef}

import scala.concurrent.{ExecutionContext, Future}

/** Explain from any number walks down the provenance chain on the fake
  * backend (S3.4): summary → group → participant → query → pair → map →
  * fixation → record, each step a value or a typed refusal, and every chain
  * ends in a fixations.csv record or a typed MissingSource.
  */
class ExplainNavigationSuite extends munit.FunSuite:
  import StoryModels.*

  private given ExecutionContext = ExecutionContext.global

  private val run7  = StoryMoments.run7
  private val scope = SummaryScope(run7, sigma2)
  private val page  = ok(PageRequest.first(PageRequest.MaximumSize))
  private val study = ok(MockStudy.load)

  /** A fresh session at t2 (run 7 current), closed after `f`. */
  private def withSession[A](f: HeadlessSession => Future[A]): Future[A] =
    HeadlessSession.open(StoryMoment.T2).flatMap { s =>
      f(s).transformWith(result => s.close.transform(_ => result))
    }

  private def children(s: HeadlessSession, place: Place): Future[Vector[Place]] =
    Provenance.children(s.navigator, place, scope, page).map(ok)

  private def fixation(key: TrialKey, i: Int): StudioRef =
    StudioRef.Fixation(key, ok(FixationIndex.of(i)))

  private val summary: Place           = Place.Summary(reporting)
  private val rememberedP17: StudioRef =
    StudioRef.ParticipantSummary(run7, reporting, sigma2, Some(remembered), "P17")

  test(
    "Explain P17 from the summary: the down steps reach P17 and the trail keeps spec and group"
  ) {
    withSession { s =>
      for
        groups <- children(s, summary)
        people <- children(s, Place.Group(reporting, remembered))
      yield
        assertEquals(
          groups,
          Vector(
            Place.Group(reporting, remembered),
            Place.Group(reporting, Response("Forgotten"))
          )
        )
        // FIXTURE.md: Remembered n = 24 participants.
        assertEquals(people.size, 24)
        assert(people.contains(Place.At(rememberedP17)))
        val start  = play(t2Compare, _ => Intent.OpenCrumb(0))
        val (m, _) = AppModel.run(
          start,
          Vector(Intent.Explain(groups.head), Intent.Explain(Place.At(rememberedP17)))
        )
        assertEquals(m.perspective, Perspective.Compare)
        assertEquals(
          Shell.context(m).trail.map(_.label).mkString(" › "),
          "Summary · by retrieval response › Remembered › P17"
        )
    }
  }

  test("P17's Remembered queries are its 17 scored Remembered retrievals, ret_07 among them") {
    withSession { s =>
      children(s, Place.At(rememberedP17)).map { queries =>
        // FIXTURE.md participant table: P17 Rem D (n) = 17.
        assertEquals(queries.size, 17)
        assert(queries.contains(Place.At(query)))
      }
    }
  }

  test("the focus query's pairs are its matched reference, then its 19 controls at 2°") {
    withSession { s =>
      children(s, Place.At(query)).map { pairs =>
        assertEquals(pairs.head, Place.At(pair))
        assertEquals(pairs.size, 20)
        assertEquals(
          pairs.tail.collect { case Place.At(StudioRef.Pair(_, _, d, _, _)) => d }.distinct,
          Vector(PairDesign.Control)
        )
      }
    }
  }

  test("the record crumb resolves to fixations.csv record 7,214 or an explicit MissingSource") {
    withSession { s =>
      val absent = MockStudy.key("P17", "ret_09")
      val nobody = TrialKey("P99", Phase.Encoding, "enc_01", 1)
      for
        maps    <- children(s, Place.At(pair))
        fixes   <- children(s, maps(1))
        focus   <- Provenance.end(s.navigator, fixation(p17enc03, 6))
        beyond  <- Provenance.end(s.navigator, fixation(p17enc03, 14))
        missing <- Provenance.end(s.navigator, fixation(absent, 1))
        unknown <- Provenance.end(s.navigator, fixation(nobody, 1))
      yield
        assertEquals(maps(1), Place.At(StudioRef.TrialMap(run7, sigma2, p17enc03)))
        // FIXTURE.md: enc_03 has 13 fixations; fixation 6 = record 7,214.
        assertEquals(fixes.size, 13)
        assertEquals(fixes(5), Place.At(fixation(p17enc03, 6)))
        assertEquals(focus, Right(ChainEnd.Record(record)))
        assertEquals(
          beyond,
          Right(ChainEnd.Missing(MissingSource.FixationOutOfRange(p17enc03, 14, 13)))
        )
        assertEquals(missing, Right(ChainEnd.Missing(MissingSource.NotAdmitted(absent))))
        assertEquals(unknown, Right(ChainEnd.Missing(MissingSource.UnknownTrial(nobody))))
    }
  }

  test("every chain from the summary ends in its fixation's own record") {
    // Walk every step; pairs share maps, so each map is walked once.
    def walk(s: HeadlessSession, place: Place): Future[Vector[Place]] =
      place match
        case Place.At(StudioRef.TrialMap(_, _, _)) => Future.successful(Vector(place))
        case _ => children(s, place).flatMap(_.flatTraverse(walk(s, _)))
    withSession { s =>
      for
        maps  <- walk(s, summary).map(_.distinct)
        fixes <- maps.flatTraverse(children(s, _))
        ends  <- fixes.traverse {
          case Place.At(f @ StudioRef.Fixation(_, _)) =>
            Provenance.end(s.navigator, f).map(f -> ok(_))
          case other => Future.failed(new AssertionError(s"not a fixation: $other"))
        }
      yield
        // Every map on a scored pair is an admitted trial's, so no chain is
        // missing its source; each fixation has its own record of its trial.
        val records = ends.collect {
          case (
                StudioRef.Fixation(t, i),
                ChainEnd.Record(StudioRef.SourceRecord(rt, Some(ri), _, n))
              ) =>
            assertEquals((rt, ri), (t, i))
            n.value
        }
        assertEquals(
          records.size,
          ends.size,
          ends.filterNot(_._2.isInstanceOf[ChainEnd.Record]).take(3)
        )
        assertEquals(records.distinct.size, records.size)
        assert(records.forall(n => n >= 1 && n <= 11520))
        assert(
          ends.contains(StudioRef.Fixation(p17enc03, fixation6) -> ChainEnd.Record(record))
        )
        // FIXTURE.md: admitted trials; every contributing query's maps are among them.
        assert(maps.size <= 937, maps.size)
    }
  }

  test("used-by counts and pages agree, and match FIXTURE.md for enc_03 and ret_07") {
    val enc03 = StudioRef.TrialMap(run7, sigma2, p17enc03)
    val ret07 = StudioRef.TrialMap(run7, sigma2, p17ret07)
    withSession { s =>
      val n = s.navigator
      for
        encCounts <- n.usedByCounts(enc03).map(ok)
        retCounts <- n.usedByCounts(ret07).map(ok)
        pages     <- UsedByRole.values.toVector.traverse(r => n.usedBy(enc03, r, page).map(ok))
        asQuery   <- n.usedBy(ret07, UsedByRole.AsQuery, page).map(ok)
        fromQuery <- children(s, Place.At(query))
        small     <- n.usedBy(enc03, UsedByRole.AsControl, ok(PageRequest.of(10, 5))).map(ok)
      yield
        // "enc_03 is used by ret_07 as the matched reference and by the 18
        // other admitted P17 queries as a control."
        assertEquals(encCounts, UsedBy(0, 1, 18))
        assertEquals(pages.map(_.total), Vector(0, 1, 18))
        assertEquals(pages(1).entries, Vector(pair))
        // ret_07's own pairs: its matched reference and 19 controls, the same
        // pairs its contrast walks down to.
        assertEquals(retCounts, UsedBy(20, 0, 0))
        assertEquals(asQuery.entries.map(Place.At(_)), fromQuery)
        assertEquals(
          (small.offset, small.total, small.next, small.entries.size),
          (10, 18, Some(15), 5)
        )
    }
  }

  test("each contributing query's control pool is the size fixture.json records") {
    withSession { s =>
      val contributing = study.queries.filter(_.d.isDefined)
      contributing
        .traverse(q =>
          s.navigator
            .pairs(StudioRef.QueryContrast(run7, sigma2, q.key), PairDesign.Control, page)
        )
        .map { results =>
          contributing.zip(results).foreach { (q, r) =>
            assertEquals(r.map(_.total), Right(q.controls.getOrElse(-1)), q.key.label)
          }
        }
    }
  }

  test("steps refuse what they cannot walk, naming their operands") {
    val cell      = ReportRef.Cell(run7, reporting, sigma2, Some(Response("Unsure")))
    val stale     = ReportRef.Cell(StoryMoments.run5, reporting, sigma2, Some(remembered))
    val failed    = study.queries.find(_.status == "failed").get
    val failedRef = StudioRef.QueryContrast(run7, sigma2, failed.key)
    withSession { s =>
      val n = s.navigator
      for
        unknownGroup <- n.participants(cell, page)
        noResult     <- n.participants(stale, page)
        noContrast   <- n.pairs(failedRef, PairDesign.Matched, page)
        wrongLevel   <- n.maps(query)
        badScale     <- n.cells(run7, reporting, ok(ScaleIndex.of(9)), page)
        notAMap      <- n.usedByCounts(query)
        notInCell    <- n.queries(
          ReportRef
            .Participant(ReportRef.Cell(run7, reporting, sigma2, Some(remembered)), "P99"),
          page
        )
      yield
        assertEquals(
          unknownGroup,
          Left(NavigationError.UnknownGroup(cell, Vector(remembered, Response("Forgotten"))))
        )
        assertEquals(
          noResult,
          Left(
            NavigationError.Backend(BackendError.NoResult(StoryMoments.run5, RunState.Stale))
          )
        )
        noContrast match
          case Left(NavigationError.NoContrast(ref, QueryStatus.Failed(d))) =>
            assertEquals(ref, failedRef)
            assertEquals(d.code, "study-failure.off-window")
          case other => fail(s"expected NoContrast, got $other")
        assertEquals(wrongLevel, Left(NavigationError.WrongLevel(query, ChainLevel.Pair)))
        assertEquals(notAMap, Left(NavigationError.WrongLevel(query, ChainLevel.Map)))
        assertEquals(
          badScale,
          Left(NavigationError.Backend(BackendError.Unavailable(DiagnosticLocus.Scale(9))))
        )
        assert(notInCell.isLeft, notInCell)
        List(unknownGroup, noResult, wrongLevel, badScale, notInCell).foreach {
          case Left(e) => assert(e.message.nonEmpty)
          case _       => ()
        }
    }
  }
