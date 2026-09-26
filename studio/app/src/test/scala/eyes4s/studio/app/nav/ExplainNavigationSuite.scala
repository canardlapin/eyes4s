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

  test("every chain from the summary ends in a record or a MissingSource") {
    def walk(s: HeadlessSession, place: Place): Future[Vector[ChainEnd]] =
      place match
        case Place.At(ref @ StudioRef.Fixation(_, _)) =>
          Provenance.end(s.navigator, ref).map(e => Vector(ok(e)))
        case _ => children(s, place).flatMap(_.flatTraverse(walk(s, _)))
    withSession { s =>
      walk(s, summary).map { ends =>
        val records = ends.collect { case ChainEnd.Record(r) => r }
        assert(ends.nonEmpty)
        assertEquals(records.size + ends.count(_.isInstanceOf[ChainEnd.Missing]), ends.size)
        assert(records.contains(record))
        // Every record is a fixations.csv data record of the golden file.
        records.foreach {
          case StudioRef.SourceRecord(_, Some(_), _, n) =>
            assert(n.value >= 1 && n.value <= 11520)
          case other => fail(s"not a fixation record: $other")
        }
      }
    }
  }

  test("steps refuse what they cannot walk, naming their operands") {
    val cell      = ReportRef.Cell(run7, reporting, sigma2, Some(Response("Unsure")))
    val stale     = ReportRef.Cell(StoryMoments.run5, reporting, sigma2, Some(remembered))
    val failed    = study.queries.find(_.status == "failed").get
    val failedRef = StudioRef.QueryContrast(run7, sigma2, failed.key)
    val other     = StudioRef.QueryContrast(run7, sigma2, MockStudy.key("P03", "ret_01"))
    withSession { s =>
      val n = s.navigator
      for
        unknownGroup <- n.participants(cell)
        noResult     <- n.participants(stale)
        noContrast   <- n.pairs(failedRef, PairDesign.Matched, page)
        wrongLevel   <- n.maps(query)
        noControls   <- n.pairs(other, PairDesign.Control, page)
        badScale     <- n.cells(run7, reporting, ok(ScaleIndex.of(9)))
        notInCell    <- n.queries(
          ReportRef
            .Participant(ReportRef.Cell(run7, reporting, sigma2, Some(remembered)), "P99")
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
        assertEquals(noControls, Left(NavigationError.NoPairs(other, PairDesign.Control)))
        assertEquals(
          badScale,
          Left(NavigationError.Backend(BackendError.Unavailable(DiagnosticLocus.Scale(9))))
        )
        assert(notInCell.isLeft, notInCell)
        List(unknownGroup, noResult, wrongLevel, noControls, badScale, notInCell).foreach {
          case Left(e) => assert(e.message.nonEmpty)
          case _       => ()
        }
    }
  }
