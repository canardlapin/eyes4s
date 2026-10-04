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

  test("a revision without declared time units is not admitted: Unavailable names it") {
    backend().flatMap(_.admission(StoryMoments.r2)).map { r =>
      assertEquals(r, Left(BackendError.Unavailable(DiagnosticLocus.Dataset(StoryMoments.r2))))
    }
  }

  test("bytes the revision did not record are refused, never admitted") {
    val tampered = new DatasetSources[IO]:
      def bytes(d: DatasetRevisionSpec, s: Source) =
        RealBackendConformanceSuite.golden
          .bytes(d, s)
          .map(_.map(b => b.updated(b.length - 2, '9'.toByte)))
      def assets(d: DatasetRevisionSpec) = RealBackendConformanceSuite.golden.assets(d)
    backend(tampered).flatMap(_.admission(StoryMoments.r3)).map { r =>
      assertEquals(r.left.map(_.code), Left("studio-backend.unavailable"))
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
      assertEquals((runs, jobs), (Vector.empty, Vector.empty))
      assertEquals(events.map(_.left.map(_.code)), Vector(Left("studio-backend.unavailable")))
  }
