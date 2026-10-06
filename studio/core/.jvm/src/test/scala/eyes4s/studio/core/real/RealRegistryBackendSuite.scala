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
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.{Command, Reducer}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.preview.*
import munit.CatsEffectSuite

class RealRegistryBackendSuite extends CatsEffectSuite:
  override def munitIOTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(5, "min")
  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private val original                     = ok(RealBackendConformanceSuite.trialLayout)

  private def edited: StudioDocument =
    val base  = original.analyses.last
    val draft = ok(
      Draft.between(StoryMoments.rev5, base, base.recipe.copy(grid = ok(GridSize.of(32, 24))))
    )
    ok(
      StudioDocument.of(
        original.datasets,
        original.analyses,
        Some(draft),
        original.runs,
        original.reporting,
        original.figures,
        original.presentation,
        original.jobs
      )
    )

  private def ready(backend: RealStudyBackend[IO]): IO[PreviewReady] =
    val budget = ok(PreviewBudget.of(PreviewBudget.MaximumPages))
    def finish(
        events: Vector[Either[BackendError, PreviewEvent]],
        id: Option[PreviewId]
    ): IO[PreviewReady] =
      events.collectFirst { case Left(error) => error } match
        case Some(error) => IO.raiseError(new AssertionError(error.message))
        case None        =>
          events.collectFirst { case Right(PreviewEvent.Ready(value)) => value } match
            case Some(value) => IO.pure(value)
            case None        =>
              val next = id
                .orElse(events.collectFirst { case Right(PreviewEvent.Initial(id, _, _)) =>
                  id
                })
                .getOrElse(fail("native preview did not identify its cursor"))
              backend
                .continuePreview(next, budget)
                .compile
                .toVector
                .flatMap(finish(_, Some(next)))
    backend.previewCounting(StoryMoments.rev5, budget).compile.toVector.flatMap(finish(_, None))

  test("live SaveAndRun and nonsequential reservations keep the exact requested run id") {
    val saved   = ok(Reducer.step(original, Command.SaveAndRun(None)))._1
    val distant = ok(
      StudioDocument.of(
        saved.datasets,
        saved.analyses,
        saved.draft,
        saved.runs.init :+ saved.runs.last.copy(id = RunId(99)),
        saved.reporting,
        saved.figures,
        saved.presentation,
        saved.jobs
      )
    )
    Vector(saved, distant).traverse_ { next =>
      val requested = next.runs.last.id
      RealStudyBackend.resource[IO](original, RealBackendConformanceSuite.golden).use {
        backend =>
          for
            before <- backend.preview(StoryMoments.rev5).map(ok)
            synced <- backend.synchronize(next).map(ok)
            job    <- backend.submit(StoryMoments.rev5).map(ok)
            _      <- backend.cancel(job.job).map(ok)
            runs   <- backend.runs
          yield
            assertEquals(before.revision, StoryMoments.rev5)
            assertEquals(synced, ())
            assertEquals(job.run, requested)
            assertEquals(job.revision, StoryMoments.rev5)
            assertEquals(runs.count(_.run == requested), 1)
      }
    }
  }

  test("same-id draft edits reject the old ready receipt and retain its immutable cursor") {
    RealStudyBackend.resource[IO](original, RealBackendConformanceSuite.golden).use { backend =>
      for
        before   <- ready(backend)
        _        <- backend.synchronize(edited).map(ok)
        rows     <- backend.previewRows(StoryMoments.rev5, ok(PageRequest.of(0, 3)))
        refused  <- backend.submitPreview(before)
        retained <- backend.continuePreview(before.id, ok(PreviewBudget.of(1))).compile.toVector
        afterContinue <- backend.previewRows(StoryMoments.rev5, ok(PageRequest.of(0, 3)))
        jobs          <- backend.jobs
      yield
        assert(afterContinue.isLeft, "an old cursor must not repopulate current draft rows")
        assert(rows.isLeft)
        assertEquals(refused.left.map(_.code), Left("studio-backend.stale-preview"))
        assert(retained.contains(Right(PreviewEvent.Ready(before))))
        assertEquals(jobs, Vector.empty)
    }
  }

  test("ambiguous newly requested runs are refused before a native job starts") {
    val first = RunRef(
      RunId(8),
      StoryMoments.rev4,
      StoryMoments.r3,
      RunLifecycle.Running,
      CoreBinding.unbound
    )
    val ambiguous = ok(
      StudioDocument.of(
        original.datasets,
        original.analyses,
        original.draft,
        original.runs ++ Vector(first, first.copy(id = RunId(9))),
        original.reporting,
        original.figures,
        original.presentation,
        original.jobs
      )
    )
    RealStudyBackend.resource[IO](original, RealBackendConformanceSuite.golden).use { backend =>
      for
        _       <- backend.synchronize(ambiguous).map(ok)
        refused <- backend.submit(StoryMoments.rev4)
        jobs    <- backend.jobs
      yield
        assertEquals(refused.left.map(_.code), Left("studio-backend.registry-refused"))
        assert(refused.left.toOption.get.message.contains("run 8"))
        assert(refused.left.toOption.get.message.contains("run 9"))
        assertEquals(jobs, Vector.empty)
    }
  }

  test(
    "changed saved definitions and a noncanonical binding cannot publish into the registry"
  ) {
    val fake = ok(eyes4s.codec.CanonicalDigest.parse[StudyPlanArtifact]("ab" * 32))
    val bad  = ok(
      StudioDocument.of(
        original.datasets,
        original.analyses.map(a =>
          if a.id == StoryMoments.rev4 then a.copy(plan = CoreBinding.Bound(fake)) else a
        ),
        original.draft,
        original.runs,
        original.reporting,
        original.figures,
        original.presentation,
        original.jobs
      )
    )
    RealStudyBackend.resource[IO](original, RealBackendConformanceSuite.golden).use { backend =>
      for
        refused         <- backend.synchronize(bad)
        snapshot        <- backend.registry.get
        originalPreview <- backend.preview(StoryMoments.rev4)
      yield
        assertEquals(refused.left.map(_.code), Left("studio-backend.registry-refused"))
        assertEquals(snapshot.document, original)
        assert(originalPreview.isRight)
    }
  }

  test("a held result keeps its prepared recipe when the live same-id draft changes") {
    RealStudyBackend.resource[IO](original, RealBackendConformanceSuite.golden).use { backend =>
      for
        status   <- backend.submit(StoryMoments.rev5).map(ok)
        events   <- backend.subscribe(status.job).map(ok)
        _        <- events.compile.drain
        before   <- backend.held(status.run).map(ok)
        _        <- backend.synchronize(edited).map(ok)
        after    <- backend.held(status.run).map(ok)
        current  <- backend.preview(StoryMoments.rev5).map(ok)
        snapshot <- backend.registry.get
      yield
        assert(before eq after)
        assertEquals(after.prepared.recipe.grid, original.analyses.last.recipe.grid)
        assertEquals(current.revision, StoryMoments.rev5)
        assertEquals(snapshot.document, edited)
    }
  }
