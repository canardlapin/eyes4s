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

package eyes4s.studio.core.execution

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import eyes4s.codec.CanonicalDigest
import eyes4s.studio.core.artifacts.{NativeArtifactError, NativeBindingFacts}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.{FakeStudyBackend, StoryMoment, StoryMoments}
import fs2.Stream
import io.circe.syntax.*
import scala.concurrent.duration.*

/** Checked metadata fixtures isolate service ordering. Actual native payload
  * verification and persisted bytes are qualified by the native journeys.
  */
class ExecutionArtifactsSuite extends munit.CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def ok[E, A](value: IO[Either[E, A]]): IO[A]     = value.map(get)
  private val stamp                                        = ExecutionTrackerSuite.stamps(1)
  private val newer                                        = ExecutionTrackerSuite.stamps(2)
  private def facts(job: ExecutionJob): NativeBindingFacts =
    val recipe = get(StoryMoments.t2).draftRecipe.get
    get(
      NativeBindingFacts.of(
        job.run,
        job.stamp.copy(
          plan = CoreBinding.Bound(get(CanonicalDigest.parse[StudyPlanArtifact]("11" * 32))),
          input = CoreBinding.Bound(get(CanonicalDigest.parse[StudyInputArtifact]("22" * 32)))
        ),
        get(SemanticIdentity.of("0123456789abcdef")),
        get(CanonicalDigest.parse[ResultArchiveArtifact]("33" * 32)),
        recipe
      )
    )

  private def artifact(event: ExecutionEvent): Boolean = event match
    case _: ExecutionEvent.ArtifactsStored | _: ExecutionEvent.ArtifactsRefused => true
    case _                                                                      => false
  private def untilArtifact(events: Stream[IO, ExecutionEvent]): IO[Vector[ExecutionEvent]] =
    events.takeThrough(e => !artifact(e)).compile.toVector.timeout(5.seconds)

  test(
    "terminal Changed and Ready precede exactly one stored fact; blocked storage never holds tracker mutex"
  ) {
    for
      fake    <- FakeStudyBackend.create[IO](StoryMoment.T2)
      entered <- Deferred[IO, Unit]
      release <- Deferred[IO, Unit]
      calls   <- Ref.of[IO, Int](0)
      _       <- ExecutionService
        .resource[IO](
          fake,
          artifacts = Some(job =>
            calls.update(_ + 1) >> entered.complete(()) >> release.get.as(Right(facts(job)))
          )
        )
        .flatMap { service =>
          service.subscribe.map(service -> _)
        }
        .use { (service, events) =>
          for
            collecting <- untilArtifact(events).start
            job        <- ok(service.submit(stamp))
            _          <- ok(fake.complete(job.id))
            _          <- entered.get.timeout(2.seconds)
            state      <- ok(service.job(job.id))
            _          <- service.require(newer).timeout(2.seconds)
            cancelled  <- ok(service.cancel(job.id)).timeout(2.seconds)
            _          <- release.complete(())
            found      <- collecting.joinWithNever
            count      <- calls.get
            repeated   <- service.adopt(job.id, stamp)
          yield
            assert(state.phase.isInstanceOf[JobPhase.Succeeded])
            assert(cancelled.phase.isInstanceOf[JobPhase.Succeeded])
            assertEquals(count, 1)
            assert(repeated.isLeft)
            val tail = found.takeRight(3)
            assert(tail.head match
              case ExecutionEvent.Changed(j) =>
                j.id == job.id && j.phase.isInstanceOf[JobPhase.Succeeded]
              case _ => false)
            assertEquals(tail(1), ExecutionEvent.Ready(RunReady(job.id, job.run, stamp)))
            assertEquals(tail(2), ExecutionEvent.ArtifactsStored(job.id, facts(job)))
            tail.foreach(event => assertEquals(event.asJson.as[ExecutionEvent], Right(event)))
        }
        .guarantee(release.complete(()).void)
    yield ()
  }

  test(
    "typed storage refusal leaves successful computation and Ready intact and names its exact job/run"
  ) {
    for
      fake <- FakeStudyBackend.create[IO](StoryMoment.T2)
      _    <- ExecutionService
        .resource[IO](
          fake,
          artifacts = Some(job =>
            IO.pure(Left(NativeArtifactError.persistence(job.run, "store", "disk refused")))
          )
        )
        .flatMap { service =>
          service.subscribe.map(service -> _)
        }
        .use { (service, events) =>
          for
            collecting <- untilArtifact(events).start
            job        <- ok(service.submit(stamp))
            _          <- ok(fake.complete(job.id))
            found      <- collecting.joinWithNever
            state      <- ok(service.job(job.id))
          yield
            assert(state.phase.isInstanceOf[JobPhase.Succeeded])
            assert(found.exists(_.isInstanceOf[ExecutionEvent.Ready]))
            found.last match
              case ExecutionEvent.ArtifactsRefused(id, run, diagnostic) =>
                assertEquals((id, run), (job.id, job.run))
                assert(diagnostic.message.contains("disk refused"))
                assertEquals(found.last.asJson.as[ExecutionEvent], Right(found.last))
              case other => fail(s"wrong artifact refusal $other")
        }
    yield ()
  }

  test("adopting an already successfully completed status also delivers stored facts once") {
    for
      fake   <- FakeStudyBackend.create[IO](StoryMoment.T2)
      status <- ok(fake.submit(stamp.revision))
      _      <- ok(fake.complete(status.job))
      calls  <- Ref.of[IO, Int](0)
      _      <- ExecutionService
        .resource[IO](
          fake,
          artifacts = Some(job => calls.updateAndGet(_ + 1).as(Right(facts(job))))
        )
        .flatMap { service =>
          service.subscribe.map(service -> _)
        }
        .use { (service, events) =>
          for
            collecting <- untilArtifact(events).start
            job        <- ok(service.adopt(status.job, stamp))
            found      <- collecting.joinWithNever
            count      <- calls.get
          yield
            assert(job.phase.isInstanceOf[JobPhase.Succeeded])
            assertEquals(count, 1)
            assertEquals(found.last, ExecutionEvent.ArtifactsStored(job.id, facts(job)))
        }
    yield ()
  }

  test("cancelled, failed and superseded jobs never invoke artifact delivery") {
    Vector("cancelled", "failed", "superseded").traverse_ { mode =>
      for
        fake  <- FakeStudyBackend.create[IO](StoryMoment.T2)
        calls <- Ref.of[IO, Int](0)
        _     <- ExecutionService
          .resource[IO](
            fake,
            artifacts = Some(job => calls.update(_ + 1).as(Right(facts(job))))
          )
          .flatMap { service =>
            service.subscribe.map(service -> _)
          }
          .use { (service, events) =>
            for
              collecting <- events
                .takeThrough {
                  case ExecutionEvent.Changed(job) => !job.phase.isTerminal
                  case _                           => true
                }
                .compile
                .toVector
                .timeout(5.seconds)
                .start
              job <- ok(service.submit(stamp))
              _   <- mode match
                case "cancelled" => ok(service.cancel(job.id)).void
                case "failed"    => ok(fake.fail(job.id, Vector.empty)).void
                case _           => service.require(newer) >> ok(fake.complete(job.id)).void
              found <- collecting.joinWithNever
              count <- calls.get
            yield
              assertEquals(count, 0)
              assert(!found.exists(artifact))
          }
      yield ()
    }
  }
