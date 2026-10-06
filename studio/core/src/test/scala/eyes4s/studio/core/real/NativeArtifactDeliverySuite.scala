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
import eyes4s.codec.{ByteDigest, CanonicalDigest}
import eyes4s.studio.core.artifacts.*
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.execution.*
import eyes4s.studio.core.fixture.{FakeStudyBackend, StoryMoment, StoryMoments}
import java.nio.charset.StandardCharsets.UTF_8
import scala.concurrent.duration.*

/** Actual CSV execution produces the verified package. A controllable event
  * source then isolates the host acknowledgement's terminal ordering.
  */
class NativeArtifactDeliverySuite extends munit.CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def ok[E, A](value: IO[Either[E, A]]): IO[A] = value.map(get)
  private val run                                      = RunId(8)
  private val fixes                                    =
    "participant,phase,trial,occurrence,ordinal,onset_ms,duration_ms,x,y,sample_count\n" +
      "P01,Encoding,enc_01,1,1,0,100,600,300,1\n" +
      "P01,Encoding,enc_02,1,1,0,100,620,320,1\n" +
      "P01,Retrieval,ret_01,1,1,0,100,604,305,1\n"
  private val trials =
    "participant,phase,trial,occurrence,item,response,display_kind,image_file\n" +
      "P01,Encoding,enc_01,1,item-a,,image,item-a.png\n" +
      "P01,Encoding,enc_02,1,item-b,,image,item-b.png\n" +
      "P01,Retrieval,ret_01,1,item-a,Remembered,image,item-a.png\n"
  private lazy val verified =
    val sample   = get(StoryMoments.t2)
    val original = sample.datasets.last
    val byRole   = Map(SourceRole.Fixations -> fixes, SourceRole.Trials -> trials)
    val sources  = get(
      Sources.of(
        original.sources.entries.map(source =>
          source.copy(bytes =
            ByteDigest.sha256(IArray.from(byRole(source.role).getBytes(UTF_8)))
          )
        )
      )
    )
    val spec   = original.copy(sources = sources)
    val assets = get(
      AssetRegistry.of(spec.id, sources.trials.get.bytes, spec.geometry.screen, Vector.empty)
    )
    val admitted = get(RealAdmission.admit(spec, fixes, trials, assets))
    val analysis = sample.analyses.last
    val recipe   = analysis.recipe.copy(
      grid = get(GridSize.of(8, 8)),
      scales = get(ScaleSet.of(Vector(get(Sigma.of(1)))))
    )
    val prepared = get(RealPrepared.of(analysis.id, spec.id, recipe, admitted))
    get(
      NativeArtifacts.build(
        run,
        RealStudyBackend
          .RealRun(prepared, get(prepared.work.run), RealStudyBackend.RunOrigin.Computed),
        NativeArtifactBudget.Default
      )
    )

  private def deliver(returned: NativeBindingFacts): IO[Vector[ExecutionEvent]] =
    val provider = new NativeArtifactProvider[IO]:
      def nativeArtifacts(requested: RunId, budget: NativeArtifactBudget) =
        IO(assertEquals(budget, verified.budget)).as(
          Either.cond(
            requested == run,
            verified,
            NativeArtifactError.NotRetained(requested, Vector(run))
          )
        )
    val sink = new NativeArtifactSink[IO]:
      def store(value: NativeArtifactPackage) =
        IO(assertEquals(value.facts, verified.facts)).as(Right(returned))
    FakeStudyBackend.create[IO](StoryMoment.T2).flatMap { fake =>
      ExecutionService
        .resource[IO](
          fake,
          artifacts = Some(job => NativeArtifactDelivery.store(provider, sink, job.run))
        )
        .flatMap { service =>
          service.subscribe.map(service -> _)
        }
        .use { (service, events) =>
          for
            collected <- events
              .takeThrough {
                case _: ExecutionEvent.ArtifactsStored | _: ExecutionEvent.ArtifactsRefused =>
                  false
                case _ => true
              }
              .compile
              .toVector
              .timeout(5.seconds)
              .start
            job   <- ok(service.submit(verified.facts.stamp))
            _     <- ok(fake.complete(job.id))
            found <- collected.joinWithNever
            state <- ok(service.job(job.id))
          yield
            assertEquals(job.run, run)
            assert(state.phase.isInstanceOf[JobPhase.Succeeded])
            assert(found.exists(_.isInstanceOf[ExecutionEvent.Ready]))
            found
        }
    }

  test("a sink cannot substitute a different result behind the verified run and stamp") {
    val expected    = verified.facts
    val substituted = get(
      NativeBindingFacts.of(
        expected.run,
        expected.stamp,
        expected.source,
        get(CanonicalDigest.parse[ResultArchiveArtifact]("44" * 32)),
        expected.recipeSnapshot
      )
    )
    deliver(substituted).map { found =>
      assert(!found.exists(_.isInstanceOf[ExecutionEvent.ArtifactsStored]))
      found.last match
        case ExecutionEvent.ArtifactsRefused(job, refusedRun, diagnostic) =>
          assertEquals(refusedRun, run)
          assert(found.exists {
            case ExecutionEvent.Changed(value) =>
              value.id == job && value.phase.isInstanceOf[JobPhase.Succeeded]
            case _ => false
          })
          assert(diagnostic.message.contains(expected.result.toString))
          assert(diagnostic.message.contains(substituted.result.toString))
        case other => fail(s"wrong delivery outcome $other")
    }
  }

  test("an exact package acknowledgement publishes the verified facts after Ready") {
    deliver(verified.facts).map { found =>
      assert(found.takeRight(2).head.isInstanceOf[ExecutionEvent.Ready])
      found.last match
        case ExecutionEvent.ArtifactsStored(_, facts) => assertEquals(facts, verified.facts)
        case other                                    => fail(s"wrong delivery outcome $other")
    }
  }
