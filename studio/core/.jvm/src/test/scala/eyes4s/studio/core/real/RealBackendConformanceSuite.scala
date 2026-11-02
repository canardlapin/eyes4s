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

import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{
  DatasetRevisionSpec,
  DefinitionRef,
  Source,
  SourceRole,
  StudioDocument
}
import eyes4s.studio.core.fixture.{GoldenAssets, GoldenCsv, StoryMoments}
import java.nio.charset.StandardCharsets

/** BackendConformanceSuite (real): the contract on [[RealStudyBackend]] over
  * fixtures/studio-golden at story moment t2.
  *
  * Every inherited contract test runs on both this native backend and the
  * fake (FakeBackendConformanceSuite). [[RealBackendConformanceSuite.Served]]
  * names the qualified contract explicitly; adding an inherited test without
  * qualifying it here fails rather than silently filtering it away.
  */
class RealBackendConformanceSuite extends BackendConformanceSuite:
  override def munitIOTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(5, "min")

  private val cleanup = ResourceTestLocalFixture(
    "real backend resources",
    Resource
      .make(Ref.of[IO, Vector[IO[Unit]]](Vector.empty))(_.get.flatMap(_.reverse.sequence_))
  )
  override def munitFixtures = List(cleanup)

  def subject: IO[BackendConformanceSuite.Subject] =
    RealBackendConformanceSuite.subjectResource.allocated.flatMap { (subject, release) =>
      cleanup().update(_ :+ release).as(subject)
    }

  override def munitTests(): Seq[Test] =
    val all     = super.munitTests()
    val missing = RealBackendConformanceSuite.Served -- all.map(_.name)
    assert(missing.isEmpty, s"Served names no conformance test: $missing")
    val unserved = all.map(_.name).toSet -- RealBackendConformanceSuite.Served
    assert(
      unserved.isEmpty,
      s"Inherited conformance tests need native qualification: $unserved"
    )
    all

object RealBackendConformanceSuite:

  /** Every current inherited backend-contract test, qualified on native
    * golden inputs and genuine runner jobs.
    */
  val Served: Set[String] = Set(
    "admission reports the FIXTURE.md inventory and window totals",
    "the ledger pages through every inventory trial once and agrees with admission",
    "refusals are values with stable codes and typed subjects",
    "a subscription reports run-monotone progress and ends with exactly one Finished",
    "cancelling a job settles it Cancelled, once, and the current run stays current",
    "the preview's counts agree with its rows",
    "the current run's result agrees with its query rows",
    "inspecting a contrast row returns the row's scores; unscored queries say why",
    "provenance runs from the run to the pair's two trials",
    "a run's pair rows page through every pair at a scale; an unknown scale is refused",
    "the enveloped JSON transport answers exactly as the backend does in process",
    "a subscription over the transport streams event frames under its request id"
  )

  /** t2 explicitly pinned to the trial-inventory layout used by its admitted
    * TrialKeys; current story presets already declare this native layout.
    */
  val trialLayout: Either[String, StudioDocument] =
    val layout = DefinitionRef.fromCore(eyes4s.plan.TrialKeyDefinitions.trialLayout)
    StoryMoments.t2.flatMap(t2 =>
      StudioDocument
        .of(
          t2.timedDatasets,
          t2.timedAnalyses.map(a => a.copy(recipe = a.recipe.copy(layout = layout))),
          t2.draft,
          t2.runs,
          t2.reporting,
          t2.figures,
          t2.presentation,
          t2.jobs
        )
        .left
        .map(_.toString)
    )

  /** fixtures/studio-golden as the host would hand it over. */
  val golden: DatasetSources[IO] = new DatasetSources[IO]:
    def bytes(dataset: DatasetRevisionSpec, source: Source): IO[Option[IArray[Byte]]] =
      IO.pure(source.role match
        case SourceRole.Fixations =>
          Some(IArray.from(GoldenCsv.fixations.getBytes(StandardCharsets.UTF_8)))
        case SourceRole.Trials =>
          Some(IArray.from(GoldenCsv.trials.getBytes(StandardCharsets.UTF_8)))
        case SourceRole.LocationTrials | SourceRole.Locations | SourceRole.LocationPairs |
            SourceRole.PhaseLedger | SourceRole.LocationLedger =>
          None)
    def assets(dataset: DatasetRevisionSpec): IO[Option[AssetRegistry]] =
      IO.pure(GoldenAssets.registry(dataset).toOption)

  /** Construct the scientific fixture through an explicit Run submission. */
  def beforeRun7(document: StudioDocument): StudioDocument =
    StudioDocument
      .of(
        document.timedDatasets,
        document.timedAnalyses,
        document.draft,
        document.runs.filterNot(_.id == StoryMoments.run7),
        document.reporting,
        document.figures.filterNot(_.run == StoryMoments.run7),
        eyes4s.studio.core.document.PresentationState.default,
        Vector.empty
      )
      .fold(e => throw AssertionError(e.toString), identity)

  def computeCurrent(backend: RealStudyBackend[IO]): IO[Unit] =
    for
      submitted <- backend
        .submit(StoryMoments.rev4)
        .flatMap(value => IO.fromEither(value.leftMap(e => new AssertionError(e.message))))
      _ <- IO.raiseUnless(submitted.run == StoryMoments.run7)(
        new AssertionError(s"Expected run7, got ${submitted.run}")
      )
      events <- backend
        .subscribe(submitted.job)
        .flatMap(value => IO.fromEither(value.leftMap(e => new AssertionError(e.message))))
      _      <- events.compile.drain
      result <- backend.result(StoryMoments.run7)
      _      <- IO.fromEither(result.leftMap(e => new AssertionError(e.message))).void
    yield ()

  def computedResource(document: StudioDocument): Resource[IO, RealStudyBackend[IO]] =
    RealStudyBackend.resource[IO](beforeRun7(document), golden).evalTap(computeCurrent)

  def subjectResource: Resource[IO, BackendConformanceSuite.Subject] =
    Resource.eval(IO.fromEither(trialLayout.left.map(new AssertionError(_)))).flatMap {
      document =>
        computedResource(document)
          .evalTap { backend =>
            backend
              .synchronize(document)
              .flatMap(value =>
                IO.fromEither(value.leftMap(e => new AssertionError(e.message)))
              )
          }
          .map { backend =>
            BackendConformanceSuite.Subject(
              backend,
              StoryMoments.r3,
              StoryMoments.run7,
              StoryMoments.rev5,
              job =>
                backend.subscribe(job).flatMap {
                  case Right(events) => events.compile.drain
                  case Left(error)   => IO.raiseError(new AssertionError(error.message))
                }
            )
          }
    }
