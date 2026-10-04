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
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{DatasetRevisionSpec, Source, SourceRole}
import eyes4s.studio.core.fixture.{GoldenAssets, GoldenCsv, StoryMoments}
import java.nio.charset.StandardCharsets

/** BackendConformanceSuite (real): the contract on [[RealStudyBackend]] over
  * fixtures/studio-golden at story moment t2.
  *
  * The real backend serves the operations of the S3.7 slices that have
  * landed (docs/studio/plan/S3.7-slices.md). The tests run here are exactly
  * [[RealBackendConformanceSuite.Served]], named so that no test is skipped
  * silently; each slice adds the tests its operations make pass. The fake
  * runs every test (FakeBackendConformanceSuite).
  */
class RealBackendConformanceSuite extends BackendConformanceSuite:
  def subject: IO[BackendConformanceSuite.Subject] =
    RealBackendConformanceSuite.subject

  override def munitTests(): Seq[Test] =
    val all     = super.munitTests()
    val missing = RealBackendConformanceSuite.Served -- all.map(_.name)
    assert(missing.isEmpty, s"Served names no conformance test: $missing")
    all.filter(t => RealBackendConformanceSuite.Served(t.name))

object RealBackendConformanceSuite:

  /** The conformance tests the landed slices serve (slice 1: admission, the
    * ledger and refusals).
    */
  val Served: Set[String] = Set(
    "admission reports the FIXTURE.md inventory and window totals",
    "the ledger pages through every inventory trial once and agrees with admission",
    "refusals are values with stable codes and typed subjects"
  )

  /** fixtures/studio-golden as the host would hand it over. */
  val golden: DatasetSources[IO] = new DatasetSources[IO]:
    def bytes(dataset: DatasetRevisionSpec, source: Source): IO[Option[IArray[Byte]]] =
      IO.pure(source.role match
        case SourceRole.Fixations =>
          Some(IArray.from(GoldenCsv.fixations.getBytes(StandardCharsets.UTF_8)))
        case SourceRole.Trials =>
          Some(IArray.from(GoldenCsv.trials.getBytes(StandardCharsets.UTF_8))))
    def assets(dataset: DatasetRevisionSpec): IO[Option[AssetRegistry]] =
      IO.pure(GoldenAssets.registry(dataset).toOption)

  def subject: IO[BackendConformanceSuite.Subject] =
    IO.fromEither(StoryMoments.t2.left.map(new AssertionError(_))).flatMap { document =>
      RealStudyBackend.create[IO](document, golden).map { backend =>
        BackendConformanceSuite.Subject(
          backend,
          StoryMoments.r3,
          StoryMoments.run7,
          StoryMoments.rev5,
          // No served test submits a job yet (execution is slice 4).
          job => IO.raiseError(new AssertionError(s"slice 1 serves no job ${job.number}"))
        )
      }
    }
