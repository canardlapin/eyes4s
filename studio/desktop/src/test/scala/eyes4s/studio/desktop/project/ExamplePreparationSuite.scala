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

package eyes4s.studio.desktop.project

import cats.effect.IO
import eyes4s.studio.app.AppModel
import eyes4s.codec.CanonicalDigest
import eyes4s.studio.core.document.{CoreBinding, ResultArchiveArtifact, StudioDocument}
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.bundle.{
  InMemoryProjectStore,
  LockOwner,
  ProjectBundle,
  SharingOptions
}
import eyes4s.studio.desktop.platform.BundledExample
import eyes4s.studio.core.session.ProjectSession
import munit.CatsEffectSuite

class ExamplePreparationSuite extends CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  test("a refused example preparation releases the copied project's writer lock") {
    for
      store   <- InMemoryProjectStore.create[IO]
      session <- ProjectSession
        .create[IO](
          store,
          get(LockOwner.of("example preparation")),
          get(AppModel.newProject).document,
          SharingOptions.complete,
          Vector.empty
        )
        .map(get)
      result <- ExamplePreparation.prepare(session)
      lock   <- store.acquire(get(LockOwner.of("after refused preparation"))).map(get)
      _      <- store.release(lock).map(get)
    yield assert(result.isLeft, result)
  }

  test("a bound run without stored native files is refused and releases its writer") {
    val original = get(StoryMoments.t2)
    val digest   = get(CanonicalDigest.parse[ResultArchiveArtifact]("ab" * 32))
    val document = get(
      StudioDocument.of(
        original.datasets,
        original.analyses,
        original.draft,
        original.runs.map(r =>
          if r.id == StoryMoments.run7 then r.copy(archive = CoreBinding.Bound(digest)) else r
        ),
        original.reporting,
        original.figures,
        original.presentation,
        original.jobs,
        original.analysisFamilies
      )
    )
    for
      source         <- BundledExample.source.map(get)
      originalBundle <- ProjectBundle.open(source).map(get)
      store          <- InMemoryProjectStore.create[IO]
      session        <- ProjectSession
        .create[IO](
          store,
          get(LockOwner.of("bound example")),
          document,
          SharingOptions.complete,
          originalBundle.manifest.inputs
        )
        .map(get)
      result <- ExamplePreparation.prepare(session)
      lock   <- store.acquire(get(LockOwner.of("after missing archive"))).map(get)
      _      <- store.release(lock).map(get)
    yield
      assert(result.isLeft, result)
      assert(result.swap.toOption.exists(_.contains("no stored result archive")), result)
  }
