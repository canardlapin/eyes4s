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

package eyes4s.studio.core.headless

import cats.effect.{IO, Ref}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.fixture.{FakeStudyBackend, StoryMoment, StoryMoments}
import munit.CatsEffectSuite

class SynchronizedBackendSuite extends CatsEffectSuite:
  test("synchronization refusals are values and do not prevent job observation/cancellation") {
    val refused = BackendError.RegistryRefused(
      DiagnosticLocus.Revision(StoryMoments.rev4),
      "Saved recipe changed."
    )
    for
      underlying <- FakeStudyBackend.create[IO](StoryMoment.T2)
      calls      <- Ref.of[IO, Int](0)
      proxy = new SynchronizedBackend[IO](
        underlying,
        () => calls.updateAndGet(_ + 1).map(_ => Left(refused))
      )
      admission <- proxy.admission(StoryMoments.r3)
      submit    <- proxy.submit(StoryMoments.rev4)
      _         <- proxy.jobs
      _         <- proxy.runs
      observed  <- calls.get
    yield
      assertEquals(admission, Left(refused))
      assertEquals(submit, Left(refused))
      assertEquals(observed, 2, "job/run observation must not wait for synchronization")
  }
