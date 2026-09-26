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

package eyes4s.studio.core.fixture

import cats.effect.IO
import eyes4s.studio.core.backend.*

/** BackendConformanceSuite (fake): the contract on [[FakeStudyBackend]]. */
class FakeBackendConformanceSuite extends BackendConformanceSuite:

  def subject: IO[BackendConformanceSuite.Subject] =
    FakeStudyBackend.create[IO](StoryMoment.T2).map { fake =>
      def step(job: JobId, stage: JobStage, done: Long): IO[Unit] =
        fake
          .advanceTo(job, stage, done)
          .flatMap(e => IO.fromEither(e.left.map(err => new AssertionError(err.message))).void)
      BackendConformanceSuite.Subject(
        fake,
        DatasetRevision(3),
        RunId(7),
        AnalysisRevision(5),
        job =>
          step(job, JobStage.Estimating, 937) >>
            step(job, JobStage.Comparing, 100) >>
            step(job, JobStage.Comparing, 44845) >>
            step(job, JobStage.Contrasting, 457) >>
            fake
              .complete(job)
              .flatMap(e =>
                IO.fromEither(e.left.map(err => new AssertionError(err.message))).void
              )
      )
    }
