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
    FakeBackendConformanceSuite.subject(IO.pure)

object FakeBackendConformanceSuite:

  private def orFail[E, A](result: IO[Either[E, A]]): IO[Unit] =
    result.flatMap(e => IO.fromEither(e.left.map(err => new AssertionError(err.toString))).void)

  /** A fresh fake at story moment t2, reached through `reach` (the fake
    * itself, or a client of it over a transport); `finish` steps the fake.
    */
  def subject(
      reach: FakeStudyBackend[IO] => IO[StudyBackend[IO]]
  ): IO[BackendConformanceSuite.Subject] =
    FakeStudyBackend.create[IO](StoryMoment.T2).flatMap { fake =>
      reach(fake).map { backend =>
        BackendConformanceSuite.Subject(
          backend,
          DatasetRevision(3),
          RunId(7),
          AnalysisRevision(5),
          job =>
            orFail(fake.advanceTo(job, Segment.Estimating(0), 937)) >>
              orFail(fake.advanceToPairs(job, 100)) >>
              orFail(fake.advanceToPairs(job, StoryMoment.RunningPairs)) >>
              orFail(fake.advanceTo(job, Segment.Contrasting(4), 457)) >>
              orFail(fake.complete(job))
        )
      }
    }
