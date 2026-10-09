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

package probe

import cats.effect.{IO, IOApp}
import eyes4s.compare.ComparisonBudget
import eyes4s.design.WorkQuanta
import eyes4s.fs2.StudyExecution

object Fs2Probe extends IOApp.Simple:
  def run: IO[Unit] =
    val work   = StudyFixture.checked(StudyFixture.plan.prepare(StudyFixture.input))
    val budget = StudyFixture.checked(ComparisonBudget.of(4))
    StudyExecution[IO].start(work, budget, WorkQuanta.default).use { run =>
      run.outcome.flatMap(outcome => IO.println(outcome.toString))
    }
