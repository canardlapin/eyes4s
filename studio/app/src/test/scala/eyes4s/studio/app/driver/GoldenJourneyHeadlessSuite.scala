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

package eyes4s.studio.app.driver

import eyes4s.studio.app.StoryModels
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.headless.HeadlessSession

import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext

/** E2E-01 headless (ticket S3.6): the golden journey ([[GoldenJourney]])
  * through the driver on the fake backend, asserting FIXTURE.md counts and
  * fixture.json scores on the way, in under 60 s.
  */
class GoldenJourneyHeadlessSuite extends munit.FunSuite:

  override val munitTimeout: Duration = 120.seconds

  private given ExecutionContext = ExecutionContext.global

  /** S3.6 acceptance: E2E-01 headless in under 60 s on the fake. */
  private val Budget: FiniteDuration = 60.seconds

  test("E2E-01 headless: the golden journey on the fake backend, in under 60 s") {
    val started = System.nanoTime()
    HeadlessSession.open(StoryMoment.T1).flatMap { session =>
      val driver = StudioDriver.open(StoryModels.firstRun)
      GoldenJourney
        .stages(session)
        .all
        .run(driver)
        .transformWith(result => session.close.transform(_ => result))
        .map { result =>
          val elapsed = (System.nanoTime() - started).nanos
          result match
            case Left(failure) => fail(failure.message)
            case Right(end)    =>
              assert(elapsed < Budget, s"E2E-01 took ${elapsed.toMillis} ms")
              assertEquals(
                end.records.collect { case DriverRecord.Stubbed(step, _) => step },
                Vector("import dialog", "compose figure", "export")
              )
              assertEquals(
                end.records.collect { case r: DriverRecord.Refused => r },
                Vector.empty
              )
        }
    }
  }
