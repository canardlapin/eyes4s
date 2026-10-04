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

package eyes4s.studio.desktop.journey

import cats.instances.future.*
import cats.syntax.all.*
import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.driver.{DriverRecord, StudioDriver}
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.headless.HeadlessSession

import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext

/** E2E-01 (ticket S10.1): the golden journey through the headless driver, its
  * numbers held to FIXTURE.md and, where the library can say, to eyes4s run
  * directly on fixtures/studio-golden ([[GoldenLibrary]]):
  *
  * new project → import the fixture → map columns → admit r3 → explore P17
  * enc_03 → Analysis rev 4 → run → Compare P17 ret_07 → summary → figure →
  * export → close → reopen → identical.
  *
  * The figure is composed and its bundle exported through the composer and
  * the desktop's bundle assembly; the project is saved to a real project
  * folder, reopened, and saved again, and the two folders are byte-identical.
  *
  * Pending, each a named stub in the driver's record:
  *  - [[Pending.Repair]]: S5.7's Repair of the two missing images;
  *  - [[Pending.LinkedSelection]]: S6.6's linked selection in Explore;
  *  - [[Pending.LibraryScores]]: scores from eyes4s itself. The fake serves
  *    fixture.json's scores, not eyes4s's on these files (fixtures/
  *    studio-golden/README.md); S0.7b freezes eyes4s's as SCORES.json and S3.7
  *    runs the real backend. Until then the scores are held to FIXTURE.md.
  */
class GoldenJourneySuite extends munit.FunSuite:

  override val munitTimeout: Duration = 300.seconds

  private given ExecutionContext = ExecutionContext.global

  test(
    "E2E-01: the golden journey, every FIXTURE.md number, eyes4s's counts, reopened identical"
  ) {
    (HeadlessSession.open(StoryMoment.T1), HeadlessSession.open(StoryMoment.T2)).tupled
      .flatMap { (session, views) =>
        val journey = GoldenRoute.Route(session, views)
        journey.scenario
          .run(StudioDriver.open(StoryModels.firstRun))
          .transformWith(result => (session.close, views.close).tupled.transform(_ => result))
          .map {
            case Left(failure) => fail(failure.message)
            case Right(end)    =>
              assertEquals(
                end.records.collect { case DriverRecord.Stubbed(step, _) => step },
                Vector(
                  "import dialog",
                  GoldenRoute.Pending.Repair._1,
                  GoldenRoute.Pending.LinkedSelection._1,
                  GoldenRoute.Pending.LibraryScores._1
                )
              )
              assertEquals(
                end.records.collect { case r: DriverRecord.Refused => r },
                Vector.empty
              )
              assert(journey.exported.contains("methods.md"), journey.exported.keySet)
          }
      }
  }
