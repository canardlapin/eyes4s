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

/** E2E-01 (ticket S10.1): the golden journey through the headless driver
  * ([[GoldenRoute]]):
  *
  * new project → import the fixture → map columns → admit r3 → explore P17
  * enc_03 → Analysis rev 4 → run → Compare P17 ret_07 → summary → figure →
  * export → close → reopen → identical.
  *
  * Every number is held to FIXTURE.md (or the parity checklist). These are
  * also held to eyes4s run directly on fixtures/studio-golden
  * ([[GoldenLibrary]], no Studio code but the r3 column mapping): the
  * admission (960/937/17 by cause/6, 11,520 records, 259 items, images found
  * 257 and the two missing), the window tallies (543 in 409 trials, none off
  * screen), the focus fixation and the enc_03/ret_07 window shares, P05's
  * three all-outside trials; the design by eyes4s's pairing (480 requested, 14
  * not admitted, 9 without a match, 457 eligible, 219,486 candidates, 8,969
  * pairs per scale with 19 or 18 controls, so 35,876 and rev 5's 44,845); and
  * the methods text's 11,311 admitted records and duration share. The scores
  * and everything only the run decides are pending (below).
  *
  * The figure is composed and its bundle assembled through the composer and
  * the desktop's bundle assembly; the project is saved to a real project
  * folder, reopened from it, and saved again with the inputs the folder
  * holds, and the two folders are byte-identical; the driver reopens the
  * document read from the folder.
  *
  * The two missing images are repaired in Data › Sources (S5.7), and the
  * trail follows the selected fixation and Next in Explore (S6.6).
  *
  * Pending, a named stub in the driver's record:
  *  - [[Pending.LibraryScores]]: what only the run decides, from eyes4s
  *    itself (M, B and D by query and scale, contributing 454 / failed 3, the
  *    group n range, every participant and group mean). The fake serves
  *    fixture.json's scores, not eyes4s's on these files (fixtures/
  *    studio-golden/README.md); S0.7b freezes eyes4s's as SCORES.json and S3.7
  *    runs the real backend. Until then these are held to FIXTURE.md.
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
                  GoldenRoute.Pending.LibraryScores._1
                )
              )
              assertEquals(
                end.records.collect { case r: DriverRecord.Refused => r },
                Vector.empty
              )
              // The bundle's README names what it holds and what it is bound to.
              val readme = journey.exported.getOrElse("README.txt", "")
              assert(readme.contains("run 6 · analysis rev 4 · data r3"), readme)
              journey.exported.keySet
                .filterNot(_ == "README.txt")
                .foreach(f => assert(readme.contains(s"- $f"), s"$f in $readme"))
              assert(journey.bundle.keySet.exists(_.endsWith(".svg")), journey.bundle.keySet)
          }
      }
  }
