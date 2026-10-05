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

package eyes4s.studio.desktop.importing

import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.desktop.StudioWindow

/** The import wizard's commit is followed (S10.5 K2): a command that creates
  * a dataset revision takes Data to it, as the column-mapping pane's commit
  * does; one that creates none leaves Data where it is. Headless.
  */
class ImportFollowSuite extends munit.FunSuite:

  private val r2 = StoryMoments.r2
  private val r3 = StoryMoments.r3

  /** The journey's document before its import, on Data at r2. */
  private def start: AppModel =
    AppModel.open(eyes4s.studio.app.driver.GoldenJourney.history, Some(StoryModels.project))

  private def run(intents: Intent*): AppModel =
    var m      = start
    val follow = StudioWindow.followImports(() => m, i => m = AppModel.update(m, i)._1)
    intents.foreach(follow)
    m

  private val importR3: Intent =
    val r2Spec = StoryModels.t1.dataset(r2).get
    Intent.Dispatch(
      Command.ImportSources(
        Some(r2),
        r2Spec.sources,
        r2Spec.mapping,
        r2Spec.units,
        r2Spec.geometry,
        DeclaredAttributes.empty,
        None,
        StoryModels.t1.dataset(r3).get.inventory
      )
    )

  test("an import that creates r3 takes Data to r3") {
    assertEquals(start.location, Location(Perspective.Data, Vector(Place.Dataset(r2))))
    val m = run(importR3)
    assertEquals(m.document.dataset(r3).map(_.decision), Some(AdmissionDecision.Pending))
    assertEquals(m.location, Location(Perspective.Data, Vector(Place.Dataset(r3))))
  }

  test("an intent that creates no revision leaves Data where it is") {
    val m =
      run(Intent.Dispatch(Command.SetUnits(r2, DeclaredUnits(Some(TimeUnit.Milliseconds)))))
    assertEquals(m.location, start.location)
  }
