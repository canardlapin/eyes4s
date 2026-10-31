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

package eyes4s.studio.app.data

import eyes4s.studio.app.{Intent, StoryModels}
import eyes4s.studio.app.nav.{DataSection, Location, Place}
import eyes4s.studio.core.document.Perspective
import munit.FunSuite

/** Data's setup steps (overhaul v4): five steps in order, the trail's
  * section current, each opening its section of the shown dataset.
  */
class DataStepsSuite extends FunSuite:
  private val m  = StoryModels.t1Data
  private val vm = SourcesVM.of(SourcesPane.empty, m)

  test("five steps in the golden's order, the trail's Admission current") {
    val steps = DataSteps.vm(m, vm)
    assertEquals(
      steps.map(s => (s.number, s.title)),
      Vector(
        (1, "Sources"),
        (2, "Columns"),
        (3, "Trial identity"),
        (4, "Geometry"),
        (5, "Admission")
      )
    )
    assertEquals(steps.filter(_.current).map(_.section), Vector(DataSection.Admission))
    assertEquals(steps.last.state, StepState.Open)
  }

  test("a step opens its section of the dataset") {
    val d    = m.document.datasets.last.id
    val step = DataSteps.vm(m, vm).find(_.section == DataSection.Geometry).get
    assertEquals(
      step.open,
      Intent.Navigate(
        Location(
          Perspective.Data,
          Vector(Place.Dataset(d), Place.DataView(DataSection.Geometry))
        )
      )
    )
  }
