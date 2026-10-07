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

package eyes4s.studio.app

class InputChecksSuite extends munit.FunSuite:
  test("superseded input checks cannot refresh the UI before or after the newest check") {
    val first  = AppModel.update(StoryModels.t2Analysis, Intent.CheckInputs)._1
    val latest = AppModel.update(first, Intent.CheckInputs)._1
    assertEquals(
      AppModel.update(latest, Intent.InputsChecked(first.checks.asked, Vector.empty)),
      (latest, Vector.empty)
    )
    val (answered, effects) = AppModel.update(
      latest,
      Intent.InputsCheckFailed(latest.checks.asked, "changed outside the studio")
    )
    assertEquals(effects, Vector.empty)
    assertEquals(answered.checks.answered, latest.checks.asked)
    assertEquals(
      AppModel.update(answered, Intent.InputsChecked(first.checks.asked, Vector.empty)),
      (answered, Vector.empty)
    )
  }
