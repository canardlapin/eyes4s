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

class AppModelSuite extends munit.FunSuite:

  test("update applies each intent once, purely") {
    val once = AppModel.update(AppModel.initial, Intent.Acknowledge)
    assertEquals(once.intentsApplied, 1L)
    assertEquals(AppModel.update(AppModel.initial, Intent.Acknowledge), once)
    assertEquals(AppModel.initial.intentsApplied, 0L)
  }
