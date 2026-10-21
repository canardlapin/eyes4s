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

package eyes4s.studio.app.project

import eyes4s.studio.app.*
import eyes4s.studio.app.keys.CommandRegistry
import eyes4s.studio.app.vm.Menus
import eyes4s.studio.core.platform.HostPath
import eyes4s.studio.core.preferences.RecentProjects

class RecentProjectsModelSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  test(
    "recent projects are per-user view data and opening one requests ordinary project admission"
  ) {
    val original          = get(AppModel.newProject)
    val path              = get(HostPath.of("/studies/example.eyes"))
    val recent            = RecentProjects.empty.opened(path)
    val (loaded, effects) = AppModel.update(original, Intent.RecentProjectsLoaded(recent))
    assertEquals(effects, Vector.empty)
    assertEquals(loaded.history, original.history)
    assertEquals(loaded.save, original.save)
    val item =
      Menus.bar(loaded).flatMap(_.items).find(_.command == CommandRegistry.openRecent.id).get
    val action = item.children.get.head
    assertEquals(action.label, path.value)
    assert(action.enabled)
    val (same, opening) = AppModel.update(loaded, action.intent)
    assertEquals(same, loaded)
    assertEquals(opening, Vector(AppEffect.OpenDialog(PlatformDialog.OpenProjectAt(path))))
    val copy = AppModel.savedCopy(loaded, get(ProjectName.of("copy")), get(ClockTime.of(12, 0)))
    assertEquals(copy.recent, recent)
  }

  test("an empty recent menu explains itself without an actionable path") {
    val model = get(AppModel.newProject)
    val item  =
      Menus.bar(model).flatMap(_.items).find(_.command == CommandRegistry.openRecent.id).get
    assertEquals(item.children.get.map(_.label), Vector("No recent projects"))
    assert(item.children.get.forall(!_.enabled))
  }
