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

package eyes4s.studio.desktop.runtime

import eyes4s.studio.app.{AppEffect, AppModel, Intent, PlatformDialog, StoryModels}

import scala.collection.mutable

/** The desktop's Elm loop, without the toolkit. */
class StudioRuntimeSuite extends munit.FunSuite:

  private final class Recording extends EffectPerformer:
    val performed = mutable.ArrayBuffer.empty[AppEffect]
    def perform(effect: AppEffect, dispatch: Intent => Unit): Unit = performed += effect

  test("a listener that throws is reported; the others and the intent's effects still run") {
    val effects = Recording()
    val runtime = StudioRuntime(StoryModels.t2Compare, effects)
    val seen    = mutable.ArrayBuffer.empty[AppModel]
    var armed   = false
    runtime.listen(_ => if armed then throw IllegalStateException("render failed"))
    runtime.listen(seen += _)
    armed = true
    runtime.dispatch(Intent.RequestImport)
    runtime.dispatch(Intent.ShowProjectInfo)
    assertEquals(
      effects.performed.toVector,
      Vector(
        AppEffect.OpenDialog(PlatformDialog.ImportSources),
        AppEffect.OpenDialog(PlatformDialog.ProjectInfo)
      )
    )
    assertEquals(seen.size, 3) // on listen, then after each intent
    assertEquals(
      runtime.listenerFailures.map(f => (f.intent, f.error.getMessage)),
      Vector(
        Intent.RequestImport   -> "render failed",
        Intent.ShowProjectInfo -> "render failed"
      )
    )
  }

  test("an intent dispatched while one is applied is applied next, in order") {
    val order                       = mutable.ArrayBuffer.empty[AppEffect]
    lazy val runtime: StudioRuntime = StudioRuntime(
      StoryModels.t2Compare,
      (effect, dispatch) =>
        order += effect
        if effect == AppEffect.OpenDialog(PlatformDialog.ImportSources) then
          dispatch(Intent.ShowProjectInfo)
    )
    runtime.dispatch(Intent.RequestImport)
    assertEquals(
      order.toVector,
      Vector(
        AppEffect.OpenDialog(PlatformDialog.ImportSources),
        AppEffect.OpenDialog(PlatformDialog.ProjectInfo)
      )
    )
  }
