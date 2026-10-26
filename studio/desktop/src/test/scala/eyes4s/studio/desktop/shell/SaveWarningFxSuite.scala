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

package eyes4s.studio.desktop.shell

import eyes4s.codec.ByteDigest
import eyes4s.studio.app.{AppEffect, AppModel, ClockTime, Intent, Notice}
import eyes4s.studio.core.bundle.StoreError
import eyes4s.studio.core.command.{Command, JournalEntry}
import eyes4s.studio.core.document.Theme
import eyes4s.studio.core.session.{PartRetention, SaveReceipt}
import eyes4s.studio.desktop.runtime.ProjectPort

class SaveWarningFxSuite extends ShellFxSuite:
  fxStage.test("a completed save with cleanup failure stays saved and shows its warning") {
    fx =>
      val warning = StoreError.Unwritable("retired part", "injected deletion refusal")
      val port    = new ProjectPort:
        def journal(entry: JournalEntry): Unit                    = ()
        def close(): Unit                                         = ()
        def save(done: Either[String, SaveReceipt] => Unit): Unit = done(
          Right(
            SaveReceipt(
              ByteDigest.sha256(IArray.empty[Byte]),
              Vector.empty,
              true,
              None,
              None,
              PartRetention.Collected(Vector.empty, Vector(warning))
            )
          )
        )
      val initial =
        AppModel
          .update(
            AppModel.newProject.toOption.get,
            Intent.Dispatch(Command.SetTheme(Theme.Dark))
          )
          ._1
      val w =
        boot(fx, initial, project = Some(port), clock = () => ClockTime.of(12, 0).toOption)
      runOnFx(w.effects.perform(AppEffect.Persist(initial.save.edits), w.runtime.dispatch))
      eventually(fx, "saved state and maintenance warning") {
        !w.runtime.model.save.edited && w.runtime.model.notice
          .contains(Notice.SaveWarning(warning.message))
      }
      assert(
        runOnFx(w.runtime.model.notice.get.message)
          .startsWith("The project was saved with a warning:")
      )
  }
