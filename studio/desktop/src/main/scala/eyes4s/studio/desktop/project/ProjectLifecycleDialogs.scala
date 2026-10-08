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

package eyes4s.studio.desktop.project

import eyes4s.studio.app.project.{ProjectCloseChoice, ProjectCloseFacts, ProjectLifecycleText}
import eyes4s.studio.core.session.Recovery
import javafx.scene.control.{Alert, ButtonBar, ButtonType}
import javafx.stage.Window

/** Nonblocking native admission: closing the dialog is always cancellation. */
final class ProjectLifecycleDialogs(owner: () => Option[Window]):
  def close(facts: ProjectCloseFacts, done: ProjectCloseChoice => Unit): Unit =
    val keep  = ButtonType("Keep open", ButtonBar.ButtonData.CANCEL_CLOSE)
    val close = ButtonType("Close without saving", ButtonBar.ButtonData.NO)
    val save  = ButtonType("Save and close", ButtonBar.ButtonData.YES)
    val alert = Alert(Alert.AlertType.CONFIRMATION)
    alert.setTitle("Close project")
    alert.setHeaderText(facts.title)
    alert.setContentText(ProjectLifecycleText.admission(facts))
    owner().foreach(alert.initOwner)
    alert.getButtonTypes.setAll((if facts.named then Vector(save, close, keep)
                                 else Vector(close, keep))*): Unit
    alert.setOnHidden(_ =>
      done(Option(alert.getResult) match
        case Some(`save`)  => ProjectCloseChoice.SaveAndClose
        case Some(`close`) => ProjectCloseChoice.CloseWithoutSaving
        case _             => ProjectCloseChoice.KeepOpen)
    )
    alert.show()

  /** None keeps the current project; true restores, false opens the saved version. */
  def recovery(value: Recovery, done: Option[Boolean] => Unit): Unit =
    val restore = ButtonType("Restore unsaved changes", ButtonBar.ButtonData.YES)
    val saved   = ButtonType("Open saved version", ButtonBar.ButtonData.NO)
    val cancel  = ButtonType("Cancel", ButtonBar.ButtonData.CANCEL_CLOSE)
    val alert   = Alert(Alert.AlertType.CONFIRMATION)
    alert.setTitle("Open project")
    alert.setHeaderText("This project has autosave recovery")
    alert.setContentText("Choose which version to open. The autosave is kept if you cancel.")
    owner().foreach(alert.initOwner)
    val canRestore = value.isInstanceOf[Recovery.Offered]
    alert.getButtonTypes.setAll((if canRestore then Vector(restore, saved, cancel)
                                 else Vector(saved, cancel))*): Unit
    alert.setOnHidden(_ =>
      done(Option(alert.getResult) match
        case Some(`restore`) => Some(true)
        case Some(`saved`)   => Some(false)
        case _               => None)
    )
    alert.show()

  def failed(operation: String, reason: String): Unit =
    val alert = Alert(Alert.AlertType.ERROR)
    alert.setTitle(operation)
    alert.setHeaderText("The current project stays open")
    alert.setContentText(reason)
    owner().foreach(alert.initOwner)
    alert.show()
