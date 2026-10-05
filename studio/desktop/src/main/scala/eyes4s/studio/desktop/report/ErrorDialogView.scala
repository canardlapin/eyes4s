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

package eyes4s.studio.desktop.report

import eyes4s.studio.app.report.ErrorDialogVM
import javafx.scene.control.{Alert, Button, ButtonType, Label, TextArea}
import javafx.scene.input.{Clipboard, ClipboardContent}
import javafx.scene.layout.VBox
import javafx.stage.Window

/** The error report dialog (ticket S1.12): binds [[ErrorDialogVM]] to a
  * non-modal alert whose report text can be selected and copied.
  */
object ErrorDialogView:

  /** The dialog pane's id, for tests and styling. */
  val PaneId: String = "error-report"

  /** Show `vm` without waiting; `closed` runs when the dialog is hidden.
    * Call on the JavaFX thread.
    */
  def show(vm: ErrorDialogVM, closed: () => Unit, owner: Option[Window] = None): Alert =
    val close  = ButtonType(vm.close, ButtonType.CLOSE.getButtonData)
    val alert  = Alert(Alert.AlertType.ERROR, "", close)
    val report = TextArea(vm.report)
    report.setEditable(false)
    report.setWrapText(false)
    report.setPrefRowCount(16)
    report.setAccessibleText(vm.reportAccessible)
    report.setId("error-report-text")
    val copy = Button(vm.copy)
    copy.setId("error-report-copy")
    copy.setOnAction { _ =>
      val content = ClipboardContent()
      content.putString(vm.report)
      Clipboard.getSystemClipboard.setContent(content)
      copy.setText(vm.copied)
    }
    val summary = Label(vm.summary)
    summary.setWrapText(true)
    val log = Label(vm.log)
    log.setWrapText(true)
    alert.setTitle(vm.title)
    alert.setHeaderText(vm.heading)
    alert.getDialogPane.setId(PaneId)
    alert.getDialogPane.setContent(VBox(8.0, summary, log, report, copy))
    alert.setResizable(true)
    owner.foreach(alert.initOwner)
    alert.setOnHidden(_ => closed())
    alert.show()
    alert
