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
  import cats.effect.unsafe.implicits.global

  /** The dialog pane's id, for tests and styling. */
  val PaneId: String = "error-report"

  /** Show `vm` without waiting; `closed` runs when the dialog is hidden.
    * Call on the JavaFX thread.
    */
  def show(
      vm: ErrorDialogVM,
      closed: () => Unit,
      owner: Option[Window] = None,
      clipboard: Option[eyes4s.studio.core.platform.Clipboard[cats.effect.IO]] = None
  ): Alert =
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
    val log = Label(vm.log)
    log.setWrapText(true)
    copy.setOnAction { _ =>
      def finish(answer: Either[String, Unit]): Unit =
        if alert.isShowing then
          answer match
            case Right(_)     => copy.setText(vm.copied)
            case Left(reason) =>
              log.setText(
                eyes4s.studio.app.text
                  .ReportText(eyes4s.studio.app.text.ReportTextId.CopyFailed, reason)
              )
      clipboard match
        case Some(host) =>
          host.writeText(vm.report).unsafeRunAsync { result =>
            val answer = result.left.map(_.toString).flatMap(_.left.map(_.message))
            javafx.application.Platform.runLater(() => finish(answer))
          }
        case None =>
          val content = ClipboardContent()
          content.putString(vm.report)
          finish(
            Either.cond(
              Clipboard.getSystemClipboard.setContent(content),
              (),
              "Clipboard refused the report."
            )
          )
    }
    val summary = Label(vm.summary)
    summary.setWrapText(true)
    alert.setTitle(vm.title)
    alert.setHeaderText(vm.heading)
    alert.getDialogPane.setId(PaneId)
    alert.getDialogPane.setContent(VBox(8.0, summary, log, report, copy))
    alert.setResizable(true)
    owner.foreach(alert.initOwner)
    alert.setOnHidden(_ => closed())
    alert.show()
    alert
