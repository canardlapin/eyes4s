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

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.importing.*
import eyes4s.studio.app.vm.FocusStop
import eyes4s.studio.core.document.{DatasetRevisionSpec, Perspective, Source}
import eyes4s.studio.core.importing.{ImportPresets, SniffedSource}
import eyes4s.studio.desktop.runtime.ProjectPort
import javafx.application.Platform
import javafx.scene.layout.{Priority, VBox}

import java.util.concurrent.CompletableFuture
import scala.util.control.NonFatal

/** The Data perspective's column-mapping pane on the desktop (Data.dc.html,
  * column mapping; see [[ColumnMappingPane]]): the import wizard's view, on
  * a re-map of the selected dataset revision. When the model's selected
  * revision changes it starts a new re-map and reads that revision's own
  * sources back from `project`, each read and sniffed off the JavaFX thread;
  * a read that finishes after the pane moved on is dropped. The wizard's
  * commit goes to the app through `app` as its one command, and its Cancel
  * reverts the edits. Use on the JavaFX thread.
  */
final class ColumnMappingPaneHost(
    model: () => AppModel,
    app: Intent => Unit,
    platform: ImportPlatform,
    presets: () => ImportPresets,
    project: Option[ProjectPort]
):
  private var shown: Option[DatasetRevisionSpec] = None
  private var generation                         = 0L
  private var remaining                          = 0
  private var committed                          = false
  private var settled                            = CompletableFuture.completedFuture(())

  val wizard: ImportWizardHost = ImportWizardHost(
    ImportWizard.newImport(model().document, ImportPresets.empty),
    () => model().document,
    intent =>
      committed = true
      app(intent)
    ,
    platform,
    () => revert()
  )

  private val empty   = ImportWizardView.label("import-empty", "t12")
  private val reading = ImportWizardView.label("import-status", "t12")
  VBox.setMargin(empty, javafx.geometry.Insets(12, 14, 12, 14))
  VBox.setMargin(reading, javafx.geometry.Insets(6, 14, 6, 14))
  VBox.setVgrow(wizard.view.node, Priority.ALWAYS)

  val node: VBox = VBox(empty, reading, wizard.view.node)
  node.getStyleClass.add("column-mapping-pane")
  render()

  /** The revision the pane shows, if any. */
  def dataset: Option[DatasetRevisionSpec] = shown

  /** Completes once every source of the current load has reached the wizard. */
  def loaded: CompletableFuture[Unit] = settled

  /** The wizard's focus stops inside the pane (none while it is hidden). */
  def focusStops: Vector[FocusStop] =
    if shown.isEmpty then Vector.empty
    else ColumnMappingPane.focusStops(ImportWizardVM.of(wizard.model, model().document))

  /** Follow the model: reload when the selected revision changes. Nothing
    * is read until the Data perspective has been shown.
    */
  def sync(m: AppModel): Unit =
    val started = shown.isDefined || m.perspective == Perspective.Data
    if started && ColumnMappingPane.mustReload(shown, m) then load(m)

  /** Cancel in the pane: drop the edits and read the revision again. After a
    * commit, the model's update has already reloaded the pane.
    */
  private def revert(): Unit =
    if committed then committed = false
    else load(model())

  private def load(m: AppModel): Unit =
    generation += 1
    val current = generation
    shown = ColumnMappingPane.selected(m)
    val opened =
      shown.flatMap(spec => ColumnMappingPane.open(m.document, spec, presets()).toOption)
    opened match
      case None =>
        remaining = 0
        settled = CompletableFuture.completedFuture(())
      case Some((w, sources)) =>
        wizard.reset(w)
        remaining = sources.size
        settled = CompletableFuture[Unit]()
        sources.foreach(read(_, current))
    render()

  private def read(source: Source, current: Long): Unit =
    project match
      case None =>
        deliver(current, ColumnMappingPane.unreadable(source, ColumnMappingPane.noProject))
      case Some(port) =>
        port.readInput(
          source,
          result =>
            // Sniff on a worker: the port may answer on the JavaFX thread.
            val worker = Thread(
              () =>
                val intent =
                  try
                    result.fold(
                      ColumnMappingPane.unreadable(source, _),
                      bytes =>
                        SniffedSource
                          .read(source.role, source.path.value, bytes)
                          .fold(
                            WizardIntent.ReadFailed(source.path.value, _),
                            WizardIntent.SourceRead(_)
                          )
                    )
                  catch
                    case NonFatal(e) =>
                      ColumnMappingPane.unreadable(
                        source,
                        Option(e.getMessage).getOrElse(e.toString)
                      )
                Platform.runLater(() => deliver(current, intent))
              ,
              s"eyes4s-remap-read-${source.path.value}"
            )
            worker.setDaemon(true)
            worker.start()
        )

  private def deliver(current: Long, intent: WizardIntent): Unit =
    if current == generation then
      wizard.dispatch(intent)
      remaining -= 1
      if remaining <= 0 then
        render()
        settled.complete(()): Unit

  private def render(): Unit =
    val vm = ColumnMappingPane.vm(shown, remaining > 0)
    empty.setText(vm.empty.getOrElse(""))
    empty.setVisible(vm.empty.isDefined)
    empty.setManaged(vm.empty.isDefined)
    reading.setText(vm.reading.getOrElse(""))
    reading.setVisible(vm.reading.isDefined)
    reading.setManaged(vm.reading.isDefined)
    wizard.view.node.setVisible(shown.isDefined)
    wizard.view.node.setManaged(shown.isDefined)
