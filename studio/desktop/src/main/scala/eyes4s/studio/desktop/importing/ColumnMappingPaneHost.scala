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
  * commit goes to the app through `app` as its one command; a commit that
  * creates a revision moves the Data selection to it. Its Cancel reverts the
  * edits. Saved presets arrive through [[presetsLoaded]]. Use on the JavaFX
  * thread.
  */
final class ColumnMappingPaneHost(
    model: () => AppModel,
    app: Intent => Unit,
    platform: ImportPlatform,
    project: Option[ProjectPort]
):
  private var shown: Option[DatasetRevisionSpec] = None
  private var opened                             = false
  private var generation                         = 0L
  private var remaining                          = 0
  private var answered                           = 0L
  private var settled                            = CompletableFuture.completedFuture(())
  private var baseline: Option[ImportWizard]     = None
  private var notice: Option[PaneNotice]         = None

  /** The generation when the wizard's current commit began, if one did. */
  private var commitFrom: Option[Long] = None
  private var committing               = false

  val wizard: ImportWizardHost = ImportWizardHost(
    ImportWizard.newImport(model().document, ImportPresets.empty),
    () => model().document,
    commit,
    platform,
    () => revert()
  )

  private val empty   = ImportWizardView.label("import-empty", "t12")
  private val reading = ImportWizardView.label("import-status", "t12")
  private val note    = ImportWizardView.label("import-problem", "t12")
  note.setWrapText(true)
  VBox.setMargin(empty, javafx.geometry.Insets(12, 14, 12, 14))
  VBox.setMargin(reading, javafx.geometry.Insets(6, 14, 6, 14))
  VBox.setMargin(note, javafx.geometry.Insets(6, 14, 6, 14))
  VBox.setVgrow(wizard.view.node, Priority.ALWAYS)

  val node: VBox = VBox(empty, note, reading, wizard.view.node)
  node.getStyleClass.add("column-mapping-pane")
  render()

  /** The revision the pane shows, if any. */
  def dataset: Option[DatasetRevisionSpec] = shown

  /** Completes once every source of the current load has reached the
    * wizard; a load that is superseded first is cancelled.
    */
  def loaded: CompletableFuture[Unit] = settled

  /** How many project reads have answered, current or stale. */
  def readsAnswered: Long = answered

  /** The notice the pane shows now, if any. */
  def shownNotice: Option[PaneNotice] = notice

  /** The wizard's focus stops inside the pane (none while it is hidden). */
  def focusStops: Vector[FocusStop] =
    if !opened then Vector.empty
    else ColumnMappingPane.focusStops(ImportWizardVM.of(wizard.model, model().document))

  /** Follow the model: reload when the selected revision changes. Nothing
    * is read until the Data perspective has been shown.
    */
  def sync(m: AppModel): Unit =
    val started = shown.isDefined || m.perspective == Perspective.Data
    if started && ColumnMappingPane.mustReload(shown, m) then load(m, elsewhere = !committing)

  /** The saved presets, read off the JavaFX thread by the window; any that
    * could not be read are named in the pane's notice.
    */
  def presetsLoaded(presets: ImportPresets, errors: Vector[String]): Unit =
    wizard.reset(wizard.model.withPresets(presets))
    if errors.nonEmpty then notice = Some(PaneNotice.PresetsUnreadable(errors))
    render()

  /** The wizard's commit: its command, then the selection follows a
    * revision the command created, so the pane reloads onto it.
    */
  private def commit(intent: Intent): Unit =
    if commitFrom.isEmpty then commitFrom = Some(generation)
    val before = model().document
    committing = true
    try
      app(intent)
      ColumnMappingPane.follow(before, model()).foreach(app)
    finally committing = false

  /** Cancel in the pane, or the close after a commit: unless the commit
    * already moved the pane to a new load, drop the edits and read the
    * revision again.
    */
  private def revert(): Unit =
    val advanced = commitFrom.exists(_ != generation)
    commitFrom = None
    if !advanced then load(model(), elsewhere = false)

  /** Start a new load. `elsewhere`: the revision changed under the pane
    * (an undo, another edit), so edits not yet applied are dropped, and said.
    */
  private def load(m: AppModel, elsewhere: Boolean): Unit =
    val dropped =
      if !elsewhere then None
      else
        shown
          .filter(_ => baseline.exists(wizard.model.editedSince))
          .map(s => PaneNotice.EditsDropped(s.id))
    if !settled.isDone then settled.cancel(false): Unit
    generation += 1
    val current = generation
    notice = dropped
    baseline = None
    shown = ColumnMappingPane.selected(m)
    val result =
      shown.map(spec => ColumnMappingPane.open(m.document, spec, wizard.model.presets))
    opened = result.exists(_.isRight)
    result match
      case Some(Right((w, sources))) =>
        wizard.reset(w)
        remaining = sources.size
        settled = CompletableFuture[Unit]()
        sources.foreach(read(_, current))
      case other =>
        other.flatMap(_.left.toOption).foreach(p => notice = Some(PaneNotice.CannotOpen(p)))
        remaining = 0
        settled = CompletableFuture.completedFuture(())
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
    answered += 1
    if current == generation then
      wizard.dispatch(intent)
      remaining -= 1
      if remaining <= 0 then
        baseline = Some(wizard.model)
        render()
        settled.complete(()): Unit

  private def render(): Unit =
    val vm = ColumnMappingPane.vm(shown, remaining > 0, notice)
    empty.setText(vm.empty.getOrElse(""))
    empty.setVisible(vm.empty.isDefined)
    empty.setManaged(vm.empty.isDefined)
    reading.setText(vm.reading.getOrElse(""))
    reading.setVisible(vm.reading.isDefined)
    reading.setManaged(vm.reading.isDefined)
    note.setText(vm.notice.getOrElse(""))
    note.setVisible(vm.notice.isDefined)
    note.setManaged(vm.notice.isDefined)
    wizard.view.node.setVisible(opened)
    wizard.view.node.setManaged(opened)
