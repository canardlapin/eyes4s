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

package eyes4s.studio.desktop

import cats.effect.unsafe.IORuntime
import eyes4s.studio.app.{AppModel, Intent, PlatformDialog}
import eyes4s.studio.app.layout.StudioLayouts
import eyes4s.studio.app.text.{MessageId, Messages}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.app.ProjectName
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.dock.{LayoutRestoreError, PerspectiveHost}
import eyes4s.studio.desktop.runtime.{
  DesktopEffects,
  PlatformDialogs,
  StudioRuntime,
  StudioSession
}
import eyes4s.studio.desktop.shell.AppShell
import javafx.application.Platform
import javafx.scene.control.{Alert, TextInputDialog}
import scaladock.fx.DockTheme

/** Why a studio window could not open. */
enum WindowError:
  case Styles(missing: MissingStylesheet)
  case Layouts(error: LayoutRestoreError)

  def message: String = this match
    case Styles(m)  => m.message
    case Layouts(e) => e.message

/** One studio window's parts (tickets S1.4, S1.5a): the services, the Elm
  * runtime, the perspective host and the shell that renders it.
  */
final class StudioWindow private (
    val session: StudioSession,
    val runtime: StudioRuntime,
    val host: PerspectiveHost,
    val shell: AppShell,
    val effects: DesktopEffects
):
  /** The window content, with the studio stylesheets. */
  def root: javafx.scene.Parent = shell.root

  /** The title the native window shows now. */
  def title: String = eyes4s.studio.app.vm.Menus.windowTitle(runtime.model)

  /** Store each perspective's arrangement in the document (view-only). */
  def captureLayouts(): Unit = runtime.dispatch(Intent.LayoutsCaptured(host.capture()))

  /** Keep `stage`'s title on the model's window title (S1.4). */
  def bind(stage: javafx.stage.Stage): Unit = runtime.listen(_ => stage.setTitle(title))

  def close(): Unit = session.close()

object StudioWindow:

  /** The studio's dock theme: `studio-dock.css`, which maps scaladock's
    * variables onto the studio tokens.
    */
  val dockThemeResource: String = s"${tokens.TokenFiles.resourceDirectory}/studio-dock.css"

  def dockTheme: Either[MissingStylesheet, DockTheme] =
    Option(getClass.getClassLoader.getResource(dockThemeResource))
      .toRight(MissingStylesheet(dockThemeResource))
      .map(url => DockTheme.Custom(url.toExternalForm))

  /** Platform dialogs as JavaFX dialogs (non-blocking). */
  def fxDialogs(model: () => AppModel, messages: Messages): PlatformDialogs =
    (dialog: PlatformDialog, dispatch: Intent => Unit) =>
      dialog match
        case PlatformDialog.RenameProject =>
          val d = TextInputDialog(model().project.fold("")(_.value))
          d.setTitle(messages(MessageId.CommandRenameProject))
          d.setHeaderText(eyes4s.studio.app.vm.Menus.windowTitle(model(), messages))
          d.setOnHidden(_ =>
            Option(d.getResult)
              .flatMap(ProjectName.of(_).toOption)
              .foreach(n => dispatch(Intent.RenameProject(n)))
          )
          d.show()
        case PlatformDialog.ProjectInfo =>
          val a = Alert(Alert.AlertType.INFORMATION)
          a.setTitle(messages(MessageId.CommandProjectInfo))
          a.setHeaderText(eyes4s.studio.app.vm.Menus.windowTitle(model(), messages))
          a.show()
        case PlatformDialog.ImportSources | PlatformDialog.OpenProject =>
          System.err.println(s"$dialog is not available until S5.2 and S2.9.")

  /** Open a window on `initial`, served by the fake backend at `moment`.
    * On the JavaFX thread. Each execution-service event reaches the model as
    * [[Intent.Execution]] on the JavaFX thread; jobs the document's running
    * runs name are adopted first (t3's run 8).
    */
  def open(
      initial: AppModel,
      moment: StoryMoment,
      theme: Theme = Theme.Light,
      dialogs: Option[PlatformDialogs] = None,
      messages: Messages = Messages.english
  )(using IORuntime): Either[WindowError, StudioWindow] =
    for
      sheets <- StudioStyles.stylesheets(theme).left.map(WindowError.Styles(_))
      dock   <- dockTheme.left.map(WindowError.Styles(_))
      window <- build(initial, moment, dock, dialogs, messages)
    yield
      window.root.getStylesheets.setAll(sheets*)
      window

  private def build(
      initial: AppModel,
      moment: StoryMoment,
      dockTheme: DockTheme,
      dialogs: Option[PlatformDialogs],
      messages: Messages
  )(using IORuntime): Either[WindowError, StudioWindow] =
    // Late-bound: the runtime, the host and the effects refer to each other.
    var runtime: Option[StudioRuntime] = None
    def dispatch(i: Intent): Unit      = runtime.foreach(_.dispatch(i))
    def later(i: Intent): Unit         = Platform.runLater(() => dispatch(i))

    val session = StudioSession.start(moment, e => later(Intent.Execution(e)))
    val host    = PerspectiveHost(
      StudioLayouts.spec,
      dockTheme,
      pane =>
        Platform.runLater { () =>
          runtime
            .filter(_.model.focusedPane != pane)
            .foreach(_.dispatch(Intent.FocusPane(pane)))
        }
    )
    val effects = DesktopEffects(
      session,
      dialogs.getOrElse(fxDialogs(() => runtime.fold(initial)(_.model), messages)),
      p =>
        host.reset(p)
        runtime.foreach(r => host.sync(r.model))
      ,
      f => Platform.runLater(() => f())
    )
    val adopted = session.adopt(initial.document)
    adopted.collect { case Left(e) => e }.foreach(e => System.err.println(e.message))
    val booted = AppModel.update(initial, Intent.JobsChanged(session.jobs))._1
    val r      = StudioRuntime(booted, effects)
    runtime = Some(r)
    val shell = AppShell(host, dispatch, messages)
    host
      .restore(booted.document.presentation.layouts, booted)
      .left
      .map { e =>
        session.close()
        WindowError.Layouts(e)
      }
      .map { _ =>
        r.listen(shell.render)
        StudioWindow(session, r, host, shell, effects)
      }
