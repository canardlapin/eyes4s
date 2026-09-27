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

import eyes4s.studio.app.Intent
import eyes4s.studio.app.importing.*
import eyes4s.studio.app.text.{ImportText, ImportTextId}
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.bundle.InputKind
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{Source, SourceRole, StudioDocument}
import eyes4s.studio.desktop.runtime.ProjectPort
import eyes4s.studio.core.importing.{ImportPreset, KeyGap, SniffedSource, SourceReadError}
import eyes4s.studio.desktop.platform.FilePresetStore
import javafx.application.Platform
import javafx.scene.Scene
import javafx.stage.{FileChooser, Stage, Window}

import java.nio.file.{Files, Path}
import java.util.concurrent.CompletableFuture
import scala.util.control.NonFatal

/** The platform services the wizard needs: a file dialog and a preset store. */
trait ImportPlatform:
  /** Ask for a file of `role`; `None` when the user cancels. */
  def chooseFile(role: SourceRole): Option[Path]

  /** Persist a newly saved preset; the error names what failed. */
  def storePreset(preset: ImportPreset): Either[String, Unit]

  /** Copy the file at `path` into the project before the command that
    * names `source` is applied. The file is read again and must still have
    * `source`'s bytes. `done` is called once, on the JavaFX thread.
    */
  def importInput(source: Source, path: Path, done: Either[String, Unit] => Unit): Unit

/** Runs the wizard's Elm loop on the desktop (ticket S5.2): each intent goes
  * through [[ImportWizard.update]], the view renders the new view-model, and
  * the effects are performed. Document commands go to the app through `app`
  * (as [[Intent.Dispatch]]), so the wizard never edits the document itself.
  * Called on the JavaFX thread; file reads run on a worker thread.
  */
final class ImportWizardHost(
    initial: ImportWizard,
    document: () => StudioDocument,
    app: Intent => Unit,
    platform: ImportPlatform,
    close: () => Unit
):
  private var state: ImportWizard = initial

  /** Where each file read came from, by digest: the bytes themselves are
    * not kept for the wizard's lifetime, only read again on commit.
    */
  private var readFrom: Map[ByteDigest, Path] = Map.empty

  val view: ImportWizardView = ImportWizardView(dispatch)
  render()

  def model: ImportWizard = state

  def render(): Unit = view.render(ImportWizardVM.of(state, document()))

  /** Start over on `wizard` (the column-mapping pane, when its dataset
    * revision changes or its edits are reverted): the files read so far are
    * forgotten.
    */
  def reset(wizard: ImportWizard): Unit =
    state = wizard
    readFrom = Map.empty
    render()

  def dispatch(intent: WizardIntent): Unit =
    val (next, effects) = ImportWizard.update(state, intent, document())
    state = next
    render()
    perform(effects)

  /** Read `path` as the `role` source, imported under `name` (a relative,
    * `/`-separated import name; the file's name by default). The bytes are
    * read, digested and sniffed off the JavaFX thread; the result is
    * dispatched on it, and the returned future completes after that.
    */
  def read(role: SourceRole, path: Path, name: Option[String] = None): CompletableFuture[Unit] =
    val importName = name.getOrElse(path.getFileName.toString)
    val done       = CompletableFuture[Unit]()
    val worker     = Thread(
      () =>
        val result =
          try
            val bytes = IArray.unsafeFromArray(Files.readAllBytes(path))
            SniffedSource
              .read(role, importName, bytes)
              .map(s => (WizardIntent.SourceRead(s), Some(s.bytes -> path)))
              .left
              .map(e => WizardIntent.ReadFailed(importName, e))
          catch
            case NonFatal(e) =>
              Left(
                WizardIntent.ReadFailed(
                  path.toString,
                  SourceReadError.Unreadable(
                    path.toString,
                    Option(e.getMessage).getOrElse(e.toString)
                  )
                )
              )
        Platform.runLater { () =>
          try
            result match
              case Right((intent, bytes)) =>
                bytes.foreach(readFrom += _)
                dispatch(intent)
              case Left(intent) => dispatch(intent)
            done.complete(()): Unit
          catch case NonFatal(e) => done.completeExceptionally(e): Unit
        }
      ,
      s"eyes4s-import-read-$importName"
    )
    worker.setDaemon(true)
    worker.start()
    done

  /** Perform a wizard update's effects in order. A new revision's files are
    * stored in the project before its command reaches the app, so the save
    * that follows the command lists them; if one cannot be stored, nothing
    * is applied, the wizard stays open and says why.
    */
  private def perform(effects: Vector[WizardEffect]): Unit =
    val inputs = effects.collect {
      case WizardEffect.Dispatch(Command.ImportSources(_, sources, _, _, _, _)) =>
        sources.entries.flatMap(s => readFrom.get(s.bytes).map(s -> _))
    }.flatten
    if inputs.isEmpty then effects.foreach(performOne)
    else
      var remaining = inputs.size
      var failed    = false
      inputs.foreach((source, path) =>
        platform.importInput(
          source,
          path,
          result =>
            if !failed then
              result match
                case Left(reason) =>
                  failed = true
                  dispatch(WizardIntent.StoreFailed(reason))
                case Right(()) =>
                  remaining -= 1
                  if remaining == 0 then effects.foreach(performOne)
        )
      )

  private def performOne(effect: WizardEffect): Unit = effect match
    case WizardEffect.Dispatch(command) => app(Intent.Dispatch(command))
    case WizardEffect.StorePreset(p)    =>
      platform.storePreset(p).left.foreach(reason => dispatch(WizardIntent.StoreFailed(reason)))
    case WizardEffect.OpenFile(role)  => platform.chooseFile(role).foreach(read(role, _): Unit)
    case check: WizardEffect.CheckKey => checkKey(check)
    case WizardEffect.Close           => close()

  /** The last streaming key check started; completes once it is dispatched. */
  @volatile private var lastCheck: CompletableFuture[Unit] =
    CompletableFuture.completedFuture(())

  /** Completes when the latest streaming key check has been dispatched. */
  def keyChecked: CompletableFuture[Unit] = lastCheck

  /** Check a trial key in one streaming pass over its file, off the JavaFX
    * thread (S5.3): the file is read again from where it was read, and only
    * the key's grouping is kept.
    */
  private def checkKey(check: WizardEffect.CheckKey): Unit =
    def refused(reason: String) = WizardIntent.KeyChecked(
      check.role,
      check.source,
      check.columns,
      check.unit,
      Left(KeyGap.Unreadable(check.file, reason))
    )
    val done = CompletableFuture[Unit]()
    lastCheck = done
    readFrom.get(check.source) match
      case None =>
        dispatch(refused("it was not read in this session"))
        done.complete(()): Unit
      case Some(path) =>
        val worker = Thread(
          () =>
            val answer =
              try
                val bytes = IArray.unsafeFromArray(Files.readAllBytes(path))
                if ByteDigest.sha256(bytes) != check.source then
                  refused(s"$path changed after it was read; read it again")
                else
                  SniffedSource
                    .decodeUtf8(check.file, bytes)
                    .fold(e => refused(e.message), KeyChecks.run(check, _))
              catch case NonFatal(e) => refused(Option(e.getMessage).getOrElse(e.toString))
            Platform.runLater { () =>
              try dispatch(answer)
              finally done.complete(()): Unit
            }
          ,
          s"eyes4s-key-check-${check.file}"
        )
        worker.setDaemon(true)
        worker.start()

object ImportWizardHost:

  /** The desktop's import platform: a JavaFX file chooser over `owner`, and
    * presets in `store`.
    */
  def fxPlatform(
      owner: () => Window,
      store: FilePresetStore,
      project: Option[ProjectPort]
  ): ImportPlatform =
    new ImportPlatform:
      def chooseFile(role: SourceRole): Option[Path] =
        val chooser = FileChooser()
        chooser.setTitle(ImportText(role match
          case SourceRole.Fixations => ImportTextId.DialogFixations
          case SourceRole.Trials    => ImportTextId.DialogTrials))
        chooser.getExtensionFilters.add(
          FileChooser.ExtensionFilter(
            ImportText(ImportTextId.DialogFilter),
            "*.csv",
            "*.tsv",
            "*.txt"
          )
        )
        Option(chooser.showOpenDialog(owner())).map(_.toPath)
      def storePreset(preset: ImportPreset): Either[String, Unit] = store.save(preset)
      def importInput(
          source: Source,
          path: Path,
          done: Either[String, Unit] => Unit
      ): Unit =
        val name = source.path.value.split('/').last
        // Answers arrive off the FX thread; the host lives on it.
        def answer(r: Either[String, Unit]): Unit = Platform.runLater(() => done(r))
        project match
          case None       => done(Left(s"$name: no project is open to store it"))
          case Some(port) =>
            val worker = Thread(
              () =>
                val read =
                  try Right(IArray.unsafeFromArray(Files.readAllBytes(path)))
                  catch
                    case NonFatal(e) =>
                      Left(s"$path: ${Option(e.getMessage).getOrElse(e.toString)}")
                read.flatMap(bytes =>
                  Either.cond(
                    ByteDigest.sha256(bytes) == source.bytes,
                    bytes,
                    s"$path changed after it was read; read it again"
                  )
                ) match
                  case Left(reason) => answer(Left(reason))
                  case Right(bytes) =>
                    port.importInput(InputKind.Source(source.role), name, bytes, answer)
              ,
              s"eyes4s-import-store-$name"
            )
            worker.setDaemon(true)
            worker.start()

  /** Open the wizard for a new import in its own window, over `document`,
    * with the presets of `store`. Its commands go to `app`.
    */
  def openWindow(
      document: () => StudioDocument,
      app: Intent => Unit,
      store: FilePresetStore,
      stylesheets: List[String],
      project: Option[ProjectPort]
  ): (Stage, ImportWizardHost) =
    val stage             = Stage()
    val (presets, errors) = store.load
    val host              = ImportWizardHost(
      ImportWizard.newImport(document(), presets),
      document,
      app,
      fxPlatform(() => stage, store, project),
      () => stage.close()
    )
    errors.foreach(e => System.err.println(e))
    val scene = Scene(host.view.node, 960, 640)
    scene.getStylesheets.setAll(stylesheets*)
    scene.getRoot.getStyleClass.add("es")
    stage.setTitle(ImportWizardVM.of(host.model, document()).title)
    stage.setScene(scene)
    stage.show()
    (stage, host)
