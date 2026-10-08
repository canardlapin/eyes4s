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

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import eyes4s.studio.app.Intent
import eyes4s.studio.app.importing.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{Source, SourceRole, StudioDocument}
import eyes4s.studio.core.importing.{
  ImportPreset,
  ImportPresets,
  KeyGap,
  SourceReadError,
  StreamedSource
}
import eyes4s.studio.core.platform.{FileSystem, HostPath, Platform as HostPlatform}
import eyes4s.studio.desktop.platform.{DesktopPlatform, FilePresetStore, JvmFileSystem}
import eyes4s.studio.desktop.runtime.ProjectPort
import fs2.{Chunk, Stream}
import javafx.application.Platform
import javafx.scene.Scene
import javafx.stage.{Stage, Window}

import java.util.concurrent.CompletableFuture
import scala.concurrent.ExecutionContext

trait ImportPlatform:
  def files: FileSystem[IO]                              = JvmFileSystem
  def sourceName(path: HostPath): Either[String, String] =
    DesktopPlatform.fileName(path).left.map(_.message)
  def chooseFile(role: SourceRole): IO[Either[String, Option[ChosenSource]]]
  def storePreset(preset: ImportPreset): IO[Either[String, Unit]]
  def loadPresets: IO[(ImportPresets, Vector[String])] =
    IO.pure((ImportPresets.empty, Vector.empty))
  def importInput(source: Source, path: HostPath): IO[Either[String, Unit]]

enum ByteSource:
  case File(path: HostPath)
  case Project(read: (Either[String, IArray[Byte]] => Unit) => Unit)

  def stream(files: FileSystem[IO]): IO[Either[String, Stream[IO, Byte]]] = this match
    case File(path) =>
      files.readStream(path, StreamedSource.ChunkBytes).map(_.left.map(_.message))
    case Project(read) =>
      IO.async_[Either[String, Stream[IO, Byte]]](done =>
        read(answer =>
          done(
            Right(
              answer.map(bytes => Stream.chunk(Chunk.array(bytes.asInstanceOf[Array[Byte]])))
            )
          )
        )
      )

object ByteSource:
  def project(port: ProjectPort, source: Source): ByteSource =
    Project(done => port.readInput(source, done))

/** One asynchronous wizard controller. Every result returns to FX and is fenced by its request. */
final class ImportWizardHost(
    initial: ImportWizard,
    document: () => StudioDocument,
    app: Intent => Unit,
    platform: ImportPlatform,
    close: () => Unit
):
  private var state                                   = initial
  private var readFrom: Map[ByteDigest, ByteSource]   = Map.empty
  private var epoch                                   = 0L
  private var serial                                  = 0L
  private var requests: Map[SourceRole, Long]         = Map.empty
  private var tasks: Map[Long, (() => Unit, Boolean)] = Map.empty
  private var roleStops: Map[SourceRole, () => Unit]  = Map.empty
  private var presetLoad                              = 0L
  private var lastImport                              = CompletableFuture.completedFuture(())
  private var lastPreset                              = CompletableFuture.completedFuture(())
  private var presetQueue = Vector.empty[(ImportPreset, CompletableFuture[Unit])]
  private var presetBusy  = false
  private var reads: Map[SourceRole, CompletableFuture[Unit]] = Map.empty
  private var disposed                                        = false
  private var storing                                         = false
  @volatile private var lastCheck = CompletableFuture.completedFuture(())

  val view: ImportWizardView = ImportWizardView(dispatch)
  render()
  def model: ImportWizard                                    = state
  def keyChecked: CompletableFuture[Unit]                    = lastCheck
  def importCompleted: CompletableFuture[Unit]               = lastImport
  def presetWritten: CompletableFuture[Unit]                 = lastPreset
  def remember(digest: ByteDigest, source: ByteSource): Unit =
    if !disposed then readFrom += digest -> source
  def render(): Unit = if !disposed then view.render(ImportWizardVM.of(state, document()))

  private def invalidate(): Unit =
    epoch += 1
    val (cancelled, kept) = tasks.partition((_, task) => disposed || task._2)
    cancelled.values.foreach(_._1())
    tasks = kept
    roleStops = Map.empty
    reads.values.foreach(_.cancel(false))
    reads = Map.empty
    lastCheck.cancel(false)
    lastCheck = CompletableFuture.completedFuture(())
    requests = Map.empty
    if disposed then
      presetQueue.foreach(_._2.cancel(false))
      presetQueue = Vector.empty
    storing = false

  def reset(wizard: ImportWizard): Unit = if !disposed then
    invalidate()
    state = wizard
    readFrom = Map.empty
    render()
  def dispose(): Unit = if !disposed then
    disposed = true
    invalidate()
  def presetsLoaded(presets: ImportPresets): Unit = if !disposed then
    state = state.withPresets(presets)
    render()

  /** Per-user loads outlive dataset resets, and never replace a newer local save. */
  def loadPresets(done: Vector[String] => Unit = _ => ()): Unit = if !disposed then
    presetLoad += 1
    val current = presetLoad
    val saved   = state.presets
    val _       = launch(platform.loadPresets, resettable = false) {
      case Right((presets, errors)) if !disposed && current == presetLoad =>
        val loaded =
          if state.presets == saved then Right(presets)
          else
            ImportPresets.of(
              presets.all.filterNot(p =>
                state.presets.names.contains(p.name)
              ) ++ state.presets.all
            )
        loaded.fold(
          e => done(errors :+ e.message),
          value => { presetsLoaded(value); done(errors) }
        )
      case Left(e) if !disposed && current == presetLoad => done(Vector(reason(e)))
      case _                                             => ()
    }

  private def reason(error: Throwable): String =
    Option(error.getMessage).getOrElse(error.toString)
  private def launch[A](io: IO[A], resettable: Boolean = true)(
      answer: Either[Throwable, A] => Unit
  ): () => Unit =
    serial += 1
    val id               = serial
    val (future, cancel) = io.unsafeToFutureCancelable()
    val stop             = () => { cancel(); () }
    tasks += id -> (stop -> resettable)
    future.onComplete { result =>
      Platform.runLater(() =>
        tasks -= id
        answer(result.toEither)
      )
    }(using ExecutionContext.global)
    stop

  def dispatch(intent: WizardIntent): Unit = if !disposed then
    val (next, effects) = ImportWizard.update(state, intent, document())
    state = next
    render()
    perform(effects)

  private def token(role: SourceRole): (Long, Long) =
    roleStops.get(role).foreach(_())
    reads.get(role).foreach(_.cancel(false))
    serial += 1
    requests += role -> serial
    (epoch, serial)
  private def current(role: SourceRole, token: (Long, Long)): Boolean =
    !disposed && epoch == token._1 && requests.get(role).contains(token._2)

  def read(
      role: SourceRole,
      path: HostPath,
      name: Option[String] = None
  ): CompletableFuture[Unit] =
    readAt(role, path, name, token(role))

  private def readAt(
      role: SourceRole,
      path: HostPath,
      name: Option[String],
      request: (Long, Long)
  ): CompletableFuture[Unit] =
    val done = CompletableFuture[Unit]()
    reads += role -> done
    val named: Either[String, String] = name.fold(platform.sourceName(path))(Right(_))
    named match
      case Left(error) =>
        if current(role, request) then
          dispatch(
            WizardIntent.ReadFailed(
              path.value,
              SourceReadError.Unreadable(path.value, error.toString)
            )
          )
        done.complete(()): Unit
      case Right(importName) =>
        val io = platform.files.readStream(path, StreamedSource.ChunkBytes).flatMap {
          case Left(e)       => IO.pure(Left(SourceReadError.Unreadable(path.value, e.message)))
          case Right(stream) => StreamedSource.preview[IO](role, importName, stream)
        }
        val stop = launch(io) { answer =>
          if current(role, request) && !done.isDone then
            answer match
              case Right(Right(source)) =>
                remember(source.bytes, ByteSource.File(path))
                dispatch(WizardIntent.SourceRead(source))
              case Right(Left(e)) => dispatch(WizardIntent.ReadFailed(importName, e))
              case Left(e)        =>
                dispatch(
                  WizardIntent.ReadFailed(
                    path.value,
                    SourceReadError.Unreadable(path.value, reason(e))
                  )
                )
            done.complete(()): Unit
          else done.cancel(false): Unit
        }
        roleStops += role -> stop
        done.whenComplete((_, _) => if done.isCancelled then stop()): Unit
    done

  private def perform(effects: Vector[WizardEffect]): Unit =
    val inputs = effects.collect {
      case WizardEffect.Dispatch(Command.ImportSources(_, sources, _, _, _, _, _, _)) =>
        sources.entries.flatMap(s =>
          readFrom.get(s.bytes).collect { case ByteSource.File(path) => s -> path }
        )
    }.flatten
    if inputs.isEmpty then effects.foreach(performOne)
    else if !storing then
      storing = true
      val at        = epoch
      val submitted = state
      val done      = CompletableFuture[Unit]()
      lastImport = done
      val _ = launch(inputs.traverse { (source, path) => platform.importInput(source, path) }) {
        answer =>
          if !disposed && epoch == at then
            storing = false
            val result = answer.left.map(reason).flatMap(_.sequence)
            result match
              case Left(e) => dispatch(WizardIntent.StoreFailed(e))
              case Right(_) if state.editedSince(submitted) =>
                dispatch(
                  WizardIntent.StoreFailed(
                    "The import draft changed while its sources were stored; apply the current draft again."
                  )
                )
              case Right(_) =>
                effects.foreach {
                  case WizardEffect.Close => done.complete(()); performOne(WizardEffect.Close)
                  case effect             => performOne(effect)
                }
            done.complete(()): Unit
          else done.cancel(false): Unit
      }

  private def performOne(effect: WizardEffect): Unit = effect match
    case WizardEffect.Dispatch(command)   => app(Intent.Dispatch(command))
    case WizardEffect.StorePreset(preset) => persistPreset(preset): Unit
    case WizardEffect.OpenFile(role)      =>
      val request = token(role)
      val stop    = launch(platform.chooseFile(role)) {
        case Right(Right(Some(chosen))) if current(role, request) =>
          readAt(role, chosen.path, Some(chosen.name), request): Unit
        case Right(Left(e)) if current(role, request) =>
          dispatch(
            WizardIntent.ReadFailed(role.toString, SourceReadError.Unreadable(role.toString, e))
          )
        case Left(e) if current(role, request) =>
          dispatch(
            WizardIntent.ReadFailed(
              role.toString,
              SourceReadError.Unreadable(role.toString, reason(e))
            )
          )
        case _ => ()
      }
      roleStops += role -> stop
    case check: WizardEffect.CheckKey => checkKey(check)
    case WizardEffect.Close           => close()

  /** Preserve user action order even if an earlier same-name write is delayed. */
  private[importing] def persistPreset(preset: ImportPreset): CompletableFuture[Unit] =
    val done = CompletableFuture[Unit]()
    lastPreset = done
    if disposed then done.cancel(false): Unit
    else
      presetQueue :+= (preset -> done)
      startPresetWrite()
    done

  private def startPresetWrite(): Unit =
    if !disposed && !presetBusy then
      presetQueue.headOption.foreach { (preset, done) =>
        presetBusy = true
        val _ = launch(platform.storePreset(preset), resettable = false) { result =>
          if disposed then done.cancel(false): Unit
          else
            result match
              case Right(Left(e)) => dispatch(WizardIntent.StoreFailed(e))
              case Left(e)        => dispatch(WizardIntent.StoreFailed(reason(e)))
              case _              => ()
            presetQueue = presetQueue.drop(1)
            presetBusy = false
            done.complete(()): Unit
            startPresetWrite()
        }
      }

  private def checkKey(check: WizardEffect.CheckKey): Unit =
    def refused(why: String) = WizardIntent.KeyChecked(
      check.role,
      check.source,
      check.columns,
      check.unit,
      Left(KeyGap.Unreadable(check.file, why))
    )
    val done = CompletableFuture[Unit]()
    val at   = epoch
    lastCheck = done
    readFrom.get(check.source) match
      case None =>
        dispatch(refused("it was not read in this session"))
        done.complete(()): Unit
      case Some(source) =>
        val io = source.stream(platform.files).flatMap {
          case Left(e)       => IO.pure(refused(e))
          case Right(stream) =>
            StreamedSource.text[IO](check.file, stream).map {
              case Left(e)                                   => refused(e.message)
              case Right(read) if read.bytes != check.source =>
                refused(s"${check.file} changed after it was read; read it again")
              case Right(read) => KeyChecks.run(check, read.value)
            }
        }
        val stop = launch(io) { result =>
          if !disposed && epoch == at && !done.isDone then
            dispatch(result.fold(e => refused(reason(e)), identity))
            done.complete(()): Unit
          else done.cancel(false): Unit
        }
        done.whenComplete((_, _) => if done.isCancelled then stop()): Unit

object ImportWizardHost:
  def onPlatform(
      platform: HostPlatform[IO],
      store: FilePresetStore,
      project: Option[ProjectPort]
  ): ImportPlatform =
    PlatformImport(platform, store.on(platform.files), project)
  def fxPlatform(
      owner: () => Window,
      store: FilePresetStore,
      project: Option[ProjectPort]
  ): ImportPlatform =
    onPlatform(DesktopPlatform.create(_ => (), () => Option(owner())), store, project)

  def openWindow(
      document: () => StudioDocument,
      app: Intent => Unit,
      store: FilePresetStore,
      stylesheets: List[String],
      project: Option[ProjectPort],
      hostPlatform: Option[HostPlatform[IO]] = None
  ): (Stage, ImportWizardHost) =
    val stage    = Stage()
    val services =
      hostPlatform.fold(fxPlatform(() => stage, store, project))(onPlatform(_, store, project))
    val host = ImportWizardHost(
      ImportWizard.newImport(document(), ImportPresets.empty),
      document,
      app,
      services,
      () => stage.close()
    )
    host.loadPresets(errors => errors.foreach(e => System.err.println(e)))
    stage.setOnHidden(_ => host.dispose())
    val scene = Scene(host.view.node, 960, 640)
    scene.getStylesheets.setAll(stylesheets*)
    scene.getRoot.getStyleClass.add("es")
    stage.setTitle(ImportWizardVM.of(host.model, document()).title)
    stage.setScene(scene)
    stage.show()
    (stage, host)
