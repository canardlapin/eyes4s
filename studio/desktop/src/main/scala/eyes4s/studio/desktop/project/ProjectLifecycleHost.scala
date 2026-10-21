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

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import eyes4s.studio.app.{AppModel, ClockTime, Intent, PlatformDialog, ProjectName}
import eyes4s.studio.app.project.*
import eyes4s.studio.app.vm.Menus
import eyes4s.studio.core.bundle.{LockOwner, ProjectStore, SharingOptions}
import eyes4s.studio.core.document.{DatasetRevisionSpec, Source}
import eyes4s.studio.core.platform.{FileKind, FileRequest, HostPath, Platform as HostPlatform}
import eyes4s.studio.core.real.DatasetSources
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.session.{ProjectSession, Recovery, SaveReceipt}
import eyes4s.studio.core.command.History
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.explore.NavigatorDisplays
import eyes4s.studio.desktop.platform.{FilePresetStore, DesktopPlatform}
import eyes4s.studio.desktop.runtime.{DatasetSourceHosts, PlatformDialogs, SessionPort}
import eyes4s.studio.desktop.trial.{StimulusError, StimulusSource}
import javafx.application.Platform as FxPlatform
import javafx.scene.Scene
import javafx.stage.Stage
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.jdk.CollectionConverters.*

/** Owns one native project window and stages replacements without moving its resources.
  * The callback on replacement reattaches application-wide appearance/error reporting.
  */
final class ProjectLifecycleHost(
    stage: Stage,
    platform: HostPlatform[IO],
    replaced: StudioWindow => Unit = _ => (),
    presets: FilePresetStore = FilePresetStore.userDefault,
    defect: (String, Throwable) => Unit = (_, _) => (),
    admission: Option[ProjectLifecycleDialogs] = None,
    displayName: HostPath => Either[eyes4s.studio.core.platform.PlatformError, String] =
      DesktopPlatform.fileName
)(using runtime: IORuntime)
    extends PlatformDialogs:
  private final class Candidate(
      val path: HostPath,
      val store: ProjectStore[IO],
      val session: ProjectSession[IO],
      val port: SessionPort,
      val name: ProjectName,
      val recovery: Option[Recovery],
      val copyHistory: Option[History] = None,
      val warnings: Vector[String] = Vector.empty
  ):
    private val released  = AtomicBoolean(false)
    def release: IO[Unit] = IO.defer {
      if released.compareAndSet(false, true) then
        IO.blocking(port.close())
          .guarantee(session.close.flatMap {
            case Right(_) => IO.unit
            case Left(e)  => IO.raiseError(IllegalStateException(e.message))
          })
      else IO.unit
    }

  private final case class Live(window: StudioWindow, project: Option[Candidate])
  private val prompts        = admission.getOrElse(ProjectLifecycleDialogs(() => Some(stage)))
  private var state          = ProjectLifecycle.empty
  private var current        = Option.empty[Live]
  private var staged         = Option.empty[Candidate]
  private var recoveryChoice = Option.empty[Boolean]
  private val ticket         = AtomicReference[Option[ProjectOperationId]](None)
  private val stopped        = AtomicBoolean(false)
  // Registered before a callback is queued, so shutdown can release a loaded
  // project even when JavaFX no longer accepts queued work.
  private val arriving = ConcurrentHashMap[ProjectOperationId, Candidate]()

  def window: Option[StudioWindow] = current.map(_.window)
  def busy: Boolean                = state.busy

  private def replaceState(next: ProjectLifecycle): Unit =
    state = next
    ticket.set(next.operation.map(_._1))

  private def context: ProjectCloseFacts = current.fold(
    ProjectCloseFacts("Eyes Studio", false, false, false)
  )(live =>
    val model    = live.window.runtime.model
    val jobs     = model.jobs.jobs.filterNot(_.phase.isTerminal)
    val jobRuns  = jobs.map(_.run).toSet
    val requests = model.document.runs.filter(r =>
      r.state == eyes4s.studio.core.document.RunLifecycle.Running
    )
    val work = jobs.map(j => ProjectWork(j.run, Some(j.id))).toSet ++
      requests.filterNot(r => jobRuns(r.id)).map(r => ProjectWork(r.id, None))
    ProjectCloseFacts(
      Menus.windowTitle(model),
      live.project.isDefined,
      model.save.edited,
      work.nonEmpty,
      work
    )
  )

  private def later(body: => Unit): Unit          = FxPlatform.runLater(() => body)
  private def release(candidate: Candidate): Unit =
    candidate.release.unsafeRunAsync {
      case Left(e) if !stopped.get => later(prompts.failed("Close project", e.getMessage))
      case _                       => ()
    }

  private def clearCandidate(): Unit =
    staged.foreach(release)
    staged = None
    recoveryChoice = None

  /** The actual launcher starts empty on the native backend. */
  def start(): Unit =
    val model = AppModel.newProject.left.map(_.message)
    model.flatMap(make(_, None)) match
      case Left(reason) => prompts.failed("Start Eyes Studio", reason)
      case Right(w)     => install(Live(w, None))
    stage.setOnCloseRequest(event =>
      event.consume()
      request(ProjectOperation.Quit)
    )

  def open(dialog: PlatformDialog, dispatch: Intent => Unit): Unit = dialog match
    case PlatformDialog.NewProject    => request(ProjectOperation.New)
    case PlatformDialog.OpenProject   => request(ProjectOperation.Open)
    case PlatformDialog.CloseProject  => request(ProjectOperation.Close)
    case PlatformDialog.SaveProjectAs => request(ProjectOperation.SaveAs)
    case _ => () // StudioWindow routes its other dialogs to the existing host.

  private val unavailableSources: DatasetSources[IO] = new DatasetSources[IO]:
    def bytes(dataset: DatasetRevisionSpec, source: Source): IO[Option[IArray[Byte]]] =
      IO.pure(None)
    def assets(dataset: DatasetRevisionSpec): IO[Option[AssetRegistry]] = IO.pure(None)

  private def make(model: AppModel, project: Option[Candidate]): Either[String, StudioWindow] =
    val stimuli: StimulusSource = project.fold[StimulusSource](asset =>
      Left(StimulusError.NotStored(asset.file, "an untitled project"))
    )(p => StimulusSource.bundle(p.store))
    scala.util
      .Try(
        StudioWindow.open(
          model,
          StoryMoment.T2,
          project.fold(NavigatorDisplays.notServed)(p => NavigatorDisplays.stored(p.port)),
          stimuli,
          project = project.map(_.port),
          presets = presets,
          defect = defect,
          nativeSources =
            Some(project.fold(unavailableSources)(p => DatasetSourceHosts.stored(p.port))),
          lifecycle = Some(this),
          hostPlatform = Some(platform)
        )
      )
      .toEither
      .left
      .map(_.getMessage)
      .flatMap(_.left.map(_.message))

  private def install(live: Live): Unit =
    val old = current
    current = Some(live)
    // Keep native scene observers, size and styles across project replacement.
    // Window-specific key handlers and theme sheets belong to the replaced root.
    Option(stage.getScene) match
      case Some(scene) => scene.setRoot(live.window.root)
      case None        => stage.setScene(Scene(live.window.root, 1440.0, 900.0))
    live.window.bind(stage)
    replaced(live.window)
    old.foreach(dispose)

  private def dispose(live: Live): Unit =
    live.window.close()
    // These operations perform only FIFO/file-system cleanup, never marshal
    // to FX. Finish releasing the writer before an immediate reopen.
    live.project.foreach(_.release.unsafeRunSync())

  private def name(path: HostPath): Either[String, ProjectName] =
    displayName(path).left
      .map(_.message)
      .flatMap(n => ProjectName.of(n.stripSuffix(".eyes")).left.map(_.message))

  private def acquire(
      action: ProjectOperation,
      path: HostPath,
      source: Option[(AppModel, Option[Candidate])]
  ): IO[Either[String, Candidate]] =
    def warnings(receipt: SaveReceipt): Vector[String] =
      (receipt.journalReconcile.toVector ++ receipt.journalReset.toVector).map(_.message)
    (for
      label <- cats.data.EitherT.fromEither[IO](name(path))
      _     <- cats.data.EitherT(
        if action == ProjectOperation.New || action == ProjectOperation.SaveAs then
          platform.files.list(path).map {
            case Left(_: eyes4s.studio.core.platform.PlatformError.Missing) => Right(())
            case Left(error)                       => Left(error.message)
            case Right(entries) if entries.isEmpty => Right(())
            case Right(_)                          =>
              val reason =
                if action == ProjectOperation.SaveAs then ProjectLifecycleText.emptyCopyLocation
                else ProjectLifecycleText.emptyNewLocation
              Left(s"${path.value}: $reason")
          }
        else IO.pure(Right(()): Either[String, Unit])
      )
      store <- cats.data.EitherT(platform.files.project(path).map(_.left.map(_.message)))
      owner <- cats.data.EitherT.fromEither[IO](
        LockOwner.of("Eyes Studio project lifecycle").left.map(_.message)
      )
      loaded <- cats.data.EitherT(action match
        case ProjectOperation.New =>
          AppModel.newProject
            .map(_.document)
            .left
            .map(_.message)
            .fold(
              e => IO.pure(Left(e)),
              d =>
                ProjectSession
                  .create[IO](store, owner, d, SharingOptions.complete, Vector.empty)
                  .map(
                    _.left
                      .map(_.message)
                      .map(session => (session, Option.empty[Recovery], Vector.empty[String]))
                  )
            )
        case ProjectOperation.SaveAs =>
          val copied: IO[Either[String, (ProjectSession[IO], SaveReceipt)]] = source match
            case Some((_, Some(project))) =>
              IO.async_[Either[String, (ProjectSession[IO], SaveReceipt)]](done =>
                project.port.saveAs(store, answer => done(Right(answer)))
              )
            case Some((model, None)) =>
              ProjectSession
                .createWithHistory[IO](
                  store,
                  owner,
                  model.history,
                  SharingOptions.complete,
                  Vector.empty
                )
                .map(_.left.map(_.message))
            case None => IO.pure(Left(ProjectLifecycleText.nothingToCopy))
          copied.map(_.map { (session, receipt) =>
            (session, Option.empty[Recovery], warnings(receipt))
          })
        case _ =>
          ProjectSession
            .open[IO](store, owner)
            .map(_.left.map(_.message).map { o =>
              val recovery = o.report.journal match
                case eyes4s.studio.core.session.JournalFinding.Pending(value) => Some(value)
                case _                                                        => None
              (o.session, recovery, Vector.empty[String])
            }))
      (session, recovery, notes) = loaded
      port <- cats.data.EitherT(IO.blocking(SessionPort.start(session)).attempt.flatMap {
        case Right(p) => IO.pure(Right(p))
        case Left(e)  => session.close.as(Left(e.getMessage))
      })
      history <-
        if action == ProjectOperation.SaveAs then
          cats.data.EitherT.liftF[IO, String, Option[History]](session.history.map(Some(_)))
        else cats.data.EitherT.pure[IO, String](Option.empty[History])
    yield Candidate(path, store, session, port, label, recovery, history, notes)).value

  private def choose(action: ProjectOperation): IO[Option[HostPath]] = action match
    case ProjectOperation.New | ProjectOperation.SaveAs =>
      val kind = FileKind.of("Eyes Studio project", Vector("eyes")).toOption.toVector
      platform.dialogs.chooseSave(
        FileRequest(ProjectLifecycleText.operation(action), kind, Some("Untitled.eyes"))
      )
    case _ => platform.dialogs.chooseDirectory("Open project")

  /** Close requests may cancel a still-loading replacement, keeping ownership clear. */
  def request(action: ProjectOperation): Unit =
    if action == ProjectOperation.SaveAs && context.activeWork then
      prompts.failed(ProjectLifecycleText.operation(action), ProjectLifecycleText.activeCopy)
      return
    val source = current.map(live => (live.window.runtime.model, live.project))
    if action == ProjectOperation.Quit && state.busy then
      replaceState(state.operation.fold(state)(o => state.cancel(o._1)))
      clearCandidate()
    val (next, id) = state.begin(action)
    replaceState(next)
    id match
      case None =>
        if !state.stopped then
          prompts.failed(
            ProjectLifecycleText.operation(action),
            "Finish the current project operation first."
          )
      case Some(operationId) =>
        action match
          case ProjectOperation.Close | ProjectOperation.Quit => admit(operationId)
          case _                                              =>
            choose(action)
              .flatMap {
                case None       => IO.pure(Right(None))
                case Some(path) => acquire(action, path, source).map(_.map(Some(_)))
              }
              .unsafeRunAsync {
                case Right(Right(Some(project))) =>
                  arriving.put(operationId, project): Unit
                  if stopped.get || !ticket.get.contains(operationId) then
                    arriving.remove(operationId): Unit
                    release(project)
                  else
                    later {
                      Option(arriving.remove(operationId)).foreach { loaded =>
                        if state.owns(operationId) then
                          staged = Some(loaded)
                          loaded.recovery match
                            case None           => admit(operationId)
                            case Some(recovery) =>
                              prompts.recovery(
                                recovery,
                                answer =>
                                  if state.owns(operationId) then
                                    answer match
                                      case None         => cancel(operationId)
                                      case Some(choice) =>
                                        recoveryChoice = Some(choice); admit(operationId)
                              )
                        else release(loaded)
                      }
                    }
                case Right(Right(None)) =>
                  later(if state.owns(operationId) then cancel(operationId))
                case Right(Left(reason)) => later(failed(operationId, action, reason))
                case Left(error)         => later(failed(operationId, action, error.getMessage))
              }

  private def failed(id: ProjectOperationId, action: ProjectOperation, reason: String): Unit =
    if state.owns(id) then
      cancel(id)
      prompts.failed(ProjectLifecycleText.operation(action), reason)

  private def cancel(id: ProjectOperationId): Unit =
    if state.owns(id) then
      replaceState(state.cancel(id))
      clearCandidate()

  private def admit(id: ProjectOperationId): Unit =
    val (next, effect) = state.prepared(id, context)
    replaceState(next)
    perform(id, effect)

  private def perform(id: ProjectOperationId, action: ProjectLifecycleAction): Unit =
    action match
      case ProjectLifecycleAction.Ignored    => ()
      case ProjectLifecycleAction.KeepOpen   => clearCandidate()
      case ProjectLifecycleAction.Ask(facts) =>
        prompts.close(
          facts,
          choice =>
            val (next, effect) = state.choose(id, choice)
            replaceState(next)
            perform(id, effect)
        )
      case ProjectLifecycleAction.Save =>
        current.foreach { live =>
          live.window.captureLayouts()
          val mark = live.window.runtime.model.save.edits
          live.project.foreach(_.port.save { answer =>
            val clock =
              if answer.isRight then platform.scheduler.now.map(Some(_)) else IO.pure(None)
            clock.unsafeRunAsync(result =>
              later {
                if state.owns(id) then
                  result.toOption.flatten.foreach(t =>
                    ClockTime
                      .of(t.localHour, t.localMinute)
                      .foreach(at => live.window.runtime.dispatch(Intent.Saved(at, mark)))
                  )
                  val succeeded      = answer.isRight && result.isRight
                  val (next, effect) = state.saved(id, succeeded, context)
                  replaceState(next)
                  if !succeeded then
                    prompts.failed(
                      "Save project",
                      answer.swap.toOption
                        .orElse(result.left.toOption.map(_.getMessage))
                        .getOrElse("Save failed.")
                    )
                  perform(id, effect)
              }
            )
          })
        }
      case ProjectLifecycleAction.Commit => commit(id)

  private def commit(id: ProjectOperationId): Unit =
    state.operation.filter(_._1 == id).foreach { (_, action, _) =>
      action match
        case ProjectOperation.Quit =>
          replaceState(state.finished(id))
          shutdown()
          stage.hide()
        case ProjectOperation.Close =>
          AppModel.newProject.left.map(_.message).flatMap(make(_, None)) match
            case Left(reason) => failed(id, action, reason)
            case Right(w)     =>
              current.foreach(_.window.captureLayouts())
              install(Live(w, None))
              replaceState(state.finished(id))
        case _ =>
          staged.foreach { project =>
            val settle = recoveryChoice match
              case Some(true) =>
                project.recovery match
                  case Some(Recovery.Offered(offer)) =>
                    project.session.accept(offer).map(_.left.map(_.message).void)
                  case _ => IO.pure(Left("The autosave cannot be restored."))
              case Some(false) =>
                project.recovery.fold(IO.pure(Right(()): Either[String, Unit]))(r =>
                  project.session.decline(r.journal).map(_.left.map(_.message).void)
                )
              case None => IO.pure(Right(()): Either[String, Unit])
            settle
              .flatMap {
                case Left(reason) => IO.pure(Left(reason))
                case Right(_)     =>
                  project.copyHistory match
                    case None =>
                      project.session.document.map(d => Right((d, Option.empty[ClockTime])))
                    case Some(history) =>
                      platform.scheduler.now.map(t =>
                        ClockTime
                          .of(t.localHour, t.localMinute)
                          .left
                          .map(_.message)
                          .map(at => (history.document, Some(at)))
                      )
              }
              .unsafeRunAsync {
                case Right(Right((document, at))) =>
                  later {
                    if state.owns(id) then
                      val model: Either[String, AppModel] = (project.copyHistory, at) match
                        case (Some(history), Some(time)) =>
                          current
                            .filter(live =>
                              live.window.runtime.model.history == history && !context.activeWork
                            )
                            .toRight(ProjectLifecycleText.changedDuringCopy)
                            .map(live =>
                              AppModel.savedCopy(live.window.runtime.model, project.name, time)
                            )
                        case (None, _) => Right(AppModel.open(document, Some(project.name)))
                        case _         => Left(ProjectLifecycleText.clockUnavailable)
                      model.flatMap(make(_, Some(project))) match
                        case Left(reason) => failed(id, action, reason)
                        case Right(w)     =>
                          if action != ProjectOperation.SaveAs then
                            current.foreach(_.window.captureLayouts())
                          staged = None
                          recoveryChoice = None
                          install(Live(w, Some(project)))
                          replaceState(state.finished(id))
                          if project.warnings.nonEmpty then
                            prompts.warning(
                              ProjectLifecycleText.operation(action),
                              project.warnings.mkString("\n")
                            )
                  }
                case Right(Left(reason)) => later(failed(id, action, reason))
                case Left(error)         => later(failed(id, action, error.getMessage))
              }
          }
    }

  /** Idempotent application stop: stale IO completions release their own candidates. */
  def shutdown(): Unit =
    if stopped.compareAndSet(false, true) then
      replaceState(state.shutdown)
      clearCandidate()
      arriving.values.asScala.foreach(release)
      arriving.clear()
      current.foreach(dispose)
      current = None
