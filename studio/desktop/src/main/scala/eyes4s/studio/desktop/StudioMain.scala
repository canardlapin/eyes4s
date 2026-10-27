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

import cats.effect.IO
import cats.syntax.all.*
import cats.effect.unsafe.implicits.global
import eyes4s.studio.app.{AppModel, Intent, ProjectName, TrialItems}
import eyes4s.studio.core.fixture.{MockStudy, StoryMoments}
import eyes4s.studio.core.preferences.UserPreferences
import eyes4s.studio.core.document.Theme
import eyes4s.studio.app.appearance.AppearancePreference
import eyes4s.studio.desktop.platform.{
  AppearancePreferenceHost,
  PreferencesLocation,
  DesktopPlatform,
  JavaFxFonts
}
import eyes4s.studio.desktop.project.ProjectLifecycleHost
import eyes4s.studio.desktop.packaging.{MacProjectFiles, PackageCheck, ProjectLaunch}
import eyes4s.studio.app.project.ProjectOperation
import eyes4s.studio.core.platform.HostPath
import javafx.application.{Application, Platform}
import eyes4s.studio.desktop.explore.{NavigatorDisplays, SessionBackend}
import eyes4s.studio.app.report.LogState
import eyes4s.studio.desktop.report.{ErrorDialogView, ErrorReporter, StudioLog}
import eyes4s.studio.desktop.trial.StimulusSource
import javafx.stage.Stage

/** The JavaFX application: renders view-models and dispatches intents
  * (tickets S1.4, S1.5a, S2.9a). Starts untitled on the native backend;
  * File New/Open/Close are owned by the project lifecycle host. The story
  * helpers below remain explicit fixtures for native acceptance tests.
  */
final class StudioApplication extends Application:

  private var window: Option[StudioWindow]                            = None
  private var lifecycle: Option[ProjectLifecycleHost]                 = None
  private var appearancePreferences: Option[AppearancePreferenceHost] = None

  private var nativeFiles: Option[() => Unit] = None
  private var owner: Option[Stage]            = None
  private lazy val platform                   = DesktopPlatform.create(
    getHostServices.showDocument,
    () => owner
  )

  /** The user's preferences (S2.8), read before the first window opens. */
  private var preferences: UserPreferences = UserPreferences.defaults

  /** The preferences this run read; the appearance is applied and saved by
    * [[AppearancePreferenceHost]], the rest by S1.5b and S2.9.
    */
  def userPreferences: UserPreferences = preferences

  // S1.2: register the bundled faces before the first scene reads its CSS. A
  // face that fails to load falls back to the platform font; say which.
  // S2.8: read the preferences; an unusable file is the defaults, logged.
  override def init(): Unit =
    StudioMain
      .loadFonts(platform.fonts)
      .unsafeRunSync()
      .foreach(p => System.err.println(p.message))
    val (loaded, problems) = StudioMain.loadPreferences()
    preferences = loaded
    problems.foreach(m => System.err.println(PreferencesLocation.redact(m)))

  override def start(stage: Stage): Unit =
    nativeFiles = Some(MacProjectFiles.install(StudioMain.launchRequests.submit))
    owner = Some(stage)
    stage.setTitle(StudioMain.title)
    // S1.12: the log, then the error reporter, before anything can fail.
    val log = StudioLog.open()
    log.left.foreach(e => System.err.println(e.message))
    val reporter = ErrorReporter(
      ErrorReporter.facts,
      () => window.flatMap(w => ErrorReporter.digestOf(w.runtime.model)),
      log.toOption.map(_.logger("eyes4s.studio")),
      log.fold(e => LogState.Unavailable(e.message), _.state),
      (vm, closed) =>
        ErrorDialogView.show(
          vm,
          closed,
          Option(stage.getScene).map(_.getWindow),
          Some(platform.clipboard)
        ): Unit,
      clock =
        () => java.time.Instant.ofEpochMilli(platform.scheduler.now.unsafeRunSync().epochMillis)
    )
    reporter.install(): Unit
    val preferenceStore = PreferencesLocation.repository.toOption
    val appearance      = AppearancePreferenceHost(
      preferenceStore,
      preferences,
      m => System.err.println(PreferencesLocation.redact(m))
    )
    appearancePreferences = Some(appearance)
    val projects = ProjectLifecycleHost(
      stage,
      platform,
      replaced = w =>
        window = Some(w)
        appearance.attach(w.runtime)
      ,
      defect = reporter.jobFailed,
      preferences = preferenceStore,
      initialRecent = preferences.recent,
      initialLayouts = preferences.layouts,
      preferencesLog = message => System.err.println(PreferencesLocation.redact(message)),
      newProjectTheme = () =>
        AppearancePreference.newProjectTheme(
          appearance.current,
          window.map(_.runtime.model.appearance.system).getOrElse(Theme.Light)
        )
    )
    lifecycle = Some(projects)
    projects.start()
    stage.show()
    StudioMain.launchRequests.attach(path =>
      Platform.runLater(() =>
        HostPath.of(path) match
          case Right(chosen) => projects.request(ProjectOperation.Open, Some(chosen))
          case Left(error)   => System.err.println(error.message)
      )
    )

  override def stop(): Unit =
    nativeFiles.foreach(_())
    nativeFiles = None
    StudioMain.launchRequests.detach()
    lifecycle.foreach(_.shutdown())
    lifecycle = None
    appearancePreferences.foreach(_.close.unsafeRunSync())
    appearancePreferences = None
    window = None
    owner = None
    Platform.exit()

/** Entry point for the desktop shell. Tests never launch it. */
object StudioMain:

  /** The bundled example runs through the native fixation-study backend. */
  val backend: SessionBackend = SessionBackend.Real

  /** The application's trial displays: the golden registry of the story
    * session, which answers only for the golden trials.csv.
    */
  val displays: NavigatorDisplays = NavigatorDisplays.golden

  def sources: eyes4s.studio.core.real.DatasetSources[cats.effect.IO] =
    eyes4s.studio.desktop.runtime.DatasetSourceHosts
      .golden(java.nio.file.Paths.get(sys.props("user.dir"), "fixtures", "studio-golden"))

  /** The story session's stimuli: fixtures/studio-golden's `stimuli/`, read
    * from the working directory (a checkout, as `sbt studioDesktop/run` has).
    * Elsewhere none is stored, and the trial view says the asset is
    * unreadable rather than showing another image.
    */
  def stimuli: StimulusSource =
    StimulusSource.directory(
      java.nio.file.Paths.get(sys.props("user.dir"), "fixtures", "studio-golden", "stimuli")
    )

  /** Register every bundled face, retaining each refusal for startup reporting. */
  def loadFonts(
      fonts: eyes4s.studio.core.platform.Fonts[IO]
  ): IO[Vector[eyes4s.studio.core.platform.PlatformError]] =
    JavaFxFonts.bundled.traverse(fonts.register).map(_.collect { case Left(error) => error })

  /** The user's preferences from their file, and what to log about it. */
  def loadPreferences(): (UserPreferences, Vector[String]) =
    PreferencesLocation.store match
      case Left(e) => (UserPreferences.defaults, Vector(s"No preferences file: ${e.message}"))
      case Right(store) =>
        val (p, problems) = store.load.unsafeRunSync()
        (p, problems.map(_.message))

  /** The window title before a project is shown. */
  val title: String = "Eyes Studio"

  /** memory-study at t2: rev 4 · run 7 · data r3 current, draft rev 5. */
  def initialModel: Either[String, AppModel] =
    for
      document <- StoryMoments.t2
      name     <- ProjectName.of("memory-study").left.map(_.message)
      study    <- MockStudy.load
    yield AppModel
      .update(
        AppModel.open(document, Some(name)),
        Intent.ItemsLoaded(TrialItems(study.inventory.map(e => e.trial -> e.item).toMap))
      )
      ._1

  private[desktop] val launchRequests: ProjectLaunch = ProjectLaunch()

  def main(args: Array[String]): Unit =
    args.toList match
      case "--package-check" :: output :: Nil => PackageCheck.run(java.nio.file.Path.of(output))
      case "--package-check" :: _             =>
        System.err.println("Usage: Eyes Studio --package-check <new output directory>")
        sys.exit(2)
      case _ =>
        launchRequests.submit(args.toVector)
        Application.launch(classOf[StudioApplication])
