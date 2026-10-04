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

import cats.effect.unsafe.implicits.global
import eyes4s.studio.app.{AppModel, Intent, ProjectName, TrialItems}
import eyes4s.studio.core.fixture.{MockStudy, StoryMoment, StoryMoments}
import javafx.application.{Application, Platform}
import javafx.scene.Scene
import javafx.scene.control.Label
import eyes4s.studio.desktop.explore.{NavigatorDisplays, SessionBackend}
import eyes4s.studio.desktop.trial.StimulusSource
import javafx.stage.Stage

/** The JavaFX application: renders view-models and dispatches intents
  * (tickets S1.4, S1.5a). Until the project lifecycle (S2.9) and the real
  * backend (S3.7) exist, it opens the memory-study project at story moment
  * t2 on the fake backend. Behaviour belongs in studio-app, where it is
  * tested headlessly.
  */
final class StudioApplication extends Application:

  private var window: Option[StudioWindow] = None

  // S1.2: register the bundled faces before the first scene reads its CSS. A
  // face that fails to load falls back to the platform font; say which.
  override def init(): Unit =
    eyes4s.studio.desktop.typography.StudioFonts
      .loadAll()
      .foreach(p => System.err.println(p.message))

  override def start(stage: Stage): Unit =
    stage.setTitle(StudioMain.title)
    StudioMain.initialModel.flatMap(
      StudioWindow
        .open(_, StoryMoment.T2, StudioMain.displays, StudioMain.stimuli)
        .left
        .map(_.message)
    ) match
      case Left(problem) =>
        stage.setScene(Scene(Label(problem), 480, 240))
      case Right(w) =>
        window = Some(w)
        w.bind(stage)
        stage.setScene(Scene(w.root, 1440, 900))
        stage.setOnCloseRequest(_ => w.captureLayouts())
    stage.show()

  override def stop(): Unit =
    window.foreach(_.close())
    Platform.exit()

/** Entry point for the desktop shell. Tests never launch it. */
object StudioMain:

  /** The application's session: the story at t2 on the fake backend. */
  val backend: SessionBackend = SessionBackend.Story

  /** The application's trial displays: the golden registry of the story
    * session, which answers only for the golden trials.csv.
    */
  val displays: NavigatorDisplays = NavigatorDisplays.of(backend, None)

  /** The story session's stimuli: fixtures/studio-golden's `stimuli/`, read
    * from the working directory (a checkout, as `sbt studioDesktop/run` has).
    * Elsewhere none is stored, and the trial view says the asset is
    * unreadable rather than showing another image.
    */
  def stimuli: StimulusSource =
    StimulusSource.directory(
      java.nio.file.Paths.get(sys.props("user.dir"), "fixtures", "studio-golden", "stimuli")
    )

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

  def main(args: Array[String]): Unit =
    Application.launch(classOf[StudioApplication], args*)
