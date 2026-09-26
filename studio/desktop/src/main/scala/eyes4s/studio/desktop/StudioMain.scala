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

import eyes4s.studio.app.AppModel
import eyes4s.studio.viz.StudioViz
import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.control.Label
import javafx.stage.Stage

/** The JavaFX application: renders view-models and dispatches intents.
  *
  * Placeholder for S0.2. It shows one label and holds no behaviour; behaviour
  * belongs in studio-app, where it is tested headlessly.
  */
final class StudioApplication extends Application:

  override def start(stage: Stage): Unit =
    stage.setTitle(StudioMain.title)
    stage.setScene(Scene(Label(StudioViz.caption(AppModel.initial)), 480, 240))
    stage.show()

/** Entry point for the desktop shell. Tests never launch it. */
object StudioMain:

  /** The window title. */
  val title: String = "Eyes Studio"

  def main(args: Array[String]): Unit =
    Application.launch(classOf[StudioApplication], args*)
