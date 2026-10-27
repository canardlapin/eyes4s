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

package eyes4s.studio.desktop.packaging

import com.sun.glass.ui.{Application as Glass}

/** Pinned JavaFX 24 macOS bridge. Glass owns Apple events; starting AWT first
  * changes native-menu ownership. JavaFX has no public open-document callback.
  * Keep this seam isolated and forward every other event to QuantumToolkit.
  * Its setEventHandler also replays documents delivered before FX startup.
  * See docs/studio/PACKAGING.md for the upstream contract and qualification.
  */
private[desktop] object MacProjectFiles:
  def install(open: Vector[String] => Unit): () => Unit =
    if !sys.props.get("os.name").exists(_.startsWith("Mac")) then () => ()
    else
      val app      = Glass.GetApplication()
      val previous = app.getEventHandler
      app.setEventHandler(Forwarding(Option(previous), open))
      () => app.setEventHandler(previous)

  private[packaging] final class Forwarding(
      previous: Option[Glass.EventHandler],
      open: Vector[String] => Unit
  ) extends Glass.EventHandler:
    override def handleOpenFilesAction(app: Glass, time: Long, files: Array[String]): Unit =
      open(Option(files).fold(Vector.empty[String])(_.toVector))
    override def handlePreferencesChanged(values: java.util.Map[String, Object]): Unit =
      previous.foreach(_.handlePreferencesChanged(values))
    override def handleWillFinishLaunchingAction(app: Glass, time: Long): Unit =
      previous.foreach(_.handleWillFinishLaunchingAction(app, time))
    override def handleDidFinishLaunchingAction(app: Glass, time: Long): Unit =
      previous.foreach(_.handleDidFinishLaunchingAction(app, time))
    override def handleWillBecomeActiveAction(app: Glass, time: Long): Unit =
      previous.foreach(_.handleWillBecomeActiveAction(app, time))
    override def handleDidBecomeActiveAction(app: Glass, time: Long): Unit =
      previous.foreach(_.handleDidBecomeActiveAction(app, time))
    override def handleWillResignActiveAction(app: Glass, time: Long): Unit =
      previous.foreach(_.handleWillResignActiveAction(app, time))
    override def handleDidResignActiveAction(app: Glass, time: Long): Unit =
      previous.foreach(_.handleDidResignActiveAction(app, time))
    override def handleDidReceiveMemoryWarning(app: Glass, time: Long): Unit =
      previous.foreach(_.handleDidReceiveMemoryWarning(app, time))
    override def handleWillHideAction(app: Glass, time: Long): Unit =
      previous.foreach(_.handleWillHideAction(app, time))
    override def handleDidHideAction(app: Glass, time: Long): Unit =
      previous.foreach(_.handleDidHideAction(app, time))
    override def handleWillUnhideAction(app: Glass, time: Long): Unit =
      previous.foreach(_.handleWillUnhideAction(app, time))
    override def handleDidUnhideAction(app: Glass, time: Long): Unit =
      previous.foreach(_.handleDidUnhideAction(app, time))
    override def handleQuitAction(app: Glass, time: Long): Unit =
      previous.foreach(_.handleQuitAction(app, time))
