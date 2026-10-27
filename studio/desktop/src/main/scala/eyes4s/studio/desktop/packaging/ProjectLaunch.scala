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

/** OS open-file events can arrive before JavaFX has installed its project host.
  * Every requested path is delivered to the ordinary lifecycle host, which owns
  * dirty-project prompts, lock refusals and concurrent-operation refusals.
  */
final class ProjectLaunch:
  private var pending  = Vector.empty[String]
  private var receiver = Option.empty[String => Unit]

  def submit(paths: Vector[String]): Unit = synchronized {
    receiver match
      case Some(deliver) => paths.foreach(deliver)
      case None          => pending ++= paths
  }

  def attach(deliver: String => Unit): Unit = synchronized {
    receiver = Some(deliver)
    pending.foreach(deliver)
    pending = Vector.empty
  }

  def detach(): Unit = synchronized {
    receiver = None
    pending = Vector.empty
  }
