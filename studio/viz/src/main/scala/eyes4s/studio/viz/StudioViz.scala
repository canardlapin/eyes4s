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

package eyes4s.studio.viz

import eyes4s.studio.app.AppModel
import eyes4s.studio.app.vm.Shell

/** Scene builders from view-models to Intaglio scenes.
  *
  * Placeholder for S0.2: Intaglio is pinned in S0.3, and the first scene builder
  * follows it. Cross-built for the JVM and Scala.js; no JavaFX here.
  */
object StudioViz:

  /** A caption for the model, standing in for a scene: its freshness badge. */
  def caption(model: AppModel): String = Shell.context(model).freshness.text
