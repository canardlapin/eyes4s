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
import intaglio.{IntaglioError, Scene, plot}
import intaglio.interaction.{KeyCodec, KeySpace, Selection}

/** Intaglio scene builders over the app model.
  *
  * Placeholder for S0.3: it proves the pinned Intaglio links on both platforms.
  * The first real builder replaces it. Values come from the model; nothing is
  * computed here.
  */
object StudioScenes:

  /** A point at the origin, then one per applied intent, at (count, count).
    *
    * The origin keeps the initial model plottable: Intaglio rejects an empty
    * continuous range.
    */
  def intents(model: AppModel): Either[IntaglioError, Scene] =
    plot(Vector.range(0L, model.intentsApplied + 1L).map(_.toDouble))
      .aes(identity, identity)
      .geomPoint()
      .scene

  /** The key space that identifies intent marks to Intaglio interaction. */
  def intentKeys: Either[IntaglioError, KeySpace[Int]] =
    KeySpace("eyes4s.studio.intents", KeyCodec.integer)

  /** No intent mark selected (intaglio-interaction). */
  val noSelection: Selection[Int] = Selection()
