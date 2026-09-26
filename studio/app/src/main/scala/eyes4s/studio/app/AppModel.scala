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

package eyes4s.studio.app

/** A user action a UI shell dispatches to the presentation layer.
  *
  * Placeholder for S0.2; S1.0 replaces it with the real intent vocabulary.
  */
enum Intent derives CanEqual:

  /** Acknowledge the current state without changing what is shown. */
  case Acknowledge

/** The UI-neutral application model that every view-model is derived from.
  *
  * Placeholder for S0.2; S1.0 replaces it. It counts the intents it has applied,
  * so the pure [[AppModel.update]] has an observable effect to test.
  */
final case class AppModel private (intentsApplied: Long) derives CanEqual

object AppModel:

  /** The model before any intent is applied. */
  val initial: AppModel = new AppModel(0L)

  /** The Elm-style update: a pure function of the model and one intent.
    *
    * The effect channel (for example `(AppModel, List[Effect])`) is decided in
    * S1.0; this placeholder returns the model alone.
    */
  def update(model: AppModel, intent: Intent): AppModel =
    intent match
      case Intent.Acknowledge => new AppModel(model.intentsApplied + 1L)
