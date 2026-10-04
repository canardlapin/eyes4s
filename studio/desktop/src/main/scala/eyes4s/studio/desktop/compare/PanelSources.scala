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

package eyes4s.studio.desktop.compare

import eyes4s.studio.app.compare.TrialContentSource
import eyes4s.studio.core.assets.AssetRef
import eyes4s.studio.desktop.trial.{StimulusError, StimulusSource}

/** Where Compare's trial panels get what they draw: each trial's content
  * through the [[TrialContentSource]] port, and stored stimuli's bytes.
  * Injected, since S6.2's trial fixations serve the content in a window.
  */
final case class PanelSources(content: TrialContentSource, stimuli: StimulusSource)

object PanelSources:

  /** No content and no stimuli: the panels say so and draw nothing. */
  val notServed: PanelSources = PanelSources(
    TrialContentSource.notServed,
    (asset: AssetRef) => Left(StimulusError.NotStored(asset.file, "no stimulus source"))
  )
