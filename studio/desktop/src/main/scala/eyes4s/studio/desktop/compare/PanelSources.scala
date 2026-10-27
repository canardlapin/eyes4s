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
import eyes4s.studio.app.text.{PanelText, PanelTextId}
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
    (asset: AssetRef) =>
      Left(StimulusError.NotStored(asset.file, PanelText(PanelTextId.NoStimulusSource)))
  )

  /** Native content is read through the same revision, display and fixation ports
    * as Explore. Capture the immutable dataset declaration before asynchronous reads;
    * do not infer a stimulus, round times, or serve another revision's content.
    * Bead g2-native-panels records the production wiring regression.
    */
  def of(
      inputs: eyes4s.studio.desktop.explore.TrialViewInputs,
      dataset: eyes4s.studio.core.backend.AnalysisRevision => Option[
        eyes4s.studio.core.document.DatasetRevisionSpec
      ],
      stimuli: StimulusSource
  ): PanelSources =
    import eyes4s.studio.app.compare.{ContentError, ContentFixation, TrialContent}
    import eyes4s.studio.app.explore.DisplaySource
    import eyes4s.studio.app.explore.BackendAnswer
    val content: TrialContentSource = (revision, trial, done) =>
      def refused(reason: String): Unit = done(Left(ContentError.Unreadable(trial, reason)))
      dataset(revision) match
        case None => refused(s"${revision.label} has no declared dataset for ${trial.label}.")
        case Some(spec) =>
          inputs.displays(
            spec,
            {
              case Left(reason)                   => refused(reason)
              case Right(DisplaySource.NotServed) => done(Left(ContentError.NotServed(trial)))
              case Right(DisplaySource.Served(registry)) =>
                if registry.dataset != spec.id || registry.screen != spec.geometry.screen then
                  refused(
                    s"Display registry ${registry.dataset.label} / ${registry.screen} differs from ${spec.id.label} / ${spec.geometry.screen} for ${trial.label}."
                  )
                else
                  registry.display(trial) match
                    case None          => done(Left(ContentError.NotServed(trial)))
                    case Some(display) =>
                      inputs.fixations(
                        revision,
                        trial,
                        {
                          case Left(reason)                         => refused(reason)
                          case Right(BackendAnswer.Refused(reason)) => refused(reason)
                          case Right(BackendAnswer.Answered(rows))  =>
                            if rows.revision != revision || rows.dataset != spec.id || rows.trial != trial
                            then
                              refused(
                                s"Fixations ${rows.revision.label}/${rows.dataset.label}/${rows.trial.label} differ from requested ${revision.label}/${spec.id.label}/${trial.label}."
                              )
                            else
                              done(
                                Right(
                                  TrialContent(
                                    display,
                                    registry.screen,
                                    rows.fixations.map(f =>
                                      ContentFixation(
                                        trial,
                                        f.ref.index,
                                        f.screenX,
                                        f.screenY,
                                        f.onsetMs,
                                        f.durationMs,
                                        f.placement
                                      )
                                    )
                                  )
                                )
                              )
                        }
                      )
            }
          )
    PanelSources(content, stimuli)
