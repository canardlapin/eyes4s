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

package eyes4s.studio.viz.figure

import eyes4s.studio.app.figures.{GazeRole, GazeTrialVM, PageVM, PanelBody}
import eyes4s.studio.app.tokens.{StageVariant, Theme}
import eyes4s.studio.core.assets.{AssetLink, AssetRef}
import intaglio.value
import eyes4s.studio.viz.trial.{
  MarkStyle,
  StimulusRaster,
  TrialFixation,
  TrialRole,
  TrialScene,
  TrialSceneInput,
  TrialSceneOptions
}

/** Panels A and B drawn (follow-up to S9.2a): the trial's display and its
  * fixations as the trial view draws them, on a light stage, marked by role
  * (query or matched reference, DESIGN_SPEC section 5). The page and its
  * export draw the same input.
  */
object FigureGaze:

  def role(r: GazeRole): TrialRole = r match
    case GazeRole.Query   => TrialRole.Query
    case GazeRole.Matched => TrialRole.Matched

  /** The trial scene input of a gaze panel; `rasters` holds the decoded
    * images the host has. A fixation the scene refuses was already counted
    * out by the view-model's marks, so none is refused here.
    */
  def input(vm: GazeTrialVM, rasters: Map[AssetRef, StimulusRaster]): TrialSceneInput =
    TrialSceneInput(
      vm.display,
      vm.screen,
      vm.marks.flatMap(m =>
        TrialFixation
          .of(vm.trial, m.index, m.screenX, m.screenY, m.durationMs, m.placement)
          .toOption
      ),
      MarkStyle.Role(role(vm.role)),
      Theme.Light,
      StageVariant.Light,
      rasters,
      TrialSceneOptions(windowOutline = false)
    )

  /** Height over width of a gaze panel's drawing: the trial panel alone,
    * without the trial view's caption band (the figure panel says it in its
    * own words).
    */
  def heightRatio(vm: GazeTrialVM): Double =
    val extent = TrialScene.extentOf(input(vm, Map.empty))
    extent.height / extent.width

  /** The trial panel's grobs of a built scene: no stage surround and no
    * caption band.
    */
  def panelOnly(scene: TrialScene): Vector[intaglio.Grob] =
    scene.plot.scene.grobs.filter(_.name.exists(_.value == TrialScene.PanelName))

  /** The stored images the page's gaze panels display, to decode for an export. */
  def assets(page: PageVM): Vector[AssetRef] =
    page.panels
      .collect { case p => p.body }
      .collect { case PanelBody.Gaze(g) => g.drawn }
      .flatten
      .flatMap(_.display.asset)
      .collect { case AssetLink.Present(a) => a }
      .distinct
