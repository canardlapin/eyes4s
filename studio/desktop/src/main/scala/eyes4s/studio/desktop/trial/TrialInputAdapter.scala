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

package eyes4s.studio.desktop.trial

import eyes4s.studio.app.Intent
import eyes4s.studio.app.text.{TrialText, TrialTextId}
import eyes4s.studio.core.selection.{SelectionState, StudioRef, ViewId}
import eyes4s.studio.desktop.plot.{MarkInputAdapter, MarkLayer, PlotFrame}
import eyes4s.studio.viz.plot.OverlayPalette
import eyes4s.studio.viz.trial.{TrialInputState, TrialTargetError, TrialTargets, accessibleText}
import intaglio.IntaglioError
import javafx.application.Platform

/** Why the adapter could not be attached. */
enum TrialInputError derives CanEqual:
  /** A pointer tolerance must be finite and not negative. */
  case InvalidTolerance(toleranceLogicalPx: Double)

  def message: String = this match
    case InvalidTolerance(t) =>
      s"pointer tolerance $t logical px is not finite and non-negative"

/** Pointer and keyboard input for a [[TrialView]] (ticket S4.2).
  *
  * The view's canvas host is one focus stop with a roving cursor over the
  * trial's fixation marks: [[eyes4s.studio.desktop.plot.MarkInputAdapter]],
  * the input of every view with marks, whose targets here are the drawn
  * trial's [[TrialTargets]]. Every fixation mark is its own pick target; a
  * pick, Enter or Escape is an [[Intent.Select]]; a hover change is the view's
  * own [[Intent.HoverOver]]. The rings are an overlay on the host, the 2 px
  * accent focus ring cased by the stage halo (DESIGN_SPEC sections 10 and 14),
  * and the focused mark's accessible text comes from its
  * [[eyes4s.studio.core.selection.StudioRef]].
  *
  * FX thread only. [[dispose]] removes every handler and listener it added.
  */
final class TrialInputAdapter private (
    core: MarkInputAdapter[StudioRef.Fixation, TrialTargetError, TrialTargets]
):

  /** The input state: cursor, hover and the projected selection. */
  def state: TrialInputState = core.state

  /** The targets of the frame on the canvas, if a trial is drawn. */
  def targets: Option[TrialTargets] = core.targets

  /** The last input the targets refused (an Intaglio picking failure). */
  def lastError: Option[TrialTargetError] = core.lastError

  /** The last overlay Intaglio refused to build or compile, if any. */
  def lastOverlayError: Option[IntaglioError] = core.lastOverlayError

  def isDisposed: Boolean = core.isDisposed

  /** The selection as the bus now holds it: redraws the overlay, emits nothing. */
  def project(selection: SelectionState): Unit = core.project(selection)

  /** The view gained or lost keyboard focus: the focus ring shows only while
    * it has it.
    */
  private[trial] def focusChanged(now: Boolean): Unit = core.focusChanged(now)

  /** Removes the adapter's handlers, listeners and overlay. Idempotent. */
  def dispose(): Unit = core.dispose()

object TrialInputAdapter:

  /** The pointer tolerance: a hit within this many logical pixels of a mark's
    * painted outline picks it.
    */
  val DefaultTolerancePx: Double = 4.0

  /** The trial view's marks: the targets of the trial the view shows, when
    * the frame on the canvas was compiled from it.
    */
  private final class TrialLayer(view: TrialView)
      extends MarkLayer[StudioRef.Fixation, TrialTargetError, TrialTargets]:
    def resolve(frame: PlotFrame): Option[Either[TrialTargetError, TrialTargets]] =
      view.status.get match
        case TrialViewStatus.Shown(scene) if scene.plot.scene eq frame.plan.scene =>
          Some(
            TrialTargets.resolve(scene, frame.device, frame.picking, frame.surface.deviceScale)
          )
        case _ => None
    def palette: Option[OverlayPalette] =
      view.input.map(in => OverlayPalette.of(in.theme, in.stage))
    def spoken(state: TrialInputState, targets: TrialTargets): Option[String] =
      view.input.map(in => state.accessibleText(in.display.trial, targets))
    def roleDescription: String = TrialText(TrialTextId.PlotRole)

  /** Attaches input to `view` as `viewId`, showing `selection`; selection and
    * hover intents go to `dispatch`. On the FX application thread.
    */
  def attach(
      view: TrialView,
      viewId: ViewId,
      selection: SelectionState,
      dispatch: Intent => Unit,
      toleranceLogicalPx: Double = DefaultTolerancePx
  ): Either[TrialInputError, TrialInputAdapter] =
    if !Platform.isFxApplicationThread then
      throw IllegalStateException(
        "TrialInputAdapter.attach must run on the JavaFX application thread, " +
          s"not ${Thread.currentThread.getName}"
      )
    else if !toleranceLogicalPx.isFinite || toleranceLogicalPx < 0.0 then
      Left(TrialInputError.InvalidTolerance(toleranceLogicalPx))
    else
      Right(
        TrialInputAdapter(
          MarkInputAdapter(
            view.plotHost,
            TrialLayer(view),
            TrialInputState.initial(viewId, selection),
            dispatch,
            toleranceLogicalPx
          )
        )
      )
