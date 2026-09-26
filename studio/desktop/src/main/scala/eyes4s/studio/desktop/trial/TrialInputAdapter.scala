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
import eyes4s.studio.core.selection.{SelectionState, ViewId}
import eyes4s.studio.desktop.plot.{PlotFrame, PlotHostStatus, PlotOverlay}
import eyes4s.studio.viz.plot.CanvasPoint
import eyes4s.studio.viz.trial.{
  OverlayPalette,
  OverlayRings,
  RovingKey,
  RovingMove,
  TrialInputEvent,
  TrialInputState,
  TrialInputStep,
  TrialScene,
  TrialTargetError,
  TrialTargets
}
import intaglio.javafx.{JavaFxCanvasContext, JavaFxRenderer}
import intaglio.{DevicePoint, IntaglioError, RenderPlan}
import javafx.application.Platform
import javafx.beans.value.ChangeListener
import javafx.event.EventHandler
import javafx.scene.AccessibleRole
import javafx.scene.canvas.GraphicsContext
import javafx.scene.input.{KeyCode, KeyEvent, MouseButton, MouseEvent}

/** Why the adapter could not be attached. */
enum TrialInputError derives CanEqual:
  /** A pointer tolerance must be finite and not negative. */
  case InvalidTolerance(toleranceLogicalPx: Double)

  def message: String = this match
    case InvalidTolerance(t) =>
      s"pointer tolerance $t logical px is not finite and non-negative"

/** Pointer and keyboard input for a [[TrialView]] (ticket S4.2).
  *
  * The adapter makes the view's canvas host one focus stop and listens on the
  * host `Region`, never on its canvases. It translates JavaFX events into
  * [[TrialInputEvent]]s for the pure [[TrialInputState]] and dispatches the
  * intents that returns: a pick, Enter or Escape is an [[Intent.Select]]; a
  * hover change is the view's own [[Intent.HoverOver]]. A pointer position
  * maps to device pixels through the drawn frame's transform, and pointer hits
  * come from that frame's named picking plan, in which every fixation mark is
  * its own target.
  *
  * Feedback is an overlay on the host ([[PlotOverlay]]): hover, the selection
  * as [[project]]ed from the bus, and a 2 px accent focus ring cased by the
  * stage halo (DESIGN_SPEC sections 10 and 14). Redrawing it never compiles
  * or redraws the scene. The focused mark's accessible text comes from its
  * [[eyes4s.studio.core.selection.StudioRef]].
  *
  * FX thread only. [[dispose]] removes every handler and listener it added.
  */
final class TrialInputAdapter private (
    view: TrialView,
    initial: TrialInputState,
    dispatch: Intent => Unit,
    toleranceLogicalPx: Double
) extends PlotOverlay:

  private val host                                             = view.plotHost
  private var current: TrialInputState                         = initial
  private var cached: Option[(PlotFrame, TrialTargets)]        = None
  private var failure: Option[TrialTargetError]                = None
  private var disposed: Boolean                                = false
  private var overlayFailure: Option[IntaglioError]            = None
  private val mouseHandler: EventHandler[MouseEvent]           = e => onMouse(e)
  private val keyHandler: EventHandler[KeyEvent]               = e => onKey(e)
  private val focusListener: ChangeListener[java.lang.Boolean] =
    (_, _, now) => commit(current.focusChanged(now.booleanValue))
  private val frameListener: ChangeListener[PlotHostStatus] = (_, _, status) =>
    status match
      case PlotHostStatus.Drawn(_) => targets.foreach(t => commit(current.retarget(t)))
      case _                       => describe()

  host.addEventHandler(MouseEvent.ANY, mouseHandler)
  host.addEventHandler(KeyEvent.KEY_PRESSED, keyHandler)
  host.focusedProperty.addListener(focusListener)
  host.status.addListener(frameListener)
  host.setFocusTraversable(true)
  host.setAccessibleRole(AccessibleRole.PARENT)
  host.setAccessibleRoleDescription(TrialText(TrialTextId.PlotRole))
  host.setOverlay(Some(this))
  targets.foreach(t => commit(current.retarget(t)))
  describe()

  /** The input state: cursor, hover and the projected selection. */
  def state: TrialInputState = current

  /** The targets of the frame on the canvas, if a trial is drawn. */
  def targets: Option[TrialTargets] =
    host.frame.flatMap { frame =>
      cached match
        case Some((f, t)) if f eq frame => Some(t)
        case _                          =>
          view.status.get match
            case TrialViewStatus.Shown(scene) if scene.plot.scene eq frame.plan.scene =>
              resolve(scene, frame)
            case _ => None
    }

  /** The last input the targets refused (an Intaglio picking failure). */
  def lastError: Option[TrialTargetError] = failure

  /** The last overlay Intaglio refused to build or compile, if any. */
  def lastOverlayError: Option[IntaglioError] = overlayFailure

  def isDisposed: Boolean = disposed

  /** The selection as the bus now holds it: redraws the overlay, emits nothing. */
  def project(selection: SelectionState): Unit =
    onFxThread("project")
    if !disposed then
      val step = current.project(selection)
      current = step.state
      if step.redraw then
        host.repaintOverlay()
        describe()

  /** Removes the adapter's handlers, listeners and overlay. Idempotent. */
  def dispose(): Unit =
    onFxThread("dispose")
    if !disposed then
      disposed = true
      host.removeEventHandler(MouseEvent.ANY, mouseHandler)
      host.removeEventHandler(KeyEvent.KEY_PRESSED, keyHandler)
      host.focusedProperty.removeListener(focusListener)
      host.status.removeListener(frameListener)
      host.setOverlay(None)
      host.setFocusTraversable(false)
      host.setAccessibleText(null)
      host.setAccessibleRoleDescription(null)
      cached = None

  // --- PlotOverlay ---------------------------------------------------------------

  def paint(gc: GraphicsContext, frame: PlotFrame): Unit =
    for
      t     <- targets
      input <- view.input
    do
      val rings = current.overlay(t)
      if rings.nonEmpty then
        val drawn = for
          scene <- OverlayRings.scene(
            rings,
            OverlayPalette.of(input.theme, input.stage),
            frame.surface.deviceWidth.toDouble,
            frame.surface.deviceHeight.toDouble,
            frame.surface.deviceScale
          )
          program <- JavaFxRenderer.compile(RenderPlan(scene, frame.plan.context))
        yield JavaFxRenderer.draw(program, JavaFxCanvasContext(gc))
        drawn.left.foreach(e => overlayFailure = Some(e))

  // --- Events ----------------------------------------------------------------------

  private def resolve(scene: TrialScene, frame: PlotFrame): Option[TrialTargets] =
    TrialTargets.resolve(scene, frame.device, frame.picking, frame.surface.deviceScale) match
      case Right(t) =>
        cached = Some((frame, t))
        Some(t)
      case Left(e) =>
        failure = Some(e)
        None

  private def onMouse(e: MouseEvent): Unit =
    val kind = e.getEventType
    if kind == MouseEvent.MOUSE_MOVED || kind == MouseEvent.MOUSE_DRAGGED then
      atPointer(e).foreach(p => handle(TrialInputEvent.PointerMoved(p)))
    else if kind == MouseEvent.MOUSE_EXITED then handle(TrialInputEvent.PointerExited)
    else if kind == MouseEvent.MOUSE_PRESSED && e.getButton == MouseButton.PRIMARY then
      host.requestFocus()
    else if kind == MouseEvent.MOUSE_CLICKED && e.getButton == MouseButton.PRIMARY then
      val toggle = e.isShiftDown || e.isShortcutDown
      atPointer(e).foreach(p => handle(TrialInputEvent.PointerClicked(p, toggle)))

  // Host-local logical coordinates to the drawn frame's device pixels.
  private def atPointer(e: MouseEvent): Option[DevicePoint] =
    host.frame.map(_.transform.canvasToDevice(CanvasPoint(e.getX, e.getY)))

  private def onKey(e: KeyEvent): Unit =
    val toggle = e.isShiftDown || e.isShortcutDown
    val key    = e.getCode match
      case KeyCode.LEFT | KeyCode.KP_LEFT   => Some(RovingKey.Move(RovingMove.Left))
      case KeyCode.RIGHT | KeyCode.KP_RIGHT => Some(RovingKey.Move(RovingMove.Right))
      case KeyCode.UP | KeyCode.KP_UP       => Some(RovingKey.Move(RovingMove.Up))
      case KeyCode.DOWN | KeyCode.KP_DOWN   => Some(RovingKey.Move(RovingMove.Down))
      case KeyCode.HOME                     => Some(RovingKey.Move(RovingMove.First))
      case KeyCode.END                      => Some(RovingKey.Move(RovingMove.Last))
      case KeyCode.PAGE_UP                  => Some(RovingKey.Move(RovingMove.Previous))
      case KeyCode.PAGE_DOWN                => Some(RovingKey.Move(RovingMove.Next))
      case KeyCode.ENTER | KeyCode.SPACE    => Some(RovingKey.Activate(toggle))
      case KeyCode.ESCAPE                   => Some(RovingKey.Clear)
      case _                                => None // Tab and the rest leave the view
    key.foreach { k =>
      handle(TrialInputEvent.Key(k))
      e.consume()
    }

  private def handle(event: TrialInputEvent): Unit =
    for
      t     <- targets
      frame <- host.frame
    do
      current.handle(event, t, toleranceLogicalPx * frame.surface.deviceScale) match
        case Right(step) => commit(step)
        case Left(e)     => failure = Some(e)

  // The state is updated before dispatching: the bus may project its result
  // back synchronously, which must see (and not be overwritten by) this step.
  private def commit(step: TrialInputStep): Unit =
    if !disposed then
      current = step.state
      step.intents.foreach(dispatch)
      if step.redraw then host.repaintOverlay()
      describe()

  private def describe(): Unit =
    if !disposed then
      val text = for
        input <- view.input
        t     <- targets
      yield current.accessibleText(input.display.trial, t)
      host.setAccessibleText(text.orNull)

  private def onFxThread(operation: String): Unit =
    if !Platform.isFxApplicationThread then
      throw IllegalStateException(
        s"TrialInputAdapter.$operation must run on the JavaFX application thread, " +
          s"not ${Thread.currentThread.getName}"
      )

object TrialInputAdapter:

  /** The pointer tolerance: a hit within this many logical pixels of a mark's
    * painted outline picks it.
    */
  val DefaultTolerancePx: Double = 4.0

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
          view,
          TrialInputState.initial(viewId, selection),
          dispatch,
          toleranceLogicalPx
        )
      )
