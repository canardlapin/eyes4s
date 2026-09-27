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

package eyes4s.studio.desktop.plot

import eyes4s.studio.app.Intent
import eyes4s.studio.app.plot.{RovingKey, RovingMove}
import eyes4s.studio.core.selection.{SelectionState, StudioRef}
import eyes4s.studio.viz.plot.{
  CanvasPoint,
  MarkInputEvent,
  MarkInputState,
  MarkInputStep,
  OverlayPalette,
  OverlayRing,
  OverlayRings,
  RovingTargets
}
import intaglio.javafx.{JavaFxCanvasContext, JavaFxRenderer}
import intaglio.{DevicePoint, IntaglioError, RenderPlan}
import javafx.application.Platform
import javafx.beans.value.ChangeListener
import javafx.event.EventHandler
import javafx.scene.{AccessibleAttribute, AccessibleRole}
import javafx.scene.canvas.GraphicsContext
import javafx.scene.input.{KeyCode, KeyEvent, MouseButton, MouseEvent}

/** The keys of a roving cursor, in a plot or a table (DESIGN_SPEC section 10):
  * arrows move, Home and End go to the first and last, Page Up and Page Down
  * step in order, Enter or Space selects (with Shift or the shortcut key,
  * adds or removes), Escape clears. Tab and every other key leave the view.
  */
object RovingKeys:
  def of(e: KeyEvent): Option[RovingKey] =
    val toggle = e.isShiftDown || e.isShortcutDown
    e.getCode match
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
      case _                                => None

/** What a [[MarkInputAdapter]] needs from the view whose marks it serves: the
  * targets of a drawn frame, the ring colours, the accessible text and role.
  */
trait MarkLayer[R <: StudioRef, E, T <: RovingTargets[R, E]]:

  /** The targets of `frame`, or `None` if the view is not showing the scene
    * the frame was compiled from.
    */
  def resolve(frame: PlotFrame): Option[Either[E, T]]

  /** The ring colours, if the view is showing something. */
  def palette: Option[OverlayPalette]

  /** The view's accessible text under `state`, if it is showing something. */
  def spoken(state: MarkInputState[R], targets: T): Option[String]

  /** The accessible role description of the view's focus stop ("plot"). */
  def roleDescription: String

/** Pointer and keyboard input for marks on a [[CanvasPlotHost]] (tickets S4.2
  * and S4.5a): a trial's fixations or a plot's marks.
  *
  * The adapter makes the host one focus stop and listens on the host
  * `Region`, never on its canvases. It translates JavaFX events into
  * [[MarkInputEvent]]s for the pure [[MarkInputState]] and dispatches the
  * intents that returns: a pick, Enter or Escape is an [[Intent.Select]]; a
  * hover change is the view's own [[Intent.HoverOver]]. A pointer position
  * maps to device pixels through the drawn frame's transform, and pointer
  * hits come from that frame's named picking plan.
  *
  * Feedback is an overlay on the host ([[PlotOverlay]]): hover, the selection
  * as [[project]]ed from the bus, and a 2 px accent focus ring (DESIGN_SPEC
  * sections 10 and 14). Redrawing it never compiles or redraws the scene.
  *
  * FX thread only. [[dispose]] removes every handler and listener it added
  * and restores what it changed on the host.
  */
final class MarkInputAdapter[R <: StudioRef, E, T <: RovingTargets[R, E]] private[desktop] (
    host: CanvasPlotHost,
    layer: MarkLayer[R, E, T],
    initial: MarkInputState[R],
    dispatch: Intent => Unit,
    toleranceLogicalPx: Double
) extends PlotOverlay:

  private var current: MarkInputState[R]                       = initial
  private var cached: Option[(PlotFrame, T)]                   = None
  private var failure: Option[E]                               = None
  private var disposed: Boolean                                = false
  private var overlayFailure: Option[IntaglioError]            = None
  private val mouseHandler: EventHandler[MouseEvent]           = e => onMouse(e)
  private val keyHandler: EventHandler[KeyEvent]               = e => onKey(e)
  private val focusListener: ChangeListener[java.lang.Boolean] =
    (_, _, now) => focusChanged(now.booleanValue)

  // What attach changed on the host, restored by dispose.
  private val priorRole                                     = host.getAccessibleRole
  private val priorRoleDescription                          = host.getAccessibleRoleDescription
  private val priorText                                     = host.getAccessibleText
  private val priorTraversable                              = host.isFocusTraversable
  private var spoken: Option[String]                        = None
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
  host.setAccessibleRoleDescription(layer.roleDescription)
  host.setOverlay(Some(this))
  targets.foreach(t => commit(current.retarget(t)))
  describe()

  /** The input state: cursor, hover and the projected selection. */
  def state: MarkInputState[R] = current

  /** The targets of the frame on the canvas, if the view's scene is drawn. */
  def targets: Option[T] =
    host.frame.flatMap { frame =>
      cached match
        case Some((f, t)) if f eq frame => Some(t)
        case _                          =>
          layer.resolve(frame).flatMap {
            case Right(t) =>
              cached = Some((frame, t))
              Some(t)
            case Left(e) =>
              failure = Some(e)
              None
          }
    }

  /** The last input the targets refused (an Intaglio picking failure). */
  def lastError: Option[E] = failure

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
        host.repaintOverlay(under = true)
        describe()

  /** Puts the cursor on `ref` without selecting it, if the scene draws it. */
  def moveFocus(ref: Option[StudioRef]): Unit =
    onFxThread("moveFocus")
    targets.foreach(t => commit(current.moveFocus(ref, t)))

  /** The view was redrawn from another scene or source: drops a focus or
    * hover the new targets lack, and redraws the overlay.
    */
  def refresh(): Unit =
    if !disposed then
      targets match
        case Some(t) => commit(current.retarget(t))
        case None    =>
          host.repaintOverlay(under = true)
          describe()

  /** The view gained or lost keyboard focus (the host's `focused` property,
    * which is true only while its window has focus): the focus ring shows
    * only while it has it.
    */
  private[desktop] def focusChanged(now: Boolean): Unit =
    if !disposed then commit(current.focusChanged(now))

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
      host.setFocusTraversable(priorTraversable)
      host.setAccessibleRole(priorRole)
      host.setAccessibleRoleDescription(priorRoleDescription)
      host.setAccessibleText(priorText)
      cached = None

  // --- PlotOverlay ---------------------------------------------------------------

  /** Hover and focus: redrawn on every pointer move and key. */
  def paint(gc: GraphicsContext, frame: PlotFrame): Unit =
    draw(gc, frame, current.pointerRings)

  /** The selection: redrawn only when the bus projects a new one, or with the
    * scene.
    */
  override def paintUnder(gc: GraphicsContext, frame: PlotFrame): Unit =
    draw(gc, frame, current.selectionRings)

  private def draw(
      gc: GraphicsContext,
      frame: PlotFrame,
      ringsOf: T => Vector[OverlayRing]
  ): Unit =
    for
      t       <- targets
      palette <- layer.palette
    do
      val rings = ringsOf(t)
      if rings.nonEmpty then
        val drawn = for
          scene <- OverlayRings.scene(
            rings,
            palette,
            frame.surface.deviceWidth.toDouble,
            frame.surface.deviceHeight.toDouble,
            frame.surface.deviceScale
          )
          program <- JavaFxRenderer.compile(RenderPlan(scene, frame.plan.context))
        yield JavaFxRenderer.draw(program, JavaFxCanvasContext(gc))
        drawn.left.foreach(e => overlayFailure = Some(e))

  // --- Events ----------------------------------------------------------------------

  private def onMouse(e: MouseEvent): Unit =
    val kind = e.getEventType
    if kind == MouseEvent.MOUSE_MOVED || kind == MouseEvent.MOUSE_DRAGGED then
      atPointer(e).foreach(p => handle(MarkInputEvent.PointerMoved(p)))
    else if kind == MouseEvent.MOUSE_EXITED then handle(MarkInputEvent.PointerExited)
    else if kind == MouseEvent.MOUSE_PRESSED && e.getButton == MouseButton.PRIMARY then
      host.requestFocus()
    else if kind == MouseEvent.MOUSE_CLICKED && e.getButton == MouseButton.PRIMARY then
      val toggle = e.isShiftDown || e.isShortcutDown
      atPointer(e).foreach(p => handle(MarkInputEvent.PointerClicked(p, toggle)))

  // Host-local logical coordinates to the drawn frame's device pixels.
  private def atPointer(e: MouseEvent): Option[DevicePoint] =
    host.frame.map(_.transform.canvasToDevice(CanvasPoint(e.getX, e.getY)))

  private def onKey(e: KeyEvent): Unit =
    RovingKeys.of(e).foreach { k =>
      handle(MarkInputEvent.Key(k))
      e.consume()
    }

  private def handle(event: MarkInputEvent): Unit =
    for
      t     <- targets
      frame <- host.frame
    do
      current.handle(event, t, toleranceLogicalPx * frame.surface.deviceScale) match
        case Right(step) => commit(step)
        case Left(e)     => failure = Some(e)

  // The state is updated before dispatching: the bus may project its result
  // back synchronously, which must see (and not be overwritten by) this step.
  private def commit(step: MarkInputStep[R]): Unit =
    if !disposed then
      current = step.state
      step.intents.foreach(dispatch)
      if step.redraw then host.repaintOverlay()
      describe()

  private def describe(): Unit =
    if !disposed then
      val text = targets.flatMap(layer.spoken(current, _))
      if text != spoken then
        spoken = text
        host.setAccessibleText(text.orNull)
        // Screen readers re-read the text only when told it changed.
        host.notifyAccessibleAttributeChanged(AccessibleAttribute.TEXT)

  private def onFxThread(operation: String): Unit =
    if !Platform.isFxApplicationThread then
      throw IllegalStateException(
        s"MarkInputAdapter.$operation must run on the JavaFX application thread, " +
          s"not ${Thread.currentThread.getName}"
      )
