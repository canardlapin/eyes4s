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

package eyes4s.studio.viz.plot

import eyes4s.studio.app.Intent
import eyes4s.studio.app.plot.{RovingKey, RovingMove, ViewSelection}
import eyes4s.studio.app.tokens.{Colour, StageToken, StageVariant, Theme, ThemedToken, Tokens}
import eyes4s.studio.core.selection.{
  ContextRevision,
  InputCause,
  SelectionMode,
  SelectionState,
  StudioRef,
  ViewId
}
import intaglio.{
  CasingWidth,
  Clip,
  DevicePoint,
  GraphicParams,
  GraphicsError,
  Grob,
  Interval,
  Point,
  Rgba,
  Scene,
  Size,
  StrokeCasing,
  StrokeWidth,
  Viewport,
  YDirection
}

/** A mark as laid out on one surface, as a roving cursor sees it: its ref,
  * where Intaglio drew its centre (device pixels), how far its painted
  * outline reaches from that centre (logical pixels) and its position in the
  * view's order (from 0).
  */
trait RovingTarget[+R <: StudioRef]:
  def ref: R
  def anchor: DevicePoint
  def reachPx: Double
  def order: Int

/** The marks of one drawn scene that a roving cursor and a pointer can reach
  * (tickets S4.2 and S4.5a). A trial's fixations and a plot's marks are both
  * this; [[MarkInputState]] handles input against either.
  */
trait RovingTargets[R <: StudioRef, +E]:

  /** The device scale the marks were laid out at. */
  def deviceScale: Double

  /** The target of `ref`, if this scene draws it. */
  def target(ref: StudioRef): Option[RovingTarget[R]]

  /** The mark under `point` (device pixels), within `toleranceDevicePx`. */
  def pick(point: DevicePoint, toleranceDevicePx: Double): Either[E, Option[RovingTarget[R]]]

  /** Where the roving cursor goes from `from` on `move`, or `None` if it stays. */
  def step(from: Option[StudioRef], move: RovingMove): Option[RovingTarget[R]]

/** The roving cursor's geometry (DESIGN_SPEC section 10), shared by every
  * view with marks. Targets are in order: a target's `order` is its index.
  */
object RovingCursor:

  /** Device positions this close (in device pixels) are one position. */
  val Epsilon: Double = 1e-6

  /** Where the cursor goes from `current` on `move`, or `None` if it stays.
    *
    * With no current mark, every move starts at the first mark (`Last` at the
    * last). A directional move goes to the nearest mark strictly on that
    * side, ties broken by order. Marks drawn at one position are a stack:
    * Right and Down step forward through it, Left and Up back, before leaving
    * it, so no stacked mark traps or hides from the cursor. `Next` and
    * `Previous` visit every mark in order.
    */
  def step[T <: RovingTarget[?]](
      targets: Vector[T],
      current: Option[T],
      move: RovingMove
  ): Option[T] =
    current match
      case None =>
        move match
          case RovingMove.Last => targets.lastOption
          case _               => targets.headOption
      case Some(at) =>
        val i = at.order
        move match
          case RovingMove.First    => targets.headOption.filter(_.order != i)
          case RovingMove.Last     => targets.lastOption.filter(_.order != i)
          case RovingMove.Next     => targets.lift(i + 1)
          case RovingMove.Previous => if i > 0 then targets.lift(i - 1) else None
          case direction           => directional(targets, at, direction)

  /** The last-drawn target whose painted reach covers `point`: a pick
    * fallback that makes the inside of a hollow mark pickable, since
    * Intaglio picks an unfilled outline on its annulus only. It can go once
    * Intaglio has an interior pick policy (intaglio bd-01M3FQQHVHHCEC349JV46VM4FB).
    */
  def inside[T <: RovingTarget[?]](
      targets: Vector[T],
      point: DevicePoint,
      deviceScale: Double
  ): Option[T] =
    targets.reverseIterator.find { t =>
      math.hypot(t.anchor.x - point.x, t.anchor.y - point.y) <= t.reachPx * deviceScale
    }

  private def directional[T <: RovingTarget[?]](
      targets: Vector[T],
      current: T,
      move: RovingMove
  ): Option[T] =
    val a                = current.anchor
    val i                = current.order
    val forward          = move == RovingMove.Right || move == RovingMove.Down
    def coincident(t: T) =
      math.abs(t.anchor.x - a.x) <= Epsilon && math.abs(t.anchor.y - a.y) <= Epsilon
    val stacked = targets.filter(t => t.order != i && coincident(t))
    val inStack =
      if forward then stacked.find(_.order > i)
      else stacked.reverseIterator.find(_.order < i)
    inStack.orElse {
      def onSide(t: T): Boolean =
        val dx = t.anchor.x - a.x
        val dy = t.anchor.y - a.y
        move match
          case RovingMove.Right => dx > Epsilon
          case RovingMove.Left  => dx < -Epsilon
          case RovingMove.Down  => dy > Epsilon
          case _                => dy < -Epsilon
      targets.iterator
        .filter(onSide)
        .minByOption(t => (math.hypot(t.anchor.x - a.x, t.anchor.y - a.y), t.order))
    }

/** One input to a view with marks, as any UI toolkit reports it. Positions
  * are in device pixels of the drawn surface.
  */
enum MarkInputEvent derives CanEqual:
  case PointerMoved(at: DevicePoint)
  case PointerExited

  /** A primary click; `toggle` when a modifier asks to add or remove. */
  case PointerClicked(at: DevicePoint, toggle: Boolean)
  case Key(key: RovingKey)

  /** The view gained or lost keyboard focus. */
  case FocusChanged(focused: Boolean)

/** What an input did: the next state, the intents to dispatch, and whether
  * the overlay must be redrawn. The scene is never recompiled for input.
  */
final case class MarkInputStep[R <: StudioRef](
    state: MarkInputState[R],
    intents: Vector[Intent],
    redraw: Boolean
)

/** The feedback rings drawn over marks, in drawing order. */
enum RingKind derives CanEqual:
  case Hover, Selected, Focus

/** One feedback ring around a mark: its centre and radius in device pixels. */
final case class OverlayRing(
    kind: RingKind,
    ref: StudioRef,
    centre: DevicePoint,
    radius: Double
) derives CanEqual

/** The token colours of the feedback rings (DESIGN_SPEC sections 4 and 14):
  * the 2 px accent focus ring cased by the ground it is drawn on, the
  * selection ring's inner and outer bands, and the hover ring.
  */
final case class OverlayPalette(
    focus: Colour,
    focusCase: Colour,
    selectedInner: Colour,
    selectedOuter: Colour,
    hover: Colour
) derives CanEqual

object OverlayPalette:

  /** Rings over a trial on its stage: cased by the stage halo, hover in the
    * on-stage colour.
    */
  def of(theme: Theme, stage: StageVariant): OverlayPalette =
    OverlayPalette(
      focus = Tokens.themed(theme, ThemedToken.Accent),
      focusCase = Tokens.staged(stage, StageToken.Halo),
      selectedInner = Tokens.themed(theme, ThemedToken.SelRingInner),
      selectedOuter = Tokens.themed(theme, ThemedToken.Ink),
      hover = Tokens.staged(stage, StageToken.OnStage)
    )

  /** Rings over a plot on the pane surface: cased by the surface, hover in
    * the secondary ink.
    */
  def onSurface(theme: Theme): OverlayPalette =
    OverlayPalette(
      focus = Tokens.themed(theme, ThemedToken.Accent),
      focusCase = Tokens.themed(theme, ThemedToken.Surface),
      selectedInner = Tokens.themed(theme, ThemedToken.SelRingInner),
      selectedOuter = Tokens.themed(theme, ThemedToken.Ink),
      hover = Tokens.themed(theme, ThemedToken.Ink2)
    )

/** Ring geometry, in logical pixels. */
object OverlayRings:

  /** The gap between a mark's painted reach and its feedback ring. */
  val GapPx: Double = 2.0

  /** The focus ring and the halo casing around it (1 px either side). */
  val FocusPx: Double       = 2.0
  val FocusCasingPx: Double = 4.0

  /** Each band of the selection ring; the outer band lies outside the inner. */
  val SelectedBandPx: Double = 2.0

  /** The hover ring. */
  val HoverPx: Double = 1.5

  /** Straight segments of each drawn ring. */
  val Segments: Int = 64

  /** The rings as an Intaglio scene on a `width` × `height` device raster at
    * `deviceScale`, for the host to draw on its overlay canvas: the colours
    * are tokens, and the focus ring's halo is Intaglio `StrokeCasing` paint.
    * Each band is a ring centred where [[OverlayRing.radius]] and the band
    * widths put it.
    */
  def scene(
      rings: Vector[OverlayRing],
      palette: OverlayPalette,
      width: Double,
      height: Double,
      deviceScale: Double
  ): Either[GraphicsError, Scene] =
    val k                         = deviceScale
    def ink(c: Colour): Rgba      = IntaglioColours.toIntaglio(c)
    def gp(c: Colour, px: Double) =
      GraphicParams.checked(stroke = Some(ink(c)), fill = None, lineWidth = px * k)
    def circle(r: OverlayRing, at: Double, style: GraphicParams) =
      val points = (0 until Segments).foldLeft[Either[GraphicsError, Vector[Point]]](
        Right(Vector.empty)
      ) { (acc, i) =>
        val a = 2.0 * math.Pi * i / Segments
        for
          ps <- acc
          p  <- Point.native(r.centre.x + at * math.cos(a), r.centre.y + at * math.sin(a))
        yield ps :+ p
      }
      points.flatMap(Grob.polygon(_, gp = style))
    def bands(r: OverlayRing): Either[GraphicsError, Vector[Grob]] = r.kind match
      case RingKind.Hover =>
        gp(palette.hover, HoverPx)
          .flatMap(circle(r, r.radius + HoverPx / 2.0 * k, _))
          .map(
            Vector(_)
          )
      case RingKind.Selected =>
        for
          inner <- gp(palette.selectedInner, SelectedBandPx)
          outer <- gp(palette.selectedOuter, SelectedBandPx)
          a     <- circle(r, r.radius + SelectedBandPx / 2.0 * k, inner)
          b     <- circle(r, r.radius + 1.5 * SelectedBandPx * k, outer)
        yield Vector(a, b)
      case RingKind.Focus =>
        for
          base   <- gp(palette.focus, FocusPx)
          width  <- StrokeWidth.devicePixels(FocusCasingPx * k)
          casing <- StrokeCasing.checked(ink(palette.focusCase), CasingWidth.Absolute(width))
          ring   <- circle(r, r.radius + FocusCasingPx / 2.0 * k, base.withCasing(casing))
        yield Vector(ring)
    for
      xs    <- Interval(0.0, width)
      ys    <- Interval(0.0, height)
      at    <- Point.npc(0.0, 0.0)
      whole <- Size.npc(1.0, 1.0)
      vp    <- Viewport.checked(at, whole, xs, ys, Clip.Off, yDirection = YDirection.Down)
      grobs <- rings.foldLeft[Either[GraphicsError, Vector[Grob]]](Right(Vector.empty)) {
        (acc, r) => acc.flatMap(gs => bands(r).map(gs ++ _))
      }
    yield Scene(Vector(Grob.group(grobs, viewport = Some(vp))))

/** The input state of a view with marks (tickets S4.2 and S4.5a): the roving
  * cursor, the local hover, the selection as last projected from the bus,
  * and whether the view has keyboard focus.
  *
  * The state is UI-neutral and pure: a JavaFX (or web) adapter translates
  * toolkit events into [[MarkInputEvent]]s, dispatches the returned intents
  * and redraws the overlay from [[overlay]]. Selection flows one way
  * ([[ViewSelection]]): a pick, Enter or Escape dispatches an
  * [[Intent.Select]]; the overlay shows only what the bus [[project]]s back.
  * Hover is the view's own: it is never a selection input
  * ([[Intent.HoverOver]] reports it to the status bar).
  */
final case class MarkInputState[R <: StudioRef] private (
    selection: ViewSelection,
    focus: Option[R],
    hover: Option[R],
    focused: Boolean
) derives CanEqual:

  def view: ViewId                = selection.view
  def selected: Vector[StudioRef] = selection.selected
  def context: ContextRevision    = selection.context
  def sequence: Long              = selection.sequence

  /** Applies one input against the targets on screen. */
  def handle[E](
      event: MarkInputEvent,
      targets: RovingTargets[R, E],
      toleranceDevicePx: Double
  ): Either[E, MarkInputStep[R]] =
    event match
      case MarkInputEvent.PointerMoved(at) =>
        targets.pick(at, toleranceDevicePx).map(hit => hovering(hit.map(_.ref)))
      case MarkInputEvent.PointerExited              => Right(hovering(None))
      case MarkInputEvent.PointerClicked(at, toggle) =>
        targets.pick(at, toleranceDevicePx).map {
          case Some(hit) =>
            copy(focus = Some(hit.ref)).choose(hit.ref, toggle, InputCause.Pointer)
          case None => unchanged
        }
      case MarkInputEvent.Key(RovingKey.Move(move)) =>
        Right(
          targets
            .step(focus, move)
            .fold(unchanged)(t => MarkInputStep(copy(focus = Some(t.ref)), Vector.empty, true))
        )
      case MarkInputEvent.Key(RovingKey.Activate(toggle)) =>
        Right(focus.filter(f => targets.target(f).isDefined).fold(unchanged) { ref =>
          choose(ref, toggle, InputCause.Keyboard)
        })
      case MarkInputEvent.Key(RovingKey.Clear) =>
        Right(submit(SelectionMode.Clear, Vector.empty, InputCause.Keyboard, redraw = false))
      case MarkInputEvent.FocusChanged(now) => Right(focusChanged(now))

  /** The view gained or lost keyboard focus; the focus ring shows only while
    * it has it. Needs no targets.
    */
  def focusChanged(now: Boolean): MarkInputStep[R] =
    MarkInputStep(copy(focused = now), Vector.empty, now != focused)

  /** Puts the cursor on `ref` without selecting, as when a table's cursor row
    * is carried to its plot. A ref the targets lack leaves it where it is.
    */
  def moveFocus(ref: Option[StudioRef], targets: RovingTargets[R, ?]): MarkInputStep[R] =
    ref.flatMap(targets.target) match
      case Some(t) if !focus.contains(t.ref) =>
        MarkInputStep(copy(focus = Some(t.ref)), Vector.empty, true)
      case _ => unchanged

  /** The selection as the bus now holds it: projected input, which redraws
    * the overlay and dispatches nothing.
    */
  def project(selection: SelectionState): MarkInputStep[R] =
    val (next, changed) = this.selection.project(selection)
    MarkInputStep(copy(selection = next), Vector.empty, changed)

  /** The same state on new targets (another layout or another scene): a focus
    * or hover on a mark the targets lack is dropped.
    */
  def retarget(targets: RovingTargets[R, ?]): MarkInputStep[R] =
    val keptHover = hover.filter(targets.target(_).isDefined)
    val next      = copy(focus = focus.filter(targets.target(_).isDefined), hover = keptHover)
    val intents   =
      if keptHover == hover then Vector.empty else Vector(Intent.HoverOver(view, None))
    MarkInputStep(next, intents, true)

  /** The feedback rings to draw, in order: the selection, the hover ring,
    * then the focus ring last, and only while the view has keyboard focus. A
    * ring's radius is where its innermost band starts; a hover or focus ring
    * on a selected mark starts outside the selection ring, so both stay
    * visible.
    */
  def overlay(targets: RovingTargets[R, ?]): Vector[OverlayRing] =
    selectionRings(targets) ++ pointerRings(targets)

  /** The selection layer: it changes only when the bus projects a new
    * selection, so a host can keep it drawn across pointer moves.
    */
  def selectionRings(targets: RovingTargets[R, ?]): Vector[OverlayRing] =
    selected.flatMap(ring(targets, RingKind.Selected, _))

  /** The pointer and cursor layer: hover, then focus. */
  def pointerRings(targets: RovingTargets[R, ?]): Vector[OverlayRing] =
    val hovered   = hover.flatMap(ring(targets, RingKind.Hover, _)).toVector
    val focusRing =
      if focused then focus.flatMap(ring(targets, RingKind.Focus, _)).toVector else Vector.empty
    hovered ++ focusRing

  private def ring(
      targets: RovingTargets[R, ?],
      kind: RingKind,
      ref: StudioRef
  ): Option[OverlayRing] =
    val outside =
      if kind != RingKind.Selected && selected.contains(ref) then
        2.0 * OverlayRings.SelectedBandPx
      else 0.0
    targets
      .target(ref)
      .map(t =>
        OverlayRing(
          kind,
          t.ref,
          t.anchor,
          (t.reachPx + OverlayRings.GapPx + outside) * targets.deviceScale
        )
      )

  /** The view's accessible text: `mark` of the focused mark (and whether it
    * is selected) when the targets draw it, else `idle`, how to use the view.
    */
  def spoken(
      targets: RovingTargets[R, ?],
      mark: (R, Boolean) => String,
      idle: => String
  ): String =
    focus.filter(targets.target(_).isDefined) match
      case Some(ref) => mark(ref, selected.contains(ref))
      case None      => idle

  private def unchanged: MarkInputStep[R] = MarkInputStep(this, Vector.empty, false)

  private def hovering(next: Option[R]): MarkInputStep[R] =
    if next == hover then unchanged
    else MarkInputStep(copy(hover = next), Vector(Intent.HoverOver(view, next)), true)

  private def choose(ref: R, toggle: Boolean, cause: InputCause): MarkInputStep[R] =
    submit(
      if toggle then SelectionMode.Toggle else SelectionMode.Replace,
      Vector(ref),
      cause,
      true
    )

  // A stamped selection input; the overlay changes only when the bus projects
  // the result back.
  private def submit(
      mode: SelectionMode,
      refs: Vector[StudioRef],
      cause: InputCause,
      redraw: Boolean
  ): MarkInputStep[R] =
    val (next, intent) = selection.submit(mode, refs, cause)
    MarkInputStep(copy(selection = next), Vector(intent), redraw)

object MarkInputState:

  /** A view with nothing focused or hovered, showing `selection`. */
  def initial[R <: StudioRef](view: ViewId, selection: SelectionState): MarkInputState[R] =
    MarkInputState[R](ViewSelection.initial(view, selection), None, None, false)
