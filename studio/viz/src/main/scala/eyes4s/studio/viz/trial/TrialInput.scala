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

package eyes4s.studio.viz.trial

import eyes4s.studio.app.Intent
import eyes4s.studio.app.text.{TrialText, TrialTextId}
import eyes4s.studio.app.tokens.{Colour, StageToken, StageVariant, Theme, ThemedToken, Tokens}
import eyes4s.studio.core.backend.TrialKey
import eyes4s.studio.core.selection.{
  ContextRevision,
  InputCause,
  InputStamp,
  SelectionInput,
  SelectionMode,
  SelectionState,
  StudioRef,
  ViewId
}
import eyes4s.studio.viz.plot.IntaglioColours
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

/** A key of a trial view's roving cursor (DESIGN_SPEC section 10). */
enum RovingKey derives CanEqual:
  /** An arrow, Home/End or Page Up/Down. */
  case Move(move: RovingMove)

  /** Enter or Space: select the focused mark; `toggle` adds or removes it. */
  case Activate(toggle: Boolean)

  /** Escape: clear the selection. */
  case Clear

/** One input to a trial view, as any UI toolkit reports it. Positions are in
  * device pixels of the drawn surface.
  */
enum TrialInputEvent derives CanEqual:
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
final case class TrialInputStep(
    state: TrialInputState,
    intents: Vector[Intent],
    redraw: Boolean
)

/** The feedback rings drawn over a trial, in drawing order. */
enum RingKind derives CanEqual:
  case Hover, Selected, Focus

/** One feedback ring around a mark: its centre and radius in device pixels. */
final case class OverlayRing(
    kind: RingKind,
    ref: StudioRef.Fixation,
    centre: DevicePoint,
    radius: Double
) derives CanEqual

/** The token colours of the feedback rings (DESIGN_SPEC sections 4 and 14):
  * the 2 px accent focus ring cased by the stage halo, the selection ring's
  * inner and outer bands, and the hover ring in the on-stage colour.
  */
final case class OverlayPalette(
    focus: Colour,
    focusCase: Colour,
    selectedInner: Colour,
    selectedOuter: Colour,
    hover: Colour
) derives CanEqual

object OverlayPalette:
  def of(theme: Theme, stage: StageVariant): OverlayPalette =
    OverlayPalette(
      focus = Tokens.themed(theme, ThemedToken.Accent),
      focusCase = Tokens.staged(stage, StageToken.Halo),
      selectedInner = Tokens.themed(theme, ThemedToken.SelRingInner),
      selectedOuter = Tokens.themed(theme, ThemedToken.Ink),
      hover = Tokens.staged(stage, StageToken.OnStage)
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

/** A trial view's input state (ticket S4.2): the roving cursor, the local
  * hover, the selection as last projected from the selection bus, and the
  * stamp of the next selection input.
  *
  * The state is UI-neutral and pure: a JavaFX (or web) adapter translates
  * toolkit events into [[TrialInputEvent]]s, dispatches the returned intents
  * and redraws the overlay from [[overlay]]. Selection flows one way: a pick,
  * Enter or Escape dispatches an [[Intent.Select]]; the overlay shows only
  * what the bus [[project]]s back, which emits nothing, so a view never echoes
  * its own or another view's selection. Hover is the view's own: it is never
  * a selection input ([[Intent.HoverOver]] reports it to the status bar).
  */
final case class TrialInputState private (
    view: ViewId,
    focus: Option[StudioRef.Fixation],
    hover: Option[StudioRef.Fixation],
    selected: Vector[StudioRef],
    context: ContextRevision,
    sequence: Long,
    focused: Boolean
) derives CanEqual:

  /** Applies one input against the targets on screen. */
  def handle(
      event: TrialInputEvent,
      targets: TrialTargets,
      toleranceDevicePx: Double
  ): Either[TrialTargetError, TrialInputStep] =
    event match
      case TrialInputEvent.PointerMoved(at) =>
        targets.pick(at, toleranceDevicePx).map(hit => hovering(hit.map(_.ref)))
      case TrialInputEvent.PointerExited              => Right(hovering(None))
      case TrialInputEvent.PointerClicked(at, toggle) =>
        targets.pick(at, toleranceDevicePx).map {
          case Some(hit) =>
            copy(focus = Some(hit.ref)).choose(hit.ref, toggle, InputCause.Pointer)
          case None => unchanged
        }
      case TrialInputEvent.Key(RovingKey.Move(move)) =>
        Right(
          targets
            .step(focus, move)
            .fold(unchanged)(t => TrialInputStep(copy(focus = Some(t.ref)), Vector.empty, true))
        )
      case TrialInputEvent.Key(RovingKey.Activate(toggle)) =>
        Right(focus.filter(f => targets.target(f).isDefined).fold(unchanged) { ref =>
          choose(ref, toggle, InputCause.Keyboard)
        })
      case TrialInputEvent.Key(RovingKey.Clear) =>
        Right(submit(SelectionMode.Clear, Vector.empty, InputCause.Keyboard, redraw = false))
      case TrialInputEvent.FocusChanged(now) => Right(focusChanged(now))

  /** The view gained or lost keyboard focus; the focus ring shows only while
    * it has it. Needs no targets.
    */
  def focusChanged(now: Boolean): TrialInputStep =
    TrialInputStep(copy(focused = now), Vector.empty, now != focused)

  /** The selection as the bus now holds it: projected input, which redraws
    * the overlay and dispatches nothing.
    */
  def project(selection: SelectionState): TrialInputStep =
    val next = copy(
      selected = selection.selected,
      context = selection.context,
      sequence = sequence.max(TrialInputState.nextSequence(view, selection))
    )
    TrialInputStep(next, Vector.empty, selection.selected != selected)

  /** The same state on new targets (another layout or another trial): a focus
    * or hover on a mark the targets lack is dropped.
    */
  def retarget(targets: TrialTargets): TrialInputStep =
    val keptHover = hover.filter(targets.target(_).isDefined)
    val next      = copy(focus = focus.filter(targets.target(_).isDefined), hover = keptHover)
    val intents   =
      if keptHover == hover then Vector.empty else Vector(Intent.HoverOver(view, None))
    TrialInputStep(next, intents, true)

  /** The feedback rings to draw, in order: hover, selection, then the focus
    * ring last, and only while the view has keyboard focus. A ring's radius is
    * where its innermost band starts; a focus ring on a selected mark starts
    * outside the selection ring, so both stay visible.
    */
  def overlay(targets: TrialTargets): Vector[OverlayRing] =
    def ring(kind: RingKind, ref: StudioRef.Fixation) =
      val outside =
        if kind == RingKind.Focus && selected.contains(ref) then
          2.0 * OverlayRings.SelectedBandPx
        else 0.0
      targets
        .target(ref)
        .map(t =>
          OverlayRing(
            kind,
            ref,
            t.anchor,
            (t.mark.reachPx + OverlayRings.GapPx + outside) * targets.deviceScale
          )
        )
    val hovered = hover.flatMap(ring(RingKind.Hover, _)).toVector
    val chosen  = selected
      .collect { case f: StudioRef.Fixation => f }
      .flatMap(
        ring(RingKind.Selected, _)
      )
    val focusRing =
      if focused then focus.flatMap(ring(RingKind.Focus, _)).toVector else Vector.empty
    hovered ++ chosen ++ focusRing

  /** The view's accessible text: the focused mark, from its semantic id, or
    * how to use the view when no mark is focused.
    */
  def accessibleText(trial: TrialKey, targets: TrialTargets): String =
    focus.filter(targets.target(_).isDefined) match
      case Some(ref) => TrialText.mark(ref, selected.contains(ref))
      case None      => TrialText(TrialTextId.PlotKeys, trial.label)

  private def unchanged: TrialInputStep = TrialInputStep(this, Vector.empty, false)

  private def hovering(next: Option[StudioRef.Fixation]): TrialInputStep =
    if next == hover then unchanged
    else TrialInputStep(copy(hover = next), Vector(Intent.HoverOver(view, next)), true)

  private def choose(
      ref: StudioRef.Fixation,
      toggle: Boolean,
      cause: InputCause
  ): TrialInputStep =
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
  ): TrialInputStep =
    val stamp = InputStamp(context, view, sequence, cause)
    TrialInputStep(
      copy(sequence = sequence + 1),
      Vector(Intent.Select(SelectionInput(stamp, mode, refs))),
      redraw
    )

object TrialInputState:

  /** A view with nothing focused or hovered, showing `selection`. */
  def initial(view: ViewId, selection: SelectionState): TrialInputState =
    TrialInputState(
      view,
      None,
      None,
      selection.selected,
      selection.context,
      nextSequence(view, selection),
      false
    )

  // The bus refuses a sequence at or below the view's last applied one, so a
  // view re-attached under the same id continues after it.
  private def nextSequence(view: ViewId, selection: SelectionState): Long =
    selection.delivered.get(view).fold(0L)(_ + 1L)
