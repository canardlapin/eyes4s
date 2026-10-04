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

import eyes4s.studio.app.maps.{GridPoint, Isolines, MapGrid, MapId, MapOpacity, MapRaster}
import eyes4s.studio.app.text.{TrialText, TrialTextId}
import eyes4s.studio.app.tokens.{
  FontFace,
  PaletteToken,
  StageToken,
  StageVariant,
  Theme,
  ThemedToken,
  Tokens,
  TypeSize
}
import eyes4s.studio.core.assets.{AssetFile, AssetRef, DisplayKind, DisplayState, TrialDisplay}
import eyes4s.studio.core.backend.TrialKey
import eyes4s.studio.core.document.ScreenSize
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}
import eyes4s.plan.{MapPlacement, OffWindowPolicy}
import eyes4s.studio.viz.plot.{
  DataPanel,
  DataPoint,
  IntaglioColours,
  PlotScene,
  PlotSceneError,
  SceneId
}
import intaglio.{
  Anchor,
  BatchColumn,
  CasingWidth,
  Clip,
  DashPattern,
  ExtentExpr,
  GraphicParams,
  GraphicsError,
  GraphicsName,
  Grob,
  GrobMeta,
  HJust,
  Interval,
  Length,
  LengthExpr,
  LineType,
  PatternPaint,
  PatternRecipe,
  Point,
  PointShape,
  RasterDimensions,
  RasterImage,
  RasterInterpolation,
  Rgba32,
  Rgba,
  Scene,
  Size,
  StrokeCasing,
  StrokeUnit,
  StrokeWidth,
  VJust,
  Viewport,
  YDirection
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/** Why a trial scene, or one of its inputs, was refused. Every case names the
  * trial and the value it refused.
  */
enum TrialSceneError derives CanEqual:

  /** A fixation position that is not a finite screen position. */
  case NonFinitePosition(trial: TrialKey, fixation: Int, x: Double, y: Double)

  /** A fixation duration that is not a positive number of milliseconds. */
  case DurationNotPositive(trial: TrialKey, fixation: Int, durationMs: Int)

  /** Two fixations of one trial with the same index. */
  case DuplicateFixation(trial: TrialKey, fixation: Int)

  /** A covering extent with no area, or not finite. */
  case EmptyExtent(left: Double, top: Double, right: Double, bottom: Double)

  /** The scene's panel or identity was refused. */
  case Plot(trial: TrialKey, error: PlotSceneError)

  /** Intaglio refused a value while building `part` of the scene. */
  case Graphics(trial: TrialKey, part: String, error: GraphicsError)

  /** The map layer's raster is of `raster`, not of its grid's map `grid`. */
  case MapMismatch(trial: TrialKey, raster: MapId, grid: MapId)

  /** The map layer is of `map`, another trial's. */
  case MapOfAnotherTrial(trial: TrialKey, map: MapId)

  def message: String = this match
    case NonFinitePosition(t, i, x, y) =>
      s"Trial ${t.label}: fixation $i is at ($x, $y), which is not a finite screen position."
    case DurationNotPositive(t, i, d) =>
      s"Trial ${t.label}: fixation $i lasts $d ms; a duration must be positive."
    case DuplicateFixation(t, i) => s"Trial ${t.label}: fixation $i is listed more than once."
    case EmptyExtent(l, t, r, b) =>
      s"The trial extent [$l, $r] × [$t, $b] (screen px) has no area or is not finite."
    case Plot(t, e)           => s"Trial ${t.label}: ${e.message}"
    case Graphics(t, part, e) => s"Trial ${t.label}: Intaglio refused the $part: ${e.message}"
    case MapMismatch(t, raster, grid) =>
      s"Trial ${t.label}: the map layer's raster is of ${raster.label}, its grid of ${grid.label}."
    case MapOfAnotherTrial(t, map) =>
      s"Trial ${t.label}: the map layer is ${map.label}, another trial's."

// ---------------------------------------------------------------------------
// Inputs
// ---------------------------------------------------------------------------

/** One fixation as the trial view draws it: its index in the trial, its
  * position in screen pixels (origin top-left, y down), its duration and
  * its already-decided core map placement. The trial view never derives this
  * classification from screen coordinates: `DroppedInitial` takes precedence
  * over `OutsideScreen`, and an outside-window fixation retains the plan's
  * Exclude or FailTrial policy.
  */
final case class TrialFixation private (
    index: FixationIndex,
    screenX: Double,
    screenY: Double,
    durationMs: Int,
    placement: MapPlacement
) derives CanEqual:
  /** Whether core includes this fixation in the trial map. */
  def contributesToMap: Boolean = placement == MapPlacement.InWindow

object TrialFixation:
  def of(
      trial: TrialKey,
      index: FixationIndex,
      screenX: Double,
      screenY: Double,
      durationMs: Int,
      placement: MapPlacement
  ): Either[TrialSceneError, TrialFixation] =
    if !screenX.isFinite || !screenY.isFinite then
      Left(TrialSceneError.NonFinitePosition(trial, index.value, screenX, screenY))
    else if durationMs <= 0 then
      Left(TrialSceneError.DurationNotPositive(trial, index.value, durationMs))
    else Right(new TrialFixation(index, screenX, screenY, durationMs, placement))

/** The role a trial plays in a comparison (DESIGN_SPEC section 5). */
enum TrialRole derives CanEqual:
  /** The query trial: filled circles in `--query`. */
  case Query

  /** The matched reference: filled diamonds in `--match`. */
  case Matched

  /** A control reference: hollow circles outlined in `--control`. */
  case Control

/** How fixation marks are drawn: role-free in Explore, by role in Analysis,
  * Compare and Figures (DESIGN_SPEC section 5).
  */
enum MarkStyle derives CanEqual:
  case Neutral
  case Role(role: TrialRole)

/** The raster of a stored asset, as far as the host has it. */
enum StimulusRaster derives CanEqual:
  /** The decoded image. */
  case Loaded(image: RasterImage)

  /** Not decoded yet. */
  case Pending

  /** The stored bytes could not be read or decoded; `reason` says why. */
  case Unreadable(reason: String)

/** A rectangle of the screen frame in screen pixels (y down), with area. */
final case class ScreenRect private (left: Double, top: Double, right: Double, bottom: Double)
    derives CanEqual:
  def width: Double  = right - left
  def height: Double = bottom - top

  /** The smallest rectangle covering this one and `(x, y)`. */
  def including(x: Double, y: Double): ScreenRect =
    new ScreenRect(left.min(x), top.min(y), right.max(x), bottom.max(y))

  /** This rectangle grown by `margin` on every side. */
  def grown(margin: Double): ScreenRect =
    new ScreenRect(left - margin, top - margin, right + margin, bottom + margin)

object ScreenRect:
  def of(
      left: Double,
      top: Double,
      right: Double,
      bottom: Double
  ): Either[TrialSceneError, ScreenRect] =
    Either.cond(
      List(left, top, right, bottom).forall(_.isFinite) && right > left && bottom > top,
      new ScreenRect(left, top, right, bottom),
      TrialSceneError.EmptyExtent(left, top, right, bottom)
    )

  /** The whole screen. */
  def screen(size: ScreenSize): ScreenRect =
    new ScreenRect(0.0, 0.0, size.width.toDouble, size.height.toDouble)

/** How much of the screen frame the trial view shows. */
enum TrialExtent derives CanEqual:
  /** The whole screen. */
  case Screen

  /** The image frame and every fixation centre, with a margin of stage
    * ([[TrialScene.GazeMarginPx]] screen pixels) around them.
    */
  case Gaze

  /** A fixed region, so that side-by-side trials share one scale. */
  case Covering(region: ScreenRect)

/** What the trial view draws besides the display: the toolbar's Points and
  * Order toggles, the analysis-window outline, and the extent.
  */
final case class TrialSceneOptions(
    points: Boolean = true,
    order: Boolean = true,
    windowOutline: Boolean = true,
    extent: TrialExtent = TrialExtent.Gaze
) derives CanEqual

/** A result map drawn over the trial (ticket S4.3b): the run's grid, its
  * raster from the map raster cache (S4.4), and the one global opacity it
  * is drawn with. The raster covers the image frame cell for cell; the
  * grid's backend-supplied isoline levels are contoured over it, cased.
  */
final case class TrialMap(
    grid: MapGrid,
    raster: MapRaster,
    opacity: MapOpacity = MapOpacity.Default,
    covers: MapCoverage = MapCoverage.ImageFrame
) derives CanEqual

/** Which region of the screen a map's grid covers: the image frame (a run's
  * map over the analysis window that is the image), or a stated region of
  * the screen in screen pixels (a backend preview over its study's window,
  * S6.2), which is drawn there and never stretched over the image.
  */
enum MapCoverage derives CanEqual:
  case ImageFrame
  case Region(region: ScreenRect)

/** The image a retrieval trial's participant was remembering, which the
  * trial did not display: absent when there is none, hidden by default, or
  * shown as an underlay with a disclosure that it was not displayed.
  */
enum RememberedImage derives CanEqual:
  case Absent
  case Hidden(asset: AssetRef)
  case Shown(asset: AssetRef)

/** Everything a trial scene is built from.
  *
  * `rasters` holds the decoded images the host has for stored assets; an
  * asset the display names and `rasters` lacks is still loading. A blank
  * display draws no image whatever `rasters` holds.
  */
final case class TrialSceneInput(
    display: TrialDisplay,
    screen: ScreenSize,
    fixations: Vector[TrialFixation],
    marks: MarkStyle,
    theme: Theme,
    stage: StageVariant,
    rasters: Map[AssetRef, StimulusRaster] = Map.empty,
    options: TrialSceneOptions = TrialSceneOptions(),
    map: Option[TrialMap] = None,
    remembered: RememberedImage = RememberedImage.Absent
)

// ---------------------------------------------------------------------------
// Output
// ---------------------------------------------------------------------------

/** One drawn fixation mark: the fixation it shows, where (in screen pixels,
  * the panel's data coordinates), its radius in logical pixels, how far its
  * painted outline reaches from its centre (`reachPx`: a diamond's half
  * diagonal, halo and casing included), its position in the trial's fixation
  * order (`order`, from 0) and the name of its grob.
  *
  * Every mark is its own named grob ([[TrialScene.markName]]), so a named
  * picking plan resolves a hit to one mark (S4.2; Intaglio's named picking
  * treats a named point batch as a single target).
  */
final case class TrialMark(
    ref: StudioRef.Fixation,
    at: DataPoint,
    radiusPx: Double,
    reachPx: Double,
    placement: MapPlacement,
    order: Int,
    name: GraphicsName
) derives CanEqual

/** What the image frame shows, after the asset registry and the rasters. */
enum FrameArt derives CanEqual:
  case Picture(asset: AssetRef)
  case Screen
  case ScreenWithCross
  case CueText(text: String)
  case Unknown
  case Missing(file: AssetFile)
  case Unreadable(file: AssetFile, reason: String)
  case Loading(file: AssetFile)

/** A trial drawn on its stage (ticket S4.3a): the scene, the region of the
  * screen it shows, the marks with their semantic ids, and what the frame
  * shows.
  *
  * The panel's data coordinates are screen pixels (y down), so the image,
  * the marks and a picking plan compiled from the same scene all resolve
  * through one [[eyes4s.studio.viz.plot.PlotTransform]].
  */
final case class TrialScene private (
    plot: PlotScene,
    extent: ScreenRect,
    frameArt: FrameArt,
    caption: String,
    marks: Vector[TrialMark],
    map: Option[MapId],
    disclosure: Option[String]
):
  private lazy val byName: Map[GraphicsName, TrialMark] = marks.map(m => m.name -> m).toMap

  /** The mark whose grob is named `name`, if it is a fixation mark (S4.2). */
  def markNamed(name: GraphicsName): Option[TrialMark] = byName.get(name)

  /** The fixation the grob named `name` shows, if it is a fixation mark. */
  def refOf(name: GraphicsName): Option[StudioRef.Fixation] = markNamed(name).map(_.ref)

  /** Width over height of the canvas this scene is laid out for; a host
    * keeps it so the image is not distorted ([[TrialScene.fit]]).
    */
  def aspect: Double = TrialScene.aspectOf(extent)

object TrialScene:

  // --- Names of the scene's grobs (the op log and the tests find them) -----
  val StageName: String    = "trial-stage"
  val FrameName: String    = "trial-image-frame"
  val StimulusName: String = "trial-stimulus"
  val WindowName: String   = "trial-window"
  val OrderName: String    = "trial-order"
  val MarksName: String    = "trial-fixations"
  val CaptionName: String  = "trial-caption"
  val UnderlayName: String = "trial-underlay"
  val MapName: String      = "trial-map"
  val IsolinesName: String = "trial-isolines"
  val RememberName: String = "trial-remembered"

  /** A map isoline: its ink, and the halo casing under it, 2 units of ink
    * over 4 of casing (bead S4.3b, bd-01M3DPFM4ZG9GV0RTYWWA1M2P9).
    */
  val IsolinePx: Double       = 2.0
  val IsolineCasingPx: Double = 4.0

  /** The data panel's viewport group: Intaglio resolves its frame by this name. */
  val PanelName: String = "trial-panel"

  /** The prefix of every fixation mark's grob name. */
  val MarkPrefix: String = "trial-fixation-"

  /** The name of the grob that draws fixation `index`: one name per mark. */
  def markName(index: FixationIndex): String = s"$MarkPrefix${index.value}"

  /** Straight segments of the ring that draws a cased (hollow control) mark. */
  val RingSegments: Int = 48

  /** The share of the canvas height below the panel, where the caption goes. */
  val CaptionFraction: Double = 0.12

  /** The stage margin of [[TrialExtent.Gaze]], in screen pixels. */
  val GazeMarginPx: Double = 48.0

  /** Mark radius in logical pixels per square root of a millisecond: mark
    * area is proportional to duration (412 ms is about 8 px).
    */
  val RadiusPerRootMs: Double = 0.4

  /** The halo of a filled mark and the outline of a hollow one (DESIGN_SPEC
    * section 12: 2 px on screen).
    */
  val HaloPx: Double = 2.0

  /** The halo casing of a hollow control mark (Intaglio `StrokeCasing`): 1 px
    * of halo either side of its 2 px outline.
    */
  val CasingPx: Double = 4.0

  /** Order lines and the analysis-window outline. */
  val OrderLinePx: Double  = 1.5
  val WindowLinePx: Double = 1.0

  /** Arms of the fixation cross, in screen pixels from the image centre. */
  val CrossArmPx: Double = 20.0

  // Intaglio physical lengths are points at 96 logical pixels per inch.
  private val PointsPerPixel         = 72.0 / 96.0
  private def pt(px: Double): Double = px * PointsPerPixel

  // Dash rhythms (device pixels; see the report on Intaglio's dash units).
  private val OrderDash            = DashPattern.unsafe(6.0, 5.0)
  private val DroppedInitialDash   = DashPattern.unsafe(1.5, 3.0)
  private val OutsideScreenDash    = DashPattern.unsafe(7.0, 2.0)
  private val OutsideExcludeDash   = DashPattern.unsafe(3.0, 2.5)
  private val OutsideFailTrialDash = DashPattern.unsafe(5.0, 1.5)
  private val WindowDash           = DashPattern.unsafe(8.0, 6.0)

  /** The radius of a mark for a fixation of `durationMs`, in logical pixels. */
  def radiusPx(durationMs: Int): Double = RadiusPerRootMs * math.sqrt(durationMs.toDouble)

  /** Width over height of a canvas showing `extent` above the caption band. */
  def aspectOf(extent: ScreenRect): Double =
    extent.width / (extent.height / (1.0 - CaptionFraction))

  /** The largest canvas of `aspect` that fits in `width` × `height`. */
  def fit(width: Double, height: Double, aspect: Double): (Double, Double) =
    if width <= 0.0 || height <= 0.0 then (0.0, 0.0)
    else if width / height > aspect then (height * aspect, height)
    else (width, width / aspect)

  /** The region of the screen `input` shows. */
  def extentOf(input: TrialSceneInput): ScreenRect =
    val p     = input.display.placement
    val frame = ScreenRect
      .of(
        p.left.toDouble,
        p.top.toDouble,
        (p.left + p.width).toDouble,
        (p.top + p.height).toDouble
      )
      .getOrElse(ScreenRect.screen(input.screen))
    input.options.extent match
      case TrialExtent.Screen           => ScreenRect.screen(input.screen)
      case TrialExtent.Covering(region) => region
      case TrialExtent.Gaze             =>
        input.fixations
          .foldLeft(frame)((r, f) => r.including(f.screenX, f.screenY))
          .grown(GazeMarginPx)

  /** One extent for trials shown side by side (S8.2's query and reference
    * panels): the smallest region covering each one's [[TrialExtent.Gaze]]
    * extent, so both are drawn at one scale. None for no inputs.
    */
  def sharedExtent(inputs: Vector[TrialSceneInput]): Option[ScreenRect] =
    inputs
      .map(i => extentOf(i.copy(options = i.options.copy(extent = TrialExtent.Gaze))))
      .reduceOption((a, b) => a.including(b.left, b.top).including(b.right, b.bottom))

  /** What the frame shows: a stored image only when it is loaded, a missing
    * asset hatched, never a blank in place of an image.
    */
  def frameArtOf(input: TrialSceneInput): FrameArt =
    def stored(asset: AssetRef): FrameArt =
      input.rasters.get(asset) match
        case Some(StimulusRaster.Loaded(_))       => FrameArt.Picture(asset)
        case Some(StimulusRaster.Unreadable(why)) => FrameArt.Unreadable(asset.file, why)
        case Some(StimulusRaster.Pending) | None  => FrameArt.Loading(asset.file)
    input.display.state match
      case DisplayState.Image(asset)           => stored(asset)
      case DisplayState.Blank                  => FrameArt.Screen
      case DisplayState.BlankWithFixationCross => FrameArt.ScreenWithCross
      case DisplayState.Cue(Some(asset))       => stored(asset)
      case DisplayState.Cue(None)              =>
        FrameArt.CueText(input.display.item.fold("")(_.value))
      case DisplayState.Unknown(Some(asset)) => stored(asset)
      case DisplayState.Unknown(None)        => FrameArt.Unknown
      case DisplayState.MissingAsset(_, f)   => FrameArt.Missing(f)

  /** The "Displayed: …" caption and the frame's placement in the screen. */
  def captionOf(input: TrialSceneInput, art: FrameArt): String =
    import TrialTextId.*
    val p       = input.display.placement
    val kind    = input.display.kind
    val display = art match
      case FrameArt.Screen                   => TrialText(DisplayedBlank)
      case FrameArt.ScreenWithCross          => TrialText(DisplayedBlankCross)
      case FrameArt.CueText(t) if t.nonEmpty => TrialText(DisplayedCueItem, t)
      case FrameArt.CueText(_)               => TrialText(DisplayedCue)
      case FrameArt.Unknown                  => TrialText(DisplayedUnknown)
      case FrameArt.Missing(f)               => TrialText(DisplayedMissing, f.value)
      case FrameArt.Unreadable(f, _)         => TrialText(DisplayedUnreadable, f.value)
      case FrameArt.Loading(f)               => TrialText(DisplayedLoading, f.value)
      case FrameArt.Picture(a) if kind == DisplayKind.Cue =>
        TrialText(DisplayedCueItem, a.file.value)
      case FrameArt.Picture(a) if kind == DisplayKind.Unknown =>
        TrialText(DisplayedUnknown) + TrialText(Separator) + a.file.value
      case FrameArt.Picture(a) => TrialText(DisplayedImage, a.file.value)
    // Pixel sizes and offsets are written plainly, as the boards write them.
    val placement = TrialText(
      Placement,
      p.width.toString,
      p.height.toString,
      p.left.toString,
      p.top.toString,
      input.screen.width.toString,
      input.screen.height.toString
    )
    display + TrialText(Separator) + placement

  /** The trial scene of `input`. */
  def apply(input: TrialSceneInput): Either[TrialSceneError, TrialScene] =
    val trial = input.display.trial
    for
      _     <- duplicates(trial, input.fixations)
      _     <- input.map.fold(Right(()))(m => mapOf(trial, m))
      id    <- sceneId(input).left.map(TrialSceneError.Plot(trial, _))
      built <- Builder(input).build.left.map { (part, e) =>
        TrialSceneError.Graphics(trial, part, e)
      }
      (grobs, viewport, extent, art, caption, marks) = built
      panel <- DataPanel(id, viewport).left.map(TrialSceneError.Plot(trial, _))
      plot  <- PlotScene(id, Scene(grobs), panel).left.map(TrialSceneError.Plot(trial, _))
    yield new TrialScene(
      plot,
      extent,
      art,
      caption,
      marks,
      input.map.map(_.grid.map),
      disclosureOf(input)
    )

  /** What the trial view says about the remembered image: that it was not
    * displayed, whenever it is shown as an underlay, and that it is not
    * shown otherwise.
    */
  def disclosureOf(input: TrialSceneInput): Option[String] = input.remembered match
    case RememberedImage.Absent    => None
    case RememberedImage.Hidden(_) => Some(TrialText(TrialTextId.RememberedHidden))
    case RememberedImage.Shown(_)  => Some(TrialText(TrialTextId.RememberedShown))

  private def mapOf(trial: TrialKey, m: TrialMap): Either[TrialSceneError, Unit] =
    if m.raster.key.map != m.grid.map then
      Left(TrialSceneError.MapMismatch(trial, m.raster.key.map, m.grid.map))
    else if m.grid.map.trial != trial then
      Left(TrialSceneError.MapOfAnotherTrial(trial, m.grid.map))
    else Right(())

  private def duplicates(
      trial: TrialKey,
      fixations: Vector[TrialFixation]
  ): Either[TrialSceneError, Unit] =
    val indices = fixations.map(_.index.value)
    indices.diff(indices.distinct).headOption match
      case Some(i) => Left(TrialSceneError.DuplicateFixation(trial, i))
      case None    => Right(())

  private def sceneId(input: TrialSceneInput): Either[PlotSceneError, SceneId] =
    val k    = input.display.trial
    val mode = input.marks match
      case MarkStyle.Neutral    => "neutral"
      case MarkStyle.Role(role) => role.toString.toLowerCase
    SceneId(
      s"studio.trial.${k.participant}.${k.phase.label}.${k.trial}.${k.occurrence}.$mode"
    )

  // The grobs of one scene. Failures name the part that was being built.
  private final class Builder(input: TrialSceneInput):
    private type R[A] = Either[(String, GraphicsError), A]

    private def part[A](name: String)(value: Either[GraphicsError, A]): R[A] =
      value.left.map(name -> _)

    private def themed(t: ThemedToken): Rgba   = IntaglioColours.themed(input.theme, t)
    private def staged(t: StageToken): Rgba    = IntaglioColours.staged(input.stage, t)
    private def palette(t: PaletteToken): Rgba =
      IntaglioColours.toIntaglio(Tokens.palette(t))

    private val placement = input.display.placement
    private val frameLeft = placement.left.toDouble
    private val frameTop  = placement.top.toDouble
    private val frameW    = placement.width.toDouble
    private val frameH    = placement.height.toDouble
    private val centreX   = frameLeft + frameW / 2.0
    private val centreY   = frameTop + frameH / 2.0

    private def stroke(
        colour: Rgba,
        widthPx: Double,
        lineType: LineType = LineType.Solid
    ): Either[GraphicsError, GraphicParams] =
      GraphicParams.checked(
        stroke = Some(colour),
        fill = None,
        lineWidth = pt(widthPx),
        lineType = lineType,
        lineWidthUnit = StrokeUnit.Point
      )

    private def fill(colour: Rgba): Either[GraphicsError, GraphicParams] =
      GraphicParams.checked(stroke = None, fill = Some(colour))

    private def font(colour: Rgba, face: FontFace, size: TypeSize) =
      Length
        .points(pt(size.px.toDouble))
        .flatMap(fs =>
          GraphicParams.checked(
            stroke = None,
            fill = Some(colour),
            fontFamily = Some(face.javaFxFamily),
            fontSize = fs
          )
        )

    private def name(value: String): Either[GraphicsError, GraphicsName] =
      GraphicsName(value, "trial scene")

    private def points(px: Double): LengthExpr = LengthExpr(Length.pointsUnsafe(pt(px)))

    private def traverse[A, B](as: Vector[A])(f: A => R[B]): R[Vector[B]] =
      as.foldLeft[R[Vector[B]]](Right(Vector.empty))((acc, a) =>
        acc.flatMap(bs => f(a).map(bs :+ _))
      )

    // The image frame as a rect in the panel's native (screen) coordinates.
    private def frameRect(gp: GraphicParams, label: String): Either[GraphicsError, Grob] =
      for
        centre <- Point.native(centreX, centreY)
        w      <- ExtentExpr.native(frameW)
        h      <- ExtentExpr.native(frameH)
        n      <- name(label)
        rect   <- Grob.rect(centre, Size.fromExtents(w, h), gp = gp, name = Some(n))
      yield rect

    private def centredText(label: String, dy: Double, gp: GraphicParams) =
      for
        at <- Point.native(centreX, centreY + dy)
        t  <- Grob.text(label, at, Anchor(HJust.Center, VJust.Center), gp = gp)
      yield t

    private def picture(asset: AssetRef): R[Vector[Grob]] =
      input.rasters.get(asset) match
        case Some(StimulusRaster.Loaded(image)) =>
          part("stimulus image")(
            for
              centre <- Point.native(centreX, centreY)
              w      <- ExtentExpr.native(frameW)
              h      <- ExtentExpr.native(frameH)
              n      <- name(StimulusName)
              img    <- Grob.image(
                image,
                centre,
                Size.fromExtents(w, h),
                interpolation = RasterInterpolation.Smooth,
                name = Some(n)
              )
            yield Vector(img)
          )
        case _ => Right(Vector.empty) // frameArtOf never pictures an unloaded asset

    // A stored image drawn across the image frame, under `label`.
    private def frameImage(image: RasterImage, label: String, smooth: Boolean) =
      for
        centre <- Point.native(centreX, centreY)
        w      <- ExtentExpr.native(frameW)
        h      <- ExtentExpr.native(frameH)
        n      <- name(label)
        img    <- Grob.image(
          image,
          centre,
          Size.fromExtents(w, h),
          interpolation =
            if smooth then RasterInterpolation.Smooth else RasterInterpolation.Nearest,
          name = Some(n)
        )
      yield img

    // The remembered image, under the map, when it is shown and loaded.
    private def underlay: R[Vector[Grob]] =
      input.remembered match
        case RememberedImage.Shown(asset) =>
          input.rasters.get(asset) match
            case Some(StimulusRaster.Loaded(image)) =>
              part("remembered-image underlay")(frameImage(image, UnderlayName, true))
                .map(Vector(_))
            case _ => Right(Vector.empty)
        case _ => Right(Vector.empty)

    // The screen region a map covers: (left, top, width, height).
    private def coverage(m: TrialMap): (Double, Double, Double, Double) = m.covers match
      case MapCoverage.ImageFrame => (frameLeft, frameTop, frameW, frameH)
      case MapCoverage.Region(r)  => (r.left, r.top, r.width, r.height)

    // The map's raster at the global opacity, cell for cell over the region
    // it covers, and its isolines, cased.
    private def mapLayer: R[Vector[Grob]] =
      input.map.fold[R[Vector[Grob]]](Right(Vector.empty)) { m =>
        val raster            = m.raster
        val drawn             = raster.drawn(m.opacity)
        val (left, top, w, h) = coverage(m)
        for
          dims <- part("map raster")(RasterDimensions(raster.width, raster.height))
          image = RasterImage.tabulate(dims) { (x, y) =>
            val p = drawn(y * raster.width + x)
            Rgba32.unsafe((p >> 16) & 0xff, (p >> 8) & 0xff, p & 0xff, (p >>> 24) & 0xff)
          }
          layer <- part("map raster")(
            for
              centre <- Point.native(left + w / 2.0, top + h / 2.0)
              we     <- ExtentExpr.native(w)
              he     <- ExtentExpr.native(h)
              n      <- name(MapName)
              img    <- Grob.image(
                image,
                centre,
                Size.fromExtents(we, he),
                interpolation = RasterInterpolation.Nearest,
                name = Some(n)
              )
            yield img
          )
          lines <- isolines(m.grid, left, top, w, h)
        yield layer +: lines
      }

    private def isolines(
        grid: MapGrid,
        left: Double,
        top: Double,
        w: Double,
        h: Double
    ): R[Vector[Grob]] =
      val cellW            = w / grid.columns
      val cellH            = h / grid.rows
      def at(p: GridPoint) = Point.native(left + p.x * cellW, top + p.y * cellH)
      val segments         = Isolines.of(grid).flatMap(_.segments)
      if segments.isEmpty then Right(Vector.empty)
      else
        part("isolines")(
          for
            ink    <- stroke(palette(PaletteToken.IsolineInk), IsolinePx)
            width  <- StrokeWidth.points(pt(IsolineCasingPx))
            casing <- StrokeCasing.checked(
              palette(PaletteToken.IsolineCase),
              CasingWidth.Absolute(width)
            )
            pairs <- segments.foldLeft[Either[GraphicsError, Vector[(Point, Point)]]](
              Right(Vector.empty)
            ) { case (acc, (a, b)) =>
              for
                ps <- acc
                pa <- at(a)
                pb <- at(b)
              yield ps :+ (pa -> pb)
            }
            n     <- name(IsolinesName)
            lines <- Grob.segments(pairs, gp = ink.withCasing(casing), name = Some(n))
          yield Vector(lines)
        )

    private def screenFill: R[Grob] =
      part("blank screen")(fill(palette(PaletteToken.Screen)).flatMap(frameRect(_, FrameName)))

    private def hatched(label: String): R[Vector[Grob]] =
      part("missing-asset hatching")(
        for
          recipe <- PatternRecipe.angledHatch(45.0, 9.0, 3.0)
          base   <- fill(themed(ThemedToken.WarnSoft))
          gp = base.withPatternFill(
            PatternPaint(
              recipe,
              themed(ThemedToken.WarnText),
              Some(themed(ThemedToken.WarnSoft))
            )
          )
          rect <- frameRect(gp, FrameName)
          xGp  <- stroke(themed(ThemedToken.WarnText), 2.0)
          arm = frameH * 0.06
          a1      <- Point.native(centreX - arm, centreY - arm)
          b1      <- Point.native(centreX + arm, centreY + arm)
          a2      <- Point.native(centreX + arm, centreY - arm)
          b2      <- Point.native(centreX - arm, centreY + arm)
          cross   <- Grob.segments(Vector(a1 -> b1, a2 -> b2), gp = xGp)
          plateAt <- Point.native(centreX, centreY + frameH * 0.16)
          plateW  <- ExtentExpr.native(frameW * 0.8)
          plateH  <- ExtentExpr.points(pt(22.0))
          plateGp <- fill(themed(ThemedToken.WarnSoft))
          plate   <- Grob.rect(plateAt, Size.fromExtents(plateW, plateH), gp = plateGp)
          textGp  <- font(themed(ThemedToken.WarnText), FontFace.SansMedium, TypeSize.T12)
          text    <- centredText(label, frameH * 0.16, textGp)
        yield Vector(rect, cross, plate, text)
      )

    private def frameArt(art: FrameArt): R[Vector[Grob]] =
      import TrialTextId.*
      art match
        case FrameArt.Picture(asset)  => picture(asset)
        case FrameArt.Screen          => screenFill.map(Vector(_))
        case FrameArt.ScreenWithCross =>
          for
            screen <- screenFill
            cross  <- part("fixation cross")(
              for
                gp <- stroke(staged(StageToken.Halo), 1.5)
                v1 <- Point.native(centreX, centreY - CrossArmPx)
                v2 <- Point.native(centreX, centreY + CrossArmPx)
                h1 <- Point.native(centreX - CrossArmPx, centreY)
                h2 <- Point.native(centreX + CrossArmPx, centreY)
                s  <- Grob.segments(Vector(v1 -> v2, h1 -> h2), gp = gp)
              yield s
            )
          yield Vector(screen, cross)
        case FrameArt.CueText(text) =>
          for
            screen <- screenFill
            label  <- part("cue text")(
              font(staged(StageToken.Halo), FontFace.SansRegular, TypeSize.T16)
                .flatMap(centredText(text, 0.0, _))
            )
          yield Vector(screen, label)
        case FrameArt.Unknown =>
          part("unknown display")(
            for
              gp    <- stroke(staged(StageToken.OnStage), 1.0, LineType.Custom(WindowDash))
              rect  <- frameRect(gp, FrameName)
              tgp   <- font(staged(StageToken.OnStage), FontFace.SansRegular, TypeSize.T28)
              glyph <- centredText(TrialText(UnknownGlyph), 0.0, tgp)
            yield Vector(rect, glyph)
          )
        case FrameArt.Missing(file)       => hatched(TrialText(MissingLabel, file.value))
        case FrameArt.Unreadable(file, _) => hatched(TrialText(UnreadableLabel, file.value))
        case FrameArt.Loading(file)       =>
          part("loading state")(
            for
              gp   <- stroke(staged(StageToken.OnStage), 1.0, LineType.Custom(WindowDash))
              rect <- frameRect(gp, FrameName)
              tgp  <- font(staged(StageToken.OnStage), FontFace.SansRegular, TypeSize.T12)
              text <- centredText(TrialText(LoadingLabel, file.value), 0.0, tgp)
            yield Vector(rect, text)
          )

    private def window(art: FrameArt): R[Vector[Grob]] =
      // The unknown and loading states already draw the frame's outline.
      val outlined = art match
        case FrameArt.Unknown | FrameArt.Loading(_) => true
        case _                                      => false
      if !input.options.windowOutline || outlined then Right(Vector.empty)
      else
        part("analysis-window outline")(
          for
            gp <- stroke(staged(StageToken.OnStage), WindowLinePx, LineType.Custom(WindowDash))
            rect  <- frameRect(gp, WindowName)
            tgp   <- font(staged(StageToken.OnStage), FontFace.SansRegular, TypeSize.T11)
            at    <- Point.native(frameLeft + frameW, frameTop)
            label <- Grob.text(
              TrialText(
                TrialTextId.WindowLabel,
                placement.width.toString,
                placement.height.toString
              ),
              at,
              Anchor(HJust.Right, VJust.Bottom),
              gp = tgp
            )
          yield Vector(rect, label)
        )

    private def markColour(role: TrialRole): Rgba = role match
      case TrialRole.Query   => themed(ThemedToken.Query)
      case TrialRole.Matched => themed(ThemedToken.Match)
      case TrialRole.Control => themed(ThemedToken.Control)

    private def shapeOf(style: MarkStyle): PointShape = style match
      case MarkStyle.Role(TrialRole.Matched) => PointShape.Diamond
      case _                                 => PointShape.Circle

    // A mark's paint (DESIGN_SPEC sections 5, 9, 12, 14): each core placement
    // has its own visible treatment. Only InMap is filled; the two window
    // policies remain visible even though neither adds an out-of-window point.
    private def markParams(f: TrialFixation): Either[GraphicsError, GraphicParams] =
      val halo = staged(StageToken.Halo)
      def included(line: LineType): Either[GraphicsError, GraphicParams] = input.marks match
        case MarkStyle.Role(TrialRole.Control) =>
          stroke(markColour(TrialRole.Control), HaloPx, line)
        case MarkStyle.Role(role) =>
          stroke(halo, HaloPx, line).map(_.withSolidFill(Some(markColour(role))))
        case MarkStyle.Neutral =>
          stroke(halo, HaloPx, line).map(_.withSolidFill(Some(themed(ThemedToken.NeutralMark))))
      f.placement match
        case MapPlacement.DroppedInitial =>
          stroke(halo, HaloPx, LineType.Custom(DroppedInitialDash))
        case MapPlacement.OutsideScreen =>
          stroke(halo, HaloPx, LineType.Custom(OutsideScreenDash))
        case MapPlacement.OutsideWindow(OffWindowPolicy.Exclude) =>
          stroke(halo, HaloPx, LineType.Custom(OutsideExcludeDash))
        // A failing trial's in-window fixations are drawn as its outside ones are:
        // the trial contributes no map. Its text and announcement say why.
        case MapPlacement.OutsideWindow(OffWindowPolicy.FailTrial) |
            MapPlacement.TrialFailed(_) =>
          stroke(halo, HaloPx, LineType.Custom(OutsideFailTrialDash))
        case MapPlacement.InWindow => included(LineType.Solid)

    private def sizeOf(f: TrialFixation): Either[GraphicsError, ExtentExpr] =
      ExtentExpr.points(pt(radiusPx(f.durationMs)))

    // A hollow control mark in the map is cased by the halo.
    private def cased(f: TrialFixation): Boolean =
      f.placement == MapPlacement.InWindow && input.marks == MarkStyle.Role(TrialRole.Control)

    // A cased mark is a closed ring with an Intaglio StrokeCasing: Intaglio
    // paints casings on linear outlines only, not on point marks. The casing
    // is paint of the ring, not a second grob, so picking sees one target.
    private def ring(f: TrialFixation, n: GraphicsName): Either[GraphicsError, Grob] =
      val r = pt(radiusPx(f.durationMs))
      def offset(base: LengthExpr, d: Double): Either[GraphicsError, LengthExpr] =
        ExtentExpr.points(math.abs(d)).map(e => if d >= 0.0 then base + e else base - e)
      val empty: Either[GraphicsError, Vector[Point]] = Right(Vector.empty)
      for
        x0       <- LengthExpr.native(f.screenX)
        y0       <- LengthExpr.native(f.screenY)
        vertices <- (0 until RingSegments).foldLeft(empty) { (acc, k) =>
          val a = 2.0 * math.Pi * k / RingSegments
          for
            ps <- acc
            x  <- offset(x0, r * math.cos(a))
            y  <- offset(y0, r * math.sin(a))
          yield ps :+ Point(x, y)
        }
        gp     <- markParams(f)
        width  <- StrokeWidth.points(pt(CasingPx))
        casing <- StrokeCasing.checked(staged(StageToken.Halo), CasingWidth.Absolute(width))
        grob   <- Grob.polygon(vertices, gp = gp.withCasing(casing), name = Some(n))
      yield grob

    private def mark(f: TrialFixation, shape: PointShape, n: GraphicsName) =
      if cased(f) then ring(f, n)
      else
        for
          at   <- Point.native(f.screenX, f.screenY)
          size <- sizeOf(f)
          gp   <- markParams(f)
          grob <- Grob.pointBatch(
            Vector(at),
            sizes = BatchColumn.Constant(size),
            shapes = BatchColumn.Constant(shape),
            graphicParams = BatchColumn.Constant(gp),
            name = Some(n)
          )
        yield grob

    private def reach(f: TrialFixation, shape: PointShape): Double =
      val r       = radiusPx(f.durationMs)
      val outline = shape match
        case PointShape.Diamond => PointShape.diamondHalfDiagonal(r)
        case _                  => r
      outline + (if cased(f) then CasingPx else HaloPx) / 2.0

    // One named grob per mark, in fixation order, inside the named marks group.
    private def marks: R[(Vector[Grob], Vector[TrialMark])] =
      val fs = input.fixations
      if !input.options.points || fs.isEmpty then Right((Vector.empty, Vector.empty))
      else
        val shape = shapeOf(input.marks)
        val trial = input.display.trial
        for
          drawn <- traverse(fs.zipWithIndex) { (f, i) =>
            part("fixation mark")(
              for
                n <- name(markName(f.index))
                g <- mark(f, shape, n)
              yield (
                g,
                TrialMark(
                  StudioRef.Fixation(trial, f.index),
                  DataPoint(f.screenX, f.screenY),
                  radiusPx(f.durationMs),
                  reach(f, shape),
                  f.placement,
                  i,
                  n
                )
              )
            )
          }
          group <- part("fixation marks")(name(MarksName))
          title = TrialText(TrialTextId.MarksTitle, trial.label)
        yield (
          Vector(
            Grob
              .annotated(Grob.group(drawn.map(_._1), name = Some(group)), GrobMeta.title(title))
          ),
          drawn.map(_._2)
        )

    private def order: R[Vector[Grob]] =
      val fs = input.fixations
      if !input.options.order || fs.size < 2 then Right(Vector.empty)
      else
        part("order lines")(
          for
            gp  <- stroke(staged(StageToken.Halo), OrderLinePx, LineType.Custom(OrderDash))
            pts <- fs.foldLeft[Either[GraphicsError, Vector[Point]]](Right(Vector.empty)) {
              (acc, f) => acc.flatMap(ps => Point.native(f.screenX, f.screenY).map(ps :+ _))
            }
            n     <- name(OrderName)
            lines <- Grob.lines(pts, gp = gp, name = Some(n))
          yield Vector(lines)
        )

    private def captions(caption: String): R[Vector[Grob]] =
      import TrialTextId.*
      val onStage = staged(StageToken.OnStage)
      part("caption")(
        for
          gp    <- font(onStage, FontFace.SansRegular, TypeSize.T11)
          top   <- LengthExpr.npc(CaptionFraction)
          left  <- LengthExpr.npc(0.0)
          right <- LengthExpr.npc(1.0)
          line1 = top - points(6.0)
          line2 = top - points(22.0)
          n    <- name(CaptionName)
          main <- Grob.text(
            caption,
            Point(left + points(8.0), line1),
            Anchor(HJust.Left, VJust.Top),
            gp = gp,
            name = Some(n)
          )
          area <-
            if input.options.points && input.fixations.nonEmpty then
              Grob
                .text(
                  TrialText(MarkerArea),
                  Point(right - points(8.0), line1),
                  Anchor(HJust.Right, VJust.Top),
                  gp = gp
                )
                .map(Vector(_))
            else Right(Vector.empty)
          remembered <- disclosureOf(input).fold[Either[GraphicsError, Vector[Grob]]](
            Right(Vector.empty)
          ) { said =>
            name(RememberName).flatMap(n =>
              Grob
                .text(
                  said,
                  Point(right - points(8.0), line2),
                  Anchor(HJust.Right, VJust.Top),
                  gp = gp,
                  name = Some(n)
                )
                .map(Vector(_))
            )
          }
          note <-
            if input.options.order && input.fixations.size >= 2 then
              Grob
                .text(
                  TrialText(OrderNote),
                  Point(left + points(8.0), line2),
                  Anchor(HJust.Left, VJust.Top),
                  gp = gp
                )
                .map(Vector(_))
            else Right(Vector.empty)
        yield (main +: area) ++ note ++ remembered
      )

    def build: R[(Vector[Grob], Viewport, ScreenRect, FrameArt, String, Vector[TrialMark])] =
      val extent  = extentOf(input)
      val art     = frameArtOf(input)
      val caption = captionOf(input, art)
      for
        viewport <- part("data panel")(
          for
            xs     <- Interval(extent.left, extent.right)
            ys     <- Interval(extent.top, extent.bottom)
            origin <- Point.npc(0.0, CaptionFraction)
            size   <- Size.npc(1.0, 1.0 - CaptionFraction)
            vp     <- Viewport.checked(
              origin,
              size,
              xs,
              ys,
              Clip.Off,
              yDirection = YDirection.Down
            )
          yield vp
        )
        stage <- part("stage surround")(
          for
            gp     <- fill(staged(StageToken.Stage))
            centre <- Point.npc(0.5, 0.5)
            whole  <- Size.npc(1.0, 1.0)
            n      <- name(StageName)
            rect   <- Grob.rect(centre, whole, gp = gp, name = Some(n))
          yield rect
        )
        panel       <- part("data panel")(name(PanelName))
        artGrobs    <- frameArt(art)
        under       <- underlay
        mapGrobs    <- mapLayer
        windowGrobs <- window(art)
        orderGrobs  <- order
        marked      <- marks
        (markGrobs, drawn) = marked
        captionGrobs <- captions(caption)
      yield (
        Vector(
          stage,
          Grob.group(
            artGrobs ++ under ++ mapGrobs ++ windowGrobs ++ orderGrobs ++ markGrobs,
            viewport = Some(viewport),
            name = Some(panel)
          )
        ) ++ captionGrobs,
        viewport,
        extent,
        art,
        caption,
        drawn
      )
