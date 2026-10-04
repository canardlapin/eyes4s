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

import eyes4s.studio.app.plot.{
  ColumnId,
  ParticipantColumns,
  PlotSource,
  PlotText,
  PlotTextId,
  PlotValue
}
import eyes4s.studio.app.text.{ParticipantText, ParticipantTextId}
import eyes4s.studio.app.tokens.{FontFace, Theme, ThemedToken, TypeSize}
import eyes4s.studio.core.selection.StudioRef
import intaglio.{
  Anchor,
  BatchColumn,
  Clip,
  DashPattern,
  ExtentExpr,
  GraphicParams,
  GraphicsError,
  GraphicsName,
  Grob,
  HJust,
  Interval,
  Length,
  LengthExpr,
  LineType,
  Point,
  PointShape,
  Rgba,
  Scene,
  Size,
  StrokeUnit,
  VJust,
  Viewport
}

/** The participant plot (ticket S4.5c; the Results board's participant D
  * plot and the Figures board's panel D): one column per reporting group,
  * each participant's mean D in it as a dot on the D (Δ cosine) axis, a
  * line joining a participant's means in neighbouring groups, and each
  * group's grand mean as a 52 px tick, over a zero rule.
  *
  * Its source is [[eyes4s.studio.app.plot.ParticipantMeans.source]]: a
  * participant's mean in a group is a [[StudioRef.ParticipantSummary]] row,
  * a group's grand mean a [[StudioRef.GroupCell]] row, and the builder
  * tells them apart by ref. Rows are placed by [[Axis.Category]] of the
  * group labels (in source order, first at the left) by [[Axis.Numeric]] of
  * D. The first group is drawn solid (filled ink dots, an ink tick) and the
  * others dashed (hollow ink-3 dots, a dashed ink-3 tick).
  *
  * A participant with no mean in a group is drawn as a positionless dashed
  * empty ring on a band below every value and the zero rule, labelled "no
  * value": missing is drawn differently from zero, and joins no line. A
  * grand mean without a value, or a row without a group, is [[Unplotted]].
  *
  * Each participant's dots are spread across their column by a fixed pixel
  * nudge, the same for the participant in every group. Every number the
  * plot writes is a row's cell as the table writes it: a grand mean beside
  * its tick and each group's n (its participants) under its column. The
  * only axis label is the zero rule's.
  *
  * Marks are ordered, and drawn, column by column: the participants in row
  * order, then the grand mean on top.
  */
final case class ParticipantPlot(columns: ParticipantColumns) extends PlotBuilder:

  def kind: String = "participant-plot"

  def build(source: PlotSource, theme: Theme): Either[PlotBuildError, BuiltPlot] =
    import ParticipantPlot.*
    def numeric(id: ColumnId) =
      for
        i <- source.indexOf(id).toRight(PlotBuildError.MissingColumn(kind, id))
        _ <- Either.cond(
          source.columns(i).format.numeric,
          (),
          PlotBuildError.NotNumeric(kind, id)
        )
      yield i
    for
      _ <- source
        .indexOf(columns.group)
        .toRight(PlotBuildError.MissingColumn(kind, columns.group))
      di <- numeric(columns.d)
      ni <- source.indexOf(columns.n).toRight(PlotBuildError.MissingColumn(kind, columns.n))
      levels   = groupLevels(source, columns.group)
      encoding = PositionEncoding(
        Axis.Category(columns.group, levels),
        Axis.Numeric(columns.d, AxisScale.Linear)
      )
      roles <- traverseAll(source.rows)(r =>
        roleOf(r.ref).toRight(PlotBuildError.UnexpectedRow(kind, r.ref))
      )
      _ <- traverseAll(levels) { label =>
        val identities = source.rows.indices.toVector
          .filter(i => source.value(i, columns.group).contains(PlotValue.Text(label)))
          .flatMap(i => groupIdentity(source.rows(i).ref))
          .distinct
        Either.cond(
          identities.size <= 1,
          (),
          PlotBuildError.AmbiguousLevel(kind, columns.group, label, identities)
        )
      }
      (drawn, unplotted, frame) = plan(source, encoding, levels, roles)
      id <- SceneId(s"studio.plot.$kind.${theme.toString.toLowerCase}").left
        .map(PlotBuildError.Scene(kind, _))
      built <- scene(theme, source, levels, drawn, frame, di, ni).left
        .map(PlotBuildError.Graphics(kind, "the participant plot", _))
      (grobs, viewport, names) = built
      marks <- traverseAll(drawn.zip(names).zipWithIndex) { case ((d, name), order) =>
        PlotMark
          .of(kind, Vector(d.row), d.at, d.reachPx, order, name)
          .flatMap(_.nudged(kind, d.dxPx, 0.0))
      }
      panel     <- DataPanel(id, viewport).left.map(PlotBuildError.Scene(kind, _))
      plotScene <- PlotScene(id, Scene(grobs), panel).left.map(PlotBuildError.Scene(kind, _))
      plot      <- BuiltPlot(
        kind,
        source,
        plotScene,
        ParticipantText(ParticipantTextId.Title),
        PlotText(
          PlotTextId.PlotKeys,
          ParticipantText(ParticipantTextId.Summary, source.caption, levels.size.toString)
        ),
        encoding,
        marks,
        unplotted
      )
    yield plot

  // The marks to draw, in order, the rows set aside, and the D range drawn.
  private def plan(
      source: PlotSource,
      encoding: PositionEncoding,
      levels: Vector[String],
      roles: Vector[ParticipantPlot.Role]
  ): (Vector[ParticipantPlot.Drawn], Vector[Unplotted], ParticipantPlot.Frame) =
    import ParticipantPlot.*
    val rows                              = source.rows.indices.toVector
    def refOf(i: Int)                     = source.rows(i).ref
    def missing(i: Int, column: ColumnId) =
      Unplotted(refOf(i), i, NoPosition.MissingValue(column))
    val levelOf: Vector[Option[Int]] =
      rows.map(i => Axis.position(encoding.x, source, i).map(_.toInt))
    val noLevel = rows.filter(levelOf(_).isEmpty).map(missing(_, columns.group))
    // Each participant's place in the source, for its nudge in every column.
    val people = rows.flatMap(i => participantOf(roles(i))).distinct
    def nudge(role: Role, level: Int): Double =
      participantOf(role).fold(0.0)(p => jitterPx(people.indexOf(p) + JitterShift * level))
    val placed = rows.flatMap(i => encoding.place(source, i).map(i -> _)).toMap
    val absent =
      rows.exists(i => roles(i) != Role.Grand && levelOf(i).isDefined && !placed.contains(i))
    val frame    = Frame.of(placed.values.map(_.y).toVector, absent)
    val perLevel = levels.indices.toVector.map { level =>
      val here     = rows.filter(levelOf(_).contains(level))
      val (gs, ps) = here.partition(roles(_) == Role.Grand)
      val dots     = ps.map { i =>
        val dx = nudge(roles(i), level)
        placed.get(i) match
          case Some(at) =>
            Drawn(
              MarkedRow(refOf(i), i, RowMarking.Placed(at)),
              at,
              dx,
              Shape.Dot(level == 0),
              participantOf(roles(i))
            )
          case None =>
            Drawn(
              MarkedRow(
                refOf(i),
                i,
                RowMarking.Positionless(NoPosition.MissingValue(columns.d))
              ),
              DataPoint(level.toDouble, frame.band),
              dx,
              Shape.Empty,
              None
            )
      }
      val ticks = gs.flatMap(i =>
        placed
          .get(i)
          .map(at =>
            Drawn(
              MarkedRow(refOf(i), i, RowMarking.Placed(at)),
              at,
              0.0,
              Shape.Tick(level == 0),
              None
            )
          )
      )
      val unticked = gs.filterNot(placed.contains).map(missing(_, columns.d))
      (dots ++ ticks, unticked)
    }
    (perLevel.flatMap(_._1), (noLevel ++ perLevel.flatMap(_._2)).sortBy(_.row), frame)

  // The grobs, the data viewport and each drawn mark's grob name, in order.
  private def scene(
      theme: Theme,
      source: PlotSource,
      levels: Vector[String],
      drawn: Vector[ParticipantPlot.Drawn],
      frame: ParticipantPlot.Frame,
      dIndex: Int,
      nIndex: Int
  ): Either[GraphicsError, (Vector[Grob], Viewport, Vector[GraphicsName])] =
    import ParticipantPlot.*
    def colour(token: ThemedToken): Rgba = IntaglioColours.themed(theme, token)
    // A data point moved by logical pixels (x right).
    def at(x: Double, y: Double, dxPx: Double) =
      for
        nx <- LengthExpr.native(x)
        ny <- LengthExpr.native(y)
        dx <- Length.points(px(dxPx))
      yield Point(nx + LengthExpr(dx), ny)
    def stroke(token: ThemedToken, widthPx: Double, lineType: LineType = LineType.Solid) =
      GraphicParams.checked(
        stroke = Some(colour(token)),
        fill = None,
        lineWidth = px(widthPx),
        lineType = lineType,
        lineWidthUnit = StrokeUnit.Point
      )
    def font(face: FontFace, token: ThemedToken) =
      Length
        .points(px(TypeSize.T11.px.toDouble))
        .flatMap(size =>
          GraphicParams.checked(
            stroke = None,
            fill = Some(colour(token)),
            fontFamily = Some(face.javaFxFamily),
            fontSize = size
          )
        )
    // The tick of each column's grand mean, by level, with its row.
    val grand = drawn.collect { case d @ Drawn(_, _, _, Shape.Tick(_), _) =>
      d.at.x.toInt -> d
    }.toMap
    val links = drawn
      .collect { case d @ Drawn(_, _, _, Shape.Dot(_), Some(p)) => (p, d) }
      .groupMap(_._1)(_._2)
      .values
      .toVector
      .flatMap(ds =>
        ds.sortBy(_.at.x).sliding(2).collect {
          case Vector(a, b) if b.at.x == a.at.x + 1.0 => (a, b)
        }
      )
      .sortBy((a, _) => a.row.row)
    for
      xScale   <- Interval(-0.5, math.max(levels.size, 1) - 0.5)
      yScale   <- Interval(frame.y0, frame.y1)
      origin   <- Point.npc(0.1, 0.16)
      size     <- Size.npc(0.86, 0.74)
      viewport <- Viewport.checked(origin, size, xScale, yScale, Clip.On)
      frameVp  <- Viewport.checked(origin, size, xScale, yScale, Clip.Off)
      whole    <- Size.npc(1.0, 1.0)
      centre   <- Point.npc(0.5, 0.5)
      surface  <- GraphicParams.checked(stroke = None, fill = Some(colour(ThemedToken.Surface)))
      background <- Grob.rect(centre, whole, gp = surface)
      left       <- LengthExpr.npc(0.0)
      right      <- LengthExpr.npc(1.0)
      gridGp     <- stroke(ThemedToken.Surface3, 1.0)
      grid       <- traverseAll(frame.grid)(y =>
        LengthExpr.native(y).map(ny => (Point(left, ny), Point(right, ny)))
      )
      gridLines <-
        if grid.isEmpty then Right(Vector.empty)
        else Grob.segments(grid, gp = gridGp).map(Vector(_))
      zeroGp   <- stroke(ThemedToken.Ink, ZeroRulePx)
      zeroY    <- LengthExpr.native(0.0)
      zeroRule <- Grob.segments(Vector((Point(left, zeroY), Point(right, zeroY))), gp = zeroGp)
      linkGp   <- GraphicParams.checked(
        stroke = Some(colour(ThemedToken.Ink3)),
        fill = None,
        lineWidth = px(1.0),
        alpha = LinkAlpha,
        lineWidthUnit = StrokeUnit.Point
      )
      linkPoints <- traverseAll(links)((a, b) =>
        for
          pa <- at(a.at.x, a.at.y, a.dxPx)
          pb <- at(b.at.x, b.at.y, b.dxPx)
        yield (pa, pb)
      )
      linked <-
        if linkPoints.isEmpty then Right(Vector.empty)
        else Grob.segments(linkPoints, gp = linkGp).map(Vector(_))
      solidDotGp  <- GraphicParams.checked(stroke = None, fill = Some(colour(ThemedToken.Ink)))
      hollowDotGp <- GraphicParams.checked(
        stroke = Some(colour(ThemedToken.Ink3)),
        fill = Some(colour(ThemedToken.Surface)),
        lineWidth = px(HollowEdgePx),
        lineWidthUnit = StrokeUnit.Point
      )
      emptyDash <- DashPattern(EmptyDash)
      // Filled with the surface, so it reads as empty yet its inside picks it.
      emptyGp <- GraphicParams.checked(
        stroke = Some(colour(ThemedToken.Ink3)),
        fill = Some(colour(ThemedToken.Surface)),
        lineWidth = px(EmptyEdgePx),
        lineType = LineType.Custom(emptyDash),
        lineWidthUnit = StrokeUnit.Point
      )
      tickDash    <- DashPattern(TickDash)
      solidTickGp <- stroke(ThemedToken.Ink, TickPx)
      dashTickGp  <- stroke(ThemedToken.Ink3, TickPx, LineType.Custom(tickDash))
      dotSize     <- ExtentExpr.points(px(DotRadiusPx))
      emptySize   <- ExtentExpr.points(px(EmptyRadiusPx))
      marked      <- traverseAll(drawn) { d =>
        for
          name <- GraphicsName(s"$MarkPrefix${d.row.row}", "participant plot mark")
          grob <- d.shape match
            case Shape.Tick(solid) =>
              for
                a <- at(d.at.x, d.at.y, -TickHalfPx)
                b <- at(d.at.x, d.at.y, TickHalfPx)
                g <- Grob.segments(
                  Vector((a, b)),
                  gp = if solid then solidTickGp else dashTickGp,
                  name = Some(name)
                )
              yield g
            case shape =>
              val (radius, gp) = shape match
                case Shape.Dot(true)  => (dotSize, solidDotGp)
                case Shape.Dot(false) => (dotSize, hollowDotGp)
                case _                => (emptySize, emptyGp)
              at(d.at.x, d.at.y, d.dxPx).flatMap(p =>
                Grob.pointBatch(
                  Vector(p),
                  sizes = BatchColumn.Constant(radius),
                  shapes = BatchColumn.Constant(PointShape.Circle),
                  graphicParams = BatchColumn.Constant(gp),
                  name = Some(name)
                )
              )
        yield (grob, name)
      }
      valueGp  <- font(FontFace.MonoMedium, ThemedToken.Ink)
      valueGp2 <- font(FontFace.MonoMedium, ThemedToken.Ink2)
      values   <- traverseAll(grand.toVector.sortBy(_._1)) { (level, tick) =>
        val solid = level == 0
        source
          .text(tick.row.row, dIndex)
          .fold[Either[GraphicsError, Vector[Grob]]](
            Right(Vector.empty)
          ) { written =>
            at(tick.at.x, tick.at.y, if solid then -ValueGapPx else ValueGapPx).flatMap(p =>
              Grob
                .text(
                  written,
                  p,
                  Anchor(if solid then HJust.Right else HJust.Left, VJust.Center),
                  gp = if solid then valueGp else valueGp2
                )
                .map(Vector(_))
            )
          }
      }
      groupGp    <- font(FontFace.SansSemiBold, ThemedToken.Ink)
      nGp        <- font(FontFace.MonoRegular, ThemedToken.Ink3)
      nY         <- LengthExpr.npc(-0.04)
      labelY     <- LengthExpr.npc(-0.12)
      underneath <- traverseAll(levels.zipWithIndex) { (label, level) =>
        for
          x     <- LengthExpr.native(level.toDouble)
          group <- Grob.text(
            label,
            Point(x, labelY),
            Anchor(HJust.Center, VJust.Top),
            gp = groupGp
          )
          n <- grand
            .get(level)
            .flatMap(tick => source.text(tick.row.row, nIndex))
            .fold[Either[GraphicsError, Vector[Grob]]](Right(Vector.empty))(n =>
              Grob
                .text(
                  ParticipantText(ParticipantTextId.GroupN, n),
                  Point(x, nY),
                  Anchor(HJust.Center, VJust.Top),
                  gp = nGp
                )
                .map(Vector(_))
            )
        yield group +: n
      }
      gutter    <- LengthExpr.npc(-0.015)
      axisGp    <- font(FontFace.MonoRegular, ThemedToken.Ink)
      zeroLabel <- Grob.text(
        ParticipantPlot.ZeroLabel,
        Point(gutter, zeroY),
        Anchor(HJust.Right, VJust.Center),
        gp = axisGp
      )
      bandGp    <- font(FontFace.SansRegular, ThemedToken.Ink3)
      bandLabel <-
        if !frame.hasBand then Right(Vector.empty)
        else
          LengthExpr
            .native(frame.band)
            .flatMap(y =>
              Grob.text(
                ParticipantText(ParticipantTextId.NoValue),
                Point(gutter, y),
                Anchor(HJust.Right, VJust.Center),
                gp = bandGp
              )
            )
            .map(Vector(_))
      titleGp <- font(FontFace.SansRegular, ThemedToken.Ink3)
      titleAt <- Point.npc(0.1, 0.98)
      title   <- Grob.text(
        ParticipantText(ParticipantTextId.DAxis),
        titleAt,
        Anchor(HJust.Left, VJust.Top),
        gp = titleGp
      )
    yield (
      Vector(
        background,
        Grob.group(gridLines :+ zeroRule, viewport = Some(frameVp)),
        Grob.group(linked ++ marked.map(_._1), viewport = Some(viewport)),
        Grob.group(
          values.flatten ++ underneath.flatten ++ (zeroLabel +: bandLabel),
          viewport = Some(frameVp)
        ),
        title
      ),
      viewport,
      marked.map(_._2)
    )

object ParticipantPlot:

  /** A participant's dot: its radius, and the edge of a hollow one, in
    * logical pixels.
    */
  val DotRadiusPx: Double  = 4.0
  val HollowEdgePx: Double = 1.5

  /** The dashed empty ring of a missing mean: larger than a dot, so it
    * never reads as one.
    */
  val EmptyRadiusPx: Double     = 6.0
  val EmptyEdgePx: Double       = 1.5
  val EmptyDash: Vector[Double] = Vector(3.0, 2.0)

  /** A grand mean's tick: 52 px long, 4 px thick, dashed 6/3 when not solid. */
  val TickHalfPx: Double       = 26.0
  val TickPx: Double           = 4.0
  val TickDash: Vector[Double] = Vector(6.0, 3.0)

  /** The gap between a tick's end and its written value. */
  val ValueGapPx: Double = TickHalfPx + 6.0

  /** The zero rule's width, and the opacity of a participant's link. */
  val ZeroRulePx: Double = 1.5
  val LinkAlpha: Double  = 0.45

  /** A participant's nudge across its column: `JitterStepPx` times one of
    * eleven steps from −5 to +5, by its place in the source, shifted by
    * `JitterShift` steps in each further column (the board's spread).
    */
  val JitterStepPx: Double = 2.2
  val JitterShift: Int     = 3

  /** The zero rule's label. */
  val ZeroLabel: String = "0"

  /** The fraction of the D range added above and below it, the gap between
    * the lowest value and the band of missing means, and the least range
    * drawn.
    */
  val Margin: Double   = 0.08
  val BandGap: Double  = 0.12
  val MinRange: Double = 0.2

  /** The prefix of each mark's grob name; the suffix is its row. */
  val MarkPrefix: String = "participant-"

  private val PointsPerPixel = 72.0 / 96.0

  // A length in logical pixels as Intaglio points (96 logical pixels per inch).
  private def px(value: Double): Double = value * PointsPerPixel

  /** The nudge of the participant at place `k`. */
  def jitterPx(k: Int): Double = ((k * 7) % 11 - 5) * JitterStepPx

  /** What a row of the plot's source is, by its ref. */
  enum Role derives CanEqual:
    case Participant(participant: String)
    case Grand

  /** The role of the row of `ref`: a participant's mean in a group, or a
    * group's grand mean.
    */
  def roleOf(ref: StudioRef): Option[Role] = ref match
    case StudioRef.ParticipantSummary(_, _, _, _, p) => Some(Role.Participant(p))
    case StudioRef.GroupCell(_, _, _, _)             => Some(Role.Grand)
    case _                                           => None

  private def participantOf(role: Role): Option[String] = role match
    case Role.Participant(p) => Some(p)
    case Role.Grand          => None

  private def groupIdentity(ref: StudioRef): Option[StudioRef] = ref match
    case StudioRef.GroupCell(_, _, _, _) => Some(ref)
    case other                           => other.parent

  /** The group labels of `source`'s rows, each once, in row order: the
    * plot's columns, left to right.
    */
  def groupLevels(source: PlotSource, group: ColumnId): Vector[String] =
    source.rows.indices.toVector
      .flatMap(i => source.value(i, group).collect { case PlotValue.Text(t) => t })
      .distinct

  /** The D range drawn, `y0` to `y1`: zero and every drawn value with
    * [[Margin]], at least [[MinRange]] wide, and below them, when some mean
    * is missing, the band its rings are drawn on. `grid` holds the D values
    * of the faint rules, at a round step, other than zero.
    */
  final case class Frame(
      y0: Double,
      y1: Double,
      band: Double,
      hasBand: Boolean,
      grid: Vector[Double]
  )

  object Frame:
    def of(values: Vector[Double], missing: Boolean): Frame =
      val lo    = (values :+ 0.0).min
      val hi    = (values :+ 0.0).max
      val range = math.max(hi - lo, MinRange)
      val pad   = range * Margin
      val y1    = hi + pad
      val low   = lo - pad
      val band  = low - range * BandGap
      val y0    = if missing then band - pad else low
      val step  = math.pow(10.0, math.floor(math.log10(range / 2.0)))
      val grid  = (math.ceil(low / step).toLong to math.floor(y1 / step).toLong)
        .filter(_ != 0L)
        .map(_ * step)
        .toVector
      Frame(y0, y1, band, missing, grid)

  /** How a mark is drawn: a dot (filled in the solid group), the empty ring
    * of a missing mean, or a grand mean's tick.
    */
  private[plot] enum Shape derives CanEqual:
    case Dot(solid: Boolean)
    case Tick(solid: Boolean)
    case Empty

  /** A mark to draw: its row, anchor, horizontal nudge, shape and, for a
    * placed participant's dot, whose it is.
    */
  private[plot] final case class Drawn(
      row: MarkedRow,
      at: DataPoint,
      dxPx: Double,
      shape: Shape,
      participant: Option[String]
  ):
    def reachPx: Double = shape match
      case Shape.Dot(true)  => DotRadiusPx
      case Shape.Dot(false) => DotRadiusPx + HollowEdgePx / 2.0
      case Shape.Empty      => EmptyRadiusPx + EmptyEdgePx / 2.0
      case Shape.Tick(_)    => math.hypot(TickHalfPx, TickPx / 2.0)

  private def traverseAll[A, E, B](as: Vector[A])(f: A => Either[E, B]): Either[E, Vector[B]] =
    as.foldLeft[Either[E, Vector[B]]](Right(Vector.empty))((acc, a) =>
      acc.flatMap(bs => f(a).map(bs :+ _))
    )
