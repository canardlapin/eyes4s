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
  LadderColumns,
  PlotSource,
  PlotText,
  PlotTextId,
  PlotValue
}
import eyes4s.studio.app.text.{Format, LadderText, LadderTextId}
import eyes4s.studio.app.tokens.{FontFace, Theme, ThemedToken, TypeSize}
import eyes4s.studio.core.backend.{PairDesign, ResultAddress}
import eyes4s.studio.core.selection.StudioRef
import intaglio.{
  Anchor,
  BatchColumn,
  Clip,
  ExtentExpr,
  GraphicParams,
  GraphicsError,
  GraphicsName,
  Grob,
  HJust,
  Interval,
  Length,
  LengthExpr,
  Point,
  PointShape,
  Rgba,
  Scene,
  Size,
  StrokeUnit,
  VJust,
  Viewport,
  YDirection
}

/** The display bins of the scale ladder's histogram fallback (ticket S4.5b;
  * bead decision of 2026-09-28): half-open cosine intervals
  * `[k / PerUnit, (k + 1) / PerUnit)`, the same for every query and scale.
  * A bar's height is the number of shown controls in its bin; no other
  * statistic is derived.
  */
object LadderBins:

  /** Bins per unit of cosine: each is 0.05 wide. */
  val PerUnit: Int = 20

  /** The lower edge of bin `k`, inclusive. */
  def lower(k: Long): Double = k.toDouble / PerUnit

  /** The upper edge of bin `k`, exclusive. */
  def upper(k: Long): Double = lower(k + 1)

  /** The bin of `value`: the `k` with `lower(k) <= value < upper(k)`, exactly
    * as the edges are written, whatever the rounding of `value * PerUnit`.
    */
  def of(value: Double): Long =
    val k = math.floor(value * PerUnit).toLong
    if value < lower(k) then k - 1 else if value >= upper(k) then k + 1 else k

/** The scale ladder of one query (ticket S4.5b; DESIGN_SPEC section 6, the
  * Main board's Contrast group): one row per scale, and on the row's cosine
  * axis every control as a hollow dot, B as a tick, M as a diamond, and D as
  * an ink bar from B to M.
  *
  * Its source is [[eyes4s.studio.app.plot.ScaleLadder.source]]: each value
  * is a row with its own ref, and the builder tells the rows apart by ref.
  * A control pair, the control reduction (B) and the matched pair (M) are
  * placed at their cosine on their scale's row ([[Axis.Numeric]] by
  * [[Axis.Category]] of the scale labels, in source order, first at the
  * top). D is a difference, not a cosine: its row has no cosine, and its bar
  * is a positionless mark drawn between the row's B and M. A row without
  * the value it is drawn by is [[Unplotted]], as a control whose score the
  * backend did not serve.
  *
  * Controls at one scale that fall within [[DodgeFraction]] of the axis of
  * each other are spread in a beeswarm, alternately [[FocusStepPx]] (the
  * `focus` scale's row, or every row when there is no focus) or [[StepPx]]
  * below and above the row, through their marks' pixel nudges. Above
  * [[HistogramAbove]] shown controls, a scale's controls are drawn instead
  * as a histogram of [[LadderBins]]: one bar per occupied bin, an aggregate
  * mark of the controls it represents whose readout names the bin's edges.
  *
  * Marks are ordered, and drawn, row by row: D, B, the controls, then M on
  * top. The plot draws no axis numbers; every number it shows is a row's.
  */
final case class ScaleLadderPlot(columns: LadderColumns, focus: Option[String] = None)
    extends PlotBuilder:

  def kind: String = "scale-ladder"

  def build(source: PlotSource, theme: Theme): Either[PlotBuildError, BuiltPlot] =
    import ScaleLadderPlot.*
    val scaleColumn = columns.scale
    val cosine      = columns.cosine
    for
      _ <- source.indexOf(scaleColumn).toRight(PlotBuildError.MissingColumn(kind, scaleColumn))
      ci <- source.indexOf(cosine).toRight(PlotBuildError.MissingColumn(kind, cosine))
      _ <- Either.cond(
        source.columns(ci).format.numeric,
        (),
        PlotBuildError.NotNumeric(kind, cosine)
      )
      levels   = scaleLevels(source, scaleColumn)
      encoding = PositionEncoding(
        Axis.Numeric(cosine, AxisScale.Linear),
        Axis.Category(scaleColumn, levels)
      )
      roles <- source.rows.traverseRows(r => roleOf(r.ref).toRight(PlotBuildError.UnexpectedRow(kind, r.ref)))
      planned <- plan(source, encoding, levels, roles)
      (drawn, unplotted) = planned
      id <- SceneId(s"studio.plot.$kind.${theme.toString.toLowerCase}").left
        .map(PlotBuildError.Scene(kind, _))
      built <- scene(theme, levels, drawn).left
        .map(PlotBuildError.Graphics(kind, "the scale ladder", _))
      (grobs, viewport, names) = built
      marks <- drawn.zip(names).zipWithIndex.foldLeft[Either[PlotBuildError, Vector[PlotMark]]](
        Right(Vector.empty)
      ) { case (acc, ((d, name), order)) =>
        for
          ms     <- acc
          mark   <- PlotMark.of(kind, d.rows, d.at, d.reachPx, order, name)
          nudged <- mark.nudged(kind, 0.0, d.dyPx)
          said   <- d.summary.fold(Right(nudged))(nudged.summarised(kind, _))
        yield ms :+ said
      }
      panel     <- DataPanel(id, viewport).left.map(PlotBuildError.Scene(kind, _))
      plotScene <- PlotScene(id, Scene(grobs), panel).left.map(PlotBuildError.Scene(kind, _))
      title = LadderText(LadderTextId.Title)
      plot <- BuiltPlot(
        kind,
        source,
        plotScene,
        title,
        PlotText(
          PlotTextId.PlotKeys,
          LadderText(LadderTextId.Summary, source.caption, levels.size.toString)
        ),
        encoding,
        marks,
        unplotted
      )
    yield plot

  // The marks to draw, in order, and the rows set aside.
  private def plan(
      source: PlotSource,
      encoding: PositionEncoding,
      levels: Vector[String],
      roles: Vector[ScaleLadderPlot.Role]
  ): Either[PlotBuildError, (Vector[ScaleLadderPlot.Drawn], Vector[Unplotted])] =
    import ScaleLadderPlot.*
    val cosine = columns.cosine
    val rows   = source.rows.indices.toVector
    def refOf(i: Int) = source.rows(i).ref
    def missing(i: Int, column: ColumnId) = Unplotted(refOf(i), i, NoPosition.MissingValue(column))
    // Each row's scale row, if its scale cell names one.
    val levelOf: Vector[Option[Int]] =
      rows.map(i => Axis.position(encoding.y, source, i).map(_.toInt))
    def placedAt(i: Int): Option[DataPoint] = encoding.place(source, i)
    val noLevel = rows.filter(levelOf(_).isEmpty).map(missing(_, columns.scale))
    val xs      = rows.flatMap(i => placedAt(i).map(_.x))
    val (x0, x1) = domain(xs)
    val dodge   = (x1 - x0) * DodgeFraction
    val perLevel = levels.indices.toVector.map { level =>
      val here = rows.filter(levelOf(_).contains(level))
      def withRole(role: Role) = here.filter(roles(_) == role)
      val step =
        if focus.forall(_ == levels(level)) then FocusStepPx else StepPx
      val means    = withRole(Role.Mean)
      val matched  = withRole(Role.Matched)
      val controls = withRole(Role.Control)
      val contrast = withRole(Role.Contrast)
      val meanAt   = means.flatMap(i => placedAt(i).map(i -> _))
      val matchAt  = matched.flatMap(i => placedAt(i).map(i -> _))
      val shown    = controls.flatMap(i => placedAt(i).map(i -> _))
      val unshown =
        (means ++ matched ++ controls).filter(placedAt(_).isEmpty).map(missing(_, cosine))
      // D is drawn between the row's first B and first M, when both are placed.
      val span = for
        (_, b) <- meanAt.headOption
        (_, m) <- matchAt.headOption
      yield (b.x, m.x)
      val contrasts: Either[PlotBuildError, (Vector[Drawn], Vector[Unplotted])] =
        contrast.foldLeft[Either[PlotBuildError, (Vector[Drawn], Vector[Unplotted])]](
          Right((Vector.empty, Vector.empty))
        ) { case (acc, i) =>
          acc.flatMap { (ds, us) =>
            if source.number(i, cosine).isDefined then
              Left(PlotBuildError.UnexpectedRow(kind, refOf(i)))
            else
              span match
                case Some((b, m)) =>
                  Right(
                    (
                      ds :+ Drawn(
                        Vector(
                          MarkedRow(
                            refOf(i),
                            i,
                            RowMarking.Positionless(NoPosition.MissingValue(cosine))
                          )
                        ),
                        DataPoint((b + m) / 2.0, level.toDouble),
                        0.0,
                        BarReachPx,
                        None,
                        Shape.Bar(b, m)
                      ),
                      us
                    )
                  )
                case None => Right((ds, us :+ missing(i, cosine)))
          }
        }
      contrasts.map { (bars, unbarred) =>
        val ticks = meanAt.map((i, at) =>
          Drawn(
            Vector(MarkedRow(refOf(i), i, RowMarking.Placed(at))),
            at,
            0.0,
            TickReachPx,
            None,
            Shape.Tick
          )
        )
        val diamonds = matchAt.map((i, at) =>
          Drawn(
            Vector(MarkedRow(refOf(i), i, RowMarking.Placed(at))),
            at,
            0.0,
            DiamondReachPx,
            None,
            Shape.Diamond
          )
        )
        val dots =
          if shown.size > HistogramAbove then histogram(source, levels(level), level, shown)
          else
            val lanes = swarm(shown.map((i, at) => (i, at.x)), dodge)
            shown.map((i, at) =>
              Drawn(
                Vector(MarkedRow(refOf(i), i, RowMarking.Placed(at))),
                at,
                lanes.getOrElse(i, 0) * step,
                DotReachPx,
                None,
                Shape.Dot
              )
            )
        (bars ++ ticks ++ dots ++ diamonds, unbarred ++ unshown)
      }
    }
    perLevel
      .foldLeft[Either[PlotBuildError, (Vector[Drawn], Vector[Unplotted])]](
        Right((Vector.empty, noLevel))
      ) { (acc, level) =>
        for
          (ds, us) <- acc
          (d, u)   <- level
        yield (ds ++ d, us ++ u)
      }
      .map((ds, us) => (ds, us.sortBy(_.row)))

  // One bar per occupied bin, in bin order, each standing for its controls.
  private def histogram(
      source: PlotSource,
      label: String,
      level: Int,
      shown: Vector[(Int, DataPoint)]
  ): Vector[ScaleLadderPlot.Drawn] =
    import ScaleLadderPlot.*
    val bins    = shown.groupBy((_, at) => LadderBins.of(at.x)).toVector.sortBy(_._1)
    val tallest = bins.map(_._2.size).maxOption.getOrElse(1)
    bins.map { (k, members) =>
      val heightPx = BarMaxPx * members.size / tallest
      val rows     = members.map((i, _) => MarkedRow(source.rows(i).ref, i, RowMarking.Represented))
      Drawn(
        rows,
        DataPoint((LadderBins.lower(k) + LadderBins.upper(k)) / 2.0, level.toDouble),
        -(BinLiftPx + heightPx / 2.0),
        math.max(heightPx / 2.0, BinReachPx),
        Option.when(rows.size > 1)(
          LadderText(
            LadderTextId.Bin,
            label,
            Format.decimal(LadderBins.lower(k), 2),
            Format.decimal(LadderBins.upper(k), 2)
          )
        ),
        Shape.Bin(LadderBins.lower(k), LadderBins.upper(k), heightPx)
      )
    }

  // The grobs, the data viewport and each drawn mark's grob name, in order.
  private def scene(
      theme: Theme,
      levels: Vector[String],
      drawn: Vector[ScaleLadderPlot.Drawn]
  ): Either[GraphicsError, (Vector[Grob], Viewport, Vector[GraphicsName])] =
    import ScaleLadderPlot.*
    def colour(token: ThemedToken): Rgba = IntaglioColours.themed(theme, token)
    def traverse[A, B](as: Vector[A])(f: A => Either[GraphicsError, B]) =
      as.foldLeft[Either[GraphicsError, Vector[B]]](Right(Vector.empty)) { (acc, a) =>
        acc.flatMap(bs => f(a).map(bs :+ _))
      }
    // A data point moved by logical pixels (y down on the panel).
    def at(x: Double, y: Double, dxPx: Double, dyPx: Double) =
      for
        nx <- LengthExpr.native(x)
        ny <- LengthExpr.native(y)
        dx <- Length.points(px(dxPx))
        dy <- Length.points(px(dyPx))
      yield Point(nx + LengthExpr(dx), ny + LengthExpr(dy))
    def box(x0: Double, x1: Double, y: Double, top: Double, bottom: Double, dx: Double = 0.0) =
      traverse(Vector((x0, top, -dx), (x1, top, dx), (x1, bottom, dx), (x0, bottom, -dx)))(
        (x, dy, ddx) => at(x, y, ddx, dy)
      )
    val allX = drawn.flatMap(d =>
      d.shape match
        case Shape.Bin(lo, hi, _) => Vector(lo, hi)
        case Shape.Bar(b, m)      => Vector(b, m)
        case _                    => Vector(d.at.x)
    )
    val (x0, x1) = domain(allX)
    val rowsN    = math.max(levels.size, 1)
    for
      xScale   <- Interval(x0, x1)
      yScale   <- Interval(-0.5, rowsN - 0.5)
      origin   <- Point.npc(0.1, 0.04)
      size     <- Size.npc(0.87, 0.84)
      viewport <- Viewport.checked(origin, size, xScale, yScale, Clip.On, 0.0, YDirection.Down)
      frameVp  <- Viewport.checked(origin, size, xScale, yScale, Clip.Off, 0.0, YDirection.Down)
      whole    <- Size.npc(1.0, 1.0)
      centre   <- Point.npc(0.5, 0.5)
      surface  <- GraphicParams.checked(stroke = None, fill = Some(colour(ThemedToken.Surface)))
      background <- Grob.rect(centre, whole, gp = surface)
      bandGp     <- GraphicParams.checked(stroke = None, fill = Some(colour(ThemedToken.Surface2)))
      lineGp     <- GraphicParams.checked(
        stroke = Some(colour(ThemedToken.Hairline)),
        fill = None,
        lineWidth = px(1.0),
        lineWidthUnit = StrokeUnit.Point
      )
      ruleGp <- GraphicParams.checked(
        stroke = Some(colour(ThemedToken.Surface3)),
        fill = None,
        lineWidth = px(1.0),
        lineWidthUnit = StrokeUnit.Point
      )
      left  <- LengthExpr.npc(0.0)
      right <- LengthExpr.npc(1.0)
      band <- focus.flatMap(f => Option(levels.indexOf(f)).filter(_ >= 0)) match
        case Some(level) =>
          for
            c    <- LengthExpr.npc(0.5).flatMap(x => LengthExpr.native(level.toDouble).map(Point(x, _)))
            w    <- ExtentExpr.npc(1.0)
            h    <- ExtentExpr.native(1.0)
            grob <- Grob.rect(c, Size.fromExtents(w, h), gp = bandGp)
          yield Vector(grob)
        case None => Right(Vector.empty)
      rules <- traverse(levels.indices.toVector) { level =>
        for
          y     <- LengthExpr.native(level.toDouble)
          below <- LengthExpr.native(level + 0.5)
          centreLine <- Grob.segments(Vector((Point(left, y), Point(right, y))), gp = lineGp)
          rule       <- Grob.segments(Vector((Point(left, below), Point(right, below))), gp = ruleGp)
        yield Vector(rule, centreLine)
      }
      inkGp <- GraphicParams.checked(stroke = None, fill = Some(colour(ThemedToken.Ink)))
      tickGp <- GraphicParams.checked(stroke = None, fill = Some(colour(ThemedToken.Control)))
      dotGp <- GraphicParams.checked(
        stroke = Some(colour(ThemedToken.Control)),
        fill = Some(colour(ThemedToken.Surface)),
        lineWidth = px(DotEdgePx),
        lineWidthUnit = StrokeUnit.Point
      )
      binGp <- GraphicParams.checked(
        stroke = Some(colour(ThemedToken.Control)),
        fill = Some(colour(ThemedToken.ControlSoft)),
        lineWidth = px(1.0),
        lineWidthUnit = StrokeUnit.Point
      )
      diamondGp <- GraphicParams.checked(
        stroke = Some(colour(ThemedToken.Surface)),
        fill = Some(colour(ThemedToken.Match)),
        lineWidth = px(DiamondEdgePx),
        lineWidthUnit = StrokeUnit.Point
      )
      dotSize     <- ExtentExpr.points(px(DotRadiusPx))
      diamondSize <- ExtentExpr.points(px(DiamondRadiusPx))
      marked <- traverse(drawn) { d =>
        val y = d.at.y
        for
          name <- GraphicsName(s"$MarkPrefix${d.rows.head.row}", "scale ladder mark")
          grob <- d.shape match
            case Shape.Bar(b, m) =>
              box(b, m, y, -BarHalfPx, BarHalfPx).flatMap(Grob.polygon(_, gp = inkGp, name = Some(name)))
            case Shape.Tick =>
              box(d.at.x, d.at.x, y, -TickHalfPx, TickHalfPx, TickHalfWidthPx)
                .flatMap(Grob.polygon(_, gp = tickGp, name = Some(name)))
            case Shape.Bin(lo, hi, h) =>
              box(lo, hi, y, -(BinLiftPx + h), -BinLiftPx).flatMap(Grob.polygon(_, gp = binGp, name = Some(name)))
            case Shape.Dot | Shape.Diamond =>
              val (shape, size, gp) =
                if d.shape == Shape.Dot then (PointShape.Circle, dotSize, dotGp)
                else (PointShape.Diamond, diamondSize, diamondGp)
              at(d.at.x, y, 0.0, d.dyPx).flatMap(p =>
                Grob.pointBatch(
                  Vector(p),
                  sizes = BatchColumn.Constant(size),
                  shapes = BatchColumn.Constant(shape),
                  graphicParams = BatchColumn.Constant(gp),
                  name = Some(name)
                )
              )
        yield (grob, name)
      }
      labelSize <- Length.points(px(TypeSize.T11.px.toDouble))
      labelGp   <- GraphicParams.checked(
        stroke = None,
        fill = Some(colour(ThemedToken.Ink3)),
        fontFamily = Some(FontFace.SansRegular.javaFxFamily),
        fontSize = labelSize
      )
      scaleGp <- GraphicParams.checked(
        stroke = None,
        fill = Some(colour(ThemedToken.Ink)),
        fontFamily = Some(FontFace.MonoRegular.javaFxFamily),
        fontSize = labelSize
      )
      gutter <- LengthExpr.npc(-0.02)
      scaleLabels <- traverse(levels.zipWithIndex) { (label, level) =>
        LengthExpr.native(level.toDouble).flatMap(y =>
          Grob.text(label, Point(gutter, y), Anchor(HJust.Right, VJust.Center), gp = scaleGp)
        )
      }
      axisAt    <- Point.npc(0.97, 0.99)
      axisTitle <- Grob.text(
        LadderText(LadderTextId.CosineAxis),
        axisAt,
        Anchor(HJust.Right, VJust.Top),
        gp = labelGp
      )
    yield (
      Vector(
        background,
        Grob.group(band ++ rules.flatten, viewport = Some(frameVp)),
        Grob.group(marked.map(_._1), viewport = Some(viewport)),
        Grob.group(scaleLabels, viewport = Some(frameVp)),
        axisTitle
      ),
      viewport,
      marked.map(_._2)
    )

object ScaleLadderPlot:

  /** Above this many shown controls at one scale, its controls are drawn as
    * a histogram of [[LadderBins]] instead of dots.
    */
  val HistogramAbove: Int = 50

  /** The beeswarm's step on the focus scale's row, and on the others, in
    * logical pixels.
    */
  val FocusStepPx: Double = 7.0
  val StepPx: Double      = 5.0

  /** Controls closer than this fraction of the cosine axis are spread. */
  val DodgeFraction: Double = 0.014

  /** A control dot's radius and edge, in logical pixels (9 px across). */
  val DotRadiusPx: Double = 4.5
  val DotEdgePx: Double   = 1.5

  /** M's diamond: its point radius and its surface-coloured edge. */
  val DiamondRadiusPx: Double = 6.0
  val DiamondEdgePx: Double   = 2.0

  /** B's tick: 3 px wide, 28 px tall. */
  val TickHalfWidthPx: Double = 1.5
  val TickHalfPx: Double      = 14.0

  /** D's bar: 4 px thick. */
  val BarHalfPx: Double = 2.0

  /** The tallest histogram bar of a scale, in logical pixels. */
  val BarMaxPx: Double = 18.0

  /** How far a histogram bar stands above its row's line, clear of D's bar,
    * so a bar never covers the point where D or B is anchored.
    */
  val BinLiftPx: Double = BarHalfPx + 1.0

  /** The fraction of the cosine range added on either side of it. */
  val Margin: Double = 0.04

  /** The prefix of each mark's grob name; the suffix is its first row. */
  val MarkPrefix: String = "ladder-"

  private val DotReachPx: Double     = DotRadiusPx + DotEdgePx / 2.0
  private val DiamondReachPx: Double =
    PointShape.diamondHalfDiagonal(DiamondRadiusPx) + DiamondEdgePx / 2.0
  private val BarReachPx: Double  = 2.0 * BarHalfPx
  private val TickReachPx: Double = math.hypot(TickHalfPx, TickHalfWidthPx)
  private val BinReachPx: Double = 6.0

  private val PointsPerPixel = 72.0 / 96.0

  // A length in logical pixels as Intaglio points (96 logical pixels per inch).
  private def px(value: Double): Double = value * PointsPerPixel

  /** What a row of the ladder's source is, by its ref. */
  enum Role derives CanEqual:
    case Matched, Mean, Contrast, Control

  /** The role of the row of `ref`: M is a matched pair, B the control
    * reduction, D the query's contrast row and a control a control pair.
    */
  def roleOf(ref: StudioRef): Option[Role] = ref match
    case StudioRef.Pair(_, _, PairDesign.Matched, _, _) => Some(Role.Matched)
    case StudioRef.Pair(_, _, PairDesign.Control, _, _) => Some(Role.Control)
    case StudioRef.QueryContrast(_, _, _)               => Some(Role.Contrast)
    case other                                          =>
      other.resultAddress.collect { case ResultAddress.Reduction(_, PairDesign.Control, _) =>
        Role.Mean
      }

  /** The scale labels of `source`'s rows, each once, in row order: the
    * ladder's rows, top to bottom.
    */
  def scaleLevels(source: PlotSource, scale: ColumnId): Vector[String] =
    source.rows.indices.toVector
      .flatMap(i => source.value(i, scale).collect { case PlotValue.Text(t) => t })
      .distinct

  /** The cosine range drawn: 0 to 1 and every drawn value, with [[Margin]]. */
  def domain(values: Vector[Double]): (Double, Double) =
    val lo  = (values :+ 0.0).min
    val hi  = (values :+ 1.0).max
    val pad = (hi - lo) * Margin
    (lo - pad, hi + pad)

  /** The beeswarm lane of each row (0, 1 below, −1 above): in order of
    * value, a control within `dodge` of the one before it takes the next
    * lane, 0 → 1 → −1 → 0; any other starts at 0.
    */
  def swarm(values: Vector[(Int, Double)], dodge: Double): Map[Int, Int] =
    values
      .sortBy((row, v) => (v, row))
      .foldLeft((Map.empty[Int, Int], Option.empty[(Double, Int)])) {
        case ((lanes, last), (row, v)) =>
          val lane = last match
            case Some((prev, l)) if math.abs(v - prev) < dodge =>
              if l == 0 then 1 else if l == 1 then -1 else 0
            case _ => 0
          (lanes.updated(row, lane), Some((v, lane)))
      }
      ._1

  /** How a mark is drawn. */
  private[plot] enum Shape derives CanEqual:
    case Dot, Diamond, Tick
    case Bar(b: Double, m: Double)
    case Bin(lower: Double, upper: Double, heightPx: Double)

  /** A mark to draw: its rows, anchor, vertical nudge, reach, summary and shape. */
  private[plot] final case class Drawn(
      rows: Vector[MarkedRow],
      at: DataPoint,
      dyPx: Double,
      reachPx: Double,
      summary: Option[String],
      shape: Shape
  )

  extension [A](as: Vector[A])
    private def traverseRows[E, B](f: A => Either[E, B]): Either[E, Vector[B]] =
      as.foldLeft[Either[E, Vector[B]]](Right(Vector.empty))((acc, a) => acc.flatMap(bs => f(a).map(bs :+ _)))
