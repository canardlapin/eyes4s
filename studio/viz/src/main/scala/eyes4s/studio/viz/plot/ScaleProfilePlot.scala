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
  PlotSource,
  PlotText,
  PlotTextId,
  PlotValue,
  ProfileColumns
}
import eyes4s.studio.app.text.{ProfileText, ProfileTextId}
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

/** The scale profile plot (ticket S4.5d; the Results board's scale profile
  * and the Figures board's panel E): mean D against the Gaussian σ of each
  * declared scale, on a log-spaced σ axis, with each participant's means
  * as one faint line and each group's grand means as bold dots joined by a
  * bold line, over a zero rule.
  *
  * Its source is [[eyes4s.studio.app.plot.ScaleProfile.source]]: a group's
  * grand mean at a scale is a [[StudioRef.ReportCell]] row and a
  * participant's mean a [[StudioRef.ReportParticipant]] row; legacy refs
  * remain accepted. The builder
  * tells series apart by ref. Rows are placed by [[Axis.Numeric]] of σ on
  * [[AxisScale.Log10]] by [[Axis.Numeric]] of D, so scales that double are
  * evenly spaced.
  *
  * A participant's line is one mark of all its rows: the rows with a value
  * are placed, any other is positionless and breaks the line; a participant
  * with no placed row is set aside, row by row. A group's grand mean at a
  * scale is one dot, and is set aside when it has no value. The first group
  * is drawn solid (filled ink dots, an ink line) and the others dashed
  * (hollow ink-3 dots, a dashed ink-3 line).
  *
  * Every number the plot writes is a row's cell as the table writes it:
  * each grand mean beside its dot and each scale's label under its σ. The
  * only axis label is the zero rule's. Marks are ordered, and drawn, with
  * the participants' lines first and then each group's dots in scale order.
  */
final case class ScaleProfilePlot(columns: ProfileColumns) extends PlotBuilder:

  def kind: String = "scale-profile"

  def build(source: PlotSource, theme: Theme): Either[PlotBuildError, BuiltPlot] =
    import ScaleProfilePlot.*
    def numeric(id: ColumnId) =
      for
        i <- source.indexOf(id).toRight(PlotBuildError.MissingColumn(kind, id))
        _ <- Either.cond(
          source.columns(i).format.numeric,
          (),
          PlotBuildError.NotNumeric(kind, id)
        )
      yield i
    val encoding = PositionEncoding(
      Axis.Numeric(columns.sigma, AxisScale.Log10),
      Axis.Numeric(columns.d, AxisScale.Linear)
    )
    for
      _      <- numeric(columns.sigma)
      di     <- numeric(columns.d)
      scaleI <- source
        .indexOf(columns.scale)
        .toRight(PlotBuildError.MissingColumn(kind, columns.scale))
      series <- traverseAll(source.rows)(r =>
        seriesOf(r.ref).toRight(PlotBuildError.UnexpectedRow(kind, r.ref))
      )
      (drawn, unplotted, meanRuns) = plan(source, encoding, series)
      id <- SceneId(s"studio.plot.$kind.${theme.toString.toLowerCase}").left
        .map(PlotBuildError.Scene(kind, _))
      built <- scene(theme, source, drawn, meanRuns, di, scaleI).left
        .map(PlotBuildError.Graphics(kind, "the scale profile", _))
      (grobs, viewport, names) = built
      marks <- traverseAll(drawn.zip(names).zipWithIndex) { case ((d, name), order) =>
        PlotMark
          .of(kind, d.rows, d.at, d.reachPx, order, name)
          .map(m => if d.shape == Shape.Line then m.withLineRuns(d.runs) else m)
      }
      panel     <- DataPanel(id, viewport).left.map(PlotBuildError.Scene(kind, _))
      plotScene <- PlotScene(id, Scene(grobs), panel).left.map(PlotBuildError.Scene(kind, _))
      plot      <- BuiltPlot(
        kind,
        source,
        plotScene,
        ProfileText(ProfileTextId.Title),
        PlotText(
          PlotTextId.PlotKeys,
          ProfileText(
            if series.contains(Series.WholeReport) then ProfileTextId.OverallSummary
            else ProfileTextId.Summary,
            source.caption,
            levelsOf(source, columns.scale).size.toString
          )
        ),
        encoding,
        marks,
        unplotted
      )
    yield plot

  // The marks to draw, in order, the rows set aside, and each group's bold
  // line as its contiguous placed runs (solid for the first group).
  private def plan(
      source: PlotSource,
      encoding: PositionEncoding,
      series: Vector[ScaleProfilePlot.Series]
  ): (
      Vector[ScaleProfilePlot.Drawn],
      Vector[Unplotted],
      Vector[(Boolean, Vector[Vector[DataPoint]])]
  ) =
    import ScaleProfilePlot.*
    val rows          = source.rows.indices.toVector
    def refOf(i: Int) = source.rows(i).ref
    val placed        = rows.flatMap(i => encoding.place(source, i).map(i -> _)).toMap
    // Why a row has no position: its σ, then its D.
    def reason(i: Int): NoPosition =
      source.value(i, columns.sigma) match
        case Some(PlotValue.Number(v)) if v <= 0.0 => NoPosition.OffScale(columns.sigma, v)
        case Some(PlotValue.Number(_))             => NoPosition.MissingValue(columns.d)
        case _                                     => NoPosition.MissingValue(columns.sigma)
    val order            = series.distinct
    val (groups, people) = order.partition {
      case Series.Group(_) | Series.WholeReport => true
      case _                                    => false
    }
    val lines = people.map { s =>
      val members = rows.filter(series(_) == s)
      members.flatMap(placed.get).headOption match
        case Some(first) =>
          val marked = members.map(i =>
            MarkedRow(
              refOf(i),
              i,
              placed.get(i).fold(RowMarking.Positionless(reason(i)))(RowMarking.Placed(_))
            )
          )
          val line = runs(members.map(placed.get))
          (Vector(Drawn(marked, first, Shape.Line, s, line)), Vector.empty)
        case None => (Vector.empty, members.map(i => Unplotted(refOf(i), i, reason(i))))
    }
    val dots = groups.zipWithIndex.map { (s, k) =>
      val members = rows.filter(series(_) == s).sortBy(i => placed.get(i).map(_.x))
      val solid   = k == 0
      (
        members.flatMap(i =>
          placed
            .get(i)
            .map(at =>
              Drawn(
                Vector(MarkedRow(refOf(i), i, RowMarking.Placed(at))),
                at,
                Shape.Dot(solid),
                s
              )
            )
        ),
        members.filterNot(placed.contains).map(i => Unplotted(refOf(i), i, reason(i)))
      )
    }
    val means = groups.zipWithIndex.map((s, k) =>
      (k == 0, runs(rows.filter(series(_) == s).map(placed.get)))
    )
    val all = lines ++ dots
    (all.flatMap(_._1), all.flatMap(_._2).sortBy(_.row), means)

  // The grobs, the data viewport and each drawn mark's grob name, in order.
  private def scene(
      theme: Theme,
      source: PlotSource,
      drawn: Vector[ScaleProfilePlot.Drawn],
      meanRuns: Vector[(Boolean, Vector[Vector[DataPoint]])],
      dIndex: Int,
      scaleIndex: Int
  ): Either[GraphicsError, (Vector[Grob], Viewport, Vector[GraphicsName])] =
    import ScaleProfilePlot.*
    def colour(token: ThemedToken): Rgba     = IntaglioColours.themed(theme, token)
    def at(p: DataPoint, dyPx: Double = 0.0) =
      for
        nx <- LengthExpr.native(p.x)
        ny <- LengthExpr.native(p.y)
        dy <- Length.points(px(dyPx))
      yield Point(nx, ny + LengthExpr(dy))
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
    def stroke(
        token: ThemedToken,
        widthPx: Double,
        lineType: LineType = LineType.Solid,
        alpha: Double = 1.0
    ) =
      GraphicParams.checked(
        stroke = Some(colour(token)),
        fill = None,
        lineWidth = px(widthPx),
        lineType = lineType,
        alpha = alpha,
        lineWidthUnit = StrokeUnit.Point
      )
    def placedOf(d: Drawn) =
      d.rows.collect { case MarkedRow(_, _, RowMarking.Placed(p)) => p }.sortBy(_.x)
    val points = drawn.flatMap(placedOf)
    val frame  = ParticipantPlot.Frame.of(points.map(_.y), false)
    // Every σ the source declares on the scale, once, with the first row
    // whose scale label names it, whether or not any value is drawn there.
    val ticks = source.rows.indices.toVector
      .flatMap(i =>
        source.number(i, columns.sigma).filter(_ > 0.0).map(v => math.log10(v) -> i)
      )
      .groupMapReduce(_._1)(_._2)((a, _) => a)
      .toVector
      .sortBy(_._1)
    val (x0, x1) = xDomain(points.map(_.x) ++ ticks.map(_._1))
    val dots     = drawn.collect { case d @ Drawn(_, _, Shape.Dot(_), _, _) => d }
    for
      xScale   <- Interval(x0, x1)
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
      bottom     <- LengthExpr.npc(0.0)
      top        <- LengthExpr.npc(1.0)
      gridGp     <- stroke(ThemedToken.Surface3, 1.0)
      hGrid      <- traverseAll(frame.grid)(y =>
        LengthExpr.native(y).map(ny => (Point(left, ny), Point(right, ny)))
      )
      vGrid <- traverseAll(ticks.map(_._1))(x =>
        LengthExpr.native(x).map(nx => (Point(nx, bottom), Point(nx, top)))
      )
      gridLines <-
        if hGrid.isEmpty && vGrid.isEmpty then Right(Vector.empty)
        else Grob.segments(hGrid ++ vGrid, gp = gridGp).map(Vector(_))
      zeroGp   <- stroke(ThemedToken.Ink, ZeroRulePx)
      zeroY    <- LengthExpr.native(0.0)
      zeroRule <- Grob.segments(Vector((Point(left, zeroY), Point(right, zeroY))), gp = zeroGp)
      lineGp   <- stroke(ThemedToken.Ink3, LinePx, alpha = LineAlpha)
      soloGp   <- GraphicParams.checked(
        stroke = None,
        fill = Some(colour(ThemedToken.Ink3)),
        alpha = LineAlpha
      )
      meanDash    <- DashPattern(MeanDash)
      solidMeanGp <- stroke(ThemedToken.Ink, MeanLinePx)
      dashMeanGp  <- stroke(ThemedToken.Ink3, MeanLinePx, LineType.Custom(meanDash))
      means       <- traverseAll(
        meanRuns.flatMap((solid, rs) => rs.filter(_.size > 1).map(solid -> _))
      ) { (solid, ps) =>
        traverseAll(ps)(at(_))
          .flatMap(Grob.lines(_, gp = if solid then solidMeanGp else dashMeanGp))
      }
      solidDotGp  <- GraphicParams.checked(stroke = None, fill = Some(colour(ThemedToken.Ink)))
      hollowDotGp <- GraphicParams.checked(
        stroke = Some(colour(ThemedToken.Ink3)),
        fill = Some(colour(ThemedToken.Surface)),
        lineWidth = px(HollowEdgePx),
        lineWidthUnit = StrokeUnit.Point
      )
      dotSize  <- ExtentExpr.points(px(DotRadiusPx))
      soloSize <- ExtentExpr.points(px(LinePx * 1.5))
      marked   <- traverseAll(drawn) { d =>
        def batch(p: Point, size: ExtentExpr, gp: GraphicParams, name: Option[GraphicsName]) =
          Grob.pointBatch(
            Vector(p),
            sizes = BatchColumn.Constant(size),
            shapes = BatchColumn.Constant(PointShape.Circle),
            graphicParams = BatchColumn.Constant(gp),
            name = name
          )
        for
          name <- GraphicsName(s"$MarkPrefix${d.rows.head.row}", "scale profile mark")
          grob <- d.shape match
            case Shape.Line =>
              // One unnamed piece per contiguous run, under the mark's name:
              // a missing mean breaks the line rather than being bridged.
              traverseAll(d.runs)(run =>
                traverseAll(run)(at(_)).flatMap {
                  case Vector(one) => batch(one, soloSize, soloGp, None)
                  case ps          => Grob.lines(ps, gp = lineGp)
                }
              ).map(pieces => Grob.group(pieces, name = Some(name)))
            case Shape.Dot(solid) =>
              at(d.at).flatMap(
                batch(_, dotSize, if solid then solidDotGp else hollowDotGp, Some(name))
              )
        yield (grob, name)
      }
      valueGp  <- font(FontFace.MonoMedium, ThemedToken.Ink)
      valueGp2 <- font(FontFace.MonoMedium, ThemedToken.Ink2)
      values   <- traverseAll(dots) { d =>
        val solid = d.shape == Shape.Dot(true)
        source
          .text(d.rows.head.row, dIndex)
          .fold[Either[GraphicsError, Vector[Grob]]](Right(Vector.empty)) { written =>
            at(d.at, if solid then ValueGapPx else -ValueGapPx).flatMap(p =>
              Grob
                .text(
                  written,
                  p,
                  Anchor(HJust.Center, if solid then VJust.Bottom else VJust.Top),
                  gp = if solid then valueGp else valueGp2
                )
                .map(Vector(_))
            )
          }
      }
      scaleGp <- font(FontFace.MonoRegular, ThemedToken.Ink2)
      tickY   <- LengthExpr.npc(-0.04)
      labels  <- traverseAll(ticks) { (x, row) =>
        source
          .text(row, scaleIndex)
          .fold[Either[GraphicsError, Vector[Grob]]](Right(Vector.empty)) { label =>
            LengthExpr
              .native(x)
              .flatMap(nx =>
                Grob
                  .text(label, Point(nx, tickY), Anchor(HJust.Center, VJust.Top), gp = scaleGp)
              )
              .map(Vector(_))
          }
      }
      gutter    <- LengthExpr.npc(-0.015)
      axisGp    <- font(FontFace.MonoRegular, ThemedToken.Ink)
      zeroLabel <- Grob.text(
        ParticipantPlot.ZeroLabel,
        Point(gutter, zeroY),
        Anchor(HJust.Right, VJust.Center),
        gp = axisGp
      )
      titleGp <- font(FontFace.SansRegular, ThemedToken.Ink3)
      yAt     <- Point.npc(0.1, 0.98)
      yTitle  <- Grob.text(
        ProfileText(ProfileTextId.DAxis),
        yAt,
        Anchor(HJust.Left, VJust.Top),
        gp = titleGp
      )
      xAt    <- Point.npc(0.53, 0.02)
      xTitle <- Grob.text(
        ProfileText(ProfileTextId.SigmaAxis),
        xAt,
        Anchor(HJust.Center, VJust.Bottom),
        gp = titleGp
      )
    yield (
      Vector(
        background,
        Grob.group(gridLines :+ zeroRule, viewport = Some(frameVp)),
        // Participants' faint lines first, then the bold means on top.
        Grob.group(
          drawn.zip(marked).collect {
            case (d, (g, _)) if d.shape == Shape.Line => g
          } ++ means ++
            drawn.zip(marked).collect { case (d, (g, _)) if d.shape != Shape.Line => g },
          viewport = Some(viewport)
        ),
        Grob.group(values.flatten ++ labels.flatten :+ zeroLabel, viewport = Some(frameVp)),
        yTitle,
        xTitle
      ),
      viewport,
      marked.map(_._2)
    )

object ScaleProfilePlot:

  /** A group's grand-mean dot: its radius, and the edge of a hollow one. */
  val DotRadiusPx: Double  = 4.5
  val HollowEdgePx: Double = 1.5

  /** A group's bold line, dashed 6/3 when not solid. */
  val MeanLinePx: Double       = 3.0
  val MeanDash: Vector[Double] = Vector(6.0, 3.0)

  /** A participant's faint line: its width and opacity. */
  val LinePx: Double    = 1.0
  val LineAlpha: Double = 0.28

  /** The zero rule's width. */
  val ZeroRulePx: Double = 1.5

  /** How far a grand mean's written value stands from its dot. */
  val ValueGapPx: Double = 9.0

  /** The fraction of the log σ range added on either side of it, and the
    * half-width drawn around a single σ, in decades.
    */
  val Margin: Double     = 0.12
  val SingleHalf: Double = 0.3

  /** The prefix of each mark's grob name; the suffix is its first row. */
  val MarkPrefix: String = "profile-"

  private val PointsPerPixel = 72.0 / 96.0

  // A length in logical pixels as Intaglio points (96 logical pixels per inch).
  private def px(value: Double): Double = value * PointsPerPixel

  /** Which series a row belongs to, by its ref. */
  enum Series derives CanEqual:
    case WholeReport
    case Group(label: String)
    case Participant(participant: String, group: Option[String])

  /** The series of the row of `ref`: a group's grand means or one
    * participant's means.
    */
  def seriesOf(ref: StudioRef): Option[Series] = ref match
    case StudioRef.GroupCell(_, _, _, g)             => Some(Series.Group(g.label))
    case StudioRef.ParticipantSummary(_, _, _, g, p) =>
      Some(Series.Participant(p, g.map(_.label)))
    case StudioRef.ReportCell(
          _,
          _,
          _,
          eyes4s.studio.core.selection.ReportGroup.Whole,
          eyes4s.studio.core.backend.ReportRole.Difference
        ) =>
      Some(Series.WholeReport)
    case StudioRef.ReportCell(
          _,
          _,
          _,
          eyes4s.studio.core.selection.ReportGroup.Level(g),
          eyes4s.studio.core.backend.ReportRole.Difference
        ) =>
      Some(Series.Group(g.label))
    case StudioRef.ReportParticipant(
          _,
          _,
          _,
          g,
          eyes4s.studio.core.backend.ReportRole.Difference,
          p
        ) =>
      val group = g match
        case eyes4s.studio.core.selection.ReportGroup.Level(value) => Some(value.label)
        case eyes4s.studio.core.selection.ReportGroup.Whole        => None
      Some(Series.Participant(p, group))
    case _ => None

  /** The scale labels of `source`'s rows, each once, in row order. */
  def levelsOf(source: PlotSource, scale: ColumnId): Vector[String] =
    source.rows.indices.toVector
      .flatMap(i => source.value(i, scale).collect { case PlotValue.Text(t) => t })
      .distinct

  /** The log10 σ range drawn: every placed σ with [[Margin]] either side. */
  def xDomain(xs: Vector[Double]): (Double, Double) =
    if xs.isEmpty then (-SingleHalf, SingleHalf)
    else
      val lo = xs.min
      val hi = xs.max
      if hi > lo then
        val pad = (hi - lo) * Margin
        (lo - pad, hi + pad)
      else (lo - SingleHalf, hi + SingleHalf)

  /** The contiguous runs of placed points of a series in scale order: a
    * point without a position ends a run, so no line bridges a missing value.
    */
  def runs(points: Vector[Option[DataPoint]]): Vector[Vector[DataPoint]] =
    points
      .foldLeft(Vector(Vector.empty[DataPoint])) {
        case (acc, Some(p)) => acc.init :+ (acc.last :+ p)
        case (acc, None)    => if acc.last.isEmpty then acc else acc :+ Vector.empty
      }
      .filter(_.nonEmpty)

  /** How a mark is drawn: a participant's line or a group's dot. */
  private[plot] enum Shape derives CanEqual:
    case Line
    case Dot(solid: Boolean)

  /** A mark to draw: its rows, anchor, shape and series, and for a line its
    * contiguous placed runs.
    */
  private[plot] final case class Drawn(
      rows: Vector[MarkedRow],
      at: DataPoint,
      shape: Shape,
      series: Series,
      runs: Vector[Vector[DataPoint]] = Vector.empty
  ):
    def reachPx: Double = shape match
      case Shape.Line       => LinePx * 1.5 + 1.0
      case Shape.Dot(true)  => DotRadiusPx
      case Shape.Dot(false) => DotRadiusPx + HollowEdgePx / 2.0

  private def traverseAll[A, E, B](as: Vector[A])(f: A => Either[E, B]): Either[E, Vector[B]] =
    as.foldLeft[Either[E, Vector[B]]](Right(Vector.empty))((acc, a) =>
      acc.flatMap(bs => f(a).map(bs :+ _))
    )
