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
  PlotSource,
  PlotText,
  PlotTextId,
  PlotValue,
  SlopeColumns,
  SlopeFrame
}
import eyes4s.studio.app.text.{Format, ParticipantText, ParticipantTextId}
import eyes4s.studio.app.tokens.{FontFace, Theme, ThemedToken}
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

/** The Results summary's slopegraphs (overhaul v4, `results.html`): one
  * small multiple per σ on a shared D axis, each participant's means in the
  * groups joined by a faint line and the participants' mean drawn heavy,
  * then "Δ by σ", the contrast at every σ.
  *
  * Its source is [[eyes4s.studio.app.plot.Slopegraphs.source]]. Rows are
  * placed by [[Axis.Panels]] (σ panel, then group) and [[Axis.Numeric]] of
  * D. One participant's means in one panel are one mark of their rows, its
  * line; the grand means of a panel are one mark, the heavy line. A mean the
  * summary does not hold is [[Unplotted]]: missing is never drawn as zero,
  * and a participant with one mean in a panel is a lone dot.
  *
  * The layout is the golden's, in logical pixels from the scene's top left:
  * it does not stretch with the pane. `frame` carries what the source does
  * not: the reported σ, the selection, the thin participants, the groups'
  * short names and Δ at every σ, all computed by the app.
  */
final case class SlopegraphPlot(columns: SlopeColumns, frame: SlopeFrame = SlopeFrame.empty)
    extends PlotBuilder:

  def kind: String = "slopegraphs"

  def build(source: PlotSource, theme: Theme): Either[PlotBuildError, BuiltPlot] =
    import SlopegraphPlot.*
    def need(id: eyes4s.studio.app.plot.ColumnId) =
      source.indexOf(id).toRight(PlotBuildError.MissingColumn(kind, id))
    for
      _  <- need(columns.scale)
      _  <- need(columns.group)
      _  <- need(columns.meanOf)
      di <- need(columns.d)
      _  <- Either.cond(
        source.columns(di).format.numeric,
        (),
        PlotBuildError.NotNumeric(kind, columns.d)
      )
      panels   = levelsOf(source, columns.scale)
      groups   = levelsOf(source, columns.group)
      step     = if groups.size > 1 then 1.0 / (groups.size - 1) else 0.0
      encoding = PositionEncoding(
        Axis.Panels(columns.scale, panels, columns.group, groups, Pitch, step),
        Axis.Numeric(columns.d, AxisScale.Linear)
      )
      roles <- traverseAll(source.rows)(r =>
        ParticipantPlot.roleOf(r.ref).toRight(PlotBuildError.UnexpectedRow(kind, r.ref))
      )
      (drawn, unplotted) = plan(source, encoding, panels, roles)
      bounds             = yBounds(drawn.flatMap(_.points.map(_.y)))
      id <- SceneId(s"studio.plot.$kind.${theme.toString.toLowerCase}").left
        .map(PlotBuildError.Scene(kind, _))
      built <- scene(theme, source, panels, groups, drawn, bounds, di).left
        .map(PlotBuildError.Graphics(kind, "the slopegraphs", _))
      (grobs, viewport, names) = built
      marks <- traverseAll(drawn.zip(names).zipWithIndex) { case ((d, name), order) =>
        PlotMark.of(kind, d.rows, d.points.head, ReachPx, order, name)
      }
      panel     <- DataPanel(id, viewport).left.map(PlotBuildError.Scene(kind, _))
      plotScene <- PlotScene(id, Scene(grobs), panel).left.map(PlotBuildError.Scene(kind, _))
      plot      <- BuiltPlot(
        kind,
        source,
        plotScene,
        ParticipantText(ParticipantTextId.Title),
        PlotText(PlotTextId.PlotKeys, summary(source, panels, groups)),
        encoding,
        marks,
        unplotted
      )
    yield plot

  private def summary(source: PlotSource, panels: Vector[String], groups: Vector[String]) =
    val deltas = frame.deltas
      .map(d =>
        s"${d.scale} ${d.mean.fold(ParticipantText(ParticipantTextId.NoValue))(Format.signed(_, 2))}"
      )
      .mkString(", ")
    ParticipantText(
      ParticipantTextId.SlopeSummary,
      source.caption,
      panels.size.toString,
      groups.size.toString,
      deltas
    )

  // The marks to draw, in order, and the rows set aside: panel by panel,
  // each participant's line in row order and then the panel's mean line.
  private def plan(
      source: PlotSource,
      encoding: PositionEncoding,
      panels: Vector[String],
      roles: Vector[ParticipantPlot.Role]
  ): (Vector[SlopegraphPlot.Drawn], Vector[Unplotted]) =
    import SlopegraphPlot.*
    val rows            = source.rows.indices.toVector
    def panelOf(i: Int) =
      source.indexOf(columns.scale).flatMap(source.text(i, _)).map(panels.indexOf(_))
    val keyed   = rows.map(i => (i, panelOf(i), roles(i)))
    val noPanel = keyed.collect { case (i, None, _) =>
      Unplotted(source.rows(i).ref, i, NoPosition.MissingValue(columns.scale))
    }
    val perPanel = panels.indices.toVector.map { p =>
      val here  = keyed.collect { case (i, Some(`p`), role) => (i, role) }
      val lines = here.map(_._2).distinct.map { role =>
        val its            = here.collect { case (i, `role`) => i }
        val (placed, none) = its.partition(i => encoding.place(source, i).isDefined)
        val missing        =
          none.map(i => Unplotted(source.rows(i).ref, i, NoPosition.MissingValue(columns.d)))
        val mark = Option.when(placed.nonEmpty) {
          val points = placed.flatMap(i => encoding.place(source, i))
          Drawn(
            placed
              .zip(points)
              .map((i, at) => MarkedRow(source.rows(i).ref, i, RowMarking.Placed(at))),
            points,
            p,
            role
          )
        }
        (mark, missing)
      }
      // The panel's mean line is drawn, and focused, after its participants.
      val (grand, people) = lines.partition(_._1.exists(_.role == ParticipantPlot.Role.Grand))
      ((people ++ grand).flatMap(_._1), lines.flatMap(_._2))
    }
    (perPanel.flatMap(_._1), (noPanel ++ perPanel.flatMap(_._2)).sortBy(_.row))

  // The grobs, the data viewport and each drawn mark's grob name, in order.
  private def scene(
      theme: Theme,
      source: PlotSource,
      panels: Vector[String],
      groups: Vector[String],
      drawn: Vector[SlopegraphPlot.Drawn],
      bounds: (Double, Double, Vector[Double]),
      dIndex: Int
  ): Either[GraphicsError, (Vector[Grob], Viewport, Vector[GraphicsName])] =
    import SlopegraphPlot.*
    val (y0, y1, ticks)                  = bounds
    def colour(token: ThemedToken): Rgba = IntaglioColours.themed(theme, token)
    def pt(v: Double)                    = Length.points(px(v)).map(LengthExpr(_))
    // A point `x` px from the scene's left and `y` px down from its top.
    def place(x: Double, y: Double) =
      for
        nx  <- pt(x)
        top <- LengthExpr.npc(1.0)
        dy  <- pt(y)
      yield Point(nx, top - dy)
    def stroke(
        token: ThemedToken,
        widthPx: Double,
        alpha: Double = 1.0,
        lineType: LineType = LineType.Solid
    ) =
      GraphicParams.checked(
        stroke = Some(colour(token)),
        fill = None,
        lineWidth = px(widthPx),
        alpha = alpha,
        lineType = lineType,
        lineWidthUnit = StrokeUnit.Point
      )
    def font(face: FontFace, token: ThemedToken, sizePx: Double) =
      Length
        .points(px(sizePx))
        .flatMap(size =>
          GraphicParams.checked(
            stroke = None,
            fill = Some(colour(token)),
            fontFamily = Some(face.javaFxFamily),
            fontSize = size
          )
        )
    def text(s: String, x: Double, y: Double, h: HJust, gp: GraphicParams) =
      place(x, y).flatMap(p => Grob.text(s, p, Anchor(h, VJust.Center), gp = gp))
    def rule(x0: Double, x1: Double, y: Double, gp: GraphicParams) =
      for
        a <- place(x0, y)
        b <- place(x1, y)
        g <- Grob.segments(Vector((a, b)), gp = gp)
      yield g
    // The D axis in pixels down from the scene's top.
    def yPx(v: Double) = DataTopPx + (y1 - v) / (y1 - y0) * DataHeightPx
    // The participants' x for a group level in panel `p`, in pixels.
    val step                    = if groups.size > 1 then StepPx / (groups.size - 1) else 0.0
    def xPx(p: Int, level: Int) = FirstXPx + p * PitchPx + level * step
    def native(d: DataPoint)    =
      for
        x <- LengthExpr.native(d.x)
        y <- LengthExpr.native(d.y)
      yield Point(x, y)
    val reported   = frame.reported.flatMap(r => Option(panels.indexOf(r)).filter(_ >= 0))
    val shortNames = groups.indices.map(i => frame.shortNames.lift(i).getOrElse(groups(i)))
    val deltaLeft  = DeltaLeftPx(panels.size)
    for
      xScale <- Interval(
        (GridLeftPx - FirstXPx) / StepPx,
        ((panels.size - 1) * PitchPx + GridRightPx - FirstXPx) / StepPx
      )
      yScale   <- Interval(y0, y1)
      gridLeft <- pt(GridLeftPx)
      top      <- LengthExpr.npc(1.0)
      bottom   <- pt(DataTopPx + DataHeightPx)
      width    <- ExtentExpr.points(px((panels.size - 1) * PitchPx + GridRightPx - GridLeftPx))
      height   <- ExtentExpr.points(px(DataHeightPx))
      size   = Size.fromExtents(width, height)
      origin = Point(gridLeft, top - bottom)
      viewport <- Viewport.checked(origin, size, xScale, yScale, Clip.Off)
      whole    <- Size.npc(1.0, 1.0)
      centre   <- Point.npc(0.5, 0.5)
      surface  <- GraphicParams.checked(stroke = None, fill = Some(colour(ThemedToken.Surface)))
      background <- Grob.rect(centre, whole, gp = surface)
      gridGp     <- stroke(ThemedToken.PlotGrid, 1.0)
      zeroGp     <- stroke(ThemedToken.Ink3, 1.0)
      grid <- traverseAll(panels.indices.toVector.flatMap(p => ticks.map(p -> _)))((p, t) =>
        rule(
          GridLeftPx + p * PitchPx,
          GridRightPx + p * PitchPx,
          yPx(t),
          if t == 0.0 then zeroGp else gridGp
        )
      )
      axisGp  <- font(FontFace.SansRegular, ThemedToken.Ink3, 10.0)
      yLabels <- traverseAll(ticks)(t =>
        text(
          if t == 0.0 then "0" else Format.signed(t, 1),
          YLabelRightPx,
          yPx(t),
          HJust.Right,
          axisGp
        )
      )
      titleGp    <- font(FontFace.SansMedium, ThemedToken.Ink, 12.0)
      strongGp   <- font(FontFace.SansSemiBold, ThemedToken.Ink, 12.0)
      reportedGp <- font(FontFace.SansRegular, ThemedToken.Ink3, 11.0)
      titles     <- traverseAll(panels.zipWithIndex) { (label, p) =>
        val x  = GridLeftPx + p * PitchPx + TitleInsetPx
        val on = reported.contains(p)
        for
          t <- text(label, x, TitleYPx, HJust.Left, if on then strongGp else titleGp)
          r <-
            if !on then Right(Vector.empty)
            else
              text(
                ParticipantText(ParticipantTextId.Reported),
                x + label.length * TitleCharPx + 6.0,
                TitleYPx,
                HJust.Left,
                reportedGp
              ).map(Vector(_))
        yield t +: r
      }
      groupLabels <- traverseAll(
        panels.indices.toVector.flatMap(p => shortNames.indices.map(p -> _))
      ) { (p, level) =>
        text(shortNames(level), xPx(p, level), GroupLabelYPx, HJust.Center, axisGp)
      }
      faintGp    <- stroke(ThemedToken.Ink3, 1.0, FaintAlpha)
      dots       <- DashPattern(ThinDash)
      thinGp     <- stroke(ThemedToken.Ink3, 1.0, FaintAlpha, LineType.Custom(dots))
      selectedGp <- stroke(ThemedToken.Ink, SelectedPx, SelectedAlpha)
      selThinGp  <- stroke(ThemedToken.Ink, SelectedPx, SelectedAlpha, LineType.Custom(dots))
      meanGp     <- stroke(ThemedToken.Ink, MeanPx)
      loneGp     <- GraphicParams.checked(stroke = None, fill = Some(colour(ThemedToken.Ink3)))
      loneSize   <- ExtentExpr.points(px(LoneRadiusPx))
      marked     <- traverseAll(drawn.zipWithIndex) { (d, k) =>
        val gp = d.role match
          case ParticipantPlot.Role.Grand          => meanGp
          case ParticipantPlot.Role.Participant(p) =>
            val thin = frame.thin.contains(p)
            if frame.selected.contains(p) then if thin then selThinGp else selectedGp
            else if thin then thinGp
            else faintGp
        for
          name   <- GraphicsName(s"$MarkPrefix$k", "slopegraph mark")
          points <- traverseAll(d.points)(native)
          grob   <-
            if points.size > 1 then Grob.lines(points, gp = gp, name = Some(name))
            else
              Grob.pointBatch(
                points,
                sizes = BatchColumn.Constant(loneSize),
                shapes = BatchColumn.Constant(PointShape.Circle),
                graphicParams = BatchColumn.Constant(loneGp),
                name = Some(name)
              )
        yield (grob, name, d)
      }
      // The selected participant's line above the faint ones, the means on top.
      ordered = marked.sortBy { (_, _, d) =>
        d.role match
          case ParticipantPlot.Role.Grand          => 2
          case ParticipantPlot.Role.Participant(p) =>
            if frame.selected.contains(p) then 1 else 0
      }
      filledGp <- GraphicParams.checked(stroke = None, fill = Some(colour(ThemedToken.Ink)))
      hollowGp <- GraphicParams.checked(
        stroke = Some(colour(ThemedToken.Ink)),
        fill = Some(colour(ThemedToken.Surface)),
        lineWidth = px(2.0),
        lineWidthUnit = StrokeUnit.Point
      )
      meanSize  <- ExtentExpr.points(px(MeanRadiusPx))
      valueGp   <- font(FontFace.SansMedium, ThemedToken.Ink, 11.0)
      meanMarks <- traverseAll(drawn.filter(_.role == ParticipantPlot.Role.Grand)) { d =>
        val last = d.points.size - 1
        traverseAll(d.rows.zip(d.points).zipWithIndex) { case ((row, at), k) =>
          for
            centre <- native(at)
            dot    <- Grob.pointBatch(
              Vector(centre),
              sizes = BatchColumn.Constant(meanSize),
              shapes = BatchColumn.Constant(PointShape.Circle),
              graphicParams = BatchColumn.Constant(if k == 0 then filledGp else hollowGp)
            )
            // The value beside the first and last mean, outside the data viewport.
            x = FirstXPx + (at.x * StepPx)
            label <- source
              .text(row.row, dIndex)
              .filter(_ => k == 0 || k == last)
              .fold[Either[GraphicsError, Vector[Grob]]](Right(Vector.empty))(written =>
                text(
                  written,
                  if k == 0 then x - ValueGapPx else x + ValueGapPx,
                  yPx(at.y),
                  if k == 0 then HJust.Right else HJust.Left,
                  valueGp
                ).map(Vector(_))
              )
          yield (dot, label)
        }
      }
      delta <- deltaPanel(theme, deltaLeft, place, text, rule, stroke, font)
    yield (
      (Vector(background) ++ grid ++ yLabels ++ titles.flatten ++ groupLabels :+
        Grob.group(
          ordered.map(_._1) ++ meanMarks.flatten.map(_._1),
          viewport = Some(viewport)
        )) ++ meanMarks.flatten.flatMap(_._2) ++ delta,
      viewport,
      marked.map(_._2)
    )

  // Δ by σ: the contrast at each σ, with how many participants are above
  // zero, left of a hairline that parts it from the panels.
  private def deltaPanel(
      theme: Theme,
      left: Double,
      place: (Double, Double) => Either[GraphicsError, Point],
      text: (String, Double, Double, HJust, GraphicParams) => Either[GraphicsError, Grob],
      rule: (Double, Double, Double, GraphicParams) => Either[GraphicsError, Grob],
      stroke: (ThemedToken, Double, Double, LineType) => Either[GraphicsError, GraphicParams],
      font: (FontFace, ThemedToken, Double) => Either[GraphicsError, GraphicParams]
  ): Either[GraphicsError, Vector[Grob]] =
    import SlopegraphPlot.*
    val deltas = frame.deltas
    if deltas.isEmpty then Right(Vector.empty)
    else
      val values         = deltas.flatMap(_.mean)
      val (d0, d1)       = deltaBounds(values)
      def yPx(v: Double) = DataTopPx + (d1 - v) / (d1 - d0) * DataHeightPx
      val svg            = left + DeltaInsetPx
      val n              = deltas.size
      def xPx(i: Int)    =
        svg + DeltaFirstPx + (if n > 1 then i * DeltaSpanPx / (n - 1) else 0.0)
      for
        hairline <- stroke(ThemedToken.Hairline, 1.0, 1.0, LineType.Solid)
        divider  <-
          for
            a <- place(left, TitleTopPx)
            b <- place(left, DividerBottomPx)
            g <- Grob.segments(Vector((a, b)), gp = hairline)
          yield g
        titleGp <- font(FontFace.SansMedium, ThemedToken.Ink, 12.0)
        title   <- text(
          ParticipantText(ParticipantTextId.DeltaTitle),
          svg + 24.0,
          TitleYPx,
          HJust.Left,
          titleGp
        )
        zeroGp    <- stroke(ThemedToken.Ink3, 1.0, 1.0, LineType.Solid)
        zero      <- rule(svg + 30.0, svg + DeltaRightPx, yPx(0.0), zeroGp)
        axisGp    <- font(FontFace.SansRegular, ThemedToken.Ink3, 10.0)
        zeroLabel <- text("0", svg + 26.0, yPx(0.0), HJust.Right, axisGp)
        countGp   <- font(FontFace.SansRegular, ThemedToken.Ink3, 9.0)
        counts    <- traverseAll(deltas.zipWithIndex)((d, i) =>
          text(
            ParticipantText(ParticipantTextId.DeltaAbove, d.higher.toString, d.paired.toString),
            xPx(i),
            yPx(0.0) - 9.0,
            HJust.Center,
            countGp
          )
        )
        scales <- traverseAll(deltas.zipWithIndex)((d, i) =>
          text(d.scale, xPx(i), GroupLabelYPx, HJust.Center, axisGp)
        )
        lineGp <- stroke(ThemedToken.Ink, 1.8, 1.0, LineType.Solid)
        placed = deltas.zipWithIndex.flatMap((d, i) => d.mean.map(v => (i, v)))
        points <- traverseAll(placed)((i, v) => place(xPx(i), yPx(v)))
        line   <-
          if points.size < 2 then Right(Vector.empty)
          else Grob.lines(points, gp = lineGp).map(Vector(_))
        dotGp <- GraphicParams.checked(
          stroke = None,
          fill = Some(IntaglioColours.themed(theme, ThemedToken.Ink))
        )
        dotSize <- ExtentExpr.points(px(3.0))
        dots    <-
          if points.isEmpty then Right(Vector.empty)
          else
            Grob
              .pointBatch(
                points,
                sizes = BatchColumn.Constant(dotSize),
                shapes = BatchColumn.Constant(PointShape.Circle),
                graphicParams = BatchColumn.Constant(dotGp)
              )
              .map(Vector(_))
        valueGp <- font(FontFace.SansRegular, ThemedToken.Ink, 10.0)
        labels  <- traverseAll(placed) { (i, v) =>
          val (dx, dy, h) =
            if i == 0 then (-7.0, 0.0, HJust.Right)
            else if i == n - 1 then (7.0, 0.0, HJust.Left)
            else (0.0, -11.0, HJust.Center)
          text(Format.signed(v, 2), xPx(i) + dx, yPx(v) + dy, h, valueGp)
        }
        captionGp <- font(FontFace.SansRegular, ThemedToken.Ink3, 11.0)
        caption   <- text(
          ParticipantText(
            ParticipantTextId.DeltaCaption,
            deltas.map(_.paired).maxOption.getOrElse(0).toString
          ),
          svg + 24.0,
          CaptionYPx,
          HJust.Left,
          captionGp
        )
      yield Vector(divider, title, zero, zeroLabel) ++ counts ++ scales ++ line ++ dots ++
        labels :+ caption

object SlopegraphPlot:

  /** The golden's geometry, in logical pixels from the scene's top left. */
  val TitleTopPx: Double      = 6.0
  val TitleYPx: Double        = 16.0
  val DataTopPx: Double       = 46.0
  val DataHeightPx: Double    = 188.0
  val GroupLabelYPx: Double   = 248.0
  val CaptionYPx: Double      = 272.0
  val DividerBottomPx: Double = 280.0

  /** The scene's natural height: the panels, their labels and Δ's caption. */
  val HeightPx: Double = 286.0

  /** The first participant column, the panel pitch and the distance between
    * a panel's first and last group.
    */
  val FirstXPx: Double = 86.0
  val PitchPx: Double  = 144.0
  val StepPx: Double   = 54.0

  /** A panel's grid, either side of its first group, and its title's inset. */
  val GridLeftPx: Double   = 48.0
  val GridRightPx: Double  = 176.0
  val TitleInsetPx: Double = 2.0
  val TitleCharPx: Double  = 6.5

  /** The D axis's labels, right-aligned left of the first panel. */
  val YLabelRightPx: Double = 36.0

  /** The panels' pitch and step in data units, for [[Axis.Panels]]. */
  val Pitch: Double = PitchPx / StepPx

  /** Δ by σ: its hairline after the last panel, its plot's inset, its first
    * point, the span of its points and its zero rule's right end.
    */
  def DeltaLeftPx(panels: Int): Double = GridRightPx + (panels - 1) * PitchPx + 22.0
  val DeltaInsetPx: Double             = 15.0
  val DeltaFirstPx: Double             = 46.0
  val DeltaSpanPx: Double              = 94.0
  val DeltaRightPx: Double             = 170.0

  /** The lines and marks. */
  val FaintAlpha: Double       = 0.3
  val SelectedAlpha: Double    = 0.85
  val SelectedPx: Double       = 1.4
  val MeanPx: Double           = 2.5
  val MeanRadiusPx: Double     = 3.5
  val LoneRadiusPx: Double     = 2.0
  val ValueGapPx: Double       = 8.0
  val ThinDash: Vector[Double] = Vector(1.5, 2.0)
  val ReachPx: Double          = 6.0

  /** The prefix of each mark's grob name; the suffix is its order. */
  val MarkPrefix: String = "slope-"

  private val PointsPerPixel = 72.0 / 96.0

  // A length in logical pixels as Intaglio points (96 logical pixels per inch).
  private def px(value: Double): Double = value * PointsPerPixel

  /** One mark: its rows, where they are, its panel and whose line it is. */
  final case class Drawn(
      rows: Vector[MarkedRow],
      points: Vector[DataPoint],
      panel: Int,
      role: ParticipantPlot.Role
  ) derives CanEqual

  /** The distinct cells of `column`, in row order. */
  def levelsOf(source: PlotSource, column: eyes4s.studio.app.plot.ColumnId): Vector[String] =
    source.rows.indices.toVector
      .flatMap(i => source.value(i, column).collect { case PlotValue.Text(t) => t })
      .distinct

  /** The D axis: zero and every value, out to the tick past the largest, at
    * a step of 0.2 (0.1 for a range under 0.4), with 0.02 of headroom.
    */
  def yBounds(values: Vector[Double]): (Double, Double, Vector[Double]) =
    val hi    = (values :+ 0.0).max
    val lo    = (values :+ 0.0).min
    val step  = if hi - lo < 0.4 then 0.1 else 0.2
    val top   = math.ceil(hi / step - 1e-9) * step
    val floor = math.floor(lo / step + 1e-9) * step
    val ticks = Iterator
      .iterate(floor)(_ + step)
      .takeWhile(_ <= top + 1e-9)
      .map(t => math.rint(t * 100.0) / 100.0)
      .toVector
    def round(v: Double) = math.rint(v * 100.0) / 100.0
    (round(floor - (if floor < 0.0 then 0.02 else 0.0)), round(top + 0.02), ticks)

  /** Δ's axis: zero and every value, to the next 0.05 past the largest plus
    * one more, as the golden's 0 to +0.25 for a largest Δ of +0.19.
    */
  def deltaBounds(values: Vector[Double]): (Double, Double) =
    val hi               = (values :+ 0.0).max
    val lo               = (values :+ 0.0).min
    def round(v: Double) = math.rint(v * 100.0) / 100.0
    (
      round(math.floor(lo / 0.05 + 1e-9) * 0.05),
      round(math.ceil(hi / 0.05 - 1e-9) * 0.05 + 0.05)
    )

  private def traverseAll[A, E, B](as: Vector[A])(f: A => Either[E, B]): Either[E, Vector[B]] =
    as.foldLeft[Either[E, Vector[B]]](Right(Vector.empty))((acc, a) =>
      acc.flatMap(bs => f(a).map(bs :+ _))
    )
