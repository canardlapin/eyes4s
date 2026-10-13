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

package eyes4s.studio.viz.figure

import eyes4s.studio.app.figures.*
import eyes4s.studio.app.plot.{ParticipantColumns, PlotSourceError, ProfileColumns}
import cats.syntax.all.*
import eyes4s.studio.app.tokens.{FontFace, PaletteToken, StageVariant, Theme, Tokens}
import eyes4s.studio.core.assets.AssetRef
import eyes4s.studio.core.backend.PairDesign
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.viz.maps.MapTileScene
import eyes4s.studio.viz.trial.{StimulusRaster, TrialScene, TrialSceneError}
import eyes4s.studio.viz.plot.{
  IntaglioColours,
  ParticipantPlot,
  PlotBuildError,
  PlotBuilder,
  PlotSceneError,
  ReferenceRole,
  ScaleProfilePlot
}
import intaglio.{
  Anchor,
  ExtentExpr,
  GraphicParams,
  GraphicsError,
  Grob,
  GrobMeta,
  GraphicsName,
  DataKey,
  PlotSemantics,
  SceneSemantics,
  SemanticId,
  HJust,
  Length,
  LengthExpr,
  LengthUnit,
  Point,
  RenderContext,
  RenderPlan,
  Rgba,
  Scene,
  Size,
  StrokeUnit,
  VJust,
  Viewport
}

/** Why a figure page could not be laid out for export. Every case names
  * what failed.
  */
enum FigurePageError derives CanEqual:
  case Columns(error: PlotSourceError)

  /** Panel `panel`'s plot could not be built. */
  case Plot(panel: String, error: PlotBuildError)

  /** Panel `panel`'s trial could not be drawn. */
  case Trial(panel: String, error: TrialSceneError)

  /** Panel `panel`'s density tile for `trial` could not be drawn. */
  case Map(panel: String, trial: String, error: PlotSceneError)

  /** Intaglio refused a value while `during`. */
  case Graphics(during: String, error: GraphicsError)

  def message: String = this match
    case Columns(e)           => e.message
    case Plot(panel, e)       => s"Panel $panel could not be drawn: ${e.message}"
    case Trial(panel, e)      => s"Panel $panel's trial could not be drawn: ${e.message}"
    case Map(panel, trial, e) =>
      s"Panel $panel's density tile for $trial could not be drawn: ${e.message}"
    case Graphics(during, e) => s"Intaglio refused $during: ${e.message}"

/** A figure page laid out for a target (ticket S9.3): one Intaglio scene of
  * the whole page in millimetres, rebuilt from the page's view-model, never
  * a screenshot. Its plots are the Compare plots, built light on paper.
  */
final case class FigurePage private (scene: Scene, widthMm: Double, heightMm: Double):

  /** The page rendered at `pixelsPerInch`: its size in whole device pixels. */
  def context(pixelsPerInch: Double): Either[FigurePageError, RenderContext] =
    RenderContext(
      width = FigurePage.pixels(widthMm, pixelsPerInch),
      height = FigurePage.pixels(heightMm, pixelsPerInch),
      pixelsPerInch = pixelsPerInch
    ).left.map(FigurePageError.Graphics("the page's render context", _))

  def plan(pixelsPerInch: Double): Either[FigurePageError, RenderPlan] =
    context(pixelsPerInch).map(RenderPlan(scene, _))

object FigurePage:
  val MillimetresPerInch: Double = 25.4

  /** A line of text is this many times its size apart from the next. */
  val LineSpacing: Double = 1.25

  /** The average glyph advance of the body face, in ems, for line breaking. */
  val AverageAdvanceEm: Double = 0.52

  /** The space inside a panel C tile, in millimetres. */
  val TilePaddingMm: Double = 1.0

  /** A panel C tile's rows below its map: the subtitle, then the score label.
    * With the one title row above the map, every tile has the same rows, so
    * every map in a row shares one top and every label one baseline.
    */
  val TileCaptionRows: Int = 2

  /** What ends a tile's row that was cut to fit. */
  val Elision: String = "…"

  /** The role glyph's diameter, in ems of the body face. IBM Plex has no
    * ● ◆ ○ (U+25CF, U+25C6, U+25CB), so the glyph is drawn as a shape.
    */
  val GlyphEm: Double = 0.72

  /** The space between the role glyph and the title, in ems. */
  val GlyphGapEm: Double = 0.45

  /** The control glyph's ring width, in points. */
  val GlyphRingPt: Double = 1.0

  /** The name of the role glyph a tile of `role` draws. */
  def glyphName(role: ReferenceRole): String = role match
    case ReferenceRole.Query   => "figure-tile-glyph-query"
    case ReferenceRole.Match   => "figure-tile-glyph-matched"
    case ReferenceRole.Control => "figure-tile-glyph-control"

  /** The role of a panel C tile, read from what it refers to: the query
    * contrast, its matched pair or a control pair.
    */
  def roleOf(tile: MapTileVM): Option[ReferenceRole] = tile.ref match
    case StudioRef.QueryContrast(_, _, _)               => Some(ReferenceRole.Query)
    case StudioRef.Pair(_, _, PairDesign.Matched, _, _) => Some(ReferenceRole.Match)
    case StudioRef.Pair(_, _, PairDesign.Control, _, _) => Some(ReferenceRole.Control)
    case _                                              => None

  def pixels(mm: Double, pixelsPerInch: Double): Int =
    math.max(1, math.ceil(mm / MillimetresPerInch * pixelsPerInch - 1e-9).toInt)

  private def ptMm(pt: Double): Double = pt * FigureType.MillimetresPerPoint

  private def lineMm(pt: Double): Double = ptMm(pt) * LineSpacing

  /** How many glyphs of the body face fit in `widthMm` at `pt`, at least one. */
  private def perLine(widthMm: Double, pt: Double): Int =
    math.max(1, (widthMm / (ptMm(pt) * AverageAdvanceEm)).toInt)

  /** `text` on one row no wider than `widthMm` at `pt`, never wrapped: as it
    * is when its code points fit the row's glyphs (as [[wrap]] counts them),
    * otherwise its first glyphs − 1 code points and [[Elision]]. Code points,
    * so a "·", "σ" or "−" counts one glyph.
    */
  def fit(text: String, widthMm: Double, pt: Double): String =
    val n = perLine(widthMm, pt)
    if text.codePointCount(0, text.length) <= n then text
    else text.substring(0, text.offsetByCodePoints(0, n - 1)) + Elision

  /** `text` broken into lines no wider than `widthMm` at `pt`, at word
    * boundaries (a word longer than a line is its own line).
    */
  def wrap(text: String, widthMm: Double, pt: Double): Vector[String] =
    val perLine = this.perLine(widthMm, pt)
    text.split(" ").toVector.filter(_.nonEmpty).foldLeft(Vector.empty[String]) {
      (lines, word) =>
        lines.lastOption match
          case Some(last) if last.length + 1 + word.length <= perLine =>
            lines.init :+ s"$last $word"
          case _ => lines :+ word
    }

  // --- Layout --------------------------------------------------------------------

  /** What a laid-out panel draws, at its top-left corner (mm, y down). */
  private final case class Placed(panel: PanelVM, x: Double, y: Double, height: Double)

  private def plotHeight(widthMm: Double): Double = widthMm * PlotGeometry.Aspect

  private def gazeHeight(vm: GazePanelVM, widthMm: Double): Double =
    vm.drawn.fold(0.0)(d => widthMm * FigureGaze.heightRatio(d))

  private def bodyHeight(panel: PanelVM, pt: Double): Double =
    val w                            = panel.widthMm.toDouble
    def lines(texts: Vector[String]) = texts.map(wrap(_, w, pt).size).sum * lineMm(pt)
    panel.body match
      case PanelBody.Plot(vm) => plotHeight(w) + lines(vm.notes)
      case PanelBody.Maps(vm) => tileHeight(vm, w, pt) + lines(Vector(vm.caption, vm.maps))
      case PanelBody.Gaze(vm) =>
        gazeHeight(vm, w) + lines(Vector(vm.heading, vm.displayed, vm.gaze))
      case PanelBody.Waiting(why)     => lines(Vector(why))
      case PanelBody.Unavailable(why) => lines(Vector(why))

  /** Panel C's tiles share the panel's width. */
  private def tileWidth(vm: DensityMapsVM, panelMm: Double): Double =
    val count = math.max(1, vm.tiles.size)
    (panelMm - (count - 1) * TilePaddingMm) / count

  private def tileMapWidth(vm: DensityMapsVM, panelMm: Double): Double =
    tileWidth(vm, panelMm) - 2 * TilePaddingMm

  private def tileMapHeight(tile: MapTileVM, mapWidthMm: Double): Double = tile.map match
    case TileMap.Drawn(_, _, region) => mapWidthMm * region.height / region.width
    case _                           => 0.0

  /** The tallest served map fixes the row's height; each map retains its own
    * recorded region aspect inside that row.
    */
  private def tileMapHeight(vm: DensityMapsVM, panelMm: Double): Double =
    val mapWidth = tileMapWidth(vm, panelMm)
    vm.tiles.map(tileMapHeight(_, mapWidth)).maxOption.getOrElse(0.0)

  /** The title row, the map row and the caption rows, padded above the
    * title, between the map and its caption, and below the caption.
    */
  private def tileHeight(vm: DensityMapsVM, panelMm: Double, pt: Double): Double =
    (1 + TileCaptionRows) * lineMm(pt) + tileMapHeight(vm, panelMm) + 3 * TilePaddingMm

  private def panelHeight(panel: PanelVM, pt: Double): Double =
    lineMm(FigureType.LetterPt) + bodyHeight(panel, pt)

  /** The panels in rows across the page, each row as tall as its tallest. */
  private def layout(page: PageVM): (Vector[Placed], Double) =
    val pageMm = page.width.mm.toDouble
    val gutter = PageLayout.GutterMm.toDouble
    val pt     = page.textPt.toDouble
    val rows   = page.panels.foldLeft(Vector.empty[Vector[PanelVM]]) { (rows, p) =>
      rows.lastOption match
        case Some(row) if row.map(_.widthMm + gutter).sum + p.widthMm <= pageMm + 1e-9 =>
          rows.init :+ (row :+ p)
        case _ => rows :+ Vector(p)
    }
    rows.foldLeft((Vector.empty[Placed], 0.0)) { case ((placed, top), row) =>
      val height = row.map(panelHeight(_, pt)).max
      val xs     = row.scanLeft(0.0)((x, p) => x + p.widthMm + gutter)
      (placed ++ row.zip(xs).map((p, x) => Placed(p, x, top, height)), top + height + gutter)
    }

  // --- Drawing ----------------------------------------------------------------------

  private def colour(token: PaletteToken): Rgba =
    IntaglioColours.toIntaglio(Tokens.palette(token))

  private def g[A](during: String)(e: Either[GraphicsError, A]): Either[FigurePageError, A] =
    e.left.map(FigurePageError.Graphics(during, _))

  private def mm(value: Double): LengthExpr = LengthExpr(Length.unsafe(value, LengthUnit.Mm))

  private def extent(value: Double): ExtentExpr =
    ExtentExpr.unsafe(Length.unsafe(value, LengthUnit.Mm))

  /** Lay `page` out for export: every panel from its template, then the
    * figure caption and the provenance stamp below the panels, all in the
    * figure typography on paper.
    */
  def build(
      page: PageVM,
      rasters: Map[AssetRef, StimulusRaster] = Map.empty
  ): Either[FigurePageError, FigurePage] =
    val pt                = page.textPt.toDouble
    val widthMm           = page.width.mm.toDouble
    val (placed, panelsH) = layout(page)
    val footLines         = Vector(page.caption, page.stamp).map(wrap(_, widthMm, pt))
    val heightMm          = panelsH + footLines.map(_.size).sum * lineMm(pt)
    // Intaglio measures y up from the bottom of the page.
    def at(x: Double, yDown: Double): Point                 = Point(mm(x), mm(heightMm - yDown))
    def gp(face: FontFace, size: Double, ink: PaletteToken) =
      g("a text style")(
        Length
          .points(size)
          .flatMap(s =>
            GraphicParams.checked(
              stroke = None,
              fill = Some(colour(ink)),
              fontFamily = Some(face.javaFxFamily),
              fontSize = s
            )
          )
      )
    def lines(texts: Vector[String], x: Double, top: Double, ink: PaletteToken) =
      gp(FigureType.BodyFace, pt, ink).flatMap { style =>
        texts.zipWithIndex.foldLeft[Either[FigurePageError, Vector[Grob]]](
          Right(Vector.empty)
        ) { case (acc, (t, i)) =>
          acc.flatMap(gs =>
            g(s"the text '$t'")(
              Grob.text(
                t,
                at(x, top + i * lineMm(pt)),
                Anchor(HJust.Left, VJust.Top),
                gp = style
              )
            ).map(gs :+ _)
          )
        }
      }
    def paragraph(text: String, x: Double, top: Double, width: Double, ink: PaletteToken) =
      val broken = wrap(text, width, pt)
      lines(broken, x, top, ink).map(gs => (gs, top + broken.size * lineMm(pt)))
    def paragraphs(
        texts: Vector[String],
        x: Double,
        top: Double,
        width: Double,
        ink: PaletteToken
    ) =
      texts.foldLeft[Either[FigurePageError, (Vector[Grob], Double)]](
        Right((Vector.empty, top))
      ) { case (acc, text) =>
        acc.flatMap((gs, y) =>
          paragraph(text, x, y, width, ink).map((more, next) => (gs ++ more, next))
        )
      }
    // Annotations retain each child scene's description and scientific
    // summary when its grobs are placed in the page's coordinate system.
    def embedded(scene: Scene, grobs: Vector[Grob], port: Viewport): Grob =
      val title       = scene.semantics.accessibleTitle
      val description = scene.semantics.plots
        .flatMap(s => (Vector(s.accessibleDescription) ++ s.description.toVector).distinct)
        .mkString("\n")
      Grob.annotated(
        Grob.group(grobs, viewport = Some(port)),
        GrobMeta(title = title, description = Option.when(description.nonEmpty)(description))
      )
    def bodyText(body: PanelBody): Vector[String] = body match
      case PanelBody.Plot(vm) => vm.notes
      case PanelBody.Maps(vm) =>
        vm.tiles.flatMap(t => Vector(t.heading, t.label)) ++ Vector(vm.caption, vm.maps)
      case PanelBody.Gaze(vm)         => Vector(vm.heading, vm.displayed, vm.gaze)
      case PanelBody.Waiting(why)     => Vector(why)
      case PanelBody.Unavailable(why) => Vector(why)
    val figureId = SemanticId.unsafe(s"studio-figure-${page.figure.number}")
    def plot(p: Placed, vm: PlotPanelVM, top: Double): Either[FigurePageError, Grob] =
      val w = p.panel.widthMm.toDouble
      val h = plotHeight(w)
      for
        builder <- builderOf(vm)
        built   <- builder
          .build(vm.source, Theme.Light)
          .left
          .map(FigurePageError.Plot(p.panel.letter.value, _))
        port <- g("the plot's viewport")(
          Viewport
            .checked(origin = at(p.x, top + h), size = Size.fromExtents(extent(w), extent(h)))
        )
      yield embedded(built.plot.scene, built.plot.scene.grobs, port)
    def trial(p: Placed, vm: GazeTrialVM, top: Double): Either[FigurePageError, Grob] =
      val w = p.panel.widthMm.toDouble
      val h = w * FigureGaze.heightRatio(vm)
      // The trial panel fills all but the scene's caption band, which is
      // left empty below the drawing.
      val whole = h / (1.0 - TrialScene.CaptionFraction)
      for
        built <- TrialScene(FigureGaze.input(vm, rasters)).left
          .map(FigurePageError.Trial(p.panel.letter.value, _))
        port <- g("the trial's viewport")(
          Viewport.checked(
            origin = at(p.x, top + whole),
            size = Size.fromExtents(extent(w), extent(whole))
          )
        )
      yield embedded(built.plot.scene, FigureGaze.panelOnly(built), port)
    // The role glyph centred on the title row, the title after it.
    def glyph(role: ReferenceRole, x: Double, rowTop: Double): Either[FigurePageError, Grob] =
      val r        = GlyphEm * ptMm(pt) / 2
      val (cx, cy) = (x + r, rowTop + lineMm(pt) / 2)
      val during   = s"the ${role.toString.toLowerCase} glyph"
      for
        n     <- g(during)(GraphicsName(glyphName(role), "figure tile glyph"))
        drawn <- role match
          case ReferenceRole.Query =>
            g(during)(
              GraphicParams
                .checked(stroke = None, fill = Some(colour(PaletteToken.FigQuery)))
                .flatMap(Grob.circle(at(cx, cy), extent(r), _, name = Some(n)))
            )
          case ReferenceRole.Match =>
            // As much ink as the query's disc: half-diagonal r·√(π/2).
            val d = r * math.sqrt(math.Pi / 2)
            g(during)(
              GraphicParams
                .checked(stroke = None, fill = Some(colour(PaletteToken.FigMatch)))
                .flatMap(gp =>
                  Grob.polygon(
                    Vector(at(cx, cy - d), at(cx + d, cy), at(cx, cy + d), at(cx - d, cy)),
                    gp,
                    name = Some(n)
                  )
                )
            )
          case ReferenceRole.Control =>
            // The ring's outer edge is the disc's edge.
            val ring = ptMm(GlyphRingPt)
            g(during)(
              GraphicParams
                .checked(
                  stroke = Some(colour(PaletteToken.FigControl)),
                  fill = None,
                  lineWidth = GlyphRingPt,
                  lineWidthUnit = StrokeUnit.Point
                )
                .flatMap(Grob.circle(at(cx, cy), extent(r - ring / 2), _, name = Some(n)))
            )
      yield drawn
    def tiles(
        p: Placed,
        vm: DensityMapsVM,
        top: Double
    ): Either[FigurePageError, Vector[Grob]] =
      val w     = p.panel.widthMm.toDouble
      val tileW = tileWidth(vm, w)
      val h     = tileHeight(vm, w, pt)
      val mapW  = tileMapWidth(vm, w)
      // Fixed rows: every tile's title, map and caption start at the same
      // heights, whatever its text or its map's aspect.
      val titleTop   = top + TilePaddingMm
      val mapTop     = titleTop + lineMm(pt)
      val captionTop = mapTop + tileMapHeight(vm, w) + TilePaddingMm
      val glyphMm    = (GlyphEm + GlyphGapEm) * ptMm(pt)
      vm.tiles.zipWithIndex.foldLeft[Either[FigurePageError, Vector[Grob]]](
        Right(Vector.empty)
      ) { case (acc, (tile, i)) =>
        val x      = p.x + i * (tileW + TilePaddingMm)
        val role   = roleOf(tile)
        val indent = role.fold(0.0)(_ => glyphMm)
        val title  = fit(tile.title, mapW - indent, pt)
        for
          gs   <- acc
          rule <- g("a tile's rule")(
            GraphicParams.checked(
              stroke = Some(colour(PaletteToken.PaperRule)),
              fill = None,
              lineWidth = 0.5,
              lineWidthUnit = StrokeUnit.Point
            )
          )
          box <- g("a tile")(
            Grob.rect(
              at(x, top),
              Size.fromExtents(extent(tileW), extent(h)),
              Anchor(HJust.Left, VJust.Top),
              gp = rule
            )
          )
          mark    <- role.toVector.traverse(glyph(_, x + TilePaddingMm, titleTop))
          heading <- lines(
            Vector(title).filter(_.nonEmpty),
            x + TilePaddingMm + indent,
            titleTop,
            PaletteToken.PaperInk
          )
          map <- tile.map match
            case TileMap.Drawn(grid, style, region) =>
              val mapH = tileMapHeight(tile, mapW)
              for
                built <- MapTileScene
                  .of(
                    s"figure-panel-${p.panel.letter.value}-tile-$i",
                    grid,
                    style,
                    region,
                    StageVariant.Light
                  )
                  .left
                  .map(FigurePageError.Map(p.panel.letter.value, tile.trial.label, _))
                port <- g("the density tile's viewport")(
                  Viewport.checked(
                    origin = at(x + TilePaddingMm, mapTop + mapH),
                    size = Size.fromExtents(extent(mapW), extent(mapH))
                  )
                )
              yield Vector(embedded(built.scene, built.scene.grobs, port))
            case _ => Right(Vector.empty)
          rows <- Vector(tile.subtitle, tile.label).zipWithIndex
            .filter(_._1.nonEmpty)
            .traverse((text, row) =>
              lines(
                Vector(fit(text, mapW, pt)),
                x + TilePaddingMm,
                captionTop + row * lineMm(pt),
                PaletteToken.PaperInk
              )
            )
        yield (gs :+ box) ++ mark ++ heading ++ map ++ rows.flatten
      }
    def panel(p: Placed): Either[FigurePageError, Vector[Grob]] =
      val w    = p.panel.widthMm.toDouble
      val body = p.y + lineMm(FigureType.LetterPt)
      for
        letterStyle <- gp(
          FigureType.LetterFace,
          FigureType.LetterPt.toDouble,
          PaletteToken.PaperInk
        )
        letter <- g("a panel letter")(
          Grob.text(
            p.panel.letter.value,
            at(p.x, p.y),
            Anchor(HJust.Left, VJust.Top),
            gp = letterStyle
          )
        )
        titleStyle <- gp(FigureType.BodyFace, pt, PaletteToken.PaperInk)
        title      <- g("a panel title")(
          Grob.text(
            p.panel.title,
            at(p.x + 2 * ptMm(FigureType.LetterPt), p.y),
            Anchor(HJust.Left, VJust.Top),
            gp = titleStyle
          )
        )
        drawn <- p.panel.body match
          case PanelBody.Plot(vm) =>
            for
              chart      <- plot(p, vm, body)
              (notes, _) <- paragraphs(
                vm.notes,
                p.x,
                body + plotHeight(w),
                w,
                PaletteToken.PaperInk2
              )
            yield chart +: notes
          case PanelBody.Maps(vm) =>
            for
              boxes     <- tiles(p, vm, body)
              (text, _) <- paragraphs(
                Vector(vm.caption, vm.maps),
                p.x,
                body + tileHeight(vm, w, pt),
                w,
                PaletteToken.PaperInk2
              )
            yield boxes ++ text
          case PanelBody.Gaze(vm) =>
            for
              drawing   <- vm.drawn.toVector.traverse(trial(p, _, body))
              (text, _) <- paragraphs(
                Vector(vm.heading, vm.displayed, vm.gaze),
                p.x,
                body + gazeHeight(vm, w),
                w,
                PaletteToken.PaperInk2
              )
            yield drawing ++ text
          case PanelBody.Waiting(why) =>
            paragraph(why, p.x, body, w, PaletteToken.PaperInk2).map(_._1)
          case PanelBody.Unavailable(why) =>
            paragraph(why, p.x, body, w, PaletteToken.PaperInk2).map(_._1)
      yield Vector(
        Grob.annotated(
          Grob.group(Vector(letter, title) ++ drawn),
          GrobMeta(
            title = Some(s"Panel ${p.panel.letter.value}: ${p.panel.title}"),
            description = Some(bodyText(p.panel.body).mkString("\n")),
            data = Vector(
              DataKey.unsafe("semantic-id") ->
                s"${figureId.value}-panel-${p.panel.letter.value}"
            )
          )
        )
      )
    for
      paperStyle <- g("the paper")(
        GraphicParams.checked(stroke = None, fill = Some(colour(PaletteToken.Paper)))
      )
      // The whole canvas, which a target rounds up to whole pixels: no strip
      // is left unpainted.
      centre <- g("the paper")(Point.npc(0.5, 0.5))
      whole  <- g("the paper")(Size.npc(1.0, 1.0))
      paper  <- g("the paper")(Grob.rect(centre, whole, gp = paperStyle))
      panels <- placed.foldLeft[Either[FigurePageError, Vector[Grob]]](Right(Vector.empty))(
        (acc, p) => acc.flatMap(gs => panel(p).map(gs ++ _))
      )
      (foot, _) <- paragraphs(
        Vector(page.caption, page.stamp),
        0,
        panelsH,
        widthMm,
        PaletteToken.PaperInk
      )
    yield
      val description = (Vector(page.caption) ++ page.panels.flatMap(p =>
        Vector(s"Panel ${p.letter.value}: ${p.title}") ++ bodyText(p.body)
      ) :+ page.stamp).filter(_.nonEmpty).mkString("\n")
      val semantics = PlotSemantics(
        figureId,
        Some(page.title),
        Some(description),
        Some(description),
        Vector.empty,
        Vector.empty,
        Vector.empty
      )
      FigurePage(
        Scene((paper +: panels) ++ foot).withSemantics(SceneSemantics.single(semantics)),
        widthMm,
        heightMm
      )

  private def builderOf(vm: PlotPanelVM): Either[FigurePageError, PlotBuilder] = vm.kind match
    case PlotKind.Participant =>
      ParticipantColumns.standard
        .map(ParticipantPlot(_, vm.lines))
        .left
        .map(FigurePageError.Columns(_))
    case PlotKind.Profile =>
      ProfileColumns.standard.map(ScaleProfilePlot(_)).left.map(FigurePageError.Columns(_))

/** The plotted panels' shape, shared by the page on screen and in export. */
object PlotGeometry:
  /** A plotted panel's height as a fraction of its width (the board's panels). */
  val Aspect: Double = 0.62
