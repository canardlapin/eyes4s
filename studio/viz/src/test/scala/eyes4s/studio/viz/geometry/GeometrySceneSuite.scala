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

package eyes4s.studio.viz.geometry

import eyes4s.studio.app.geometry.*
import eyes4s.studio.app.maps.{DensityBand, DensityCasing, DensityGround}
import eyes4s.studio.app.tokens.{
  PaletteToken,
  StageToken,
  StageVariant,
  Theme,
  ThemedToken,
  Tokens,
  Wcag
}
import eyes4s.studio.viz.plot.{IntaglioColours, MissingAssetHatch}
import eyes4s.studio.core.assets.AssetFile
import eyes4s.studio.core.backend.{Phase, TrialKey}
import intaglio.{Grob, LineCap, Point, YDirection, value}

/** The placement pictures as Intaglio scenes (ticket S5.5): screen-pixel
  * data coordinates with y down, the image frame and its outline, the
  * records drawn where eyes4s placed them (off-screen ones only counted),
  * and the density's non-empty cells, opaque, under the two-band stage
  * casing where their level needs it.
  */
class GeometrySceneSuite extends munit.FunSuite:

  private val frame = FramePicture(1920, 1080, 448, 156, 1024, 768)
  private val trial = TrialKey("P05", Phase.Retrieval, "ret_04", 1)

  private def get[A](e: Either[eyes4s.studio.viz.plot.PlotSceneError, A]): A =
    e.fold(err => fail(err.message), identity)

  private def names(g: Grob): Vector[String] = g match
    case Grob.Group(children, _, name) => name.map(_.value).toVector ++ children.flatMap(names)
    case other                         => other.name.map(_.value).toVector

  test("a thumbnail draws the screen, the image frame, its outline and the on-screen records") {
    val picture = ThumbnailPicture(
      trial,
      3,
      1,
      1,
      Vector(
        MarkPicture(700.0, 420.0, MarkPlace.Inside, corrected = false),
        MarkPicture(260.0, 120.0, MarkPlace.OutsideWindow, corrected = true),
        MarkPicture(2000.0, 420.0, MarkPlace.OutsideScreen, corrected = false)
      ),
      FrameState.Present
    )
    val scene = get(
      GeometryScene.thumbnail(
        "geometry.thumb.0",
        Theme.Light,
        StageVariant.Dark,
        frame,
        picture
      )
    )
    assertEquals(scene.panel.xDomain.upper, 1920.0)
    assertEquals(scene.panel.yDomain.upper, 1080.0)
    assertEquals(scene.panel.yDirection, YDirection.Down)
    assertEquals(
      scene.scene.grobs.flatMap(names),
      Vector("image-frame", "records", "window-outline")
    )
    val batch = scene.scene.grobs.flatMap {
      case Grob.Group(children, _, _) => children.collect { case b: Grob.PointBatch => b }
      case _                          => Vector.empty
    }
    // The off-screen record is counted by the label, not drawn.
    assertEquals(batch.map(_.points.size), Vector(2))
  }

  test("a thumbnail whose records are all off screen still draws its frames") {
    val picture = ThumbnailPicture(
      trial,
      1,
      0,
      1,
      Vector(MarkPicture(-50.0, 20.0, MarkPlace.OutsideScreen, corrected = false)),
      FrameState.Present
    )
    val scene = get(
      GeometryScene.thumbnail("geometry.thumb.1", Theme.Dark, StageVariant.Mid, frame, picture)
    )
    assertEquals(scene.scene.grobs.flatMap(names), Vector("image-frame", "window-outline"))
  }

  /** Every grob of the scene, groups flattened. */
  private def flat(g: Grob): Vector[Grob] = g match
    case Grob.Group(children, _, _) => children.flatMap(flat)
    case other                      => Vector(other)

  private def named(scene: eyes4s.studio.viz.plot.PlotScene, n: String): Vector[Grob] =
    scene.scene.grobs.flatMap(flat).filter(_.name.exists(_.value == n))

  private val forest = AssetFile.of("forest-044.png").fold(e => fail(e.message), identity)

  private def framed(state: FrameState, theme: Theme = Theme.Light) =
    val picture = ThumbnailPicture(
      trial,
      2,
      0,
      0,
      Vector(
        MarkPicture(700.0, 420.0, MarkPlace.Inside, corrected = false),
        MarkPicture(900.0, 500.0, MarkPlace.Inside, corrected = false)
      ),
      state
    )
    get(GeometryScene.thumbnail("geometry.thumb.m", theme, StageVariant.Dark, frame, picture))

  test("a missing asset's thumbnail is hatched with an X under its records, never blank") {
    Vector(Theme.Light, Theme.Dark).foreach { theme =>
      val scene = framed(FrameState.Missing(forest), theme)
      assertEquals(
        scene.scene.grobs.flatMap(names),
        Vector(
          GeometryScene.MissingFieldName,
          GeometryScene.MissingCrossName,
          "records",
          "window-outline"
        )
      )
      val warnText = IntaglioColours.themed(theme, ThemedToken.WarnText)
      val warnSoft = IntaglioColours.themed(theme, ThemedToken.WarnSoft)
      // The field is the image frame, in the thumbnail-scale hatch.
      named(scene, GeometryScene.MissingFieldName) match
        case Vector(r: Grob.Rect) =>
          val hatch = r.gp.fillPattern.getOrElse(fail("the missing frame has no hatch"))
          assertEquals(hatch.ink, warnText)
          assertEquals(hatch.background, Some(warnSoft))
          val thumb = MissingAssetHatch.Thumbnail
          assertEquals(
            hatch.recipe,
            intaglio.PatternRecipe
              .angledHatch(45.0, thumb.spacing, thumb.lineWidth)
              .fold(e => fail(e.toString), identity)
          )
        case other => fail(s"expected one hatched rect, got $other")
      // The X: two warn-text strokes crossing at the frame's centre (960, 540).
      named(scene, GeometryScene.MissingCrossName) match
        case Vector(x: Grob.Segments) =>
          assertEquals(x.segments.size, 2)
          assertEquals(x.gp.stroke, Some(warnText))
        case other => fail(s"expected the X's segments, got $other")
      assert(
        scene.scene.semantics.plots.head.description
          .exists(_.contains("Missing asset · forest-044.png")),
        scene.scene.semantics.plots.head.description
      )
    }
  }

  test("an unreadable asset's thumbnail is hatched too; a present one is not") {
    val unreadable = framed(FrameState.Unreadable(forest, "not a PNG"))
    assertEquals(named(unreadable, GeometryScene.MissingFieldName).size, 1)
    assertEquals(named(unreadable, GeometryScene.MissingCrossName).size, 1)
    assert(
      unreadable.scene.semantics.plots.head.description
        .exists(_.contains("Unreadable asset · forest-044.png"))
    )
    val present = framed(FrameState.Present)
    assertEquals(
      present.scene.grobs.flatMap(names),
      Vector("image-frame", "records", "window-outline")
    )
    present.scene.grobs.flatMap(flat).foreach {
      case r: Grob.Rect => assert(r.gp.fillPattern.isEmpty, r)
      case _            => ()
    }
    assert(!present.scene.semantics.plots.head.description.exists(_.contains("asset")))
  }

  /** The density scene's drawn grobs inside its viewport group, in order. */
  private def densityLayers(scene: eyes4s.studio.viz.plot.PlotScene): Vector[Grob] =
    scene.scene.grobs.flatMap {
      case Grob.Group(children, _, _) => children
      case _                          => Vector.empty
    }

  private def densityCells(scene: eyes4s.studio.viz.plot.PlotScene): Vector[Grob] =
    densityLayers(scene).collect {
      case Grob.Group(rects, _, Some(n)) if n.value == "density" => rects
    }.flatten

  private def densityStrokes(scene: eyes4s.studio.viz.plot.PlotScene, name: String) =
    densityLayers(scene).collect {
      case s: Grob.Segments if s.name.exists(_.value == name) => s
    }

  /** A density on the 64 × 36 grid with `cells` (index → level) occupied. */
  private def gridOf(cells: (Int, Int)*): DensityPicture =
    val at = cells.toMap
    DensityPicture(
      64,
      36,
      Vector.tabulate(64 * 36)(i => at.getOrElse(i, 0)),
      2,
      DensitySteps.Default
    )

  test("the density draws one opaque rect per non-empty cell") {
    val picture = gridOf(0 -> 1, (15 * 64 + 38) -> 4)
    val scene   =
      get(GeometryScene.density("geometry.density.0", StageVariant.Dark, frame, picture))
    val cells = densityCells(scene)
    assertEquals(cells.size, 2)
    cells.foreach {
      case r: Grob.Rect =>
        assertEquals(r.gp.alpha, 1.0)
        assert(r.gp.fill.exists(_.alpha == 1.0), r.gp.fill)
      case other => fail(s"expected a rect, got $other")
    }
    // Level 1 (the singleton in cell 0) is ramp-0; level 4 is ramp-3.
    val ramp = (t: PaletteToken) => Some(IntaglioColours.toIntaglio(Tokens.palette(t)))
    assertEquals(
      cells.collect { case r: Grob.Rect => r.gp.fill },
      Vector(ramp(PaletteToken.Ramp0), ramp(PaletteToken.Ramp3))
    )
  }

  private def pt(x: Double, y: Double) =
    Point.native(x, y).fold(e => fail(e.toString), identity)

  private def bandStroke(stage: StageVariant, band: DensityBand) =
    Some(IntaglioColours.toIntaglio(band.colour(stage)))

  test("an edge is cased by its level against its own ground, with one band under the fills") {
    // 1920 / 64 = 30 px cells; the image frame spans x 448-1472, y 156-924.
    // On Dark: a singleton outside the frame reads on the stage (13.38) and
    // is not cased; >=64 outside it is (1.50), in the halo; a singleton
    // inside the frame is cased against the screen (2.17), in the inner
    // casing (white --halo is 2.82 there).
    val outside1 = 0
    val outside4 = 30 * 64 + 2  // (60, 900): below the frame's left
    val inside1  = 15 * 64 + 30 // (900, 450)
    val picture  = gridOf(outside1 -> 1, outside4 -> 4, inside1 -> 1)
    val scene    = get(GeometryScene.density("d", StageVariant.Dark, frame, picture))
    // The casing is stroked before (under) the fills.
    assertEquals(
      densityLayers(scene).flatMap(_.name.map(_.value)),
      Vector(
        "image-frame",
        GeometryScene.DensityHaloName,
        GeometryScene.DensityCasingName,
        "density",
        "window-outline"
      )
    )
    val halo  = densityStrokes(scene, GeometryScene.DensityHaloName).head
    val inner = densityStrokes(scene, GeometryScene.DensityCasingName).head
    assertEquals(
      halo.segments.toSet,
      Set(
        pt(60, 900) -> pt(90, 900),
        pt(60, 930) -> pt(90, 930),
        pt(60, 900) -> pt(60, 930),
        pt(90, 900) -> pt(90, 930)
      )
    )
    assertEquals(
      inner.segments.toSet,
      Set(
        pt(900, 450) -> pt(930, 450),
        pt(900, 480) -> pt(930, 480),
        pt(900, 450) -> pt(900, 480),
        pt(930, 450) -> pt(930, 480)
      )
    )
    // One band of 1 px outside the cell: stroked 2 px wide, centred on the
    // edge, square caps; each in the band DensityCasing derives.
    val pointsPerPixel = 72.0 / 96.0
    List(halo, inner).foreach { s =>
      assertEqualsDouble(
        s.gp.lineWidth,
        2.0 * GeometryScene.DensityBandPx * pointsPerPixel,
        1e-12
      )
      assertEquals(s.gp.lineCap, LineCap.Square)
    }
    assertEquals(
      DensityCasing.band(4, StageVariant.Dark, DensityGround.Stage),
      Some(DensityBand.Halo)
    )
    assertEquals(
      DensityCasing.band(1, StageVariant.Dark, DensityGround.Screen),
      Some(DensityBand.InnerCasing)
    )
    assertEquals(halo.gp.stroke, bandStroke(StageVariant.Dark, DensityBand.Halo))
    assertEquals(inner.gp.stroke, bandStroke(StageVariant.Dark, DensityBand.InnerCasing))
  }

  test("an edge is split where the image frame's boundary crosses it") {
    // Cell (14, 6) spans x 420-450, y 180-210; the frame's left edge, x 448,
    // crosses its top and bottom neighbours. On Dark, a singleton is cased
    // only on the screen side; >=64 only on the stage side.
    val cell = 6 * 64 + 14
    val one  = get(GeometryScene.density("d", StageVariant.Dark, frame, gridOf(cell -> 1)))
    assertEquals(densityStrokes(one, GeometryScene.DensityHaloName), Vector.empty)
    assertEquals(
      densityStrokes(one, GeometryScene.DensityCasingName).head.segments.toSet,
      Set(
        pt(448, 180) -> pt(450, 180),
        pt(448, 210) -> pt(450, 210),
        pt(450, 180) -> pt(450, 210)
      )
    )
    val dense = get(GeometryScene.density("d", StageVariant.Dark, frame, gridOf(cell -> 4)))
    assertEquals(densityStrokes(dense, GeometryScene.DensityCasingName), Vector.empty)
    assertEquals(
      densityStrokes(dense, GeometryScene.DensityHaloName).head.segments.toSet,
      Set(
        pt(420, 180) -> pt(448, 180),
        pt(420, 210) -> pt(448, 210),
        pt(420, 180) -> pt(420, 210)
      )
    )
  }

  test("edges between two occupied cells are not cased, and a line is stroked once") {
    // Cells (20, 10), (21, 10) and (20, 11), inside the frame: an L of three
    // levels, each cased against the screen on Mid. The shared edges are
    // level against level; each outer line is one run.
    val picture = gridOf((10 * 64 + 20) -> 1, (10 * 64 + 21) -> 2, (11 * 64 + 20) -> 3)
    val scene   = get(GeometryScene.density("d", StageVariant.Mid, frame, picture))
    assertEquals(densityStrokes(scene, GeometryScene.DensityHaloName), Vector.empty)
    val inner = densityStrokes(scene, GeometryScene.DensityCasingName).head
    assertEquals(
      inner.segments.toSet,
      Set(
        pt(600, 300) -> pt(660, 300), // the top of (20, 10) and (21, 10), one run
        pt(630, 330) -> pt(660, 330), // the bottom of (21, 10)
        pt(600, 360) -> pt(630, 360), // the bottom of (20, 11)
        pt(600, 300) -> pt(600, 360), // the left of (20, 10) and (20, 11), one run
        pt(660, 300) -> pt(660, 330), // the right of (21, 10)
        pt(630, 330) -> pt(630, 360)  // the right of (20, 11)
      )
    )
    assertEquals(inner.segments.size, 6)
  }

  test("on Light, a fill that reads on its ground has no casing; others wear the halo") {
    // Outside the frame: >=64 (10.38) and 16-63 (4.58) read on the stage.
    val light = get(
      GeometryScene.density("d", StageVariant.Light, frame, gridOf(100 -> 4, 104 -> 3))
    )
    assertEquals(densityStrokes(light, GeometryScene.DensityHaloName), Vector.empty)
    assertEquals(densityStrokes(light, GeometryScene.DensityCasingName), Vector.empty)
    // A singleton on the Light stage is cased, in the stage's ink halo.
    val single = get(GeometryScene.density("d", StageVariant.Light, frame, gridOf(100 -> 1)))
    val halo   = densityStrokes(single, GeometryScene.DensityHaloName)
    assertEquals(halo.map(_.segments.size), Vector(4))
    assertEquals(halo.head.gp.stroke, bandStroke(StageVariant.Light, DensityBand.Halo))
    // A singleton next to an uncased >=64: only its own outer edges.
    val mixed = get(
      GeometryScene.density("d", StageVariant.Light, frame, gridOf(100 -> 1, 101 -> 4))
    )
    assertEquals(
      densityStrokes(mixed, GeometryScene.DensityHaloName).map(_.segments.size),
      Vector(3)
    )
    // No occupied cell: no casing at all.
    val none = get(GeometryScene.density("d", StageVariant.Dark, frame, gridOf()))
    assertEquals(densityStrokes(none, GeometryScene.DensityHaloName), Vector.empty)
    assertEquals(densityStrokes(none, GeometryScene.DensityCasingName), Vector.empty)
  }

  test("the density's semantics name its cells and its four level ranges") {
    def described(picture: DensityPicture): String =
      get(
        GeometryScene.density("density", StageVariant.Dark, frame, picture)
      ).scene.semantics.plots.head.description
        .getOrElse(fail("the density has no description"))
    assert(
      clue(described(DensityPicture(2, 2, Vector(0, 1, 2, 4), 3, DensitySteps.Default)))
        .endsWith(
          "3 cells drawn at four log-spaced levels: 1–3, 4–15, 16–63 and at least 64 records per cell."
        )
    )
    // Computed from the picture's own steps, with thousands separators; one
    // cell in the singular (review ticket 24).
    assert(
      clue(described(DensityPicture(2, 1, Vector(2, 0), 300, DensitySteps.anchored(300L))))
        .endsWith(
          ". 1 cell drawn at four log-spaced levels: 1–255, 256–1,023, 1,024–4,095 and at least " +
            "4,096 records per cell."
        )
    )
    // An empty study keeps the default key.
    assert(
      clue(described(DensityPicture(2, 1, Vector(0, 0), 0, DensitySteps.Default))).endsWith(
        "0 cells drawn at four log-spaced levels: 1–3, 4–15, 16–63 and at least 64 records per cell."
      )
    )
  }

  /** Reference contrasts are quoted to two decimals; half a unit of the last
    * quoted place.
    */
  private val QuotedPlaces = 0.005

  test("level 1 keeps ramp-0 on every stage, opaque; its measured contrast is pinned") {
    // The owner kept the ramp (DESIGN_SPEC section 15.1, 2026-10-12) and drew
    // it opaque, cased where it needs it (option B, 2026-10-15): a singleton
    // on the Light stage is faint on its own, and its casing carries it
    // (TokenContrastSuite). This pins what the code draws so that a change
    // to it is deliberate.
    val ramp0 = Tokens.palette(PaletteToken.Ramp0)
    val stage = (v: StageVariant) => Tokens.staged(v, StageToken.Stage)
    val cells = densityCells(
      get(GeometryScene.density("d", StageVariant.Dark, frame, gridOf(0 -> 1)))
    )
    assertEquals(cells.collect { case r: Grob.Rect => r.gp.alpha }, Vector(1.0))
    val measured =
      StageVariant.values.toVector.map(v =>
        v -> Wcag.contrast(ramp0, stage(v)).fold(e => fail(e.message), identity)
      )
    val pinned =
      Vector(StageVariant.Dark -> 13.38, StageVariant.Mid -> 3.49, StageVariant.Light -> 1.16)
    assertEquals(measured.map(_._1).toSet, pinned.map(_._1).toSet)
    pinned.foreach { (v, expected) =>
      assertEqualsDouble(measured.toMap.apply(v), expected, QuotedPlaces, clue(v))
    }
  }

  test("placement diagrams describe counted records, including records outside the screen") {
    val picture = ThumbnailPicture(
      trial,
      3,
      1,
      1,
      Vector(
        MarkPicture(700, 420, MarkPlace.Inside, corrected = false),
        MarkPicture(260, 120, MarkPlace.OutsideWindow, corrected = true),
        MarkPicture(2000, 420, MarkPlace.OutsideScreen, corrected = false)
      ),
      FrameState.Present
    )
    val thumbnail =
      get(GeometryScene.thumbnail("thumb", Theme.Light, StageVariant.Dark, frame, picture))
    val density = get(
      GeometryScene.density(
        "density",
        StageVariant.Dark,
        frame,
        DensityPicture(2, 2, Vector(0, 1, 2, 4), 3, DensitySteps.Default)
      )
    )
    val scenes = Vector(thumbnail, density)
    assertEquals(scenes.map(_.scene.semantics.plots.head.id).distinct.size, 2)
    assert(thumbnail.scene.semantics.plots.head.description.exists(_.contains("2 marks drawn")))
    assert(density.scene.semantics.plots.head.description.exists(_.contains("3 cells drawn")))
    scenes.foreach { scene =>
      val context = intaglio.RenderContext(640, 400).fold(e => fail(e.toString), identity)
      val svg     = intaglio.svg.SvgRenderer
        .render(intaglio.RenderPlan(scene.scene, context))
        .fold(e => fail(e.toString), _.value)
      assert(svg.contains("<title id="))
      assert(svg.contains("<desc id="))
      assert(svg.contains("3 records"))
    }
  }
