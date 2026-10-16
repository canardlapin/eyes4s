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
import eyes4s.studio.app.maps.DensityCasing
import eyes4s.studio.app.tokens.{
  PaletteToken,
  StageToken,
  StageVariant,
  Theme,
  ThemedToken,
  TokenUsage,
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

  test("a cased level's edges against empty cells wear the halo, then the inner casing") {
    // 1920 / 64 = 30 px cells. A singleton in the corner cell (0, 0) and a
    // ≥64 cell at column 38, row 15: on Dark every level is cased.
    val picture = gridOf(0 -> 1, (15 * 64 + 38) -> 4)
    val scene   = get(GeometryScene.density("d", StageVariant.Dark, frame, picture))
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
    val halo                     = densityStrokes(scene, GeometryScene.DensityHaloName).head
    val inner                    = densityStrokes(scene, GeometryScene.DensityCasingName).head
    def pt(x: Double, y: Double) = Point.native(x, y).fold(e => fail(e.toString), identity)
    // The corner cell: its right and bottom edges only (the screen's border
    // is not a ground); the interior cell: all four.
    val expected = Set(
      pt(0, 30)     -> pt(30, 30),
      pt(1140, 450) -> pt(1170, 450),
      pt(1140, 480) -> pt(1170, 480),
      pt(30, 0)     -> pt(30, 30),
      pt(1140, 450) -> pt(1140, 480),
      pt(1170, 450) -> pt(1170, 480)
    )
    assertEquals(halo.segments.toSet, expected)
    assertEquals(halo.segments.size, expected.size)
    assertEquals(inner.segments, halo.segments)
    // Two bands of 1 px outside the cell: the halo is stroked 4 px wide, the
    // inner casing 2 px, centred on the edge; square caps close the corners.
    val pointsPerPixel = 72.0 / 96.0
    assertEqualsDouble(
      halo.gp.lineWidth,
      4.0 * GeometryScene.DensityBandPx * pointsPerPixel,
      1e-12
    )
    assertEqualsDouble(
      inner.gp.lineWidth,
      2.0 * GeometryScene.DensityBandPx * pointsPerPixel,
      1e-12
    )
    assertEquals((halo.gp.lineCap, inner.gp.lineCap), (LineCap.Square, LineCap.Square))
    assertEquals(
      halo.gp.stroke,
      Some(IntaglioColours.staged(StageVariant.Dark, StageToken.Halo))
    )
    assertEquals(
      inner.gp.stroke,
      Some(
        IntaglioColours.toIntaglio(
          Tokens.resolve(
            TokenUsage.innerCasing(StageVariant.Dark),
            Theme.Light,
            StageVariant.Dark
          )
        )
      )
    )
  }

  test("edges between two occupied cells are not cased, and a line is stroked once") {
    // Cells (10, 5), (11, 5) and (10, 6): an L of three levels. The shared
    // edges are level against level; each outer line is one run.
    val picture = gridOf((5 * 64 + 10) -> 1, (5 * 64 + 11) -> 2, (6 * 64 + 10) -> 3)
    val scene   = get(GeometryScene.density("d", StageVariant.Mid, frame, picture))
    val halo    = densityStrokes(scene, GeometryScene.DensityHaloName).head
    def pt(x: Double, y: Double) = Point.native(x, y).fold(e => fail(e.toString), identity)
    assertEquals(
      halo.segments.toSet,
      Set(
        pt(300, 150) -> pt(360, 150), // the top of (10, 5) and (11, 5), one run
        pt(330, 180) -> pt(360, 180), // the bottom of (11, 5)
        pt(300, 210) -> pt(330, 210), // the bottom of (10, 6)
        pt(300, 150) -> pt(300, 210), // the left of (10, 5) and (10, 6), one run
        pt(360, 150) -> pt(360, 180), // the right of (11, 5)
        pt(330, 180) -> pt(330, 210)  // the right of (10, 6)
      )
    )
    assertEquals(halo.segments.size, 6)
  }

  test("a level the stage does not case has no casing; Light >=64 reads on its own") {
    val dense = gridOf((15 * 64 + 38) -> 4)
    val light = get(GeometryScene.density("d", StageVariant.Light, frame, dense))
    assert(!DensityCasing.needed(4, StageVariant.Light))
    assertEquals(densityStrokes(light, GeometryScene.DensityHaloName), Vector.empty)
    assertEquals(densityStrokes(light, GeometryScene.DensityCasingName), Vector.empty)
    // A singleton on Light is cased, with the stage's own bands.
    val single = get(GeometryScene.density("d", StageVariant.Light, frame, gridOf(100 -> 1)))
    val halo   = densityStrokes(single, GeometryScene.DensityHaloName)
    assertEquals(halo.map(_.segments.size), Vector(4))
    assertEquals(
      halo.head.gp.stroke,
      Some(IntaglioColours.staged(StageVariant.Light, StageToken.Halo))
    )
    // An uncased ≥64 next to a cased level: only the cased cell's outer edges.
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
