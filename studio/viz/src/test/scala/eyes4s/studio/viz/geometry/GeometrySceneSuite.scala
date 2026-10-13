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
import eyes4s.studio.app.tokens.{
  Colour,
  Greyscale,
  PaletteToken,
  StageToken,
  StageVariant,
  Theme,
  Tokens,
  Wcag
}
import eyes4s.studio.viz.plot.IntaglioColours
import eyes4s.studio.core.backend.{Phase, TrialKey}
import intaglio.{Grob, YDirection, value}

/** The placement pictures as Intaglio scenes (ticket S5.5): screen-pixel
  * data coordinates with y down, the image frame and its outline, the
  * records drawn where eyes4s placed them (off-screen ones only counted),
  * and the density's non-empty cells at the map opacity.
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
      )
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
      Vector(MarkPicture(-50.0, 20.0, MarkPlace.OutsideScreen, corrected = false))
    )
    val scene = get(
      GeometryScene.thumbnail("geometry.thumb.1", Theme.Dark, StageVariant.Mid, frame, picture)
    )
    assertEquals(scene.scene.grobs.flatMap(names), Vector("image-frame", "window-outline"))
  }

  test("the density draws one rect per non-empty cell, at the map opacity") {
    val levels =
      Vector.tabulate(64 * 36)(i => if i == 15 * 64 + 38 then 4 else if i == 0 then 1 else 0)
    val picture = DensityPicture(64, 36, levels, 2, DensitySteps.Default)
    val scene   =
      get(GeometryScene.density("geometry.density.0", StageVariant.Dark, frame, picture))
    val cells = scene.scene.grobs.flatMap {
      case Grob.Group(children, _, _) =>
        children.collect {
          case Grob.Group(rects, _, Some(n)) if n.value == "density" => rects
        }.flatten
      case _ => Vector.empty
    }
    assertEquals(cells.size, 2)
    cells.foreach {
      case r: Grob.Rect => assertEquals(r.gp.alpha, GeometryScene.DensityOpacity)
      case other        => fail(s"expected a rect, got $other")
    }
    // Level 1 (the singleton in cell 0) is ramp-0; level 4 is ramp-3.
    val ramp = (t: PaletteToken) => Some(IntaglioColours.toIntaglio(Tokens.palette(t)))
    assertEquals(
      cells.collect { case r: Grob.Rect => r.gp.fill },
      Vector(ramp(PaletteToken.Ramp0), ramp(PaletteToken.Ramp3))
    )
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
    // Computed from the picture's own steps, with thousands separators.
    assert(
      clue(described(DensityPicture(2, 1, Vector(2, 0), 300, DensitySteps.anchored(300L))))
        .endsWith(
          "1 cells drawn at four log-spaced levels: 1–255, 256–1,023, 1,024–4,095 and at least " +
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

  /** WCAG contrast of `fg` at `opacity` composited in sRGB over the opaque
    * `bg`, as [[Wcag.contrast]] composites a translucent token.
    */
  private def contrastAt(fg: Colour, opacity: Double, bg: Colour): Double =
    def lum(r: Double, g: Double, b: Double): Double =
      0.2126 * Greyscale.toLinear(r) + 0.7152 * Greyscale.toLinear(g) +
        0.0722 * Greyscale.toLinear(b)
    def mix(f: Int, b: Int): Double = (f * opacity + b * (1.0 - opacity)) / 255.0
    val a = lum(mix(fg.red, bg.red), mix(fg.green, bg.green), mix(fg.blue, bg.blue))
    val c = lum(bg.red / 255.0, bg.green / 255.0, bg.blue / 255.0)
    (math.max(a, c) + 0.05) / (math.min(a, c) + 0.05)

  test("level 1 keeps ramp-0 on every stage; its measured contrast is pinned") {
    // The owner kept the ramp (DESIGN_SPEC section 15.1, 2026-10-12): a
    // singleton on the Light stage is faint. Stage-aware colours are a
    // separate proposal (mote bd-01M4SJ6T6MMGAW1PRGREGVSX50); this pins what
    // the code draws so that a change to it is deliberate.
    val ramp0 = Tokens.palette(PaletteToken.Ramp0)
    val stage = (v: StageVariant) => Tokens.staged(v, StageToken.Stage)
    // The helper is Wcag's arithmetic: at full opacity it agrees with it.
    StageVariant.values.foreach { v =>
      assertEqualsDouble(
        contrastAt(ramp0, 1.0, stage(v)),
        Wcag.contrast(ramp0, stage(v)).fold(e => fail(e.message), identity),
        1e-12
      )
    }
    val measured =
      StageVariant.values.toVector.map(v =>
        v -> contrastAt(ramp0, GeometryScene.DensityOpacity, stage(v))
      )
    val pinned =
      Vector(StageVariant.Dark -> 5.57, StageVariant.Mid -> 2.25, StageVariant.Light -> 1.09)
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
      )
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
