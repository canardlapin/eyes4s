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

import eyes4s.studio.app.tokens.{StageVariant, Theme, Tokens}
import eyes4s.studio.core.assets.Display
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}
import eyes4s.studio.viz.plot.{DataPoint, IntaglioColours, PlotSurface, PlotTransform}
import eyes4s.studio.viz.trial.TrialSamples.*
import intaglio.{
  DeviceElement,
  DevicePoint,
  DevicePrimitive,
  DeviceScene,
  GraphicParams,
  RenderPlan,
  Rgba,
  value
}
import munit.ScalaCheckSuite
import org.scalacheck.{Gen, Prop}

/** The trial scene (S4.3a): one transform for the image, the marks and the
  * picking frame; every display kind on every theme and stage; missing is
  * never blank; a blank display never shows an image; colours are tokens.
  */
class TrialSceneSuite extends ScalaCheckSuite:

  /** Device positions agree to well below a device pixel. */
  private val Tolerance = 1e-6

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private def input(
      d: Display,
      fixations: Vector[TrialFixation] = ret07Fixations,
      marks: MarkStyle = MarkStyle.Neutral,
      theme: Theme = Theme.Light,
      stage: StageVariant = StageVariant.Dark,
      loaded: Boolean = true,
      options: TrialSceneOptions = TrialSceneOptions()
  ): TrialSceneInput =
    TrialSceneInput(
      display(ret07, d),
      screen,
      fixations,
      marks,
      theme,
      stage,
      if loaded then Map(beach -> StimulusRaster.Loaded(raster)) else Map.empty,
      options
    )

  private def lower(scene: TrialScene, surface: PlotSurface): (PlotTransform, DeviceScene) =
    val transform = right(PlotTransform.resolve(scene.plot, surface))
    (transform, right(RenderPlan(scene.plot.scene, transform.renderContext).deviceScene))

  private def primitives(elements: Vector[DeviceElement]): Vector[DevicePrimitive] =
    elements.flatMap {
      case DeviceElement.Mark(p)                  => Vector(p)
      case DeviceElement.Group(_, _, _, children) => primitives(children)
      case DeviceElement.Annotated(_, children)   => primitives(children)
    }

  private def named(p: DevicePrimitive): Option[String] = p match
    case DevicePrimitive.Disc(_, _, _, _, n)                   => n.map(_.value)
    case DevicePrimitive.PointBatch(_, _, _, _, n)             => n.map(_.value)
    case DevicePrimitive.Polyline(_, _, _, n)                  => n.map(_.value)
    case DevicePrimitive.CompoundPolygon(_, _, n)              => n.map(_.value)
    case DevicePrimitive.RectShape(_, _, _, _, _, _, n)        => n.map(_.value)
    case DevicePrimitive.TextRun(_, _, _, _, _, _, _, _, _, n) => n.map(_.value)
    case DevicePrimitive.Image(_, _, _, _, _, _, _, n)         => n.map(_.value)

  private def near(a: DevicePoint, b: DevicePoint): Boolean =
    math.abs(a.x - b.x) <= Tolerance && math.abs(a.y - b.y) <= Tolerance

  // ---------------------------------------------------------------------------
  // One transform for image, marks and picking
  // ---------------------------------------------------------------------------

  private val genSurface: Gen[PlotSurface] =
    for
      w <- Gen.choose(160.0, 1800.0)
      h <- Gen.choose(120.0, 1100.0)
      s <- Gen.oneOf(1.0, 1.25, 1.5, 2.0, 3.0)
    yield right(PlotSurface(w, h, s))

  private val genFixations: Gen[Vector[TrialFixation]] =
    for
      n  <- Gen.choose(1, 24)
      xs <- Gen.listOfN(n, Gen.choose(0.0, 1919.0))
      ys <- Gen.listOfN(n, Gen.choose(0.0, 1079.0))
      ds <- Gen.listOfN(n, Gen.choose(2, 1600))
      os <- Gen.listOfN(n, Gen.oneOf(WindowSide.Inside, WindowSide.Outside))
    yield xs.lazyZip(ys).lazyZip(ds).lazyZip(os).toVector.zipWithIndex.map {
      case ((x, y, d, o), i) =>
        right(TrialFixation.of(ret07, right(FixationIndex.of(i + 1)), x, y, d, o))
    }

  private val genMarks: Gen[MarkStyle] =
    Gen.oneOf(MarkStyle.Neutral +: TrialRole.values.toList.map(MarkStyle.Role(_)))

  private val genExtent: Gen[TrialExtent] = Gen.oneOf(
    Gen.const(TrialExtent.Gaze),
    Gen.const(TrialExtent.Screen),
    Gen.const(TrialExtent.Covering(right(ScreenRect.of(100.0, 50.0, 1700.0, 1000.0))))
  )

  property("image, marks and picking frame share one transform, on any surface") {
    Prop.forAll(genSurface, genFixations, genMarks, genExtent) { (surface, fs, marks, extent) =>
      val scene = right(
        TrialScene(
          input(
            Display.Image(eyes4s.studio.core.assets.AssetLink.Present(beach)),
            fs,
            marks,
            options = TrialSceneOptions(extent = extent)
          )
        )
      )
      val (transform, device) = lower(scene, surface)
      val prims               = primitives(device.elements)

      // The marks are drawn where the transform puts their fixations.
      val drawn = prims.collect {
        case DevicePrimitive.PointBatch(points, _, _, _, Some(n))
            if n.value == TrialScene.MarksName =>
          points
      }.flatten
      assertEquals(drawn.size, fs.size)
      scene.marks.zip(drawn).foreach { (mark, at) =>
        assert(near(right(transform.dataToDevice(mark.at)), at), s"$mark drawn at $at")
        assertEquals(
          mark.at,
          DataPoint(fs(mark.batchIndex).screenX, fs(mark.batchIndex).screenY)
        )
        // A canvas position maps back to the fixation (the picking direction).
        val back = transform.canvasToData(transform.deviceToCanvas(at))
        assertEqualsDouble(back.x, mark.at.x, 1e-6)
        assertEqualsDouble(back.y, mark.at.y, 1e-6)
        assertEquals(
          scene.refAt(mark.batchIndex),
          Some(StudioRef.Fixation(ret07, fs(mark.batchIndex).index))
        )
      }

      // The image fills the image frame the same transform places.
      val images = prims.collect { case i: DevicePrimitive.Image => i }
      assertEquals(images.size, 1)
      val image       = images.head
      val topLeft     = right(transform.dataToDevice(DataPoint(448.0, 156.0)))
      val bottomRight = right(transform.dataToDevice(DataPoint(1472.0, 924.0)))
      assert(near(DevicePoint(image.x, image.y), topLeft), s"$image vs $topLeft")
      assert(
        near(DevicePoint(image.x + image.width, image.y + image.height), bottomRight),
        s"$image vs $bottomRight"
      )

      // The picking frame is the plan compiled against the surface's context,
      // which is the one the transform resolved (RenderContext has no value
      // equality, so the lowered scenes are compared).
      assertEquals(
        right(
          RenderPlan(scene.plot.scene, right(surface.renderContext(scene.plot.id))).deviceScene
        ),
        device
      )
      true
    }
  }

  test("marks keep their data position when the surface changes") {
    val scene = right(TrialScene(input(Display.BlankWithFixationCross)))
    val a     = right(PlotTransform.resolve(scene.plot, right(PlotSurface(600, 420, 1.0))))
    val b     = right(PlotTransform.resolve(scene.plot, right(PlotSurface(1200, 840, 2.0))))
    scene.marks.foreach { m =>
      val ca = right(a.dataToCanvas(m.at))
      val cb = right(b.dataToCanvas(m.at))
      assertEqualsDouble(cb.x, ca.x * 2.0, 1e-6)
      assertEqualsDouble(cb.y, ca.y * 2.0, 1e-6)
    }
  }

  // ---------------------------------------------------------------------------
  // Display kinds, themes and stages
  // ---------------------------------------------------------------------------

  private val tokenColours: Set[Rgba] =
    (for
      theme <- Theme.values.toList
      stage <- StageVariant.values.toList
      ref   <- Tokens.all
    yield IntaglioColours.toIntaglio(Tokens.resolve(ref, theme, stage))).toSet

  private def colours(gp: GraphicParams): List[Rgba] =
    gp.stroke.toList ++ gp.fill.toList ++ gp.fillPattern.toList.flatMap(p =>
      p.ink :: p.background.toList
    )

  private def paints(p: DevicePrimitive): List[GraphicParams] = p match
    case DevicePrimitive.Disc(_, _, _, gp, _)             => List(gp)
    case DevicePrimitive.PointBatch(points, _, _, gps, _) =>
      points.indices.map(gps.valueAt).toList
    case DevicePrimitive.Polyline(_, _, gp, _)                  => List(gp)
    case DevicePrimitive.CompoundPolygon(_, gp, _)              => List(gp)
    case DevicePrimitive.RectShape(_, _, _, _, _, gp, _)        => List(gp)
    case DevicePrimitive.TextRun(_, _, _, _, _, _, _, _, gp, _) => List(gp)
    case DevicePrimitive.Image(_, _, _, _, _, _, _, _)          => Nil

  for
    (kind, d) <- displays
    theme     <- Theme.values.toList
    stage     <- StageVariant.values.toList
    marks     <- List(MarkStyle.Neutral, MarkStyle.Role(TrialRole.Control))
  do
    test(
      s"$kind renders on the ${stage.cssName} stage in ${theme.toString.toLowerCase}, $marks"
    ) {
      val scene       = right(TrialScene(input(d, marks = marks, theme = theme, stage = stage)))
      val (_, device) = lower(scene, right(PlotSurface(640, 480, 2.0)))
      val prims       = primitives(device.elements)
      assert(prims.exists(p => named(p).contains(TrialScene.StageName)))
      assert(prims.exists(p => named(p).contains(TrialScene.MarksName)))
      val foreign = prims.flatMap(paints).flatMap(colours).filterNot(tokenColours)
      assert(foreign.isEmpty, s"colours that are not tokens: ${foreign.distinct}")
    }

  test("each display kind shows what it displayed") {
    val arts = displays.map((k, d) => k -> right(TrialScene(input(d))).frameArt).toMap
    assertEquals(arts("image"), FrameArt.Picture(beach))
    assertEquals(arts("blank"), FrameArt.Screen)
    assertEquals(arts("blank-cross"), FrameArt.ScreenWithCross)
    assertEquals(arts("cue"), FrameArt.CueText("beach-042"))
    assertEquals(arts("cue-image"), FrameArt.Picture(beach))
    assertEquals(arts("unknown"), FrameArt.Unknown)
    assertEquals(arts("missing"), FrameArt.Missing(file("forest-044.png")))
    assertEquals(arts("cue-missing"), FrameArt.Missing(file("kitchen-081.png")))
  }

  test("a missing asset is hatched and labelled, never a blank screen") {
    def frame(d: Display): GraphicParams =
      val (_, device) = lower(right(TrialScene(input(d))), right(PlotSurface(640, 480, 1.0)))
      primitives(device.elements).collect {
        case DevicePrimitive.RectShape(_, _, _, _, _, gp, Some(n))
            if n.value == TrialScene.FrameName =>
          gp
      }.head
    val missing = frame(displays.toMap.apply("missing"))
    val blank   = frame(Display.Blank)
    assert(missing.fillPattern.isDefined, "the missing frame is not hatched")
    assertEquals(blank.fillPattern, None)
    assertNotEquals(missing.fill, blank.fill)
    val (_, device) = lower(
      right(TrialScene(input(displays.toMap.apply("missing")))),
      right(PlotSurface(640, 480, 1.0))
    )
    val texts = primitives(device.elements).collect { case t: DevicePrimitive.TextRun =>
      t.label
    }
    assert(texts.contains("Missing asset · forest-044.png"), texts.mkString("\n"))
  }

  test("an asset not loaded yet is a labelled loading state, and an unreadable one hatched") {
    val image   = Display.Image(eyes4s.studio.core.assets.AssetLink.Present(beach))
    val pending = right(TrialScene(input(image, loaded = false)))
    assertEquals(pending.frameArt, FrameArt.Loading(beach.file))
    val broken = right(
      TrialScene(
        input(image).copy(rasters = Map(beach -> StimulusRaster.Unreadable("digest mismatch")))
      )
    )
    assertEquals(broken.frameArt, FrameArt.Unreadable(beach.file, "digest mismatch"))
  }

  test("a blank-display query shows no image, even with the item's image at hand") {
    List(Display.Blank, Display.BlankWithFixationCross).foreach { d =>
      val scene       = right(TrialScene(input(d, marks = MarkStyle.Role(TrialRole.Query))))
      val (_, device) = lower(scene, right(PlotSurface(640, 480, 1.0)))
      val images = primitives(device.elements).collect { case i: DevicePrimitive.Image => i }
      assertEquals(images, Vector.empty, s"$d drew an image")
    }
  }

  // ---------------------------------------------------------------------------
  // Captions, order lines, outside marks
  // ---------------------------------------------------------------------------

  test("the caption states what was displayed and where") {
    assertEquals(
      right(TrialScene(input(Display.BlankWithFixationCross))).caption,
      "Displayed: blank + fixation cross · 1024×768 at (448, 156) in 1920×1080"
    )
    assertEquals(
      right(TrialScene(input(displays.toMap.apply("image")))).caption,
      "Displayed: image beach-042.png · 1024×768 at (448, 156) in 1920×1080"
    )
  }

  test("order lines join fixation centres in order and are labelled as order") {
    val scene       = right(TrialScene(input(Display.BlankWithFixationCross)))
    val (t, device) = lower(scene, right(PlotSurface(800, 600, 1.0)))
    val prims       = primitives(device.elements)
    val line        = prims.collect {
      case DevicePrimitive.Polyline(points, false, _, Some(n))
          if n.value == TrialScene.OrderName =>
        points
    }
    assertEquals(line.size, 1)
    assertEquals(line.head.size, 12)
    line.head.zip(ret07Fixations).foreach { (p, f) =>
      assert(near(p, right(t.dataToDevice(DataPoint(f.screenX, f.screenY)))))
    }
    val texts = prims.collect { case x: DevicePrimitive.TextRun => x.label }
    assert(texts.contains("Lines show order between fixation centres, not measured saccades"))
    assert(!texts.exists(_.toLowerCase.contains("saccade path")))
    val off = right(
      TrialScene(input(Display.Blank, options = TrialSceneOptions(order = false)))
    )
    val (_, quiet) = lower(off, right(PlotSurface(800, 600, 1.0)))
    assert(!primitives(quiet.elements).exists(p => named(p).contains(TrialScene.OrderName)))
  }

  test("outside-window fixations are flagged: dashed and unfilled") {
    List(MarkStyle.Neutral, MarkStyle.Role(TrialRole.Query)).foreach { style =>
      val scene       = right(TrialScene(input(Display.BlankWithFixationCross, marks = style)))
      val (_, device) = lower(scene, right(PlotSurface(800, 600, 1.0)))
      val gps         = primitives(device.elements).collect {
        case DevicePrimitive.PointBatch(points, _, _, gps, Some(n))
            if n.value == TrialScene.MarksName =>
          points.indices.map(gps.valueAt)
      }.flatten
      val outside = scene.marks.filter(_.window == WindowSide.Outside).map(_.batchIndex)
      assertEquals(outside, Vector(8))
      outside.foreach { i =>
        assertEquals(gps(i).fill, None)
        assert(gps(i).lineType.dash.isDefined, s"$style: outside mark is not dashed")
      }
      scene.marks.filter(_.window == WindowSide.Inside).foreach { m =>
        assert(gps(m.batchIndex).fill.isDefined)
        assertEquals(gps(m.batchIndex).lineType.dash, None)
      }
    }
  }

  test("role marks: query filled circle, matched diamond, control hollow and cased") {
    import intaglio.PointShape
    def batch(style: MarkStyle) =
      val scene       = right(TrialScene(input(Display.BlankWithFixationCross, marks = style)))
      val (_, device) = lower(scene, right(PlotSurface(800, 600, 1.0)))
      primitives(device.elements).collect {
        case DevicePrimitive.PointBatch(_, _, shapes, gps, Some(n)) => (n.value, shapes, gps)
      }
    val query   = batch(MarkStyle.Role(TrialRole.Query))
    val matched = batch(MarkStyle.Role(TrialRole.Matched))
    val control = batch(MarkStyle.Role(TrialRole.Control))
    assertEquals(query.map(_._1), Vector(TrialScene.MarksName))
    assertEquals(query.head._2.valueAt(0), PointShape.Circle)
    assertEquals(
      query.head._3.valueAt(0).fill,
      Some(IntaglioColours.themed(Theme.Light, eyes4s.studio.app.tokens.ThemedToken.Query))
    )
    assertEquals(matched.head._2.valueAt(0), PointShape.Diamond)
    assertEquals(control.map(_._1), Vector(TrialScene.CasingName, TrialScene.MarksName))
    val mark = control.last._3.valueAt(0)
    assertEquals(mark.fill, None)
    assertEquals(
      mark.stroke,
      Some(IntaglioColours.themed(Theme.Light, eyes4s.studio.app.tokens.ThemedToken.Control))
    )
  }

  test("mark area is proportional to duration") {
    val scene = right(TrialScene(input(Display.Blank)))
    val pairs = scene.marks.map(m => (m.radiusPx, ret07Fixations(m.batchIndex).durationMs))
    val ratio = pairs.map((r, d) => r * r / d)
    ratio.foreach(q => assertEqualsDouble(q, ratio.head, 1e-9))
  }

  // ---------------------------------------------------------------------------
  // Layout and refusals
  // ---------------------------------------------------------------------------

  test("fit keeps the canvas aspect inside the available area") {
    val scene = right(
      TrialScene(input(Display.Blank, options = TrialSceneOptions(extent = TrialExtent.Screen)))
    )
    val aspect = scene.aspect
    assertEqualsDouble(aspect, 1920.0 / (1080.0 / (1.0 - TrialScene.CaptionFraction)), 1e-9)
    val (w, h) = TrialScene.fit(1000, 300, aspect)
    assertEqualsDouble(h, 300.0, 1e-9)
    assertEqualsDouble(w / h, aspect, 1e-9)
    val (w2, h2) = TrialScene.fit(300, 1000, aspect)
    assertEqualsDouble(w2, 300.0, 1e-9)
    assertEqualsDouble(w2 / h2, aspect, 1e-9)
    assertEquals(TrialScene.fit(0, 100, aspect), (0.0, 0.0))
  }

  test("the gaze extent covers the frame and every fixation, with a margin") {
    val e = right(TrialScene(input(Display.Blank))).extent
    assertEqualsDouble(e.left, 234.7 - TrialScene.GazeMarginPx, 1e-9)
    assertEqualsDouble(e.top, 156.0 - TrialScene.GazeMarginPx, 1e-9)
    assertEqualsDouble(e.right, 1472.0 + TrialScene.GazeMarginPx, 1e-9)
    assertEqualsDouble(e.bottom, 924.0 + TrialScene.GazeMarginPx, 1e-9)
  }

  test("refused inputs name the trial and the value") {
    val i1 = right(FixationIndex.of(1))
    // Double.toString differs between the JVM and Scala.js: compare the value,
    // then the platform-independent part of its message.
    val nonFinite = TrialFixation.of(ret07, i1, Double.NaN, 2.5, 100, WindowSide.Inside)
    nonFinite match
      case Left(TrialSceneError.NonFinitePosition(trial, 1, x, 2.5)) =>
        assertEquals(trial, ret07)
        assert(x.isNaN)
      case other => fail(s"unexpected $other")
    val named = "Trial P17 · ret_07: fixation 1 is at (NaN, 2.5)"
    assert(nonFinite.left.exists(_.message.startsWith(named)), nonFinite.toString)
    assertEquals(
      TrialFixation.of(ret07, i1, 1.0, 2.0, 0, WindowSide.Inside),
      Left(TrialSceneError.DurationNotPositive(ret07, 1, 0))
    )
    val twice = ret07Fixations
      .take(2)
      .map(f =>
        right(TrialFixation.of(ret07, i1, f.screenX, f.screenY, f.durationMs, f.window))
      )
    assertEquals(
      TrialScene(input(Display.Blank, fixations = twice)).map(_.caption),
      Left(TrialSceneError.DuplicateFixation(ret07, 1))
    )
    assertEquals(
      ScreenRect.of(10, 10, 10, 20),
      Left(TrialSceneError.EmptyExtent(10, 10, 10, 20))
    )
  }

  test("a trial with no fixations draws its display and no marks") {
    val scene =
      right(TrialScene(input(Display.BlankWithFixationCross, fixations = Vector.empty)))
    val (_, device) = lower(scene, right(PlotSurface(640, 480, 1.0)))
    assertEquals(scene.marks, Vector.empty)
    assert(!primitives(device.elements).exists(p => named(p).contains(TrialScene.MarksName)))
  }
