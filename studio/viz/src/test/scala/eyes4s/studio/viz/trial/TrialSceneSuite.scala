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

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
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

  private def isMark(p: DevicePrimitive): Boolean =
    named(p).exists(_.startsWith(TrialScene.MarkPrefix))

  // Where a mark primitive is centred: a point batch's point, a ring's centroid.
  private def centre(p: DevicePrimitive): DevicePoint = p match
    case DevicePrimitive.PointBatch(points, _, _, _, _) => points.head
    case DevicePrimitive.Polyline(points, true, _, _)   =>
      DevicePoint(points.map(_.x).sum / points.size, points.map(_.y).sum / points.size)
    case other => fail(s"a fixation mark is drawn as $other")

  // Each mark's primitive by its grob name.
  private def markPrimitives(prims: Vector[DevicePrimitive]): Map[String, DevicePrimitive] =
    prims.filter(isMark).flatMap(p => named(p).map(_ -> p)).toMap

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
      os <- Gen.listOfN(
        n,
        Gen.oneOf(
          MapPlacement.InWindow,
          MapPlacement.OutsideWindow(OffWindowPolicy.Exclude),
          MapPlacement.OutsideWindow(OffWindowPolicy.FailTrial),
          MapPlacement.OutsideScreen,
          MapPlacement.DroppedInitial
        )
      )
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

      // Every mark is its own named grob, drawn where the transform puts its
      // fixation.
      val drawn = markPrimitives(prims)
      assertEquals(prims.count(isMark), fs.size)
      assertEquals(drawn.size, fs.size)
      scene.marks.foreach { mark =>
        val at = centre(drawn(mark.name.value))
        assert(near(right(transform.dataToDevice(mark.at)), at), s"$mark drawn at $at")
        assertEquals(mark.at, DataPoint(fs(mark.order).screenX, fs(mark.order).screenY))
        // A canvas position maps back to the fixation (the picking direction).
        val back = transform.canvasToData(transform.deviceToCanvas(at))
        assertEqualsDouble(back.x, mark.at.x, 1e-6)
        assertEqualsDouble(back.y, mark.at.y, 1e-6)
        assertEquals[Option[StudioRef], Option[StudioRef]](
          scene.refOf(mark.name),
          Some(StudioRef.Fixation(ret07, fs(mark.order).index))
        )
        assertEquals(mark.name.value, TrialScene.markName(fs(mark.order).index))
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
      assertEquals(prims.count(isMark), ret07Fixations.size)
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

  test("the core placement states have distinct scene treatments") {
    List(MarkStyle.Neutral, MarkStyle.Role(TrialRole.Query)).foreach { style =>
      val scene       = right(TrialScene(input(Display.BlankWithFixationCross, marks = style)))
      val (_, device) = lower(scene, right(PlotSurface(800, 600, 1.0)))
      val gps         = markPrimitives(primitives(device.elements)).collect {
        case (n, DevicePrimitive.PointBatch(_, _, _, gps, _)) => n -> gps.valueAt(0)
      }
      val outside =
        scene.marks.filter(_.placement == MapPlacement.OutsideWindow(OffWindowPolicy.Exclude))
      assertEquals(outside.map(_.order), Vector(8))
      outside.foreach { m =>
        assertEquals(gps(m.name.value).fill, None)
        assert(
          gps(m.name.value).lineType.dash.isDefined,
          s"$style: excluded mark is not dashed"
        )
      }
      scene.marks.filter(_.placement == MapPlacement.InWindow).foreach { m =>
        assert(gps(m.name.value).fill.isDefined)
        assertEquals(gps(m.name.value).lineType.dash, None)
      }
    }
  }

  test("core provenance distinguishes off-window policies from outside-screen fixation") {
    val screen = right(Frame.screen("screen", 1920, 1080))
    val window = right(
      Subframe.of(screen, FrameId("image"), right(Bounds.of[Px](448, 156, 1472, 924)))
    )
    val retrieval = StudyKey("P17", "beach-042", "recall")
    val encoding  = StudyKey("P17", "beach-042", "encode")
    def path(key: StudyKey, points: (Double, Double)*): Scanpath[Px] =
      val clock = ClockId(s"${key.participant}/${key.phase}")
      val fixes = points.zipWithIndex.map { case ((x, y), i) =>
        right(
          Event.Fixation.withoutDispersion(
            right(
              Interval.of(clock, Instant.micros(i * 1000L), Instant.micros(i * 1000L + 412L))
            ),
            Pt[Px](x, y),
            1
          )
        )
      }
      right(Scanpath.of(screen, clock, IArray.from(fixes)))
    val studyInput = StudyInput(
      Trials(
        Vector(
          Trial(
            retrieval,
            (),
            path(retrieval, (960, 540), (2000, 500), (100, 100), (1148, 456))
          ),
          Trial(encoding, (), path(encoding, (900, 500)))
        )
      )
    )
    def placements(policy: OffWindowPolicy): Vector[MapPlacement] =
      val geometry = right(
        StudyGeometry.windowed(window, right(Grid.over(window.frame, 64, 48)), policy)
      )
      val plan = right(
        StudyPlan.configure(
          studyInput.reference,
          StudyKey.layout(DefinitionId.studyLayout),
          geometry,
          "recall",
          "encode",
          Weight.Duration,
          Vector(StudyScale.Native(StudyEstimate.Binned())),
          None,
          FailurePolicy.RequireAll,
          StudyMethod.cosine[Px](DefinitionId.cosine),
          (),
          initialFixations = InitialFixationPolicy.dropFirst[Px]
        )
      )
      val provenance = right(CoordinateProvenance.of(plan, studyInput, None))
      (0 until 4).toVector.map(i =>
        right(provenance.fixation(retrieval, right(ScanpathPosition.of(i)))).trail.placement
      )
    val excluded = placements(OffWindowPolicy.Exclude)
    val failing  = placements(OffWindowPolicy.FailTrial)
    assertEquals(
      excluded,
      Vector(
        MapPlacement.DroppedInitial,
        MapPlacement.OutsideScreen,
        MapPlacement.OutsideWindow(OffWindowPolicy.Exclude),
        MapPlacement.InWindow
      )
    )
    assertEquals(failing(1), MapPlacement.OutsideScreen)
    assertEquals(failing(2), MapPlacement.OutsideWindow(OffWindowPolicy.FailTrial))
    val states    = excluded.take(3) :+ failing(2) :+ excluded.last
    val fixations = states.zipWithIndex.map { case (placement, i) =>
      right(
        TrialFixation.of(
          ret07,
          right(FixationIndex.of(i + 1)),
          700.0 + i,
          400.0,
          200,
          placement
        )
      )
    }
    assertEquals(fixations.map(_.placement), states)
    assertEquals(
      fixations.filter(_.contributesToMap).map(_.placement),
      Vector(MapPlacement.InWindow)
    )
    val scene       = right(TrialScene(input(Display.Blank, fixations, MarkStyle.Neutral)))
    val (_, device) = lower(scene, right(PlotSurface(800, 600, 1.0)))
    val gps         = markPrimitives(primitives(device.elements)).collect {
      case (n, DevicePrimitive.PointBatch(_, _, _, params, _)) => n -> params.valueAt(0)
    }
    val drawn = scene.marks.map(mark => mark.placement -> gps(mark.name.value))
    assertEquals(drawn.map(_._1), states)
    assert(drawn.take(4).forall(_._2.fill.isEmpty))
    assert(drawn.takeRight(1).forall(_._2.fill.nonEmpty))
    assertEquals(drawn.map(_._2.lineType).distinct.size, states.size)
    // eyes4s marks the failing trial's in-window fixation TrialFailed: it is
    // drawn as the trial's outside fixation is, and adds nothing to the map.
    val trialFailed = failing(3)
    assert(trialFailed.isInstanceOf[MapPlacement.TrialFailed], trialFailed)
    val both = Vector(failing(2), trialFailed).zipWithIndex.map { case (placement, i) =>
      right(
        TrialFixation.of(
          ret07,
          right(FixationIndex.of(i + 1)),
          700.0 + i,
          400.0,
          200,
          placement
        )
      )
    }
    assert(both.forall(!_.contributesToMap))
    val failScene       = right(TrialScene(input(Display.Blank, both, MarkStyle.Neutral)))
    val (_, failDevice) = lower(failScene, right(PlotSurface(800, 600, 1.0)))
    val failParams      = markPrimitives(primitives(failDevice.elements)).collect {
      case (n, DevicePrimitive.PointBatch(_, _, _, params, _)) => n -> params.valueAt(0)
    }
    val drawnFail = failScene.marks.map(mark => failParams(mark.name.value))
    assertEquals(drawnFail.map(_.lineType).distinct.size, 1)
    assertEquals(drawnFail.map(_.fill).distinct.size, 1)
  }

  test("role marks: query filled circle, matched diamond, control hollow and cased") {
    import eyes4s.studio.app.tokens.{StageToken, ThemedToken}
    import intaglio.{CasingWidth, PointShape, StrokeWidth}
    def marks(style: MarkStyle, scale: Double = 1.0) =
      val scene       = right(TrialScene(input(Display.BlankWithFixationCross, marks = style)))
      val (_, device) = lower(scene, right(PlotSurface(800, 600, scale)))
      (scene, primitives(device.elements).filter(isMark))
    val (_, query) = marks(MarkStyle.Role(TrialRole.Query))
    assertEquals(query.size, ret07Fixations.size)
    val shapes = query.collect { case DevicePrimitive.PointBatch(_, _, s, gps, _) =>
      (s.valueAt(0), gps.valueAt(0))
    }
    assertEquals(shapes.size, query.size)
    assertEquals(shapes.head._1, PointShape.Circle)
    assertEquals(
      shapes.head._2.fill,
      Some(IntaglioColours.themed(Theme.Light, ThemedToken.Query))
    )
    val (_, matched) = marks(MarkStyle.Role(TrialRole.Matched))
    val diamonds     = matched.collect { case DevicePrimitive.PointBatch(_, _, s, _, _) =>
      s.valueAt(0)
    }
    assertEquals(diamonds, Vector.fill(matched.size)(PointShape.Diamond))
    // A control mark in the window is one ring whose casing is Intaglio
    // StrokeCasing paint (the halo, 4 px on screen): no second casing grob.
    for scale <- List(1.0, 2.0) do
      val (scene, control) = marks(MarkStyle.Role(TrialRole.Control), scale)
      assertEquals(control.size, ret07Fixations.size)
      val rings = control.collect { case DevicePrimitive.Polyline(points, true, gp, _) =>
        (points, gp)
      }
      assertEquals(rings.size, scene.marks.count(_.placement == MapPlacement.InWindow))
      rings.foreach { (points, gp) =>
        assertEquals(points.size, TrialScene.RingSegments)
        assertEquals(gp.fill, None)
        assertEquals(gp.stroke, Some(IntaglioColours.themed(Theme.Light, ThemedToken.Control)))
        val casing = gp.casing.getOrElse(fail("a control ring has no casing"))
        assertEquals(casing.color, IntaglioColours.staged(StageVariant.Dark, StageToken.Halo))
        assertEquals(
          casing.width,
          CasingWidth.Absolute(StrokeWidth.devicePixelsUnsafe(TrialScene.CasingPx * scale))
        )
      }
      // The ring is the mark's size: its vertices lie at the mark's radius.
      scene.marks.filter(_.placement == MapPlacement.InWindow).zip(rings).foreach { (m, ring) =>
        val c = centre(DevicePrimitive.Polyline(ring._1, true, ring._2, None))
        ring._1.foreach { v =>
          assertEqualsDouble(math.hypot(v.x - c.x, v.y - c.y), m.radiusPx * scale, 1e-6)
        }
      }
  }

  test("mark area is proportional to duration") {
    val scene = right(TrialScene(input(Display.Blank)))
    val pairs = scene.marks.map(m => (m.radiusPx, ret07Fixations(m.order).durationMs))
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
    val nonFinite = TrialFixation.of(ret07, i1, Double.NaN, 2.5, 100, MapPlacement.InWindow)
    nonFinite match
      case Left(TrialSceneError.NonFinitePosition(trial, 1, x, 2.5)) =>
        assertEquals(trial, ret07)
        assert(x.isNaN)
      case other => fail(s"unexpected $other")
    val named = "Trial P17 · ret_07: fixation 1 is at (NaN, 2.5)"
    assert(nonFinite.left.exists(_.message.startsWith(named)), nonFinite.toString)
    assertEquals(
      TrialFixation.of(ret07, i1, 1.0, 2.0, 0, MapPlacement.InWindow),
      Left(TrialSceneError.DurationNotPositive(ret07, 1, 0))
    )
    val twice = ret07Fixations
      .take(2)
      .map(f =>
        right(TrialFixation.of(ret07, i1, f.screenX, f.screenY, f.durationMs, f.placement))
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
    assert(!primitives(device.elements).exists(isMark))
  }

  test("an off-screen fixation mid-trial is drawn where it is, dashed, never clamped") {
    // eyes4s keeps an off-screen fixation in its scanpath (OutsideScreen); it
    // is outside the analysis window too, so the trial view draws it dashed.
    val rows = Vector(
      (900.0, 500.0, 200, MapPlacement.InWindow),
      (-60.0, 1150.0, 180, MapPlacement.OutsideScreen),
      (2000.0, -30.0, 160, MapPlacement.OutsideScreen),
      (1000.0, 600.0, 220, MapPlacement.InWindow)
    )
    val fs = rows.zipWithIndex.map { case ((x, y, d, w), i) =>
      right(TrialFixation.of(ret07, right(FixationIndex.of(i + 1)), x, y, d, w))
    }
    val extents = List(
      TrialExtent.Screen,
      TrialExtent.Gaze,
      TrialExtent.Covering(right(ScreenRect.of(448, 156, 1472, 924)))
    )
    for
      extent <- extents
      style  <- List(MarkStyle.Neutral, MarkStyle.Role(TrialRole.Query))
    do
      val options = TrialSceneOptions(extent = extent)
      val scene   =
        right(TrialScene(input(Display.BlankWithFixationCross, fs, style, options = options)))
      val (transform, device) = lower(scene, right(PlotSurface(800, 600, 1.0)))
      val drawn               = markPrimitives(primitives(device.elements))
      assertEquals(scene.marks.map(_.order), Vector(0, 1, 2, 3), s"$extent")
      assertEquals(drawn.size, fs.size, s"$extent")
      scene.marks.zip(fs).foreach { (mark, f) =>
        assertEquals(mark.at, DataPoint(f.screenX, f.screenY), s"$extent: $mark")
        assert(
          near(right(transform.dataToDevice(mark.at)), centre(drawn(mark.name.value))),
          s"$extent: $mark is not drawn at its own position"
        )
      }
      val gps = drawn.collect { case (n, DevicePrimitive.PointBatch(_, _, _, gps, _)) =>
        n -> gps.valueAt(0)
      }
      scene.marks.filter(_.placement == MapPlacement.OutsideScreen).foreach { m =>
        assertEquals(gps(m.name.value).fill, None)
        assert(gps(m.name.value).lineType.dash.isDefined, s"$extent: ${m.order} is not dashed")
      }
    val gaze = TrialScene.extentOf(
      input(Display.Blank, fs, options = TrialSceneOptions(extent = TrialExtent.Gaze))
    )
    assert(gaze.left < -60.0 && gaze.top < -30.0 && gaze.right > 2000.0 && gaze.bottom > 1150.0)
  }
