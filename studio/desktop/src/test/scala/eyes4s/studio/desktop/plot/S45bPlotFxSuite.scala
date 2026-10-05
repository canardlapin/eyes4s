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

package eyes4s.studio.desktop.plot

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.studio.app.plot.*
import eyes4s.studio.app.text.{Format, LadderText, LadderTextId}
import eyes4s.studio.app.tokens.{Colour, Theme, ThemedToken, Tokens}
import eyes4s.studio.app.{AppEffect, AppModel, Intent}
import eyes4s.studio.core.backend.{PairDesign, Phase, ResultAddress, RunId, TrialKey}
import eyes4s.studio.core.fixture.{FakeStudyBackend, MockStudy, StoryMoment}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef, ViewId}
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.harness.{FxStage, StageSize, StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.runtime.{EffectPerformer, StudioRuntime}
import eyes4s.studio.desktop.typography.StudioFonts
import eyes4s.studio.viz.plot.{LadderBins, PlotTargets, RingKind, RowMarking, ScaleLadderPlot}
import intaglio.DevicePoint
import io.circe.Json
import javafx.event.Event
import javafx.geometry.Point2D
import javafx.scene.SnapshotParameters
import javafx.scene.image.WritableImage
import javafx.scene.input.{MouseButton, MouseEvent, PickResult}
import javafx.scene.layout.{HBox, Priority}
import javafx.scene.transform.Transform

import scala.concurrent.duration.Duration

/** The scale ladder in the plot host (ticket S4.5b), on real JavaFX: the
  * fake backend's ladder of the fixture's focus query shows, on its focus
  * row and in its table, exactly fixture.json's M, B, D and 2° control
  * scores; a scale with 60 controls falls back to a histogram whose bars
  * select the controls they stand for and read out their bin.
  */
class S45bPlotFxSuite extends StudioFxSuite:

  override protected def stageSize: StageSize = StageSize(1200, 560)

  override val munitTimeout: Duration = Duration(300, "s")

  override def beforeAll(): Unit =
    super.beforeAll()
    val problems = runOnFx(StudioFonts.loadAll())
    assert(problems.isEmpty, problems.map(_.message).mkString("\n"))

  private val TimeoutMillis = 120000L

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val plotView  = right(ViewId.of("compare.scale-ladder"))
  private val tableView = right(ViewId.of("compare.scale-ladder.table"))
  private val columns   = right(LadderColumns.standard)
  private val run       = RunId(7)
  private val focus     = MockStudy.key("P17", "ret_07")

  /** The focus query's ladder as the fake backend serves it at story moment t2. */
  private lazy val fixtureLadder: ScaleLadder =
    FakeStudyBackend
      .create[IO](StoryMoment.T2)
      .flatMap { fake =>
        fake.result(run).flatMap { summary =>
          ScaleLadder.load[IO](fake.inspect, fake.navigator)(run, focus, right(summary).scales)
        }
      }
      .unsafeRunSync()
      .fold(e => fail(e.message), identity)

  // fixture.json's record of the focus query, read without the backend.
  private lazy val fixtureFocus: Json =
    val json = right(io.circe.parser.parse(MockStudy.fixtureText))
    val all  = right(json.hcursor.downField("participants").as[Vector[Json]])
    all
      .filter(_.hcursor.get[String]("id").contains("P17"))
      .flatMap(p => p.hcursor.get[Vector[Json]]("queries").getOrElse(Vector.empty))
      .find(_.hcursor.get[String]("trial").contains("ret_07"))
      .getOrElse(fail("fixture.json has no P17 ret_07"))

  // --- The host and the app loop ------------------------------------------------------

  private final class Wired(fx: FxStage, focusScale: Option[String]):
    private val none = new EffectPerformer:
      def perform(effect: AppEffect, dispatch: Intent => Unit): Unit = ()
    val runtime = StudioRuntime(right(AppModel.newProject), none)
    val twin    = runOnFx(
      right(
        PlotTwin.attach(
          ScaleLadderPlot(columns, focusScale),
          plotView,
          tableView,
          runtime.model.selection,
          i => runtime.dispatch(i)
        )
      )
    )
    runOnFx(runtime.listen(m => twin.project(m.selection)))
    def host: CanvasPlotHost        = twin.plotHost
    def selected: Vector[StudioRef] = runOnFx(runtime.model.selection.selected)

    val root: HBox = runOnFx {
      val box = HBox(twin.plotNode, twin.tableNode)
      HBox.setHgrow(twin.plotNode, Priority.ALWAYS)
      twin.plotNode.setPrefWidth(640)
      twin.tableNode.setPrefWidth(540)
      box.getStyleClass.add("es")
      box.getStylesheets.setAll(
        right(StudioStyles.stylesheets(Theme.Light).left.map(_.message))*
      )
      box
    }
    fx.show(root)
    runOnFx {
      fx.scene.addEventFilter(MouseEvent.ANY, (e: MouseEvent) => if !firing then e.consume())
    }

  private var firing = false

  private def showAndDraw(w: Wired, shown: PlotSource, k: Double = 1.0): PlotTargets =
    runOnFx {
      w.host.setOutputScaleOverride(Some(k))
      w.twin.show(shown, Theme.Light)
    }
    val deadline                     = System.currentTimeMillis + TimeoutMillis
    def settled: Option[PlotTargets] = runOnFx {
      w.root.applyCss()
      w.root.layout()
      (w.twin.status.get, w.host.status.get) match
        case (PlotTwinStatus.Shown(plot), PlotHostStatus.Drawn(frame))
            if (frame.plan.scene eq plot.plot.scene) && frame.surface.deviceScale == k &&
              frame.surface.logicalWidth == w.host.getWidth &&
              frame.surface.logicalHeight == w.host.getHeight =>
          w.twin.input.targets
        case (PlotTwinStatus.Refused(_, e), _) => fail(e.message)
        case _                                 => None
    }
    var result = settled
    while result.isEmpty do
      if System.currentTimeMillis > deadline then
        fail(s"not drawn within $TimeoutMillis ms: ${runOnFx(w.twin.status.get)}")
      Thread.sleep(5)
      result = settled
    result.get

  private def click(w: Wired, t: PlotTargets, at: DevicePoint): Unit =
    val c       = t.transform.deviceToCanvas(at)
    val local   = Point2D(c.x, c.y)
    val inScene = runOnFx(w.host.localToScene(local))
    runOnFx {
      List(MouseEvent.MOUSE_PRESSED, MouseEvent.MOUSE_RELEASED, MouseEvent.MOUSE_CLICKED)
        .foreach { kind =>
          firing = true
          try
            Event.fireEvent(
              w.host,
              MouseEvent(
                kind,
                inScene.getX,
                inScene.getY,
                inScene.getX,
                inScene.getY,
                MouseButton.PRIMARY,
                1,
                false,
                false,
                false,
                false,
                kind == MouseEvent.MOUSE_PRESSED,
                false,
                false,
                true,
                false,
                true,
                PickResult(w.host, inScene.getX, inScene.getY)
              )
            )
          finally firing = false
        }
    }

  private def snapshot(w: Wired): WritableImage =
    runOnFx {
      val params = SnapshotParameters()
      params.setTransform(Transform.scale(1.0, 1.0))
      w.host.snapshot(params, null)
    }

  private def near(image: WritableImage, p: DevicePoint, c: Colour, tol: Int): Boolean =
    val px = math.floor(p.x).toInt
    val py = math.floor(p.y).toInt
    px >= 0 && py >= 0 && px < image.getWidth.toInt && py < image.getHeight.toInt && {
      val argb = image.getPixelReader.getArgb(px, py)
      math.abs(((argb >> 16) & 0xff) - c.red) <= tol &&
      math.abs(((argb >> 8) & 0xff) - c.green) <= tol && math.abs((argb & 0xff) - c.blue) <= tol
    }

  private def rowTexts(w: Wired): Vector[Vector[String]] =
    assert(runOnFx(w.twin.table.onScreen), "the table draws no rows")
    runOnFx(w.twin.table.rowTexts)

  private def rowSelected(w: Wired): Vector[Boolean] =
    assert(runOnFx(w.twin.table.onScreen), "the table draws no rows")
    runOnFx(w.twin.table.rowSelected)

  // --- The fixture's focus row --------------------------------------------------------

  fxStage.test("the focus row and its table show fixture.json's M, B, D and control scores") {
    fx =>
      val ladder = fixtureLadder
      val source = right(ScaleLadder.source(ladder, columns))
      val w      = Wired(fx, Some("2°"))
      val t      = showAndDraw(w, source)
      val plot   = runOnFx(w.twin.plot).getOrElse(fail("no plot"))
      val at2    = ladder.scales.find(_.label == "2°").getOrElse(fail("no 2° scale"))
      val scale2 = FakeStudyBackend.FocusScale
      assertEquals(at2.label, scale2)
      val i2                     = ladder.scales.indexOf(at2)
      def fixture(field: String) =
        right(fixtureFocus.hcursor.get[Vector[Double]](field))(i2)
      val scores = right(fixtureFocus.hcursor.get[Vector[Json]]("control_scores_2deg")).map {
        c =>
          (right(c.hcursor.get[String]("trial")), right(c.hcursor.get[Double]("cos")))
      }
      assertEquals(scores.size, 19)

      // The table writes the focus row's values as fixture.json has them.
      val texts = rowTexts(w)
      assertEquals(texts, source.rows.map(source.cellsOf))
      def cells(ref: StudioRef) = texts(right(source.rowOf(ref).toRight(ref)))
      assertEquals(cells(at2.matched)(4), Format.decimal(fixture("M"), 2))
      assertEquals(cells(at2.mean)(4), Format.decimal(fixture("B"), 2))
      assertEquals(cells(at2.contrast)(5), Format.signed(fixture("D"), 2))
      assertEquals(
        at2.controls.map(c => (c.reference.trial, cells(c.ref)(4))).sortBy(_._1),
        scores.map((trial, cos) => (trial, Format.decimal(cos, 2))).sortBy(_._1)
      )

      // The plot places B, M and every control at those values on the 2° row.
      val level = ScaleLadderPlot.scaleLevels(source, columns.scale).indexOf(scale2).toDouble
      def placedX(ref: StudioRef): Double =
        val target = right(t.target(ref).toRight(s"no mark for $ref"))
        val nudge  = target.mark.nudgePx
        val back   = t.transform.deviceToData(
          DevicePoint(target.anchor.x - nudge.dxPx, target.anchor.y - nudge.dyPx)
        )
        assertEqualsDouble(back.y, level, 1e-6)
        back.x
      assertEqualsDouble(placedX(at2.matched), fixture("M"), 1e-6)
      assertEqualsDouble(placedX(at2.mean), fixture("B"), 1e-6)
      assertEquals(
        at2.controls.map(c => (c.reference.trial, placedX(c.ref))).sortBy(_._1).map(_._1),
        scores.sortBy(_._1).map(_._1)
      )
      at2.controls.foreach { c =>
        val served =
          scores.find(_._1 == c.reference.trial).map(_._2).getOrElse(fail(c.toString))
        assertEqualsDouble(placedX(c.ref), served, 1e-6)
      }
      // Beeswarm: close controls are nudged at most 7 px off the row.
      at2.controls.foreach { c =>
        val dy = right(t.target(c.ref).toRight(c.ref)).mark.nudgePx.dyPx
        assert(Set(0.0, 7.0, -7.0).contains(dy), s"${c.reference.trial} nudged $dy")
      }
      assert(at2.controls.exists(c => t.target(c.ref).exists(_.mark.nudgePx.dyPx != 0.0)))

      // D is drawn between B and M: its mark is positionless and it reads out D.
      val dMark = right(plot.markOf(at2.contrast).toRight("no D mark"))
      assertEquals(
        dMark.rows.map(_.marking),
        Vector(
          RowMarking.Positionless(
            eyes4s.studio.viz.plot.NoPosition.MissingValue(columns.cosine)
          )
        )
      )
      runOnFx(w.twin.input.moveFocus(Some(at2.contrast)))
      assertEquals(
        runOnFx(t.accessibleText(w.twin.input.state)),
        right(source.rowOf(at2.contrast).flatMap(source.rowText).toRight("no D row"))
      )

      // M's diamond and B's tick are painted in their tokens.
      val image = snapshot(w)
      assert(
        near(
          image,
          right(t.target(at2.matched).toRight("M")).anchor,
          Tokens.themed(Theme.Light, ThemedToken.Match),
          40
        ),
        "no M diamond"
      )
      val tick = right(t.target(at2.mean).toRight("B")).anchor
      assert(
        near(
          image,
          DevicePoint(tick.x, tick.y - 10.0),
          Tokens.themed(Theme.Light, ThemedToken.Control),
          40
        ),
        "no B tick"
      )

      // Clicking M selects M's row and nothing else.
      click(w, t, right(t.target(at2.matched).toRight("M")).anchor)
      assertEquals(w.selected, Vector(at2.matched))
      assertEquals(rowSelected(w), source.rows.map(_.ref == at2.matched))
      fx.snapshot(StudioTheme.Light)
      runOnFx(w.twin.dispose())
  }

  // --- The histogram fallback ---------------------------------------------------------

  /** A query with 60 controls at one scale: test values, not results. */
  private lazy val sixty: ScaleLadder =
    val scale    = right(ScaleIndex.of(0))
    val query    = TrialKey("P01", Phase.Retrieval, "ret_01", 1)
    val controls = Vector.tabulate(60) { k =>
      val trial = TrialKey("P01", Phase.Encoding, f"enc_$k%02d", 1)
      LadderControl(
        StudioRef.Pair(run, scale, PairDesign.Control, query, trial),
        trial,
        Some(f"item-$k%02d"),
        Some(0.12 + 0.75 * k / 60.0)
      )
    }
    val matched = TrialKey("P01", Phase.Encoding, "enc_60", 1)
    ScaleLadder(
      run,
      query,
      Vector(
        LadderScale(
          scale,
          "2°",
          StudioRef.Pair(run, scale, PairDesign.Matched, query, matched),
          matched,
          "beach-001",
          0.81,
          right(
            StudioRef.fromAddress(run, ResultAddress.Reduction(0, PairDesign.Control, query))
          ),
          0.49,
          60,
          StudioRef.QueryContrast(run, scale, query),
          0.32,
          controls
        )
      )
    )

  fxStage.test(
    "60 controls fall back to a histogram; a bar selects its controls and names its bin"
  ) { fx =>
    val source = right(ScaleLadder.source(sixty, columns))
    val w      = Wired(fx, Some("2°"))
    val t      = showAndDraw(w, source)
    val plot   = runOnFx(w.twin.plot).getOrElse(fail("no plot"))
    val bars   = t.targets.filter(_.mark.rows.forall(_.marking == RowMarking.Represented))
    assert(bars.size > 1, bars.size)
    // Every control is represented by exactly one bar; none is drawn as a dot.
    assertEquals(
      bars.flatMap(_.refs).sortBy(r => source.rowOf(r)),
      sixty.scales.head.controls.map(_.ref)
    )
    assertEquals(t.targets.size, bars.size + 3)
    // The table still lists all 63 rows.
    assertEquals(rowTexts(w), source.rows.map(source.cellsOf))

    val bar     = bars.maxBy(_.refs.size)
    val k       = LadderBins.of(bar.mark.at.x)
    val summary = LadderText(
      LadderTextId.Bin,
      "2°",
      Format.decimal(LadderBins.lower(k), 2),
      Format.decimal(LadderBins.upper(k), 2)
    )
    runOnFx(w.twin.input.moveFocus(Some(bar.ref)))
    assertEquals(
      runOnFx(t.accessibleText(w.twin.input.state)),
      PlotText(PlotTextId.MarkRows, bar.refs.size.toString, summary)
    )
    // The bar is painted in the control tokens, and a click selects its controls.
    val image = snapshot(w)
    assert(
      near(image, bar.anchor, Tokens.themed(Theme.Light, ThemedToken.ControlSoft), 40),
      "no bar"
    )
    click(w, t, bar.anchor)
    assertEquals(w.selected, bar.refs)
    assertEquals(rowSelected(w), source.rows.map(r => bar.refs.contains(r.ref)))
    assertEquals(
      runOnFx(w.twin.input.state.selectionRings(t)).map(r => (r.kind, r.ref)),
      Vector((RingKind.Selected, bar.ref))
    )
    // Each of its controls is accounted for by that bar.
    bar.refs.foreach(r => assertEquals(plot.markOf(r).map(_.ref), Some(bar.ref)))
    fx.snapshot(StudioTheme.Light)
    runOnFx(w.twin.dispose())
  }
