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

import eyes4s.studio.app.plot.*
import eyes4s.studio.app.tokens.{Colour, Theme, ThemedToken, Tokens}
import eyes4s.studio.app.{AppEffect, AppModel, HoverAt, Intent}
import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.selection.{FixationIndex, StudioRef, ViewId}
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.harness.{FxStage, StageSize, StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.runtime.{EffectPerformer, StudioRuntime}
import eyes4s.studio.desktop.typography.StudioFonts
import eyes4s.studio.viz.plot.{DataPoint, PlotTargets, TimelinePlot}
import intaglio.DevicePoint
import javafx.event.Event
import javafx.event.EventType
import javafx.geometry.Point2D
import javafx.scene.SnapshotParameters
import javafx.scene.control.Label
import javafx.scene.image.WritableImage
import javafx.scene.input.{MouseButton, MouseEvent, PickResult}
import javafx.scene.layout.{HBox, Priority}
import javafx.scene.transform.Transform

import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

/** The timeline in the plot host (ticket S4.5e), on real JavaFX: a bar per
  * fixation, and a drag across the plot that selects, in the plot and its
  * table, exactly the fixations whose onset lies in the dragged span.
  */
class S45ePlotFxSuite extends StudioFxSuite:

  override protected def stageSize: StageSize = StageSize(1200, 300)

  override val munitTimeout: Duration = Duration(300, "s")

  override def beforeAll(): Unit =
    super.beforeAll()
    val problems = runOnFx(StudioFonts.loadAll())
    assert(problems.isEmpty, problems.map(_.message).mkString("\n"))

  private val TimeoutMillis = 120000L

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val plotView  = right(ViewId.of("explore.timeline"))
  private val tableView = right(ViewId.of("explore.timeline.table"))
  private val columns   = right(TimelineColumns.standard)
  private val trial     = TrialKey("P17", Phase.Encoding, "enc_03", 1)

  /** The Explore board's 13 fixations (onset, duration ms): test values in
    * the board's rhythm, not results.
    */
  private val intervals: Vector[(Long, Long)] = Vector(
    (0L, 180L),
    (336L, 320L),
    (812L, 260L),
    (1228L, 380L),
    (1764L, 240L),
    (2160L, 412L),
    (2652L, 200L),
    (2932L, 350L),
    (3362L, 220L),
    (3662L, 300L),
    (4042L, 260L),
    (4382L, 110L),
    (4572L, 360L)
  )

  private lazy val timeline: Timeline = right(
    Timeline.of(
      trial,
      intervals.zipWithIndex.map { case ((on, du), k) =>
        TimelineFixation(right(FixationIndex.of(k + 1)), on, du)
      }
    )
  )

  private def ref(i: Int): StudioRef = StudioRef.Fixation(trial, right(FixationIndex.of(i)))

  // --- The host and the app loop ------------------------------------------------------

  private final class Wired(fx: FxStage):
    private val none = new EffectPerformer:
      def perform(effect: AppEffect, dispatch: Intent => Unit): Unit = ()
    val runtime = StudioRuntime(right(AppModel.newProject), none)
    val twin    = runOnFx(
      right(
        PlotTwin.attach(
          TimelinePlot(columns, playheadMs = Some(2160.0)),
          plotView,
          tableView,
          runtime.model.selection,
          i => runtime.dispatch(i)
        )
      )
    )
    runOnFx(runtime.listen(m => twin.project(m.selection)))
    val brush                       = runOnFx(PlotBrushAdapter.attach(twin))
    def host: CanvasPlotHost        = twin.plotHost
    def selected: Vector[StudioRef] = runOnFx(runtime.model.selection.selected)
    def hover: Option[HoverAt]      = runOnFx(runtime.model.hover)
    def readout: Option[String]     = runOnFx(twin.readoutText)

    val root: HBox = runOnFx {
      val box = HBox(twin.plotNode, twin.tableNode)
      HBox.setHgrow(twin.plotNode, Priority.ALWAYS)
      twin.plotNode.setPrefWidth(660)
      twin.tableNode.setPrefWidth(520)
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

  private def showAndDraw(w: Wired, shown: PlotSource): PlotTargets =
    runOnFx {
      w.host.setOutputScaleOverride(Some(1.0))
      w.twin.show(shown, Theme.Light)
    }
    val deadline                     = System.currentTimeMillis + TimeoutMillis
    def settled: Option[PlotTargets] = runOnFx {
      w.root.applyCss()
      w.root.layout()
      (w.twin.status.get, w.host.status.get) match
        case (PlotTwinStatus.Shown(plot), PlotHostStatus.Drawn(frame))
            if (frame.plan.scene eq plot.plot.scene) && frame.surface.deviceScale == 1.0 &&
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

  private def fire(
      w: Wired,
      t: PlotTargets,
      at: DevicePoint,
      still: Boolean,
      kinds: EventType[MouseEvent]*
  ): Unit =
    val c       = t.transform.deviceToCanvas(at)
    val inScene = runOnFx(w.host.localToScene(Point2D(c.x, c.y)))
    runOnFx {
      kinds.foreach { kind =>
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
              if kind == MouseEvent.MOUSE_MOVED || kind == MouseEvent.MOUSE_EXITED then
                MouseButton.NONE
              else MouseButton.PRIMARY,
              if kind == MouseEvent.MOUSE_MOVED || kind == MouseEvent.MOUSE_EXITED then 0
              else 1,
              false,
              false,
              false,
              false,
              kind == MouseEvent.MOUSE_PRESSED,
              false,
              false,
              true,
              false,
              still,
              PickResult(w.host, inScene.getX, inScene.getY)
            )
          )
        finally firing = false
      }
    }

  private def hoverAt(w: Wired, t: PlotTargets, at: DevicePoint): Unit =
    fire(w, t, at, true, MouseEvent.MOUSE_MOVED)

  private def click(w: Wired, t: PlotTargets, at: DevicePoint): Unit =
    fire(
      w,
      t,
      at,
      true,
      MouseEvent.MOUSE_PRESSED,
      MouseEvent.MOUSE_RELEASED,
      MouseEvent.MOUSE_CLICKED
    )

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
    runOnFx(w.twin.table.rowNodes.map(_.getChildren.asScala.toVector.collect { case l: Label =>
      l.getText
    }))

  // Where a mark's row is placed, in data coordinates: its anchor without its nudge.
  private def placed(t: PlotTargets, ref: StudioRef) =
    val target = right(t.target(ref).toRight(s"no mark for $ref"))
    val nudge  = target.mark.nudgePx
    t.transform.deviceToData(
      DevicePoint(target.anchor.x - nudge.dxPx, target.anchor.y - nudge.dyPx)
    )

  private def anchor(t: PlotTargets, ref: StudioRef): DevicePoint =
    right(t.target(ref).toRight(s"no mark for $ref")).anchor

  private def rowSelected(w: Wired): Vector[Boolean] =
    runOnFx(w.twin.table.rowNodes.map(_.getPseudoClassStates.contains(TableTwinView.Selected)))

  private def device(t: PlotTargets, ms: Double): DevicePoint =
    right(t.transform.dataToDevice(DataPoint(ms, 20.0)))

  // A drag from `a` to `b` ms: press, drag, release, and the click JavaFX
  // sends after a release, none of them still since the press.
  private def drag(w: Wired, t: PlotTargets, a: Double, b: Double): Unit =
    fire(w, t, device(t, a), true, MouseEvent.MOUSE_PRESSED)
    fire(w, t, device(t, (a + b) / 2.0), false, MouseEvent.MOUSE_DRAGGED)
    fire(w, t, device(t, b), false, MouseEvent.MOUSE_RELEASED, MouseEvent.MOUSE_CLICKED)

  fxStage.test("a brush selects exactly the fixations that begin in it, in plot and table") {
    fx =>
      val source = right(Timeline.source(timeline, columns))
      val w      = Wired(fx)
      val t      = showAndDraw(w, source)
      assertEquals(t.targets.map(_.ref), timeline.refs)
      assertEquals(rowTexts(w), source.rows.map(source.cellsOf))
      // Each bar sits at its onset by its duration, painted ink-3.
      intervals.zipWithIndex.foreach { case ((on, du), k) =>
        val at = placed(t, ref(k + 1))
        assertEqualsDouble(at.x, on.toDouble, 1e-6)
        assertEqualsDouble(at.y, du.toDouble, 1e-6)
      }
      val image = snapshot(w)
      val mid   = right(t.transform.dataToDevice(DataPoint(2160.0 + 206.0, 100.0)))
      assert(near(image, mid, Tokens.themed(Theme.Light, ThemedToken.Ink3), 40), "no bar")
      // Hovering a bar says its fixation's onset and duration.
      hoverAt(w, t, mid)
      assertEquals(w.hover, Some(HoverAt(plotView, ref(6))))
      assertEquals(w.readout, Some("Fixation 6, Onset ms 2,160, Duration ms 412"))
      assert(anchor(t, ref(6)).x < mid.x)

      // The board's brush, 1.20 to 2.80 s: fixations 4 to 7 begin in it.
      drag(w, t, 1200.0, 2800.0)
      val brushed = runOnFx(w.brush.brushed).getOrElse(fail("no brush"))
      // The span is where the pointer was, to within one device pixel.
      val perPx = t.transform.deviceToData(DevicePoint(1.0, 0.0)).x -
        t.transform.deviceToData(DevicePoint(0.0, 0.0)).x
      assertEqualsDouble(brushed.span.from, 1200.0, perPx)
      assertEqualsDouble(brushed.span.until, 2800.0, perPx)
      // The selection is exactly the rows whose onset the span holds.
      assertEquals(w.selected, PlotBrush.rows(source, columns.onset, brushed.span))
      val expected = intervals.zipWithIndex.collect {
        case ((on, _), k) if on >= 1200 && on < 2800 => ref(k + 1)
      }
      assertEquals(expected, Vector(ref(4), ref(5), ref(6), ref(7)))
      assertEquals(w.selected, expected)
      assertEquals(rowSelected(w), source.rows.map(r => expected.contains(r.ref)))
      // Each selected bar is ringed.
      assertEquals(
        runOnFx(w.twin.input.state.selectionRings(t)).map(_.ref),
        expected
      )
      fx.snapshot(StudioTheme.Light)

      // Dragged backwards over 0.30 to 1.00 s: fixations 2 and 3.
      drag(w, t, 1000.0, 300.0)
      assertEquals(w.selected, Vector(ref(2), ref(3)))

      // A brush in which no fixation begins clears the selection, though
      // fixation 8 runs through it.
      drag(w, t, 3000.0, 3300.0)
      assertEquals(w.selected, Vector.empty)

      // A still click still picks one bar.
      click(w, t, right(t.transform.dataToDevice(DataPoint(2160.0 + 206.0, 100.0))))
      assertEquals(w.selected, Vector(ref(6)))
      runOnFx(w.brush.dispose())
      runOnFx(w.twin.dispose())
  }
