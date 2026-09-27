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
import eyes4s.studio.app.{AppEffect, AppModel, Intent}
import eyes4s.studio.core.selection.{
  InputCause,
  InputStamp,
  SelectionInput,
  SelectionMode,
  StudioRef,
  ViewId
}
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.harness.{FxStage, StageSize, StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.runtime.{EffectPerformer, StudioRuntime}
import eyes4s.studio.desktop.typography.StudioFonts
import eyes4s.studio.viz.plot.{DotPlot, PlotTarget, PlotTargets, RingKind}
import intaglio.DevicePoint
import javafx.event.{Event, EventType}
import javafx.geometry.Point2D
import javafx.scene.SnapshotParameters
import javafx.scene.control.Label
import javafx.scene.image.WritableImage
import javafx.scene.input.{KeyCode, MouseButton, MouseEvent, PickResult}
import javafx.scene.layout.{HBox, Priority}
import javafx.scene.transform.Transform

import java.lang.ref.WeakReference
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

/** The common plot host and its TableTwin (S4.5a), on real JavaFX with
  * synthetic events: the plot draws each mark at its table row's values,
  * the table writes every row of the same source, selecting a row selects
  * its mark and selecting a mark its row through a real app loop, each is
  * one keyboard focus stop whose Enter and Escape select and clear, a
  * refused plot still lists its rows, and 200 attach/dispose cycles leak
  * nothing.
  */
class PlotKitFxSuite extends StudioFxSuite:

  override protected def stageSize: StageSize = StageSize(1200, 560)

  override val munitTimeout: Duration = Duration(300, "s")

  override def beforeAll(): Unit =
    super.beforeAll()
    val problems = runOnFx(StudioFonts.loadAll())
    assert(problems.isEmpty, problems.map(_.message).mkString("\n"))

  private val TimeoutMillis = 120000L

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val plotView  = right(ViewId.of("results.participant-plot"))
  private val tableView = right(ViewId.of("results.participant-plot.table"))
  private val otherView = right(ViewId.of("results.participant-table"))

  private def col(id: String, header: String, f: ColumnFormat) =
    PlotColumn(right(ColumnId.of(id)), header, f)
  private val m = col("m", "Mean M", ColumnFormat.Decimal(2))
  private val d = col("d", "Mean D", ColumnFormat.Signed(2))

  /** Twelve participants' test values; P09 has no D, so no mark. Fixed test
    * data for the host, not results.
    */
  private val source: PlotSource = right(
    PlotSource(
      "Participant D by mean M",
      Vector(
        col("participant", "Participant", ColumnFormat.Label),
        col("n", "Contributing", ColumnFormat.Count),
        m,
        col("b", "Mean B", ColumnFormat.Decimal(2)),
        d
      ),
      Vector(
        ("P01", 19, 0.61, 0.33, 0.28),
        ("P02", 20, 0.72, 0.36, 0.36),
        ("P03", 18, 0.55, 0.41, 0.14),
        ("P04", 20, 0.49, 0.47, 0.02),
        ("P05", 17, 0.80, 0.38, 0.42),
        ("P06", 19, 0.66, 0.30, 0.36),
        ("P07", 20, 0.58, 0.61, -0.03),
        ("P08", 16, 0.70, 0.35, 0.35),
        ("P09", 0, 0.52, 0.44, Double.NaN),
        ("P10", 19, 0.77, 0.29, 0.48),
        ("P11", 18, 0.45, 0.40, 0.05),
        ("P17", 19, 0.73, 0.35, 0.38)
      ).map { (p, n, mm, bb, dd) =>
        PlotRow(
          StudioRef.Participant(p),
          Vector(
            PlotValue.Text(p),
            PlotValue.Number(n.toDouble),
            PlotValue.Number(mm),
            PlotValue.Number(bb),
            if dd.isNaN then PlotValue.Missing else PlotValue.Number(dd)
          )
        )
      }
    )
  )

  private val dots = DotPlot(m.id, d.id, "Participants · D by M")

  // --- The host, the app loop and the stage -----------------------------------------

  /** A plot host wired to a real app loop: intents go through
    * `AppModel.update`, and every model's selection is projected back.
    */
  private final class Wired(fx: FxStage, theme: Theme = Theme.Light):
    val emitted      = ArrayBuffer.empty[Intent]
    private val none = new EffectPerformer:
      def perform(effect: AppEffect, dispatch: Intent => Unit): Unit = ()
    val runtime = StudioRuntime(right(AppModel.newProject), none)
    val twin    = runOnFx(
      right(
        PlotTwin.attach(
          dots,
          plotView,
          tableView,
          runtime.model.selection,
          i => { emitted += i; runtime.dispatch(i) }
        )
      )
    )
    runOnFx(runtime.listen(m => twin.project(m.selection)))
    def host: CanvasPlotHost        = twin.plotHost
    def table: TableTwinView        = twin.table
    def selected: Vector[StudioRef] = runOnFx(runtime.model.selection.selected)

    val root: HBox = runOnFx {
      val box = HBox(twin.plotNode, twin.tableNode)
      HBox.setHgrow(twin.plotNode, Priority.ALWAYS)
      twin.plotNode.setPrefWidth(620)
      twin.tableNode.setPrefWidth(560)
      box.getStyleClass.add("es")
      box.getStylesheets.setAll(right(StudioStyles.stylesheets(theme).left.map(_.message))*)
      box
    }
    fx.show(root)
    isolateFromOsPointer(fx)

  /** Shows the source at output scale `k` and waits until it is drawn and targeted. */
  private def showAndDraw(w: Wired, k: Double, theme: Theme = Theme.Light): PlotTargets =
    runOnFx {
      w.host.setOutputScaleOverride(Some(k))
      w.twin.show(source, theme)
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
        case _ => None
    }
    var result = settled
    while result.isEmpty do
      if System.currentTimeMillis > deadline then
        fail(s"not drawn within $TimeoutMillis ms: ${runOnFx(w.twin.status.get)}")
      Thread.sleep(5)
      result = settled
    result.get

  // --- Isolation from the OS pointer (as InputAdapterFxSuite) ------------------------

  private var firing      = false
  private val IsolatedKey = "eyes4s.s45a.os-pointer-isolated"

  private def isolateFromOsPointer(fx: FxStage): Unit =
    runOnFx {
      if !fx.scene.getProperties.containsKey(IsolatedKey) then
        fx.scene.getProperties.put(IsolatedKey, true)
        fx.scene.addEventFilter(MouseEvent.ANY, (e: MouseEvent) => if !firing then e.consume())
    }

  // Fires a synthetic primary-button mouse event at the host, at host-local (x, y).
  private def mouse(w: Wired, kind: EventType[MouseEvent], at: Point2D, toggle: Boolean): Unit =
    val host     = w.host
    val inScene  = host.localToScene(at)
    val onScreen = Option(host.localToScreen(at)).getOrElse(inScene)
    firing = true
    try
      Event.fireEvent(
        host,
        MouseEvent(
          kind,
          inScene.getX,
          inScene.getY,
          onScreen.getX,
          onScreen.getY,
          MouseButton.PRIMARY,
          1,
          toggle,
          false,
          false,
          false,
          kind == MouseEvent.MOUSE_PRESSED,
          false,
          false,
          true,
          false,
          true,
          PickResult(host, inScene.getX, inScene.getY)
        )
      )
    finally firing = false

  private def clickMark(w: Wired, t: PlotTargets, target: PlotTarget, toggle: Boolean = false) =
    val c  = t.transform.deviceToCanvas(target.anchor)
    val at = Point2D(c.x, c.y)
    runOnFx {
      List(MouseEvent.MOUSE_PRESSED, MouseEvent.MOUSE_RELEASED, MouseEvent.MOUSE_CLICKED)
        .foreach(mouse(w, _, at, toggle))
    }

  private def clickRow(fx: FxStage, w: Wired, row: Int): Unit =
    firing = true
    try fx.robot.click(runOnFx(w.table.rowNodes(row)))
    finally firing = false

  private def selectByOther(w: Wired, ref: StudioRef, sequence: Long): Unit =
    runOnFx {
      val context = w.runtime.model.selection.context
      w.runtime.dispatch(
        Intent.Select(
          SelectionInput(
            InputStamp(context, otherView, sequence, InputCause.Pointer),
            SelectionMode.Replace,
            Vector(ref)
          )
        )
      )
    }

  // --- Probes -------------------------------------------------------------------------

  private def snapshotHost(w: Wired, k: Int): WritableImage =
    runOnFx {
      val params = SnapshotParameters()
      params.setTransform(Transform.scale(k.toDouble, k.toDouble))
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
    runOnFx(w.table.rowNodes.map(_.getChildren.asScala.toVector.collect { case l: Label =>
      l.getText
    }))

  private def rowSelected(w: Wired): Vector[Boolean] =
    runOnFx(w.table.rowNodes.map(_.getPseudoClassStates.contains(TableTwinView.Selected)))

  private def rowOf(ref: StudioRef): Int = source.rowOf(ref).getOrElse(fail(s"no row $ref"))

  private def dispose(w: Wired): Unit = runOnFx(w.twin.dispose())

  // --- One source ---------------------------------------------------------------------

  fxStage.test("the plot draws each mark at its row's values; the table writes every row") {
    fx =>
      val w = Wired(fx)
      List(1.0, 2.0).foreach { k =>
        val t = showAndDraw(w, k)
        // The table lists every row, P09 included, written by the source.
        assertEquals(rowTexts(w), source.rows.indices.map(source.cells).toVector)
        assertEquals(runOnFx(w.table.vm).map(_.rows.map(_.ref)), Some(source.rows.map(_.ref)))
        // The plot draws every row with an M and a D, where its values are.
        val plot = runOnFx(w.twin.plot).getOrElse(fail("no plot"))
        assertEquals(
          t.targets.map(_.ref),
          source.rows.map(_.ref).filterNot(_ == StudioRef.Participant("P09"))
        )
        assertEquals(plot.unplotted.map(_.ref), Vector(StudioRef.Participant("P09")))
        val image = snapshotHost(w, k.toInt)
        val dot   = Tokens.themed(Theme.Light, ThemedToken.Ink2)
        t.targets.foreach { target =>
          val row  = rowOf(target.ref)
          val back = t.transform.deviceToData(target.anchor)
          assertEquals(
            source.text(row, 2),
            PlotSource.write(PlotValue.Number(back.x), m.format)
          )
          assertEquals(
            source.text(row, 4),
            PlotSource.write(PlotValue.Number(back.y), d.format)
          )
          assert(near(image, target.anchor, dot, 40), s"${k}x: no dot drawn at ${target.ref}")
        }
      }
      dispose(w)
  }

  // --- Selection sync -----------------------------------------------------------------

  fxStage.test("selecting a row selects its mark; the scene is not redrawn") { fx =>
    val w      = Wired(fx)
    val t      = showAndDraw(w, 2.0)
    val before = runOnFx(w.host.profile)
    source.rows.indices.filterNot(_ == rowOf(StudioRef.Participant("P09"))).foreach { row =>
      val ref = source.rows(row).ref
      clickRow(fx, w, row)
      assertEquals(w.selected, Vector(ref))
      assertEquals(
        runOnFx(w.twin.input.state.selectionRings(t)).map(r => (r.kind, r.ref, r.centre)),
        Vector((RingKind.Selected, ref, t.target(ref).get.anchor))
      )
      assertEquals(rowSelected(w), source.rows.map(_.ref == ref))
      assertEquals(runOnFx(w.table.state.cursor), Some(ref))
    }
    val after = runOnFx(w.host.profile)
    assertEquals((after.compiles, after.baseDraws), (before.compiles, before.baseDraws))
    assert(after.underDraws > before.underDraws, "the selection layer was not redrawn")
    // A row the plot does not draw is selected in the table, with no ring.
    clickRow(fx, w, rowOf(StudioRef.Participant("P09")))
    assertEquals(w.selected, Vector(StudioRef.Participant("P09")))
    assertEquals(runOnFx(w.twin.input.state.selectionRings(t)), Vector.empty)
    assert(w.emitted.forall {
      case Intent.Select(i)       => i.stamp.origin == tableView
      case Intent.HoverOver(v, _) => v == plotView
      case _                      => false
    })
    dispose(w)
  }

  fxStage.test("selecting a mark selects its row; a modifier click toggles it off") { fx =>
    val w = Wired(fx)
    List(1.0, 2.0).foreach { k =>
      val t = showAndDraw(w, k)
      t.targets.foreach { target =>
        clickMark(w, t, target)
        assertEquals(w.selected, Vector(target.ref), s"at ${k}x")
        assertEquals(rowSelected(w), source.rows.map(_.ref == target.ref))
        assertEquals(
          runOnFx(w.table.vm).map(_.rows.filter(_.selected).map(_.ref)),
          Some(Vector(target.ref))
        )
      }
      clickMark(w, t, t.targets.last, toggle = true)
      assertEquals(w.selected, Vector.empty)
      assertEquals(rowSelected(w), source.rows.map(_ => false))
    }
    dispose(w)
  }

  fxStage.test("another view's selection shows in both, and neither echoes it") { fx =>
    val w      = Wired(fx)
    val t      = showAndDraw(w, 1.0)
    val target = t.targets(3)
    selectByOther(w, target.ref, 0L)
    assertEquals(w.emitted.toVector, Vector.empty)
    assertEquals(rowSelected(w), source.rows.map(_.ref == target.ref))
    assertEquals(runOnFx(w.twin.input.state.selectionRings(t)).map(_.ref), Vector(target.ref))
    dispose(w)
  }

  // --- Keyboard -----------------------------------------------------------------------

  fxStage.test("plot and table are each one focus stop: keys alone select, Escape clears") {
    fx =>
      val w = Wired(fx)
      val t = showAndDraw(w, 1.0)
      // The plot: Home, then Page Down in order, Enter.
      runOnFx(w.host.requestFocus())
      assert(runOnFx(w.host.getScene.getFocusOwner eq w.host), "the plot is one focus stop")
      t.targets.foreach { target =>
        fx.robot.press(KeyCode.HOME)
        (0 until target.order).foreach(_ => fx.robot.press(KeyCode.PAGE_DOWN))
        fx.robot.press(KeyCode.ENTER)
        assertEquals(w.selected, Vector(target.ref))
        // The focused mark says what its row says.
        val row = runOnFx(w.table.vm).get.rows(rowOf(target.ref))
        assertEquals(runOnFx(w.host.getAccessibleText), row.accessibleText)
      }
      fx.robot.press(KeyCode.ESCAPE)
      assertEquals(w.selected, Vector.empty)
      // The table: one stop, arrows move the row cursor, Enter selects.
      runOnFx(w.table.requestFocus())
      assert(runOnFx(w.table.getScene.getFocusOwner eq w.table), "the table is one focus stop")
      fx.robot.press(KeyCode.HOME)
      fx.robot.press(KeyCode.DOWN)
      fx.robot.press(KeyCode.DOWN)
      fx.robot.press(KeyCode.ENTER)
      assertEquals(w.selected, Vector(source.rows(2).ref))
      assertEquals(
        runOnFx(w.twin.input.state.selectionRings(t)).map(_.ref),
        Vector(source.rows(2).ref)
      )
      assertEquals(
        runOnFx(w.table.getAccessibleText),
        PlotText.selected(source.rowText(2), selected = true)
      )
      fx.robot.press(KeyCode.DOWN)
      fx.robot.press(KeyCode.ENTER, eyes4s.studio.desktop.harness.Modifiers(shift = true))
      assertEquals(w.selected, Vector(source.rows(2).ref, source.rows(3).ref))
      fx.robot.press(KeyCode.ESCAPE)
      assertEquals(w.selected, Vector.empty)
      assert(runOnFx(w.table.getScene.getFocusOwner eq w.table), "Escape keeps focus")
      dispose(w)
  }

  fxStage.test("with OS window focus, moving between plot and table carries the cursor") { fx =>
    val w = Wired(fx)
    val t = showAndDraw(w, 1.0)
    runOnFx {
      fx.stage.toFront()
      fx.stage.requestFocus()
      w.host.requestFocus()
    }
    fx.awaitLayout()
    assume(runOnFx(fx.stage.isFocused), "the test window did not receive OS focus")
    fx.robot.press(KeyCode.END)
    val last = t.targets.last.ref
    assertEquals(runOnFx(w.twin.input.state.focus), Some(last))
    runOnFx(w.table.requestFocus())
    fx.awaitLayout()
    assertEquals(runOnFx(w.table.state.cursor), Some(last))
    fx.robot.press(KeyCode.UP)
    val above = source.rows(rowOf(last) - 1).ref
    runOnFx(w.host.requestFocus())
    fx.awaitLayout()
    assertEquals(runOnFx(w.twin.input.state.focus), Some(above))
    assertEquals(w.selected, Vector.empty, "carrying the cursor selects nothing")
    dispose(w)
  }

  // --- Refusal and lifecycle ------------------------------------------------------------

  fxStage.test("a refused plot says why, and its table still lists every row") { fx =>
    val w     = Wired(fx)
    val label = right(ColumnId.of("participant"))
    runOnFx {
      val bad = right(
        PlotTwin.attach(
          DotPlot(label, d.id, "bad"),
          plotView,
          tableView,
          w.runtime.model.selection,
          _ => ()
        )
      )
      w.root.getChildren.setAll(bad.plotNode, bad.tableNode)
      bad.show(source, Theme.Light)
      bad.status.get match
        case PlotTwinStatus.Refused(s, e) =>
          assertEquals(s, source)
          assertEquals(e.message, s"plot dot-plot: column 'participant' is not numeric")
        case other => fail(s"unexpected $other")
      val refusal = bad.plotNode.getChildren.asScala.collectFirst { case l: Label => l }.get
      assert(refusal.isVisible)
      assertEquals(refusal.getText, "plot dot-plot: column 'participant' is not numeric")
      assertEquals(bad.table.vm.map(_.rows.size), Some(source.rows.size))
      assertEquals(bad.input.targets, None)
      bad.dispose()
    }
    dispose(w)
  }

  fxStage.test("attaching refuses a shared view id and an invalid tolerance") { _ =>
    runOnFx {
      assertEquals(
        PlotTwin
          .attach(dots, plotView, plotView, AppModel.newProject.toOption.get.selection, _ => ())
          .left
          .map(_.message),
        Left(s"the plot and its table are both view '${plotView.value}'")
      )
      assert(
        PlotTwin
          .attach(
            dots,
            plotView,
            tableView,
            AppModel.newProject.toOption.get.selection,
            _ => (),
            -1.0
          )
          .isLeft
      )
    }
  }

  fxStage.test("200 attach/show/dispose cycles leave nothing reachable") { fx =>
    val w = Wired(fx)
    showAndDraw(w, 1.0)
    dispose(w)
    val refs = ArrayBuffer.empty[WeakReference[PlotTwin]]
    (0 until 4).foreach { _ =>
      runOnFx {
        (0 until 50).foreach { _ =>
          val twin = right(
            PlotTwin.attach(dots, plotView, tableView, w.runtime.model.selection, _ => ())
          )
          w.root.getChildren.setAll(twin.plotNode, twin.tableNode)
          twin.show(source, Theme.Light)
          refs += WeakReference(twin)
          twin.dispose()
          twin.dispose() // idempotent
          w.root.getChildren.clear()
        }
      }
    }
    assertEquals(refs.size, 200)
    // Compiles still in flight hold a host until they deliver; wait for them.
    var live     = refs.count(_.get != null)
    val deadline = System.currentTimeMillis + 30000L
    while live > 0 && System.currentTimeMillis < deadline do
      System.gc()
      Thread.sleep(20)
      live = refs.count(_.get != null)
    assertEquals(live, 0, "disposed plot hosts are still reachable")
  }

  // --- Board snapshots ----------------------------------------------------------------

  fxStage.test("snapshot: a plot with a selected mark beside its table, in both themes") { fx =>
    Theme.values.foreach { theme =>
      val w      = Wired(fx, theme)
      val t      = showAndDraw(w, 2.0, theme)
      val target = t.targets.find(_.ref == StudioRef.Participant("P17")).get
      clickMark(w, t, target)
      runOnFx(w.table.requestFocus())
      fx.snapshot(if theme == Theme.Light then StudioTheme.Light else StudioTheme.Dark)
      dispose(w)
    }
  }
