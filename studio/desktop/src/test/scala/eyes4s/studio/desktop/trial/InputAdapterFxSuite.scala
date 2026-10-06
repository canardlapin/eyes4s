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

package eyes4s.studio.desktop.trial

import eyes4s.studio.app.text.TrialText
import eyes4s.plan.MapPlacement
import eyes4s.studio.app.tokens.{Colour, StageToken, StageVariant, Theme, ThemedToken, Tokens}
import eyes4s.studio.app.{AppEffect, AppModel, Intent}
import eyes4s.studio.core.assets.TrialDisplay
import eyes4s.studio.core.selection.{
  FixationIndex,
  InputCause,
  InputStamp,
  SelectionInput,
  SelectionMode,
  StudioRef,
  ViewId
}
import eyes4s.studio.desktop.harness.{FxStage, StageSize, StudioFxSuite}
import eyes4s.studio.desktop.plot.{CanvasPlotHost, PlotFrame, PlotHostProfile, PlotHostStatus}
import eyes4s.studio.desktop.runtime.{EffectPerformer, StudioRuntime}
import eyes4s.studio.desktop.typography.StudioFonts
import eyes4s.studio.app.plot.RovingMove
import eyes4s.studio.viz.plot.{OverlayRings, RingKind}
import eyes4s.studio.viz.trial.*
import intaglio.DevicePoint
import javafx.event.{Event, EventType}
import javafx.geometry.Point2D
import javafx.scene.SnapshotParameters
import javafx.scene.image.WritableImage
import javafx.scene.input.{KeyCode, MouseButton, MouseEvent, PickResult}
import javafx.scene.layout.StackPane
import javafx.scene.transform.Transform

import java.lang.ref.WeakReference
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration

/** The JavaFX input adapter (S4.2), on real JavaFX with synthetic events:
  * pointer picks at 1x and 2x resolve to the right fixation, keyboard alone
  * selects any mark, Escape clears, a selection projected from the bus draws
  * without emitting, the focus ring is accent cased by the halo, input never
  * recompiles or redraws the scene, 1,000 attach/dispose cycles leak nothing,
  * and selection feedback on 11,520 marks is measured and recorded.
  */
class InputAdapterFxSuite extends StudioFxSuite:

  override protected def stageSize: StageSize = StageSize(1100, 700)

  override val munitTimeout: Duration = Duration(600, "s")

  override def beforeAll(): Unit =
    super.beforeAll()
    val problems = runOnFx(StudioFonts.loadAll())
    assert(problems.isEmpty, problems.map(_.message).mkString("\n"))

  private val TimeoutMillis = 120000L

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val golden: StimulusSource = StimulusSource.directory(GoldenTrials.stimuli)
  private val viewId                 = right(ViewId.of("trial.query"))
  private val otherView              = right(ViewId.of("compare.ladder"))

  private lazy val enc03    = GoldenTrials.display("P17", "enc_03")
  private lazy val ret07    = GoldenTrials.display("P17", "ret_07")
  private lazy val enc03Fix = GoldenTrials.fixations("P17", "enc_03")
  private lazy val ret07Fix = GoldenTrials.fixations("P17", "ret_07")

  private def input(
      d: TrialDisplay,
      fixations: Vector[TrialFixation],
      marks: MarkStyle = MarkStyle.Neutral,
      theme: Theme = Theme.Light
  ): TrialSceneInput =
    TrialSceneInput(d, GoldenTrials.screen, fixations, marks, theme, StageVariant.Dark)

  // --- The view, the app loop and the adapter ------------------------------------

  /** A trial view wired to a real app loop: intents go through
    * `AppModel.update`, and every model's selection is projected back.
    */
  private final class Wired(val view: TrialView):
    val emitted      = ArrayBuffer.empty[Intent]
    private val none = new EffectPerformer:
      def perform(effect: AppEffect, dispatch: Intent => Unit): Unit = ()
    val runtime = StudioRuntime(right(AppModel.newProject), none)
    val adapter = runOnFx(
      right(
        TrialInputAdapter.attach(
          view,
          viewId,
          runtime.model.selection,
          i => { emitted += i; runtime.dispatch(i) }
        )
      )
    )
    runOnFx(runtime.listen(m => adapter.project(m.selection)))
    def host: CanvasPlotHost        = view.plotHost
    def selected: Vector[StudioRef] = runOnFx(runtime.model.selection.selected)

  private def viewIn(fx: FxStage): TrialView =
    val view = runOnFx(TrialView(golden))
    fx.show(runOnFx(StackPane(view)))
    isolateFromOsPointer(fx)
    view

  // --- Isolation from the OS pointer ------------------------------------------
  //
  // The test stage is a real window. Wherever the physical pointer is, the OS
  // delivers its own MOUSE_ENTERED/MOVED/EXITED events to it, for instance when
  // another suite's window closes and uncovers this one. Those events move the
  // adapter's hover between the test's synthetic moves (a real hover change,
  // not a defect), which made hover assertions intermittent under a loaded
  // full-suite run. Every mouse event not fired by this suite is consumed at
  // the scene and recorded, so the tests see only their own input.

  // True only while this suite fires a synthetic mouse event (FX thread).
  private var firing = false

  /** Mouse events the OS delivered to the test stage, consumed unseen. */
  private val foreign = ArrayBuffer.empty[String]

  private val IsolatedKey = "eyes4s.s42.os-pointer-isolated"

  private def isolateFromOsPointer(fx: FxStage): Unit =
    runOnFx {
      foreign.clear()
      if !fx.scene.getProperties.containsKey(IsolatedKey) then
        fx.scene.getProperties.put(IsolatedKey, true)
        fx.scene.addEventFilter(
          MouseEvent.ANY,
          (e: MouseEvent) =>
            if !firing then
              foreign += s"${e.getEventType} at (${e.getSceneX}, ${e.getSceneY})"
              e.consume()
        )
    }

  /** Shows `in` at output scale `k` and waits until it is drawn and targeted. */
  private def showAndDraw(w: Wired, in: TrialSceneInput, k: Double): (PlotFrame, TrialTargets) =
    runOnFx {
      w.host.setOutputScaleOverride(Some(k))
      w.view.show(in)
    }
    val deadline                                   = System.currentTimeMillis + TimeoutMillis
    def settled: Option[(PlotFrame, TrialTargets)] = runOnFx {
      w.view.getScene.getRoot.applyCss()
      w.view.getScene.getRoot.layout()
      (w.view.status.get, w.host.status.get) match
        case (TrialViewStatus.Shown(scene), PlotHostStatus.Drawn(frame))
            if !scene.frameArt.isInstanceOf[FrameArt.Loading] &&
              (frame.plan.scene eq scene.plot.scene) && frame.surface.deviceScale == k &&
              frame.surface.logicalWidth == w.host.getWidth &&
              frame.surface.logicalHeight == w.host.getHeight =>
          w.adapter.targets.map(frame -> _)
        case _ => None
    }
    var result = settled
    while result.isEmpty do
      if System.currentTimeMillis > deadline then
        fail(s"not drawn within $TimeoutMillis ms: ${runOnFx(w.view.status.get)}")
      Thread.sleep(5)
      result = settled
    result.get

  // Fires a synthetic mouse event at the host, at host-local logical (x, y).
  private def mouse(
      w: Wired,
      kind: EventType[MouseEvent],
      at: Point2D,
      toggle: Boolean = false,
      still: Boolean = true
  ): Unit =
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
          still,
          PickResult(host, inScene.getX, inScene.getY)
        )
      )
    finally firing = false

  private def click(w: Wired, at: Point2D, toggle: Boolean = false): Unit =
    mouse(w, MouseEvent.MOUSE_PRESSED, at, toggle)
    mouse(w, MouseEvent.MOUSE_RELEASED, at, toggle)
    mouse(w, MouseEvent.MOUSE_CLICKED, at, toggle)

  // Where a device point of the frame lies in the host's logical coordinates.
  private def local(frame: PlotFrame, p: DevicePoint): Point2D =
    val c = frame.transform.deviceToCanvas(p)
    Point2D(c.x, c.y)

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

  // --- Pixel probes -------------------------------------------------------------

  private def snapshotHost(w: Wired, k: Int): WritableImage =
    runOnFx {
      val params = SnapshotParameters()
      params.setTransform(Transform.scale(k.toDouble, k.toDouble))
      w.host.snapshot(params, null)
    }

  private def near(image: WritableImage, x: Double, y: Double, c: Colour, tol: Int): Boolean =
    val px = math.floor(x).toInt
    val py = math.floor(y).toInt
    px >= 0 && py >= 0 && px < image.getWidth.toInt && py < image.getHeight.toInt && {
      val argb = image.getPixelReader.getArgb(px, py)
      math.abs(((argb >> 16) & 0xff) - c.red) <= tol &&
      math.abs(((argb >> 8) & 0xff) - c.green) <= tol && math.abs((argb & 0xff) - c.blue) <= tol
    }

  // How many of 24 points around a circle (host-local device pixels of a
  // snapshot at the frame's own scale) are `c`.
  private def onCircle(image: WritableImage, centre: DevicePoint, r: Double, c: Colour): Int =
    (0 until 24).count { i =>
      val a = 2.0 * math.Pi * i / 24.0
      near(image, centre.x + r * math.cos(a), centre.y + r * math.sin(a), c, 48)
    }

  // --- Pointer --------------------------------------------------------------------

  fxStage.test("a synthetic click on every mark selects that fixation, at 1x and 2x") { fx =>
    val w = Wired(viewIn(fx))
    List(1.0, 2.0).foreach { k =>
      val (frame, targets) = showAndDraw(w, input(enc03, enc03Fix), k)
      assertEquals(targets.targets.size, 13)
      val before = runOnFx(w.host.profile)
      targets.targets.foreach { t =>
        runOnFx(click(w, local(frame, t.anchor)))
        assertEquals(w.selected, Vector(t.ref), s"at ${k}x")
        assertEquals(runOnFx(w.adapter.state.focus), Some(t.ref))
        assert(runOnFx(w.host.getScene.getFocusOwner eq w.host), "a click focuses the view")
      }
      // A modifier click toggles the last mark off.
      runOnFx(click(w, local(frame, targets.targets.last.anchor), toggle = true))
      assertEquals(w.selected, Vector.empty)
      // Input redrew the overlay only: no compile, no scene redraw.
      val after = runOnFx(w.host.profile)
      assertEquals((after.compiles, after.baseDraws), (before.compiles, before.baseDraws))
      assert(after.overlayDraws > before.overlayDraws)
    }
    assert(w.emitted.forall {
      case Intent.Select(i)       => i.stamp.origin == viewId
      case Intent.HoverOver(v, _) => v == viewId
      case _                      => false
    })
    runOnFx(w.adapter.dispose())
    runOnFx(w.view.dispose())
  }

  fxStage.test("a click whose press drifted still picks: a trial view has no brush") { fx =>
    val w                = Wired(viewIn(fx))
    val (frame, targets) = showAndDraw(w, input(enc03, enc03Fix), 1.0)
    val t                = targets.targets(5)
    val at               = local(frame, t.anchor)
    val moved            = Point2D(at.getX + 1.0, at.getY)
    runOnFx {
      mouse(w, MouseEvent.MOUSE_PRESSED, at)
      mouse(w, MouseEvent.MOUSE_RELEASED, moved, still = false)
      mouse(w, MouseEvent.MOUSE_CLICKED, moved, still = false)
    }
    assertEquals(w.selected, Vector(t.ref))
    runOnFx(w.adapter.dispose())
    runOnFx(w.view.dispose())
  }

  fxStage.test("pointer hover is local: it is drawn and reported, never selected") { fx =>
    val w                = Wired(viewIn(fx))
    val (frame, targets) = showAndDraw(w, input(enc03, enc03Fix), 2.0)
    val t                = targets.targets(5)
    runOnFx(mouse(w, MouseEvent.MOUSE_MOVED, local(frame, t.anchor)))
    assertEquals(runOnFx(w.adapter.state.hover), Some(t.ref))
    assertEquals(runOnFx(w.runtime.model.hover.map(_.target)), Some(t.ref))
    assertEquals(w.selected, Vector.empty)
    assert(w.emitted.forall(_.isInstanceOf[Intent.HoverOver]))
    runOnFx(mouse(w, MouseEvent.MOUSE_EXITED, local(frame, t.anchor)))
    assertEquals(runOnFx(w.adapter.state.hover), None)
    assertEquals(runOnFx(w.runtime.model.hover), None)
    runOnFx(w.adapter.dispose())
    runOnFx(w.view.dispose())
  }

  // --- Keyboard -------------------------------------------------------------------

  fxStage.test(
    "keyboard alone selects any mark; arrows go to the nearest mark; Escape clears"
  ) { fx =>
    val w      = Wired(viewIn(fx))
    val (_, t) = showAndDraw(w, input(enc03, enc03Fix), 2.0)
    runOnFx(w.host.requestFocus())
    assert(runOnFx(w.host.getScene.getFocusOwner eq w.host), "the view is one focus stop")
    t.targets.foreach { target =>
      fx.robot.press(KeyCode.HOME)
      (0 until target.mark.order).foreach(_ => fx.robot.press(KeyCode.PAGE_DOWN))
      fx.robot.press(KeyCode.ENTER)
      assertEquals(w.selected, Vector(target.ref))
      assertEquals(
        runOnFx(w.host.getAccessibleText),
        TrialText.mark(target.ref, target.mark.placement, selected = true)
      )
    }
    // Arrows: to the nearest mark strictly on that side.
    val from = t.targets.head
    fx.robot.press(KeyCode.HOME)
    List(KeyCode.RIGHT -> RovingMove.Right, KeyCode.DOWN -> RovingMove.Down).foreach {
      (code, move) =>
        fx.robot.press(KeyCode.HOME)
        fx.robot.press(code)
        assertEquals(
          runOnFx(w.adapter.state.focus),
          t.step(Some(from.ref), move).map(_.ref)
        )
    }
    fx.robot.press(KeyCode.ESCAPE)
    assertEquals(w.selected, Vector.empty)
    assert(runOnFx(w.host.getScene.getFocusOwner eq w.host), "Escape keeps focus")
    runOnFx(w.adapter.dispose())
    runOnFx(w.view.dispose())
  }

  fxStage.test("the focus ring is 2 px accent, cased by the halo, drawn over the stage") { fx =>
    val w      = Wired(viewIn(fx))
    val (f, t) = showAndDraw(w, input(ret07, ret07Fix), 2.0)
    runOnFx(w.host.requestFocus())
    fx.robot.press(KeyCode.HOME)
    // The host's focused property needs OS window focus, which a test run
    // cannot rely on: drive the adapter's focus input directly.
    runOnFx(w.adapter.focusChanged(true))
    val ring = runOnFx(w.adapter.state.overlay(t)) match
      case Vector(r) if r.kind == RingKind.Focus => r
      case other                                 => fail(s"unexpected overlay $other")
    assertEquals(ring.ref, t.targets.head.ref)
    val image  = snapshotHost(w, 2)
    val accent = Tokens.themed(Theme.Light, ThemedToken.Accent)
    val halo   = Tokens.staged(StageVariant.Dark, StageToken.Halo)
    // The accent band is centred 2 px outside the ring's start; the halo lies
    // in the outer casing band, 1 px beyond the accent (device px at 2x).
    assert(onCircle(image, ring.centre, ring.radius + 4.0, accent) >= 16, "no accent ring")
    assert(onCircle(image, ring.centre, ring.radius + 7.0, halo) >= 16, "no halo casing")
    assertEquals(
      runOnFx(w.host.getAccessibleText),
      TrialText.mark(t.targets.head.ref, t.targets.head.mark.placement, false)
    )
    assertEquals(f.surface.deviceScale, 2.0)
    assertEquals(runOnFx(w.adapter.lastOverlayError), None)
    runOnFx(w.adapter.dispose())
    runOnFx(w.view.dispose())
  }

  fxStage.test("with OS window focus, the host's focused property shows the focus ring") { fx =>
    val w      = Wired(viewIn(fx))
    val (_, t) = showAndDraw(w, input(ret07, ret07Fix), 1.0)
    runOnFx {
      fx.stage.toFront()
      fx.stage.requestFocus()
      w.host.requestFocus()
    }
    fx.awaitLayout()
    assume(runOnFx(fx.stage.isFocused), "the test window did not receive OS focus")
    fx.robot.press(KeyCode.HOME)
    assert(runOnFx(w.host.isFocused))
    assertEquals(
      runOnFx(w.adapter.state.overlay(t)).map(r => (r.kind, r.ref)),
      Vector((RingKind.Focus, t.targets.head.ref))
    )
    runOnFx(w.adapter.dispose())
    runOnFx(w.view.dispose())
  }

  // --- Projection -----------------------------------------------------------------

  fxStage.test("hover on a selected mark rings outside the selection, in both themes") { fx =>
    Theme.values.foreach { theme =>
      val w      = Wired(viewIn(fx))
      val (f, t) = showAndDraw(w, input(ret07, ret07Fix, theme = theme), 2.0)
      val target = t.targets(4)
      selectByOther(w, target.ref, 0L)
      runOnFx(mouse(w, MouseEvent.MOUSE_MOVED, local(f, target.anchor)))
      val rings = runOnFx(w.adapter.state.overlay(t))
      assertEquals(rings.map(_.kind), Vector(RingKind.Selected, RingKind.Hover))
      val Vector(selected, hover) = rings: @unchecked
      val k                       = 2.0
      // The hover ring starts outside both selection bands.
      assertEqualsDouble(
        hover.radius - selected.radius,
        2.0 * OverlayRings.SelectedBandPx * k,
        1e-9
      )
      val image = snapshotHost(w, 2)
      val inner = Tokens.themed(theme, ThemedToken.SelRingInner)
      val outer = Tokens.themed(theme, ThemedToken.Ink)
      val onHov = Tokens.staged(StageVariant.Dark, StageToken.OnStage)
      val band  = OverlayRings.SelectedBandPx * k
      // Both selection bands keep their colours under the hover ring ...
      assert(
        onCircle(image, selected.centre, selected.radius + band / 2.0, inner) >= 16,
        s"$theme: the selection's inner band is painted over"
      )
      assert(
        onCircle(image, selected.centre, selected.radius + 1.5 * band, outer) >= 16,
        s"$theme: the selection's outer band is painted over"
      )
      // ... and the hover ring shows outside them.
      assert(
        onCircle(image, hover.centre, hover.radius + OverlayRings.HoverPx / 2.0 * k, onHov) >=
          16,
        s"$theme: no hover ring"
      )
      runOnFx(w.adapter.dispose())
      runOnFx(w.view.dispose())
    }
  }

  fxStage.test("a selection projected from the bus draws the ring without emitting") { fx =>
    val w      = Wired(viewIn(fx))
    val (_, t) = showAndDraw(w, input(ret07, ret07Fix), 2.0)
    val target = t.targets(4)
    // Measure one projection in one FX turn: an OS focus event may redraw
    // the overlay between separate runOnFx calls, independently of selection.
    val (before, after) = runOnFx {
      val before = w.host.profile
      selectByOther(w, target.ref, 0L)
      (before, w.host.profile)
    }
    assertEquals(w.selected, Vector(target.ref))
    assertEquals(w.emitted.toVector, Vector.empty, "the view echoed a projected selection")
    assertEquals(
      after,
      before.copy(overlayDraws = before.overlayDraws + 1, underDraws = before.underDraws + 1)
    )
    val ring = runOnFx(w.adapter.state.overlay(t)) match
      case Vector(r) if r.kind == RingKind.Selected => r
      case other                                    => fail(s"unexpected overlay $other")
    val image = snapshotHost(w, 2)
    val inner = Tokens.themed(Theme.Light, ThemedToken.SelRingInner)
    assert(onCircle(image, ring.centre, ring.radius + 2.0, inner) >= 16, "no selection ring")
    // Another view's selection of something this trial does not draw: no ring.
    selectByOther(w, StudioRef.Participant("P17"), 1L)
    assertEquals(runOnFx(w.adapter.state.overlay(t)), Vector.empty)
    assertEquals(w.emitted.toVector, Vector.empty)
    runOnFx(w.adapter.dispose())
    runOnFx(w.view.dispose())
  }

  // --- Lifecycle ------------------------------------------------------------------

  fxStage.test("1,000 attach/dispose cycles leave no handler, listener or overlay behind") {
    fx =>
      val view = viewIn(fx)
      // What the host was before any adapter: dispose must restore it.
      val prior = runOnFx {
        val h = view.plotHost
        (
          h.getAccessibleRole,
          h.getAccessibleRoleDescription,
          h.getAccessibleText,
          h.isFocusTraversable
        )
      }
      val w = Wired(view)
      showAndDraw(w, input(enc03, enc03Fix), 1.0)
      runOnFx(w.adapter.dispose())
      val profile = runOnFx(w.host.profile)
      val refs    = ArrayBuffer.empty[WeakReference[TrialInputAdapter]]
      (0 until 10).foreach { _ =>
        runOnFx {
          (0 until 100).foreach { _ =>
            val a =
              right(TrialInputAdapter.attach(view, viewId, w.runtime.model.selection, _ => ()))
            refs += WeakReference(a)
            a.dispose()
            a.dispose() // idempotent
          }
        }
      }
      assertEquals(refs.size, 1000)
      var live     = refs.count(_.get != null)
      val deadline = System.currentTimeMillis + 30000L
      while live > 0 && System.currentTimeMillis < deadline do
        System.gc()
        Thread.sleep(20)
        live = refs.count(_.get != null)
      assertEquals(live, 0, "disposed adapters are still reachable from the view")
      runOnFx {
        val h = w.host
        assertEquals(
          (
            h.getAccessibleRole,
            h.getAccessibleRoleDescription,
            h.getAccessibleText,
            h.isFocusTraversable
          ),
          prior
        )
      }
      // Input never touched the scene, and a fresh adapter still works.
      assertEquals(runOnFx(w.host.profile).compiles, profile.compiles)
      assertEquals(runOnFx(w.host.profile).baseDraws, profile.baseDraws)
      val fresh  = Wired(view)
      val (f, t) = showAndDraw(fresh, input(enc03, enc03Fix), 1.0)
      runOnFx(click(fresh, local(f, t.targets(2).anchor)))
      assertEquals(fresh.selected, Vector(t.targets(2).ref))
      runOnFx(fresh.adapter.dispose())
      runOnFx(view.dispose())
  }

  // --- 11,520 marks ---------------------------------------------------------------

  private val Columns = 120
  private val Rows    = 96

  /** 11,520 fixations on a grid over the image frame, in record order. */
  private lazy val dense: Vector[TrialFixation] =
    val k = GoldenTrials.key("P17", "ret_07")
    (for
      row <- 0 until Rows
      col <- 0 until Columns
    yield (row, col)).zipWithIndex.map { case ((row, col), i) =>
      right(
        TrialFixation.of(
          k,
          right(FixationIndex.of(i + 1)),
          452.0 + col * (1016.0 / (Columns - 1)),
          160.0 + row * (760.0 / (Rows - 1)),
          40,
          MapPlacement.InWindow
        )
      )
    }.toVector

  private def percentile(sorted: Vector[Double], q: Double): Double =
    sorted(math.floor(q * (sorted.size - 1)).toInt)

  private def evidence(name: String, lines: Seq[String]): Unit =
    val root = Paths.get(sys.props.getOrElse("eyes4s.studio.snapshots", "target/x")).getParent
    val file = root.resolve("studio-evidence").resolve(name)
    Files.createDirectories(file.getParent)
    Files.write(file, lines.mkString("", "\n", "\n").getBytes(UTF_8))
    println(s"--- $file\n${lines.mkString("\n")}")

  private def machine: Seq[String] = Seq(
    s"os=${sys.props("os.name")} ${sys.props("os.arch")} ${sys.props("os.version")}",
    s"java=${sys.props("java.version")}",
    s"processors=${Runtime.getRuntime.availableProcessors}",
    s"prism=${sys.props.getOrElse("prism.order", "default")}"
  )

  fxStage.test("selection feedback on 11,520 marks: measured and recorded") { fx =>
    assertEquals(dense.size, 11520)
    val w       = Wired(viewIn(fx))
    val shownAt = System.nanoTime
    val (f, t)  = showAndDraw(w, input(ret07, dense), 2.0)
    val drawnMs = (System.nanoTime - shownAt) / 1e6
    assertEquals(t.targets.size, 11520)
    val random   = scala.util.Random(42L)
    val handler  = ArrayBuffer.empty[Double]
    val snapshot = ArrayBuffer.empty[Double]
    val before   = runOnFx(w.host.profile)
    (0 until 120).foreach { i =>
      val target = t.targets(random.nextInt(t.targets.size))
      val at     = local(f, target.anchor)
      val (h, s) = runOnFx {
        val t0 = System.nanoTime
        mouse(w, MouseEvent.MOUSE_CLICKED, at)
        val t1 = System.nanoTime
        snapshotHost(w, 1)
        val t2 = System.nanoTime
        ((t1 - t0) / 1e6, (t2 - t0) / 1e6)
      }
      assertEquals(w.selected, Vector(target.ref))
      if i >= 20 then
        handler += h
        snapshot += s
    }
    val after = runOnFx(w.host.profile)
    assertEquals((after.compiles, after.baseDraws), (before.compiles, before.baseDraws))
    val hs = handler.toVector.sorted
    val ss = snapshot.toVector.sorted
    evidence(
      "S4.2-selection-latency.txt",
      machine ++ Seq(
        "measurement=synthetic MOUSE_CLICKED on the host to overlay redrawn (handler), " +
          "and to a synchronous host snapshot (snapshot); FX thread; not OS-pointer-to-display",
        "loop=adapter -> Intent.Select -> StudioRuntime/AppModel.update -> project -> overlay",
        s"marks=${t.targets.size}",
        s"device_scale=${f.surface.deviceScale}",
        s"surface_logical=${f.surface.logicalWidth}x${f.surface.logicalHeight}",
        s"show_to_drawn_ms=${"%.1f".format(drawnMs)}",
        "warmup_events=20",
        s"samples=${hs.size}",
        s"handler_median_ms=${"%.3f".format(percentile(hs, 0.5))}",
        s"handler_p95_ms=${"%.3f".format(percentile(hs, 0.95))}",
        s"snapshot_median_ms=${"%.3f".format(percentile(ss, 0.5))}",
        s"snapshot_p95_ms=${"%.3f".format(percentile(ss, 0.95))}",
        s"scene_compiles_during_input=${after.compiles - before.compiles}",
        s"scene_draws_during_input=${after.baseDraws - before.baseDraws}"
      )
    )
    // The acceptance bound (< 100 ms) on the named machine's pipeline: the
    // feedback itself (overlay redrawn) always, and the full loop through a
    // synchronous snapshot too, except under the software pipeline (CI,
    // prism.order=sw), where the snapshot re-rasterises all 11,520 marks.
    assert(percentile(hs, 0.5) < 100.0, s"selection feedback median ${percentile(hs, 0.5)} ms")
    if !sys.props.get("prism.order").contains("sw") then
      assert(
        percentile(ss, 0.5) < 100.0,
        s"feedback to snapshot median ${percentile(ss, 0.5)} ms"
      )
    runOnFx(w.adapter.dispose())
    runOnFx(w.view.dispose())
  }

  fxStage.test("hover over 11,520 selected marks redraws only the pointer layer (recorded)") {
    fx =>
      val w      = Wired(viewIn(fx))
      val (f, t) = showAndDraw(w, input(ret07, dense), 2.0)
      // Another view selects every mark: one projection draws the selection layer.
      val all                    = t.targets.map(_.ref)
      val (projectMs, projected) = runOnFx {
        val before = w.host.profile
        val t0     = System.nanoTime
        w.runtime.dispatch(
          Intent.Select(
            SelectionInput(
              InputStamp(w.runtime.model.selection.context, otherView, 0L, InputCause.Pointer),
              SelectionMode.Replace,
              all
            )
          )
        )
        ((System.nanoTime - t0) / 1e6, w.host.profile.underDraws - before.underDraws)
      }
      assertEquals(projected, 1L)
      assertEquals(w.selected.size, 11520)
      assertEquals(runOnFx(w.adapter.lastOverlayError), None)
      val random = scala.util.Random(11L)
      val moves  = ArrayBuffer.empty[Double]
      val before = runOnFx(w.host.profile)
      (0 until 120).foreach { i =>
        val target = t.targets(random.nextInt(t.targets.size))
        val ms     = runOnFx {
          val t0 = System.nanoTime
          mouse(w, MouseEvent.MOUSE_MOVED, local(f, target.anchor))
          (System.nanoTime - t0) / 1e6
        }
        assertEquals(
          runOnFx(w.adapter.state.hover),
          Some(target.ref),
          runOnFx(s"move $i; OS pointer events consumed: ${foreign.mkString(", ")}")
        )
        if i >= 20 then moves += ms
      }
      val after = runOnFx(w.host.profile)
      // Hover never redraws the selection layer or the scene.
      assertEquals(
        (after.underDraws, after.compiles, after.baseDraws),
        (before.underDraws, before.compiles, before.baseDraws)
      )
      assert(after.overlayDraws > before.overlayDraws)
      assertEquals(w.emitted.collect { case i: Intent.Select => i }.toVector, Vector.empty)
      val ms = moves.toVector.sorted
      evidence(
        "S4.2-large-selection.txt",
        machine ++ Seq(
          "measurement=11,520 marks all selected from another view; then synthetic " +
            "MOUSE_MOVED on the host, each onto a new mark, to the pointer layer redrawn; " +
            "FX thread; not OS-pointer-to-display",
          s"marks=${t.targets.size} selected=${all.size}",
          s"device_scale=${f.surface.deviceScale}",
          s"surface_logical=${f.surface.logicalWidth}x${f.surface.logicalHeight}",
          s"project_all_ms=${"%.1f".format(projectMs)} (bus update + selection layer drawn once)",
          "warmup_events=20",
          s"samples=${ms.size}",
          s"hover_move_median_ms=${"%.3f".format(percentile(ms, 0.5))}",
          s"hover_move_p95_ms=${"%.3f".format(percentile(ms, 0.95))}",
          s"selection_layer_draws_during_hover=${after.underDraws - before.underDraws}",
          s"os_pointer_events_consumed=${runOnFx(foreign.size)} " +
            s"${runOnFx(foreign.take(3).mkString("[", "; ", "]"))}",
          s"scene_draws_during_hover=${after.baseDraws - before.baseDraws}"
        )
      )
      runOnFx(w.adapter.dispose())
      runOnFx(w.view.dispose())
  }

  // --- The cost of one name per mark -------------------------------------------------

  /** The same 11,520 neutral marks as one named point batch, or as one named
    * single-point batch per mark, in one panel.
    */
  private def denseScene(perMark: Boolean): intaglio.Scene =
    import eyes4s.studio.viz.plot.IntaglioColours
    import intaglio.{
      BatchColumn,
      Clip,
      ExtentExpr,
      GraphicParams,
      GraphicsName,
      Grob,
      Interval,
      Point,
      PointShape,
      Scene,
      Size,
      StrokeUnit,
      Viewport,
      YDirection
    }
    val pt = 72.0 / 96.0
    val gp = right(
      GraphicParams.checked(
        stroke = Some(IntaglioColours.staged(StageVariant.Dark, StageToken.Halo)),
        fill = Some(IntaglioColours.themed(Theme.Light, ThemedToken.NeutralMark)),
        lineWidth = 2.0 * pt,
        lineWidthUnit = StrokeUnit.Point
      )
    )
    val size   = right(ExtentExpr.points(TrialScene.radiusPx(40) * pt))
    val points = dense.map(f => right(Point.native(f.screenX, f.screenY)))
    def batch(ps: Vector[Point], name: String) = right(
      Grob.pointBatch(
        ps,
        sizes = BatchColumn.Constant(size),
        shapes = BatchColumn.Constant(PointShape.Circle),
        graphicParams = BatchColumn.Constant(gp),
        name = Some(GraphicsName.unsafe(name))
      )
    )
    val marks =
      if perMark then points.zipWithIndex.map((p, i) => batch(Vector(p), s"mark-$i"))
      else Vector(batch(points, "marks"))
    val vp = right(
      Viewport.checked(
        right(Point.npc(0.0, 0.0)),
        right(Size.npc(1.0, 1.0)),
        right(Interval(440.0, 1480.0)),
        right(Interval(150.0, 930.0)),
        Clip.Off,
        yDirection = YDirection.Down
      )
    )
    Scene(
      Vector(Grob.group(marks, viewport = Some(vp), name = Some(GraphicsName.unsafe("panel"))))
    )

  private def medianMs(runs: Int)(body: => Any): Double =
    (0 until 2).foreach(_ => body)
    val times = (0 until runs)
      .map { _ =>
        val t0 = System.nanoTime
        body
        (System.nanoTime - t0) / 1e6
      }
      .toVector
      .sorted
    percentile(times, 0.5)

  fxStage.test("one name per mark: cost at 11,520 marks against one batch (recorded)") { _ =>
    import eyes4s.studio.viz.plot.{PlotSurface, SceneId}
    import intaglio.interaction.NamedPicking
    import intaglio.javafx.{JavaFxCanvasContext, JavaFxRenderer}
    import intaglio.{RenderPlan, value}
    val surface = right(PlotSurface(1100.0, 700.0, 2.0))
    val context = right(surface.renderContext(right(SceneId("s4.2.cost"))))
    val random  = scala.util.Random(7L)
    val queries = Vector.fill(200)(
      DevicePoint(
        random.nextDouble * surface.deviceWidth,
        random.nextDouble * surface.deviceHeight
      )
    )
    def measure(perMark: Boolean): Seq[(String, Double)] =
      val scene   = denseScene(perMark)
      val plan    = RenderPlan(scene, context)
      val program = right(JavaFxRenderer.compile(plan))
      val picking = right(NamedPicking.compile(scene, context))
      val canvas  = runOnFx(
        javafx.scene.canvas.Canvas(surface.deviceWidth.toDouble, surface.deviceHeight.toDouble)
      )
      Seq(
        "lower_ms"          -> medianMs(5)(right(plan.deviceScene)),
        "javafx_compile_ms" -> medianMs(5)(right(JavaFxRenderer.compile(plan))),
        "named_picking_ms"  -> medianMs(5)(right(NamedPicking.compile(scene, context))),
        "draw_snapshot_ms"  -> medianMs(5)(runOnFx {
          val gc = canvas.getGraphicsContext2D
          gc.clearRect(0, 0, canvas.getWidth, canvas.getHeight)
          JavaFxRenderer.draw(program, JavaFxCanvasContext(gc))
          canvas.snapshot(null, null): Unit
        }),
        "200_hits_ms" -> medianMs(5)(queries.foreach(q => right(picking.hits(q, 8.0))))
      )
    val batch   = measure(perMark = false)
    val perMark = measure(perMark = true)
    // What each plan resolves: one batch is one target; per-mark names, one each.
    val one  = right(NamedPicking.compile(denseScene(perMark = false), context))
    val each = right(NamedPicking.compile(denseScene(perMark = true), context))
    assertEquals((one.targetCount, each.targetCount), (1, 11520))
    val trial   = right(TrialScene(input(ret07, dense))).plot
    val frameMs = medianMs(3)(right(PlotFrame.compile(trial, surface)))
    val frame   = right(PlotFrame.compile(trial, surface))
    assertEquals(frame.picking.names.count(_.value.startsWith(TrialScene.MarkPrefix)), 11520)
    evidence(
      "S4.2-per-mark-cost.txt",
      machine ++ Seq(
        "workaround=one GraphicsName per fixation mark (Intaglio NamedPicking treats a " +
          "named point batch as one target; upstream bd-01M3FJKMB1CKWYCY58AS6A2Y16)",
        s"marks=11520 surface=${surface.deviceWidth}x${surface.deviceHeight} device px (2x)",
        "statistic=median of 5 after 2 warm-ups",
        "metric batch_ms per_mark_ms ratio"
      ) ++ batch.zip(perMark).map { case ((k, b), (_, m)) =>
        f"$k%s $b%.2f $m%.2f ${m / b}%.1fx"
      } :+ f"trial_frame_compile_ms (PlotFrame: program, device scene, picking) $frameMs%.1f"
    )
  }
