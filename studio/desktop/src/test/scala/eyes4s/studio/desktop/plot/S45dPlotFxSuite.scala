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
import eyes4s.studio.app.tokens.{Colour, Theme, ThemedToken, Tokens}
import eyes4s.studio.app.{AppEffect, AppModel, HoverAt, Intent}
import eyes4s.studio.core.backend.{ReportRole, RunId}
import eyes4s.studio.core.document.ReportingId
import eyes4s.studio.core.fixture.{FakeStudyBackend, MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.selection.{StudioRef, ViewId}
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.journey.StoredQueryExpectations
import eyes4s.studio.desktop.harness.{FxStage, StageSize, StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.runtime.{EffectPerformer, StudioRuntime}
import eyes4s.studio.desktop.typography.StudioFonts
import eyes4s.studio.viz.plot.{PlotTargets, RowMarking, ScaleProfilePlot}
import intaglio.DevicePoint
import io.circe.Json
import javafx.event.Event
import javafx.event.EventType
import javafx.geometry.Point2D
import javafx.scene.SnapshotParameters
import javafx.scene.image.WritableImage
import javafx.scene.input.{MouseButton, MouseEvent, PickResult}
import javafx.scene.layout.{HBox, Priority}
import javafx.scene.transform.Transform

import scala.concurrent.duration.Duration

/** The scale profile in the plot host (ticket S4.5d), on real JavaFX: the
  * fake backend's run 7 at its declared scales shows, in the plot and its
  * table, fixture.json's group and participant D by scale, on a σ axis
  * whose doublings are evenly spaced; hovering a group mean shows its n,
  * and clicking a participant's line selects its rows.
  */
class S45dPlotFxSuite extends StudioFxSuite:

  override protected def stageSize: StageSize = StageSize(1200, 560)

  override val munitTimeout: Duration = Duration(300, "s")

  override def beforeAll(): Unit =
    super.beforeAll()
    val problems = runOnFx(StudioFonts.loadAll())
    assert(problems.isEmpty, problems.map(_.message).mkString("\n"))

  private val TimeoutMillis = 120000L

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val plotView  = right(ViewId.of("results.scale-profile"))
  private val tableView = right(ViewId.of("results.scale-profile.table"))
  private val columns   = right(ProfileColumns.standard)
  private val reporting = right(ReportingId.of("by-retrieval-response"))
  private val run       = RunId(7)

  /** Run 7's profile at its analysis revision's declared scales, as the fake
    * backend serves it at story moment t2.
    */
  private lazy val fixtureProfile: ScaleProfile =
    val doc    = right(StoryMoments.t2)
    val scales = right(
      doc.run(run).flatMap(r => doc.analysis(r.analysis)).map(_.recipe.scales).toRight("no run")
    )
    val groupedSpec = right(StoryMoments.byResponse)
    val overallSpec = right(
      eyes4s.studio.core.document.ReportingSpec.of(
        right(ReportingId.of(reporting.value + "-overall")),
        "Overall",
        None,
        Vector.empty,
        None,
        eyes4s.studio.core.document.ReportingWeight.ParticipantMeans
      )
    )
    import cats.syntax.all.*
    (for
      backend <- FakeStudyBackend.create[IO](StoryMoment.T2)
      summary <- backend.result(run).map(right)
      grouped <- summary.scales.indices.toVector.traverse(i =>
        backend.report(run, groupedSpec, i).map(right)
      )
      overall <- summary.scales.indices.toVector.traverse(i =>
        backend.report(run, overallSpec, i).map(right)
      )
    yield ScaleProfile.of(grouped, overall, scales, summary.scales).left.map(_.message))
      .unsafeRunSync()
      .fold(e => fail(e), identity)

  // fixture.json's summary, read without the backend.
  private lazy val fixtureSummary: Json =
    val json = right(io.circe.parser.parse(MockStudy.fixtureText))
    json.hcursor.downField("summary").focus.getOrElse(json)

  // --- The host and the app loop ------------------------------------------------------

  private final class Wired(fx: FxStage):
    private val none = new EffectPerformer:
      def perform(effect: AppEffect, dispatch: Intent => Unit): Unit = ()
    val runtime = StudioRuntime(right(AppModel.newProject), none)
    val twin    = runOnFx(
      right(
        PlotTwin.attach(
          ScaleProfilePlot(columns),
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
              true,
              PickResult(w.host, inScene.getX, inScene.getY)
            )
          )
        finally firing = false
      }
    }

  private def hoverAt(w: Wired, t: PlotTargets, at: DevicePoint): Unit =
    fire(w, t, at, MouseEvent.MOUSE_MOVED)

  private def click(w: Wired, t: PlotTargets, at: DevicePoint): Unit =
    fire(
      w,
      t,
      at,
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
    assert(runOnFx(w.twin.table.onScreen), "the table draws no rows")
    runOnFx(w.twin.table.rowTexts)

  // Where a mark's row is placed, in data coordinates: its anchor without its nudge.
  private def placed(t: PlotTargets, ref: StudioRef) =
    val target = right(t.target(ref).toRight(s"no mark for $ref"))
    val nudge  = target.mark.nudgePx
    t.transform.deviceToData(
      DevicePoint(target.anchor.x - nudge.dxPx, target.anchor.y - nudge.dyPx)
    )

  private def anchor(t: PlotTargets, ref: StudioRef): DevicePoint =
    right(t.target(ref).toRight(s"no mark for $ref")).anchor

  // --- The fixture's profile -----------------------------------------------------------

  fxStage.test("σ is log-spaced and every point is fixture.json's; hover shows a group's n") {
    fx =>
      val profile = fixtureProfile
      val source  = right(ScaleProfile.source(profile, columns))
      val w       = Wired(fx)
      val t       = showAndDraw(w, source)
      assertEquals(rowTexts(w), source.rows.map(source.cellsOf))
      // 24 participant lines and 2 × 4 group means.
      assertEquals(t.targets.size, 24 + 8)

      // Each group mean sits at log10 σ and fixture.json's D.
      profile.groups.foreach { g =>
        val ds = (0 until 4).toVector.map(scale =>
          StoredQueryExpectations.grand(
            ReportRole.Difference,
            scale,
            Some(eyes4s.studio.core.backend.Response(g.name))
          )
        )
        g.points.zip(ds).foreach { (p, d) =>
          val at = placed(t, p.ref)
          assertEqualsDouble(at.x, math.log10(p.sigma.degrees), 1e-9)
          assertEqualsDouble(at.y, d, StoredQueryExpectations.Precision)
        }
      }
      // On the canvas, the protocol's doublings (0.5, 1, 2, 4°) are evenly spaced.
      val xs    = profile.groups.head.points.map(p => anchor(t, p.ref).x)
      val steps = xs.zip(xs.tail).map((a, b) => b - a)
      assert(steps.head > 40.0, steps)
      steps.foreach(s => assertEqualsDouble(s, steps.head, 1e-6))

      // Each participant's line holds its four points, at fixture.json's D.
      val participants =
        right(fixtureSummary.hcursor.downField("participants").as[Vector[Json]])
      participants.foreach { p =>
        val id     = right(p.hcursor.get[String]("id"))
        val series = profile.participants.find(_.name == id).getOrElse(fail(id))
        val ds     = (0 until 4).toVector.map(scale =>
          StoredQueryExpectations.mean(id, ReportRole.Difference, scale)
        )
        val plot = runOnFx(w.twin.plot).getOrElse(fail("no plot"))
        val mark = plot.markOf(series.points.head.ref).getOrElse(fail(id))
        assertEquals(mark.refs, series.points.map(_.ref))
        assertEquals(mark.rows.size, ds.size)
        mark.rows.zip(series.points).zip(ds).foreach { case ((row, pt), d) =>
          row.marking match
            case RowMarking.Placed(at) =>
              assertEqualsDouble(at.x, math.log10(pt.sigma.degrees), 1e-9)
              assertEqualsDouble(at.y, d, StoredQueryExpectations.Precision)
            case other => fail(s"$id should be placed: $other")
        }
      }

      // The solid group's mean is an ink dot.
      val rem   = profile.groups.head.points(2)
      val image = snapshot(w)
      assert(near(image, anchor(t, rem.ref), Tokens.themed(Theme.Light, ThemedToken.Ink), 40))

      // Hovering it shows its group's n.
      hoverAt(w, t, anchor(t, rem.ref))
      assertEquals(w.hover, Some(HoverAt(plotView, rem.ref)))
      assertEquals(
        w.readout,
        Some("Mean of Remembered, Scale 2°, σ (°) 2.00, D +0.30, n 24 participants")
      )

      // Clicking a participant's line selects all its rows.
      val p17 = profile.participants.find(_.name == "P17").getOrElse(fail("P17"))
      click(w, t, anchor(t, p17.points.head.ref))
      assertEquals(w.selected, p17.points.map(_.ref))
      fx.snapshot(StudioTheme.Light)
      runOnFx(w.twin.dispose())
  }
