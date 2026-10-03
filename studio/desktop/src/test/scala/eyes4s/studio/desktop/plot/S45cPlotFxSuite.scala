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
import eyes4s.studio.core.backend.{Response, RunId}
import eyes4s.studio.core.document.ReportingId
import eyes4s.studio.core.fixture.{FakeStudyBackend, MockStudy, StoryMoment}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef, ViewId}
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.harness.{FxStage, StageSize, StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.runtime.{EffectPerformer, StudioRuntime}
import eyes4s.studio.desktop.typography.StudioFonts
import eyes4s.studio.viz.plot.{ParticipantPlot, PlotTargets, RowMarking}
import intaglio.DevicePoint
import io.circe.Json
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

/** The participant plot in the plot host (ticket S4.5c), on real JavaFX: the
  * fake backend's participant means at 2° show, in the plot and its table,
  * exactly fixture.json's means and grand means; hovering a mark shows its
  * group's n (a grand mean's participants, a participant's queries); and a
  * missing mean is drawn as a dashed empty ring apart from a zero mean.
  */
class S45cPlotFxSuite extends StudioFxSuite:

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
  private val columns   = right(ParticipantColumns.standard)
  private val reporting = right(ReportingId.of("by-retrieval-response"))
  private val run       = RunId(7)

  /** The run's participant means at the fixture's focus scale, as the fake
    * backend serves them at story moment t2.
    */
  private lazy val fixtureMeans: ParticipantMeans =
    FakeStudyBackend
      .create[IO](StoryMoment.T2)
      .flatMap(_.result(run))
      .unsafeRunSync()
      .flatMap { summary =>
        val index = summary.scales.indexOf(FakeStudyBackend.FocusScale)
        ScaleIndex
          .of(index)
          .left
          .map(_.message)
          .flatMap(ParticipantMeans.of(summary, reporting, _).left.map(_.message))
      }
      .fold(e => fail(e.toString), identity)

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
          ParticipantPlot(columns),
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

  // --- The fixture's participants -----------------------------------------------------

  fxStage.test("fixture.json's 24 participants and grand means; hover shows each group's n") {
    fx =>
      val means  = fixtureMeans
      val source = right(ParticipantMeans.source(means, columns))
      val w      = Wired(fx)
      val t      = showAndDraw(w, source)
      assertEquals(t.targets.size, 50)
      assertEquals(rowTexts(w), source.rows.map(source.cellsOf))

      // Every participant sits at fixture.json's mean in its group's column.
      val participants =
        right(fixtureSummary.hcursor.downField("participants").as[Vector[Json]])
      assertEquals(participants.size, 24)
      for
        (label, level) <- Vector("Remembered", "Forgotten").zipWithIndex
        p              <- participants
      do
        val id  = right(p.hcursor.get[String]("id"))
        val d   = right(p.hcursor.downField(label).get[Double]("D"))
        val ref = StudioRef.ParticipantSummary(
          run,
          reporting,
          means.scale,
          Some(Response(label)),
          id
        )
        val at = placed(t, ref)
        assertEqualsDouble(at.x, level.toDouble, 1e-6)
        assertEqualsDouble(at.y, d, 1e-6)
      val grands = means.groups.map(g => g.group.label -> g).toMap
      Vector("Remembered", "Forgotten").zipWithIndex.foreach { (label, level) =>
        val at = placed(t, grands(label).ref)
        assertEqualsDouble(at.x, level.toDouble, 1e-6)
        assertEqualsDouble(
          at.y,
          right(fixtureSummary.hcursor.get[Double](s"grand_D_$label")),
          1e-6
        )
      }

      // The solid group's grand mean is an ink tick.
      val image = snapshot(w)
      assert(
        near(
          image,
          anchor(t, grands("Remembered").ref),
          Tokens.themed(Theme.Light, ThemedToken.Ink),
          40
        ),
        "no Remembered tick"
      )

      // Hovering a grand mean shows its group's n of participants.
      assertEquals(w.readout, None)
      val forgotten = grands("Forgotten")
      hoverAt(w, t, anchor(t, forgotten.ref))
      assertEquals(w.hover, Some(HoverAt(plotView, forgotten.ref)))
      val n = right(fixtureSummary.hcursor.get[Int]("n_Forgotten"))
      assertEquals(
        w.readout,
        Some(s"Group Forgotten, Mean of all participants, D +0.15, n $n")
      )
      assert(runOnFx(w.twin.plotNode.lookup(".plot-readout").isVisible))

      // Hovering a participant's dot shows its n of queries in that group.
      val p17 = means.cells
        .find(c => c.participant == "P17" && c.group == Response.Forgotten)
        .getOrElse(fail("no P17"))
      hoverAt(w, t, anchor(t, p17.ref))
      assertEquals(w.hover, Some(HoverAt(plotView, p17.ref)))
      assertEquals(w.readout, Some("Group Forgotten, Mean of P17, D +0.32, n 2"))

      // Selected and no longer hovered, the mark still says its n.
      click(w, t, anchor(t, p17.ref))
      assertEquals(w.selected, Vector(p17.ref))
      fire(w, t, DevicePoint(1.0, 1.0), MouseEvent.MOUSE_EXITED)
      assertEquals(w.hover, None)
      assertEquals(w.readout, Some("Group Forgotten, Mean of P17, D +0.32, n 2"))
      fx.snapshot(StudioTheme.Light)
      runOnFx(w.twin.dispose())
  }

  // --- Missing is not zero ------------------------------------------------------------

  /** Test values, not results: P01 has a zero Remembered mean, P02 none. */
  private lazy val zeroAndMissing: ParticipantMeans =
    val scale = right(ScaleIndex.of(2))
    def cell(p: String, g: Response, d: Option[Double], n: Option[Int]) =
      ParticipantCell(
        StudioRef.ParticipantSummary(run, reporting, scale, Some(g), p),
        g,
        p,
        d,
        n
      )
    val (r, f) = (Response.Remembered, Response.Forgotten)
    ParticipantMeans(
      run,
      reporting,
      scale,
      "2°",
      Vector(
        GroupGrandMean(StudioRef.GroupCell(run, reporting, scale, r), r, 0.10, 2),
        GroupGrandMean(StudioRef.GroupCell(run, reporting, scale, f), f, 0.05, 2)
      ),
      Vector(
        cell("P01", r, Some(0.0), Some(5)),
        cell("P02", r, None, Some(0)),
        cell("P03", r, Some(0.20), Some(6)),
        cell("P01", f, Some(0.10), Some(3)),
        cell("P02", f, Some(0.0), Some(4)),
        cell("P03", f, Some(-0.10), Some(2))
      )
    )

  fxStage.test("a missing mean is drawn as a dashed empty ring, apart from a zero mean") { fx =>
    val means                 = zeroAndMissing
    val source                = right(ParticipantMeans.source(means, columns))
    val w                     = Wired(fx)
    val t                     = showAndDraw(w, source)
    val plot                  = runOnFx(w.twin.plot).getOrElse(fail("no plot"))
    val zero                  = means.cells.head.ref
    val missing               = means.cells(1).ref
    def cells(ref: StudioRef) = rowTexts(w)(right(source.rowOf(ref).toRight(ref)))
    assertEquals(cells(zero)(2), "0.00")
    assertEquals(cells(missing)(2), PlotSource.MissingText)

    // The zero mean is placed on zero; the missing one has no position.
    assertEqualsDouble(placed(t, zero).y, 0.0, 1e-6)
    assertEquals(
      plot.markOf(missing).map(_.rows.map(_.marking)),
      Some(
        Vector(
          RowMarking.Positionless(eyes4s.studio.viz.plot.NoPosition.MissingValue(columns.d))
        )
      )
    )
    // It is drawn below the zero rule and every value, in its group's column.
    val zeroAt    = anchor(t, zero)
    val missingAt = anchor(t, missing)
    val lowest    = means.cells.flatMap(c => c.d.map(_ => anchor(t, c.ref).y)).max
    assert(missingAt.y > lowest + 2.0 * ParticipantPlot.EmptyRadiusPx, s"$missingAt vs $lowest")
    assertEqualsDouble(placed(t, missing).x, 0.0, 1e-6)

    // A zero mean is a filled ink dot; a missing one is empty, ringed in ink-3.
    val image   = snapshot(w)
    val ink     = Tokens.themed(Theme.Light, ThemedToken.Ink)
    val surface = Tokens.themed(Theme.Light, ThemedToken.Surface)
    val ink3    = Tokens.themed(Theme.Light, ThemedToken.Ink3)
    assert(near(image, zeroAt, ink, 40), "the zero mean is not an ink dot")
    assert(near(image, missingAt, surface, 12), "the missing mean is not empty")
    assert(!near(image, missingAt, ink, 40), "the missing mean is drawn as a value")
    val ring = (0 until 72).map { k =>
      val a = 2.0 * math.Pi * k / 72
      DevicePoint(
        missingAt.x + ParticipantPlot.EmptyRadiusPx * math.cos(a),
        missingAt.y + ParticipantPlot.EmptyRadiusPx * math.sin(a)
      )
    }
    val inked = ring.count(near(image, _, ink3, 60))
    assert(inked > 6, s"$inked of ${ring.size} ring points are ink-3")
    // Dashed: the ring has gaps.
    assert(inked < ring.size - 6, s"$inked of ${ring.size} ring points are ink-3")

    // Hovering it says it has no D and no queries.
    hoverAt(w, t, missingAt)
    assertEquals(w.hover, Some(HoverAt(plotView, missing)))
    assertEquals(
      w.readout,
      Some(s"Group Remembered, Mean of P02, D ${PlotSource.MissingText}, n 0")
    )
    fx.snapshot(StudioTheme.Light)
    runOnFx(w.twin.dispose())
  }
