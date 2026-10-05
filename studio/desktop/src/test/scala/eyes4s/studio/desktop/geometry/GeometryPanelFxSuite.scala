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

package eyes4s.studio.desktop.geometry

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.codec.ByteDigest
import eyes4s.studio.app.geometry.*
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.{FakeStudyBackend, StoryMoment, StoryMoments}
import eyes4s.studio.core.geometry.{CorrectionLedger, Placement, SourcePositions}
import eyes4s.studio.core.importing.GeometryField
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.harness.{FxStage, StageSize, StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.plot.PlotHostStatus
import eyes4s.studio.desktop.trial.GoldenTrials
import eyes4s.studio.desktop.typography.StudioFonts
import javafx.event.ActionEvent
import javafx.scene.control.Labeled
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.scene.layout.StackPane
import javafx.scene.text.Text

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** The geometry panel's JavaFX view (ticket S5.5; Data.dc.html, geometry)
  * on fixtures/studio-golden and the fake backend: the board's declared
  * geometry, the worked example of record 7,214, the placement pictures and
  * the counts ("543 of 11,520 records · 409 trials", outside screen apart);
  * every change redraws the pictures and creates a dataset draft; a switch
  * of the off-screen policy is such a change; a marked orientation is a
  * recorded rule, never a rewritten coordinate.
  *
  * Every interaction goes through the controls' own values and events, so
  * no test depends on OS window focus. The 250 ms redraw is measured as a
  * receipt (change → every picture on its canvas) and asserted against a
  * generous bound, so a loaded CI machine does not make it flaky; the
  * measured time is printed.
  */
class GeometryPanelFxSuite extends StudioFxSuite:
  import StoryMoments.{r2, r3}

  // The board's geometry column is 372 px wide.
  override protected def stageSize: StageSize = StageSize(372, 900)

  override def beforeAll(): Unit =
    super.beforeAll()
    runOnFx(StudioFonts.loadAll()).foreach(p => fail(p.message))

  /** The redraw the ticket asks for, and the bound a test enforces. */
  val RedrawTarget: Double = 250.0
  val RedrawBound: Double  = 4 * RedrawTarget

  private val r4 = DatasetRevision(4)

  private lazy val goldenBytes: IArray[Byte] =
    IArray.from(Files.readAllBytes(GoldenTrials.golden.resolve("fixations.csv")))

  /** The fake backend's placement previews and counts. */
  final class Inputs(moment: StoryMoment) extends GeometryInputs:
    private val backend      = FakeStudyBackend.create[IO](moment).unsafeRunSync()
    @volatile var placements = 0
    def placement(
        spec: DatasetRevisionSpec,
        done: Either[String, PlacementPreview] => Unit
    ): Unit =
      placements += 1
      backend.placement(spec).map(r => done(r.left.map(_.message))).unsafeRunAndForget()
    def admission(d: DatasetRevision, done: Either[String, AdmissionSummary] => Unit): Unit =
      backend.admission(d).map(r => done(r.left.map(_.message))).unsafeRunAndForget()

  /** The app's runtime reduced to what the pane needs: a model, updates, and
    * the pane synced after each.
    */
  final class Rig(start: AppModel, moment: StoryMoment):
    var model: AppModel             = start
    val inputs                      = Inputs(moment)
    val intents                     = scala.collection.mutable.ArrayBuffer.empty[Intent]
    lazy val host: GeometryPaneHost = GeometryPaneHost(() => model, dispatch, inputs)
    def dispatch(i: Intent): Unit   =
      intents += i
      model = AppModel.update(model, i)._1
      host.sync(model)

  def mount(fx: FxStage, start: AppModel, moment: StoryMoment): Rig =
    val rig    = Rig(start, moment)
    val sheets = StudioStyles.stylesheets(Theme.Light).fold(e => fail(e.message), identity)
    runOnFx {
      fx.scene.getStylesheets.setAll(sheets*)
      val root = StackPane(rig.host.node)
      root.getStyleClass.add("es")
      fx.scene.setRoot(root)
      rig.host.sync(rig.model)
    }
    fx.awaitLayout()
    rig

  /** Poll on the FX thread until `ready` holds. */
  def await(fx: FxStage, what: String, seconds: Int = 30)(ready: => Boolean): Unit =
    val until = System.nanoTime() + seconds * 1000000000L
    while !runOnFx(ready) do
      if System.nanoTime() > until then fail(s"timed out waiting for $what")
      Thread.sleep(10)
    fx.awaitLayout()

  def loaded(fx: FxStage, rig: Rig): Unit =
    await(fx, "records and counts") {
      rig.host.state.placement.toOption.isDefined && rig.host.state.counts.toOption.isDefined
    }

  /** The next redraw after `seen` receipts. */
  def redraw(fx: FxStage, rig: Rig, seen: Int): RedrawReceipt =
    await(fx, "a redraw")(rig.host.receipts.size > seen)
    val receipt = runOnFx(rig.host.receipts.last)
    println(f"GeometryPanelFxSuite: redraw ${receipt.generation} in ${receipt.millis}%.1f ms")
    receipt

  def drawn(l: Labeled): String = runOnFx {
    l.getChildrenUnmodifiable.asScala
      .collectFirst { case t: Text => t.getText }
      .getOrElse(l.getText)
  }

  def texts(fx: FxStage): Vector[String] = runOnFx {
    fx.scene.getRoot
      .lookupAll(".label")
      .asScala
      .collect { case l: Labeled if l.isVisible && l.getText.nonEmpty => l.getText }
      .toVector
  }

  def select(model: AppModel, ref: eyes4s.studio.core.selection.StudioRef): AppModel =
    StoryModels.play(model, m => StoryModels.select(m, "data.geometry", ref))

  def t1: AppModel = select(StoryModels.t1Data, StoryModels.record)
  def t2: AppModel =
    StoryModels.play(
      AppModel.open(StoryModels.t2, Some(StoryModels.project)),
      _ => Intent.Navigate(Location(Perspective.Data, Vector(Place.Dataset(r3))))
    )

  // ---------------------------------------------------------------------------

  test(
    "the golden source: 11,520 records, 543 outside the image frame in 409 trials, none off screen"
  ) {
    val spec      = StoryModels.t1.dataset(r3).get
    val positions = SourcePositions.read(spec, goldenBytes).fold(p => fail(p.message), identity)
    assertEquals((positions.records, positions.unplaced.size), (11520, 0))
    val ledger = CorrectionLedger.of(spec).fold(p => fail(p.message), identity)
    val placed = ledger.placeAll(positions.positions).fold(p => fail(p.message), identity)
    val window = placed.filter(_.placement == Placement.OutsideWindow)
    assertEquals(window.size, 543)
    assertEquals(window.map(_.source.trial).distinct.size, 409)
    assertEquals(placed.count(_.placement == Placement.OutsideScreen), 0)
    assertEquals(positions.position(7214).map(p => (p.x, p.y)), Some((1148.0, 456.0)))
  }

  fxStage.test("t1: the board's geometry, the worked example of record 7,214 and the counts") {
    fx =>
      assumeFullStage(fx)
      val rig = mount(fx, t1, StoryMoment.T1)
      loaded(fx, rig)
      redraw(fx, rig, 0)
      val v = rig.host.view
      assertEquals(drawn(v.title), "Geometry")
      assertEquals(drawn(v.kind), "Dataset · re-admit")
      val shown = texts(fx)
      Vector(
        "Gaze coordinates",
        "Screen px · origin top-left · y down",
        "1920 × 1080 px",
        "1024 × 768 at (448, 156)",
        "Image frame",
        "35",
        "· declared, not calibrated",
        "Record 7,214 raw",
        "screen (1148, 456) px",
        "→ image frame",
        "(700, 300) px · (+5.4°, +2.4°)",
        "from image centre · x right · y up",
        "Check placement",
        "All trials overlaid",
        "Outside image frame",
        "Outside screen"
      ).foreach(s => assert(shown.contains(s), s"'$s' is not shown: $shown"))
      assertEquals(drawn(v.outsideWindow.value), "543 of 11,520 records · 409 trials")
      assertEquals(drawn(v.outsideScreen.value), "0 of 11,520 records · 0 trials")
      assert(drawn(v.densityCaption).startsWith("11,520 records, screen coordinates."))
      assertEquals(
        runOnFx(v.physical.map(_.getPromptText)),
        Vector("not recorded", "not recorded")
      )
      // Every picture is on its canvas.
      val hosts = v.thumbnails.map(_.host) :+ v.density
      runOnFx(hosts.map(_.status.get)).foreach {
        case PlotHostStatus.Drawn(_) => ()
        case other                   => fail(s"a picture is not drawn: $other")
      }
      assertEquals(runOnFx(v.thumbnails.map(t => drawn(t.caption))).count(_.nonEmpty), 4)
      // P05's three failed queries lie wholly outside: the worst is shown.
      assert(
        runOnFx(v.thumbnails.map(t => drawn(t.caption)))
          .contains("P05 · ret_04 · 11 of 11 outside"),
        runOnFx(v.thumbnails.map(t => drawn(t.caption)))
      )
      fx.snapshot(StudioTheme.Light)
  }

  fxStage.test(
    "every change redraws the pictures within the bound and creates a dataset draft"
  ) { fx =>
    assumeFullStage(fx)
    val rig = mount(fx, t2, StoryMoment.T2)
    loaded(fx, rig)
    redraw(fx, rig, 0)
    assertEquals(rig.model.document.datasets.map(_.id), Vector(r2, r3))

    // A geometry edit on admitted r3 creates draft r4, which the selection follows.
    runOnFx {
      val top = rig.host.view.fields(GeometryField.ImageTop)
      top.setText("150")
      top.fireEvent(ActionEvent())
    }
    val first = redraw(fx, rig, 1)
    val draft = rig.model.document.dataset(r4).getOrElse(fail("no draft r4"))
    assertEquals(draft.decision, AdmissionDecision.Pending)
    assertEquals(draft.geometry.image.top, 150)
    assertEquals(rig.model.document.dataset(r3).map(_.geometry.image.top), Some(156))
    assertEquals(GeometryPanel.selected(rig.model).map(_.id), Some(r4))
    assertEquals((first.key.dataset, first.key.geometry.image.top), (r4, 150))
    assert(first.millis <= RedrawBound, s"redraw took ${first.millis} ms")
    // The draft's geometry differs from r3's, so the backend places it again.
    assertEquals(rig.inputs.placements, 2)

    // A second edit edits the pending draft in place.
    runOnFx {
      val ppd = rig.host.view.fields(GeometryField.PixelsPerDegree)
      ppd.setText("36")
      ppd.fireEvent(ActionEvent())
    }
    val second = redraw(fx, rig, 2)
    assertEquals(rig.model.document.datasets.map(_.id), Vector(r2, r3, r4))
    assertEquals(
      rig.model.document.dataset(r4).map(_.geometry.pixelsPerDegree.value),
      Some(36.0)
    )
    assertEquals(second.key.geometry.pixelsPerDegree.value, 36.0)
    assert(second.millis <= RedrawBound, s"redraw took ${second.millis} ms")
  }

  fxStage.test("the off-screen policy chooses from the keyboard: ↓ in its group (S10.5 K3)") {
    fx =>
      assumeFullStage(fx)
      val rig = mount(fx, t2, StoryMoment.T2)
      loaded(fx, rig)
      redraw(fx, rig, 0)
      val v = rig.host.view
      runOnFx(v.policies(OffScreenChoice.ExcludeRecord).requestFocus())
      fx.robot.press(javafx.scene.input.KeyCode.DOWN)
      redraw(fx, rig, 1)
      assertEquals(
        rig.model.document.dataset(r4).map(_.admission.offScreen),
        Some(OffScreenChoice.QuarantineTrial)
      )
  }

  fxStage.test(
    "switching the off-screen policy creates a draft; outside screen and outside window apart"
  ) { fx =>
    assumeFullStage(fx)
    val rig = mount(fx, t2, StoryMoment.T2)
    loaded(fx, rig)
    redraw(fx, rig, 0)
    val v = rig.host.view
    assertEquals(runOnFx(v.policies(OffScreenChoice.ExcludeRecord).isSelected), true)
    assert(drawn(v.outsideScreen.note).contains("not a quarantine"))
    runOnFx(v.policies(OffScreenChoice.QuarantineTrial).fire())
    val receipt = redraw(fx, rig, 1)
    val draft   = rig.model.document.dataset(r4).getOrElse(fail("no draft r4"))
    assertEquals(draft.admission.offScreen, OffScreenChoice.QuarantineTrial)
    assertEquals(draft.decision, AdmissionDecision.Pending)
    assertEquals(
      rig.model.document.dataset(r3).map(_.admission.offScreen),
      Some(OffScreenChoice.ExcludeRecord)
    )
    assertEquals(receipt.key.admission.offScreen, OffScreenChoice.QuarantineTrial)
    assert(receipt.millis <= RedrawBound, s"redraw took ${receipt.millis} ms")
    await(fx, "the parent's counts")(rig.host.state.counts.toOption.exists(_.dataset == r3))
    assertEquals(runOnFx(v.policies(OffScreenChoice.QuarantineTrial).isSelected), true)
    assertEquals(drawn(v.outsideWindow.value), "543 of 11,520 records · 409 trials")
    assertEquals(drawn(v.outsideScreen.value), "0 of 11,520 records · 0 trials")
    // The counts are r3's, admitted under ExcludeRecord: the note says so.
    assert(
      drawn(v.outsideScreen.note).contains("not a quarantine"),
      drawn(v.outsideScreen.note)
    )
    assertEquals(
      drawn(v.countsSource),
      "Counts: eyes4s admission of r3. r4 is pending; its own counts follow its verification."
    )
  }

  fxStage.test(
    "a marked wrong orientation is a recorded rule; the source coordinates never change"
  ) { fx =>
    assumeFullStage(fx)
    val rig = mount(fx, t1, StoryMoment.T1)
    loaded(fx, rig)
    redraw(fx, rig, 0)
    val v      = rig.host.view
    val before = runOnFx(rig.host.state.placement.toOption.get)
    val p05    = TrialKey("P05", Phase.Retrieval, "ret_04", 1)
    val at = runOnFx(v.thumbnails.indexWhere(t => drawn(t.caption).startsWith("P05 · ret_04")))
    assert(at >= 0)
    assertEquals(runOnFx(v.mark.isDisabled), true)
    runOnFx {
      val picture = v.thumbnails(at).picture
      picture.fireEvent(
        KeyEvent(KeyEvent.KEY_PRESSED, "", "", KeyCode.ENTER, false, false, false, false)
      )
    }
    assertEquals(runOnFx(rig.host.state.marked), Some(p05))
    assert(runOnFx(v.thumbnails(at).picture.getStyleClass.contains("marked")))
    // Choosing the trial redraws too (the worked example follows it).
    redraw(fx, rig, 1)
    runOnFx(v.mark.fire())
    fx.awaitLayout()
    assertEquals(drawn(v.orientationTitle), "Mark P05 · ret_04 as wrong orientation")
    runOnFx(v.record.fire())
    val receipt = redraw(fx, rig, 2)
    val rule    = CorrectionRule(CorrectionTarget.Trial(p05), CoordinateCorrection.FlipX)
    val spec    = rig.model.document.dataset(r3).get
    assertEquals(spec.admission.corrections, Vector(rule))
    assertEquals(spec.decision, AdmissionDecision.Pending)
    assertEquals(spec.sources, StoryModels.t1.dataset(r3).get.sources)
    assertEquals(spec.sources.fixations.map(_.bytes), Some(ByteDigest.sha256(goldenBytes)))
    // The records as recorded are the source's, unchanged; the drawn marks moved.
    // The redraw above was of the new placement (its key carries the rule).
    val after = runOnFx(rig.host.state.placement.toOption.get)
    assertEquals(
      after.records.map(r => (r.record, r.rawX, r.rawY)),
      before.records.map(r => (r.record, r.rawX, r.rawY))
    )
    assertEquals(receipt.key.admission.corrections, Vector(rule))
    val thumb = runOnFx(rig.host.shownPictures.get.thumbnails.find(_.trial == p05).get)
    val raw   = before.byTrial(p05)
    assertEquals(thumb.marks.map(m => (m.x, m.y)), raw.map(p => (1920.0 - p.rawX, p.rawY)))
    assert(thumb.marks.forall(_.corrected))
    assert(texts(fx).contains("1 · flip horizontally · P05 · ret_04"), texts(fx))
    assert(receipt.millis <= RedrawBound, s"redraw took ${receipt.millis} ms")
  }
