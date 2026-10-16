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
import eyes4s.studio.app.explore.DisplaySource
import eyes4s.studio.app.geometry.*
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.maps.{DensityCasing, DensityGround}
import eyes4s.studio.app.tokens.{StageToken, StageVariant, Theme, Tokens}
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.{FakeStudyBackend, StoryMoment, StoryMoments}
import eyes4s.studio.core.geometry.{CorrectionLedger, Placement, SourcePositions}
import eyes4s.studio.core.importing.GeometryField
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.explore.NavigatorDisplays
import eyes4s.studio.desktop.harness.{FxStage, StageSize, StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.plot.PlotHostStatus
import eyes4s.studio.desktop.shell.{A11yChecks, KeyboardFocus}
import eyes4s.studio.desktop.trial.GoldenTrials
import eyes4s.studio.desktop.typography.StudioFonts
import javafx.event.ActionEvent
import javafx.scene.control.Labeled
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.scene.layout.{Region, StackPane}
import javafx.scene.paint.Color
import javafx.scene.{AccessibleAttribute, AccessibleRole}
import javafx.scene.text.Text

import java.nio.file.{Files, StandardCopyOption}
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
    def displays(spec: DatasetRevisionSpec, done: Either[String, DisplaySource] => Unit): Unit =
      NavigatorDisplays.golden.read(spec, done)

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
    await(fx, "records, counts and displays") {
      rig.host.state.placement.toOption.isDefined && rig.host.state.counts.toOption.isDefined &&
      rig.host.state.displays.toOption.isDefined
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

  /** A press of `code` on `n`, as the keyboard delivers it to the focus owner. */
  def press(n: javafx.scene.Node, code: KeyCode): Unit =
    n.fireEvent(KeyEvent(KeyEvent.KEY_PRESSED, "", "", code, false, false, false, false))

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
      val captions = runOnFx(v.thumbnails.map(t => (drawn(t.name), drawn(t.count))))
      assertEquals(captions.count(_._2.nonEmpty), 4)
      // P05's three failed queries lie wholly outside: the worst is shown,
      // its trial and its count on two lines.
      assert(captions.contains(("P05 · ret_04", "11 of 11 outside")), captions)
      // The fixture's first missing image (review ticket 25): its trial is a
      // representative, hatched, captioned by its file in the warning ink.
      assert(captions.contains(("P01 · enc_08", "Missing asset · forest-044.png")), captions)
      val warned = runOnFx(
        v.thumbnails
          .filter(_.count.getStyleClass.contains("geometry-warn"))
          .map(t => t.name.getText)
      )
      assertEquals(warned, Vector("P01 · enc_08"))
      val missingThumb = runOnFx(
        v.thumbnails.find(_.name.getText == "P01 · enc_08").map(_.node.getAccessibleText)
      )
      assert(
        missingThumb.exists(
          _.startsWith("P01 · enc_08: missing asset, forest-044.png not found")
        ),
        missingThumb
      )
      // The density key at the golden fixture's levels (s = 4, DESIGN_SPEC
      // section 15.1), in closed ranges.
      assertEquals(
        runOnFx(v.densitySwatches.map(drawn)),
        Vector("1–3", "4–15", "16–63", "≥64")
      )
      assertEquals(
        drawn(v.densityKey),
        "Records per cell, log steps · 64 × 36 screen cells"
      )
      fx.snapshot(StudioTheme.Light)
  }

  fxStage.test(
    "the density key follows the stage: opaque swatches, each with its stage-ground band"
  ) { fx =>
    assumeFullStage(fx)
    val rig = mount(fx, t1, StoryMoment.T1)
    loaded(fx, rig)
    redraw(fx, rig, 0)
    val v                                                   = rig.host.view
    def fxColour(c: eyes4s.studio.app.tokens.Colour): Color = Color.web(c.javaFxCss)
    val stages                                              = Vector(
      StageAppearance.Dark  -> StageVariant.Dark,
      StageAppearance.Mid   -> StageVariant.Mid,
      StageAppearance.Light -> StageVariant.Light
    )
    stages.foreach { (appearance, stage) =>
      val seen   = rig.host.receipts.size
      val before = rig.model.document.presentation.stage
      runOnFx(rig.dispatch(Intent.Dispatch(Command.SetStage(appearance))))
      assertEquals(rig.model.document.presentation.stage, appearance)
      // A stage change redraws the density, so that its cells and casing
      // match the key.
      if before != appearance then redraw(fx, rig, seen): Unit
      fx.awaitLayout()
      val swatches = runOnFx(v.densitySwatches.map { l =>
        val chip = l.getGraphic.asInstanceOf[Region]
        chip.applyCss()
        val fill    = chip.getChildrenUnmodifiable.get(0).asInstanceOf[Region]
        val ground  = chip.getBackground.getFills.asScala.map(_.getFill).toVector
        val strokes = Option(chip.getBorder).toVector.flatMap(
          _.getStrokes.asScala.map(b =>
            (b.getTopStroke, b.getWidths.getTop, b.getInsets.getTop)
          )
        )
        (fill.getOpacity, ground, strokes)
      })
      swatches.zipWithIndex.foreach { case ((opacity, ground, strokes), level) =>
        val what = s"${stage.cssName} stage, swatch ${level + 1}"
        assertEquals(opacity, 1.0, what)
        assertEquals(ground, Vector(fxColour(Tokens.staged(stage, StageToken.Stage))), what)
        // A swatch sits on the stage: the stage ground's band, 1 px outside.
        val expected = DensityCasing
          .band(level + 1, stage, DensityGround.Stage)
          .map(b => (fxColour(b.colour(stage)), 1.0, -1.0))
          .toVector
        assertEquals(strokes, expected, what)
      }
      // >=64 on Dark; 4-15 to >=64 on Mid; 1-3 and 4-15 on Light.
      assertEquals(
        swatches.map(_._3.nonEmpty),
        stage match
          case StageVariant.Dark  => Vector(false, false, false, true)
          case StageVariant.Mid   => Vector(false, true, true, true)
          case StageVariant.Light => Vector(true, true, false, false)
      )
      fx.snapshot(StudioTheme.Light).foreach { p =>
        Files.copy(
          p,
          p.resolveSibling(s"density-${stage.cssName}-${p.getFileName}"),
          StandardCopyOption.REPLACE_EXISTING
        )
      }
    }
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

  /** The background fills of `r` as (colour, top inset). */
  private def fills(r: Region): List[(Color, Double)] =
    Option(r.getBackground).toList.flatMap(
      _.getFills.asScala.toList.map(f => (f.getFill.asInstanceOf[Color], f.getInsets.getTop))
    )

  private val SansSemibold = "IBM Plex Sans SmBld"
  private val MonoMedium   = "IBM Plex Mono Medm"

  private def light(t: eyes4s.studio.app.tokens.ThemedToken): Color =
    val c = eyes4s.studio.app.tokens.Tokens.themed(Theme.Light, t)
    Color.rgb(c.red, c.green, c.blue, c.alphaPercent / 100.0)

  fxStage.test(
    "the thumbnails are one listbox stop: a cursor framed under keyboard focus, keys, Enter marks"
  ) { fx =>
    assumeFullStage(fx)
    val rig = mount(fx, t1, StoryMoment.T1)
    loaded(fx, rig)
    redraw(fx, rig, 0)
    val v      = rig.host.view
    val strip  = v.strip
    val items  = v.thumbnails.map(_.node)
    val accent = light(eyes4s.studio.app.tokens.ThemedToken.Accent)
    val frame = List(accent -> -2.0, light(eyes4s.studio.app.tokens.ThemedToken.Surface) -> 0.0)
    def framed: Vector[Boolean] = runOnFx {
      strip.getScene.getRoot.applyCss()
      items.map(n => fills(n).exists(_._1 == accent))
    }
    def focusItem = runOnFx(strip.queryAccessibleAttribute(AccessibleAttribute.FOCUS_ITEM))

    // A listbox named as the board names it; its items read as the thumbnails.
    assertEquals(runOnFx(strip.getAccessibleRole), AccessibleRole.LIST_VIEW)
    assertEquals(
      runOnFx(strip.getAccessibleText),
      "Placement check, 4 trials; arrow keys move, Enter or Space chooses the trial to mark"
    )
    assertEquals(
      runOnFx(strip.queryAccessibleAttribute(AccessibleAttribute.ITEM_COUNT)),
      Integer.valueOf(4)
    )
    runOnFx(v.thumbnails.zipWithIndex.foreach { (t, i) =>
      assertEquals(t.node.getAccessibleRole, AccessibleRole.LIST_ITEM)
      assert(
        t.node.getAccessibleText.startsWith(s"${t.name.getText}: "),
        t.node.getAccessibleText
      )
      assert(
        strip.queryAccessibleAttribute(AccessibleAttribute.ITEM_AT_INDEX, Integer.valueOf(i)) eq
          t.node
      )
    })

    // One Tab stop: nothing inside the strip takes the focus, and Tab
    // leaves the strip for the density.
    val inside = runOnFx {
      def all(n: javafx.scene.Node): Vector[javafx.scene.Node] = n +: (n match
        case p: javafx.scene.Parent => p.getChildrenUnmodifiable.asScala.toVector.flatMap(all)
        case _                      => Vector.empty)
      all(strip).filter(_.isFocusTraversable)
    }
    assertEquals(inside.map(_.toString), Vector(strip.toString))
    runOnFx {
      KeyboardFocus.byKeyboard(strip)
      strip.requestFocusTraversal(javafx.scene.TraversalDirection.NEXT)
    }
    assertEquals(
      runOnFx(fx.scene.getFocusOwner.getAccessibleText),
      runOnFx(v.density.getParent.getAccessibleText)
    )

    // A click or the program focuses it with no frame; the keyboard frames
    // the home thumbnail (none is marked: the first), and only it.
    runOnFx(KeyboardFocus.byPointer(strip))
    assertEquals(framed, Vector.fill(4)(false))
    runOnFx(KeyboardFocus.byKeyboard(strip))
    assertEquals(framed, Vector(true, false, false, false))
    assertEquals(runOnFx(fills(items(0))), frame)
    assert(focusItem eq items(0))
    // The strip itself draws no ring.
    assert(runOnFx(!fills(strip).exists(_._1 == accent)))

    // Right steps, Down moves a row, Home and End go to the ends.
    def at(code: KeyCode): Vector[Boolean] =
      runOnFx(press(strip, code))
      framed
    assertEquals(at(KeyCode.RIGHT), Vector(false, true, false, false))
    assertEquals(at(KeyCode.DOWN), Vector(false, false, false, true))
    assertEquals(at(KeyCode.LEFT), Vector(false, false, true, false))
    assertEquals(at(KeyCode.UP), Vector(true, false, false, false))
    assertEquals(at(KeyCode.END), Vector(false, false, false, true))
    assertEquals(at(KeyCode.HOME), Vector(true, false, false, false))
    assertEquals(at(KeyCode.DOWN), Vector(false, false, true, false))
    assert(focusItem eq items(2))

    // Enter marks the cursor's trial, as a click on its thumbnail does.
    val third = runOnFx(v.thumbnails(2).trial).getOrElse(fail("no third thumbnail"))
    runOnFx(press(strip, KeyCode.ENTER))
    assertEquals(runOnFx(rig.host.state.marked), Some(third))
    // The marked trial is selected: the selection fill inside the cursor
    // frame, its trial in Mono 500 and its count at weight 600, and no
    // accent ring of its own (DESIGN_SPEC section 15).
    val selFill = light(eyes4s.studio.app.tokens.ThemedToken.SelFill)
    def picked  = runOnFx {
      strip.getScene.getRoot.applyCss()
      val t = v.thumbnails(2)
      (
        fills(t.node),
        Option(t.picture.getBorder).exists(!_.getStrokes.isEmpty),
        t.name.getFont.getFamily,
        t.count.getFont.getFamily
      )
    }
    assertEquals(
      picked,
      (List(accent -> -2.0, selFill -> 0.0), false, MonoMedium, SansSemibold)
    )
    assertEquals(runOnFx(v.thumbnails(1).name.getFont.getFamily) == MonoMedium, false)
    assertEquals(
      runOnFx(
        strip
          .queryAccessibleAttribute(AccessibleAttribute.SELECTED_ITEMS)
          .asInstanceOf[java.util.List[?]]
          .asScala
          .toVector
          .map(n => n.asInstanceOf[AnyRef] eq items(2))
      ),
      Vector(true)
    )
    // Off the strip, no frame: the selection fill alone, and no accent.
    runOnFx(KeyboardFocus.away(strip))
    assertEquals(framed, Vector.fill(4)(false))
    assertEquals(picked, (List(selFill -> -2.0), false, MonoMedium, SansSemibold))
    assertEquals(runOnFx(fills(items(1))), Nil)
    // The selected thumbnail's caption stays legible over the selection fill.
    assertEquals(runOnFx(A11yChecks.lowContrast(strip)), Vector.empty[String])
  }

  fxStage.test("the thumbnails' home: a focused strip with no cursor frames the marked trial") {
    fx =>
      assumeFullStage(fx)
      val rig = mount(fx, t1, StoryMoment.T1)
      loaded(fx, rig)
      redraw(fx, rig, 0)
      val v     = rig.host.view
      val third = runOnFx(v.thumbnails(2).trial).getOrElse(fail("no third thumbnail"))
      runOnFx(rig.host.dispatch(GeometryIntent.MarkTrial(third)))
      fx.awaitLayout()
      val accent = light(eyes4s.studio.app.tokens.ThemedToken.Accent)
      runOnFx(KeyboardFocus.byKeyboard(v.strip))
      val framed = runOnFx {
        v.strip.getScene.getRoot.applyCss()
        v.thumbnails.map(t => fills(t.node).exists(_._1 == accent))
      }
      assertEquals(framed, Vector(false, false, true, false))
      assertEquals(runOnFx(v.strip.cursor), Some(2))
      // Space marks the cursor's trial, as Enter does.
      val fourth = runOnFx(v.thumbnails(3).trial).getOrElse(fail("no fourth thumbnail"))
      runOnFx(press(v.strip, KeyCode.RIGHT))
      runOnFx(press(v.strip, KeyCode.SPACE))
      assertEquals(runOnFx(rig.host.state.marked), Some(fourth))
      // A click moves the cursor to its thumbnail and marks it.
      val first = runOnFx(v.thumbnails(0).trial).getOrElse(fail("no first thumbnail"))
      runOnFx(
        v.thumbnails(0)
          .node
          .fireEvent(
            javafx.scene.input.MouseEvent(
              javafx.scene.input.MouseEvent.MOUSE_CLICKED,
              1,
              1,
              1,
              1,
              javafx.scene.input.MouseButton.PRIMARY,
              1,
              false,
              false,
              false,
              false,
              true,
              false,
              false,
              true,
              false,
              false,
              null
            )
          )
      )
      assertEquals(runOnFx(rig.host.state.marked), Some(first))
      assertEquals(runOnFx(v.strip.cursor), Some(0))
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
    val at     = runOnFx(v.thumbnails.indexWhere(t => drawn(t.name) == "P05 · ret_04"))
    assert(at >= 0)
    assertEquals(runOnFx(v.mark.isDisabled), true)
    // The strip's cursor to P05's thumbnail, then Enter (S29).
    runOnFx {
      KeyboardFocus.byKeyboard(v.strip)
      press(v.strip, KeyCode.HOME)
      (0 until at).foreach(_ => press(v.strip, KeyCode.RIGHT))
      press(v.strip, KeyCode.ENTER)
    }
    assertEquals(runOnFx(rig.host.state.marked), Some(p05))
    // Rebuilding the pictures can move the marked trial to another slot.
    redraw(fx, rig, 1)
    val markedAt = runOnFx(v.thumbnails.indexWhere(t => drawn(t.name) == "P05 · ret_04"))
    assert(markedAt >= 0)
    assert(
      runOnFx(
        v.thumbnails(markedAt).node.getPseudoClassStates.contains(GeometryPanelView.Selected)
      )
    )
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
