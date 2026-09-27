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

package eyes4s.studio.desktop.importing

import eyes4s.studio.app.importing.*
import eyes4s.studio.app.tokens.{Colour, Theme, ThemedToken, Tokens}
import eyes4s.studio.app.{Intent, StoryModels}
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.command.{ChangeKind, Command}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.importing.{ColumnChoice, ImportPreset, ImportPresets}
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.harness.{
  FxStage,
  SnapshotScale,
  StageSize,
  StudioFxSuite,
  StudioTheme
}
import eyes4s.studio.desktop.platform.{FilePresetStore, TempDirs}
import eyes4s.studio.desktop.trial.GoldenTrials
import eyes4s.studio.desktop.typography.StudioFonts
import javafx.scene.control.{ComboBox, Labeled}
import javafx.geometry.Point2D
import javafx.scene.layout.StackPane
import javafx.scene.text.Text

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import javax.imageio.ImageIO
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** The import wizard's JavaFX view (ticket S5.2; Data.dc.html, column
  * mapping) on fixtures/studio-golden: the board's four columns, role menus
  * that dispatch intents, the typed required-role issue naming its column,
  * the "Dataset · re-admit" commands a commit sends to the app, and presets
  * saved to disk that re-apply to a second file. Every interaction goes
  * through the robot's synthetic events or the controls' own values, so no
  * test depends on OS window focus.
  */
class ImportWizardFxSuite extends StudioFxSuite:
  import StoryModels.{empty, ok, t1, t2}

  // The column-mapping group of the Data board is about this size.
  override protected def stageSize: StageSize = StageSize(960, 640)

  override def beforeAll(): Unit =
    super.beforeAll()
    runOnFx(StudioFonts.loadAll()).foreach(p => fail(p.message))

  val fixations: Path = GoldenTrials.golden.resolve("fixations.csv")
  val trials: Path    = GoldenTrials.golden.resolve("trials.csv")

  /** A platform with no dialogs: files are read by the test. */
  final class Recorder(
      store: Option[FilePresetStore] = None,
      refuseInputs: Option[String] = None,
      refusePresets: Option[String] = None
  ) extends ImportPlatform:
    val stored                                     = mutable.ArrayBuffer.empty[ImportPreset]
    def chooseFile(role: SourceRole): Option[Path] = None
    def storePreset(p: ImportPreset): Either[String, Unit] =
      refusePresets.fold {
        stored += p
        store.fold(Right(()))(_.save(p))
      }(Left(_))
    val imported = mutable.ArrayBuffer.empty[String]
    def importInput(
        source: Source,
        bytes: IArray[Byte],
        done: Either[String, Unit] => Unit
    ): Unit =
      refuseInputs.fold {
        imported += source.path.value
        done(Right(()))
      }(reason => done(Left(reason)))

  final case class Mounted(
      host: ImportWizardHost,
      app: mutable.ArrayBuffer[Intent],
      closed: () => Boolean,
      platform: Recorder
  ):
    def view: ImportWizardView = host.view

  def mount(
      fx: FxStage,
      wizard: ImportWizard,
      document: StudioDocument,
      theme: Theme = Theme.Light,
      platform: Recorder = Recorder()
  ): Mounted =
    val app    = mutable.ArrayBuffer.empty[Intent]
    var closed = false
    val host   = runOnFx(
      ImportWizardHost(wizard, () => document, app += _, platform, () => closed = true)
    )
    val sheets = StudioStyles.stylesheets(theme).fold(e => fail(e.message), identity)
    runOnFx {
      fx.scene.getStylesheets.setAll(sheets*)
      val root = StackPane(host.view.node)
      root.getStyleClass.add("es")
      fx.scene.setRoot(root)
    }
    fx.awaitLayout()
    Mounted(host, app, () => closed, platform)

  /** Read a file through the host (off the FX thread) and wait for it. */
  def readIn(
      fx: FxStage,
      m: Mounted,
      role: SourceRole,
      path: Path,
      name: Option[String] = None
  ): Unit =
    runOnFx(m.host.read(role, path, name)).get(30, java.util.concurrent.TimeUnit.SECONDS)
    fx.awaitLayout()

  def drawn(l: Labeled): String = runOnFx {
    l.getChildrenUnmodifiable.asScala
      .collectFirst { case t: Text => t.getText }
      .getOrElse(l.getText)
  }

  def menu(m: Mounted, column: String): ComboBox[ChoiceVM] =
    runOnFx(m.view.fixations.menu(column)).getOrElse(fail(s"no role menu for $column"))

  /** Choose `choice` in `column`'s role menu, as a user's pick does. */
  def pick(fx: FxStage, m: Mounted, column: String, choice: ColumnChoice): Unit =
    val box = menu(m, column)
    runOnFx(box.getItems.asScala.find(_.choice == choice).foreach(box.setValue))
    fx.awaitLayout()

  def declareMs(fx: FxStage, m: Mounted): Unit =
    runOnFx {
      val t = m.view.time
      t.getItems.asScala.find(_.unit.contains(TimeUnit.Milliseconds)).foreach(t.setValue)
    }
    fx.awaitLayout()

  def dispatched(m: Mounted): Vector[Command] =
    m.app.toVector.collect { case Intent.Dispatch(c) => c }

  fxStage.test("golden fixations.csv: the board's four columns, one 28 px row per column") {
    fx =>
      assumeFullStage(fx)
      val m = mount(fx, ImportWizard.newImport(t2, ImportPresets.empty), t2)
      readIn(fx, m, SourceRole.Fixations, fixations)
      fx.awaitLayout()
      val table = m.view.fixations
      assertEquals(
        runOnFx(table.head.getChildren.asScala.toVector.collect { case l: Labeled =>
          l.getText
        }),
        Vector("fixations.csv column", "First records", "Role", "Units (declared)")
      )
      val header = Files.readAllLines(fixations, UTF_8).get(0).split(",").toVector
      val rows   = runOnFx(table.rows.getChildren.asScala.toVector)
      assertEquals(rows.size, header.size)
      rows.foreach(r => assertEqualsDouble(runOnFx(r.getLayoutBounds.getHeight), 28, 0.5))
      assertEquals(
        header.map(c => runOnFx(menu(m, c).getAccessibleText)),
        header.map(c => s"Role for $c")
      )
      assertEquals(
        drawn(table.summary),
        "fixations.csv · 11,520 records · 10 columns · comma-separated"
      )
      assertEquals(
        header.map(c => runOnFx(Option(menu(m, c).getValue).map(_.label).getOrElse(""))),
        Vector(
          "Participant",
          "Phase",
          "Trial",
          "Occurrence",
          "Ordinal",
          "x",
          "y",
          "Onset",
          "Duration",
          "Sample count"
        )
      )
      // The closed menus draw the whole role (the board's words, untruncated).
      assertEquals(
        header.map(c => runOnFx(Option(menu(m, c).getButtonCell).map(drawn))).last,
        Some("Sample count")
      )
      // Units: declared, never inferred from onset_ms.
      val units = runOnFx(
        rows.map(
          _.asInstanceOf[javafx.scene.layout.GridPane].getChildren
            .get(3)
            .asInstanceOf[Labeled]
            .getText
        )
      )
      assertEquals(
        units.drop(5),
        Vector("screen px", "screen px", "undeclared", "undeclared", "samples")
      )
      fx.snapshot(StudioTheme.Light)
  }

  fxStage.test("removing a required role blocks the commit with an issue naming the column") {
    fx =>
      val m = mount(fx, ImportWizard.newImport(t2, ImportPresets.empty), t2)
      readIn(fx, m, SourceRole.Fixations, fixations)
      declareMs(fx, m)
      pick(fx, m, "sample_count", ColumnChoice.Attribute)
      assertEquals(
        runOnFx(m.view.tabs(WizardTab.FixationMapping).getText),
        "Column mapping (1)"
      )
      fx.robot.click(m.view.commit)
      assertEquals(dispatched(m), Vector.empty)
      assert(runOnFx(m.view.issues.isVisible))
      assert(!runOnFx(m.view.fixations.page.isVisible))
      val issues = runOnFx(m.view.issueList.getChildren.asScala.toVector.collect {
        case l: Labeled => l.getText
      })
      assertEquals(issues.size, 1)
      assert(issues.head.contains("sample count role"), issues.head)
      assert(issues.head.contains("unassigned columns: sample_count"), issues.head)
      assertEquals(drawn(m.view.problem), "1 issue blocks the import.")
  }

  fxStage.test("a commit sends one Dataset · re-admit ImportSources to the app and closes") {
    fx =>
      val m = mount(fx, ImportWizard.newImport(t2, ImportPresets.empty), t2)
      readIn(fx, m, SourceRole.Fixations, fixations)
      readIn(fx, m, SourceRole.Trials, trials)
      declareMs(fx, m)
      assertEquals(drawn(m.view.commit), "Import as r4")
      fx.robot.click(m.view.commit)
      dispatched(m) match
        case Vector(c @ Command.ImportSources(parent, sources, mapping, units, _)) =>
          assertEquals(c.kind, ChangeKind.DatasetReadmit)
          assertEquals(parent, Some(DatasetRevision(3)))
          // The same bytes as the story moment's sources: the golden files.
          assertEquals(
            sources.entries.map(_.bytes),
            t2.dataset(DatasetRevision(3)).get.sources.entries.map(_.bytes)
          )
          assertEquals(mapping, t2.dataset(DatasetRevision(3)).get.mapping)
          assertEquals(units.time, Some(TimeUnit.Milliseconds))
        case other => fail(s"expected one ImportSources, got $other")
      assert(m.closed())
      assertEquals(runOnFx(m.view.kind.getText), "Dataset · re-admit")
      // Both files' bytes go into the project with the command.
      assertEquals(m.platform.imported.toVector, Vector("fixations.csv", "trials.csv"))
  }

  fxStage.test("a preset saved to disk re-applies to a second file") { fx =>
    val dir = Files.createTempDirectory("eyes4s-presets")
    try
      val store = FilePresetStore(dir)
      val first = mount(
        fx,
        ImportWizard.newImport(t2, ImportPresets.empty),
        t2,
        platform = Recorder(Some(store))
      )
      readIn(fx, first, SourceRole.Fixations, fixations)
      declareMs(fx, first)
      // A decision no suggestion makes: occurrence stays an attribute.
      pick(fx, first, "occurrence", ColumnChoice.Attribute)
      assert(runOnFx(first.view.presetSave.isDisabled))
      fx.robot.click(first.view.presetName)
      runOnFx(first.view.presetName.setText("Golden export"))
      fx.awaitLayout()
      fx.robot.click(first.view.presetSave)
      assertEquals(first.platform.stored.map(_.name.value).toVector, Vector("Golden export"))
      assertEquals(drawn(first.view.status), "Saved preset Golden export.")

      // A second session's file: the same export with its columns reordered.
      val lines  = Files.readAllLines(fixations, UTF_8).asScala.take(50).toVector
      val second = dir.resolve("session2.csv")
      Files.writeString(
        second,
        lines
          .map(l => {
            val c = l.split(",", -1).toVector; (c.drop(4) ++ c.take(4)).mkString(",")
          })
          .mkString("\n"),
        UTF_8
      )
      val (presets, problems) = store.load
      assertEquals(problems, Vector.empty)
      val next =
        mount(fx, ImportWizard.newImport(t2, presets), t2, platform = Recorder(Some(store)))
      readIn(fx, next, SourceRole.Fixations, second)
      // Before the preset: suggestions, and no time unit declared.
      assertEquals(
        runOnFx(Option(menu(next, "occurrence").getValue).map(_.choice)),
        Some(ColumnChoice.Role(ColumnRole.Occurrence))
      )
      assertEquals(runOnFx(Option(next.view.time.getValue).flatMap(_.unit)), None)
      runOnFx(next.view.presetSelect.setValue("Golden export"))
      fx.robot.click(next.view.presetApply)
      assertEquals(drawn(next.view.status), "Applied preset Golden export to session2.csv.")
      fx.robot.click(next.view.commit)
      dispatched(next) match
        case Vector(Command.ImportSources(_, _, mapping, units, _)) =>
          assertEquals(mapping.column(ColumnRole.Occurrence), None)
          assertEquals(
            mapping.bindings,
            t2.dataset(DatasetRevision(3))
              .get
              .mapping
              .bindings
              .filterNot(_.role == ColumnRole.Occurrence)
          )
          assertEquals(units.time, Some(TimeUnit.Milliseconds))
        case other => fail(s"expected one ImportSources, got $other")
    finally TempDirs.remove(dir)
  }

  fxStage.test(
    "re-mapping pending r3 applies SetUnits in place; unchanged rows keep their nodes"
  ) { fx =>
    val m = mount(fx, ok(ImportWizard.remap(t1, DatasetRevision(3), ImportPresets.empty)), t1)
    readIn(fx, m, SourceRole.Fixations, fixations, Some("inputs/fixations.csv"))
    assertEquals(drawn(m.view.commit), "Apply to r3")
    val xMenu = menu(m, "x")
    // Rows are updated in place, the one just picked in included, so a
    // focused menu keeps its focus across renders.
    val occurrence = menu(m, "occurrence")
    pick(fx, m, "occurrence", ColumnChoice.Attribute)
    assert(menu(m, "x") eq xMenu)
    assert(menu(m, "occurrence") eq occurrence)
    assertEquals(
      runOnFx(Option(occurrence.getValue).map(_.choice)),
      Some(ColumnChoice.Attribute)
    )
    pick(fx, m, "occurrence", ColumnChoice.Role(ColumnRole.Occurrence))
    runOnFx {
      val t = m.view.time
      t.getItems.asScala.find(_.unit.contains(TimeUnit.Microseconds)).foreach(t.setValue)
    }
    fx.awaitLayout()
    fx.robot.click(m.view.commit)
    assertEquals(
      dispatched(m),
      Vector(Command.SetUnits(DatasetRevision(3), DeclaredUnits(Some(TimeUnit.Microseconds))))
    )
  }

  fxStage.test(
    "a first import declares geometry on the Geometry tab; typed fields reach the model"
  ) { fx =>
    val m = mount(fx, ImportWizard.newImport(empty, ImportPresets.empty), empty)
    readIn(fx, m, SourceRole.Fixations, fixations)
    declareMs(fx, m)
    fx.robot.click(m.view.commit)
    assert(runOnFx(m.view.geometry.isVisible))
    assertEquals(runOnFx(m.view.tabs(WizardTab.Geometry).isSelected), true)
    val values = Vector("1920", "1080", "448", "156", "1024", "768", "35")
    eyes4s.studio.core.importing.GeometryField.values.toVector.zip(values).foreach { (f, v) =>
      val field = m.view.geometryFields(f)
      fx.robot.click(field)
      runOnFx(field.setText(v))
    }
    fx.awaitLayout()
    assertEquals(m.host.model.geometry.parse.map(_.screen.render), Right("1920×1080 px"))
    fx.robot.click(m.view.commit)
    assertEquals(dispatched(m).map(_.kind), Vector(ChangeKind.DatasetReadmit))
  }

  fxStage.test("tabs switch pages by click; snapshots in both themes") { fx =>
    val m = mount(fx, ImportWizard.newImport(t2, ImportPresets.empty), t2, theme = Theme.Dark)
    readIn(fx, m, SourceRole.Fixations, fixations)
    readIn(fx, m, SourceRole.Trials, trials)
    fx.robot.click(m.view.tabs(WizardTab.TrialMetadata))
    assert(runOnFx(m.view.trials.page.isVisible))
    assertEquals(runOnFx(m.view.trials.rows.getChildren.size), 8)
    assertEquals(
      runOnFx(m.view.trials.menu("display_kind").map(_.getValue.label)),
      Some("Attribute (pass-through)")
    )
    fx.robot.click(m.view.tabs(WizardTab.FixationMapping))
    assert(runOnFx(m.view.fixations.page.isVisible))
    fx.snapshot(StudioTheme.Dark)
  }

  fxStage.test("pixels: token surfaces in both themes; the issue row's column in --fail") {
    fx =>
      assumeFullStage(fx)
      Vector(Theme.Light -> StudioTheme.Light, Theme.Dark -> StudioTheme.Dark).foreach {
        (theme, snapshotTheme) =>
          val m = mount(fx, ImportWizard.newImport(t2, ImportPresets.empty), t2, theme = theme)
          readIn(fx, m, SourceRole.Fixations, fixations)
          fx.awaitLayout()
          val file  = fx.snapshot(snapshotTheme, List(SnapshotScale.X1)).head
          val image = ImageIO.read(file.toFile)
          def rgb(p: Point2D): (Int, Int, Int) =
            val argb = image.getRGB(math.floor(p.getX).toInt, math.floor(p.getY).toInt)
            ((argb >> 16) & 0xff, (argb >> 8) & 0xff, argb & 0xff)
          def near(a: (Int, Int, Int), c: Colour, tolerance: Int): Boolean =
            math.abs(a._1 - c.red) <= tolerance && math.abs(a._2 - c.green) <= tolerance &&
              math.abs(a._3 - c.blue) <= tolerance
          // Below the last row, inside the table's scroll viewport: --surface.
          val below = runOnFx {
            val rows = m.view.fixations.rows
            rows.localToScene(Point2D(rows.getWidth / 2, rows.getHeight + 20))
          }
          val surface = Tokens.themed(theme, ThemedToken.Surface)
          assert(
            near(rgb(below), surface, 2),
            s"$theme: ${rgb(below)} is not ${surface.hexRgb}"
          )
          // onset_ms has an issue (its unit is undeclared): its name is drawn in --fail.
          val fail  = Tokens.themed(theme, ThemedToken.Fail)
          val onset = runOnFx(m.view.fixations.rowNode("onset_ms").get.getChildren.get(0))
          val box   = runOnFx(onset.localToScene(onset.getLayoutBounds))
          val inked = for
            x <- box.getMinX.toInt until box.getMaxX.toInt
            y <- box.getMinY.toInt until box.getMaxY.toInt
            if near(rgb(Point2D(x, y)), fail, 40)
          yield (x, y)
          assert(inked.size > 10, s"$theme: onset_ms is not drawn in ${fail.hexRgb}")
          // A row without an issue is drawn in --ink, never --fail.
          val xName = runOnFx(m.view.fixations.rowNode("x").get.getChildren.get(0))
          val xBox  = runOnFx(xName.localToScene(xName.getLayoutBounds))
          val xFail = for
            x <- xBox.getMinX.toInt until xBox.getMaxX.toInt
            y <- xBox.getMinY.toInt until xBox.getMaxY.toInt
            if near(rgb(Point2D(x, y)), fail, 40)
          yield (x, y)
          assertEquals(xFail.size, 0, s"$theme: x is drawn in --fail")
      }
  }

  fxStage.test("a new import's files are stored in the project, so the next save succeeds") {
    fx =>
      import cats.effect.IO
      import cats.effect.unsafe.implicits.global
      import eyes4s.studio.core.bundle.{BundleSamples, LockOwner, SharingOptions}
      import eyes4s.studio.core.command.JournalEntry
      import eyes4s.studio.core.session.ProjectSession
      import eyes4s.studio.desktop.platform.FileProjectStore
      import eyes4s.studio.desktop.runtime.SessionPort
      val dir = Files.createTempDirectory("eyes4s-import")
      try
        val store   = FileProjectStore.at[IO](dir.resolve("memory-study.eyes")).unsafeRunSync()
        val owner   = LockOwner.of("ImportWizardFxSuite").fold(e => fail(e.toString), identity)
        val session = ProjectSession
          .create(store, owner, t2, SharingOptions.complete, BundleSamples.inputsFor(t2))
          .unsafeRunSync()
          .fold(e => fail(e.message), identity)
        val port = SessionPort.start(session)
        // A second session's export: bytes the project does not hold yet.
        val second = dir.resolve("session2.csv")
        Files.writeString(
          second,
          Files.readAllLines(fixations, UTF_8).asScala.take(40).mkString("\n"),
          UTF_8
        )
        val app      = mutable.ArrayBuffer.empty[Intent]
        val platform = ImportWizardHost.fxPlatform(
          () => fx.stage,
          FilePresetStore(dir.resolve("presets")),
          Some(port)
        )
        val host = runOnFx(
          ImportWizardHost(
            ImportWizard.newImport(t2, ImportPresets.empty),
            () => t2,
            app += _,
            platform,
            () => ()
          )
        )
        runOnFx(host.read(SourceRole.Fixations, second))
          .get(30, java.util.concurrent.TimeUnit.SECONDS)
        runOnFx(host.dispatch(WizardIntent.DeclareTime(Some(TimeUnit.Milliseconds))))
        runOnFx(host.dispatch(WizardIntent.Commit))
        val deadline = System.nanoTime + 30_000_000_000L
        while runOnFx(app.isEmpty) && System.nanoTime < deadline do Thread.sleep(20)
        val command = runOnFx(app.toVector.collectFirst { case Intent.Dispatch(c) => c })
          .getOrElse(fail("the command never reached the app"))
        port.journal(JournalEntry.Apply(command))
        val saved = scala.concurrent.Promise[Either[String, Unit]]()
        port.save(r => saved.success(r.map(_ => ())))
        assertEquals(
          scala.concurrent.Await
            .result(saved.future, scala.concurrent.duration.Duration(30, "s")),
          Right(())
        )
        assertEquals(runOnFx(host.model.problem), None)
        assertEquals(session.saved.unsafeRunSync().datasets.last.id, DatasetRevision(4))
        port.close()
        session.close.unsafeRunSync(): Unit
      finally TempDirs.remove(dir)
  }

  fxStage.test("a file the project cannot store applies nothing and keeps the wizard open") {
    fx =>
      val m = mount(
        fx,
        ImportWizard.newImport(t2, ImportPresets.empty),
        t2,
        platform = Recorder(refuseInputs = Some("fixations.csv: disk full"))
      )
      readIn(fx, m, SourceRole.Fixations, fixations)
      declareMs(fx, m)
      fx.robot.click(m.view.commit)
      assertEquals(dispatched(m), Vector.empty)
      assert(!m.closed())
      assertEquals(drawn(m.view.problem), "Not saved: fixations.csv: disk full")
  }

  fxStage.test("a preset the store cannot write says so") { fx =>
    val m = mount(
      fx,
      ImportWizard.newImport(t2, ImportPresets.empty),
      t2,
      platform = Recorder(refusePresets = Some("presets: read-only"))
    )
    readIn(fx, m, SourceRole.Fixations, fixations)
    runOnFx(m.view.presetName.setText("Lab"))
    fx.awaitLayout()
    fx.robot.click(m.view.presetSave)
    assertEquals(drawn(m.view.problem), "Not saved: presets: read-only")
    assertEquals(drawn(m.view.status), "")
  }
