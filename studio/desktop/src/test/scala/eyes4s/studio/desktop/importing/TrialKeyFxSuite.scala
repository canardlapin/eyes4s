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
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.app.{Intent, StoryModels}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.importing.{ImportPreset, ImportPresets, KeyPart}
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.harness.{FxStage, StageSize, StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.platform.TempDirs
import eyes4s.studio.desktop.trial.GoldenTrials
import eyes4s.studio.desktop.typography.StudioFonts
import javafx.scene.Node
import javafx.scene.control.Labeled
import javafx.scene.layout.StackPane
import javafx.scene.text.Text

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** The trial key builder's JavaFX view (ticket S5.3; Data.dc.html, key
  * builder) inside the import wizard, on fixtures/studio-golden and on a
  * copy of it whose trial labels repeat across two blocks. Every action goes
  * through the controls' own `fire`, so no test depends on OS window focus.
  */
class TrialKeyFxSuite extends StudioFxSuite:
  import StoryModels.t2

  // The column-mapping group of the Data board is about this size.
  override protected def stageSize: StageSize = StageSize(960, 640)

  override def beforeAll(): Unit =
    super.beforeAll()
    runOnFx(StudioFonts.loadAll()).foreach(p => fail(p.message))

  val fixations: Path = GoldenTrials.golden.resolve("fixations.csv")
  val trials: Path    = GoldenTrials.golden.resolve("trials.csv")

  /** A platform with no dialogs; files are read by the test. */
  object Quiet extends ImportPlatform:
    def chooseFile(role: SourceRole): Option[Path]                                = None
    def storePreset(p: ImportPreset): Either[String, Unit]                        = Right(())
    def importInput(s: Source, p: Path, done: Either[String, Unit] => Unit): Unit =
      done(Right(()))

  final case class Mounted(host: ImportWizardHost, app: mutable.ArrayBuffer[Intent]):
    def view: ImportWizardView = host.view
    def key: TrialKeyView      = host.view.key

  def mount(fx: FxStage, theme: Theme = Theme.Light): Mounted =
    val app  = mutable.ArrayBuffer.empty[Intent]
    val host = runOnFx(
      ImportWizardHost(
        ImportWizard.newImport(t2, ImportPresets.empty),
        () => t2,
        app += _,
        Quiet,
        () => ()
      )
    )
    val sheets = StudioStyles.stylesheets(theme).fold(e => fail(e.message), identity)
    runOnFx {
      fx.scene.getStylesheets.setAll(sheets*)
      val root = StackPane(host.view.node)
      root.getStyleClass.add("es")
      fx.scene.setRoot(root)
    }
    fx.awaitLayout()
    Mounted(host, app)

  def readIn(fx: FxStage, m: Mounted, role: SourceRole, path: Path): Unit =
    runOnFx(m.host.read(role, path)).get(30, java.util.concurrent.TimeUnit.SECONDS)
    fx.awaitLayout()

  def drawn(l: Labeled): String = runOnFx {
    l.getChildrenUnmodifiable.asScala
      .collectFirst { case t: Text => t.getText }
      .getOrElse(l.getText)
  }

  def shown(n: Node): Boolean = runOnFx(n.isVisible && n.isManaged)

  /** Golden with the board's occurrence case: for P01–P19, enc_19 and enc_20
    * become a second block's enc_01 and enc_02 (occurrence 2), so 38 keys
    * name two presentations each. Both files, in a fresh directory.
    */
  def blocksCopy(dir: Path): (Path, Path) =
    def relabel(line: String): String =
      val cells = line.split(",", -1)
      val p     = cells(0)
      if cells.length > 3 && p >= "P01" && p <= "P19" && cells(1) == "Encoding" then
        cells(2) match
          case "enc_19" => cells(2) = "enc_01"; cells(3) = "2"
          case "enc_20" => cells(2) = "enc_02"; cells(3) = "2"
          case _        => ()
      cells.mkString(",")
    def copy(from: Path): Path =
      val lines = Files.readAllLines(from, UTF_8).asScala.toVector
      val to    = dir.resolve(from.getFileName)
      Files.writeString(
        to,
        (lines.head +: lines.tail.map(relabel)).mkString("", "\n", "\n"),
        UTF_8
      )
      to
    (copy(fixations), copy(trials))

  fxStage.test("golden: Participant + Phase + Trial + Occurrence = 960 unique keys, drawn") {
    fx =>
      assumeFullStage(fx)
      val m = mount(fx)
      readIn(fx, m, SourceRole.Fixations, fixations)
      readIn(fx, m, SourceRole.Trials, trials)
      val k = m.key
      assertEquals(drawn(k.title), "Trial identity")
      assertEquals(
        drawn(k.rule),
        "Every record and every inventory entry must resolve to exactly one key."
      )
      assertEquals(
        KeyPart.values.toVector.map(p => drawn(k.blocks(p))),
        Vector("Participant", "Phase", "Trial", "Occurrence")
      )
      assertEquals(drawn(k.count), "960 unique keys")
      assertEquals(drawn(k.detail), "· 0 duplicates · Occurrence is 1 for every trial")
      val lines = runOnFx(k.lines.getChildren.asScala.toVector.collect { case l: Labeled => l })
      assertEquals(
        lines.map(drawn),
        Vector(
          "fixations.csv: 954 unique keys · 0 duplicates · Occurrence is 1 for every trial"
        )
      )
      assert(!shown(k.check))
      assert(!shown(k.repeatedScroll))
      assert(runOnFx(k.occurrence.isSelected))
      // Board .keyblk: 26 px blocks, the whole key row inside the stage.
      KeyPart.values.foreach(p =>
        assertEqualsDouble(runOnFx(k.blocks(p).getLayoutBounds.getHeight), 26, 0.5)
      )
      val bounds = runOnFx(k.row.localToScene(k.row.getLayoutBounds))
      assert(
        bounds.getMaxY <= 640 && bounds.getMaxX <= 960 && bounds.getMinY > 0,
        bounds.toString
      )
      assertEquals(
        runOnFx(k.occurrence.getAccessibleText),
        "Occurrence: column occurrence"
      )
      fx.snapshot(StudioTheme.Light)
  }

  fxStage.test(
    "leaving Occurrence out: 38 keys repeat, every trial listed, the commit blocked"
  ) { fx =>
    assumeFullStage(fx)
    val dir = Files.createTempDirectory("s53-key")
    try
      val (fix, inv) = blocksCopy(dir)
      val m          = mount(fx)
      readIn(fx, m, SourceRole.Fixations, fix)
      readIn(fx, m, SourceRole.Trials, inv)
      runOnFx {
        val t = m.view.time
        t.getItems.asScala.find(_.unit.contains(TimeUnit.Milliseconds)).foreach(t.setValue)
      }
      fx.awaitLayout()
      val k = m.key
      // With Occurrence: eyes4s identifies a trial by its label; conflicts warn.
      assertEquals(drawn(k.count), "922 keys")
      assertEquals(
        drawn(k.detail),
        "· 38 keys repeat · Occurrence 1–2 · 38 later presentations"
      )
      assert(!shown(k.check))
      // Leave Occurrence out: the toggle dispatches through the wizard.
      runOnFx(k.occurrence.fire())
      fx.awaitLayout()
      assert(!runOnFx(k.occurrence.isSelected))
      val lines = runOnFx(k.lines.getChildren.asScala.toVector.collect { case l: Labeled => l })
      assertEquals(
        lines.map(drawn),
        Vector(
          "fixations.csv: 916 keys · 38 keys repeat without Occurrence · Occurrence left out: " +
            "1 for every trial"
        )
      )
      assert(runOnFx(lines.head.getStyleClass.contains("blocking")))
      assert(shown(k.check))
      assert(drawn(k.check).startsWith("Studio check · fixations.csv: 38 keys repeat"))
      // Every repeated key of both files, each with both trials.
      val entries = runOnFx(k.repeated.getChildren.asScala.toVector)
      assertEquals(entries.size, 76)
      val fixationEntry = runOnFx(
        entries
          .map(_.getAccessibleText)
          .find(_.startsWith("Key P01 · Encoding · enc_01 resolves to 2 trials"))
      )
      assert(fixationEntry.isDefined, entries.size.toString)
      assert(shown(k.repeatedScroll))
      // The commit is refused; nothing reaches the app.
      runOnFx(m.view.commit.fire())
      fx.awaitLayout()
      assertEquals(m.app.toVector, Vector.empty)
      assert(drawn(m.view.problem).startsWith("Studio check · fixations.csv"))
      assert(runOnFx(m.view.fixations.page.isVisible))
      // Putting Occurrence back lifts the check.
      runOnFx(k.occurrence.fire())
      fx.awaitLayout()
      assert(runOnFx(k.occurrence.isSelected))
      assert(!shown(k.check))
      fx.snapshot(StudioTheme.Light)
    finally TempDirs.remove(dir)
  }

  fxStage.test("a key on a many-valued column: the host checks it in one streaming pass") {
    fx =>
      val dir = Files.createTempDirectory("s53-stream")
      try
        // 5,000 distinct trial labels: past the sniffer's cap, so not kept.
        val rows = (1 to 5000).map(i => s"P01,Encoding,t$i,1,1,960,540,300,200,100") :+
          "P01,Encoding,t7,2,2,960,540,600,200,100"
        val file = dir.resolve("fixations.csv")
        Files.writeString(
          file,
          ("participant,phase,trial,block,ordinal,x,y,onset_ms,duration_ms,sample_count" +: rows)
            .mkString("", "\n", "\n"),
          UTF_8
        )
        val m = mount(fx)
        readIn(fx, m, SourceRole.Fixations, file)
        runOnFx(m.host.keyChecked).get(30, java.util.concurrent.TimeUnit.SECONDS)
        fx.awaitLayout()
        assertEquals(drawn(m.key.count), "5,000 keys")
        assertEquals(
          drawn(m.key.detail),
          "· 1 key names more than one occurrence · Occurrence 1–2 · 1 later presentations"
        )
        // Leaving Occurrence out runs a second pass; the Studio check follows it.
        // The pass runs off the FX thread: until it answers, the line says so.
        val during = runOnFx {
          m.key.occurrence.fire()
          m.key.detail.getText
        }
        assertEquals(during, "checking every record…")
        runOnFx(m.host.keyChecked).get(30, java.util.concurrent.TimeUnit.SECONDS)
        fx.awaitLayout()
        assertEquals(
          drawn(m.key.detail),
          "· 1 key repeats without Occurrence · Occurrence left out: 1 for every trial"
        )
        assert(drawn(m.key.check).startsWith("Studio check · fixations.csv: 1 key repeats"))
      finally TempDirs.remove(dir)
  }

  fxStage.test("dark theme: the key builder renders with the dark tokens") { fx =>
    val m = mount(fx, Theme.Dark)
    readIn(fx, m, SourceRole.Fixations, fixations)
    assertEquals(drawn(m.key.count), "954 unique keys")
    assert(!shown(m.key.lines))
    fx.snapshot(StudioTheme.Dark)
  }
