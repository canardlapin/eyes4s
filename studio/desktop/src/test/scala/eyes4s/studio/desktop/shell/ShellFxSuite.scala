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

package eyes4s.studio.desktop.shell

import cats.effect.unsafe.implicits.global
import eyes4s.studio.app.{AppModel, ClockTime, Intent, PlatformDialog}
import eyes4s.studio.app.text.Format
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.app.vm.{Shell, ShellText}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.explore.NavigatorDisplays
import eyes4s.studio.desktop.trial.StimulusSource
import eyes4s.studio.desktop.harness.{FxStage, Modifiers, StudioFxSuite}
import eyes4s.studio.desktop.platform.FilePresetStore
import eyes4s.studio.desktop.runtime.{DesktopEffects, PlatformDialogs, ProjectPort}
import javafx.scene.control.{Button, Label, Labeled}
import javafx.scene.text.Text

import scala.jdk.CollectionConverters.*

/** The shared harness of the shell FX suites (S1.4–S1.11): boot a studio
  * window on a story model over the fake backend, dispatch, wait, and read
  * the shell back as the S1.0 text rendering (`ShellText`).
  */
abstract class ShellFxSuite extends StudioFxSuite:

  // --- Opening windows -------------------------------------------------------

  override def beforeAll(): Unit =
    super.beforeAll()
    eyes4s.studio.desktop.typography.StudioFonts.loadAll().foreach(p => fail(p.message))

  protected val opened = scala.collection.mutable.ArrayBuffer.empty[StudioWindow]

  private val presetDirs = scala.collection.mutable.ArrayBuffer.empty[java.nio.file.Path]

  /** An empty preset store of this test's own, removed after it. */
  protected def noPresets(): FilePresetStore =
    val dir = java.nio.file.Files.createTempDirectory("eyes4s-shell-presets")
    presetDirs += dir
    FilePresetStore(dir)

  override def afterEach(context: AfterEach): Unit =
    opened.foreach(w => runOnFx(w.close()))
    opened.clear()
    presetDirs.foreach(eyes4s.studio.desktop.platform.TempDirs.remove)
    presetDirs.clear()
    super.afterEach(context)

  /** Records every dialog asked for; types `rename` into Rename…. */
  protected final class Dialogs(rename: Option[String]) extends PlatformDialogs:
    val asked = scala.collection.mutable.ArrayBuffer.empty[PlatformDialog]
    def open(dialog: PlatformDialog, dispatch: Intent => Unit): Unit =
      asked += dialog
      if dialog == PlatformDialog.RenameProject then
        rename.foreach(t => dispatch(StudioWindow.renameAnswer(t)))

  protected def boot(
      fx: FxStage,
      model: AppModel,
      moment: StoryMoment = StoryMoment.T2,
      dialogs: PlatformDialogs = Dialogs(None),
      theme: Theme = Theme.Light,
      project: Option[ProjectPort] = None,
      clock: () => Option[ClockTime] = DesktopEffects.wallClock,
      // Synthetic key events never reach a native menu, so the shell suites
      // exercise the window's own key path, as it runs on Linux; KeymapFxSuite
      // checks the native split separately.
      nativeMenu: Boolean = false,
      // No saved import presets unless a suite brings its own.
      presets: FilePresetStore = noPresets(),
      // The story sessions' display kinds (fixtures/studio-golden).
      displays: NavigatorDisplays = NavigatorDisplays.golden,
      // The golden fixture's stimuli.
      stimuli: StimulusSource =
        StimulusSource.directory(eyes4s.studio.desktop.trial.GoldenTrials.stimuli),
      // Explore's source records: the window's backend's unless a suite
      // brings its own.
      records: Option[eyes4s.studio.app.explore.SourceRecordsSource] = None,
      // Compare's trial panels: none unless a suite brings its own.
      panels: eyes4s.studio.desktop.compare.PanelSources =
        eyes4s.studio.desktop.compare.PanelSources.notServed
  ): StudioWindow =
    val w = runOnFx(
      StudioWindow
        .open(
          model,
          moment,
          displays,
          stimuli,
          theme,
          dialogs = Some(dialogs),
          project = project,
          clock = clock,
          nativeMenu = nativeMenu,
          presets = presets,
          records = records,
          panels = panels
        )
        .fold(e => fail(e.message), identity)
    )
    opened += w
    fx.show(w.root)
    runOnFx(w.bind(fx.stage))
    w

  protected def dispatch(fx: FxStage, w: StudioWindow, intent: Intent): Unit =
    runOnFx(w.runtime.dispatch(intent))
    fx.awaitLayout()

  /** Wait (up to 20 s) until `cond` holds on the FX thread. */
  protected def eventually(fx: FxStage, what: String)(cond: => Boolean): Unit =
    val deadline = System.nanoTime + 20_000_000_000L
    while !runOnFx(cond) do
      if System.nanoTime > deadline then fail(s"timed out waiting for $what")
      Thread.sleep(25)
      fx.awaitLayout()

  protected val isMac = sys.props.get("os.name").exists(_.toLowerCase.contains("mac"))

  /** ⌘ as the robot presses it: Command on macOS, Control elsewhere. */
  protected val shortcut = if isMac then Modifiers(meta = true) else Modifiers(control = true)

  // --- Reading the shell back as the S1.0 text rendering ------------------------

  protected def action(b: Button, label: String): String =
    if b.isDisabled then s"[$label]" else label

  /** The text a labeled control actually draws: after mnemonic parsing and
    * any truncation, so "ret_07" drawn as "ret07" or "Summary…" fails.
    */
  protected def drawn(l: Labeled): String =
    l.getChildrenUnmodifiable.asScala
      .collectFirst { case t: Text => t.getText }
      .getOrElse(l.getText)

  protected def texts(w: StudioWindow): Vector[String] = runOnFx {
    val bar          = w.shell.appBar
    val jobs         = bar.jobs
    val perspectives = Perspective.values.toVector.map { p =>
      val t        = bar.switcher(p)
      val shortcut = t.getGraphic.asInstanceOf[Label].getText
      s"${drawn(t)} $shortcut${if t.isSelected then " (current)" else ""}"
    }
    val app = Vector(
      s"app.name: ${drawn(bar.wordmark)}",
      s"app.project: ${drawn(bar.project)}",
      s"app.perspectives: ${perspectives.mkString(" | ")}",
      s"app.jobs: ${(Vector(jobs.drawnParts.mkString(" · ")) ++ Option
          .when(jobs.cancel.isVisible)(action(jobs.cancel, jobs.cancel.getText))).mkString(" | ")}"
    ) ++ Option.when(jobs.progress.isVisible)(
      s"app.jobs.progress: ${Format.percent(jobs.progress.getProgress)}"
    )
    val strip = w.shell.contextStrip.node.getChildren.asScala.toVector
    val nav   = strip.take(2).collect { case b: Button => action(b, b.getAccessibleText) }
    val trail = w.shell.contextStrip.crumbs.map { b =>
      if b.getPseudoClassStates.contains(Fx.Current) then s"*${drawn(b)}*" else drawn(b)
    }
    val chips = strip.drop(4)
    val fresh = chips.collect {
      case l: Label if l.getStyleClass.contains("freshness") => drawn(l)
    }
    val notes = chips.collect {
      case l: Label if l.getStyleClass.contains("context-note") => drawn(l)
    }
    val draft   = chips.collect { case b: Button => drawn(b) }
    val context = Vector(
      s"context.nav: ${nav.mkString(" | ")}",
      s"context.trail: ${trail.mkString(" › ")}",
      s"context.freshness: ${fresh.head}"
    ) ++ fresh.drop(1).map(n => s"context.newer: $n") ++ notes.map(n => s"context.note: $n") ++
      draft.map(d => s"context.draft: $d")
    val bannerNode = w.shell.banner.node
    val banner     =
      if !bannerNode.isVisible then Vector.empty
      else
        val kids    = bannerNode.getChildren.asScala.toVector
        val labels  = kids.collect { case l: Label => l }
        val buttons = kids.collect { case b: Button => action(b, drawn(b)) }
        Vector(s"banner.lead: ${drawn(labels.head)}") ++
          labels.drop(1).map(l => s"banner.detail: ${drawn(l)}") ++
          Option.when(buttons.nonEmpty)(s"banner.actions: ${buttons.mkString(" | ")}")
    val status                               = w.shell.statusBar
    def words(box: javafx.scene.layout.HBox) =
      box.getChildren.asScala.toVector
        .filter(_.isManaged)
        .collect {
          case l: Label  => drawn(l)
          case b: Button => action(b, drawn(b))
        }
        .mkString(" ")
    val line =
      s"status: ${words(status.selected)} | ${drawn(status.hint)} | ${words(status.job)} | ${drawn(status.saved)}"
    val notice =
      Option.when(w.shell.notice.node.isVisible)(s"notice: ${drawn(w.shell.notice.text)}")
    val confirm = Option.when(w.shell.confirmation.node.isVisible) {
      val buttons = w.shell.confirmation.node.getChildren.asScala.toVector.collect {
        case b: Button => action(b, drawn(b))
      }
      // Drawn as Keep, then Discard (the destructive button last).
      s"confirm: ${drawn(w.shell.confirmation.text)} | ${buttons.reverse.mkString(" | ")}"
    }
    app ++ context ++ banner ++ Vector(line) ++ notice ++ confirm
  }

  /** The S1.0 text rendering of the same model, minus the window line. */
  protected def expected(w: StudioWindow): Vector[String] = runOnFx {
    ShellText.render(Shell.project(w.runtime.model)).split("\n").toVector.drop(1)
  }

  protected def at(m: AppModel, i: Intent): AppModel = AppModel.update(m, i)._1
