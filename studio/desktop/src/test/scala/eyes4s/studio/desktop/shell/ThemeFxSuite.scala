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

import eyes4s.studio.app.appearance.Appearance
import eyes4s.studio.app.tokens.{ThemedToken, Tokens, Wcag, WcagError}
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.document.{Perspective, Theme}
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.harness.StudioTheme
import eyes4s.studio.desktop.{StudioWindow, ThemeHost}
import javafx.scene.Node
import javafx.scene.control.{Labeled, Menu, RadioMenuItem, TextInputControl}
import javafx.scene.layout.Region
import javafx.scene.paint.Color
import javafx.scene.text.Text
import scaladock.fx.DockTheme

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*

/** Theme switching (ticket S1.10; board MainDark.dc.html): View › Appearance
  * › Dark restyles the window, its dock with the theme's generated
  * DockTheme.Custom, and a popped-out dock window; System follows the
  * platform. Every perspective is snapshotted dark, and a switch leaves the
  * hash of every value the window shows unchanged.
  */
class ThemeFxSuite extends ShellFxSuite:

  private val moments: Vector[(Perspective, () => AppModel)] = Vector(
    Perspective.Data     -> (() => StoryModels.t1Data),
    Perspective.Explore  -> (() => StoryModels.t2Explore),
    Perspective.Analysis -> (() => StoryModels.t2Analysis),
    Perspective.Compare  -> (() => StoryModels.t2Compare),
    Perspective.Figures  -> (() => StoryModels.t2Figures)
  )

  /** The View menu's Appearance choice `a`, as drawn. */
  private def choice(w: StudioWindow, a: Appearance): RadioMenuItem = runOnFx {
    val view = w.shell.menus.find(_.getText == "View").getOrElse(fail("no View menu"))
    view.getItems.asScala
      .collectFirst { case m: Menu if m.getText == "Appearance" => m }
      .getOrElse(fail("no Appearance submenu"))
      .getItems
      .asScala
      .collectFirst {
        case r: RadioMenuItem if r.getId == s"view.appearance-${a.toString.toLowerCase}" => r
      }
      .getOrElse(fail(s"no $a item"))
  }

  private def choose(
      fx: eyes4s.studio.desktop.harness.FxStage,
      w: StudioWindow,
      a: Appearance
  ) =
    runOnFx(choice(w, a).fire())
    fx.awaitLayout()

  /** Every text the window's scene shows or announces, in scene order. */
  private def values(root: Node): Vector[String] =
    def walk(n: Node): Vector[String] =
      val own = n match
        case l: Labeled          => Vector(l.getText)
        case t: TextInputControl => Vector(t.getText)
        case t: Text             => Vector(t.getText)
        case _                   => Vector.empty
      val said     = Option(n.getAccessibleText).toVector
      val children = n match
        case p: javafx.scene.Parent => p.getChildrenUnmodifiable.asScala.toVector.flatMap(walk)
        case _                      => Vector.empty
      (own ++ said).filter(s => s != null && s.nonEmpty) ++ children
    walk(root)

  // The window's values once its asynchronous reads have answered: the same
  // on two reads half a second apart.
  private def settled(fx: eyes4s.studio.desktop.harness.FxStage, w: StudioWindow) =
    var last     = runOnFx(values(w.root))
    var stable   = false
    val deadline = System.nanoTime + 20_000_000_000L
    while !stable do
      if System.nanoTime > deadline then fail("the window's values did not settle")
      Thread.sleep(500)
      fx.awaitLayout()
      val now = runOnFx(values(w.root))
      stable = now == last
      last = now
    last

  private def hash(texts: Vector[String]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(texts.mkString("\u0000").getBytes(UTF_8))
      .map(b => f"$b%02x")
      .mkString

  /** A looked-up colour as the window's sheets resolve it now. */
  private def resolved(w: StudioWindow, token: ThemedToken): String = runOnFx {
    val probe = Region()
    probe.setStyle(s"-fx-background-color: -es-${token.cssName};")
    probe.setManaged(false)
    w.shell.root.getChildren.add(probe)
    probe.applyCss()
    val c = probe.getBackground.getFills.get(0).getFill.asInstanceOf[Color]
    w.shell.root.getChildren.remove(probe)
    f"#${(c.getRed * 255).round}%02X${(c.getGreen * 255).round}%02X${(c.getBlue * 255).round}%02X"
  }

  private def dark(t: ThemedToken) = Tokens.themed(eyes4s.studio.app.tokens.Theme.Dark, t)

  for (p, model) <- moments do
    fxStage.test(s"${p.label}: Dark restyles the window and its dock; no shown value changes") {
      fx =>
        val w =
          boot(fx, model(), if p == Perspective.Data then StoryMoment.T1 else StoryMoment.T2)
        assertEquals(runOnFx(w.runtime.model.perspective), p)
        fx.awaitLayout()
        val before = settled(fx, w)
        assertEquals(runOnFx(choice(w, Appearance.Light).isSelected), true)
        choose(fx, w, Appearance.Dark)
        assertEquals(runOnFx(w.runtime.model.document.presentation.theme), Theme.Dark)
        assertEquals(runOnFx(w.themes.theme), Some(Theme.Dark))
        assert(runOnFx(w.root.getStylesheets.asScala.exists(_.endsWith("/studio-dark.css"))))
        assertEquals(
          runOnFx(w.host.dock.theme),
          ThemeHost.dockTheme(Theme.Dark).fold(e => fail(e.message), identity)
        )
        assertEquals(runOnFx(choice(w, Appearance.Dark).isSelected), true)
        // The window's sheets resolve to the dark tokens.
        assertEquals(resolved(w, ThemedToken.Surface), dark(ThemedToken.Surface).hexRgb)
        assertEquals(resolved(w, ThemedToken.Query), dark(ThemedToken.Query).hexRgb)
        val after = settled(fx, w)
        assertEquals(hash(after), hash(before), after.diff(before))
        fx.snapshot(StudioTheme.Dark)
        // And back: the light sheets, the same values.
        choose(fx, w, Appearance.Light)
        assert(runOnFx(w.root.getStylesheets.asScala.exists(_.endsWith("/studio.css"))))
        assertEquals(hash(settled(fx, w)), hash(before))
    }

  fxStage.test(
    "MainDark: the dark query and matched roles differ by 20 L*; role pills reach 4.5:1"
  ) { fx =>
    val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
    choose(fx, w, Appearance.Dark)
    def ok(e: Either[WcagError, Double]) = e.fold(err => fail(err.message), identity)
    // The window shows exactly these tokens…
    Vector(
      ThemedToken.Query,
      ThemedToken.Match,
      ThemedToken.QuerySoft,
      ThemedToken.MatchSoft,
      ThemedToken.MatchText,
      ThemedToken.ControlSoft,
      ThemedToken.ControlText
    ).foreach(t => assertEquals(resolved(w, t), dark(t).hexRgb, t))
    // …and they meet the board's two dark items.
    val spread =
      ok(Wcag.lightness(dark(ThemedToken.Query))) - ok(Wcag.lightness(dark(ThemedToken.Match)))
    assert(math.abs(spread) >= 20.0, spread)
    Vector(
      ThemedToken.Query       -> ThemedToken.QuerySoft,
      ThemedToken.MatchText   -> ThemedToken.MatchSoft,
      ThemedToken.ControlText -> ThemedToken.ControlSoft
    ).foreach { (fg, bg) =>
      val c = ok(Wcag.contrast(dark(fg), dark(bg)))
      assert(c >= 4.5, s"$fg on $bg: $c")
    }
  }

  fxStage.test("a popped-out dock window follows the theme, with the studio's tokens") { fx =>
    val w     = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
    val stage = runOnFx(fx.scene.getWindow)
    val pane  = runOnFx(w.host.dock.state.groups.find(_.tabs.nonEmpty).get)
    runOnFx(w.host.dock.popOut(pane.id))
    fx.awaitLayout()
    val popout = runOnFx(
      javafx.stage.Window.getWindows.asScala.find(win => !(win eq stage) && win.isShowing)
    ).getOrElse(fail("no popped-out window"))
    choose(fx, w, Appearance.Dark)
    val sheets = runOnFx {
      def all(n: javafx.scene.Parent): Vector[String] =
        n.getStylesheets.asScala.toVector ++ n.getChildrenUnmodifiable.asScala.toVector
          .collect { case p: javafx.scene.Parent =>
            p
          }
          .flatMap(all)
      all(popout.getScene.getRoot)
    }
    assert(sheets.exists(_.endsWith("/studio-dock-dark.css")), sheets)
    assert(!sheets.exists(_.endsWith("/studio-dock-light.css")), sheets)
    // The popped-out window resolves the studio's dark tokens.
    val surface = runOnFx {
      val probe = Region()
      probe.setStyle("-fx-background-color: -es-surface;")
      // A pane inside the popped-out window, its stylesheets the window's.
      def firstPane(n: javafx.scene.Node): Option[javafx.scene.layout.Pane] = n match
        case p: javafx.scene.layout.Pane => Some(p)
        case p: javafx.scene.Parent      =>
          p.getChildrenUnmodifiable.asScala.iterator.map(firstPane).collectFirst {
            case Some(x) =>
              x
          }
        case _ => None
      val region = firstPane(popout.getScene.getRoot).getOrElse(fail("no pane in the popout"))
      region.getChildren.add(probe)
      probe.applyCss()
      val c = probe.getBackground.getFills.get(0).getFill.asInstanceOf[Color]
      region.getChildren.remove(probe)
      f"#${(c.getRed * 255).round}%02X${(c.getGreen * 255).round}%02X${(c.getBlue * 255).round}%02X"
    }
    assertEquals(surface, dark(ThemedToken.Surface).hexRgb)
    runOnFx(w.host.dock.dockAllBack())
  }

  fxStage.test("System follows the platform's reported theme; a fixed choice stops it") { fx =>
    val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
    choose(fx, w, Appearance.System)
    assertEquals(runOnFx(choice(w, Appearance.System).isSelected), true)
    dispatch(fx, w, Intent.SystemTheme(Theme.Dark))
    assertEquals(runOnFx(w.themes.theme), Some(Theme.Dark))
    dispatch(fx, w, Intent.SystemTheme(Theme.Light))
    assertEquals(runOnFx(w.themes.theme), Some(Theme.Light))
    choose(fx, w, Appearance.Dark)
    dispatch(fx, w, Intent.SystemTheme(Theme.Light))
    assertEquals(runOnFx(w.themes.theme), Some(Theme.Dark))
  }

  fxStage.test("a registered window (the import wizard's) follows a switch until it closes") {
    fx =>
      val w     = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
      val stage = runOnFx {
        val st = javafx.stage.Stage()
        st.setScene(javafx.scene.Scene(javafx.scene.layout.StackPane(), 200, 100))
        st.show()
        st
      }
      val scene = runOnFx(stage.getScene)
      runOnFx(w.themes.register(scene))
      assertEquals(runOnFx(w.themes.registered), 1)
      assert(runOnFx(scene.getStylesheets.asScala.exists(_.endsWith("/studio.css"))))
      choose(fx, w, Appearance.Dark)
      assert(runOnFx(scene.getStylesheets.asScala.exists(_.endsWith("/studio-dark.css"))))
      // Closed, it is forgotten: a later switch no longer restyles it.
      runOnFx(stage.hide())
      fx.awaitLayout()
      assertEquals(runOnFx(w.themes.registered), 0)
      choose(fx, w, Appearance.Light)
      assert(runOnFx(scene.getStylesheets.asScala.exists(_.endsWith("/studio-dark.css"))))
  }
