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

import eyes4s.studio.app.tokens.{Theme, ThemedToken, Tokens}
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.harness.{FxStage, SnapshotScale, StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.typography.StudioFonts
import javafx.css.PseudoClass
import javafx.scene.{AccessibleAttribute, AccessibleRole}
import javafx.scene.control.{Button, Label, ToggleButton}
import javafx.scene.input.KeyCode
import javafx.scene.layout.{HBox, Region, VBox}
import javafx.scene.paint.Color

import scala.jdk.CollectionConverters.*

/** The segmented bar (REVIEW ticket 16, slice b; `SegmentedBar`, after the
  * System board's `.sg` specimen in `studio-controls.css`). It is one Tab
  * stop, the chosen segment; the arrow keys, Home and End move the focus and
  * the choice together, wrapping, and Space and Enter choose the focused
  * segment. Each segment reads as a radio button named by the caller. The
  * track is surface-3; a segment is a 20 px label in ink-2 at weight 500,
  * the chosen one the selection fill in a hairline edge in ink at weight 600.
  * Keyboard focus is the accent ring of the focused segment alone; a click
  * draws none. A disabled bar fades to 0.4 unless a reason is drawn beside
  * it, which is then its segments' accessible help.
  *
  * Keys go through [[eyes4s.studio.desktop.harness.StudioRobot]]; `:hover`
  * is set explicitly (a headless run moves no pointer). Each drawing check
  * reads the rendered fills, strokes and fonts, in both themes.
  */
class SegmentedBarFxSuite extends StudioFxSuite:

  override def beforeAll(): Unit =
    super.beforeAll()
    val problems = runOnFx(StudioFonts.loadAll())
    assert(problems.isEmpty, problems.map(_.message).mkString("\n"))

  private val Hover = PseudoClass.getPseudoClass("hover")

  private val SansSemibold = "IBM Plex Sans SmBld"
  private val SansMedium   = "IBM Plex Sans Medm"

  /** The fade of a control with no workflow meaning (DESIGN_SPEC 15). */
  private val FadedOpacity = 0.4

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  /** The token colours of one theme. */
  private final class Palette(theme: Theme):
    private def c(t: ThemedToken): Color =
      val v = Tokens.themed(theme, t)
      Color.rgb(v.red, v.green, v.blue, v.alphaPercent / 100.0)
    val accent: Color   = c(ThemedToken.Accent)
    val ink: Color      = c(ThemedToken.Ink)
    val ink2: Color     = c(ThemedToken.Ink2)
    val surface2: Color = c(ThemedToken.Surface2)
    val surface3: Color = c(ThemedToken.Surface3)
    val hairline: Color = c(ThemedToken.Hairline)
    val selFill: Color  = c(ThemedToken.SelFill)

  private val themes = Vector(
    (Theme.Light, StudioTheme.Light, Palette(Theme.Light)),
    (Theme.Dark, StudioTheme.Dark, Palette(Theme.Dark))
  )

  private def fills(r: Region): List[Any] =
    Option(r.getBackground).toList.flatMap(_.getFills.asScala.toList.map(_.getFill))

  private def strokes(r: Region): List[Any] =
    Option(r.getBorder).toList.flatMap(_.getStrokes.asScala.toList.map(_.getTopStroke))

  private enum Sigma derives CanEqual:
    case One, Two, Four

  private val labels = Map(Sigma.One -> "1°", Sigma.Two -> "2°", Sigma.Four -> "4°")

  private def segments(chosen: Option[Sigma]): Vector[SegmentedBar.Segment[Sigma]] =
    Sigma.values.toVector.map(s =>
      SegmentedBar.Segment(s, labels(s), s"Sigma: ${labels(s)}", chosen.contains(s))
    )

  /** A bar between two buttons, under `theme`'s studio sheets, recording
    * every choice it reports.
    */
  private final class Specimen(theme: Theme, chosen: Option[Sigma]):
    val reported       = Vector.newBuilder[Sigma]
    val before: Button = Button("before")
    val after: Button  = Button("after")
    val bar            = SegmentedBar[Sigma](s => reported += s)
    bar.show(segments(chosen))
    val faded = SegmentedBar[Sigma](_ => ())
    faded.show(segments(Some(Sigma.Two)))
    faded.node.setDisable(true)
    val held = SegmentedBar[Sigma](_ => ())
    held.show(segments(Some(Sigma.Two)))
    val why: Label = Label("Waiting for preflight")
    held.attachReason(why)
    held.node.setDisable(true)
    val root: VBox = VBox(
      6.0,
      HBox(6.0, before, bar.node, after),
      HBox(6.0, faded.node, held.node, why)
    )
    root.setStyle("-fx-padding: 8;")
    root.getStyleClass.add("studio-shell")
    root.getStylesheets.setAll(right(StudioStyles.stylesheets(theme))*)
    def seg(s: Sigma): ToggleButton = bar.segmentNodes(s.ordinal)
    def all: Vector[ToggleButton]   = bar.segmentNodes

  private def shown(fx: FxStage, theme: Theme, chosen: Option[Sigma] = Some(Sigma.Two)) =
    val s = runOnFx(Specimen(theme, chosen))
    fx.show(s.root)
    fx.awaitLayout()
    runOnFx(s.root.applyCss())
    s

  private def owner(fx: FxStage) = runOnFx(fx.scene.getFocusOwner)

  fxStage.test("at rest: a surface-3 track; the chosen segment the selection fill at 600") {
    fx =>
      themes.foreach { (theme, studioTheme, p) =>
        val s = shown(fx, theme)
        runOnFx {
          val clue = theme.toString
          assertEquals(fills(s.bar.node), List(p.surface3), clue)
          s.all.foreach { b =>
            assertEquals(b.getHeight, 20.0, s"$clue $b")
            assertEquals(strokes(b), List(Color.TRANSPARENT), s"$clue $b")
          }
          Vector(Sigma.One, Sigma.Four).map(s.seg).foreach { b =>
            assertEquals(fills(b), List(Color.TRANSPARENT), s"$clue $b")
            assertEquals(b.getTextFill, p.ink2, s"$clue $b")
            assertEquals(b.getFont.getFamily, SansMedium, s"$clue $b")
            assertEquals(b.getFont.getSize, 12.0, s"$clue $b")
          }
          val chosen = s.seg(Sigma.Two)
          assertEquals(fills(chosen), List(p.hairline, p.selFill), clue)
          assertEquals(chosen.getTextFill, p.ink, clue)
          assertEquals(chosen.getFont.getFamily, SansSemibold, clue)
          // The track's 2 px inset and the segments' 2 px gap.
          assertEquals(s.seg(Sigma.One).getLayoutX, 2.0, clue)
          assertEquals(
            s.seg(Sigma.Two).getLayoutX,
            s.seg(Sigma.One).getLayoutX + s.seg(Sigma.One).getWidth + 2.0,
            clue
          )
        }
        fx.snapshot(studioTheme, List(SnapshotScale.X2)): Unit
      }
  }

  fxStage.test("hover is surface-2 and pressed the hairline") { fx =>
    themes.foreach { (theme, _, p) =>
      val s                     = shown(fx, theme)
      val b                     = s.seg(Sigma.One)
      def read(change: => Unit) = runOnFx { change; s.root.applyCss(); fills(b) }
      assertEquals(
        read(b.pseudoClassStateChanged(Hover, true)),
        List(p.surface2),
        theme.toString
      )
      read(b.pseudoClassStateChanged(Hover, false))
      assertEquals(read(b.arm()), List(p.hairline), theme.toString)
      read(b.disarm())
    }
  }

  fxStage.test("one Tab stop: the chosen segment, or the first while none is") { fx =>
    val s = shown(fx, Theme.Light)
    assertEquals(runOnFx(s.all.map(_.isFocusTraversable)), Vector(false, true, false))
    runOnFx(KeyboardFocus.byPointer(s.before))
    fx.robot.press(KeyCode.TAB)
    assert(owner(fx) eq s.seg(Sigma.Two), s"Tab reached ${owner(fx)}")
    fx.robot.press(KeyCode.TAB)
    assert(owner(fx) eq s.after, s"a second Tab reached ${owner(fx)}")
    fx.robot.press(KeyCode.TAB, eyes4s.studio.desktop.harness.Modifiers(shift = true))
    assert(owner(fx) eq s.seg(Sigma.Two), s"Shift+Tab reached ${owner(fx)}")

    val none = shown(fx, Theme.Light, chosen = None)
    assertEquals(runOnFx(none.all.map(_.isFocusTraversable)), Vector(true, false, false))
    assertEquals(runOnFx(none.bar.chosen), None)
  }

  fxStage.test("arrows, Home and End move the focus and the choice; Space and Enter choose") {
    fx =>
      val s = shown(fx, Theme.Light)
      runOnFx(KeyboardFocus.byKeyboard(s.seg(Sigma.Two)))
      def at(expected: Sigma, clue: String): Unit =
        assert(owner(fx) eq s.seg(expected), s"$clue: the focus is on ${owner(fx)}")
        assertEquals(runOnFx(s.bar.chosen), Some(expected), clue)
        assert(runOnFx(s.seg(expected).isFocusVisible), s"$clue: no keyboard focus")
        assertEquals(
          runOnFx(s.all.map(_.isFocusTraversable)),
          Sigma.values.toVector.map(_ == expected),
          clue
        )
      fx.robot.press(KeyCode.RIGHT); at(Sigma.Four, "Right")
      fx.robot.press(KeyCode.RIGHT); at(Sigma.One, "Right wraps")
      fx.robot.press(KeyCode.LEFT); at(Sigma.Four, "Left wraps")
      fx.robot.press(KeyCode.UP); at(Sigma.Two, "Up")
      fx.robot.press(KeyCode.DOWN); at(Sigma.Four, "Down")
      fx.robot.press(KeyCode.HOME); at(Sigma.One, "Home")
      fx.robot.press(KeyCode.END); at(Sigma.Four, "End")
      fx.robot.press(KeyCode.END); at(Sigma.Four, "End again")
      assertEquals(
        s.reported.result(),
        Vector(Sigma.Four, Sigma.One, Sigma.Four, Sigma.Two, Sigma.Four, Sigma.One, Sigma.Four),
        "one report per change, none for a choice already made"
      )

      // With none chosen, the first segment is the stop; Space and Enter
      // choose the focused segment.
      val n = shown(fx, Theme.Light, chosen = None)
      runOnFx(KeyboardFocus.byKeyboard(n.seg(Sigma.One)))
      fx.robot.press(KeyCode.SPACE)
      assertEquals(runOnFx(n.bar.chosen), Some(Sigma.One), "Space")
      val e = shown(fx, Theme.Light, chosen = None)
      runOnFx(KeyboardFocus.byKeyboard(e.seg(Sigma.One)))
      fx.robot.press(KeyCode.ENTER)
      assertEquals(runOnFx(e.bar.chosen), Some(Sigma.One), "Enter")
      assertEquals(
        (n.reported.result(), e.reported.result()),
        (Vector(Sigma.One), Vector(Sigma.One))
      )
  }

  fxStage.test("a click chooses and focuses its segment; show() reports nothing") { fx =>
    val s = shown(fx, Theme.Light)
    fx.robot.click(s.seg(Sigma.Four))
    assert(owner(fx) eq s.seg(Sigma.Four), s"the click focused ${owner(fx)}")
    assertEquals(runOnFx(s.bar.chosen), Some(Sigma.Four))
    fx.robot.click(s.seg(Sigma.Four))
    assertEquals(runOnFx(s.bar.chosen), Some(Sigma.Four), "a second click keeps the choice")
    runOnFx(s.bar.show(segments(Some(Sigma.One))))
    assertEquals(runOnFx(s.bar.chosen), Some(Sigma.One))
    assert(owner(fx) eq s.seg(Sigma.Four), "show() keeps the segments and the focus")
    assertEquals(s.reported.result(), Vector(Sigma.Four))
  }

  fxStage.test("keyboard focus rings the focused segment alone; a click draws no accent") {
    fx =>
      themes.foreach { (theme, _, p) =>
        val s                = shown(fx, theme)
        def drawn(b: Region) = fills(b) ++ strokes(b)
        def accents          = runOnFx {
          s.root.applyCss()
          (s.bar.node +: s.all).filter(r => drawn(r).contains(p.accent))
        }
        runOnFx(KeyboardFocus.byKeyboard(s.seg(Sigma.Two)))
        assertEquals(accents, Vector(s.seg(Sigma.Two)), s"$theme: the chosen segment")
        assertEquals(runOnFx(strokes(s.seg(Sigma.Two))), List(p.accent), theme.toString)
        assertEquals(
          runOnFx(fills(s.seg(Sigma.Two))),
          List(p.hairline, p.selFill),
          s"$theme: focused and chosen shows both"
        )
        fx.robot.press(KeyCode.RIGHT)
        assertEquals(accents, Vector(s.seg(Sigma.Four)), s"$theme: the ring follows the arrow")
        // Hovered under the ring, the ring stays.
        runOnFx(s.seg(Sigma.Four).pseudoClassStateChanged(Hover, true))
        assertEquals(accents, Vector(s.seg(Sigma.Four)), s"$theme: hovered")
        runOnFx(s.seg(Sigma.Four).pseudoClassStateChanged(Hover, false))
        runOnFx(KeyboardFocus.byPointer(s.before))
        fx.robot.click(s.seg(Sigma.One))
        assert(owner(fx) eq s.seg(Sigma.One), theme.toString)
        assertEquals(accents, Vector.empty, s"$theme: a click")
      }
  }

  fxStage.test("disabled fades to 0.4 once; with a drawn reason it stays readable") { fx =>
    themes.foreach { (theme, _, p) =>
      val s = shown(fx, theme)
      runOnFx {
        val clue = theme.toString
        assertEquals(s.faded.node.getOpacity, FadedOpacity, clue)
        // The bar fades once; its segments do not fade again inside it.
        (s.faded.segmentNodes ++ s.held.segmentNodes).foreach(b =>
          assertEquals(b.getOpacity, 1.0, s"$clue $b")
        )
        assertEquals(s.held.node.getOpacity, 1.0, clue)
        assert(s.held.node.getStyleClass.contains(DisabledReason.StyleClass), clue)
        (s.held.node +: s.held.segmentNodes).foreach(n =>
          assertEquals(n.getAccessibleHelp, "Waiting for preflight", s"$clue $n")
        )
        assertEquals(s.held.segmentNodes(1).getTextFill, p.ink, clue)
      }
      runOnFx(s.why.setText("Waiting for the backend"))
      assertEquals(
        runOnFx(s.held.segmentNodes.map(_.getAccessibleHelp).distinct),
        Vector("Waiting for the backend"),
        "the help follows the drawn reason"
      )
    }
  }

  fxStage.test("each segment reads as a radio button, named by the caller") { fx =>
    val s = shown(fx, Theme.Light)
    runOnFx {
      s.all.zip(Sigma.values).foreach { (b, sigma) =>
        assertEquals(b.getAccessibleRole, AccessibleRole.RADIO_BUTTON, b.toString)
        assertEquals(b.getText, labels(sigma))
        assertEquals(b.getAccessibleText, s"Sigma: ${labels(sigma)}")
        assertEquals(
          b.queryAccessibleAttribute(AccessibleAttribute.SELECTED),
          java.lang.Boolean.valueOf(sigma == Sigma.Two),
          b.toString
        )
      }
    }
  }
