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
import eyes4s.studio.desktop.harness.{SnapshotScale, StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.tokens.TokenFiles
import eyes4s.studio.desktop.typography.StudioFonts
import javafx.css.PseudoClass
import javafx.scene.control.{Button, ButtonBase, CheckBox, Label, RadioButton, Slider}
import javafx.scene.layout.{HBox, Region, VBox}
import javafx.scene.paint.Color

import scala.jdk.CollectionConverters.*

/** Radio buttons, check boxes and sliders on the tokens (REVIEW ticket 16,
  * slice a; `studio-controls.css`, after the System board's `.rd` specimen).
  * A radio or a check box is a 22 px row around a 14 px box drawn in ink:
  * unchecked an ink-3 edge on the surface, checked an ink edge and dot or an
  * ink box with a surface mark; a chosen radio's label is set at weight 600.
  * The row is surface-3 on hover and hairline while pressed. A slider is a
  * hairline-edged surface-3 track under a 14 px ink-2 thumb, ink on hover.
  * Keyboard focus is the 2 px accent border of the row, drawn only under
  * `:focus-visible`; a click or the program draws no accent anywhere. A
  * disabled control fades to 0.4 unless a reason is drawn beside it
  * (`has-reason`): then it is opaque, its box hairline on surface-2.
  *
  * `:hover` is set explicitly (a headless run moves no pointer); focus is
  * reached by traversal or by `requestFocus()` ([[KeyboardFocus]]). Each
  * check reads the rendered fills, strokes, sizes and fonts, in both themes.
  */
class ControlsFxSuite extends StudioFxSuite:

  override def beforeAll(): Unit =
    super.beforeAll()
    val problems = runOnFx(StudioFonts.loadAll())
    assert(problems.isEmpty, problems.map(_.message).mkString("\n"))

  private val Hover = PseudoClass.getPseudoClass("hover")

  private val SansSemibold = "IBM Plex Sans SmBld"
  private val SansRegular  = "IBM Plex Sans"

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
    val ink3: Color     = c(ThemedToken.Ink3)
    val surface: Color  = c(ThemedToken.Surface)
    val surface2: Color = c(ThemedToken.Surface2)
    val surface3: Color = c(ThemedToken.Surface3)
    val hairline: Color = c(ThemedToken.Hairline)

  private val themes = Vector(
    (Theme.Light, StudioTheme.Light, Palette(Theme.Light)),
    (Theme.Dark, StudioTheme.Dark, Palette(Theme.Dark))
  )

  /** The background fill colours of `r`, outermost first. */
  private def fills(r: Region): List[Any] =
    Option(r.getBackground).toList.flatMap(_.getFills.asScala.toList.map(_.getFill))

  /** The colours of `r`'s border strokes. */
  private def strokes(r: Region): List[Any] =
    Option(r.getBorder).toList.flatMap(_.getStrokes.asScala.toList.map(_.getTopStroke))

  private def part(c: Region, selector: String): Region =
    Option(c.lookup(selector)).collect { case r: Region => r }.getOrElse(fail(s"no $selector"))

  private def box(c: Region): Region = c match
    case _: RadioButton => part(c, ".radio")
    case _: CheckBox    => part(c, ".box")
    case _              => part(c, ".thumb")

  private def mark(c: Region): Region = c match
    case _: RadioButton => part(c, ".dot")
    case _              => part(c, ".mark")

  /** Every fill and stroke `c` and its parts draw. */
  private def drawnColours(c: Region): List[Any] =
    val parts = c match
      case s: Slider => List(c, part(s, ".track"), part(s, ".thumb"))
      case _         => List(c, box(c), mark(c))
    parts.flatMap(r => fills(r) ++ strokes(r))

  /** `read` after `change` and CSS. */
  private def after[A](c: Region)(change: => Unit)(read: => A): A = runOnFx {
    change
    c.getScene.getRoot.applyCss()
    read
  }

  /** One of each control, under `theme`'s studio sheets. */
  private final class Specimen(theme: Theme):
    val away: Button            = Button("away")
    val radio: RadioButton      = RadioButton("Fixed")
    val chosen: RadioButton     = RadioButton("Fixed")
    val check: CheckBox         = CheckBox("Outside")
    val checked: CheckBox       = CheckBox("Outside")
    val mixed: CheckBox         = CheckBox("Outside")
    val slider: Slider          = Slider(0.0, 1.0, 0.6)
    val fadedRadio: RadioButton = RadioButton("Fixed")
    val fadedCheck: CheckBox    = CheckBox("Outside")
    val fadedSlider: Slider     = Slider(0.0, 1.0, 0.6)
    val heldRadio: RadioButton  = RadioButton("Fixed")
    val heldCheck: CheckBox     = CheckBox("Outside")
    val heldSlider: Slider      = Slider(0.0, 1.0, 0.6)
    val why: Label              = Label("Waiting for preflight")
    chosen.setSelected(true)
    checked.setSelected(true)
    mixed.setIndeterminate(true)
    heldRadio.setSelected(true)
    heldCheck.setSelected(true)
    Vector(fadedRadio, fadedCheck, fadedSlider, heldRadio, heldCheck, heldSlider)
      .foreach(_.setDisable(true))
    Vector(heldRadio, heldCheck, heldSlider).foreach(DisabledReason.attach(_, why))
    val enabled: Vector[Region] = Vector(radio, chosen, check, checked, mixed, slider)
    val root: VBox              = VBox(
      6.0,
      HBox(6.0, away, radio, chosen, check, checked, mixed, slider),
      HBox(6.0, fadedRadio, fadedCheck, fadedSlider),
      HBox(6.0, heldRadio, heldCheck, heldSlider, why)
    )
    root.setStyle("-fx-padding: 8;")
    root.getStyleClass.add("studio-shell")
    root.getStylesheets.setAll(right(StudioStyles.stylesheets(theme))*)

  private def shown(fx: eyes4s.studio.desktop.harness.FxStage, theme: Theme): Specimen =
    val s = runOnFx(Specimen(theme))
    fx.show(s.root)
    fx.awaitLayout()
    runOnFx(s.root.applyCss())
    s

  fxStage.test("at rest and checked: a 14 px box in ink on a 22 px row, in both themes") { fx =>
    themes.foreach { (theme, studioTheme, p) =>
      val s = shown(fx, theme)
      runOnFx {
        val clue = theme.toString
        // The row: 22 px, nothing drawn but its transparent ring.
        s.enabled.foreach { c =>
          assertEquals(c.getHeight, 22.0, s"$clue $c")
          assertEquals(strokes(c), List(Color.TRANSPARENT), s"$clue $c")
          assert(!drawnColours(c).contains(p.accent), s"$clue $c draws the accent at rest")
        }
        Vector(s.radio, s.chosen, s.check, s.checked, s.mixed).foreach { c =>
          assertEquals(fills(c), List(Color.TRANSPARENT), s"$clue $c")
          assertEquals((box(c).getWidth, box(c).getHeight), (14.0, 14.0), s"$clue $c")
          assertEquals(c.getTextFill, p.ink, s"$clue $c")
        }
        // Unchecked: an ink-3 edge on the surface, no mark.
        Vector(s.radio, s.check).foreach { c =>
          assertEquals(fills(box(c)), List(p.ink3, p.surface), s"$clue $c")
          assertEquals(fills(mark(c)), List(Color.TRANSPARENT), s"$clue $c")
        }
        // Checked: an ink edge and an ink dot; an ink box and a surface mark.
        assertEquals(fills(box(s.chosen)), List(p.ink, p.surface), clue)
        assertEquals(fills(mark(s.chosen)), List(p.ink), clue)
        assertEquals((mark(s.chosen).getWidth, mark(s.chosen).getHeight), (6.0, 6.0), clue)
        Vector(s.checked, s.mixed).foreach { c =>
          assertEquals(fills(box(c)), List(p.ink), s"$clue $c")
          assertEquals(fills(mark(c)), List(p.surface), s"$clue $c")
        }
        // A chosen radio's label is semibold; a checked box's is not.
        assertEquals(s.chosen.getFont.getFamily, SansSemibold, clue)
        assertEquals(s.radio.getFont.getFamily, SansRegular, clue)
        assertEquals(s.checked.getFont.getFamily, SansRegular, clue)
        // The slider: a hairline-edged surface-3 track, a 14 px ink-2 thumb.
        assertEquals(fills(part(s.slider, ".track")), List(p.hairline, p.surface3), clue)
        val thumb = part(s.slider, ".thumb")
        assertEquals(fills(thumb), List(p.ink2), clue)
        assertEquals((thumb.getWidth, thumb.getHeight), (14.0, 14.0), clue)
        assertEquals(thumb.getEffect, null, clue)
      }
      fx.snapshot(studioTheme, List(SnapshotScale.X2)): Unit
    }
  }

  fxStage.test("hover is surface-3 and pressed the hairline; a hovered thumb is ink") { fx =>
    themes.foreach { (theme, _, p) =>
      val s = shown(fx, theme)
      Vector(s.radio, s.chosen, s.check, s.checked).foreach { c =>
        val clue = s"$theme $c"
        assertEquals(
          after(c)(c.pseudoClassStateChanged(Hover, true))(fills(c)),
          List(p.surface3),
          clue
        )
        // Hover changes the row, never the box.
        val boxed = runOnFx(fills(box(c)))
        assert(boxed.headOption.exists(f => f == p.ink || f == p.ink3), clue)
        assertEquals(
          after(c)(c.pseudoClassStateChanged(Hover, false))(fills(c)),
          List(Color.TRANSPARENT),
          clue
        )
        val b = c.asInstanceOf[ButtonBase]
        assertEquals(after(c)(b.arm())(fills(c)), List(p.hairline), clue)
        after(c)(b.disarm())(())
      }
      val thumb = runOnFx(part(s.slider, ".thumb"))
      assertEquals(
        after(thumb)(thumb.pseudoClassStateChanged(Hover, true))(fills(thumb)),
        List(p.ink),
        theme.toString
      )
      after(thumb)(thumb.pseudoClassStateChanged(Hover, false))(())
    }
  }

  fxStage.test("keyboard focus draws the accent ring on the row; a click draws no accent") {
    fx =>
      themes.foreach { (theme, _, p) =>
        val s = shown(fx, theme)
        s.enabled.foreach { c =>
          val clue    = s"$theme $c"
          val clicked = after(c)(KeyboardFocus.byPointer(c))(drawnColours(c))
          assert(!clicked.contains(p.accent), s"$clue: the accent under programmatic focus")
          assertEquals(after(c)(KeyboardFocus.byKeyboard(c))(strokes(c)), List(p.accent), clue)
          // The ring is the row's border alone: the box keeps its drawing.
          val boxed = runOnFx(fills(box(c)))
          assert(!boxed.contains(p.accent), s"$clue: the box draws the accent")
          // Hovered or pressed under the ring, the ring stays.
          assertEquals(
            after(c)(c.pseudoClassStateChanged(Hover, true))(strokes(c)),
            List(p.accent),
            clue
          )
          after(c)(c.pseudoClassStateChanged(Hover, false))(())
          after(c)(KeyboardFocus.byPointer(s.away))(())
        }
        // Space on a focused check box toggles it, and the ring stays.
        val toggled = after(s.check) {
          KeyboardFocus.byKeyboard(s.check)
          s.check.fire()
        }((s.check.isSelected, strokes(s.check), fills(box(s.check))))
        assertEquals(toggled, (true, List(p.accent), List(p.ink)), theme.toString)
      }
  }

  fxStage.test("disabled fades to 0.4; with a drawn reason it stays readable") { fx =>
    themes.foreach { (theme, _, p) =>
      val s = shown(fx, theme)
      runOnFx {
        val clue = theme.toString
        Vector(s.fadedRadio, s.fadedCheck, s.fadedSlider).foreach { c =>
          assertEquals(c.getOpacity, FadedOpacity, s"$clue $c")
        }
        Vector(s.heldRadio, s.heldCheck, s.heldSlider).foreach { c =>
          assertEquals(c.getOpacity, 1.0, s"$clue $c")
          assert(c.getStyleClass.contains(DisabledReason.StyleClass), s"$clue $c")
        }
        Vector(s.heldRadio, s.heldCheck).foreach { c =>
          assertEquals(c.getTextFill, p.ink2, s"$clue $c")
          assertEquals(fills(box(c)), List(p.hairline, p.surface2), s"$clue $c")
          assertEquals(fills(mark(c)), List(p.ink2), s"$clue $c")
        }
        assertEquals(fills(part(s.heldSlider, ".thumb")), List(p.ink3), clue)
      }
    }
  }

  test("the controls sheet is a studio sheet and a dock window's") {
    assert(StudioStyles.resources(Theme.Light).last == StudioStyles.controlsStylesheetResource)
    Theme.values.foreach { theme =>
      val sheet = TokenFiles.dockSheet(theme)
      assert(sheet.contains("@import \"studio-controls.css\";"), sheet)
    }
  }
