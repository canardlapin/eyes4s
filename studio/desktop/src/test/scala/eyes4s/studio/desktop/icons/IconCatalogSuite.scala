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

package eyes4s.studio.desktop.icons

import eyes4s.studio.app.icons.*
import eyes4s.studio.app.tokens.{Theme, ThemedToken, Tokens}
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.harness.{FxStage, StageSize, StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.typography.StudioFonts
import javafx.geometry.Pos
import javafx.scene.SnapshotParameters
import javafx.scene.control.Label
import javafx.scene.layout.{GridPane, HBox, Region, StackPane, VBox}
import javafx.scene.paint.Color
import javafx.scene.shape.SVGPath

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

/** The icon set (ticket S1.3): complete against the ticket and the boards,
  * parsed by JavaFX, painted in the current colour in both themes, and never
  * an icon-only button without a name.
  */
class IconCatalogSuite extends StudioFxSuite:

  private val columns = 6

  override protected def stageSize: StageSize =
    StageSize(columns * 150, ((Icon.values.length + columns - 1) / columns) * 64 + 16)

  /** The ticket's list (bead S1.3 scope), in its order. */
  private val ticketIcons = List(
    "chevron-down",
    "chevron-right",
    "back",
    "forward",
    "minimize",
    "maximize",
    "pop-out",
    "sort",
    "jobs",
    "search",
    "lock",
    "warning",
    "display-image",
    "display-blank",
    "display-cross",
    "display-cue",
    "display-unknown",
    "display-missing",
    "role-query",
    "role-matched",
    "role-control",
    "eye-aperture"
  )

  /** Glyphs the boards draw beyond the ticket's list. */
  private val boardIcons = List(
    "jobs-running",
    "jobs-failed",
    "compare-revisions",
    "folder",
    "file",
    "play",
    "step-back",
    "step-forward",
    "check",
    "freshness-current",
    "freshness-draft",
    "freshness-cancelled",
    "freshness-stale"
  )

  test("the catalogue is the ticket's list plus the boards' other glyphs") {
    assertEquals(Icon.values.toList.map(_.id), ticketIcons ++ boardIcons)
  }

  test("the role and aperture shapes are the spec's: filled circle, diamond, hollow circle") {
    assertEquals(Icon.RoleQuery.layers.map(_.paint), List(IconPaint.Fill))
    assertEquals(Icon.RoleMatched.layers.map(_.paint), List(IconPaint.Fill))
    assertEquals(Icon.RoleControl.layers.map(_.paint), List(IconPaint.Stroke))
    assertEquals(
      Icon.EyeAperture.layers.map(_.paint),
      List(IconPaint.Stroke, IconPaint.FillEvenOdd)
    )
  }

  test("a board icon's drawing appears on the board it cites") {
    val buildRoot = Paths.get(
      String(
        getClass.getClassLoader
          .getResourceAsStream("eyes4s/studio/desktop/build-root.txt")
          .readAllBytes,
        UTF_8
      ).trim
    )
    // Icons drawn on the board's own 16 grid are copied verbatim.
    val verbatim = List(
      Icon.Back,
      Icon.Forward,
      Icon.Minimize,
      Icon.PopOut,
      Icon.Sort,
      Icon.Jobs,
      Icon.JobsRunning,
      Icon.CompareRevisions,
      Icon.StepBack,
      Icon.StepForward,
      Icon.Play,
      Icon.Folder,
      Icon.File
    )
    verbatim.foreach { icon =>
      val file = icon.source match
        case IconSource.Board(f) => f
        case IconSource.Drawn    => fail(s"${icon.id} is not a board icon")
      val board =
        String(Files.readAllBytes(buildRoot.resolve(s"docs/studio/design/$file")), UTF_8)
      icon.layers.foreach { l =>
        assert(
          board.contains(s"d=\"${l.pathData}\""),
          s"${icon.id}: '${l.pathData}' not in $file"
        )
      }
    }
    assertEquals(
      Icon.values.toList.filter(_.source == IconSource.Drawn).map(_.id),
      List("search", "warning", "display-cue", "display-unknown")
    )
  }

  fxStage.test("JavaFX parses every icon inside its 16x16 view box") { fx =>
    Icon.values.foreach { icon =>
      icon.layers.foreach { layer =>
        val bounds = fx.runOnFx {
          val p = StudioIcons.path(layer)
          p.getLayoutBounds
        }
        assert(bounds.getWidth > 0 || bounds.getHeight > 0, s"${icon.id}: empty path")
        val slack = Icon.strokeWidth // a mitred corner may reach past half the stroke
        assert(
          bounds.getMinX >= -slack && bounds.getMinY >= -slack &&
            bounds.getMaxX <= Icon.viewBox + slack && bounds.getMaxY <= Icon.viewBox + slack,
          s"${icon.id}: $bounds"
        )
      }
      val node = fx.runOnFx(StudioIcons.graphic(icon))
      assert(fx.runOnFx(node.getStyleClass.contains(s"icon-${icon.id}")), icon.id)
    }
  }

  fxStage.test("icon-only buttons carry their accessible text; plain strings do not compile") {
    fx =>
      val label = AccessibleText.parse("Back (⌘[)").fold(e => fail(e.message), identity)
      val b     = fx.runOnFx(StudioIcons.button(Icon.Back, label))
      assertEquals(fx.runOnFx(b.getAccessibleText), "Back (⌘[)")
      assertEquals(fx.runOnFx(b.getTooltip.getText), "Back (⌘[)")
      assertEquals(fx.runOnFx(b.getText), "")
      import scala.compiletime.testing.typeCheckErrors
      assert(typeCheckErrors("StudioIcons.button(Icon.Back, \"Back\")").nonEmpty)
  }

  // ---------------------------------------------------------------------------
  // Rendering: every icon, both themes, in the current colour
  // ---------------------------------------------------------------------------

  private def sheet(): GridPane =
    val grid = GridPane()
    grid.setHgap(0)
    grid.setVgap(0)
    grid.setStyle("-fx-background-color: -es-surface; -fx-padding: 8;")
    Icon.values.zipWithIndex.foreach { (icon, i) =>
      val big   = StudioIcons.graphic(icon, IconSize.Px20)
      val small = StudioIcons.graphic(icon, IconSize.Px14)
      val name  = Label(icon.id)
      name.getStyleClass.addAll("t11", "mono")
      name.setStyle("-fx-text-fill: -es-ink-2;")
      val row = HBox(8, big, small)
      row.setAlignment(Pos.CENTER_LEFT)
      val cell = VBox(4, row, name)
      cell.setPrefSize(150, 64)
      grid.add(cell, i % columns, i / columns)
    }
    grid

  private def useTheme(fx: FxStage, theme: Theme): Unit =
    val sheets = StudioStyles.stylesheets(theme).fold(e => fail(e.message), identity)
    fx.runOnFx(fx.scene.getStylesheets.setAll(sheets*)): Unit

  // The opaque pixels of one icon rendered at 4x on a transparent background.
  private def inkPixels(fx: FxStage, node: Region): List[Color] =
    fx.runOnFx {
      val params = SnapshotParameters()
      params.setFill(Color.TRANSPARENT)
      params.setTransform(javafx.scene.transform.Transform.scale(4, 4))
      val image  = node.snapshot(params, null)
      val reader = image.getPixelReader
      (for
        x <- 0 until image.getWidth.toInt
        y <- 0 until image.getHeight.toInt
        c = reader.getColor(x, y)
        if c.getOpacity > 0.99
      yield c).toList
    }

  fxStage.test("every icon renders in the current colour, light and dark; sheet written") {
    fx =>
      assertEquals(StudioFonts.loadAll(), Nil)
      val grid = fx.runOnFx(sheet())
      useTheme(fx, Theme.Light)
      fx.show(fx.runOnFx(StackPane(grid)))
      List(Theme.Light -> StudioTheme.Light, Theme.Dark -> StudioTheme.Dark).foreach {
        (theme, studioTheme) =>
          useTheme(fx, theme)
          val files = fx.snapshot(studioTheme)
          files.foreach(f => assert(Files.size(f) > 0L, f))
          val ink = Tokens.themed(theme, ThemedToken.Ink)
          Icon.values.foreach { icon =>
            val node = fx.runOnFx(StudioIcons.graphic(icon, IconSize.Px16))
            fx.runOnFx(grid.add(node, 0, 100))
            fx.awaitLayout()
            val opaque = inkPixels(fx, node)
            fx.runOnFx(grid.getChildren.remove(node)): Unit
            assert(opaque.nonEmpty, s"$theme ${icon.id}: nothing drawn")
            val off = opaque.filterNot { c =>
              math.abs(c.getRed * 255 - ink.red) <= 1.0 &&
              math.abs(c.getGreen * 255 - ink.green) <= 1.0 &&
              math.abs(c.getBlue * 255 - ink.blue) <= 1.0
            }
            assert(
              off.isEmpty,
              s"$theme ${icon.id}: ${off.size} opaque pixel(s) not ink, e.g. ${off.head}"
            )
          }
      }
  }

  fxStage.test("a control recolours its icon by redefining -es-current") { fx =>
    val icon = fx.runOnFx {
      val g = StudioIcons.graphic(Icon.RoleQuery)
      g.setStyle("-es-current: -es-accent;")
      g
    }
    useTheme(fx, Theme.Light)
    fx.show(fx.runOnFx(StackPane(icon)))
    val accent = Tokens.themed(Theme.Light, ThemedToken.Accent)
    val fill   = fx.runOnFx(
      icon.lookup(".icon-fill").asInstanceOf[SVGPath].getFill.asInstanceOf[Color]
    )
    assertEquals(
      (
        math.round(fill.getRed * 255),
        math.round(fill.getGreen * 255),
        math.round(fill.getBlue * 255)
      ),
      (accent.red.toLong, accent.green.toLong, accent.blue.toLong)
    )
  }
