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

import eyes4s.plan.MapPlacement
import eyes4s.studio.app.maps.{ColourLimits, MapColours, MapPalette}
import eyes4s.studio.app.tokens.*
import eyes4s.studio.core.assets.Display
import eyes4s.studio.core.selection.FixationIndex
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.harness.StudioTheme
import eyes4s.studio.desktop.plot.{CanvasPlotHost, PlotHostStatus}
import eyes4s.studio.desktop.trial.GoldenTrials
import eyes4s.studio.viz.trial.*
import javafx.geometry.Insets
import javafx.scene.control.{Button, CheckBox, Label, RadioButton, Slider}
import javafx.scene.layout.{HBox, VBox}
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle

/** System is a design specimen sheet, not an application perspective.
  * This native sheet uses the real type CSS, controls and Intaglio trial
  * scene builder. Behaviour and the remaining states are reviewed through
  * their actual application moments and the focused component suites.
  */
class SystemSpecimenFxSuite extends ShellFxSuite:
  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private def label(text: String, styles: String*): Label =
    val l = Label(text)
    l.getStyleClass.addAll(styles*)
    l
  private def colour(c: Colour): Color =
    Color.rgb(c.red, c.green, c.blue, c.alphaPercent / 100.0)

  Vector(Theme.Light -> StudioTheme.Light, Theme.Dark -> StudioTheme.Dark).foreach {
    (theme, capture) =>
      fxStage.test(s"System ${theme.toString}") { fx =>
        val hosts = scala.collection.mutable.ArrayBuffer.empty[CanvasPlotHost]
        val root  = runOnFx {
          val tokens = VBox(3.0, label("Tokens · light / dark", "t13"))
          ThemedToken.values.foreach { t =>
            val name = label(t.cssName, "mono", "t11")
            name.setMinWidth(160)
            val swatches = Vector(Theme.Light, Theme.Dark).map(th =>
              Rectangle(70, 15, colour(Tokens.themed(th, t)))
            )
            tokens.getChildren.add(HBox(6.0, (name +: swatches)*))
          }
          val ramp = VBox(5.0, label("Maps · served palette", "t13"))
          Vector(
            MapPalette.Mass       -> Vector.tabulate(9)(_ / 8.0),
            MapPalette.Difference -> Vector.tabulate(9)(i => -1.0 + i / 4.0)
          ).foreach { (palette, values) =>
            val style = ColourLimits.spanning(palette, Vector.empty)
            val cells = values.map { value =>
              val argb = MapColours.argb(style, Some(value))
              Rectangle(
                32,
                16,
                Color.rgb(
                  (argb >>> 16) & 255,
                  (argb >>> 8) & 255,
                  argb & 255,
                  ((argb >>> 24) & 255) / 255.0
                )
              )
            }
            ramp.getChildren.addAll(label(palette.toString, "t11"), HBox(cells*))
          }
          tokens.getChildren.add(ramp)
          val types = VBox(7.0, label("Type · bundled faces", "t13"))
          Vector(
            ("t11 caption", Vector("t11")),
            ("t11-strong field", Vector("t11-strong")),
            ("t12 Body text", Vector("t12")),
            ("t12-label Export · Table", Vector("t12-label")),
            ("t12-strong Selected", Vector("t12-strong")),
            ("t13 Section title", Vector("t13")),
            ("t16 Figure", Vector("t16")),
            ("+0.62", Vector("mono", "t28")),
            ("480 · 7,214", Vector("mono", "t12")),
            ("0.94 · 937", Vector("mono-500", "t12")),
            ("Methods prose only", Vector("serif", "t12"))
          ).foreach((text, styles) => types.getChildren.add(label(text, styles*)))
          types.getChildren.add(label("Shared controls", "t13"))
          val chosen   = RadioButton("Fixed"); chosen.setSelected(true)
          val check    = CheckBox("Outside"); check.setSelected(true)
          val why      = label("Waiting for preflight", "t11")
          val disabled = Button("Save & run"); disabled.setDisable(true)
          DisabledReason.attach(disabled, why)
          val segments = SegmentedBar[Int](_ => ())
          segments.show(
            Vector(1, 2, 4).map(i => SegmentedBar.Segment(i, s"$i°", s"Scale $i°", i == 2))
          )
          types.getChildren.addAll(
            RadioButton("Fixed"),
            chosen,
            CheckBox("Outside"),
            check,
            Slider(0, 1, 0.6),
            segments.node,
            disabled,
            why,
            label(
              "States and keyboard behaviour:\nControlsFxSuite · FocusVisibleFxSuite",
              "t11"
            )
          )
          types.setPrefWidth(300)
          val roles    = VBox(7.0, label("Role marks · real Intaglio trial scenes", "t13"))
          val display  = GoldenTrials.display("P17", "ret_07").copy(display = Display.Blank)
          val fixation = ok(
            TrialFixation.of(
              display.trial,
              ok(FixationIndex.of(1)),
              960,
              540,
              412,
              MapPlacement.InWindow
            )
          )
          StageVariant.values.foreach { stage =>
            roles.getChildren.add(
              label(s"${stage.toString} stage · query / matched / control", "t11")
            )
            val row = HBox(5.0)
            TrialRole.values.foreach { role =>
              val host = CanvasPlotHost()
              host.setMinSize(180, 125); host.setPrefSize(180, 125); host.setMaxSize(180, 125)
              val scene = ok(
                TrialScene(
                  TrialSceneInput(
                    display,
                    GoldenTrials.screen,
                    Vector(fixation),
                    MarkStyle.Role(role),
                    theme,
                    stage,
                    options = TrialSceneOptions(
                      order = false,
                      windowOutline = false,
                      extent = TrialExtent.Gaze,
                      captionLines = Some(Vector(""))
                    )
                  )
                )
              )
              host.show(scene.plot)
              hosts += host
              row.getChildren.add(host)
            }
            roles.getChildren.add(row)
          }
          roles.getChildren.add(
            label(
              "Maps and cased isolines: Explore / Compare / Figures captures.\n" +
                "Display kinds: TrialViewFxSuite. Freshness: story-moment captures.\n" +
                "Architecture and four change kinds: documentation review.",
              "t11"
            )
          )
          val columns = HBox(20.0, tokens, types, roles)
          val page    = VBox(
            12.0,
            label("Eyes Studio · System native specimens", "t16"),
            label("Test-only component sheet · not an application screen", "t11"),
            columns
          )
          page.setPadding(Insets(16))
          page.getStyleClass.add("studio-shell")
          val sheets: List[String] = ok(StudioStyles.stylesheets(theme))
          page.getStylesheets.setAll(sheets*)
          page
        }
        fx.show(root)
        eventually(fx, "all nine role specimens draw") {
          hosts.forall(_.status.get.isInstanceOf[PlotHostStatus.Drawn])
        }
        assertEquals(hosts.size, 9)
        fx.snapshot(capture)
        runOnFx(hosts.foreach(_.dispose()))
        ()
      }
  }
