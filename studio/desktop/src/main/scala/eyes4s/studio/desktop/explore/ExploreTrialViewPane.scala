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

package eyes4s.studio.desktop.explore

import eyes4s.studio.app.explore.*
import eyes4s.studio.core.assets.DisplayKind
import eyes4s.studio.desktop.tokens.TokenFiles
import eyes4s.studio.desktop.trial.TrialView
import javafx.geometry.Pos
import javafx.scene.control.{Button, Label, ToggleButton}
import javafx.scene.layout.*
import javafx.scene.shape.{Circle, Line, Rectangle, Shape}

/** Explore's trial view pane (ticket S6.2; Explore.dc.html, centre): the
  * toolbar (the trial, the Points, Order and Map toggles, the dashed preview
  * label and the canvas hint), the trial on its stage, and the legend. It
  * binds an [[ExploreTrialViewVM]]; every word and state comes from it.
  */
final class ExploreTrialViewPane(dispatch: TrialViewIntent => Unit, trialView: TrialView):
  import ExploreTrialViewPane.*

  // Programmatic updates of controls must not echo back as intents.
  private var rendering                           = false
  private def fire(intent: TrialViewIntent): Unit = if !rendering then dispatch(intent)

  val title: Label = label("explore-title", "t12")

  val toggles: Map[TrialToggle, ToggleButton] = TrialToggle.values.toVector.map { tg =>
    val b = ToggleButton()
    b.setMnemonicParsing(false)
    b.getStyleClass.addAll("explore-toggle", "t12")
    b.setOnAction(_ => fire(TrialViewIntent.Switch(tg)))
    tg -> b
  }.toMap

  /** "Map: preview · σ 2° · not a result", in its dashed frame. */
  val mapLabel: Label = label("explore-preview", "t11")
  val mapNote: Label  = label("explore-note", "t11")
  val hint: Label     = label("explore-hint", "t11")

  private val bar = HBox(
    (Vector[javafx.scene.Node](title) ++
      TrialToggle.values.toVector.map(toggles) ++
      Vector(mapLabel, mapNote, spacer(), hint))*
  )
  bar.getStyleClass.add("explore-bar")
  bar.setAlignment(Pos.CENTER_LEFT)

  val note: Label = label("explore-note", "t11")
  note.setWrapText(true)
  val retry: Button = Button()
  retry.setMnemonicParsing(false)
  retry.getStyleClass.add("explore-button")
  retry.setOnAction(_ => fire(TrialViewIntent.Retry))
  private val status = HBox(note, retry)
  status.getStyleClass.add("explore-status")
  status.setAlignment(Pos.CENTER_LEFT)

  private val stage = StackPane(trialView)
  stage.getStyleClass.add("explore-stage")
  VBox.setVgrow(stage, Priority.ALWAYS)

  val legendTitle: Label = label("explore-legend-title", "t11")
  val legend: FlowPane   = FlowPane()
  legend.getStyleClass.add("explore-legend")
  private val legendRow = HBox(legendTitle, legend)
  legendRow.getStyleClass.add("explore-legend-row")
  HBox.setHgrow(legend, Priority.ALWAYS)

  val node: VBox = VBox(bar, status, stage, legendRow)
  node.getStyleClass.add("explore-panel")
  // The legend's display glyphs are the navigator's.
  Vector(stylesheetResource, TrialsNavigatorView.stylesheetResource).foreach(r =>
    Option(getClass.getClassLoader.getResource(r))
      .foreach(url => node.getStylesheets.add(url.toExternalForm))
  )

  /** Bind `vm`: the toolbar, the status and the legend; `refused` are the
    * marks the scene could not draw.
    */
  def render(vm: ExploreTrialViewVM, refused: Vector[String]): Unit =
    rendering = true
    try
      show(title, Option(vm.title).filter(_.nonEmpty))
      vm.toggles.foreach { t =>
        val b = toggles(t.toggle)
        b.setText(t.label)
        b.setSelected(t.on)
        b.setDisable(!t.enabled)
        b.setAccessibleText(t.accessible)
      }
      show(mapLabel, vm.mapLabel)
      mapLabel.setAccessibleText(vm.mapLabel.orNull)
      show(mapNote, vm.mapNote)
      hint.setText(vm.hint)
      show(note, Option((vm.note.toVector ++ refused).mkString(" ")).filter(_.nonEmpty))
      vm.retry.foreach { r =>
        retry.setText(r)
        retry.setAccessibleText(r)
      }
      visible(retry, vm.retry.isDefined)
      visible(status, vm.note.isDefined || refused.nonEmpty || vm.retry.isDefined)
      legendTitle.setText(vm.legendTitle)
      legend.getChildren.setAll(vm.legend.map { e =>
        val text = label("explore-legend-label", "t11")
        text.setText(e.label)
        val item = HBox(swatch(e.swatch), text)
        item.setAlignment(Pos.CENTER_LEFT)
        item.getStyleClass.add("explore-legend-item")
        item
      }*)
      visible(legendRow, vm.legend.nonEmpty)
    finally rendering = false

object ExploreTrialViewPane:

  val stylesheetResource: String = s"${TokenFiles.resourceDirectory}/studio-explore.css"

  def label(classes: String*): Label =
    val l = Label()
    l.getStyleClass.addAll(classes*)
    l

  private def spacer(): Region =
    val r = Region()
    HBox.setHgrow(r, Priority.ALWAYS)
    r

  private def show(l: Label, text: Option[String]): Unit =
    l.setText(text.getOrElse(""))
    visible(l, text.isDefined)

  private def visible(n: javafx.scene.Node, on: Boolean): Unit =
    n.setVisible(on)
    n.setManaged(on)

  /** The style class of a legend swatch, for tests and the stylesheet. */
  def swatchClass(s: LegendSwatch): String = s match
    case LegendSwatch.Fixation       => "swatch-fixation"
    case LegendSwatch.OutsideWindow  => "swatch-outside-window"
    case LegendSwatch.OutsideScreen  => "swatch-outside-screen"
    case LegendSwatch.DroppedInitial => "swatch-dropped"
    case LegendSwatch.OrderLine      => "swatch-order"
    case LegendSwatch.PreviewMap     => "swatch-preview"
    case LegendSwatch.Display(_)     => "swatch-display"
    case LegendSwatch.MissingAsset   => "swatch-missing"

  /** A 12 px swatch: a mark, a line, a preview patch or a display glyph. */
  def swatch(s: LegendSwatch): Pane =
    val shapes: Vector[Shape] = s match
      case LegendSwatch.Fixation | LegendSwatch.OutsideWindow | LegendSwatch.OutsideScreen |
          LegendSwatch.DroppedInitial =>
        Vector(Circle(6, 6, 4))
      case LegendSwatch.OrderLine  => Vector(Line(1, 9, 11, 3))
      case LegendSwatch.PreviewMap => Vector(Rectangle(1.5, 2.5, 9, 7))
      case LegendSwatch.Display(_) | LegendSwatch.MissingAsset => Vector.empty
    val pane = s match
      case LegendSwatch.Display(kind) =>
        TrialsNavigatorView.glyph(kind match
          case DisplayKind.Image                  => TrialGlyph.Image
          case DisplayKind.Blank                  => TrialGlyph.Blank
          case DisplayKind.BlankWithFixationCross => TrialGlyph.BlankWithCross
          case DisplayKind.Cue                    => TrialGlyph.Cue
          case DisplayKind.Unknown                => TrialGlyph.Unknown)
      case LegendSwatch.MissingAsset => TrialsNavigatorView.glyph(TrialGlyph.MissingAsset)
      case _                         =>
        shapes.foreach(_.getStyleClass.add("swatch-shape"))
        val p = Pane(shapes*)
        p.setMinSize(12, 12)
        p.setPrefSize(12, 12)
        p.setMaxSize(12, 12)
        p
    pane.getStyleClass.addAll("explore-swatch", swatchClass(s))
    pane
