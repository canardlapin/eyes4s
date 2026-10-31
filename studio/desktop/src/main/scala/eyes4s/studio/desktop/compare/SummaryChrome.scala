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

package eyes4s.studio.desktop.compare

import eyes4s.studio.app.Intent
import eyes4s.studio.app.compare.{
  HeaderChoice,
  PairedDescriptiveVM,
  SlopeKey,
  SummaryBoardText,
  SummaryBoardTextId,
  SummaryBoardVM
}
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.selection.ScaleIndex
import javafx.geometry.Pos
import javafx.scene.control.{
  Hyperlink,
  Label,
  MenuButton,
  RadioMenuItem,
  MenuItem,
  SeparatorMenuItem,
  ToggleGroup
}
import javafx.scene.layout.{HBox, Priority, Region, VBox}
import javafx.scene.shape.Line
import javafx.scene.text.TextFlow

/** The Results summary's chrome around its slopegraphs and table (overhaul
  * v4, `results.html`): the headline, the reporting menu in the plot's
  * header, the legend under the plot and the facts under the table. Every
  * word comes from [[SummaryBoardVM]] and the paired descriptive.
  */
final class SummaryChrome(
    app: Intent => Unit,
    chooseScale: ScaleIndex => Unit,
    openInspector: () => Unit
):
  import SummaryChrome.*

  private val value = Label()
  private val key   = Label()
  private val line  = Label()
  value.getStyleClass.addAll("summary-value", "t28")
  key.getStyleClass.addAll("summary-key", "t12")
  line.getStyleClass.addAll("summary-line", "t11")
  private val valueRow = HBox(8.0, value, key)
  valueRow.setAlignment(Pos.BASELINE_LEFT)

  private val caveat = javafx.scene.text.Text(SummaryBoardText(SummaryBoardTextId.Caveat) + " ")
  caveat.getStyleClass.addAll("summary-caveat", "t11")
  private val more = Hyperlink(SummaryBoardText(SummaryBoardTextId.More))
  more.getStyleClass.addAll("summary-more", "t11")
  more.setFocusTraversable(false)
  more.setOnAction(_ => openInspector())
  private val caveatFlow = TextFlow(caveat, more)
  caveatFlow.getStyleClass.add("summary-caveat-flow")
  caveatFlow.setPrefWidth(CaveatPx)
  caveatFlow.setMaxWidth(CaveatPx)
  caveatFlow.setMinWidth(Region.USE_PREF_SIZE)

  /** The headline: Δ at the reported σ, how many are higher and the range,
    * and what D is (the golden's caveat).
    */
  val headline: HBox =
    val left   = VBox(valueRow, line)
    val spacer = Region()
    HBox.setHgrow(spacer, Priority.ALWAYS)
    val h = HBox(18.0, left, spacer, caveatFlow)
    h.setAlignment(Pos.BOTTOM_LEFT)
    h.getStyleClass.add("summary-headline")
    h

  /** The reporting menu in the plot's header: the saved specifications,
    * the σ to report at, and the inspector (overhaul decision 5).
    */
  val reporting: MenuButton = MenuButton()
  reporting.getStyleClass.addAll("summary-menu", "t12")
  reporting.setMnemonicParsing(false)

  /** The keys under the slopegraphs. */
  val legend: HBox = HBox(16.0)
  legend.getStyleClass.add("summary-legend")
  legend.setAlignment(Pos.CENTER_LEFT)

  /** The facts under the participant table. */
  val footer: HBox = HBox(18.0)
  footer.getStyleClass.add("summary-footer")
  footer.setAlignment(Pos.CENTER_LEFT)

  def render(board: SummaryBoardVM, paired: Option[PairedDescriptiveVM]): Unit =
    value.setText(paired.flatMap(_.value).getOrElse(""))
    key.setText(paired.fold("")(_.heading))
    line.setText(
      paired.fold("")(p => (p.higher.toVector :+ p.note).mkString(PairedSeparator))
    )
    headline.setAccessibleText(paired.map(_.spoken).orNull)
    val shown = paired.isDefined
    headline.setVisible(shown); headline.setManaged(shown)
    reporting.setText(board.reporting)
    reporting.setAccessibleText(board.reporting)
    reporting.setVisible(board.reporting.nonEmpty)
    reporting.setManaged(board.reporting.nonEmpty)
    val specs                                                                = ToggleGroup()
    val scales                                                               = ToggleGroup()
    def item(text: String, on: Boolean, group: ToggleGroup, act: () => Unit) =
      val i = RadioMenuItem(text)
      i.setMnemonicParsing(false)
      i.setToggleGroup(group)
      i.setSelected(on)
      i.setOnAction(_ => act())
      i
    val specItems = board.choices.collect { case HeaderChoice.Spec(id, label, on) =>
      item(
        label,
        on,
        specs,
        () => app(Intent.Navigate(Location(Perspective.Results, Vector(Place.Summary(id)))))
      )
    }
    val scaleItems = board.choices.collect { case HeaderChoice.Scale(scale, label, on, ok) =>
      val i = item(label, on, scales, () => chooseScale(scale))
      i.setDisable(!ok)
      i
    }
    val edit = MenuItem(SummaryBoardText(SummaryBoardTextId.EditReporting))
    edit.setOnAction(_ => openInspector())
    reporting.getItems.setAll(
      (specItems ++ Vector(SeparatorMenuItem()) ++ scaleItems ++ Vector(
        SeparatorMenuItem(),
        edit
      ))*
    )
    legend.getChildren.setAll(board.legend.map(keyNode)*)
    legend.setVisible(board.legend.nonEmpty); legend.setManaged(board.legend.nonEmpty)
    footer.getChildren.setAll(board.footer.map { f =>
      val l = Label(f); l.getStyleClass.addAll("summary-fact", "t11"); l
    }*)
    footer.setVisible(board.footer.nonEmpty); footer.setManaged(board.footer.nonEmpty)

  private def keyNode(k: SlopeKey): javafx.scene.Node =
    def keyed(style: String, text: String) =
      val glyph = Line(1.0, 3.0, 17.0, 3.0)
      glyph.getStyleClass.addAll("summary-key-line", style)
      val l = Label(text, glyph)
      l.setGraphicTextGap(6.0)
      l.getStyleClass.addAll("summary-key-item", "t11")
      l
    k match
      case SlopeKey.Mean(t)        => keyed("summary-key-mean", t)
      case SlopeKey.Participant(t) => keyed("summary-key-participant", t)
      case SlopeKey.Selected(t)    => keyed("summary-key-selected", t)
      case SlopeKey.Thin(t)        =>
        val l = keyed("summary-key-thin", t)
        l.getGraphic match
          case g: Line => g.getStrokeDashArray.setAll(1.5, 2.0)
          case _       => ()
        l
      case SlopeKey.Note(t) =>
        val l = Label(t); l.getStyleClass.addAll("summary-key-item", "t11"); l

object SummaryChrome:
  /** The caveat's width, the golden's 340 px. */
  val CaveatPx: Double = 340.0

  /** Between the headline's count and its range: the paired note's own. */
  val PairedSeparator: String = " · "
