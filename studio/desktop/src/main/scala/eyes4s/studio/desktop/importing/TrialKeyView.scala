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
import eyes4s.studio.core.importing.KeyPart
import eyes4s.studio.desktop.tokens.TokenFiles
import javafx.scene.control.*
import javafx.scene.layout.*

import scala.jdk.CollectionConverters.*

/** The trial key builder's JavaFX view (ticket S5.3; Data.dc.html, key
  * builder): the key's blocks joined by "+", its key lines, the Studio check
  * and every repeated key with its trials. It binds a [[TrialKeyVM]] and
  * dispatches [[WizardIntent.IncludeOccurrence]]; it computes nothing. The
  * import wizard shows it under the fixation column mapping, where the board
  * draws it.
  */
final class TrialKeyView(dispatch: WizardIntent => Unit):
  import ImportWizardView.{label, spacer}

  val node: VBox = VBox()
  node.getStyleClass.add("key-builder")
  Option(getClass.getClassLoader.getResource(TrialKeyView.stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  // --- title and rule ------------------------------------------------------------
  val title: Label    = label("key-title", "t13")
  val rule: Label     = label("key-rule", "t11")
  private val heading = HBox()
  heading.getStyleClass.add("key-heading")
  heading.getChildren.setAll(title, spacer(), rule)

  // --- the key: blocks, "=", the count and its detail ------------------------------
  private var current: Option[TrialKeyVM] = None

  /** The occurrence block is a toggle: it adds the occurrence to the key or
    * leaves it out. The other blocks are labels.
    */
  val occurrence: ToggleButton = ToggleButton()
  occurrence.setMnemonicParsing(false)
  occurrence.getStyleClass.addAll("key-block", "key-toggle", "t12")
  occurrence.setOnAction { _ =>
    // The toggle's own state is the view-model's; the intent decides.
    current
      .flatMap(_.blocks.find(_.part == KeyPart.Occurrence))
      .foreach(b => occurrence.setSelected(b.included))
    current
      .flatMap(_.blocks.find(_.part == KeyPart.Occurrence))
      .flatMap(_.toggle)
      .foreach(t => dispatch(WizardIntent.IncludeOccurrence(t.include)))
  }

  val blocks: Map[KeyPart, Labeled] = KeyPart.values.toVector.map { part =>
    part -> (part match
      case KeyPart.Occurrence => occurrence
      case _                  => label("key-block", "t12"))
  }.toMap

  private val separators = Vector.fill(KeyPart.values.length - 1)(label("key-sep", "t12"))
  private val equalsSign = label("key-sep", "t12")
  val count: Label       = label("key-count", "mono", "t12")
  val detail: Label      = label("key-detail", "t11")
  detail.setMinWidth(0)
  detail.setTextOverrun(OverrunStyle.ELLIPSIS)
  HBox.setHgrow(detail, Priority.ALWAYS)

  val row: HBox = HBox()
  row.getStyleClass.add("key-row")
  row.getChildren.setAll(
    (KeyPart.values.toVector.zipWithIndex.flatMap { (part, i) =>
      (blocks(part): javafx.scene.Node) +: separators.lift(i).toVector
    } ++ Vector(equalsSign, count, detail)).asJava
  )

  // --- further lines, the Studio check, repeated keys -------------------------------
  val lines: VBox  = VBox()
  val check: Label = label("key-check", "t12")
  check.setWrapText(true)
  check.setMaxWidth(Double.MaxValue)
  val empty: Label = label("key-empty", "t11")

  /** Every repeated key, each with every trial it resolves to. */
  val repeated: VBox = VBox()
  repeated.getStyleClass.add("key-repeats")
  val repeatedScroll: ScrollPane = ScrollPane(repeated)
  repeatedScroll.setFitToWidth(true)
  repeatedScroll.setHbarPolicy(ScrollPane.ScrollBarPolicy.NEVER)
  // The list is read, not operated: the occurrence toggle is the view's one stop.
  repeatedScroll.setFocusTraversable(false)
  repeatedScroll.getStyleClass.addAll("edge-to-edge", "key-repeats-scroll")

  node.getChildren.setAll(heading, empty, row, lines, check, repeatedScroll)

  private def show(n: javafx.scene.Node, on: Boolean): Unit =
    n.setVisible(on)
    n.setManaged(on)

  private def toggle(n: javafx.scene.Node, styleClass: String, on: Boolean): Unit =
    val classes = n.getStyleClass
    if on && !classes.contains(styleClass) then classes.add(styleClass): Unit
    else if !on then classes.removeAll(styleClass): Unit

  private def tone(n: javafx.scene.Node, t: KeyTone): Unit =
    toggle(n, "warning", t == KeyTone.Warning)
    toggle(n, "blocking", t == KeyTone.Blocking)

  def render(vm: TrialKeyVM): Unit =
    val previous = current
    current = Some(vm)
    title.setText(vm.title)
    rule.setText(vm.rule)
    empty.setText(vm.empty.getOrElse(""))
    show(empty, vm.empty.isDefined)
    separators.foreach(_.setText(vm.plus))
    equalsSign.setText(vm.equals)
    vm.blocks.foreach { b =>
      val node = blocks(b.part)
      node.setText(b.label)
      node.setAccessibleText(b.accessible)
      toggle(node, "left-out", !b.included)
      if b.part == KeyPart.Occurrence then
        occurrence.setSelected(b.included)
        occurrence.setDisable(b.toggle.isEmpty)
        occurrence.setAccessibleHelp(b.toggle.map(_.label).orNull)
        occurrence.setTooltip(b.toggle.map(t => Tooltip(t.label)).orNull)
    }
    val first = vm.lines.headOption
    count.setText(first.fold("")(_.count))
    detail.setText(first.fold("")(_.detail))
    count.setAccessibleText(first.map(_.accessible).orNull)
    first.foreach(l => tone(detail, l.tone))
    show(equalsSign, first.isDefined)
    show(count, first.isDefined)
    show(detail, first.isDefined)
    if !previous.map(_.lines).contains(vm.lines) then
      lines.getChildren.setAll(
        vm.lines
          .drop(1)
          .map { l =>
            val text = label("key-line", "t11")
            text.setText(l.accessible)
            text.setAccessibleText(l.accessible)
            tone(text, l.tone)
            text: javafx.scene.Node
          }
          .asJava
      )
      repeated.getChildren.setAll(vm.lines.flatMap { l =>
        l.repeated.map { r =>
          val key = label("key-repeat-key", "mono", "t11")
          key.setText(r.label)
          val trials = label("key-repeat-trials", "mono", "t11")
          trials.setText(r.summary)
          trials.setMinWidth(0)
          trials.setTextOverrun(OverrunStyle.ELLIPSIS)
          HBox.setHgrow(trials, Priority.ALWAYS)
          val entry = HBox(key, trials)
          entry.getStyleClass.add("key-repeat")
          entry.setAccessibleText(r.accessible)
          entry.setFocusTraversable(false)
          entry: javafx.scene.Node
        } ++ l.more.map { text =>
          val more = label("key-repeat-more", "t11")
          more.setText(text)
          more: javafx.scene.Node
        }
      }.asJava): Unit
    show(lines, vm.lines.size > 1)
    check.setText(vm.check.getOrElse(""))
    show(check, vm.check.isDefined)
    show(repeatedScroll, vm.lines.exists(_.repeated.nonEmpty))

object TrialKeyView:

  /** The key builder's stylesheet, beside the wizard's. */
  val stylesheetResource: String = s"${TokenFiles.resourceDirectory}/studio-key.css"
