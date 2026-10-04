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

package eyes4s.studio.desktop.admission

import eyes4s.plan.AdmissionDecision as CoreAdmissionDecision
import eyes4s.studio.app.admission.*
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.tokens.TokenFiles
import javafx.geometry.Pos
import javafx.scene.control.*
import javafx.scene.input.{KeyCode, MouseButton, MouseEvent}
import javafx.scene.layout.*

/** The admission ledger's JavaFX view (ticket S5.6; Data.dc.html,
  * admission). It binds an [[AdmissionLedgerVM]] to nodes and dispatches
  * [[LedgerIntent]]s; every word, number, choice and enablement comes from
  * the view-model. Each count is a button that opens its trials; a trial of
  * the open count opens in Explore on Enter or a double click.
  */
final class AdmissionLedgerView(dispatch: LedgerIntent => Unit):
  import AdmissionLedgerView.*

  // Programmatic updates of controls must not echo back as intents.
  private var rendering                        = false
  private def fire(intent: LedgerIntent): Unit = if !rendering then dispatch(intent)

  // --- the counts -----------------------------------------------------------------
  val empty: Label   = label("ledger-note", "t12")
  val problem: Label = label("ledger-problem", "t12")
  problem.setWrapText(true)

  val header: Label       = label("t11")
  val trialsHeader: Label = label("t11")
  private val head        = HBox(header, spacer(), trialsHeader)
  head.getStyleClass.add("ledger-head")

  private val rowBox                    = VBox()
  private var rowViews: Vector[RowView] = Vector.empty

  /** The count rows now shown, in order. */
  def rows: Vector[RowView] = rowViews

  val equation: Label = label("ledger-note", "t11")
  equation.setWrapText(true)
  val issues: VBox = VBox()
  issues.getStyleClass.add("ledger-text")
  private val definitionFlow = VBox()
  val hint: Label            = label("ledger-hint", "t11")
  hint.setWrapText(true)
  private val text = VBox(equation, definitionFlow, hint)
  text.getStyleClass.add("ledger-text")

  // --- the open count's trials --------------------------------------------------------
  val openedTitle: Label = label("ledger-opened-title", "t12")
  val close: Button      = button("ledger-button")
  close.setOnAction(_ => fire(LedgerIntent.Close))
  private val openedHead = HBox(openedTitle, spacer(), close)
  openedHead.setAlignment(Pos.CENTER_LEFT)
  val openedNote: Label            = label("ledger-note", "t11")
  val trials: ListView[TrialRowVM] = ListView()
  trials.getStyleClass.add("ledger-trials")
  trials.setPrefHeight(180)
  trials.setCellFactory(_ =>
    new ListCell[TrialRowVM]:
      getStyleClass.add("t11")
      override def updateItem(item: TrialRowVM, isEmpty: Boolean): Unit =
        super.updateItem(item, isEmpty)
        if isEmpty || item == null then
          setText(null)
          setAccessibleText(null)
        else
          setText(s"${item.label} · ${item.item} — ${item.detail}")
          setAccessibleText(item.accessible)
  )
  trials.setOnKeyPressed(e =>
    if e.getCode == KeyCode.ENTER then
      openSelected()
      e.consume()
  )
  trials.addEventHandler(
    MouseEvent.MOUSE_CLICKED,
    (e: MouseEvent) =>
      if e.getButton == MouseButton.PRIMARY && e.getClickCount == 2 then openSelected()
  )
  private def openSelected(): Unit =
    Option(trials.getSelectionModel.getSelectedItem).foreach(r =>
      fire(LedgerIntent.OpenTrial(r.trial))
    )
  private val opened = VBox(openedHead, openedNote, trials)
  opened.getStyleClass.add("ledger-opened")

  private val counts = VBox(head, rowBox, text, issues, opened)
  private val scroll = ScrollPane(counts)
  scroll.setFitToWidth(true)
  scroll.setHbarPolicy(ScrollPane.ScrollBarPolicy.NEVER)
  scroll.getStyleClass.add("edge-to-edge")
  HBox.setHgrow(scroll, Priority.ALWAYS)

  // --- the decision --------------------------------------------------------------------------
  private val decisionValues                           = CoreAdmissionDecision.values.toVector
  val decisionTitle: Label                             = label("ledger-decision-title", "t13")
  private val group                                    = ToggleGroup()
  val choices: Map[CoreAdmissionDecision, RadioButton] = decisionValues.map { d =>
    val r = RadioButton()
    r.setMnemonicParsing(false)
    r.getStyleClass.addAll("ledger-choice", "t12")
    r.setToggleGroup(group)
    r.setOnAction(_ => fire(LedgerIntent.ChooseDecision(d)))
    d -> r
  }.toMap
  val choiceNotes: Map[CoreAdmissionDecision, Label] = decisionValues.map { d =>
    d -> wrapping("ledger-choice-note", "t11")
  }.toMap
  private val choiceBox = VBox(
    decisionValues.flatMap(d => Vector(choices(d), choiceNotes(d)))*
  )
  choiceBox.setSpacing(4)
  val changes: Label     = wrapping("ledger-note", "t11")
  val consequence: Label = wrapping("ledger-note", "t11")
  val admit: Button      = button("ledger-button", "primary")
  admit.setOnAction(_ => fire(LedgerIntent.Admit))
  val admitNote: Label    = wrapping("ledger-warn", "t11")
  val status: Label       = wrapping("ledger-note", "t12")
  val countsSource: Label = wrapping("ledger-note", "t11")
  val retry: Button       = button("ledger-button")
  retry.setOnAction(_ => fire(LedgerIntent.Retry))
  private val decision = VBox(
    decisionTitle,
    choiceBox,
    changes,
    consequence,
    admit,
    admitNote,
    status,
    countsSource,
    retry
  )
  decision.getStyleClass.add("ledger-decision")
  // The column keeps its width and scrolls when the pane is short.
  private val decisionScroll = ScrollPane(decision)
  decisionScroll.setFitToWidth(true)
  decisionScroll.setHbarPolicy(ScrollPane.ScrollBarPolicy.NEVER)
  decisionScroll.getStyleClass.add("edge-to-edge")
  decisionScroll.setMinWidth(Region.USE_PREF_SIZE)

  private val body = HBox(scroll, decisionScroll)
  VBox.setVgrow(body, Priority.ALWAYS)

  val node: VBox = VBox(empty, problem, body)
  node.getStyleClass.add("ledger-panel")
  Option(getClass.getClassLoader.getResource(stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  /** Bind `vm`: text, numbers, choices, enablement and visibility. */
  def render(vm: AdmissionLedgerVM): Unit =
    rendering = true
    try
      show(empty, vm.empty)
      show(problem, vm.problem)
      visible(body, vm.empty.isEmpty)
      header.setText(vm.header)
      trialsHeader.setText(vm.trialsHeader)

      // Rows are kept while the same counts are shown, so the focused count
      // keeps its focus when the trail opens it.
      if rowViews.map(_.ref) != vm.rows.map(_.ref) then
        rowViews = vm.rows.map(r => RowView(r.ref, fire))
        rowBox.getChildren.setAll(rowViews.map(_.node)*): Unit
      rowViews.zip(vm.rows).foreach((v, r) => v.render(r))

      show(equation, vm.equation)
      definitionFlow.getChildren.setAll(vm.definitions.map { d =>
        val term = label("ledger-term", "t11")
        term.setText(d.term)
        val rest = label("ledger-definitions", "t11")
        rest.setText(d.text)
        rest.setWrapText(true)
        val line = HBox(term, rest)
        line.setAlignment(Pos.BASELINE_LEFT)
        line
      }*)
      hint.setText(vm.hint)
      issues.getChildren.setAll(vm.issues.map { i =>
        val l = label("ledger-warn", "t11")
        l.setWrapText(true)
        l.setText(i)
        l
      }*)
      visible(issues, vm.issues.nonEmpty)
      visible(text, vm.rows.nonEmpty)

      vm.opened match
        case None =>
          visible(opened, false)
          trials.getItems.clear()
        case Some(o) =>
          visible(opened, true)
          openedTitle.setText(o.title)
          close.setText(o.close)
          close.setAccessibleText(o.close)
          show(openedNote, o.note)
          if trials.getItems.size != o.rows.size ||
            !trials.getItems.toArray.sameElements(o.rows)
          then trials.getItems.setAll(o.rows*): Unit
          trials.setAccessibleText(o.title)

      decisionTitle.setText(vm.decisionTitle)
      group.getToggles.forEach(_.setSelected(false))
      vm.decisions.foreach { d =>
        val r = choices(d.value)
        r.setText(d.label)
        r.setSelected(d.selected)
        r.setDisable(!vm.canDecide)
        r.setAccessibleText(s"${d.label}: ${d.note}")
        choiceNotes(d.value).setText(d.note)
      }
      show(changes, vm.changes)
      show(consequence, vm.consequence)
      admit.setText(vm.admit)
      admit.setAccessibleText(vm.admit)
      admit.setDisable(!vm.canAdmit)
      visible(admit, vm.status.isEmpty)
      show(admitNote, vm.admitNote)
      show(status, vm.status)
      countsSource.setText(vm.countsSource)
      vm.retry.foreach { r =>
        retry.setText(r)
        retry.setAccessibleText(r)
      }
      visible(retry, vm.retry.isDefined)
    finally rendering = false

  def dispose(): Unit = ()

object AdmissionLedgerView:

  val stylesheetResource: String = s"${TokenFiles.resourceDirectory}/studio-ledger.css"

  def label(classes: String*): Label =
    val l = Label()
    l.getStyleClass.addAll(classes*)
    l

  def button(classes: String*): Button =
    val b = Button()
    b.setMnemonicParsing(false)
    b.getStyleClass.addAll(classes*)
    b

  /** A label that wraps to as many lines as its text needs. */
  def wrapping(classes: String*): Label =
    val l = label(classes*)
    l.setWrapText(true)
    l.setMinHeight(Region.USE_PREF_SIZE)
    l

  def spacer(): Region =
    val r = Region()
    HBox.setHgrow(r, Priority.ALWAYS)
    r

  private def show(l: Label, text: Option[String]): Unit =
    l.setText(text.getOrElse(""))
    visible(l, text.isDefined)

  private def visible(n: javafx.scene.Node, on: Boolean): Unit =
    n.setVisible(on)
    n.setManaged(on)

  /** One count: what it counts, a note or detail, and its number as the
    * button that opens its trials.
    */
  final class RowView(val ref: StudioRef, fire: LedgerIntent => Unit):
    val label: Label  = wrapping("ledger-label", "t12")
    val detail: Label = AdmissionLedgerView.label("ledger-detail", "t11")
    val count: Button = button("ledger-count", "mono", "t12")
    count.setOnAction(_ => fire(LedgerIntent.Open(ref)))
    private val text = VBox(label, detail)
    text.setAlignment(Pos.CENTER_LEFT)
    HBox.setHgrow(text, Priority.ALWAYS)
    val node: HBox = HBox(text, count)
    node.getStyleClass.add("ledger-row")

    def render(vm: CountRowVM): Unit =
      label.setText(vm.label)
      show(detail, vm.detail.orElse(vm.note))
      count.setText(vm.value)
      count.setDisable(!vm.counted)
      count.setAccessibleText(vm.accessible)
      val classes = node.getStyleClass
      classes.removeAll("total", "plain", "group", "cause", "reported", "opened"): Unit
      val style = vm.style match
        case CountStyle.Total    => "total"
        case CountStyle.Plain    => "plain"
        case CountStyle.Group    => "group"
        case CountStyle.Cause    => "cause"
        case CountStyle.Reported => "reported"
      classes.add(style): Unit
      if vm.opened then classes.add("opened"): Unit
