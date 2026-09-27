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
import eyes4s.studio.core.document.SourceRole
import eyes4s.studio.core.importing.GeometryField
import eyes4s.studio.desktop.tokens.TokenFiles
import javafx.scene.AccessibleRole
import javafx.scene.control.*
import javafx.scene.layout.*
import javafx.util.StringConverter

import scala.jdk.CollectionConverters.*

/** The import wizard's JavaFX view (ticket S5.2; Data.dc.html, column
  * mapping). It binds an [[ImportWizardVM]] to nodes and dispatches
  * [[WizardIntent]]s; every word, choice and enablement comes from the
  * view-model. Rows are rebuilt only when their view-models change, so a
  * focused role menu keeps its focus across renders.
  */
final class ImportWizardView(dispatch: WizardIntent => Unit):
  import ImportWizardView.*

  val node: VBox = VBox()
  node.getStyleClass.add("import-wizard")
  Option(getClass.getClassLoader.getResource(stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  // Programmatic updates of controls must not echo back as intents.
  private var rendering                        = false
  private def fire(intent: WizardIntent): Unit = if !rendering then dispatch(intent)

  // --- header: tabs and the kind of change ----------------------------------
  private val tabGroup                   = ToggleGroup()
  val tabs: Map[WizardTab, ToggleButton] = WizardTab.values.toVector.map { tab =>
    val b = ToggleButton()
    b.setMnemonicParsing(false)
    b.getStyleClass.addAll("import-tab", "t12")
    b.setToggleGroup(tabGroup)
    b.setOnAction(_ => fire(WizardIntent.ChooseTab(tab)))
    tab -> b
  }.toMap
  val kind: Label    = label("import-kind", "t11")
  private val header = HBox()
  header.getStyleClass.add("import-header")
  header.getChildren.setAll(
    (WizardTab.values.toVector.map(tabs) ++ Vector(spacer(), kind)).asJava
  )
  HBox.setMargin(kind, javafx.geometry.Insets(0, 8, 0, 0))

  // --- mapping tables -------------------------------------------------------------
  val fixations: MappingTable = MappingTable(SourceRole.Fixations, fire)
  val trials: MappingTable    = MappingTable(SourceRole.Trials, fire)

  /** The declared time unit (fixations only). */
  val time: ComboBox[TimeUnitOptionVM] = ComboBox()
  time.getStyleClass.addAll("time-select", "mono", "t11")
  time.setConverter(converter(_.label))
  time.setOnAction(_ =>
    Option(time.getValue).foreach(o => fire(WizardIntent.DeclareTime(o.unit)))
  )
  private val timeLabel = label("import-label", "t11")
  timeLabel.setLabelFor(time)
  private val timeNote = label("import-note", "t11")
  fixations.toolbar.getChildren.addAll(timeLabel, time)
  fixations.footer.getChildren.add(0, timeNote)

  // --- presets ---------------------------------------------------------------------
  val presetSelect: ComboBox[String] = ComboBox()
  presetSelect.getStyleClass.addAll("preset-select", "t12")
  val presetApply: Button   = button("import-button", "t12")
  val presetName: TextField = TextField()
  presetName.getStyleClass.addAll("import-field", "t12")
  presetName.textProperty.addListener((_, _, v) => fire(WizardIntent.TypePresetName(v)))
  val presetSave: Button      = button("import-button", "t12")
  private val presetLabel     = label("import-label", "t11")
  private val presetNameLabel = label("import-label", "t11")
  presetLabel.setLabelFor(presetSelect)
  presetNameLabel.setLabelFor(presetName)
  presetApply.setOnAction(_ =>
    Option(presetSelect.getValue).foreach(n => fire(WizardIntent.ApplyPreset(n)))
  )
  presetSave.setOnAction(_ => fire(WizardIntent.SavePreset))
  private val presets = HBox()
  presets.getStyleClass.add("import-toolbar")
  presets.getChildren.setAll(
    presetLabel,
    presetSelect,
    presetApply,
    spacer(),
    presetNameLabel,
    presetName,
    presetSave
  )
  fixations.page.getChildren.add(presets)

  // --- trial key (S5.3): under the column mapping, as the board draws it ----------
  val key: TrialKeyView = TrialKeyView(fire)
  fixations.page.getChildren.add(key.node)

  // --- geometry ----------------------------------------------------------------------
  val geometryFields: Map[GeometryField, TextField] =
    GeometryField.values.toVector.map { f =>
      val t = TextField()
      t.getStyleClass.addAll("import-field", "mono", "t12")
      t.setPrefColumnCount(8)
      t.textProperty.addListener((_, _, v) => fire(WizardIntent.EditGeometry(f, v)))
      f -> t
    }.toMap
  private val geometryLabels = GeometryField.values.toVector.map { f =>
    val l = label("import-label", "t11")
    l.setLabelFor(geometryFields(f))
    f -> l
  }.toMap
  private val geometryNote = label("import-note", "t11")
  val geometry: VBox       = VBox()
  private val geometryGrid = GridPane()
  geometryGrid.setHgap(10)
  geometryGrid.setVgap(6)
  GeometryField.values.toVector.zipWithIndex.foreach { (f, i) =>
    geometryGrid.add(geometryLabels(f), 0, i)
    geometryGrid.add(geometryFields(f), 1, i)
  }
  geometry.setSpacing(10)
  geometry.setPadding(javafx.geometry.Insets(12, 14, 12, 14))
  geometry.getChildren.setAll(geometryNote, geometryGrid)

  // --- data issues --------------------------------------------------------------------
  val issuesSummary: Label = label("import-summary", "t12")
  val issueList: VBox      = VBox()
  val issues: VBox         = VBox()
  issues.getChildren.setAll(issuesSummary, issueList)
  VBox.setMargin(issuesSummary, javafx.geometry.Insets(10, 14, 6, 14))

  // --- pages and footer -----------------------------------------------------------------
  private val pages = StackPane()
  VBox.setVgrow(pages, Priority.ALWAYS)
  pages.getChildren.setAll(fixations.page, trials.page, geometry, issues)

  val problem: Label = label("import-problem", "t12")
  val status: Label  = label("import-status", "t12")
  val cancel: Button = button("import-button", "t12")
  val commit: Button = button("import-button", "primary", "t12")
  cancel.setOnAction(_ => fire(WizardIntent.Cancel))
  commit.setOnAction(_ => fire(WizardIntent.Commit))
  private val footer = HBox()
  footer.getStyleClass.add("import-footer")
  footer.getChildren.setAll(problem, status, spacer(), cancel, commit)

  node.getChildren.setAll(header, pages, footer)

  private var last: Option[ImportWizardVM] = None

  /** The page each tab shows. */
  def page(tab: WizardTab): Region = tab match
    case WizardTab.FixationMapping => fixations.page
    case WizardTab.TrialMetadata   => trials.page
    case WizardTab.Geometry        => geometry
    case WizardTab.DataIssues      => issues

  def render(vm: ImportWizardVM): Unit =
    rendering = true
    try
      vm.tabs.foreach { t =>
        val b = tabs(t.tab)
        b.setText(t.label)
        b.setAccessibleText(t.label)
        b.setSelected(t.selected)
      }
      kind.setText(vm.kind)
      WizardTab.values.foreach { t =>
        val p = page(t)
        p.setVisible(t == vm.tab)
        p.setManaged(t == vm.tab)
      }
      fixations.render(vm.fixations)
      trials.render(vm.trials)
      key.render(vm.key)
      fixations.setNote(vm.attributesNote)
      trials.setNote(s"${vm.trialsNote} ${vm.attributesNote}")

      timeLabel.setText(vm.time.label)
      timeNote.setText(vm.time.note)
      if !last.map(_.time.options).contains(vm.time.options) then
        time.getItems.setAll(vm.time.options.asJava): Unit
      time.setValue(vm.time.options.find(_.unit == vm.time.selected).orNull)
      time.setAccessibleText(vm.time.label)
      time.setDisable(vm.fixations.rows.isEmpty)

      presetLabel.setText(vm.presets.label)
      presetNameLabel.setText(vm.presets.nameLabel)
      if !last.map(_.presets.names).contains(vm.presets.names) then
        presetSelect.getItems.setAll(vm.presets.names.asJava): Unit
      presetSelect.setPromptText(vm.presets.empty)
      presetSelect.setDisable(vm.presets.names.isEmpty || vm.fixations.rows.isEmpty)
      presetSelect.setAccessibleText(vm.presets.label)
      presetApply.setText(vm.presets.apply)
      presetApply.setDisable(presetSelect.isDisabled)
      if presetName.getText != vm.presets.name then presetName.setText(vm.presets.name)
      presetName.setAccessibleText(vm.presets.nameLabel)
      presetSave.setText(vm.presets.save)
      presetSave.setDisable(!vm.presets.canSave)

      geometryNote.setText(vm.geometryNote)
      vm.geometry.foreach { g =>
        geometryLabels(g.field).setText(g.label)
        val f = geometryFields(g.field)
        f.setAccessibleText(g.label)
        if f.getText != g.value then f.setText(g.value)
      }

      issuesSummary.setText(vm.issuesSummary)
      if !last.map(_.issues).contains(vm.issues) then
        issueList.getChildren.setAll(vm.issues.map { i =>
          val l = label("import-issue", "t12")
          if !i.blocking then l.getStyleClass.add("warning"): Unit
          l.setText(i.text)
          l.setWrapText(true)
          l.setMaxWidth(Double.MaxValue)
          l: javafx.scene.Node
        }.asJava): Unit

      problem.setText(vm.problem.getOrElse(""))
      status.setText(vm.status.getOrElse(""))
      cancel.setText(vm.cancel)
      commit.setText(vm.commit)
      commit.setDisable(!vm.canCommit)
      last = Some(vm)
    finally rendering = false

object ImportWizardView:

  /** The wizard's stylesheet, beside the shell's. */
  val stylesheetResource: String = s"${TokenFiles.resourceDirectory}/studio-import.css"

  private[importing] def label(classes: String*): Label =
    val l = Label()
    l.getStyleClass.addAll(classes*)
    l

  private[importing] def button(classes: String*): Button =
    val b = Button()
    b.setMnemonicParsing(false)
    b.getStyleClass.addAll(classes*)
    b

  private[importing] def spacer(): Region =
    val r = Region()
    HBox.setHgrow(r, Priority.ALWAYS)
    r

  private[importing] def converter[A](show: A => String): StringConverter[A] =
    new StringConverter[A]:
      def toString(a: A): String   = if a == null then "" else show(a)
      def fromString(s: String): A = throw UnsupportedOperationException("not editable")

  /** The board's four columns: 120 px, the rest, 150 px, 96 px. */
  private[importing] def columns(grid: GridPane): Unit =
    val name    = ColumnConstraints(120)
    val samples = ColumnConstraints()
    samples.setHgrow(Priority.ALWAYS)
    samples.setMinWidth(0)
    val role  = ColumnConstraints(150)
    val units = ColumnConstraints(96)
    grid.getColumnConstraints.setAll(name, samples, role, units): Unit

/** One mapping table: a toolbar (file summary and the choose button), the
  * header row, one row per column, and the empty state.
  */
final class MappingTable(role: SourceRole, fire: WizardIntent => Unit):
  import ImportWizardView.*

  val summary: Label = label("import-summary", "mono", "t11")
  val choose: Button = button("import-button", "t12")
  choose.setOnAction(_ => fire(WizardIntent.RequestFile(role)))
  val toolbar: HBox = HBox()
  toolbar.getStyleClass.add("import-toolbar")
  toolbar.getChildren.setAll(summary, spacer(), choose)

  val head: GridPane = GridPane()
  head.getStyleClass.addAll("map-head", "t11")
  columns(head)
  private val headers = Vector.fill(4)(label("t11"))
  headers.zipWithIndex.foreach((l, i) => head.add(l, i, 0))

  val rows: VBox   = VBox()
  val empty: Label = label("import-empty", "t12")
  val footer: VBox = VBox()
  private val note = label("import-note", "t11")
  footer.getChildren.setAll(note)
  footer.setSpacing(4)
  footer.setPadding(javafx.geometry.Insets(8, 14, 8, 14))
  VBox.setMargin(empty, javafx.geometry.Insets(12, 14, 12, 14))

  private val scroll = ScrollPane(rows)
  scroll.setFitToWidth(true)
  scroll.setHbarPolicy(ScrollPane.ScrollBarPolicy.NEVER)
  scroll.getStyleClass.add("edge-to-edge")
  VBox.setVgrow(scroll, Priority.ALWAYS)

  val page: VBox = VBox()
  page.getChildren.setAll(toolbar, head, empty, scroll, footer)

  /** A drawn row: its view-model, its node and its role menu. */
  /** A drawn row: its nodes and the view-model they show now. A row is
    * updated in place while its column stays in the table, so the role menu
    * a user just picked in keeps its focus.
    */
  private final class Drawn(
      val column: String,
      val node: GridPane,
      val samples: Label,
      val menu: ComboBox[ChoiceVM],
      val units: Label,
      var vm: MappingRowVM
  )

  private var drawn: Vector[Drawn]           = Vector.empty
  private var drawnChoices: Vector[ChoiceVM] = Vector.empty

  /** The role menu of `column`'s row, if the table shows it. */
  def menu(column: String): Option[ComboBox[ChoiceVM]] =
    drawn.find(_.column == column).map(_.menu)

  /** The row node of `column`, if the table shows it. */
  def rowNode(column: String): Option[GridPane] =
    drawn.find(_.column == column).map(_.node)

  /** The attributes note sits under the table. */
  def setNote(text: String): Unit = note.setText(text)

  def render(vm: MappingTableVM): Unit =
    summary.setText(vm.summary.getOrElse(""))
    choose.setText(vm.choose)
    choose.setAccessibleText(vm.choose)
    headers.zip(vm.headers).foreach((l, h) => l.setText(h))
    empty.setText(vm.empty.getOrElse(""))
    empty.setVisible(vm.empty.isDefined)
    empty.setManaged(vm.empty.isDefined)
    head.setVisible(vm.rows.nonEmpty)
    head.setManaged(vm.rows.nonEmpty)
    val columnsNow = vm.rows.map(_.column.value)
    if drawn.map(_.column) == columnsNow && drawnChoices == vm.choices then
      drawn.zip(vm.rows).foreach((d, r) => if d.vm != r then update(d, r))
    else
      val reusable =
        if drawnChoices == vm.choices then drawn.map(d => d.column -> d).toMap else Map.empty
      drawn = vm.rows.map(r =>
        reusable.get(r.column.value) match
          case Some(d) => update(d, r); d
          case None    => row(r, vm.choices)
      )
      drawnChoices = vm.choices
      rows.getChildren.setAll(drawn.map(d => d.node: javafx.scene.Node).asJava): Unit

  /** Show `r` in `d`'s nodes. */
  private def update(d: Drawn, r: MappingRowVM): Unit =
    d.vm = r
    d.samples.setText(r.samples)
    d.menu.setValue(d.menu.getItems.asScala.find(_.choice == r.choice).orNull)
    d.menu.setAccessibleText(r.accessible)
    d.units.setText(r.units)
    toggle(d.units, "undeclared", !r.unitsDeclared)
    toggle(d.node, "has-issue", r.issue.isDefined)
    d.node.setAccessibleHelp(r.issue.orNull)

  private def toggle(n: javafx.scene.Node, styleClass: String, on: Boolean): Unit =
    val classes = n.getStyleClass
    if on && !classes.contains(styleClass) then classes.add(styleClass): Unit
    else if !on then classes.removeAll(styleClass): Unit

  private def choiceCell(text: ChoiceVM => String): ListCell[ChoiceVM] =
    new ListCell[ChoiceVM]:
      override def updateItem(item: ChoiceVM, empty: Boolean): Unit =
        super.updateItem(item, empty)
        setText(if empty || item == null then "" else text(item))

  private def row(r: MappingRowVM, choices: Vector[ChoiceVM]): Drawn =
    val grid = GridPane()
    grid.getStyleClass.add("map-row")
    columns(grid)
    val name = label("map-column", "mono", "t12")
    name.setText(r.column.value)
    val samples = label("map-samples", "mono", "t11")
    samples.setMinWidth(0)
    samples.setTextOverrun(OverrunStyle.ELLIPSIS)
    val menu = ComboBox[ChoiceVM]()
    menu.getStyleClass.addAll("role-select", "t12")
    menu.setConverter(converter(_.label))
    // The open list marks required roles; the closed menu shows the board's words.
    menu.setCellFactory(_ => choiceCell(_.menuLabel))
    menu.setButtonCell(choiceCell(_.label))
    menu.getItems.setAll(choices.asJava)
    menu.setAccessibleRole(AccessibleRole.COMBO_BOX)
    menu.setMaxWidth(Double.MaxValue)
    val units = label("map-units", "mono", "t11")
    grid.add(name, 0, 0)
    grid.add(samples, 1, 0)
    grid.add(menu, 2, 0)
    grid.add(units, 3, 0)
    val d = Drawn(r.column.value, grid, samples, menu, units, r)
    update(d, r)
    // Compared with the row's current view-model, not the one it was built with.
    menu.setOnAction(_ =>
      Option(menu.getValue)
        .filter(_.choice != d.vm.choice)
        .foreach(c => fire(WizardIntent.Choose(role, d.vm.column, c.choice)))
    )
    d
