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
import eyes4s.studio.core.document.{DisplayColumns, SourceRole}
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

  /** A re-map's "Remove trials.csv from this revision" or "Keep trials.csv"
    * (S5.4 follow-up), in the trial metadata page's toolbar.
    */
  val trialsSource: Button                       = button("import-button", "t12")
  private var trialsChoice: Option[WizardIntent] = None
  trialsSource.setOnAction(_ => trialsChoice.foreach(fire))
  trials.toolbar.getChildren.add(trialsSource)

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

  /** Trial inventory display bindings: selecting no kind clears its file too. */
  val displayKind: ComboBox[DisplayColumnOptionVM] = ComboBox()
  val displayFile: ComboBox[DisplayColumnOptionVM] = ComboBox()
  private val displayKindLabel                     = label("import-label", "t11")
  private val displayFileLabel                     = label("import-label", "t11")
  private val displayNote                          = label("import-note", "t11")
  displayNote.setWrapText(true)
  displayKindLabel.setLabelFor(displayKind)
  displayFileLabel.setLabelFor(displayFile)
  Vector(displayKind, displayFile).foreach { box =>
    box.getStyleClass.addAll("role-select", "t12")
    box.setConverter(converter(_.label))
    box.setAccessibleRole(AccessibleRole.COMBO_BOX)
  }
  displayKind.setOnAction(_ =>
    Option(displayKind.getValue).foreach { option =>
      val file = Option(displayFile.getValue).flatMap(_.column)
      fire(WizardIntent.DeclareDisplays(option.column.map(DisplayColumns(_, file))))
    }
  )
  displayFile.setOnAction(_ =>
    Option(displayFile.getValue).foreach { option =>
      Option(displayKind.getValue).flatMap(_.column).foreach { kind =>
        fire(WizardIntent.DeclareDisplays(Some(DisplayColumns(kind, option.column))))
      }
    }
  )
  private val displayControls =
    HBox(10, displayKindLabel, displayKind, displayFileLabel, displayFile)
  trials.footer.getChildren.addAll(displayControls, displayNote)

  /** Optional trial duration, with units declared before its column is selectable. */
  val inventoryDurationUnit: ComboBox[TimeUnitOptionVM]        = ComboBox()
  val inventoryDurationColumn: ComboBox[DisplayColumnOptionVM] = ComboBox()
  private val inventoryDurationUnitLabel                       = label("import-label", "t11")
  private val inventoryDurationColumnLabel                     = label("import-label", "t11")
  private val inventoryDurationNote                            = label("import-note", "t11")
  inventoryDurationNote.setWrapText(true)
  inventoryDurationUnit.setConverter(converter(_.label))
  inventoryDurationColumn.setConverter(converter(_.label))
  inventoryDurationUnit.getStyleClass.addAll("role-select", "t12")
  inventoryDurationColumn.getStyleClass.addAll("role-select", "t12")
  inventoryDurationUnitLabel.setLabelFor(inventoryDurationUnit)
  inventoryDurationColumnLabel.setLabelFor(inventoryDurationColumn)
  inventoryDurationUnit.setOnAction(_ =>
    Option(inventoryDurationUnit.getValue).foreach(o =>
      fire(WizardIntent.DeclareInventoryDurationUnit(o.unit))
    )
  )
  inventoryDurationColumn.setOnAction(_ =>
    Option(inventoryDurationColumn.getValue).foreach(o =>
      fire(WizardIntent.DeclareInventoryDurationColumn(o.column))
    )
  )
  trials.footer.getChildren.addAll(
    HBox(
      10,
      inventoryDurationUnitLabel,
      inventoryDurationUnit,
      inventoryDurationColumnLabel,
      inventoryDurationColumn
    ),
    inventoryDurationNote
  )

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
  geometryNote.setWrapText(true)
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

  private var last: Option[ImportWizardVM]             = None
  private var detached: Map[WizardTab, DataWizardPage] = Map.empty

  /** Move one page into its own dock node; the same controls and draft remain in use. */
  def detach(tab: WizardTab): DataWizardPage =
    detached.get(tab).getOrElse {
      val content = page(tab)
      pages.getChildren.remove(content)
      val hosted = DataWizardPage(tab, content, fire)
      detached += tab -> hosted
      last.foreach(hosted.render)
      hosted
    }

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
      // Docked pages use their dock tabs; File → Import keeps its own strip.
      val offered = vm.tabs.filterNot(t => detached.contains(t.tab))
      WizardTab.values.foreach { t =>
        val shown = vm.showTabs && offered.size > 1 && offered.exists(_.tab == t)
        tabs(t).setVisible(shown)
        tabs(t).setManaged(shown)
      }
      kind.setText(vm.kind)
      val selected = if detached.contains(vm.tab) then WizardTab.FixationMapping else vm.tab
      WizardTab.values.foreach { t =>
        val p     = page(t)
        val shown = detached.contains(t) || t == selected
        p.setVisible(shown)
        p.setManaged(shown)
      }
      fixations.render(vm.fixations)
      trials.render(vm.trials)
      key.render(vm.key)
      fixations.setNote(vm.attributesNote)
      trials.setNote(s"${vm.trialsNote} ${vm.attributesNote}")
      trialsChoice = vm.trialsSource.map(_._2)
      trialsSource.setText(vm.trialsSource.fold("")(_._1))
      trialsSource.setAccessibleText(vm.trialsSource.fold("")(_._1))
      trialsSource.setVisible(vm.trialsSource.isDefined)
      trialsSource.setManaged(vm.trialsSource.isDefined)

      displayKindLabel.setText(vm.displays.kindLabel)
      displayFileLabel.setText(vm.displays.fileLabel)
      displayNote.setText(vm.displays.note)
      Vector(displayKind, displayFile).foreach { box =>
        if !last.map(_.displays.options).contains(vm.displays.options) then
          box.getItems.setAll(vm.displays.options.asJava): Unit
      }
      displayKind.setValue(
        vm.displays.options.find(_.column == vm.displays.selected.map(_.kind)).orNull
      )
      displayFile.setValue(
        vm.displays.options.find(_.column == vm.displays.selected.flatMap(_.file)).orNull
      )
      displayKind.setAccessibleText(vm.displays.kindLabel)
      displayFile.setAccessibleText(vm.displays.fileLabel)
      displayKind.setDisable(!vm.displays.enabled)
      displayFile.setDisable(!vm.displays.enabled || vm.displays.selected.isEmpty)

      inventoryDurationUnitLabel.setText(vm.inventoryDuration.unitLabel)
      inventoryDurationColumnLabel.setText(vm.inventoryDuration.columnLabel)
      inventoryDurationNote.setText(vm.inventoryDuration.note)
      if !last.map(_.inventoryDuration.units).contains(vm.inventoryDuration.units) then
        inventoryDurationUnit.getItems.setAll(vm.inventoryDuration.units.asJava): Unit
      if !last.map(_.inventoryDuration.columns).contains(vm.inventoryDuration.columns) then
        inventoryDurationColumn.getItems.setAll(vm.inventoryDuration.columns.asJava): Unit
      inventoryDurationUnit.setValue(
        vm.inventoryDuration.units.find(_.unit == vm.inventoryDuration.selectedUnit).orNull
      )
      inventoryDurationColumn.setValue(
        vm.inventoryDuration.columns
          .find(_.column == vm.inventoryDuration.selected.map(_.column))
          .orNull
      )
      inventoryDurationUnit.setAccessibleText(vm.inventoryDuration.unitLabel)
      inventoryDurationColumn.setAccessibleText(vm.inventoryDuration.columnLabel)
      inventoryDurationUnit.setDisable(!vm.inventoryDuration.enabled)
      inventoryDurationColumn.setDisable(
        !vm.inventoryDuration.enabled || vm.inventoryDuration.selectedUnit.isEmpty
      )

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
      presetApply.setAccessibleText(vm.presets.apply)
      presetApply.setDisable(presetSelect.isDisabled)
      if presetName.getText != vm.presets.name then presetName.setText(vm.presets.name)
      presetName.setAccessibleText(vm.presets.nameLabel)
      presetSave.setText(vm.presets.save)
      presetSave.setAccessibleText(vm.presets.save)
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
      cancel.setAccessibleText(vm.cancel)
      commit.setText(vm.commit)
      commit.setAccessibleText(vm.commit)
      commit.setDisable(!vm.canCommit)
      detached.values.foreach(_.render(vm))
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
  // Its rows' role menus are the stops, not the viewport.
  scroll.setFocusTraversable(false)
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
    choose.setVisible(vm.canChoose)
    choose.setManaged(vm.canChoose)
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

/** A detached Data page: one node per dock pane, with a shared wizard's actions. */
final class DataWizardPage(
    tab: WizardTab,
    content: Region,
    dispatch: WizardIntent => Unit
):
  import ImportWizardView.*

  private val empty   = label("import-empty", "t12")
  private val notice  = label("import-problem", "t12")
  private val reading = label("import-status", "t12")
  private val problem = label("import-problem", "t12")
  private val status  = label("import-status", "t12")
  Vector(empty, notice, reading, problem, status).foreach(_.setWrapText(true))
  val cancel: Button = button("import-button", "t12")
  val commit: Button = button("import-button", "primary", "t12")
  cancel.setOnAction(_ => dispatch(WizardIntent.Cancel))
  commit.setOnAction(_ => dispatch(WizardIntent.Commit))
  private val footer = HBox(spacer(), cancel, commit)
  footer.getStyleClass.add("import-footer")
  private val scroll = ScrollPane(content)
  scroll.setFitToWidth(true)
  scroll.setHbarPolicy(ScrollPane.ScrollBarPolicy.NEVER)
  scroll.setFocusTraversable(false)
  VBox.setVgrow(scroll, Priority.ALWAYS)
  val node: VBox = VBox(empty, notice, reading, scroll, problem, status, footer)
  node.getStyleClass.add("import-wizard")
  Option(getClass.getClassLoader.getResource(stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  private var opened                       = false
  private var shownProblem: Option[String] = None
  private var shownStatus: Option[String]  = None

  private def showMessages(): Unit =
    Vector(problem -> shownProblem, status -> shownStatus).foreach { (l, text) =>
      l.setText(text.getOrElse(""))
      l.setVisible(opened && text.isDefined)
      l.setManaged(opened && text.isDefined)
    }

  def availability(available: Boolean, vm: ColumnMappingPaneVM): Unit =
    opened = available
    Vector(empty -> vm.empty, notice -> vm.notice, reading -> vm.reading).foreach { (l, text) =>
      l.setText(text.getOrElse(""))
      l.setVisible(text.isDefined)
      l.setManaged(text.isDefined)
    }
    Vector(scroll, footer).foreach { n =>
      n.setVisible(opened)
      n.setManaged(opened)
    }
    showMessages()

  def render(vm: ImportWizardVM): Unit =
    cancel.setText(vm.cancel)
    cancel.setAccessibleText(ColumnMappingPane.siblingAction(tab, vm.cancel))
    commit.setText(vm.commit)
    commit.setAccessibleText(ColumnMappingPane.siblingAction(tab, vm.commit))
    commit.setDisable(!vm.canCommit)
    shownProblem = vm.problem.filter(_.nonEmpty)
    shownStatus = vm.status.filter(_.nonEmpty)
    showMessages()
