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

package eyes4s.studio.desktop.geometry

import eyes4s.studio.app.geometry.*
import eyes4s.studio.core.document.OffScreenChoice
import eyes4s.studio.core.importing.GeometryField
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.plot.{CanvasPlotHost, PlotHostStatus}
import eyes4s.studio.desktop.tokens.TokenFiles
import eyes4s.studio.viz.plot.PlotScene
import javafx.geometry.Pos
import javafx.scene.AccessibleRole
import javafx.scene.control.*
import javafx.scene.input.{KeyCode, MouseEvent}
import javafx.scene.layout.*

import scala.jdk.CollectionConverters.*

/** The geometry panel's JavaFX view (ticket S5.5; Data.dc.html, geometry).
  * It binds a [[GeometryPanelVM]] to nodes and dispatches
  * [[GeometryIntent]]s; every word, choice and enablement comes from the
  * view-model. The placement pictures are scenes built elsewhere
  * ([[showScenes]]), each on its own [[CanvasPlotHost]].
  */
final class GeometryPanelView(dispatch: GeometryIntent => Unit):
  import GeometryPanelView.*

  // Programmatic updates of controls must not echo back as intents.
  private var rendering                          = false
  private def fire(intent: GeometryIntent): Unit = if !rendering then dispatch(intent)

  // --- header -----------------------------------------------------------------
  val title: Label   = label("geometry-title", "t13")
  val kind: Label    = label("geometry-kind", "t11")
  private val header = HBox(title, spacer(), kind)
  header.getStyleClass.add("geometry-header")

  val target: Label = label("geometry-note", "t11")
  val empty: Label  = label("geometry-note", "t12")

  // --- declared geometry --------------------------------------------------------
  private val facts = GridPane()
  facts.getStyleClass.add("geometry-kv")

  val fields: Map[GeometryField, TextField] = GeometryField.values.toVector.map { f =>
    val t = TextField()
    t.getStyleClass.addAll("geometry-field", "mono", "t12")
    t.setPrefColumnCount(5)
    t.textProperty.addListener((_, _, now) => fire(GeometryIntent.EditField(f, now)))
    t.setOnAction(_ => fire(GeometryIntent.CommitFields))
    t.focusedProperty.addListener((_, was, now) =>
      if was && !now then fire(GeometryIntent.CommitFields)
    )
    f -> t
  }.toMap
  private val fieldLabels: Map[GeometryField, Label] =
    GeometryField.values.toVector.map { f =>
      val l = label("geometry-label", "t11")
      l.setMinWidth(Region.USE_PREF_SIZE)
      l.setLabelFor(fields(f))
      f -> l
    }.toMap
  private val fieldGrid = GridPane()
  fieldGrid.getStyleClass.add("geometry-fields")
  GeometryField.values.toVector.zipWithIndex.foreach { (f, i) =>
    fieldGrid.add(fieldLabels(f), (i % 2) * 2, i / 2)
    fieldGrid.add(fields(f), (i      % 2) * 2 + 1, i / 2)
  }

  /** Viewing distance and physical screen width: not recorded anywhere. */
  val physical: Vector[TextField] = Vector.fill(2) {
    val t = TextField()
    t.getStyleClass.addAll("geometry-field", "t12")
    t.setEditable(false)
    t.setDisable(true)
    t.setPrefColumnCount(6)
    t
  }
  private val physicalLabels = Vector.fill(2) {
    val l = label("geometry-label", "t11")
    l.setMinWidth(Region.USE_PREF_SIZE)
    l
  }
  physicalLabels.zip(physical).foreach((l, t) => l.setLabelFor(t))
  // After the seven fields: the pixels-per-degree row's free half, then a row.
  fieldGrid.add(physicalLabels(0), 2, 3)
  fieldGrid.add(physical(0), 3, 3)
  fieldGrid.add(physicalLabels(1), 0, 4)
  fieldGrid.add(physical(1), 1, 4)
  val physicalNote: Label = label("geometry-note", "t11")
  physicalNote.setWrapText(true)

  // --- worked example -------------------------------------------------------------
  val example: GridPane = GridPane()
  example.getStyleClass.add("geometry-example")

  // --- placement check ----------------------------------------------------------------
  val placementTitle: Label = label("geometry-section-title", "t13")
  val placementNote: Label  = label("geometry-note", "t11")
  private val placementHead = HBox(placementTitle, spacer(), placementNote)
  placementHead.setAlignment(Pos.BASELINE_LEFT)

  /** The four thumbnails: a plot host each, with its label. */
  val thumbnails: Vector[Thumbnail] = Vector.tabulate(ThumbnailCount)(_ => Thumbnail(fire))
  private val thumbGrid             = GridPane()
  thumbGrid.getStyleClass.add("geometry-thumbs")
  thumbnails.zipWithIndex.foreach((t, i) => thumbGrid.add(t.node, i % 2, i / 2))
  val positionsNote: Label = label("geometry-note", "t11")

  val density: CanvasPlotHost = plotHost()
  private val densityBox      = sized(density)
  densityBox.setFocusTraversable(true)
  densityBox.setAccessibleRole(AccessibleRole.IMAGE_VIEW)
  val densityTitle: Label   = label("geometry-strong", "t11")
  val densityCaption: Label = label("geometry-note", "t11")
  densityCaption.setWrapText(true)
  val mark: Button = button("geometry-button", "t12")
  mark.setOnAction(_ => fire(GeometryIntent.OpenOrientation))
  val markNote: Label     = label("geometry-note", "t11")
  private val densityText = VBox(densityTitle, densityCaption)
  densityText.getStyleClass.add("geometry-stack")
  HBox.setHgrow(densityText, Priority.ALWAYS)
  private val densityRow = HBox(densityBox, densityText)
  densityRow.getStyleClass.add("geometry-row")

  // --- the marking form ---------------------------------------------------------------------
  val orientationTitle: Label                 = label("geometry-strong", "t12")
  private val fixGroup                        = ToggleGroup()
  val fixes: Map[OrientationFix, RadioButton] = OrientationFix.values.toVector.map { f =>
    val r = RadioButton()
    r.getStyleClass.add("t12")
    r.setToggleGroup(fixGroup)
    r.setOnAction(_ => fire(GeometryIntent.ChooseFix(f)))
    f -> r
  }.toMap
  private val scopeGroup                         = ToggleGroup()
  val scopes: Map[OrientationScope, RadioButton] = OrientationScope.values.toVector.map { s =>
    val r = RadioButton()
    r.getStyleClass.add("t12")
    r.setToggleGroup(scopeGroup)
    r.setOnAction(_ => fire(GeometryIntent.ChooseScope(s)))
    s -> r
  }.toMap
  val record: Button = button("geometry-button", "t12")
  record.getStyleClass.add("primary")
  record.setOnAction(_ => fire(GeometryIntent.RecordOrientation))
  val cancel: Button = button("geometry-button", "t12")
  cancel.setOnAction(_ => fire(GeometryIntent.CancelOrientation))
  private val orientation = VBox(
    orientationTitle,
    HBox(fixes(OrientationFix.FlipX), fixes(OrientationFix.FlipY)),
    HBox(scopes(OrientationScope.ThisTrial), scopes(OrientationScope.ThisParticipant)),
    HBox(record, cancel)
  )
  orientation.getStyleClass.addAll("geometry-form", "geometry-stack")
  orientation.getChildren.asScala.collect { case h: HBox =>
    h.getStyleClass.add("geometry-row")
  }

  // --- recorded corrections -----------------------------------------------------------------
  val rulesTitle: Label = label("geometry-strong", "t12")
  val rulesNote: Label  = label("geometry-note", "t11")
  rulesNote.setWrapText(true)
  val rules: VBox = VBox()
  rules.getStyleClass.add("geometry-stack")
  val rulesEmpty: Label = label("geometry-note", "t11")

  // --- off-screen policy -----------------------------------------------------------------------
  val policyTitle: Label                          = label("geometry-strong", "t12")
  private val policyGroup                         = ToggleGroup()
  val policies: Map[OffScreenChoice, RadioButton] = OffScreenChoice.values.toVector.map { c =>
    val r = RadioButton()
    r.getStyleClass.add("t12")
    r.setToggleGroup(policyGroup)
    r.setOnAction(_ => fire(GeometryIntent.ChooseOffScreen(c)))
    c -> r
  }.toMap
  val policyNote: Label = label("geometry-note", "t11")
  policyNote.setWrapText(true)

  // --- counts ------------------------------------------------------------------------------------
  val outsideWindow: CountView = CountView(fire)
  val outsideScreen: CountView = CountView(fire)
  val countsSource: Label      = label("geometry-note", "t11")
  countsSource.setWrapText(true)

  val problem: Label = label("geometry-problem", "t12")
  problem.setWrapText(true)

  private def section(children: javafx.scene.Node*): VBox =
    val box = VBox(children*)
    box.getStyleClass.addAll("geometry-section", "geometry-stack")
    box

  private val declared = section(
    target,
    facts,
    fieldGrid,
    physicalNote,
    example
  )
  private val placement = section(
    placementHead,
    thumbGrid,
    positionsNote,
    densityRow,
    mark,
    markNote,
    orientation
  )
  private val ledger = section(rulesTitle, rulesNote, rules, rulesEmpty)
  private val policy = section(
    policyTitle,
    policies(OffScreenChoice.ExcludeRecord),
    policies(OffScreenChoice.QuarantineTrial),
    policyNote
  )
  private val counted = section(outsideWindow.node, outsideScreen.node, countsSource)

  private val content = VBox(empty, problem, declared, placement, ledger, policy, counted)
  content.getStyleClass.add("geometry-content")
  private val scroll = ScrollPane(content)
  scroll.setFitToWidth(true)
  scroll.setHbarPolicy(ScrollPane.ScrollBarPolicy.NEVER)
  scroll.getStyleClass.add("edge-to-edge")
  VBox.setVgrow(scroll, Priority.ALWAYS)

  val node: VBox = VBox(header, scroll)
  node.getStyleClass.add("geometry-panel")
  Option(getClass.getClassLoader.getResource(stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  /** Bind `vm`: text, choices, enablement and visibility. */
  def render(vm: GeometryPanelVM): Unit =
    rendering = true
    try
      title.setText(vm.title)
      kind.setText(vm.kind)
      show(empty, vm.empty)
      show(target, vm.target)
      show(problem, vm.problem)
      val hasDataset = vm.empty.isEmpty
      Vector(declared, placement, ledger, policy, counted).foreach(visible(_, hasDataset))

      facts.getChildren.clear()
      vm.facts.zipWithIndex.foreach { (f, row) =>
        val key = label("geometry-label", "t11")
        key.setText(f.label)
        key.setMinWidth(Region.USE_PREF_SIZE)
        val value = label("geometry-value", if f.mono then "mono" else "t12")
        value.getStyleClass.add("t12")
        value.setText(f.value)
        val cell = HBox(value)
        cell.getStyleClass.add("geometry-row")
        f.note.foreach { n =>
          val note = label("geometry-warn", "t11")
          note.setText(n)
          cell.getChildren.add(note)
        }
        facts.add(key, 0, row)
        facts.add(cell, 1, row)
      }
      vm.fields.foreach { f =>
        fieldLabels(f.field).setText(f.label)
        val t = fields(f.field)
        if t.getText != f.value then t.setText(f.value)
        t.setAccessibleText(f.label)
      }
      vm.physical.zipWithIndex.foreach { case ((name, prompt), i) =>
        physicalLabels(i).setText(name)
        physical(i).setPromptText(prompt)
        physical(i).setAccessibleText(s"$name: $prompt")
      }
      physicalNote.setText(vm.physicalNote)

      example.getChildren.clear()
      vm.example.zipWithIndex.foreach { (r, row) =>
        val key = label("geometry-label", "t11")
        key.setText(r.label)
        key.setMinWidth(Region.USE_PREF_SIZE)
        val value = label("geometry-value", "mono")
        value.setWrapText(true)
        value.getStyleClass.add("t11")
        value.setText(r.value)
        example.add(key, 0, row)
        example.add(value, 1, row)
      }
      visible(example, vm.example.nonEmpty)

      placementTitle.setText(vm.placementTitle)
      placementNote.setText(vm.placementNote)
      thumbnails.zipWithIndex.foreach { (t, i) => t.render(vm.thumbnails.lift(i)) }
      show(positionsNote, vm.positionsNote)
      densityTitle.setText(vm.densityTitle)
      densityCaption.setText(vm.densityCaption)
      densityBox.setAccessibleText(vm.densityAccessible)
      mark.setText(vm.mark)
      mark.setDisable(!vm.canMark)
      show(markNote, vm.markNote)

      vm.orientation match
        case None    => visible(orientation, false)
        case Some(o) =>
          visible(orientation, true)
          orientationTitle.setText(o.title)
          o.fixes.foreach(c =>
            fixes(c.value).setText(c.label)
            fixes(c.value).setSelected(c.selected)
          )
          o.scopes.foreach(c =>
            scopes(c.value).setText(c.label)
            scopes(c.value).setSelected(c.selected)
          )
          record.setText(o.record)
          cancel.setText(o.cancel)

      rulesTitle.setText(vm.rulesTitle)
      rulesNote.setText(vm.rulesNote)
      rules.getChildren.setAll(vm.rules.map { r =>
        val text = label("geometry-value", "mono")
        text.getStyleClass.add("t11")
        text.setText(r.text)
        val remove = button("geometry-button", "t11")
        remove.setText("×")
        remove.setAccessibleText(r.remove)
        remove.setTooltip(Tooltip(r.remove))
        remove.setOnAction(_ => fire(GeometryIntent.RemoveRule(r.index)))
        val row = HBox(text, spacer(), remove)
        row.getStyleClass.add("geometry-row")
        row
      }.asJava)
      show(rulesEmpty, vm.rulesEmpty)

      policyTitle.setText(vm.policyTitle)
      vm.policies.foreach { c =>
        policies(c.value).setText(c.label)
        policies(c.value).setSelected(c.selected)
      }
      policyNote.setText(vm.policyNote)
      outsideWindow.render(vm.outsideWindow)
      outsideScreen.render(vm.outsideScreen)
      countsSource.setText(vm.countsSource)
    finally rendering = false

  /** Show one scene per thumbnail (fewer thumbnails clear the rest) and the
    * density; `drawn` runs once, on the FX thread, when every one of them is
    * on its canvas.
    */
  def showScenes(thumbs: Vector[PlotScene], all: PlotScene, drawn: () => Unit): Unit =
    val hosts  = thumbnails.map(_.host).take(thumbs.size) :+ density
    val scenes = thumbs :+ all
    thumbnails.drop(thumbs.size).foreach(_.host.clear())
    var done          = false
    def check(): Unit =
      val ready = hosts.zip(scenes).forall { (h, s) =>
        h.status.get match
          case PlotHostStatus.Drawn(frame) => frame.sceneId == s.id
          case _                           => false
      }
      if ready && !done then
        done = true
        drawn()
    hosts.foreach(h =>
      h.status.addListener(
        new javafx.beans.value.ChangeListener[PlotHostStatus]:
          def changed(
              o: javafx.beans.value.ObservableValue[? <: PlotHostStatus],
              was: PlotHostStatus,
              now: PlotHostStatus
          ): Unit =
            check()
            if done then h.status.removeListener(this)
      )
    )
    hosts.zip(scenes).foreach((h, s) => h.show(s))
    check()

  /** Clear every picture (no records to draw). */
  def clearScenes(): Unit =
    thumbnails.foreach(_.host.clear())
    density.clear()

  def dispose(): Unit =
    thumbnails.foreach(_.host.dispose())
    density.dispose()

object GeometryPanelView:

  val stylesheetResource: String = s"${TokenFiles.resourceDirectory}/studio-geometry.css"

  val ThumbnailCount: Int = GeometryPictures.Thumbnails

  /** A thumbnail's size in logical pixels (the board's 164 × 92). */
  val ThumbWidth: Double  = 164.0
  val ThumbHeight: Double = 92.0

  def label(classes: String*): Label =
    val l = Label()
    l.getStyleClass.addAll(classes*)
    l

  def button(classes: String*): Button =
    val b = Button()
    b.setMnemonicParsing(false)
    b.getStyleClass.addAll(classes*)
    b

  def spacer(): Region =
    val r = Region()
    HBox.setHgrow(r, Priority.ALWAYS)
    r

  def plotHost(): CanvasPlotHost = CanvasPlotHost()

  /** `host` in a box of the thumbnail size. */
  def sized(host: CanvasPlotHost): StackPane =
    val box = StackPane(host)
    box.getStyleClass.add("geometry-picture")
    box.setMinSize(ThumbWidth, ThumbHeight)
    box.setPrefSize(ThumbWidth, ThumbHeight)
    box.setMaxSize(ThumbWidth, ThumbHeight)
    box

  private def show(l: Label, text: Option[String]): Unit =
    l.setText(text.getOrElse(""))
    visible(l, text.isDefined)

  private def visible(n: javafx.scene.Node, on: Boolean): Unit =
    n.setVisible(on)
    n.setManaged(on)

  /** One representative trial: a focusable picture that marks its trial
    * for correction (click, Enter or Space), and its label.
    */
  final class Thumbnail(fire: GeometryIntent => Unit):
    val host: CanvasPlotHost = plotHost()
    val picture: StackPane   = sized(host)
    val caption: Label       = label("geometry-caption", "mono", "t11")
    caption.setWrapText(true)
    caption.setMaxWidth(ThumbWidth)
    private var trial: Option[eyes4s.studio.core.backend.TrialKey] = None
    picture.setFocusTraversable(true)
    picture.setAccessibleRole(AccessibleRole.TOGGLE_BUTTON)
    picture.addEventHandler(MouseEvent.MOUSE_CLICKED, _ => mark())
    picture.setOnKeyPressed(e =>
      if e.getCode == KeyCode.ENTER || e.getCode == KeyCode.SPACE then
        mark()
        e.consume()
    )
    val node: VBox = VBox(picture, caption)
    node.getStyleClass.addAll("geometry-thumb", "geometry-stack")

    private def mark(): Unit = trial.foreach(t => fire(GeometryIntent.MarkTrial(t)))

    def render(vm: Option[ThumbnailVM]): Unit =
      trial = vm.map(_.trial)
      caption.setText(vm.fold("")(_.label))
      picture.setAccessibleText(vm.fold("")(_.accessible))
      val classes = picture.getStyleClass
      classes.remove("marked")
      if vm.exists(_.marked) then classes.add("marked"): Unit
      node.setVisible(vm.isDefined)

  /** A count of records outside a frame, with what it means. */
  final class CountView(fire: GeometryIntent => Unit):
    val title: Label = label("geometry-strong", "t12")
    // The count opens the eyes4s tally it shows (S5.5 follow-up).
    val value: Button = Button()
    value.getStyleClass.addAll("geometry-count", "mono", "t12")
    value.setMnemonicParsing(false)
    value.setMinWidth(Region.USE_PREF_SIZE)
    private var ref: Option[StudioRef] = None
    value.setOnAction(_ => ref.foreach(r => fire(GeometryIntent.OpenCount(r))))
    val note: Label = label("geometry-note", "t11")
    note.setWrapText(true)
    private val head = HBox(title, spacer(), value)
    head.setAlignment(Pos.CENTER_LEFT)
    val node: VBox = VBox(head, note)
    node.getStyleClass.add("geometry-stack")

    def render(vm: CountVM): Unit =
      title.setText(vm.title)
      value.setText(vm.value.getOrElse("—"))
      value.setAccessibleText(s"${vm.title}: ${vm.value.getOrElse("—")}")
      ref = vm.ref
      value.setDisable(vm.ref.isEmpty)
      note.setText(vm.note)
