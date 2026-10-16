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
import eyes4s.studio.app.plot.RovingMove
import eyes4s.studio.app.tokens.{StageToken, StageVariant, Theme, TokenUsage, Tokens}
import eyes4s.studio.core.document.OffScreenChoice
import eyes4s.studio.core.importing.GeometryField
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.plot.{CanvasPlotHost, PlotHostStatus}
import eyes4s.studio.desktop.shell.CompositeFocus
import eyes4s.studio.desktop.tokens.TokenFiles
import eyes4s.studio.viz.geometry.GeometryScene
import eyes4s.studio.viz.plot.PlotScene
import javafx.collections.FXCollections
import javafx.css.PseudoClass
import javafx.geometry.Pos
import javafx.scene.{AccessibleAttribute, AccessibleRole, Node}
import javafx.scene.control.*
import javafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
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
  val thumbnails: Vector[Thumbnail] = Vector.tabulate(ThumbnailCount)(Thumbnail(_))

  /** The thumbnails' strip: one list stop with a cursor ([[ThumbnailStrip]]). */
  val strip: ThumbnailStrip = ThumbnailStrip(thumbnails, fire)
  val positionsNote: Label  = label("geometry-note", "t11")

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

  /** The density key's four swatches, lowest level first, each labelled
    * with its count range; then its caption (DESIGN_SPEC section 15.1).
    */
  val densitySwatches: Vector[Label] = Vector.tabulate(DensityPicture.Levels)(swatch)
  val densityKey: Label              = label("geometry-note", "t11")
  densityKey.setWrapText(true)
  // The board ends this row with the marking button; it keeps its own row
  // below the key, so a narrow panel never elides a range or the button.
  private val keyRow = HBox(densitySwatches*)
  keyRow.getStyleClass.add("geometry-key")

  // --- the marking form ---------------------------------------------------------------------
  val orientationTitle: Label                 = label("geometry-strong", "t12")
  private val fixGroup                        = ToggleGroup()
  val fixes: Map[OrientationFix, RadioButton] = OrientationFix.values.toVector.map { f =>
    val r = RadioButton()
    r.getStyleClass.add("t12")
    r.setToggleGroup(fixGroup)
    eyes4s.studio.desktop.shell.Fx.onChosen(r)(fire(GeometryIntent.ChooseFix(f)))
    f -> r
  }.toMap
  private val scopeGroup                         = ToggleGroup()
  val scopes: Map[OrientationScope, RadioButton] = OrientationScope.values.toVector.map { s =>
    val r = RadioButton()
    r.getStyleClass.add("t12")
    r.setToggleGroup(scopeGroup)
    eyes4s.studio.desktop.shell.Fx.onChosen(r)(fire(GeometryIntent.ChooseScope(s)))
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
    eyes4s.studio.desktop.shell.Fx.onChosen(r)(fire(GeometryIntent.ChooseOffScreen(c)))
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
    strip,
    positionsNote,
    densityRow,
    keyRow,
    densityKey,
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
  // The pane is the focus stop; Tab goes through its controls, not the scroller.
  scroll.setFocusTraversable(false)
  VBox.setVgrow(scroll, Priority.ALWAYS)

  val node: VBox = VBox(header, scroll)
  node.getStyleClass.add("geometry-panel")
  Option(getClass.getClassLoader.getResource(stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  private var editor: Option[Region] = None

  /** Whether the panel's own typed fields edit the geometry: until
    * [[embed]] gives it another editor.
    */
  def editsFields: Boolean = editor.isEmpty

  /** Show `form` as the geometry's one editor, after the declared facts
    * where the panel's own typed fields were: they are then hidden, and so
    * are the panel's no-revision sentence (the form says that itself) and
    * its title, which the pane's tab already shows (the board's header holds
    * only the tab and the kind). In the window, `form` is the import draft's
    * geometry page, so a geometry edit joins the draft's one revision commit
    * (Column mapping, Trial metadata and Data issues share it). The form
    * scrolls with the panel's body.
    */
  def embed(form: Region): Unit =
    editor.foreach(e => declared.getChildren.remove(e): Unit)
    editor = Some(form)
    form.setMinHeight(Region.USE_PREF_SIZE)
    declared.getChildren.add(declared.getChildren.indexOf(facts) + 1, form)
    GeometryField.values.foreach { f =>
      visible(fields(f), false)
      visible(fieldLabels(f), false)
    }
    visible(title, false)

  /** Bind `vm`: text, choices, enablement and visibility. */
  def render(vm: GeometryPanelVM): Unit =
    rendering = true
    try
      title.setText(vm.title)
      kind.setText(vm.kind)
      show(empty, vm.empty.filter(_ => editsFields))
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
      strip.render(vm.thumbnails, vm.thumbnailsAccessible)
      show(positionsNote, vm.positionsNote)
      densityTitle.setText(vm.densityTitle)
      densityCaption.setText(vm.densityCaption)
      densityBox.setAccessibleText(vm.densityAccessible)
      densitySwatches.zip(vm.densityKey.labels).foreach((l, text) => l.setText(text))
      densitySwatches
        .zip(vm.densityKey.cased)
        .foreach((l, cased) => dressSwatch(l, vm.densityKey.stage, cased))
      densityKey.setText(vm.densityKey.caption)
      mark.setText(vm.mark)
      mark.setAccessibleText(vm.mark)
      mark.setDisable(!vm.canMark)
      show(markNote, vm.markNote)

      vm.orientation match
        case None    => visible(orientation, false)
        case Some(o) =>
          visible(orientation, true)
          orientationTitle.setText(o.title)
          o.fixes.foreach(c => choose(fixes(c.value), c.label, c.selected))
          o.scopes.foreach(c => choose(scopes(c.value), c.label, c.selected))
          record.setText(o.record)
          record.setAccessibleText(o.record)
          cancel.setText(o.cancel)
          cancel.setAccessibleText(o.cancel)

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
      vm.policies.foreach(c => choose(policies(c.value), c.label, c.selected))
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

  /** A swatch's size in logical pixels (the board's 12 × 10). */
  val SwatchWidth: Double  = 12.0
  val SwatchHeight: Double = 10.0

  /** The key swatch of density level `i` (0 is the lowest): ramp stop `i`,
    * opaque, over the stage, as the density draws its cells. [[dressSwatch]]
    * sets its stage and its casing.
    */
  def swatch(i: Int): Label =
    val fill = Region()
    fill.getStyleClass.addAll("geometry-swatch-fill", s"level-$i")
    val chip = StackPane(fill)
    chip.getStyleClass.add("geometry-swatch")
    chip.setMinSize(SwatchWidth, SwatchHeight)
    chip.setPrefSize(SwatchWidth, SwatchHeight)
    chip.setMaxSize(SwatchWidth, SwatchHeight)
    val l = label("geometry-note", "t11")
    l.setGraphic(chip)
    // A range is never elided.
    l.setMinWidth(Region.USE_PREF_SIZE)
    l

  /** The width of each band of a cased swatch's border, in logical pixels:
    * the density's own band ([[GeometryScene.DensityBandPx]]).
    */
  val SwatchBandPx: Double = GeometryScene.DensityBandPx

  /** Put `swatch`'s chip on `stage` and, when its level is `cased` there
    * ([[DensityKeyVM.cased]]), border it outside the chip with the two-band
    * stage casing as the density draws it (DESIGN_SPEC section 15.1, "Key"):
    * the stage halo outermost and [[TokenUsage.innerCasing]] next to the fill.
    */
  def dressSwatch(swatch: Label, stage: StageVariant, cased: Boolean): Unit =
    val ground = s"-fx-background-color: ${Tokens.staged(stage, StageToken.Stage).javaFxCss};"
    val border =
      if !cased then ""
      else
        val halo = Tokens.staged(stage, StageToken.Halo).javaFxCss
        // The inner casing is a palette token: every theme resolves it alike.
        val inner = Tokens.resolve(TokenUsage.innerCasing(stage), Theme.Light, stage).javaFxCss
        val band  = SwatchBandPx
        s" -fx-border-color: $halo, $inner; -fx-border-width: $band, $band;" +
          s" -fx-border-insets: ${-2 * band}, ${-band};"
    swatch.getGraphic.setStyle(ground + border)

  /** `host` in a box of the thumbnail size. */
  def sized(host: CanvasPlotHost): StackPane =
    val box = StackPane(host)
    box.getStyleClass.add("geometry-picture")
    box.setMinSize(ThumbWidth, ThumbHeight)
    box.setPrefSize(ThumbWidth, ThumbHeight)
    box.setMaxSize(ThumbWidth, ThumbHeight)
    box

  private def choose(r: RadioButton, text: String, selected: Boolean): Unit =
    r.setText(text)
    r.setAccessibleText(text)
    r.setSelected(selected)

  private def show(l: Label, text: Option[String]): Unit =
    l.setText(text.getOrElse(""))
    visible(l, text.isDefined)

  private def visible(n: javafx.scene.Node, on: Boolean): Unit =
    n.setVisible(on)
    n.setManaged(on)

  /** The style class of a thumbnail, an item of the strip. */
  val ThumbStyle: String = "geometry-thumb"

  /** The thumbnail under the strip's cursor (drawn only while the keyboard
    * holds the strip's focus).
    */
  val Cursor: PseudoClass = PseudoClass.getPseudoClass("cursor")

  /** The thumbnail of the trial chosen for marking: drawn with the selection
    * fill, never the accent (DESIGN_SPEC section 15: selection is a fill,
    * focus is a ring).
    */
  val Selected: PseudoClass = PseudoClass.getPseudoClass("selected")

  /** The cursor move a press of `code` is, if any: the arrows, Home and End. */
  def keyMove(code: KeyCode): Option[RovingMove] = code match
    case KeyCode.LEFT | KeyCode.KP_LEFT   => Some(RovingMove.Left)
    case KeyCode.RIGHT | KeyCode.KP_RIGHT => Some(RovingMove.Right)
    case KeyCode.UP | KeyCode.KP_UP       => Some(RovingMove.Up)
    case KeyCode.DOWN | KeyCode.KP_DOWN   => Some(RovingMove.Down)
    case KeyCode.HOME                     => Some(RovingMove.First)
    case KeyCode.END                      => Some(RovingMove.Last)
    case _                                => None

  /** One representative trial, item `index` of the strip: its picture and
    * its caption on two lines, the trial and then its count, so it never
    * wraps mid-phrase. The item reads as its thumbnail's accessible name.
    */
  final class Thumbnail(val index: Int):
    val host: CanvasPlotHost = plotHost()
    val picture: StackPane   = sized(host)
    val name: Label          = label("geometry-value", "mono", "t11")
    val count: Label         = label("geometry-caption", "t11")
    Vector(name, count).foreach(_.setMaxWidth(ThumbWidth))
    private var shown: Option[ThumbnailVM] = None
    private val caption                    = VBox(name, count)

    /** The item: a list item that answers its index and whether it is the
      * trial chosen for marking.
      */
    val node: VBox = new VBox(picture, caption):
      override def queryAccessibleAttribute(
          attribute: AccessibleAttribute,
          parameters: AnyRef*
      ): AnyRef = attribute match
        case AccessibleAttribute.INDEX    => Integer.valueOf(index)
        case AccessibleAttribute.SELECTED => java.lang.Boolean.valueOf(marked)
        case _ => super.queryAccessibleAttribute(attribute, parameters*)
    node.getStyleClass.addAll(ThumbStyle, "geometry-stack")
    node.setAccessibleRole(AccessibleRole.LIST_ITEM)

    /** The trial this thumbnail shows, if it shows one. */
    def trial: Option[eyes4s.studio.core.backend.TrialKey] = shown.map(_.trial)

    /** Whether it shows the trial chosen for marking. */
    def marked: Boolean = shown.exists(_.marked)

    def render(vm: Option[ThumbnailVM]): Unit =
      shown = vm
      name.setText(vm.fold("")(_.name))
      count.setText(vm.fold("")(_.count))
      node.setAccessibleText(vm.fold("")(_.accessible))
      node.pseudoClassStateChanged(Selected, marked)
      // A missing asset's caption is in the warning ink, as the board draws it.
      count.getStyleClass.remove("geometry-warn")
      if vm.exists(_.assetMissing) then count.getStyleClass.add("geometry-warn"): Unit
      node.setVisible(vm.isDefined)

  /** The placement thumbnails as the board's listbox (Data.dc.html, check
    * placement; DESIGN_SPEC section 15, composite focus): one focus stop
    * while any thumbnail is shown, with a cursor on one thumbnail. Focus
    * puts the cursor on its home thumbnail when it has none (the trial chosen
    * for marking, else the first; [[GeometryPanelVM.thumbnailHome]]); the
    * arrows, Home and End move it ([[GeometryPanelVM.thumbnailMove]]); Enter
    * or Space marks its trial for correction, as a click on a thumbnail does
    * (a click also puts the cursor there). The cursor's thumbnail draws the
    * accent frame only while the keyboard holds the strip's focus; the strip
    * draws none. A reader hears a list of trials, the cursor's as its focus
    * item and the marked one as selected.
    */
  final class ThumbnailStrip(items: Vector[Thumbnail], fire: GeometryIntent => Unit)
      extends GridPane:
    getStyleClass.add("geometry-thumbs")
    setAccessibleRole(AccessibleRole.LIST_VIEW)
    setFocusTraversable(false)
    items.foreach { t =>
      add(
        t.node,
        t.index % GeometryPanelVM.ThumbnailColumns,
        t.index / GeometryPanelVM.ThumbnailColumns
      )
      t.node.addEventHandler(
        MouseEvent.MOUSE_CLICKED,
        (_: MouseEvent) =>
          if t.trial.isDefined then
            requestFocus()
            moveTo(t.index)
            activate()
      )
    }
    private var shown: Vector[ThumbnailVM]                         = Vector.empty
    private var under: Option[eyes4s.studio.core.backend.TrialKey] = None

    /** The index of the thumbnail under the cursor, if the cursor is on one. */
    def cursor: Option[Int] = under.map(k => shown.indexWhere(_.trial == k)).filter(_ >= 0)

    /** The thumbnail the cursor frame is drawn on: the cursor's, else the
      * home one, where focus puts the cursor.
      */
    def cursorItem: Option[Int] = cursor.orElse(GeometryPanelVM.thumbnailHome(shown))

    focusedProperty.addListener((_, _, now) =>
      if now && cursor.isEmpty then cursorItem.foreach(moveTo)
    )
    addEventHandler(
      KeyEvent.KEY_PRESSED,
      (e: KeyEvent) =>
        keyMove(e.getCode) match
          case Some(move) =>
            e.consume()
            cursorItem.foreach(at =>
              moveTo(GeometryPanelVM.thumbnailMove(at, move, shown.size))
            )
          case None if e.getCode == KeyCode.ENTER || e.getCode == KeyCode.SPACE =>
            e.consume()
            if cursor.isEmpty then cursorItem.foreach(moveTo)
            activate()
          case None => ()
    )

    private def moveTo(i: Int): Unit =
      shown.lift(i).foreach { t =>
        if !under.contains(t.trial) then
          under = Some(t.trial)
          mark()
          notifyAccessibleAttributeChanged(AccessibleAttribute.FOCUS_ITEM)
      }

    // Marks the cursor's trial for correction.
    private def activate(): Unit =
      cursor.flatMap(shown.lift).foreach(t => fire(GeometryIntent.MarkTrial(t.trial)))

    private def mark(): Unit =
      val at = cursorItem
      items.foreach(t => t.node.pseudoClassStateChanged(Cursor, at.contains(t.index)))
      CompositeFocus.noCursor(this, at.isEmpty)

    /** Shows `thumbnails` (fewer hide the rest), the strip named `accessible`.
      * The cursor stays on its trial while it is shown.
      */
    def render(thumbnails: Vector[ThumbnailVM], accessible: String): Unit =
      val selected = shown.indexWhere(_.marked)
      shown = thumbnails.take(items.size)
      items.foreach(t => t.render(shown.lift(t.index)))
      setAccessibleText(accessible)
      setFocusTraversable(shown.nonEmpty)
      if isFocused && cursor.isEmpty then cursorItem.foreach(moveTo)
      mark()
      if shown.indexWhere(_.marked) != selected then
        notifyAccessibleAttributeChanged(AccessibleAttribute.SELECTED_ITEMS)

    override def queryAccessibleAttribute(
        attribute: AccessibleAttribute,
        parameters: AnyRef*
    ): AnyRef =
      def item(i: Int): Option[Node] = items.lift(i).filter(_ => i < shown.size).map(_.node)
      attribute match
        case AccessibleAttribute.ITEM_COUNT    => Integer.valueOf(shown.size)
        case AccessibleAttribute.ITEM_AT_INDEX =>
          parameters.headOption.collect { case i: Integer => i.intValue }.flatMap(item).orNull
        case AccessibleAttribute.FOCUS_ITEM         => cursorItem.flatMap(item).orNull
        case AccessibleAttribute.MULTIPLE_SELECTION => java.lang.Boolean.FALSE
        case AccessibleAttribute.SELECTED_ITEMS     =>
          FXCollections.observableArrayList[Node](
            items.filter(t => t.index < shown.size && t.marked).map(_.node)*
          )
        case _ => super.queryAccessibleAttribute(attribute, parameters*)

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
      value.setAccessibleText(vm.accessible)
      ref = vm.ref
      value.setDisable(vm.ref.isEmpty)
      note.setText(vm.note)
