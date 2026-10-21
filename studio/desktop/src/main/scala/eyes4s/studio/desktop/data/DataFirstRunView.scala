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

package eyes4s.studio.desktop.data

import eyes4s.studio.app.{AppModel, PlatformDialog}
import eyes4s.studio.app.data.{FirstRunFile, FirstRunImport}
import cats.syntax.all.*
import eyes4s.studio.app.text.{FirstRunText as Text, FirstRunTextId as T}
import eyes4s.studio.core.document.{Preset, SourceRole}
import eyes4s.studio.core.platform.HostPath
import eyes4s.studio.desktop.shell.{CompositeFocus, SegmentedBar}
import javafx.scene.control.{Button, Label, ListCell, ListView, ScrollPane, Tooltip}
import javafx.scene.layout.{HBox, VBox, Priority}
import javafx.scene.input.{KeyCode, MouseButton, TransferMode}
import scala.jdk.CollectionConverters.*

/** First-run choices route to the normal lifecycle and import wizard. */
final class DataFirstRunView(open: PlatformDialog => Unit):
  private var selected                                          = Preset.EncodingRetrieval
  private var recent                                            = Option.empty[Vector[HostPath]]
  private def label(text: String, style: String = "t12"): Label =
    val l = Label(text)
    l.getStyleClass.add(style)
    l.setWrapText(true)
    l.setMnemonicParsing(false)
    l
  private def button(id: T)(action: => Unit): Button =
    val b = Button(Text(id))
    b.getStyleClass.addAll("import-button", "t12")
    b.setMnemonicParsing(false)
    b.setWrapText(true)
    b.setAccessibleText(Text(id))
    b.setOnAction(_ => action)
    b
  private def choose(role: SourceRole): Unit =
    open(PlatformDialog.FirstImport(FirstRunImport(choose = Some(role), preset = selected)))
  val fixations = button(T.ChooseFixations)(choose(SourceRole.Fixations))
  fixations.getStyleClass.add("primary")
  val trials = button(T.ChooseTrials)(choose(SourceRole.Trials))
  val images = button(T.ChooseImages)(
    open(PlatformDialog.FirstImport(FirstRunImport(preset = selected, chooseImages = true)))
  )
  val example = button(T.Example)(open(PlatformDialog.OpenExample))
  private def card(name: T, required: T, help: T, pick: Button): VBox =
    val heading = HBox(12, label(Text(name), "mono-500"), label(Text(required), "t12-strong"))
    val box     = VBox(10, heading, label(Text(help)), pick, label(Text(T.DropHere), "t11"))
    box.getStyleClass.add("first-run-card")
    box.setPadding(javafx.geometry.Insets(14))
    HBox.setHgrow(box, Priority.ALWAYS)
    box.setMaxWidth(Double.MaxValue)
    box.setPrefWidth(300)
    box
  val presets                     = SegmentedBar[Preset](p => { selected = p; showPresets() })
  private val description         = label(Text(T.EncodingHelp))
  private def showPresets(): Unit =
    val options = Vector(
      Preset.EncodingRetrieval -> T.Encoding,
      Preset.PerceptionImagery -> T.Imagery,
      Preset.Recognition       -> T.Recognition
    )
    presets.show(
      options.map((p, id) =>
        SegmentedBar.Segment(p, Text(id), Text(T.Presets) + ": " + Text(id), selected == p)
      )
    )
    description.setText(Text(selected match
      case Preset.PerceptionImagery => T.ImageryHelp
      case Preset.Recognition       => T.RecognitionHelp
      case _                        => T.EncodingHelp))
  showPresets()
  val sources = VBox(
    12,
    label(Text(T.NoSources), "t13"),
    label(Text(T.SourcesHelp)),
    button(T.AddFixations)(choose(SourceRole.Fixations))
  )
  sources.setPadding(javafx.geometry.Insets(20))
  sources.setAlignment(javafx.geometry.Pos.CENTER_LEFT)
  val notice     = label("")
  val recentList = ListView[HostPath]()
  recentList.setAccessibleText(Text(T.Recent))
  recentList.setFixedCellSize(30)
  CompositeFocus.track(recentList)
  recentList.setCellFactory(_ =>
    new ListCell[HostPath]:
      private val hint = Tooltip()
      setTooltip(hint)
      setMnemonicParsing(false)
      override def updateItem(item: HostPath, empty: Boolean): Unit =
        super.updateItem(item, empty)
        val text = Option(item).filter(_ => !empty).fold("")(_.value)
        setText(text)
        setAccessibleText(text)
        hint.setText(text)
      setOnMouseClicked(e =>
        if !isEmpty && e.getButton == MouseButton.PRIMARY && e.getClickCount == 1 then
          Option(getItem).foreach(path => open(PlatformDialog.OpenProjectAt(path)))
          e.consume()
      )
  )
  recentList.focusedProperty.addListener { (_, _, focused) =>
    if focused && !recentList.getItems.isEmpty && recentList.getFocusModel.getFocusedIndex < 0
    then recentList.getFocusModel.focus(0)
  }
  recentList.setOnKeyPressed(e =>
    if e.getCode == KeyCode.ENTER && !e.isControlDown && !e.isMetaDown && !e.isAltDown then
      Option(recentList.getFocusModel.getFocusedItem)
        .foreach(path => open(PlatformDialog.OpenProjectAt(path)))
      e.consume()
  )
  private val noRecent    = label(Text(T.NoRecent), "t11")
  private val recentNodes = VBox(6, noRecent, recentList)
  private val cards       = HBox(
    14,
    card(T.Fixations, T.Required, T.FixationsHelp, fixations),
    card(T.Trials, T.Recommended, T.TrialsHelp, trials)
  )
  private val imageRow =
    HBox(14, VBox(4, label(Text(T.Images), "t12-strong"), label(Text(T.ImagesHelp))), images)
  private val recentHeading = HBox(14, label(Text(T.Recent), "t13"), example)
  private val content       = VBox(
    20,
    VBox(6, label(Text(T.Title), "t28"), label(Text(T.Intro), "t16")),
    cards,
    imageRow,
    VBox(
      8,
      label(Text(T.Presets), "t13"),
      label(Text(T.PresetHelp), "t11"),
      presets.node,
      description
    ),
    VBox(6, recentHeading, recentNodes),
    label(Text(T.Local), "t11"),
    notice
  )
  content.setPadding(javafx.geometry.Insets(36, 40, 20, 40))
  val node = ScrollPane(content)
  node.setFitToWidth(true)
  node.setFocusTraversable(false)
  node.setId("data-first-run")
  Option(
    getClass.getClassLoader.getResource(
      eyes4s.studio.desktop.importing.ImportWizardView.stylesheetResource
    )
  ).foreach { url =>
    node.getStylesheets.add(url.toExternalForm): Unit
    sources.getStylesheets.add(url.toExternalForm): Unit
  }
  private def question(number: Int, heading: T, help: T): HBox =
    HBox(
      10,
      label(number.toString, "mono-500"),
      VBox(4, label(Text(heading), "t12-strong"), label(Text(help)))
    )
  val checklist = VBox(
    14,
    label(Text(T.Checklist), "t13"),
    label(Text(T.ChecklistIntro)),
    question(1, T.ColumnRoles, T.ColumnRolesHelp),
    question(2, T.TimeUnits, T.TimeUnitsHelp),
    question(3, T.FramePlacement, T.FramePlacementHelp),
    question(4, T.PixelsPerDegree, T.PixelsPerDegreeHelp),
    question(5, T.TrialIdentity, T.TrialIdentityHelp),
    label(Text(T.AdmissionReview)),
    label(Text(T.Readmit), "t11")
  )
  checklist.setPadding(javafx.geometry.Insets(16))
  checklist.setId("data-first-run-checklist")

  def sync(model: AppModel): Unit =
    val paths = model.recent.paths
    if !recent.contains(paths) then
      recent = Some(paths)
      recentList.getItems.setAll(paths.asJava)
      recentList.setVisible(paths.nonEmpty)
      recentList.setManaged(paths.nonEmpty)
      recentList.setPrefHeight(paths.size.min(4) * 30 + 2)
      recentList.setMaxHeight(recentList.getPrefHeight)
      noRecent.setVisible(paths.isEmpty)
      noRecent.setManaged(paths.isEmpty)

  /** Validate the whole drop before starting a project operation. */
  def dropFiles(files: Vector[java.io.File]): Boolean =
    val parsed = files
      .traverse { file =>
        HostPath
          .of(file.toString)
          .left
          .map(_.message)
          .map(path => FirstRunFile(path, file.getName, file.isDirectory))
      }
      .flatMap(entries => FirstRunImport.dropped(entries).left.map(_.message))
    parsed match
      case Right(request) =>
        open(PlatformDialog.FirstImport(request.copy(preset = selected)))
        true
      case Left(reason) =>
        notice.setText(Text(T.BadDropDetails, reason))
        false

  /** The actual window-wide drop handler uses the same all-files admission. */
  def installDrops(root: javafx.scene.Parent, empty: () => Boolean): Unit =
    root.setOnDragOver { event =>
      if empty() && event.getDragboard.hasFiles then
        event.acceptTransferModes(TransferMode.COPY)
        event.consume()
    }
    root.setOnDragDropped { event =>
      if empty() && event.getDragboard.hasFiles then
        event.setDropCompleted(dropFiles(event.getDragboard.getFiles.asScala.toVector))
        event.consume()
    }
