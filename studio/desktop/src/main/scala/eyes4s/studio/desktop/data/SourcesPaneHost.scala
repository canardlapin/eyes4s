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

import eyes4s.codec.ByteDigest
import eyes4s.studio.app.data.*
import eyes4s.studio.app.text.{SourcesText, SourcesTextId}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.assets.{AssetFile, DisplayKind}
import eyes4s.studio.core.bundle.InputKind
import eyes4s.studio.desktop.explore.NavigatorDisplays
import eyes4s.studio.desktop.runtime.ProjectPort
import eyes4s.studio.desktop.tokens.TokenFiles
import javafx.application.Platform
import javafx.geometry.Pos
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.{GridPane, HBox, Region, VBox}
import javafx.stage.{FileChooser, Window}

import java.nio.file.Files

/** Where Repair… finds the image to show for a missing file: its name and
  * bytes, or `None` when the user cancelled. `done` may be called on any
  * thread.
  */
trait AssetFiles:
  def locate(
      file: AssetFile,
      done: Either[String, Option[(AssetFile, IArray[Byte])]] => Unit
  ): Unit

object AssetFiles:
  /** A file chooser titled for the missing file, over images. */
  def chooser(owner: () => Window): AssetFiles =
    (file, done) =>
      val chooser = FileChooser()
      chooser.setTitle(s"Locate ${file.value}")
      chooser.getExtensionFilters.add(
        FileChooser.ExtensionFilter("Images", "*.png", "*.jpg", "*.jpeg", "*.bmp", "*.gif")
      )
      Option(chooser.showOpenDialog(owner())) match
        case None    => done(Right(None))
        case Some(f) =>
          val read =
            try
              AssetFile
                .of(f.getName)
                .left
                .map(_.message)
                .map(n => Some(n -> IArray.unsafeFromArray(Files.readAllBytes(f.toPath))))
            catch
              case e: java.io.IOException => Left(Option(e.getMessage).getOrElse(e.toString))
          done(read)

/** The Data perspective's Sources pane on the desktop (ticket S5.7;
  * Data.dc.html, left): it binds a [[SourcesVM]] and performs the pane's
  * effects. Repair… asks [[AssetFiles]] for the image, stores its bytes in
  * the project as a stimulus input, and answers with their SHA-256; a
  * window without a project cannot store one, and says so. FX thread only.
  */
final class SourcesPaneHost(
    model: () => AppModel,
    app: Intent => Unit,
    displays: NavigatorDisplays,
    files: AssetFiles,
    project: Option[ProjectPort]
):

  private var pane     = SourcesPane.empty
  private var disposed = false

  val view: SourcesView       = SourcesView(i => dispatch(i))
  def node: javafx.scene.Node = view.node

  /** The pane's state now. */
  def state: SourcesPane = pane

  /** The view-model now shown. */
  def vm: SourcesVM = SourcesVM.of(pane, model())

  /** The pane's controls after its own stop: Retry, Repair… and Show…. */
  def focusStops: Vector[FocusStop] =
    val v = vm
    Option
      .when(v.retry)(FocusStop(A11yRole.Button, SourcesText(SourcesTextId.RetryRead)))
      .toVector ++
      v.missing.toVector.flatMap(m =>
        Vector(FocusStop(A11yRole.Button, m.repair), FocusStop(A11yRole.Button, m.showTrials))
      )

  def sync(m: AppModel): Unit =
    if !disposed then
      val (next, effects) = SourcesPane.sync(pane, m)
      pane = next
      perform(effects)
      view.render(SourcesVM.of(pane, m))

  def dispatch(intent: SourcesIntent): Unit =
    if !disposed then
      val (next, effects) = SourcesPane.update(pane, model(), intent)
      pane = next
      perform(effects)
      view.render(vm)

  def dispose(): Unit = disposed = true

  private def later(intent: SourcesIntent): Unit = Platform.runLater(() => dispatch(intent))

  private def perform(effects: Vector[SourcesEffect]): Unit =
    effects.foreach {
      case SourcesEffect.ReadRegistry(spec, ask) =>
        displays.read(spec, r => later(SourcesIntent.RegistryRead(spec.id, ask, r)))
      case SourcesEffect.App(intent)           => app(intent)
      case SourcesEffect.Locate(dataset, file) =>
        project match
          case None =>
            dispatch(
              SourcesIntent.NotLocated(file, SourcesText(SourcesTextId.RepairNeedsProject))
            )
          case Some(port) =>
            files.locate(
              file,
              {
                case Left(reason)               => later(SourcesIntent.NotLocated(file, reason))
                case Right(None)                => ()
                case Right(Some((name, bytes))) =>
                  port.importInput(
                    InputKind.StimulusImage,
                    name.value,
                    bytes,
                    {
                      case Left(reason) => later(SourcesIntent.NotLocated(file, reason))
                      case Right(())    =>
                        later(
                          SourcesIntent.Located(dataset, file, name, ByteDigest.sha256(bytes))
                        )
                    }
                  )
              }
            )
    }

object SourcesPaneHost:
  val stylesheetResource: String = s"${TokenFiles.resourceDirectory}/studio-sources.css"

/** The Sources pane's nodes: it binds a [[SourcesVM]] and sends
  * [[SourcesIntent]]s. FX thread only.
  */
final class SourcesView(dispatch: SourcesIntent => Unit):
  private def label(classes: String*): Label =
    val l = Label()
    l.getStyleClass.addAll(classes*)
    l

  private def button(text: String, intent: SourcesIntent): Button =
    val b = Button(text)
    b.setAccessibleText(text)
    b.setMnemonicParsing(false)
    b.getStyleClass.addAll("sources-button", "t11")
    b.setOnAction(_ => dispatch(intent))
    b

  val empty: Label  = label("sources-empty", "t12")
  val sources: VBox = VBox()
  sources.getStyleClass.add("sources-list")
  val title: Label    = label("sources-title", "lbl")
  val kinds: GridPane = GridPane()
  kinds.getStyleClass.addAll("sources-kinds", "t11")
  kinds.setHgap(6.0)
  kinds.setVgap(5.0)
  val status: Label = label("sources-status", "t11")
  status.setWrapText(true)
  val retry: Button       = button(SourcesText(SourcesTextId.RetryRead), SourcesIntent.Retry)
  val missingTitle: Label = label("sources-missing-title", "t12")
  val missingBody: Label  = label("sources-missing-body", "t11")
  missingBody.setWrapText(true)
  missingBody.setMinHeight(Region.USE_PREF_SIZE)
  val repair: Button     = button("", SourcesIntent.Repair)
  val showTrials: Button = button("", SourcesIntent.ShowTrials)
  private val missingRow = HBox(6.0, repair, showTrials)
  val missing: VBox = VBox(6.0, HBox(6.0, glyph(None), missingTitle), missingBody, missingRow)
  missing.getStyleClass.add("sources-missing")
  val note: Label = label("sources-note", "t11")
  note.setWrapText(true)

  val node: VBox = VBox(empty, sources, title, kinds, status, retry, missing, note)
  node.getStyleClass.add("sources-panel")
  Option(getClass.getClassLoader.getResource(SourcesPaneHost.stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  /** The rows of "What each trial displayed", as shown: label and count. */
  def kindRows: Vector[(String, String)] =
    import scala.jdk.CollectionConverters.*
    kinds.getChildren.asScala.toVector
      .collect { case l: Label => l.getText }
      .grouped(2)
      .collect { case Vector(a, b) => (a, b) }
      .toVector

  /** The source cards' lines, as shown. */
  def sourceLines: Vector[Vector[String]] =
    import scala.jdk.CollectionConverters.*
    sources.getChildren.asScala.toVector.map {
      case box: VBox =>
        box.lookupAll(".label").asScala.toVector.collect { case l: Label => l.getText }
      case _ => Vector.empty
    }

  def render(vm: SourcesVM): Unit =
    show(empty, vm.empty)
    sources.getChildren.setAll(vm.sources.map { s =>
      val name = label("sources-name", "mono", "t12")
      name.setText(s.name)
      val kind = label("sources-kind", "t11")
      kind.setText(s.kind)
      val spacer = Region()
      HBox.setHgrow(spacer, javafx.scene.layout.Priority.ALWAYS)
      val head = HBox(name, spacer, kind)
      head.setAlignment(Pos.CENTER_LEFT)
      val lines = s.count.toVector.map { c =>
        val l = label("sources-count", "mono", "t11"); l.setText(c); l
      } :+ { val l = label("sources-stored", "mono", "t11"); l.setText(s.stored); l }
      val card = VBox((head +: lines)*)
      card.getStyleClass.add("sources-card")
      card
    }*)
    title.setText(vm.displaysTitle)
    kinds.getChildren.clear()
    vm.displays.zipWithIndex.foreach { (d, i) =>
      val text = label("sources-kind-label")
      text.setText(d.label)
      val count = label("sources-kind-count", "mono")
      count.setText(d.count)
      if !d.shown then
        text.getStyleClass.add("sources-dim")
        count.getStyleClass.add("sources-dim"): Unit
      kinds.add(glyph(Some(d.kinds.head).filter(_ => d.shown)), 0, i)
      kinds.add(text, 1, i)
      kinds.add(count, 2, i)
    }
    show(status, vm.status)
    retry.setVisible(vm.retry)
    retry.setManaged(vm.retry)
    vm.missing match
      case Some(m) =>
        missingTitle.setText(m.title)
        missingBody.setText(m.body)
        repair.setText(m.repair)
        repair.setAccessibleText(m.repair)
        showTrials.setText(m.showTrials)
        showTrials.setAccessibleText(m.showTrials)
        missing.setVisible(true)
        missing.setManaged(true)
      case None =>
        missing.setVisible(false)
        missing.setManaged(false)
    show(note, vm.note)

  private def show(l: Label, text: Option[String]): Unit =
    l.setText(text.getOrElse(""))
    l.setVisible(text.isDefined)
    l.setManaged(text.isDefined)

  /** A 14×11 swatch of a display kind; `None` is the hatched missing asset. */
  private def glyph(kind: Option[DisplayKind]): Region =
    val r = Region()
    r.getStyleClass.add("sources-glyph")
    r.getStyleClass.add(kind.fold("sources-glyph-missing") {
      case DisplayKind.Image                  => "sources-glyph-image"
      case DisplayKind.BlankWithFixationCross => "sources-glyph-cross"
      case _                                  => "sources-glyph-blank"
    })
    r.setMinSize(14.0, 11.0)
    r.setPrefSize(14.0, 11.0)
    r.setMaxSize(14.0, 11.0)
    r
