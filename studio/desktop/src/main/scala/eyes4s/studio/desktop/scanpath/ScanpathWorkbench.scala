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

package eyes4s.studio.desktop.scanpath

import eyes4s.studio.core.scanpath.ScanpathStudy
import io.circe.{Json, parser}
import java.nio.file.{Files, Path, StandardOpenOption}
import javafx.beans.property.ReadOnlyStringWrapper
import javafx.concurrent.Task
import javafx.scene.Scene
import javafx.scene.control.{Button, ComboBox, Label, TableColumn, TableView}
import javafx.scene.layout.{HBox, Priority, VBox}
import javafx.stage.{FileChooser, Stage, Window}
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** A separate scanpath workspace: explicit input/schedule, physical-screen
  * normalization, five components and RequireAll accounting. Saved workspaces
  * carry both the input and the exact result, verified on reopening.
  */
final class ScanpathWorkbench(
    openFile: () => Option[Path],
    saveFile: () => Option[Path],
    choose: Option[(Boolean, Option[Path] => Unit) => Unit] = None
):
  val open  = Button("Open scanpath request or workspace…")
  val run   = Button("Run MultiMatch")
  val save  = Button("Save workspace…")
  val phase = ComboBox[String]()
  phase.setAccessibleText("Query phase")
  val component = ComboBox[String]()
  component.setAccessibleText("MultiMatch component")
  component.getItems.setAll(ScanpathStudy.components.asJava)
  component.setValue("shape")
  val inputLabel = Label("No scanpath input loaded")
  val status     = Label("Open a scanpath request to begin.")
  status.setWrapText(true)
  Vector(open, run, phase, save).foreach(control =>
    eyes4s.studio.desktop.shell.DisabledReason.attach(control, status)
  )
  val queryTable = TableView[Vector[String]]()
  Vector(
    "Query",
    "Participant",
    "Phase",
    "Matched",
    "Mean control",
    "Difference",
    "Unavailable pairs"
  ).zipWithIndex.foreach { (title, index) =>
    val column = TableColumn[Vector[String], String](title)
    column.setCellValueFactory(row => ReadOnlyStringWrapper(row.getValue.apply(index)))
    column.setPrefWidth(if index == 0 then 175 else 125)
    queryTable.getColumns.add(column)
  }
  val pairTable = TableView[Vector[String]]()
  Vector("Query", "Reference", "Design", "Score", "Failure").zipWithIndex.foreach {
    (title, index) =>
      val column = TableColumn[Vector[String], String](title)
      column.setCellValueFactory(row => ReadOnlyStringWrapper(row.getValue.apply(index)))
      column.setPrefWidth(if index == 4 then 400 else 150)
      pairTable.getColumns.add(column)
  }
  private val convention = Label(
    "MultiMatch · ungrouped · physical screen diagonal · minimum 3 fixations per path · RequireAll (any failed pair makes its query unavailable). Saved values retain full precision; tables show six decimals."
  )
  convention.setWrapText(true)
  val node = VBox(
    10,
    convention,
    inputLabel,
    HBox(10, open, Label("Phase"), phase, run, save),
    HBox(10, Label("Component"), component),
    status,
    queryTable,
    pairTable
  )
  node.setStyle("-fx-padding: 16;")
  VBox.setVgrow(queryTable, Priority.ALWAYS)
  VBox.setVgrow(pairTable, Priority.ALWAYS)
  private var study: Option[ScanpathStudy]               = None
  private var completed: Option[Json]                    = None
  private var active: Option[Task[Either[String, Unit]]] = None
  private var choosing                                   = false
  private var closed                                     = false
  def result: Option[Json]                               = completed
  def busy: Boolean                                      = active.nonEmpty || choosing
  def queryTexts: Vector[Vector[String]]                 = queryTable.getItems.asScala.toVector
  private def controls(): Unit                           =
    open.setDisable(busy)
    run.setDisable(busy || study.isEmpty)
    phase.setDisable(busy || study.isEmpty)
    save.setDisable(busy || completed.isEmpty)
  private def show(): Unit =
    val name                               = component.getValue
    def text(j: Json, key: String): String = j.hcursor.get[String](key).getOrElse("")
    def number(j: Json): String            =
      j.asNumber.map(_.toDouble).fold("unavailable")(ScanpathWorkbench.display)
    val queries =
      completed.toVector.flatMap(_.hcursor.get[Vector[Json]]("queries").getOrElse(Vector.empty))
    queryTable.getItems.setAll(queries.map { q =>
      val values = q.hcursor.downField("values").downField(name)
      Vector(text(q, "id"), text(q, "participant"), text(q, "phase")) ++
        Vector("observed", "baseline", "difference").map(k =>
          number(values.downField(k).focus.getOrElse(Json.Null))
        ) :+
        q.hcursor.get[Vector[Json]]("unavailable_pairs").getOrElse(Vector.empty).size.toString
    }.asJava)
    val pairs =
      completed.toVector.flatMap(_.hcursor.get[Vector[Json]]("pairs").getOrElse(Vector.empty))
    pairTable.getItems.setAll(pairs.map { p =>
      Vector(
        text(p, "source"),
        text(p, "reference"),
        text(p, "kind"),
        number(p.hcursor.downField("values").downField(name).focus.getOrElse(Json.Null)),
        text(p, "error")
      )
    }.asJava)
    if completed.nonEmpty then
      val unavailable = queries.count(_.hcursor.downField("values").focus.forall(_.isNull))
      status.setText(
        s"${queries.size} queries; ${queries.size - unavailable} contributing; $unavailable unavailable. ${pairs.size} pairs; ${pairs.count(_.hcursor.downField("error").succeeded)} failed. All five components retained."
      )
  private def background(work: => Either[String, Unit])(done: => Unit): Unit =
    val task = new Task[Either[String, Unit]]:
      def call(): Either[String, Unit] = Try(work).toEither.left.map(_.toString).flatten
    active = Some(task)
    status.setText("Working…")
    controls()
    task.setOnSucceeded { _ =>
      active = None
      if !closed then
        task.getValue match
          case Left(error) => status.setText(error)
          case Right(_)    => done
        controls()
    }
    task.setOnFailed { _ =>
      active = None
      if !closed then
        status.setText(
          Option(task.getException).fold("Scanpath computation failed")(_.toString)
        )
        controls()
    }
    val worker = Thread(task, "studio-scanpath")
    worker.setDaemon(true)
    worker.start()
  private def choosePath(save: Boolean)(use: Path => Unit): Unit =
    if !closed && !busy then
      val previousStatus = status.getText
      choosing = true
      status.setText(
        eyes4s.studio.app.text.RecipeText(
          eyes4s.studio.app.text.RecipeTextId.ScanpathChoosing
        )
      )
      controls()
      def selected(path: Option[Path]): Unit =
        choosing = false
        if !closed then
          status.setText(previousStatus)
          controls()
          path.foreach(use)
      choose match
        case None       => selected(if save then saveFile() else openFile())
        case Some(pick) =>
          Try(pick(save, selected)).failed.foreach { error =>
            selected(None)
            status.setText(error.toString)
          }

  open.setOnAction { _ =>
    choosePath(false) { path =>
      var loaded: Option[(ScanpathStudy, Option[Json], String)] = None
      background {
        for
          json <- parser.parse(Files.readString(path)).left.map(_.message)
          isWorkspace = json.hcursor.get[Int]("scanpath_workspace_version").toOption
          _ <- Either.cond(
            isWorkspace.forall(_ == 1),
            (),
            "Unsupported scanpath workspace version"
          )
          input <-
            if isWorkspace.isDefined then json.hcursor.get[Json]("request").left.map(_.message)
            else Right(json)
          parsed <- ScanpathStudy.read(input).left.map(_.message)
          held   <-
            if isWorkspace.isDefined then
              json.hcursor.get[Json]("result").left.map(_.message).map(Some(_))
            else Right(None)
          selected = held
            .flatMap(_.hcursor.get[String]("phase").toOption)
            .getOrElse(parsed.phases.head)
          _ <- held.fold[Either[String, Unit]](Right(()))(saved =>
            parsed
              .run(selected)
              .left
              .map(_.message)
              .flatMap(actual =>
                Either.cond(
                  actual == saved,
                  (),
                  "Saved scanpath result differs from its input and declared computation"
                )
              )
          )
        yield loaded = Some((parsed, held, selected))
      } {
        loaded.foreach { (parsed, held, selected) =>
          study = Some(parsed)
          val c = parsed.request.hcursor
          inputLabel.setText(
            s"${path.getFileName} · physical screen ${c.get[Int]("screen_width_px").getOrElse(0)} × ${c.get[Int]("screen_height_px").getOrElse(0)} px"
          )
          completed = None
          phase.getItems.setAll(parsed.phases.asJava)
          phase.setValue(selected)
          completed = held
          status.setText("Request loaded. Choose a phase, then Run MultiMatch.")
          show()
        }
      }
    }
  }
  run.setOnAction { _ =>
    study.foreach { captured =>
      val selected             = phase.getValue
      var output: Option[Json] = None
      background(
        captured.run(selected).left.map(_.message).map(value => output = Some(value))
      ) {
        completed = output
        show()
      }
    }
  }
  save.setOnAction { _ =>
    for input <- study; output <- completed do
      choosePath(true) { path =>
        val document = Json.obj(
          "scanpath_workspace_version" -> Json.fromInt(1),
          "request"                    -> input.request,
          "result"                     -> output
        )
        background {
          Try(
            Files.writeString(path, document.noSpaces, StandardOpenOption.CREATE_NEW)
          ).toEither.left.map(_.toString).map(_ => ())
        } {
          status.setText(
            "Workspace saved with its input, exact pair scores and all five components."
          )
        }
      }
  }
  component.setOnAction(_ => show())
  phase.setOnAction { _ =>
    if completed.exists(_.hcursor.get[String]("phase").toOption != Option(phase.getValue)) then
      completed = None
      show()
      status.setText("Phase changed. Run MultiMatch to compute these queries.")
      controls()
  }
  controls()
  def dispose(): Unit =
    closed = true
    choosing = false
    active.foreach(_.cancel())
    active = None

object ScanpathWorkbench:
  def display(value: Double): String =
    java.lang.String.format(java.util.Locale.ROOT, "%.6f", Double.box(value))
  def open(
      owner: Window,
      platform: Option[eyes4s.studio.core.platform.Platform[cats.effect.IO]] = None
  ): Stage =
    import cats.effect.unsafe.implicits.global
    import eyes4s.studio.app.text.{RecipeText, RecipeTextId}
    import eyes4s.studio.core.platform.{FileKind, FileRequest}
    val stage = Stage()
    stage.initOwner(owner)
    stage.setTitle(RecipeText(RecipeTextId.ScanpathWorkspaceTitle))
    val chooser = FileChooser()
    chooser.getExtensionFilters.add(
      FileChooser.ExtensionFilter(RecipeText(RecipeTextId.ScanpathFileKind), "*.json")
    )
    var view: Option[ScanpathWorkbench] = None
    val choose                          = platform.map(host =>
      (save: Boolean, answer: Option[Path] => Unit) =>
        val request = FileRequest(
          RecipeText(RecipeTextId.ScanpathFileKind),
          FileKind
            .of(RecipeText(RecipeTextId.ScanpathFileKind), Vector("json"))
            .toOption
            .toVector,
          None
        )
        val selected =
          if save then host.dialogs.chooseSave(request) else host.dialogs.chooseOpen(request)
        selected.unsafeRunAsync(result =>
          javafx.application.Platform.runLater { () =>
            if stage.isShowing then
              result match
                case Left(error) =>
                  answer(None)
                  view.foreach(_.status.setText(error.toString))
                case Right(path) =>
                  val decoded =
                    scala.util.Try(path.map(p => java.nio.file.Paths.get(p.value))).toEither
                  decoded match
                    case Left(error) =>
                      answer(None)
                      view.foreach(_.status.setText(error.toString))
                    case Right(value) => answer(value)
          }
        )
    )
    val content = ScanpathWorkbench(
      () => Option(chooser.showOpenDialog(stage)).map(_.toPath),
      () => Option(chooser.showSaveDialog(stage)).map(_.toPath),
      choose
    )
    view = Some(content)
    stage.setScene(Scene(content.node, 1150, 850))
    stage.setOnHidden(_ => content.dispose())
    stage.show()
    stage
