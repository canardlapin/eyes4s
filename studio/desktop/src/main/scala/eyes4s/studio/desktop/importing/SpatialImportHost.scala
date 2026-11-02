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

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import eyes4s.codec.{ByteDigest, CanonicalDigest}
import eyes4s.studio.app.Intent
import eyes4s.studio.core.artifacts.LocationNativePackage
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.importing.StreamedSource
import eyes4s.studio.core.platform.HostPath
import javafx.application.Platform
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.{HBox, Priority, Region, VBox}

import java.util.concurrent.CompletableFuture
import scala.concurrent.ExecutionContext

/** The five-file, location-only FIGRIM importer. It deliberately has no
  * timing controls: the checked bundle declares its fixed pixel frame, count
  * bins, cosine comparisons, and explicit triples in [[LocationNativePackage]].
  */
final class SpatialImportHost(
    document: () => StudioDocument,
    app: Intent => Unit,
    platform: ImportPlatform,
    close: () => Unit
):
  import SpatialImportHost.*

  private var selected  = Map.empty[SourceRole, Selected]
  private var admission = Option.empty[Admission]
  private var issue     = Option.empty[String]
  private var busy      = false
  private var disposed  = false
  private var epoch     = 0L
  private var serial    = 0L
  private var tasks     = Map.empty[Long, () => Unit]

  val view: SpatialImportView = SpatialImportView(choose, submit, () => close())
  render()

  def importCompleted: CompletableFuture[Unit] = lastImport
  private var lastImport                       = CompletableFuture.completedFuture(())

  private def reason(error: Throwable): String =
    Option(error.getMessage).getOrElse(error.toString)

  private def launch[A](io: IO[A])(done: Either[Throwable, A] => Unit): () => Unit =
    serial += 1
    val id               = serial
    val (future, cancel) = io.unsafeToFutureCancelable()
    val stop             = () => { cancel(); () }
    tasks += id -> stop
    future.onComplete(result =>
      Platform.runLater(() => {
        tasks -= id
        done(result.toEither)
      })
    )(using ExecutionContext.global)
    stop

  private def active(at: Long): Boolean = !disposed && epoch == at

  private def render(): Unit =
    if !disposed then view.render(selected, admission.map(_.summary), issue, busy)

  def dispose(): Unit = if !disposed then
    disposed = true
    epoch += 1
    tasks.values.foreach(_())
    tasks = Map.empty
    lastImport.cancel(false): Unit

  private def choose(role: SourceRole): Unit = if !disposed && !busy then
    busy = true
    issue = None
    render()
    val at = epoch
    val _  = launch(platform.chooseFile(role).flatMap {
      case Left(error)         => IO.pure(Left(error))
      case Right(None)         => IO.pure(Right(None))
      case Right(Some(chosen)) => read(role, chosen.path, chosen.name).map(_.map(Some(_)))
    }) {
      case Right(Right(Some(value))) if active(at) =>
        selected += role -> value
        admission = None
        busy = false
        validate(at)
      case Right(Right(None)) if active(at) =>
        busy = false
        render()
      case Right(Left(error)) if active(at) =>
        busy = false
        issue = Some(error)
        render()
      case Left(error) if active(at) =>
        busy = false
        issue = Some(reason(error))
        render()
      case _ => ()
    }

  private def read(
      role: SourceRole,
      path: HostPath,
      name: String
  ): IO[Either[String, Selected]] =
    SourcePath.of(name).leftMap(_.message) match
      case Left(error)       => IO.pure(Left(error))
      case Right(sourcePath) =>
        platform.files.readStream(path, StreamedSource.ChunkBytes).flatMap {
          case Left(error)   => IO.pure(Left(error.message))
          case Right(stream) =>
            StreamedSource
              .bytes[IO](name, stream)
              .map(_.leftMap(_.message).map { bytes =>
                val source = Source(role, sourcePath, ByteDigest.sha256(bytes), None)
                Selected(source, path, bytes)
              })
        }

  private def proposed(
      document: StudioDocument,
      selection: Map[SourceRole, Selected]
  ): Either[String, LocationDatasetRevision] =
    val id = DatasetRevision(document.datasets.lastOption.fold(1)(_.id.number + 1))
    LocationBundleSources
      .of(LocationBundleSources.roles.flatMap(selection.get).map(_.source))
      .leftMap(_.message)
      .map(sources =>
        LocationDatasetRevision(
          id,
          None,
          sources,
          LocationProfile.FigrimV1,
          LocationAdmissionDecision.Pending
        )
      )

  private def validate(at: Long): Unit =
    if LocationBundleSources.roles.forall(selected.contains) then
      busy = true
      render()
      val before   = document()
      val snapshot = selected
      val checked  = IO.delay(proposed(before, snapshot).flatMap { revision =>
        val files = LocationBundleSources.roles.map(role =>
          role -> IArray.genericWrapArray(snapshot(role).bytes).toIndexedSeq
        )
        LocationNativePackage
          .admit(revision, files)
          .leftMap(_.message)
          .map(accepted => revision.sources -> accepted)
      })
      val _ = launch(checked) {
        case Right(Right((sources, accepted))) if active(at) && document() == before =>
          admission = Some(
            Admission(
              accepted.inputCanonical,
              sources,
              Summary(
                accepted.plan.input.trials.size,
                accepted.plan.queries.size,
                accepted.imported.phaseLedger.count(_.gazeState == "missing_phase"),
                accepted.imported.unpairedRetrievals.size,
                accepted.imported.locationLedger.count(value => !value.admitted)
              )
            )
          )
          busy = false
          render()
        case Right(Right(_)) if active(at) =>
          busy = false
          issue = Some(
            "The project changed while the spatial bundle was checked; choose a source again."
          )
          render()
        case Right(Left(error)) if active(at) =>
          busy = false
          issue = Some(error)
          render()
        case Left(error) if active(at) =>
          busy = false
          issue = Some(reason(error))
          render()
        case _ => ()
      }
    else render()

  private def submit(): Unit = if !disposed && !busy then
    admission match
      case None =>
        issue = Some("Choose all five FIGRIM bundle files before importing.")
        render()
      case Some(accepted) =>
        busy = true
        issue = None
        render()
        val at     = epoch
        val before = document()
        val done   = CompletableFuture[Unit]()
        lastImport = done
        val _ = launch(
          LocationBundleSources.roles
            .traverse(role => platform.importInput(selected(role).source, selected(role).path))
            .map(_.sequence)
        ) {
          case Right(Right(_)) if active(at) && document() == before =>
            busy = false
            app(
              Intent.Dispatch(
                Command.AppendLocationDataset(
                  None,
                  accepted.sources,
                  LocationProfile.FigrimV1,
                  accepted.input
                )
              )
            )
            done.complete(())
            close()
          case Right(Right(_)) if active(at) =>
            busy = false
            issue = Some(
              "The project changed while sources were stored; no spatial dataset was appended."
            )
            done.complete(())
            render()
          case Right(Left(error)) if active(at) =>
            busy = false
            issue = Some(error)
            done.complete(())
            render()
          case Left(error) if active(at) =>
            busy = false
            issue = Some(reason(error))
            done.complete(())
            render()
          case _ => done.cancel(false): Unit
        }

object SpatialImportHost:
  private[importing] final case class Selected(
      source: Source,
      path: HostPath,
      bytes: IArray[Byte]
  )
  private[importing] final case class Admission(
      input: CanonicalDigest[LocationInputArtifact],
      sources: LocationBundleSources,
      summary: Summary
  )
  final case class Summary(maps: Int, queries: Int, missing: Int, unpaired: Int, excluded: Int)

/** JavaFX-only view for the fixed spatial bundle. */
final class SpatialImportView(
    choose: SourceRole => Unit,
    submit: () => Unit,
    cancel: () => Unit
):
  import SpatialImportHost.*

  val node: VBox = VBox(12)
  node.getStyleClass.add("import-wizard")
  private val heading = Label("Spatial locations — FIGRIM bundle")
  heading.getStyleClass.addAll("import-kind", "t11")
  private val note = Label(
    "Five checked source files. Fixed 1000 × 1000 px frame; 10 × 10 count bins; cosine comparison triples. No timing is imported."
  )
  note.setWrapText(true)
  note.getStyleClass.addAll("import-note", "t11")
  val files: Map[SourceRole, Button] = LocationBundleSources.roles.map { role =>
    val button = Button()
    button.getStyleClass.addAll("import-button", "t12")
    button.setOnAction(_ => choose(role))
    role -> button
  }.toMap
  val summary = Label()
  summary.getStyleClass.addAll("import-summary", "t12")
  val problem = Label()
  problem.getStyleClass.addAll("import-problem", "t12")
  val cancelButton = Button("Cancel")
  cancelButton.getStyleClass.addAll("import-button", "t12")
  cancelButton.setOnAction(_ => cancel())
  val commit = Button("Import FIGRIM bundle")
  commit.getStyleClass.addAll("import-button", "primary", "t12")
  commit.setOnAction(_ => submit())
  private val rows   = VBox(8)
  private val footer = HBox(8, problem, spacer(), cancelButton, commit)
  node.getChildren.setAll(heading, note, rows, summary, footer)
  VBox.setVgrow(rows, Priority.ALWAYS)

  def render(
      selected: Map[SourceRole, Selected],
      accepted: Option[Summary],
      error: Option[String],
      busy: Boolean
  ): Unit =
    rows.getChildren.setAll(LocationBundleSources.roles.map { role =>
      val button = files(role)
      button.setText(
        selected
          .get(role)
          .fold(s"Choose ${role.label}")(s => s"${role.label}: ${s.source.path.value}")
      )
      button.setDisable(busy)
      HBox(10, button)
    }*)
    summary.setText(
      accepted.fold("")(s =>
        s"${s.maps} maps · ${s.queries} comparison triples · ${s.missing} missing phases · ${s.unpaired} unpaired retrievals · ${s.excluded} excluded locations"
      )
    )
    summary.setVisible(accepted.nonEmpty)
    summary.setManaged(accepted.nonEmpty)
    problem.setText(error.getOrElse(""))
    problem.setVisible(error.nonEmpty)
    problem.setManaged(error.nonEmpty)
    commit.setDisable(busy || accepted.isEmpty)
    cancelButton.setDisable(busy)

  private def spacer(): Region =
    val value = Region()
    HBox.setHgrow(value, Priority.ALWAYS)
    value
