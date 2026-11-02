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

package eyes4s.studio.desktop.locations

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.layout.{PaneId, StudioLayouts}
import eyes4s.studio.app.locations.SpatialContext
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.core.artifacts.AdmittedLocations
import eyes4s.studio.core.backend.BackendError
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.real.RealStudyBackend
import eyes4s.studio.desktop.runtime.StudioSession
import eyes4s.results.{LocationReport, LocationTrialMetadata, LocationTripleOutcome}
import javafx.application.Platform
import javafx.beans.property.ReadOnlyStringWrapper
import javafx.scene.Node
import javafx.scene.control.*
import javafx.scene.layout.{Priority, VBox}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipOutputStream}
import scala.jdk.CollectionConverters.*

/** Spatial scientific views inside the normal document's four perspectives. */
final class SpatialWorkflowHost(
    model: () => AppModel,
    dispatch: Intent => Unit,
    session: StudioSession
):
  private def label(text: String): Label =
    val result = Label(text)
    result.setWrapText(true)
    result.setMinHeight(javafx.scene.layout.Region.USE_PREF_SIZE)
    result.getStyleClass.add("t12")
    result
  private def box(nodes: Node*): VBox =
    val result = VBox(12, nodes*)
    result.setPadding(javafx.geometry.Insets(16))
    result
  private def scroll(content: Node): ScrollPane =
    val result = ScrollPane(content)
    result.setFitToWidth(true)
    result
  private def button(text: String)(action: => Unit): Button =
    val result = Button(text)
    result.setOnAction(_ => action)
    result
  private val profile =
    "FIGRIM spatial profile: 1000 × 1000 pixels, 10 × 10 uniform count bins, cosine similarity. D is matched cosine minus control cosine."
  private val limits =
    "Descriptive, equal-query summaries. Observer slots are source labels, not participant identities. No timing, person-level weighting, standard errors or confidence intervals are inferred."
  private var disposed                                                       = false
  private var dataSerial                                                     = 0L
  private var runSerial                                                      = 0L
  private var requestedData: Option[LocationDatasetRevision]                 = None
  private var admitted: Option[AdmittedLocations]                            = None
  private var requestedRun: Option[RunRef]                                   = None
  private var result: Option[(RealStudyBackend.LocationRun, LocationReport)] = None
  private var dataMessage   = "Select a spatial dataset."
  private var resultMessage = "Select a completed run, or run the analysis."
  private var exporting     = false

  val name: TextField = TextField("Spatial comparison")
  name.setAccessibleText("Spatial analysis name")
  val saveAnalysis: Button   = button("Save analysis")(save())
  val run: Button            = button("Save and run")(runAnalysis())
  val createAnalysis: Button = button("Create analysis")(save())
  val exportReport: Button   = button("Export CSV bundle…")(exportBundle())
  val sources                = box()
  val admission              = box()
  val analyses               = box()
  val preflight              = box()
  val runs                   = box()
  val summary                = box()
  val provenance             = box()
  val report                 = box()
  val reportNav              = box(label("Spatial comparison"), exportReport)
  private val recipe         =
    box(label(profile), label(limits), label("Analysis name"), name, saveAnalysis)
  private val profileView = box(
    label(profile),
    label(
      "Coordinates are pixels with a downward y axis. Every source ordinal and exclusion remains in the saved ledgers."
    )
  )
  private val methods = box(
    label(profile),
    label(limits),
    label(
      "The report retains explicit focal, matched and control identities. Missing phases, excluded locations and unpaired retrievals are counted separately from comparison failures."
    )
  )
  val scores: TableView[LocationTripleOutcome] = TableView()
  scores.setAccessibleText("Spatial query scores")
  scores.setPlaceholder(label("No stored scores loaded."))
  private def column(title: String, width: Double)(
      value: LocationTripleOutcome => String
  ): Unit =
    val col = TableColumn[LocationTripleOutcome, String](title)
    col.setPrefWidth(width)
    col.setCellValueFactory(cell => ReadOnlyStringWrapper(value(cell.getValue)))
    scores.getColumns.add(col): Unit
  private def score(value: Either[eyes4s.plan.LocationRunError, Double]): String =
    value.fold(_.message, eyes4s.results.DecimalText.render)
  column("Query", 115)(_.query)
  column("Focal", 130)(_.focal.value)
  column("Matched", 130)(_.matched.value)
  column("Control", 130)(_.control.value)
  column("Matched cosine", 140)(r => score(r.matchedCosine))
  column("Control cosine", 140)(r => score(r.controlCosine))
  column("D", 130)(r => score(r.difference))
  VBox.setVgrow(scores, Priority.ALWAYS)

  val nodes: Vector[(PaneId, Node)] = Vector(
    StudioLayouts.spatialSources    -> scroll(sources),
    StudioLayouts.spatialData       -> scroll(admission),
    StudioLayouts.spatialProfile    -> scroll(profileView),
    StudioLayouts.spatialAnalyses   -> scroll(analyses),
    StudioLayouts.spatialRecipe     -> scroll(recipe),
    StudioLayouts.spatialPreflight  -> scroll(preflight),
    StudioLayouts.spatialRuns       -> scroll(runs),
    StudioLayouts.spatialSummary    -> scroll(summary),
    StudioLayouts.spatialQueries    -> scores,
    StudioLayouts.spatialProvenance -> scroll(provenance),
    StudioLayouts.spatialReportNav  -> scroll(reportNav),
    StudioLayouts.spatialReport     -> scroll(report),
    StudioLayouts.spatialMethods    -> scroll(methods)
  )

  private def native[A](
      work: RealStudyBackend[IO] => IO[Either[BackendError, A]]
  ): IO[Either[String, A]] =
    session.rawBackend match
      case backend: RealStudyBackend[IO] => work(backend).map(_.leftMap(_.message))
      case _ => IO.pure(Left("Spatial analysis requires a native project session."))

  private def dataset(m: AppModel): Option[LocationDatasetRevision] =
    if m.perspective == Perspective.Data then SpatialContext.data(m)
    else SpatialContext.analysisData(m)

  def sync(m: AppModel): Unit = if !disposed then
    if m.perspective == Perspective.Data || m.perspective == Perspective.Analysis then
      val selected = dataset(m)
      if selected != requestedData then
        dataSerial += 1
        val serial = dataSerial
        requestedData = selected
        admitted = None
        dataMessage =
          selected.fold("Select a spatial dataset.")(_ => "Checking stored sources…")
        selected.foreach { spec =>
          val snapshot = m.document
          session.run(session.synchronize(snapshot).flatMap {
            case Left(error) => IO.pure(Left(error.message))
            case Right(_)    => native(_.locationAdmission(spec.id))
          }) { answer =>
            Platform.runLater(() =>
              if !disposed && serial == dataSerial then
                answer.leftMap(_.getMessage).flatten match
                  case Right(value) => admitted = Some(value); dataMessage = "Sources verified"
                  case Left(error)  => dataMessage = error
                render(model())
            )
          }
        }
    val shown = SpatialContext.shown(m)
    if shown != requestedRun then
      runSerial += 1
      val serial = runSerial
      requestedRun = shown
      result = None
      scores.getItems.clear()
      resultMessage = shown.fold("Select a completed run, or run the analysis.")(r =>
        s"Reading stored ${r.id.label}…"
      )
      shown.foreach { ref =>
        val read = native { backend =>
          backend.locationResult(ref.id).flatMap {
            case Left(BackendError.ResultRestoring(_)) =>
              backend.awaitRestore(ref.id).flatMap {
                case Left(error) => IO.pure(Left(error))
                case Right(_)    => backend.locationResult(ref.id)
              }
            case other => IO.pure(other)
          }
        }.map(
          _.flatMap(value =>
            LocationReport
              .of(
                value.admitted.plan,
                value.result,
                Some(
                  value.admitted.imported.trialMetadata.map(m =>
                    LocationTrialMetadata(
                      m.id,
                      m.imageIndex,
                      m.observerSlot,
                      m.image,
                      m.imageFamily,
                      m.phase,
                      m.category,
                      m.sdt
                    )
                  )
                )
              )
              .leftMap(_.message)
              .map(value -> _)
          )
        )
        session.run(read) { answer =>
          Platform.runLater(() =>
            if !disposed && serial == runSerial then
              answer.leftMap(_.getMessage).flatten match
                case Right(value) =>
                  result = Some(value)
                  resultMessage = s"${ref.id.label.capitalize} · ${
                      if ref.archive.isInstanceOf[CoreBinding.Bound[?]] then "stored results"
                      else "computed results; archive not yet confirmed"
                    }"
                  scores.getItems.setAll(value._2.triples.asJava)
                case Left(error) => resultMessage = error
              render(model())
          )
        }
      }
    render(m)

  private var countCache = Map.empty[LocationDatasetRevision, String]
  private def counts(value: AdmittedLocations): String = countCache.getOrElse(
    value.dataset, {
      val text = describeCounts(value)
      countCache = (countCache + (value.dataset -> text)).take(3)
      text
    }
  )
  private def describeCounts(value: AdmittedLocations): String =
    val imported = value.imported
    val excluded = imported.locationLedger.count(!_.admitted)
    val missing  = imported.phaseLedger.count(_.gazeState == "missing_phase")
    s"${imported.input.trials.size} admitted trials · ${imported.queries.size} explicit comparison queries\n${imported.locationLedger.size - excluded} admitted locations · $excluded excluded locations\n$missing missing phases · ${imported.unpairedRetrievals.size} unpaired retrieval records"

  private def render(m: AppModel): Unit =
    sources.getChildren.setAll(
      (Vector(label("Project datasets")) ++ m.document.datasets.map(d =>
        button(s"${d.id.label} · ${d.kind}")(
          dispatch(Intent.Navigate(Location(Perspective.Data, Vector(Place.Dataset(d.id)))))
        )
      ) ++ requestedData.toVector.flatMap(
        _.sourceFiles.map(s => label(s"${s.role.label}\n${s.path.value}"))
      )).asJava
    )
    admission.getChildren.setAll(
      (Vector(label(dataMessage)) ++ admitted.toVector.map(v => label(counts(v))) ++ Vector(
        createAnalysis
      )).asJava
    )
    analyses.getChildren.setAll(
      (Vector(label("Saved analyses")) ++ m.document.analyses.map(a =>
        button(s"${a.id.label} · ${a.revisionName.value}")(
          dispatch(
            Intent.Navigate(
              Location(
                Perspective.Analysis,
                eyes4s.studio.app.analysis.AnalysisSelection.trail(m.document, a.id)
              )
            )
          )
        )
      )).asJava
    )
    val block = requestedData.flatMap(d => m.sources.block(d.id))
    val busy  = m.jobs.jobs.exists(!_.phase.isTerminal)
    val ready = admitted.exists(_.plan.queries.nonEmpty) && block.isEmpty && !busy
    saveAnalysis.setDisable(admitted.isEmpty)
    createAnalysis.setDisable(admitted.isEmpty)
    run.setText(if SpatialContext.analysis(m).isDefined then "Run analysis" else "Save and run")
    run.setDisable(!ready)
    preflight.getChildren.setAll(
      (Vector(
        label(if ready then "Ready" else if busy then "A run is active" else "Not ready"),
        label(dataMessage)
      ) ++
        admitted.toVector.map(v => label(counts(v))) ++ block.toVector.map(b =>
          label(s"Stored source check: $b")
        ) ++ Vector(run)).asJava
    )
    runs.getChildren.setAll(
      (Vector(label("Completed runs")) ++ m.document.runs
        .filter(_.state == RunLifecycle.Completed)
        .map(r =>
          button(
            s"${r.id.label.capitalize} · ${m.document.analysisDefinition(r.analysis).map(_.kind).fold("Unknown")(_.toString)}"
          )(dispatch(Intent.Dispatch(Command.ShowRun(Some(r.id)))))
        )).asJava
    )
    val description = result.toVector.flatMap { (value, reportValue) =>
      val s = reportValue.summary
      Vector(
        label(s"${s.successfulQueries} successful queries · ${s.failedQueries} failed queries"),
        label(
          s"Mean D: ${s.meanDifference.fold("Unavailable")(eyes4s.results.DecimalText.render)}"
        ),
        label(counts(value.admitted)),
        label(limits)
      )
    }
    summary.getChildren.setAll((Vector(label(resultMessage)) ++ description).asJava)
    report.getChildren.setAll(
      (Vector(label(resultMessage), label(profile)) ++ result.toVector.flatMap {
        (value, reportValue) =>
          val s = reportValue.summary
          Vector(
            label(
              s"Equal-query mean D: ${s.meanDifference.fold("Unavailable")(eyes4s.results.DecimalText.render)}"
            ),
            label(
              s"${s.successfulQueries} successful queries; ${s.failedQueries} failed queries."
            ),
            label(counts(value.admitted)),
            label(limits)
          )
      }).asJava
    )
    provenance.getChildren.setAll(
      (Vector(label("Stored scientific identities")) ++ result.toVector.flatMap { (value, _) =>
        Vector(
          label(value.analysis.plan.render),
          label(value.analysis.recipe.input.render),
          label(s"Source bundle: ${value.admitted.imported.sourceDigest}"),
          label(value.origin.toString)
        )
      }).asJava
    )
    exportReport.setDisable(result.isEmpty || exporting)

  private def save(): Unit = admitted.foreach { value =>
    val before   = model().document
    val existing = SpatialContext.analysis(model()).filter(_.dataset == value.dataset.id)
    val family   = existing
      .flatMap(a =>
        before.familyOf(a.id).flatMap(id => before.analysisFamilies.flatMap(_.family(id)))
      )
      .map(Right(_))
      .getOrElse(
        before.nextFamilyId
          .leftMap(_.message)
          .flatMap(id => AnalysisFamily.of(id, name.getText).leftMap(_.message))
      )
    val command = for
      f     <- family
      title <- RevisionName.of(name.getText).leftMap(_.message)
    yield Command.SaveLocationAnalysis(f, value.dataset.id, value.planCanonical, title, profile)
    command match
      case Left(error) => dataMessage = error; render(model())
      case Right(save) =>
        dispatch(Intent.Dispatch(save))
        model().document.analyses.lastOption
          .filterNot(a => before.analyses.exists(_.id == a.id))
          .foreach(a =>
            dispatch(
              Intent.Navigate(
                Location(
                  Perspective.Analysis,
                  eyes4s.studio.app.analysis.AnalysisSelection.trail(model().document, a.id)
                )
              )
            )
          )
  }

  private def runAnalysis(): Unit = admitted.foreach { value =>
    def matching = SpatialContext.analysis(model()).filter(_.dataset == value.dataset.id)
    if matching.isEmpty then save()
    matching.foreach(a => dispatch(Intent.Dispatch(Command.ReserveLocationRun(a.id))))
  }

  private def exportBundle(): Unit = result.foreach { (value, reportValue) =>
    val chooser = javafx.stage.FileChooser()
    chooser.setTitle("Export spatial report")
    chooser.setInitialFileName("spatial-report.zip")
    chooser.getExtensionFilters.add(
      javafx.stage.FileChooser.ExtensionFilter("CSV bundle", "*.zip")
    )
    Option(chooser.showSaveDialog(Option(exportReport.getScene).map(_.getWindow).orNull))
      .foreach { file =>
        exporting = true
        render(model())
        session.run(IO.blocking {
          import eyes4s.io.csv
          val target = file.toPath.toAbsolutePath
          val temp   = Files.createTempFile(target.getParent, ".spatial-report-", ".zip")
          try
            val zip = ZipOutputStream(Files.newOutputStream(temp))
            try
              def entry(name: String, bytes: Array[Byte]): Unit =
                zip.putNextEntry(ZipEntry(name)); zip.write(bytes); zip.closeEntry()
              reportValue.tables.foreach(t =>
                entry(s"${t.family.toString}.csv", t.csv.encode.getBytes(UTF_8))
              )
              value.admitted.sourceFiles.foreach((role, bytes) =>
                entry(s"sources/${role.toString}.txt", bytes.toArray)
              )
              entry(
                "methods.txt",
                s"$profile\n$limits\n${counts(value.admitted)}\nPlan: ${value.analysis.plan.render}\nInput: ${value.analysis.recipe.input.render}\n"
                  .getBytes(UTF_8)
              )
            finally zip.close()
            Files.move(temp, target, StandardCopyOption.REPLACE_EXISTING)
            ()
          finally { Files.deleteIfExists(temp): Unit }
        }) { answer =>
          Platform.runLater(() =>
            if !disposed then
              exporting = false
              resultMessage =
                answer.fold(e => s"Export failed: ${e.getMessage}", _ => "CSV bundle exported")
              render(model())
          )
        }
      }
  }

  def dispose(): Unit =
    disposed = true
    dataSerial += 1
    runSerial += 1
