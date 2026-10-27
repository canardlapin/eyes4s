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

package eyes4s.studio.desktop.journey

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.plan.{FieldId, RawValue}
import eyes4s.studio.app.{Intent, Notice}
import eyes4s.studio.app.figures.BundleItem
import eyes4s.studio.app.compare.QueriesAnswer
import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.app.keys.{CommandId, CommandRegistry}
import eyes4s.studio.app.nav.{DataSection, Location, Place}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.figures.MethodsReads
import eyes4s.studio.core.platform.{HostPath, InMemoryPlatform}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}
import eyes4s.studio.desktop.{StudioStyles, StudioWindow}
import eyes4s.studio.desktop.harness.{FxStage, SnapshotScale, StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.importing.ImportWizardHost
import eyes4s.studio.desktop.platform.{FilePresetStore, JvmFileSystem}
import eyes4s.studio.desktop.project.ProjectLifecycleHost
import eyes4s.studio.desktop.shell.ShellFxSuite
import eyes4s.studio.desktop.trial.TrialViewStatus
import eyes4s.studio.core.importing.{ColumnChoice, GeometryField}
import io.circe.{Json, parser}
import io.circe.syntax.*
import javafx.scene.control.{Button, Menu, MenuItem}
import java.nio.file.{Files, Path}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** Opt-in production lifecycle, ordinary import/recipe controls, real exports and fresh-JVM readback.
  * The caller supplies external files and expected API results; no study is encoded in this suite.
  */
class PrivateProductionFxSuite extends ShellFxSuite:
  import PrivateProductionInputs.{get, write}
  private final case class Tolerance(absolute: Double, relative: Double):
    def approxEquals(a: Double, b: Double): Boolean =
      a.isFinite && b.isFinite && math
        .abs(a - b) <= absolute + relative * math.max(math.abs(a), math.abs(b))
  override val munitTimeout: Duration = 60.minutes
  private val ScoreTolerance          = Tolerance(absolute = 2e-12, relative = 2e-12)
  private def read(path: Path): Json  = get(parser.parse(Files.readString(path)))
  private def path(p: Path): HostPath = get(HostPath.of(p.toString))

  private def until(fx: FxStage, what: String, timeout: FiniteDuration = 15.minutes)(
      condition: => Boolean
  ): Unit =
    val deadline = System.nanoTime() + timeout.toNanos
    while !runOnFx(condition) do
      if System.nanoTime() > deadline then fail(s"Timed out: $what")
      Thread.sleep(50)
    fx.awaitLayout()
    println(s"[private-production] $what")

  private def invoke(host: ProjectLifecycleHost, id: CommandId): Unit = runOnFx {
    def items(item: MenuItem): Vector[MenuItem] = item match
      case m: Menu => m.getItems.asScala.toVector.flatMap(items)
      case other   => Vector(other)
    val command = host.window.get.shell.menus
      .flatMap(_.getItems.asScala)
      .flatMap(items)
      .find(_.getId == id.value)
      .getOrElse(fail(s"No command ${id.value}"))
    assert(!command.isDisable)
    command.fire()
  }

  private def capture(fx: FxStage, root: Path, name: String, dark: Boolean = false): Unit =
    fx.snapshot(if dark then StudioTheme.Dark else StudioTheme.Light, List(SnapshotScale.X1))
      .foreach(p => Files.copy(p, root.resolve(s"$name.png")))

  private def compareRows(found: Vector[QueryRow], expected: Vector[QueryRow]): Unit =
    assertEquals(found.map(_.query), expected.map(_.query))
    found.zip(expected).foreach { (a, b) =>
      assertEquals(
        (a.item, a.response, a.matched, a.controls, a.declaredReferences),
        (b.item, b.response, b.matched, b.controls, b.declaredReferences)
      )
      (a.status, b.status) match
        case (QueryStatus.Contributing(am, ab, ad), QueryStatus.Contributing(bm, bb, bd)) =>
          Vector(am -> bm, ab -> bb, ad -> bd).foreach { (xs, ys) =>
            assertEquals(xs.size, ys.size)
            xs.zip(ys).foreach((x, y) => assert(ScoreTolerance.approxEquals(x, y)))
          }
        case (as, bs) => assertEquals(as, bs)
    }

  private def queries(w: StudioWindow): Vector[QueryRow] =
    get(w.session.await(MethodsReads.queryRows[IO](w.session.reads.queries, RunId(1))))

  private def archive(w: StudioWindow): String = runOnFx {
    w.runtime.model.document.run(RunId(1)).get.archive match
      case CoreBinding.Bound(d) => d.sha256.hex
      case _                    => fail("No stored native result")
  }

  sys.env.get("EYES4S_PRIVATE_RUN").foreach { _ =>
    fxStage.test("private study completes the production lifecycle and fresh-process reopen") {
      fx =>
        require(StudioFxSuite.headless, "Private workflow requires headless FX")
        val input = PrivateProductionInputs.load
        val root  = input.root
        require(PrivateProductionInputs.external(StudioFxSuite.snapshotRoot).startsWith(root))
        val reopen         = sys.env.get("EYES4S_PRIVATE_REOPEN").contains("1")
        val expected       = get(read(root.resolve("api-queries.json")).as[Vector[QueryRow]])
        val apiResult      = read(root.resolve("api-result.json")).hcursor
        val expectedDigest = get(apiResult.get[String]("result_sha256"))
        val recipe         = get(read(root.resolve("api-recipe.json")).as[Recipe])
        val projectPath    = root.resolve("production.eyes")
        val memory         = InMemoryPlatform.create[IO]().unsafeRunSync()
        val platform       = memory.platform.copy(files = JvmFileSystem)
        val host           = runOnFx(
          ProjectLifecycleHost(
            fx.stage,
            platform,
            presets =
              FilePresetStore(root.resolve(if reopen then "presets-reopen" else "presets"))
          )
        )
        runOnFx(host.start())
        try
          memory.answer(Some(path(projectPath))).unsafeRunSync()
          invoke(
            host,
            if reopen then CommandRegistry.openProject.id else CommandRegistry.newProject.id
          )
          until(fx, "production project window open") {
            !host.busy && host.window.exists(_.runtime.model.project.isDefined)
          }
          val w = runOnFx(host.window.get)
          assertEquals(w.session.fixture, None)
          assert(runOnFx(w.project.isDefined))
          if !reopen then
            val (wizardStage, importer) = runOnFx(
              ImportWizardHost.openWindow(
                () => w.runtime.model.document,
                StudioWindow.followImports(() => w.runtime.model, w.runtime.dispatch),
                FilePresetStore(root.resolve("presets")),
                get(StudioStyles.stylesheets(eyes4s.studio.app.tokens.Theme.Light)),
                w.project,
                Some(platform)
              )
            )
            runOnFx(w.ownDialog(wizardStage))
            try
              Vector(SourceRole.Fixations -> "fixations.csv", SourceRole.Trials -> "trials.csv")
                .foreach { (role, name) =>
                  runOnFx(importer.read(role, path(root.resolve(name))))
                    .get(10, java.util.concurrent.TimeUnit.MINUTES)
                }
              def pick(
                  table: eyes4s.studio.desktop.importing.MappingTable,
                  name: String,
                  role: ColumnRole
              ): Unit = runOnFx {
                val menu = table.menu(name).getOrElse(fail(s"No mapped column $name"))
                menu.setValue(
                  menu.getItems.asScala.find(_.choice == ColumnChoice.Role(role)).get
                )
              }
              input.mapping.bindings
                .foreach(b => pick(importer.view.fixations, b.column.value, b.role))
              input.inventory.bindings
                .foreach(b => pick(importer.view.trials, b.column.value, b.role))
              runOnFx {
                val time = importer.view.time
                time.setValue(
                  time.getItems.asScala.find(_.unit.contains(TimeUnit.Milliseconds)).get
                )
                val kind = importer.view.displayKind
                kind.setValue(
                  kind.getItems.asScala.find(_.column.exists(_.value == "display_kind")).get
                )
                val file = importer.view.displayFile
                file.setValue(
                  file.getItems.asScala.find(_.column.exists(_.value == "image_file")).get
                )
                Vector(
                  GeometryField.ScreenWidth     -> input.geometry.screen.width.toDouble,
                  GeometryField.ScreenHeight    -> input.geometry.screen.height.toDouble,
                  GeometryField.ImageLeft       -> 0.0,
                  GeometryField.ImageTop        -> 0.0,
                  GeometryField.ImageWidth      -> input.geometry.screen.width.toDouble,
                  GeometryField.ImageHeight     -> input.geometry.screen.height.toDouble,
                  GeometryField.PixelsPerDegree -> input.geometry.pixelsPerDegree.value
                ).foreach((field, number) =>
                  importer.view
                    .geometryFields(field)
                    .setText(
                      if field == GeometryField.PixelsPerDegree then number.toString
                      else number.toInt.toString
                    )
                )
              }
              until(fx, "all import declarations accepted") { !importer.view.commit.isDisabled }
              runOnFx(importer.view.commit.fire())
              importer.importCompleted.get(10, java.util.concurrent.TimeUnit.MINUTES)
              until(fx, "production host selected imported revision") {
                w.runtime.model.document.dataset(DatasetRevision(1)).isDefined
              }
            finally runOnFx { importer.dispose(); wizardStage.close() }
            dispatch(
              fx,
              w,
              Intent.Navigate(
                Location(
                  Perspective.Data,
                  Vector(
                    Place.Dataset(DatasetRevision(1)),
                    Place.DataView(DataSection.Admission)
                  )
                )
              )
            )
            until(fx, "full admission ledger ready") {
              w.admission.state.counts.isInstanceOf[Loading.Ready[?]]
            }
            val admission =
              get(w.session.await(w.session.backend.admission(DatasetRevision(1))))
            val apiAdmission =
              get(read(root.resolve("api-admission.json")).as[AdmissionSummary])
            assertEquals(admission, apiAdmission.copy(state = admission.state))
            val ledger = get(
              w.session.await(
                LedgerPages.all[IO](p => w.session.backend.ledger(DatasetRevision(1), p))
              )
            )
            assertEquals(
              ledger,
              get(read(root.resolve("api-ledger.json")).as[Vector[LedgerEntry]])
            )
            write(root.resolve("ui-admission.json"), admission.asJson)
            capture(fx, root, "01-admission")
            runOnFx(
              w.admission.view.choices(eyes4s.plan.AdmissionDecision.ReviewExclusions).fire()
            )
            until(fx, "admission action enabled") { !w.admission.view.admit.isDisabled }
            runOnFx(w.admission.view.admit.fire())
            until(fx, "all admissible trials admitted") {
              w.runtime.model.document.dataset(DatasetRevision(1)).exists(_.decision.isAdmitted)
            }
            dispatch(fx, w, Intent.SwitchPerspective(Perspective.Analysis))
            runOnFx(w.recipe.choose(Preset.Recognition))
            until(fx, "descriptor-driven recipe form ready") { w.recipe.editor.ready }
            runOnFx {
              def id(s: String)                      = get(FieldId.of(s))
              def group(fields: (String, RawValue)*) =
                RawValue.Group(fields.toVector.map((k, v) => id(k) -> v))
              val form = w.recipe.editor
              val raw  = form.state.values.get
                .updated(
                  id("phases"),
                  group(
                    "focal"     -> RawValue.Choice(input.text("focal_phase")),
                    "reference" -> RawValue.Choice(input.text("reference_phase"))
                  )
                )
                .updated(
                  id("grid"),
                  group(
                    "columns" -> RawValue.Number(recipe.grid.columns.toString),
                    "rows"    -> RawValue.Number(recipe.grid.rows.toString)
                  )
                )
                .updated(
                  id("window"),
                  group(
                    "xMin" -> RawValue.Number("0"),
                    "yMin" -> RawValue.Number("0"),
                    "xMax" -> RawValue.Number(input.number("screen_width_px").toString),
                    "yMax" -> RawValue.Number(input.number("screen_height_px").toString)
                  )
                )
                .updated(
                  id("angularScale"),
                  RawValue.Number(input.number("pixels_per_degree").toString)
                )
                .updated(
                  id("scales"),
                  RawValue.Items(
                    Vector(
                      RawValue.Variant(
                        "degrees.gaussian",
                        Vector(
                          id("sigma") -> RawValue.Number(
                            recipe.scales.values.head.degrees.toString
                          ),
                          id("edges") -> RawValue.Choice("Truncate")
                        )
                      )
                    )
                  )
                )
                .updated(id("weight"), RawValue.Choice("Duration"))
                .updated(
                  id("pairing"),
                  group(
                    "matched" -> RawValue.Variant(
                      "select",
                      Vector(id("occurrence") -> RawValue.Variant("first", Vector.empty))
                    ),
                    "controls"  -> RawValue.Choice("sameSelection"),
                    "unmatched" -> RawValue.Choice("reportNoMatch")
                  )
                )
                .updated(
                  id("expectedUnmatched"),
                  group(
                    "column" -> RawValue.Text(input.text("expected_unmatched_column")),
                    "values" -> RawValue
                      .Items(Vector(RawValue.Text(input.text("expected_unmatched_value")))),
                    "assignments" -> RawValue.Items(Vector.empty)
                  )
                )
              form.editValues(raw)
            }
            until(fx, "recipe can be applied") { !w.recipe.editor.apply.isDisabled }
            runOnFx(w.recipe.editor.apply.fire())
            until(fx, "resolved design and preflight ready") { w.preflight.runButton._2 }
            assertEquals(
              runOnFx(w.runtime.model.document.draftRecipe.map(r => r.copy(input = None))),
              Some(recipe)
            )
            runOnFx {
              w.preflight.runName.setText(
                "Exploratory pixels: sigma80 px; virtual angular labels"
              )
              w.preflight.runDescription.setText(input.text("geometry_note"))
            }
            capture(fx, root, "02-analysis")
            runOnFx(w.preflight.pressRun())
            until(fx, "full real study execution complete") {
              w.runtime.model.jobs.ready.exists(_.run == RunId(1))
            }
          until(fx, "native archive delivery settled") {
            w.runtime.model.document
              .run(RunId(1))
              .exists(_.archive.isInstanceOf[CoreBinding.Bound[?]]) ||
            w.runtime.model.notice.exists(_.isInstanceOf[Notice.ArtifactsRefused])
          }
          runOnFx(w.runtime.model.notice)
            .collect { case Notice.ArtifactsRefused(_, _, diagnostic) => diagnostic }
            .foreach { diagnostic =>
              write(root.resolve("archive-refusal.json"), diagnostic.asJson)
              capture(fx, root, "archive-refusal")
              fail(diagnostic.message)
            }
          assertEquals(archive(w), expectedDigest)
          dispatch(
            fx,
            w,
            Intent.Dispatch(
              Command.PutReporting(
                get(
                  ReportingSpec.of(
                    get(ReportingId.of("private-response")),
                    "Exploratory spatial validation: sigma80 px",
                    Some(get(Covariate.of("response"))),
                    Vector.empty,
                    None,
                    ReportingWeight.ParticipantMeans,
                    None
                  )
                )
              )
            )
          )
          dispatch(fx, w, Intent.ShowRun(RunId(1)))
          dispatch(fx, w, Intent.SwitchPerspective(Perspective.Compare))
          dispatch(
            fx,
            w,
            Intent.FocusPane(get(eyes4s.studio.app.layout.PaneId.of("compare.query-table")))
          )
          until(fx, "complete query table served") {
            w.summary.summary.queries.exists {
              case QueriesAnswer.Answered(rows) => rows.size == expected.size
              case _                            => false
            }
          }
          capture(fx, root, "plot-requested")
          var plotPoll = 0
          until(fx, "participant and scale-profile plots are drawn", 60.seconds) {
            plotPoll += 1
            if plotPoll % 100 == 1 then
              val vm = w.summary.vm
              println(
                s"[private-production/plots] participant=${vm.participantPlot.map(_.fold(identity, p => s"served(${p.rows.size})"))}; profile=${vm.profile.map(_.fold(identity, p => s"served(${p.rows.size})"))}; layout=${w.runtime.model.layout}"
              )

            Vector(w.summary.participantPlot, w.summary.scaleProfile).forall { twin =>
              twin.status.get match
                case eyes4s.studio.desktop.plot.PlotTwinStatus.Shown(_)          => true
                case eyes4s.studio.desktop.plot.PlotTwinStatus.Refused(_, error) =>
                  fail(error.toString)
                case _ => false
            }
          }
          val found = queries(w)
          compareRows(found, expected)
          until(fx, "query table view model rendered") {
            w.summary.vm.queries.exists(_.isRight)
          }
          until(fx, "query table is visible and drawing rows") { w.summary.queryTable.onScreen }
          val table = get(runOnFx(w.summary.vm.queries).get)
          assertEquals(table.rows.size, found.size)
          assertEquals(runOnFx(w.summary.queryTable.rowTexts), table.rows.map(table.cellsOf))
          assertEquals(archive(w), expectedDigest)
          write(
            root.resolve(if reopen then "reopened-queries.json" else "ui-queries.json"),
            found.asJson
          )
          capture(fx, root, if reopen then "07-reopened" else "03-summary")
          if !reopen then
            val focus = found.find(_.status.isInstanceOf[QueryStatus.Contributing]).get.query
            dispatch(
              fx,
              w,
              Intent.Explain(
                Place.At(StudioRef.QueryContrast(RunId(1), get(ScaleIndex.of(0)), focus))
              )
            )
            capture(fx, root, "map-requested")
            var mapPoll = 0
            until(fx, "query and reference density maps served", 60.seconds) {
              mapPoll += 1
              if mapPoll % 100 == 1 then
                val states =
                  Vector(w.summary.panels.queryView, w.summary.panels.referenceView).map {
                    view =>
                      val status = view.status.get match
                        case TrialViewStatus.Shown(scene) => s"shown/map=${scene.map.nonEmpty}"
                        case other                        => other.toString
                      s"request=${view.requestedMap.isDefined}; input=${view.input.isDefined}; status=$status; refusal=${view.mapRefused}"
                  }
                println(
                  s"[private-production/maps] ${states.mkString(" | ")}; panels=${w.summary.panelsVM.toString.take(1600)}; trail=${w.runtime.model.location}"
                )

              Vector(w.summary.panels.queryView, w.summary.panels.referenceView).forall(view =>
                view.requestedMap.nonEmpty && (view.status.get match
                  case TrialViewStatus.Shown(scene) => scene.map.nonEmpty
                  case _                            => false)
              )
            }
            capture(fx, root, "04-trial-maps")
            dispatch(fx, w, Intent.Dispatch(Command.SetTheme(Theme.Dark)))
            fx.awaitLayout()
            capture(fx, root, "05-trial-maps-dark", dark = true)
            dispatch(fx, w, Intent.Dispatch(Command.SetTheme(Theme.Light)))
            val pairs = get(
              w.session.await(
                w.session.navigator.pairs(
                  StudioRef.QueryContrast(RunId(1), get(ScaleIndex.of(0)), focus),
                  PairDesign.Matched,
                  get(PageRequest.of(0, 1))
                )
              )
            )
            val maps   = get(w.session.await(w.session.navigator.maps(pairs.entries.head)))
            val points = get(
              w.session
                .await(w.session.navigator.fixations(maps.query, get(PageRequest.of(0, 1))))
            )
            val source = get(w.session.await(w.session.navigator.record(points.entries.head)))
            dispatch(fx, w, Intent.Explain(Place.At(source)))
            dispatch(
              fx,
              w,
              Intent.FocusPane(eyes4s.studio.app.layout.StudioLayouts.sourceRecords)
            )
            val selectSource = runOnFx(
              w.sourceRecords.current.selection
                .submit(
                  eyes4s.studio.core.selection.SelectionMode.Replace,
                  Vector(source),
                  eyes4s.studio.core.selection.InputCause.Programmatic
                )
                ._2
            )
            dispatch(fx, w, selectSource)
            until(fx, "exact original source record selected and displayed", 60.seconds) {
              val state = w.sourceRecords.current
              state.cursor
                .flatMap(i => eyes4s.studio.app.explore.SourceRecords.rowAt(state, i))
                .exists(_.ref == source)
            }
            runOnFx(w.sourceRecords.toggleRaw())
            until(fx, "verbatim source fields displayed", 60.seconds) {
              w.sourceRecords.rawText.isDefined
            }
            capture(fx, root, "source-record")
            source match
              case StudioRef.SourceRecord(_, _, _, number) =>
                val page = get(
                  w.session.await(
                    w.session.backend.sourceRecords(AnalysisRevision(1), number.value, 1)
                  )
                )
                assertEquals(page.rows.head.ref, source)
                assert(page.rows.head.screen.nonEmpty)
                write(root.resolve("source-record.json"), page.asJson)
              case other => fail(s"Expected source record, got $other")
            val reporting = get(ReportingId.of("private-response"))
            dispatch(
              fx,
              w,
              Intent.Dispatch(
                Command.CreateFigure(
                  RunId(1),
                  reporting,
                  Vector(
                    PanelSpec(
                      get(PanelLetter.of("A")),
                      "Exploratory spatial contrast; sigma80 px; virtual angular labels",
                      PanelScale.At(recipe.scales.values.head),
                      PanelSelection.AllQueries
                    )
                  )
                )
              )
            )
            dispatch(
              fx,
              w,
              Intent.Navigate(Location(Perspective.Figures, Vector(Place.Figures)))
            )
            until(fx, "figure export data served") {
              w.figures.vm.bundle.exists(b => BundleItem.Default.subsetOf(b.written.toSet))
            }
            capture(fx, root, "06-figure")
            val exportDir = root.resolve("exports")
            Files.createDirectories(exportDir)
            memory.answer(Some(path(exportDir))).unsafeRunSync()
            val exportButton = runOnFx(
              w.root
                .lookupAll(".button")
                .asScala
                .collectFirst {
                  case button: Button
                      if button.getText == "Export bundle…" && !button.isDisabled =>
                    button
                }
                .getOrElse(fail("No enabled export bundle button"))
            )
            runOnFx(exportButton.fire())
            until(fx, "complete figure and result bundle exported") {
              Files.isRegularFile(exportDir.resolve("figure-1-bundle/README.txt"))
            }
          until(fx, "production document saved") { !w.runtime.model.save.edited }
          invoke(host, CommandRegistry.closeProject.id)
          until(fx, "normal project close released the window") {
            !host.busy && host.window.exists(_.runtime.model.project.isEmpty)
          }
          write(
            root.resolve(if reopen then "reopen-result.json" else "ui-result.json"),
            Json.obj(
              "status"        -> Json.fromString("passed"),
              "queries"       -> Json.fromInt(found.size),
              "result_sha256" -> Json.fromString(expectedDigest),
              "fresh_process" -> Json.fromBoolean(reopen),
              "pid"           -> Json.fromLong(ProcessHandle.current().pid()),
              "route"         -> Json.fromString(
                "ProjectLifecycleHost File New/Open/Close; ordinary import and recipe controls; native backend; real figure bundle export"
              )
            )
          )
        finally runOnFx(host.shutdown())
    }
  }
