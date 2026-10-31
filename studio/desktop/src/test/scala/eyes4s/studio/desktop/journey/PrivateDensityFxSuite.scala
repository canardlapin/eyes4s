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

import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import eyes4s.plan.TrialKeyDefinitions
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.app.nav.{DataSection, Location, Place}
import eyes4s.studio.app.compare.QueriesAnswer
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.session.ProjectSession
import eyes4s.studio.desktop.harness.{FxStage, SnapshotScale, StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.explore.NavigatorDisplays
import eyes4s.studio.desktop.platform.FileProjectStore
import eyes4s.studio.desktop.runtime.{DatasetSourceHosts, SessionPort}
import eyes4s.studio.desktop.shell.ShellFxSuite
import io.circe.{Json, parser}
import io.circe.syntax.*
import eyes4s.studio.app.importing.{ImportWizard, WizardTab}
import eyes4s.studio.core.importing.{ColumnChoice, GeometryField, ImportPresets}
import eyes4s.studio.core.platform.HostPath
import eyes4s.studio.desktop.importing.ImportWizardHost
import javafx.scene.control.Button
import scala.jdk.CollectionConverters.*
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.concurrent.duration.*

/** Opt-in real-window test. Requests, saved projects, logs and snapshots stay outside Git.
  * File-dialog answers supply paths; mappings, geometry, recipe configuration,
  * admission and Save & run use actual controls. No fixture backend is installed.
  */
class PrivateDensityFxSuite extends ShellFxSuite:
  private final case class Tolerance(absolute: Double, relative: Double):
    def approxEquals(a: Double, b: Double): Boolean =
      a.isFinite && b.isFinite && math
        .abs(a - b) <= absolute + relative * math.max(math.abs(a), math.abs(b))
  override val munitTimeout: Duration          = 20.minutes
  private val ScoreTolerance                   = Tolerance(absolute = 2e-12, relative = 2e-12)
  private def get[E, A](e: Either[E, A]): A    = e.fold(x => fail(x.toString), identity)
  private def read(p: Path): Json              = get(parser.parse(Files.readString(p)))
  private def write(p: Path, json: Json): Unit =
    Files.writeString(p, json.spaces2, StandardOpenOption.CREATE_NEW): Unit

  private def until(fx: FxStage, what: String)(condition: => Boolean): Unit =
    val deadline = System.nanoTime() + 180.seconds.toNanos
    while !runOnFx(condition) do
      if System.nanoTime() > deadline then fail(s"Timed out: $what")
      Thread.sleep(25)
    // State polling lets normal pulses run; layout is needed once before UI inspection.
    fx.awaitLayout()

  private def safe(path: Path): Path =
    val p              = path.toRealPath()
    var ancestor: Path = p
    while ancestor != null do
      require(!Files.exists(ancestor.resolve(".git")), "Private artifacts cannot be in Git")
      ancestor = ancestor.getParent
    p

  private def project(
      path: Path,
      document: StudioDocument,
      entries: Vector[(Source, IArray[Byte])]
  ) =
    val owner = get(LockOwner.of("Private density UI harness"))
    for
      store    <- Resource.eval(FileProjectStore.at[IO](path))
      imported <- Resource.eval(
        Resource.make(store.acquire(owner).map(get))(lock => store.release(lock).map(get)).use {
          lock =>
            entries.traverse { (s, bytes) =>
              ProjectBundle
                .importInput(store, lock, InputKind.Source(s.role), s.path.value, bytes)
                .map(get)
            }
        }
      )
      session <- Resource.make(
        ProjectSession
          .create(store, owner, document, SharingOptions.complete, imported)
          .map(get)
      )(_.close.map(get))
      port <- Resource.make(IO.blocking(SessionPort.start(session)))(p =>
        IO.blocking(p.close())
      )
    yield port

  sys.env.get("EYES4S_PRIVATE_RUN").foreach { directory =>
    fxStage.test("private fixation density analysis runs through the real window") { fx =>
      require(
        !sys.env.contains("CI") && !sys.env.contains("GITHUB_ACTIONS"),
        "Private UI disabled in CI"
      )
      val root = safe(Path.of(directory))
      require(StudioFxSuite.headless, "Private harness must use headless FX")
      Files.createDirectories(StudioFxSuite.snapshotRoot)
      require(
        safe(StudioFxSuite.snapshotRoot).startsWith(root),
        "Snapshots must stay in the private run"
      )
      val request              = read(root.resolve("request.json")).hcursor
      def number(name: String) = get(request.get[Double](name))
      val sources              = get(request.get[Vector[Json]]("sources"))
      val expected = get(read(root.resolve("eyes4s.json")).hcursor.get[Vector[Json]]("sources"))
        .map(j => get(j.hcursor.get[String]("id")) -> j)
        .toMap
      val phases   = sources.map(j => get(j.hcursor.get[String]("phase"))).distinct.sorted
      val initial  = get(AppModel.newProject)
      val ppd      = get(DeclaredPixelsPerDegree.of(number("pixels_per_degree")))
      val geometry = get(
        Geometry.of(
          get(
            ScreenSize.of(
              get(request.get[Int]("screen_width_px")),
              get(request.get[Int]("screen_height_px"))
            )
          ),
          get(
            ImagePlacement.of(
              number("x_min").toInt,
              number("y_min").toInt,
              (number("x_max") - number("x_min")).toInt,
              (number("y_max") - number("y_min")).toInt
            )
          ),
          ppd
        )
      )
      val dataset     = DatasetRevision(1)
      val projectPath = root.resolve("density.eyes")
      val results     = project(projectPath, initial.document, Vector.empty)
        .use { port =>
          IO.blocking {
            val w = boot(
              fx,
              initial,
              project = Some(port),
              stimuli =
                eyes4s.studio.desktop.trial.StimulusSource.directory(root.resolve("stimuli")),
              displays = NavigatorDisplays.stored(port),
              nativeSources = Some(DatasetSourceHosts.stored(port))
            )
            try
              assertEquals(w.session.fixture, None)
              val importer = runOnFx(
                ImportWizardHost(
                  ImportWizard.newImport(w.runtime.model.document, ImportPresets.empty),
                  () => w.runtime.model.document,
                  w.runtime.dispatch,
                  ImportWizardHost.fxPlatform(() => fx.stage, noPresets(), Some(port)),
                  () => ()
                )
              )
              try
                runOnFx {
                  importer.view.node.getStyleClass.add("es")
                  importer.view.node.getStylesheets.addAll(w.root.getStylesheets)
                }
                fx.show(importer.view.node)
                Vector(
                  SourceRole.Fixations -> "fixations.csv",
                  SourceRole.Trials    -> "trials.csv"
                )
                  .foreach { (role, name) =>
                    runOnFx(importer.read(role, get(HostPath.of(root.resolve(name).toString))))
                      .get(60, java.util.concurrent.TimeUnit.SECONDS)
                  }
                def pick(
                    table: eyes4s.studio.desktop.importing.MappingTable,
                    column: String,
                    role: ColumnRole
                ): Unit = runOnFx {
                  val menu   = table.menu(column).getOrElse(fail(s"Missing role menu: $column"))
                  val option =
                    menu.getItems.asScala.find(_.choice == ColumnChoice.Role(role)).get
                  menu.setValue(option)
                }
                NativeCommandJourneyFixture.mapping.bindings
                  .foreach(b => pick(importer.view.fixations, b.column.value, b.role))
                NativeCommandJourneyFixture.inventory.bindings
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
                  importer.view.tabs(WizardTab.Geometry).fire()
                  Vector(
                    GeometryField.ScreenWidth  -> number("screen_width_px"),
                    GeometryField.ScreenHeight -> number("screen_height_px"),
                    GeometryField.ImageLeft    -> number("x_min"),
                    GeometryField.ImageTop     -> number("y_min"),
                    GeometryField.ImageWidth   -> (number("x_max") - number("x_min")),
                    GeometryField.ImageHeight  -> (number("y_max") - number("y_min"))
                  ).foreach((field, value) =>
                    importer.view.geometryFields(field).setText(value.toInt.toString)
                  )
                  importer.view
                    .geometryFields(GeometryField.PixelsPerDegree)
                    .setText(ppd.value.toString)
                }
                until(fx, "import controls validate mappings and geometry") {
                  !importer.view.commit.isDisabled
                }
                fx.snapshot(StudioTheme.Light, List(SnapshotScale.X1))
                  .foreach(p => Files.copy(p, root.resolve("import-settings.png")))
                fx.robot.click(importer.view.commit)
                importer.importCompleted.get(60, java.util.concurrent.TimeUnit.SECONDS)
                until(fx, "imported dataset") {
                  w.runtime.model.document.dataset(dataset).isDefined
                }
                assertEquals(
                  runOnFx(w.runtime.model.document.dataset(dataset).map(_.geometry)),
                  Some(geometry)
                )
              finally runOnFx(importer.dispose())
              fx.show(w.root)
              dispatch(
                fx,
                w,
                Intent.Navigate(
                  Location(
                    Perspective.Data,
                    Vector(Place.Dataset(dataset), Place.DataView(DataSection.Admission))
                  )
                )
              )
              until(fx, "admission ready") {
                w.admission.state.counts.isInstanceOf[Loading.Ready[?]]
              }
              runOnFx(
                w.admission.view.choices(eyes4s.plan.AdmissionDecision.ReviewExclusions).fire()
              )
              until(fx, "admit enabled") { !w.admission.view.admit.isDisabled }
              fx.robot.click(w.admission.view.admit)
              until(fx, "dataset admitted") {
                w.runtime.model.document.dataset(dataset).exists(_.decision.isAdmitted)
              }
              phases.zipWithIndex.map { (phase, index) =>
                val recipe = Recipe(
                  runOnFx(w.runtime.model.document.latestAnalysis.flatMap(_.recipe.input)),
                  DefinitionRef.fromCore(TrialKeyDefinitions.trialLayout),
                  MethodSpec(get(DefinitionRef.of("eyes4s.cosine", 1)), Vector.empty),
                  PhasePair(Phase(phase), Phase.Encoding),
                  WeightChoice.Duration,
                  FailureChoice.RequireAll,
                  get(GridSize.of(get(request.get[Int]("nx")), get(request.get[Int]("ny")))),
                  Some(
                    get(
                      AnalysisWindow
                        .of(number("x_min"), number("y_min"), number("x_max"), number("y_max"))
                    )
                  ),
                  Some(OffWindowChoice.Exclude),
                  get(ScaleSet.of(Vector(get(Sigma.of(number("sigma_px") / ppd.value))))),
                  Some(ppd),
                  MatchedChoice.RequireOne,
                  ControlChoice.SameSelection,
                  UnmatchedChoice.Refuse,
                  InitialFixationChoice.KeepAll
                )
                dispatch(fx, w, Intent.SwitchPerspective(Perspective.Analysis))
                if index == 0 then runOnFx(w.recipe.choose(Preset.EncodingRetrieval))
                dispatch(
                  fx,
                  w,
                  Intent.FocusPane(get(eyes4s.studio.app.layout.PaneId.of("analysis.recipe")))
                )
                until(fx, "recipe editor ready") {
                  w.recipe.editor.ready && w.recipe.editor.node.isVisible && !w.recipe.editor.node.isDisabled
                }
                val form = w.recipe.editor
                runOnFx {
                  val id = (name: String) => get(eyes4s.plan.FieldId.of(name))
                  import eyes4s.plan.RawValue
                  def group(parts: (String, RawValue)*) =
                    RawValue.Group(parts.toVector.map((name, value) => id(name) -> value))
                  val raw = form.state.values.get
                    .updated(
                      id("phases"),
                      group(
                        "focal"     -> RawValue.Choice(phase),
                        "reference" -> RawValue.Choice("Encoding")
                      )
                    )
                    .updated(
                      id("grid"),
                      group(
                        "columns" -> RawValue.Number(get(request.get[Int]("nx")).toString),
                        "rows"    -> RawValue.Number(get(request.get[Int]("ny")).toString)
                      )
                    )
                    .updated(
                      id("window"),
                      group(
                        "xMin" -> RawValue.Number(number("x_min").toString),
                        "yMin" -> RawValue.Number(number("y_min").toString),
                        "xMax" -> RawValue.Number(number("x_max").toString),
                        "yMax" -> RawValue.Number(number("y_max").toString)
                      )
                    )
                    .updated(id("angularScale"), RawValue.Number(ppd.value.toString))
                    .updated(
                      id("scales"),
                      RawValue.Items(
                        Vector(
                          RawValue.Variant(
                            "degrees.gaussian",
                            Vector(
                              id("sigma") -> RawValue
                                .Number((number("sigma_px") / ppd.value).toString),
                              id("edges") -> RawValue.Choice("Truncate")
                            )
                          )
                        )
                      )
                    )
                  val pairing = raw.get(id("pairing")) match
                    case RawValue.Group(parts) =>
                      RawValue.Group(parts.map {
                        case (field, _) if field.value == "unmatched" =>
                          field -> RawValue.Choice("refuse")
                        case other => other
                      })
                    case other => other
                  form.editValues(
                    raw
                      .updated(id("weight"), RawValue.Choice("Duration"))
                      .updated(id("pairing"), pairing)
                  )
                }
                until(fx, "recipe settings can be applied") { !form.apply.isDisabled }
                runOnFx(w.recipe.node.setVvalue(1.0))
                fx.awaitLayout()
                fx.robot.click(form.apply)
                until(fx, "resolved recipe and run control") {
                  w.resolvedDesign.state.preview.receipt
                    .exists(_.recipe.contains(recipe)) && w.preflight.runButton._2
                }
                assertEquals(runOnFx(w.runtime.model.document.draftRecipe), Some(recipe))
                runOnFx {
                  w.preflight.runName.setText(phase)
                  w.preflight.runDescription.setText(get(request.get[String]("geometry_note")))
                }
                fx.snapshot(StudioTheme.Light, List(SnapshotScale.X1))
                  .foreach(p => Files.copy(p, root.resolve(s"phase-${index + 1}-settings.png")))
                val runButton =
                  runOnFx(w.preflight.node.lookup(".preflight-run").asInstanceOf[Button])
                runOnFx(w.preflight.node.setVvalue(1.0))
                fx.awaitLayout()
                fx.robot.click(runButton)
                val run = RunId(index + 1)
                until(fx, "analysis complete") {
                  w.runtime.model.jobs.ready.exists(_.run == run)
                }
                assertEquals(
                  runOnFx(w.runtime.model.document.latestAnalysis.map(_.studio.name.value)),
                  Some(phase)
                )
                assertEquals(
                  runOnFx(w.runtime.model.document.latestAnalysis.map(_.studio.description)),
                  Some(get(request.get[String]("geometry_note")))
                )
                until(fx, "native artifacts saved") {
                  w.runtime.model.document
                    .run(run)
                    .exists(_.archive.isInstanceOf[CoreBinding.Bound[?]])
                }
                dispatch(
                  fx,
                  w,
                  Intent.Dispatch(
                    Command.PutReporting(
                      get(
                        ReportingSpec.of(
                          get(ReportingId.of("density-participants")),
                          "Participant means",
                          None,
                          Vector.empty,
                          None,
                          ReportingWeight.ParticipantMeans,
                          None
                        )
                      )
                    )
                  )
                )
                dispatch(fx, w, Intent.ShowRun(run))
                dispatch(fx, w, Intent.SwitchPerspective(Perspective.Results))
                val wanted = sources.filter(j => get(j.hcursor.get[String]("phase")) == phase)
                until(fx, "all query results delivered to the view") {
                  w.summary.summary.queries.exists {
                    case QueriesAnswer.Answered(rows) => rows.size == wanted.size
                    case _                            => false
                  } && w.summary.vm.queries.exists(_.isRight)
                }
                val rows = runOnFx(w.summary.summary.queries).get match
                  case QueriesAnswer.Answered(rows) => rows
                  case other                        => fail(s"Query view missing: $other")
                assertEquals(
                  rows.map(_.query.trial).toSet,
                  wanted.map(j => get(j.hcursor.get[String]("id"))).toSet
                )
                rows.foreach { row =>
                  val e        = expected(row.query.trial).hcursor
                  val controls = get(e.get[Vector[String]]("controls"))
                  assertEquals(row.matched.map(_.trial), Some(get(e.get[String]("template"))))
                  assertEquals(row.matched.map(_.participant), Some(row.query.participant))
                  assertEquals(row.controls, Some(controls.size))
                  row.status match
                    case QueryStatus.Contributing(m, b, d) =>
                      Vector("observed" -> m, "baseline" -> b, "difference" -> d).foreach {
                        (name, values) =>
                          assertEquals(values.size, 1)
                          assert(
                            ScoreTolerance.approxEquals(values.head, get(e.get[Double](name))),
                            s"$name differs"
                          )
                      }
                    case other => fail(s"Query not contributing: $other")
                }
                val view = get(runOnFx(w.summary.vm.queries).get)
                assertEquals(view.rows.size, rows.size)
                val pairs = get(
                  w.session.await(
                    eyes4s.studio.core.figures.MethodsReads.pairRowsAt[IO](
                      w.session.reads.pairRows,
                      run,
                      0
                    )
                  )
                ).flatMap(_.rows)
                rows.foreach { row =>
                  val e   = expected(row.query.trial).hcursor
                  val own = pairs.filter(_.query == row.query)
                  assert(own.forall(_.reference.participant == row.query.participant))
                  assertEquals(
                    own.filter(_.design == PairDesign.Matched).map(_.reference.trial),
                    Vector(get(e.get[String]("template")))
                  )
                  assertEquals(
                    own.filter(_.design == PairDesign.Control).map(_.reference.trial).sorted,
                    get(e.get[Vector[String]]("controls")).sorted
                  )
                }
                write(root.resolve(s"phase-${index + 1}-ui-rows.json"), rows.asJson)
                fx.snapshot(StudioTheme.Light, List(SnapshotScale.X1))
                  .foreach(p => Files.copy(p, root.resolve(s"phase-${index + 1}-results.png")))
                until(fx, "project journal saved") { !w.runtime.model.save.edited }
                Json.obj(
                  "phase"          -> Json.fromString(phase),
                  "run"            -> Json.fromInt(run.number),
                  "queries"        -> Json.fromInt(rows.size),
                  "displayed_rows" -> Json.fromInt(view.rows.size)
                )
              }
            finally
              runOnFx(w.close())
              opened -= w
          }
        }
        .unsafeRunSync()
      val reopened = (for
        store  <- FileProjectStore.at[IO](projectPath)
        bundle <- ProjectBundle.open(store)
      yield get(bundle)).unsafeRunSync()
      assert(reopened.science.verified)
      assertEquals(reopened.document.runs.size, phases.size)
      write(
        root.resolve("ui-result.json"),
        Json.obj(
          "status"  -> Json.fromString("passed"),
          "backend" -> Json.fromString("real"),
          "route"   -> Json.fromString(
            "actual import mapping, geometry and recipe controls; admission, Save & run, Compare UI; verified saved project"
          ),
          "runs"    -> Json.arr(results*),
          "project" -> Json.fromString("density.eyes")
        )
      )
    }
  }
