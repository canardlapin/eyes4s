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
import eyes4s.codec.ByteDigest
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
import javafx.scene.control.Button
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.concurrent.duration.*

/** Opt-in real-window test. Requests, saved projects, logs and snapshots stay outside Git.
  * File-dialog answers and the explicit recipe seed use typed application commands;
  * admission and Save & run use the actual controls. No fixture backend is installed.
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
      val phases  = sources.map(j => get(j.hcursor.get[String]("phase"))).distinct.sorted
      val initial = get(AppModel.newProject)
      val entries =
        Vector(SourceRole.Fixations -> "fixations.csv", SourceRole.Trials -> "trials.csv").map {
          (role, name) =>
            val bytes = IArray.from(Files.readAllBytes(root.resolve(name)))
            Source(role, get(SourcePath.of(name)), ByteDigest.sha256(bytes), None) -> bytes
        }
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
      val dataset = DatasetRevision(1)
      val command = Command.ImportSources(
        None,
        get(Sources.of(entries.map(_._1))),
        NativeCommandJourneyFixture.mapping,
        NativeCommandJourneyFixture.units,
        geometry,
        DeclaredAttributes.empty,
        None,
        Some(NativeCommandJourneyFixture.inventory)
      )
      val projectPath = root.resolve("density.eyes")
      val results     = project(projectPath, initial.document, entries)
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
              dispatch(fx, w, Intent.Dispatch(command))
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
                  None,
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
                val fields = StudioFields(
                  Preset.Custom,
                  get(RevisionName.of(phase)),
                  get(
                    request.get[String]("geometry_note")
                  ) + "; fixed pixel kernel; exhaustive controls"
                )
                dispatch(
                  fx,
                  w,
                  Intent.Dispatch(if index == 0 then
                    Command.StartAnalysis(dataset, recipe, fields)
                  else Command.StartFamily(phase, dataset, recipe, fields))
                )
                dispatch(fx, w, Intent.SwitchPerspective(Perspective.Analysis))
                until(fx, "resolved recipe and run control") {
                  w.resolvedDesign.state.preview.receipt
                    .exists(_.recipe.contains(recipe)) && w.preflight.runButton._2
                }
                assertEquals(runOnFx(w.runtime.model.document.draftRecipe), Some(recipe))
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
                dispatch(fx, w, Intent.SwitchPerspective(Perspective.Compare))
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
                dispatch(
                  fx,
                  w,
                  Intent.FocusPane(
                    get(
                      eyes4s.studio.app.layout.PaneId.of("compare.query-table")
                    )
                  )
                )
                fx.awaitLayout()
                assertEquals(
                  runOnFx(w.summary.queryTable.rowTexts),
                  view.rows.map(view.cellsOf)
                )
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
            "typed import and recipe seed; admission, Save & run, Compare UI; verified saved project"
          ),
          "runs"    -> Json.arr(results*),
          "project" -> Json.fromString("density.eyes")
        )
      )
    }
  }
