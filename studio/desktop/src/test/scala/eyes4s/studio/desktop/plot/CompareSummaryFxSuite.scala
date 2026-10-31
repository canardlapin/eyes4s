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

package eyes4s.studio.desktop.plot

import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.layout.{CompareLayout, StudioLayouts}
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.text.Format
import eyes4s.studio.core.backend.{Response, ReportRole}
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.selection.{StudioRef, ReportGroup}
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.{FxStage, StudioTheme}
import eyes4s.studio.desktop.shell.ShellFxSuite
import eyes4s.studio.app.compare.{QueriesAnswer, SummaryAnswer, ReportAnswer}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.app.{AppEffect, Intent}
import eyes4s.studio.core.backend.{BackendError, PageRequest, RunId}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.compare.{CompareSummaryHost, SummaryInputs}
import eyes4s.studio.desktop.runtime.{EffectPerformer, StudioRuntime}
import eyes4s.studio.viz.plot.{DataPoint, RowMarking, SlopegraphPlot}
import javafx.scene.control.{Label, RadioMenuItem}
import javafx.scene.layout.HBox

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

import scala.jdk.CollectionConverters.*

/** Compare's summary layout in the studio window (ticket S8.6;
  * Results.dc.html) at story moment t3: run 7's participant table is
  * FIXTURE.md's, the participant plot shows both grand means and each
  * group's n, the σ selector serves every scale, run 8's freshness shows while
  * it runs, P05's failed queries have no value, and Explain P17 carries the
  * spec and the group into the query layout.
  */
class CompareSummaryFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(180, "s")

  /** Participant values independently averaged from fixture.json's stored
    * contributing queries at 2° (SCORES.json's), rounded half up; they read
    * as FIXTURE.md's table of SCORES.json's participant means.
    */
  private val fixtureTable: Vector[Vector[String]] = Vector(
    "P01 | 20 | 19 | 0 | 0 | 1 | 0.73 | 0.31 | +0.42 | +0.47 (11) | +0.35 (8)",
    "P02 | 20 | 19 | 0 | 0 | 1 | 0.71 | 0.29 | +0.43 | +0.46 (13) | +0.35 (6)",
    "P03 | 20 | 19 | 0 | 1 | 0 | 0.80 | 0.33 | +0.47 | +0.49 (15) | +0.37 (4)",
    "P04 | 20 | 19 | 0 | 0 | 1 | 0.71 | 0.36 | +0.35 | +0.44 (12) | +0.20 (7)",
    "P05 | 20 | 17 | 3 | 0 | 0 | 0.70 | 0.35 | +0.35 | +0.39 (13) | +0.22 (4)",
    "P06 | 20 | 19 | 0 | 0 | 1 | 0.69 | 0.38 | +0.31 | +0.38 (11) | +0.22 (8)",
    "P07 | 20 | 19 | 0 | 1 | 0 | 0.69 | 0.35 | +0.34 | +0.38 (15) | +0.21 (4)",
    "P08 | 20 | 19 | 0 | 1 | 0 | 0.75 | 0.41 | +0.34 | +0.40 (12) | +0.25 (7)",
    "P09 | 20 | 19 | 0 | 0 | 1 | 0.72 | 0.35 | +0.37 | +0.41 (15) | +0.21 (4)",
    "P10 | 20 | 19 | 0 | 1 | 0 | 0.80 | 0.37 | +0.43 | +0.44 (16) | +0.34 (3)",
    "P11 | 20 | 19 | 0 | 1 | 0 | 0.70 | 0.34 | +0.36 | +0.47 (13) | +0.12 (6)",
    "P12 | 20 | 19 | 0 | 0 | 1 | 0.75 | 0.34 | +0.41 | +0.48 (12) | +0.29 (7)",
    "P13 | 20 | 19 | 0 | 0 | 1 | 0.69 | 0.39 | +0.30 | +0.36 (11) | +0.22 (8)",
    "P14 | 20 | 19 | 0 | 1 | 0 | 0.72 | 0.38 | +0.34 | +0.40 (14) | +0.17 (5)",
    "P15 | 20 | 19 | 0 | 1 | 0 | 0.73 | 0.35 | +0.38 | +0.39 (13) | +0.36 (6)",
    "P16 | 20 | 19 | 0 | 0 | 1 | 0.73 | 0.33 | +0.40 | +0.47 (12) | +0.27 (7)",
    "P17 | 20 | 19 | 0 | 0 | 1 | 0.72 | 0.34 | +0.38 | +0.40 (17) | +0.22 (2)",
    "P18 | 20 | 19 | 0 | 1 | 0 | 0.76 | 0.38 | +0.38 | +0.43 (16) | +0.14 (3)",
    "P19 | 20 | 19 | 0 | 0 | 1 | 0.73 | 0.35 | +0.37 | +0.41 (15) | +0.22 (4)",
    "P20 | 20 | 19 | 0 | 0 | 1 | 0.75 | 0.38 | +0.37 | +0.44 (14) | +0.20 (5)",
    "P21 | 20 | 19 | 0 | 1 | 0 | 0.76 | 0.40 | +0.36 | +0.38 (17) | +0.23 (2)",
    "P22 | 20 | 19 | 0 | 0 | 1 | 0.74 | 0.36 | +0.38 | +0.50 (12) | +0.19 (7)",
    "P23 | 20 | 19 | 0 | 0 | 1 | 0.77 | 0.40 | +0.37 | +0.41 (14) | +0.28 (5)",
    "P24 | 20 | 19 | 0 | 0 | 1 | 0.77 | 0.38 | +0.39 | +0.46 (13) | +0.25 (6)"
  ).map(_.split('|').toVector.map(_.trim))

  // The overhaul's columns (results.html): participant, contributing queries,
  // and each group's D with its n; the Δ columns follow (app suites check them).
  private val fixtureLeading: Vector[Vector[String]] = fixtureTable.map { r =>
    def split(cell: String) =
      val (d, n) = cell.span(_ != ' ')
      Vector(d, n.trim.stripPrefix("(").stripSuffix(")"))
    Vector(r(0), r(2)) ++ split(r(9)) ++ split(r(10))
  }

  private def ascii(s: String): String = s.replace(Format.Minus, "-")

  // The summary's tables sit in tabs not shown here: their rows are read
  // from the tables' state (TableTwinViewFxSuite covers the drawn rows).
  private def rows(t: TableTwinView): Vector[Vector[String]] =
    runOnFx(t.modelRowTexts.map(_.map(ascii)))

  /** Report at `σ`: the reporting menu's choice in the plot's header. */
  private def reportAt(w: StudioWindow, sigma: String): Unit = runOnFx {
    w.summary.chrome.reporting.getItems.asScala
      .collectFirst { case i: RadioMenuItem if i.getText == s"Report at $sigma" => i }
      .getOrElse(fail(s"no $sigma choice"))
      .fire()
  }

  private def loaded(fx: FxStage, w: StudioWindow): Unit =
    eventually(fx, "the 2° report is available")(
      w.summary.vm.scales.exists(c => c.scale == StoryModels.sigma2 && c.available)
    )
    reportAt(w, "σ 2°")
    eventually(fx, "the summary and its queries are read") {
      val vm = w.summary.vm
      vm.participants.exists(_.isRight) && vm.queries.exists(_.isRight) &&
      vm.scales.nonEmpty && vm.scales.forall(_.available) &&
      w.summary.participantPlot.status.get.isInstanceOf[PlotTwinStatus.Shown]
    }

  fxStage.test("participant table and plot match the stored query values") { fx =>
    val w = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
    loaded(fx, w)
    assertEquals(rows(w.summary.participantTable).map(_.take(6)), fixtureLeading)
    // The slopegraphs: 24 participants' lines and the mean line at each σ.
    val plot = runOnFx(w.summary.participantPlot.plot).getOrElse(fail("no plot"))
    assertEquals(plot.marks.size, 4 * 25)
    val grand = plot.marks.filter(_.ref.isInstanceOf[StudioRef.ReportCell])
    assertEquals(grand.size, 4)
    val at2 = grand
      .find(_.ref match
        case StudioRef.ReportCell(_, _, scale, _, _) => scale == StoryModels.sigma2
        case _                                       => false)
      .getOrElse(fail("no 2° mean line"))
    val di = plot.source.columns.indexWhere(_.id.value == "d")
    assertEquals(at2.rows.flatMap(r => plot.source.text(r.row, di)), Vector("+0.43", "+0.24"))
    // The facts under the table and the newer run's freshness.
    val board = runOnFx(w.summary.board)
    assert(
      board.footer.contains(
        "454 of 480 queries contribute (14 not admitted, 9 no match, 3 failed)"
      ),
      board.footer
    )
    assertEquals(runOnFx(w.summary.vm.freshness), Some("Rev 5 · run 8 running · 48%"))
    eventually(fx, "the paired headline is shown")(w.summary.headline.isVisible)
    // The σ choices in the reporting menu, 2° reported.
    val choices = runOnFx(
      w.summary.chrome.reporting.getItems.asScala.toVector.collect {
        case i: RadioMenuItem if i.getText.startsWith("Report at") =>
          (i.getText, i.isDisable, i.isSelected)
      }
    )
    assertEquals(
      choices,
      Vector(
        ("Report at σ 0.5°", false, false),
        ("Report at σ 1°", false, false),
        ("Report at σ 2°", false, true),
        ("Report at σ 4°", false, false)
      )
    )
    // Δ by σ at the four protocol scales.
    assertEquals(
      runOnFx(w.summary.vm.slopes.deltas.map(_.scale)),
      Vector("0.5°", "1°", "2°", "4°")
    )
    fx.snapshot(StudioTheme.Light)
  }

  fxStage.test("the headline's paired descriptive reads +0.18, P15 to P11, 24 of 24") { fx =>
    val w = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
    loaded(fx, w)
    eventually(fx, "the paired headline is shown")(w.summary.headline.isVisible)
    val (texts, inScene, spoken) = runOnFx {
      val h = w.summary.headline
      (
        h.lookupAll(".label").asScala.toVector.collect { case l: Label => l.getText },
        h.getScene != null && w.summary.participantPlotNode.getChildren.get(0) == h,
        h.getAccessibleText
      )
    }
    assertEquals(
      texts,
      Vector(
        "+0.18",
        "Δ D, Remembered − Forgotten at σ 2°",
        "Higher for Remembered in 24 of 24 participants · range +0.04 (P15) to +0.34 (P11) · " +
          "descriptive, no tests"
      )
    )
    assert(inScene, "the headline sits over the participant plot in the window")
    assert(spoken.contains("mean difference +0.18"), spoken)
    fx.snapshot(StudioTheme.Light)
  }

  fxStage.test("the legend names P17 and the thin lines; the facts name P05's failures") { fx =>
    val w = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
    loaded(fx, w)
    val board = runOnFx(w.summary.board)
    assertEquals(
      board.legend.map {
        case eyes4s.studio.app.compare.SlopeKey.Mean(t)        => t
        case eyes4s.studio.app.compare.SlopeKey.Participant(t) => t
        case eyes4s.studio.app.compare.SlopeKey.Selected(t)    => t
        case eyes4s.studio.app.compare.SlopeKey.Thin(t)        => t
        case eyes4s.studio.app.compare.SlopeKey.Note(t)        => t
      },
      Vector(
        "mean of 24",
        "one participant",
        "P17, selected",
        "n<3 in a group",
        "σ in degrees, declared 35 px/° (not calibrated)"
      )
    )
    assertEquals(
      board.footer.drop(1),
      Vector(
        "P05: ret_04 (Forgotten), ret_11, ret_16 (Remembered) failed",
        "P17, P21: 2 Forgotten queries"
      )
    )
    assertEquals(runOnFx(w.summary.vm.slopes.thin), Set("P17", "P21"))
    fx.snapshot(StudioTheme.Light)
  }

  fxStage.test("Explain P17 carries the spec and the group into the query layout") { fx =>
    val w          = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
    val remembered = Response.Remembered
    val p17        = StudioRef.ReportParticipant(
      StoryMoments.run7,
      StoryModels.reporting,
      StoryModels.sigma2,
      ReportGroup.Level(remembered),
      ReportRole.Difference,
      "P17"
    )
    loaded(fx, w)
    dispatch(
      fx,
      w,
      StoryModels.select(runOnFx(w.runtime.model), "compare.participant-plot", p17)
    )
    eventually(fx, "Explain is offered")(w.summary.vm.explain.isDefined)
    assertEquals(
      runOnFx(w.summary.vm.explain.map(_.keeps)),
      Some("keeps: by retrieval response › Remembered › P17")
    )
    runOnFx(w.summary.explainNow())
    fx.awaitLayout()
    val trail = runOnFx(w.runtime.model.location.trail)
    assertEquals(
      trail.takeRight(3),
      Vector(
        Place.Summary(StoryModels.reporting),
        Place.Group(StoryModels.reporting, remembered),
        Place.At(p17)
      )
    )
    assertEquals(StudioLayouts.compareLayout(trail), CompareLayout.Query)
  }

  fxStage.test("P05's failed queries show no value; its means are drawn at their values") {
    fx =>
      val w = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
      loaded(fx, w)
      val failed = runOnFx(w.summary.vm.queries.flatMap(_.toOption)).toVector
        .flatMap(q => q.rows.map(q.cellsOf).filter(_.lift(3).contains("failed")))
      assertEquals(failed.size, 3)
      failed.foreach { r =>
        assert(r.head.startsWith("P05"), r)
        assertEquals(r.drop(4), Vector.fill(3)("—"))
      }
      // In the participant table P05 counts its 17 contributing queries.
      assertEquals(rows(w.summary.participantTable)(4).take(2), Vector("P05", "17"))
      // In the 2° panel P05's two means are placed at their values, never at
      // zero, and no participant mean is missing in the fixture.
      val plot = runOnFx(w.summary.participantPlot.plot).getOrElse(fail("no plot"))
      def position(group: Response): DataPoint =
        val ref = StudioRef.ReportParticipant(
          StoryMoments.run7,
          StoryModels.reporting,
          StoryModels.sigma2,
          ReportGroup.Level(group),
          ReportRole.Difference,
          "P05"
        )
        plot.marks
          .flatMap(_.rows)
          .find(_.ref == ref)
          .map(_.marking)
          .collect { case RowMarking.Placed(point) => point }
          .getOrElse(fail(s"No placed P05 mean for ${group.label}"))
      val ReportMeanTolerance = 1e-14
      val remembered          = position(Response.Remembered)
      val forgotten           = position(Response.Forgotten)
      assertEquals(remembered.x, 2 * SlopegraphPlot.Pitch)
      assertEquals(forgotten.x, 2 * SlopegraphPlot.Pitch + 1.0)
      assertEqualsDouble(remembered.y, 5.033771 / 13, ReportMeanTolerance)
      assertEqualsDouble(forgotten.y, 0.863573 / 4, ReportMeanTolerance)
      assertEquals(plot.unplotted, Vector.empty)
  }

  // --- Another run ------------------------------------------------------------------

  fxStage.test(
    "showing another run clears every pane until it is read, and says why it is not"
  ) { fx =>
    // Answers wait until the test gives them.
    val asked   = mutable.Map.empty[RunId, SummaryAnswer => Unit]
    val queried = mutable.Map.empty[RunId, QueriesAnswer => Unit]
    val inputs  = new SummaryInputs:
      def summary(run: RunId, done: SummaryAnswer => Unit): Unit = asked.update(run, done)
      def queries(run: RunId, done: QueriesAnswer => Unit): Unit = queried.update(run, done)
      override def report(
          run: RunId,
          spec: eyes4s.studio.core.document.ReportingSpec,
          scale: eyes4s.studio.core.selection.ScaleIndex,
          done: ReportAnswer => Unit
      ): Unit =
        given ExecutionContext = ExecutionContext.global
        HeadlessSession
          .open(StoryMoment.T3)
          .flatMap { session =>
            session
              .report(run, spec, scale.value)
              .transformWith(result => session.close.transform(_ => result))
          }
          .foreach(answer =>
            done(answer.fold(ReportAnswer.Refused(_), ReportAnswer.Answered(_)))
          )
      def inspect(
          run: RunId,
          address: eyes4s.studio.core.backend.ResultAddress,
          done: eyes4s.studio.app.compare.PairAnswer => Unit
      ): Unit = ()
      def ladder(
          run: RunId,
          query: eyes4s.studio.core.backend.TrialKey,
          scales: Vector[String],
          done: eyes4s.studio.app.compare.LadderAnswer => Unit
      ): Unit = ()
      def pairs(
          run: RunId,
          scale: eyes4s.studio.core.selection.ScaleIndex,
          done: eyes4s.studio.app.compare.PairsAnswer => Unit
      ): Unit = ()
    val none = new EffectPerformer:
      def perform(effect: AppEffect, dispatch: Intent => Unit): Unit = ()
    val runtime = StudioRuntime(StoryModels.t3Summary, none)
    val host    =
      runOnFx(CompareSummaryHost(() => runtime.model, i => runtime.dispatch(i), inputs))
    val box = runOnFx {
      val b = HBox(host.participantPlot.plotNode, host.participantNode)
      b.getStyleClass.add("es")
      b.getStylesheets.setAll(StudioStyles.stylesheets(Theme.Light).toOption.get*)
      b
    }
    fx.show(box)
    runOnFx(runtime.listen(host.sync))
    // Run 7, answered with the fake backend's own summary and queries.
    given ExecutionContext = ExecutionContext.global
    val page               = PageRequest.first(PageRequest.MaximumSize).toOption.get
    val (r7, q7)           = Await.result(
      HeadlessSession.open(StoryMoment.T3).flatMap { s =>
        s.result(StoryMoments.run7)
          .zip(s.queries(StoryMoments.run7, page))
          .transformWith(x => s.close.transform(_ => x))
      },
      Duration(60, "s")
    )
    runOnFx {
      asked(StoryMoments.run7)(SummaryAnswer.Answered(r7.toOption.get))
      queried(StoryMoments.run7)(QueriesAnswer.Answered(q7.toOption.get.rows))
    }
    eventually(fx, "run 7 is drawn") {
      host.participantPlot.status.get.isInstanceOf[PlotTwinStatus.Shown] &&
      host.participantTable.rowCount == 24
    }
    // A theme change redraws the plots in the new theme, from the same sources.
    val previousSource       = runOnFx(host.vm.participants.get.toOption.get)
    def sceneOf(t: PlotTwin) = runOnFx(t.plot.map(_.plot.id.value))
    assert(
      sceneOf(host.participantPlot).exists(_.endsWith(".light")),
      sceneOf(host.participantPlot)
    )
    runOnFx(
      runtime.dispatch(
        Intent.Dispatch(Command.SetTheme(eyes4s.studio.core.document.Theme.Dark))
      )
    )
    assert(
      sceneOf(host.participantPlot).exists(_.endsWith(".dark")),
      sceneOf(host.participantPlot)
    )
    assert(runOnFx(host.vm.participants.get.toOption.get eq previousSource))
    runOnFx(
      runtime.dispatch(
        Intent.SwitchPerspective(eyes4s.studio.core.document.Perspective.Results)
      )
    )
    assert(runOnFx(host.vm.participants.get.toOption.get eq previousSource))
    assertEquals(
      runOnFx(host.vm.freshness),
      runOnFx(eyes4s.studio.app.vm.Shell.context(runtime.model).newer.map(_.text))
    )
    // Run 5 is shown: while it is read, no pane keeps run 7's values.
    runOnFx(runtime.dispatch(Intent.Dispatch(Command.ShowRun(Some(StoryMoments.run5)))))
    def cleared(): Unit = runOnFx {
      assertEquals(host.participantPlot.status.get, PlotTwinStatus.Empty)
      assertEquals(host.participantTable.rowCount, 0)
    }
    cleared()
    assertEquals(runOnFx(host.vm.status), Some("Reading run 5…"))
    // Run 5 is refused: still nothing of run 7, and the status says why.
    runOnFx(
      asked(StoryMoments.run5)(
        SummaryAnswer.Refused(
          BackendError.UnknownRun(StoryMoments.run5, Vector(StoryMoments.run7))
        )
      )
    )
    cleared()
    val said = runOnFx(
      host.participantNode.lookupAll(".label").asScala.toVector.collect { case l: Label =>
        l.getText
      }
    )
    assert(said.exists(_.startsWith("Run 5 could not be read")), said)
    runOnFx(host.dispose())
  }

  private lazy val savedUngroupedSummary: eyes4s.studio.app.AppModel =
    val original = StoryModels.t3Summary.document.reporting.head
    val spec     = eyes4s.studio.core.document.ReportingSpec
      .of(
        original.id,
        "Saved ungrouped",
        None,
        original.filters,
        original.minimumPerGroup,
        eyes4s.studio.core.document.ReportingWeight.PooledQueries
      )
      .fold(e => fail(e.message), identity)
    eyes4s.studio.app.AppModel
      .update(StoryModels.t3Summary, Intent.Dispatch(Command.PutReporting(spec)))
      ._1

  Vector(
    "derived overall" -> (() => StoryModels.t3Summary),
    "saved ungrouped" -> (() => savedUngroupedSummary)
  ).foreach { (context, initial) =>
    fxStage.test(
      s"a whole participant keeps its exact spec through Explain ($context)"
    ) { fx =>
      val w = boot(fx, initial(), StoryMoment.T3)
      loaded(fx, w)
      val table = runOnFx(w.summary.participantTable.source)
        .getOrElse(fail("no returned participant table"))
      val person = table.rows.head.ref
      // The whole participant's own spec: the saved one, or its derived overall.
      val expected = person match
        case StudioRef.ReportParticipant(
              _,
              id,
              _,
              ReportGroup.Whole,
              ReportRole.Difference,
              _
            ) =>
          eyes4s.studio.app.compare.CompareSummary
            .reportingSpec(runOnFx(w.runtime.model.document), id)
            .getOrElse(fail(s"$id must resolve"))
        case other => fail(s"not a whole participant: $other")
      assert(expected.groupBy.isEmpty)
      val root = eyes4s.studio.app.compare.CompareSummary.reporting(runOnFx(w.runtime.model))
      dispatch(
        fx,
        w,
        StoryModels.select(runOnFx(w.runtime.model), "compare.participant-table", person)
      )
      eventually(fx, "the whole participant has an accessible Explain action")(
        w.summary.vm.explain.exists(_.keeps.contains("All queries"))
      )
      runOnFx(w.summary.explainNow())
      val explained = runOnFx(w.runtime.model)
      assertEquals(explained.selection.selected, Vector(person))
      assert(explained.location.trail.contains(Place.At(person)), explained.location)
      // The trail keeps the summary's own spec; the person's ref keeps its own.
      assertEquals(eyes4s.studio.app.compare.CompareSummary.reporting(explained), root)
      assertEquals(
        eyes4s.studio.app.compare.CompareSummary.reportingSpec(explained.document, expected.id),
        Some(expected)
      )
    }

  }
