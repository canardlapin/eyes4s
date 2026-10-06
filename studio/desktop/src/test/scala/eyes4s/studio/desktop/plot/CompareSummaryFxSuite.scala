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
import eyes4s.studio.viz.plot.{DataPoint, RowMarking}
import javafx.scene.control.{Label, ToggleButton}
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
    * contributing queries at 2°. P24 Forgotten rounds to .18 from these
    * stored values; the older illustrative summary rounds before storage.
    */
  private val fixtureTable: Vector[Vector[String]] = Vector(
    "P01 | 20 | 19 | 0 | 0 | 1 | 0.52 | 0.35 | +0.17 | +0.26 (11) | +0.06 (8)",
    "P02 | 20 | 19 | 0 | 0 | 1 | 0.58 | 0.36 | +0.22 | +0.27 (13) | +0.12 (6)",
    "P03 | 20 | 19 | 0 | 1 | 0 | 0.68 | 0.35 | +0.33 | +0.38 (15) | +0.15 (4)",
    "P04 | 20 | 19 | 0 | 0 | 1 | 0.65 | 0.36 | +0.29 | +0.36 (12) | +0.18 (7)",
    "P05 | 20 | 17 | 3 | 0 | 0 | 0.27 | 0.34 | -0.08 | -0.03 (13) | -0.24 (4)",
    "P06 | 20 | 19 | 0 | 0 | 1 | 0.59 | 0.34 | +0.25 | +0.30 (11) | +0.17 (8)",
    "P07 | 20 | 19 | 0 | 1 | 0 | 0.62 | 0.35 | +0.27 | +0.32 (15) | +0.10 (4)",
    "P08 | 20 | 19 | 0 | 1 | 0 | 0.61 | 0.35 | +0.25 | +0.31 (12) | +0.15 (7)",
    "P09 | 20 | 19 | 0 | 0 | 1 | 0.67 | 0.35 | +0.32 | +0.34 (15) | +0.24 (4)",
    "P10 | 20 | 19 | 0 | 1 | 0 | 0.78 | 0.35 | +0.43 | +0.45 (16) | +0.33 (3)",
    "P11 | 20 | 19 | 0 | 1 | 0 | 0.57 | 0.35 | +0.22 | +0.28 (13) | +0.11 (6)",
    "P12 | 20 | 19 | 0 | 0 | 1 | 0.64 | 0.35 | +0.28 | +0.34 (12) | +0.19 (7)",
    "P13 | 20 | 19 | 0 | 0 | 1 | 0.47 | 0.36 | +0.11 | +0.17 (11) | +0.02 (8)",
    "P14 | 20 | 19 | 0 | 1 | 0 | 0.62 | 0.35 | +0.27 | +0.29 (14) | +0.22 (5)",
    "P15 | 20 | 19 | 0 | 1 | 0 | 0.45 | 0.35 | +0.10 | +0.18 (13) | -0.08 (6)",
    "P16 | 20 | 19 | 0 | 0 | 1 | 0.74 | 0.35 | +0.38 | +0.44 (12) | +0.28 (7)",
    "P17 | 20 | 19 | 0 | 0 | 1 | 0.73 | 0.35 | +0.38 | +0.38 (17) | +0.32 (2)",
    "P18 | 20 | 19 | 0 | 1 | 0 | 0.76 | 0.35 | +0.41 | +0.42 (16) | +0.37 (3)",
    "P19 | 20 | 19 | 0 | 0 | 1 | 0.59 | 0.35 | +0.24 | +0.29 (15) | +0.06 (4)",
    "P20 | 20 | 19 | 0 | 0 | 1 | 0.59 | 0.37 | +0.22 | +0.26 (14) | +0.12 (5)",
    "P21 | 20 | 19 | 0 | 1 | 0 | 0.67 | 0.35 | +0.31 | +0.32 (17) | +0.27 (2)",
    "P22 | 20 | 19 | 0 | 0 | 1 | 0.54 | 0.35 | +0.19 | +0.27 (12) | +0.06 (7)",
    "P23 | 20 | 19 | 0 | 0 | 1 | 0.64 | 0.34 | +0.30 | +0.34 (14) | +0.21 (5)",
    "P24 | 20 | 19 | 0 | 0 | 1 | 0.65 | 0.34 | +0.30 | +0.36 (13) | +0.18 (6)"
  ).map(_.split('|').toVector.map(_.trim))

  private def ascii(s: String): String = s.replace(Format.Minus, "-")

  // The summary's tables sit in tabs not shown here: their rows are read
  // from the tables' state (TableTwinViewFxSuite covers the drawn rows).
  private def rows(t: TableTwinView): Vector[Vector[String]] =
    runOnFx(t.modelRowTexts.map(_.map(ascii)))

  private def labels(w: StudioWindow): Vector[String] =
    runOnFx(
      w.summary.participantNode
        .lookupAll(".label")
        .asScala
        .toVector
        .collect { case l: Label => l.getText }
    )

  private def loaded(fx: FxStage, w: StudioWindow): Unit =
    eventually(fx, "the 2° report is available")(
      w.summary.vm.scales.exists(c => c.scale == StoryModels.sigma2 && c.available)
    )
    runOnFx {
      val two = w.summary.participantNode
        .lookupAll(".toggle-button")
        .asScala
        .collectFirst {
          case b: ToggleButton if b.getText == "σ 2°" => b
        }
        .getOrElse(fail("no 2° control"))
      two.fire()
    }
    eventually(fx, "the summary and its queries are read") {
      val vm = w.summary.vm
      vm.participants.exists(_.isRight) && vm.queries.exists(_.isRight) &&
      w.summary.participantPlot.status.get.isInstanceOf[PlotTwinStatus.Shown] &&
      w.summary.scaleProfile.status.get.isInstanceOf[PlotTwinStatus.Shown] &&
      w.summary.queryTable.rowCount > 0
    }

  fxStage.test("participant table and plot match the stored query values") { fx =>
    val w = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
    loaded(fx, w)
    assertEquals(rows(w.summary.participantTable), fixtureTable)
    // The plot: 24 participants in each group and both grand means, each
    // saying its group's n.
    val plot = runOnFx(w.summary.participantPlot.plot).getOrElse(fail("no plot"))
    assertEquals(plot.marks.size, 50)
    val grand =
      plot.marks.filter(_.ref.isInstanceOf[StudioRef.ReportCell]).flatMap(plot.readout)
    assertEquals(
      grand,
      Vector(
        "Group Remembered, Mean of all participants, D +0.30, n 24 participants",
        "Group Forgotten, Mean of all participants, D +0.15, n 24 participants"
      )
    )
    // The notes, the newer run's freshness and the σ selector.
    val shown = labels(w)
    Vector(
      "Means: equal participant weight",
      "Per-group n: 2–17 queries per participant",
      "Rev 5 · run 8 running · 48%"
    ).foreach(t => assert(shown.contains(t), s"'$t' not shown in $shown"))
    assert(!shown.exists(_.startsWith("Paired n")), "the story spec declares no contrast")
    val toggles = runOnFx(
      w.summary.participantNode.lookupAll(".toggle-button").asScala.toVector.collect {
        case b: ToggleButton => (b.getText, b.isDisable, b.isSelected)
      }
    )
    assertEquals(
      toggles,
      Vector(
        ("σ 0.5°", false, false),
        ("σ 1°", false, false),
        ("σ 2°", false, true),
        ("σ 4°", false, false)
      )
    )
    // The scale profile at the four protocol scales.
    val profile = runOnFx(w.summary.scaleProfile.plot).getOrElse(fail("no profile"))
    assertEquals(profile.source.rows.size, (2 + 24) * 4)
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
    val shown = labels(w)
    assert(shown.contains("keeps: by retrieval response › Remembered › P17"), shown)
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
      val failed = rows(w.summary.queryTable).filter(_.lift(3).contains("failed"))
      assertEquals(failed.size, 3)
      failed.foreach { r =>
        assert(r.head.startsWith("P05"), r)
        assertEquals(r.drop(4), Vector.fill(3)("—"))
      }
      // In the participant table P05's three failed queries are counted, not scored.
      assertEquals(rows(w.summary.participantTable)(4).take(4), Vector("P05", "20", "17", "3"))
      // In the plot P05's two means are placed at their values, never at zero,
      // and no participant mean is missing in the fixture.
      val plot            = runOnFx(w.summary.participantPlot.plot).getOrElse(fail("no plot"))
      def at(g: Response) =
        plot
          .markOf(
            StudioRef.ReportParticipant(
              StoryMoments.run7,
              StoryModels.reporting,
              StoryModels.sigma2,
              ReportGroup.Level(g),
              ReportRole.Difference,
              "P05"
            )
          )
          .map(_.rows.map(_.marking))
      def position(group: Response): DataPoint =
        at(group)
          .flatMap(_.headOption)
          .collect { case RowMarking.Placed(point) =>
            point
          }
          .getOrElse(fail(s"No placed P05 mean for ${group.label}"))
      val ReportMeanTolerance = 1e-14
      val remembered          = position(Response.Remembered)
      val forgotten           = position(Response.Forgotten)
      assertEquals(remembered.x, 0.0)
      assertEquals(forgotten.x, 1.0)
      assertEqualsDouble(remembered.y, -0.34 / 13, ReportMeanTolerance)
      assertEqualsDouble(forgotten.y, -0.94 / 4, ReportMeanTolerance)
      assert(!plot.marks.exists(_.rows.exists(_.marking.isInstanceOf[RowMarking.Positionless])))
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
      val b = HBox(
        host.participantPlot.plotNode,
        host.scaleProfile.plotNode,
        host.participantNode,
        host.queryTable
      )
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
      host.participantTable.rowCount == 24 && host.queryTable.rowCount > 0 &&
      host.scaleProfile.status.get.isInstanceOf[PlotTwinStatus.Shown]
    }
    // A theme change redraws the plots in the new theme, from the same sources.
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
    assert(sceneOf(host.scaleProfile).exists(_.endsWith(".dark")), sceneOf(host.scaleProfile))
    // Run 5 is shown: while it is read, no pane keeps run 7's values.
    runOnFx(runtime.dispatch(Intent.Dispatch(Command.ShowRun(Some(StoryMoments.run5)))))
    def cleared(): Unit = runOnFx {
      assertEquals(host.participantPlot.status.get, PlotTwinStatus.Empty)
      assertEquals(host.scaleProfile.status.get, PlotTwinStatus.Empty)
      assertEquals(host.participantTable.rowCount, 0)
      assertEquals(host.queryTable.rowCount, 0)
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
