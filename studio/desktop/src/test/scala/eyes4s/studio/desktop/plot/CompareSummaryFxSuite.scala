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
import eyes4s.studio.app.nav.{Location, Place}
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
import eyes4s.studio.app.plot.ParticipantNotes
import eyes4s.studio.viz.plot.{Annotations, DataPoint, ParticipantPlot, RowMarking}
import javafx.scene.control.{Label, ToggleButton}
import javafx.geometry.Point2D
import javafx.scene.input.{MouseButton, MouseEvent, PickResult}
import javafx.scene.layout.{HBox, Region}

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
      vm.scales.nonEmpty && vm.scales.forall(_.available) &&
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
        "Group Remembered, Mean of all participants, D +0.43, n 24 participants",
        "Group Forgotten, Mean of all participants, D +0.24, n 24 participants"
      )
    )
    // The notes, the newer run's freshness and the σ selector.
    val shown = labels(w)
    Vector(
      "Means: equal participant weight",
      "Per-group n: 2–17 queries per participant",
      "Rev 5 · run 8 running · 48%"
    ).foreach(t => assert(shown.contains(t), s"'$t' not shown in $shown"))
    // t3's spec declares Remembered − Forgotten (design review ticket 27):
    // the paired n note and the paired headline.
    assert(shown.contains("Paired n = 24 participants"), shown)
    eventually(fx, "the paired headline is shown")(w.summary.headline.isVisible)
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
    assertEquals(profile.source.rows.size, (1 + 24) * 4)
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
        "Δ Remembered − Forgotten · paired n 24",
        "+0.18",
        "higher in 24 of 24",
        "Range +0.04 P15 to +0.34 P11 · computed before rounding participant means · " +
          "Descriptive only, no intervals or tests"
      )
    )
    assert(inScene, "the headline sits over the participant plot in the window")
    assert(spoken.contains("mean difference +0.18"), spoken)
    fx.snapshot(StudioTheme.Light)
  }

  fxStage.test("the participant plot's annotation column: P17 selected and P05's failures") {
    fx =>
      assumeFullStage(fx)
      val w = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
      loaded(fx, w)
      eventually(fx, "the plot is drawn with its notes")(
        runOnFx(w.summary.participantPlot.plot).exists(_.description.contains("Annotations:"))
      )
      val (plot, spoken) = runOnFx(
        (
          w.summary.participantPlot.plot.getOrElse(fail("no plot")),
          w.summary.participantPlot.plotHost.getAccessibleText
        )
      )
      def all(g: intaglio.Grob): Vector[intaglio.Grob] = g +: g.children.flatMap(all)
      val drawn = plot.plot.scene.grobs.flatMap(all).collect { case t: intaglio.Grob.Text =>
        t.label
      }
      // The board's ten lines, the group means as served.
      val notes = Vector(
        "P17 · selected",
        "19 of 20 contributing",
        "Rem +0.40 (n 17)",
        "Forg +0.22 (n 2)",
        "P05 · 17 of 20 contributing",
        "Rem +0.39 (n 13)",
        "Forg +0.22 (n 4)",
        "3 failed queries:",
        "2 Remembered, 1 Forgotten",
        "no value, not zero"
      )
      assertEquals(drawn.filter(notes.contains), notes)
      // The marks are unchanged by the column: 24 participants twice, two grand means.
      assertEquals(plot.marks.size, 50)
      val said =
        "Annotations: P17 · selected, 19 of 20 contributing, Remembered +0.40, n 17, " +
          "Forgotten +0.22, n 2; P05 · 17 of 20 contributing, Remembered +0.39, n 13, " +
          "Forgotten +0.22, n 4, 3 failed queries, 2 Remembered, 1 Forgotten, no value, not zero"
      assert(plot.description.contains(said), plot.description)
      assert(Option(spoken).exists(_.contains(said)), s"accessible text: $spoken")
      fx.snapshot(StudioTheme.Light)
  }

  fxStage.test("a short participant plot writes only the notes its frame holds; taller, more") {
    fx =>
      assumeFullStage(fx)
      val w    = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
      val twin = w.summary.participantPlot
      loaded(fx, w)
      val p17 = Vector(
        "P17 · selected",
        "19 of 20 contributing",
        "Rem +0.40 (n 17)",
        "Forg +0.22 (n 2)"
      )
      val p05 = Vector(
        "P05 · 17 of 20 contributing",
        "Rem +0.39 (n 13)",
        "Forg +0.22 (n 4)",
        "3 failed queries:",
        "2 Remembered, 1 Forgotten",
        "no value, not zero"
      )
      val written                                      = (p17 ++ p05 :+ "+1 more").toSet
      def all(g: intaglio.Grob): Vector[intaglio.Grob] = g +: g.children.flatMap(all)
      // The note lines the plot now draws, and its host's height.
      def drawn(): (Vector[String], Double) = runOnFx(
        (
          twin.plot.toVector
            .flatMap(_.plot.scene.grobs.flatMap(all))
            .collect { case t: intaglio.Grob.Text if written(t.label) => t.label },
          twin.plotHost.getHeight
        )
      )
      // Both blocks and the gap between them take 11 slots.
      val both = p17.size + ParticipantNotes.BlockGapSlots + p05.size
      // A pane 120 px tall: its frame holds 6 slots, fewer than both blocks,
      // so the board's constant 15 slots would have written below it.
      val short = 120.0
      runOnFx(twin.plotNode.setMaxHeight(short))
      eventually(fx, "the short plot draws P17 and what is left")(
        drawn()._1 == (p17 :+ "+1 more")
      )
      val (_, height) = drawn()
      val frame       = ParticipantPlot.frameHeightPx(height)
      // The canvas sits inside the pane's padding.
      assert(height > 0.0 && height <= short, s"$height px")
      assert(both * Annotations.SlotPx > frame, s"$both slots fit a $frame px frame")
      assert((p17.size + 1) * Annotations.SlotPx <= frame, s"$frame px frame")
      assertEquals(runOnFx(w.summary.vm.plotNotes.omitted), 1)
      val said = runOnFx(twin.plot.map(_.description)).getOrElse(fail("no plot"))
      assert(said.contains("1 more participant not annotated"), said)
      assert(!said.contains("P05 · 17 of 20 contributing"), said)
      fx.snapshot(StudioTheme.Light)
      // Taller again: the column has room for both blocks.
      runOnFx(twin.plotNode.setMaxHeight(Region.USE_COMPUTED_SIZE))
      eventually(fx, "the taller plot draws both blocks")(drawn()._1 == p17 ++ p05)
      val (_, tall) = drawn()
      assert(both * Annotations.SlotPx <= ParticipantPlot.frameHeightPx(tall), s"$tall px")
      assertEquals(runOnFx(w.summary.vm.plotNotes.omitted), 0)
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
      assertEqualsDouble(remembered.y, 5.033771 / 13, ReportMeanTolerance)
      assertEqualsDouble(forgotten.y, 0.863573 / 4, ReportMeanTolerance)
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
    assert(sceneOf(host.scaleProfile).exists(_.endsWith(".dark")), sceneOf(host.scaleProfile))
    assert(runOnFx(host.vm.participants.get.toOption.get eq previousSource))
    runOnFx(
      runtime.dispatch(
        Intent.SwitchPerspective(eyes4s.studio.core.document.Perspective.Explore)
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
      s"the Results grand mean keeps exact cell, line, table and navigation context on return ($context)"
    ) { fx =>
      val w = boot(fx, initial(), StoryMoment.T3)
      loaded(fx, w)
      val twin = w.summary.scaleProfile
      val plot = runOnFx(twin.plot).getOrElse(fail("no profile plot"))
      val cell = plot.source.rows
        .take(4)
        .map(_.ref)
        .collectFirst {
          case ref @ StudioRef.ReportCell(_, _, scale, ReportGroup.Whole, ReportRole.Difference)
              if scale == StoryModels.sigma2 =>
            ref
        }
        .getOrElse(fail("no whole-report 2° cell"))
      val expected = eyes4s.studio.app.compare.CompareSummary
        .reportingSpec(runOnFx(w.runtime.model.document), cell.reporting)
        .getOrElse(fail("the curve's reporting identity must resolve"))
      assert(expected.groupBy.isEmpty)
      def targets = runOnFx {
        twin.plotHost.status.get match
          case PlotHostStatus.Drawn(frame) if frame.plan.scene eq plot.plot.scene =>
            twin.input.targets
          case _ => None
      }
      eventually(fx, "the grand mean's actual canvas is drawn")(
        targets.exists(_.target(cell).isDefined)
      )
      val target = targets.get
      val at     = target.transform.deviceToCanvas(target.target(cell).get.anchor)
      val scene  = runOnFx(twin.plotHost.localToScene(Point2D(at.x, at.y)))
      runOnFx {
        Vector(MouseEvent.MOUSE_PRESSED, MouseEvent.MOUSE_RELEASED, MouseEvent.MOUSE_CLICKED)
          .foreach { kind =>
            eyes4s.studio.desktop.harness.StudioRobot.fireEvent(
              twin.plotHost,
              MouseEvent(
                kind,
                scene.getX,
                scene.getY,
                scene.getX,
                scene.getY,
                MouseButton.PRIMARY,
                1,
                false,
                false,
                false,
                false,
                kind == MouseEvent.MOUSE_PRESSED,
                false,
                false,
                true,
                false,
                true,
                PickResult(twin.plotHost, scene.getX, scene.getY)
              )
            )
          }
      }
      eventually(fx, "exact grand-mean cell selected")(
        w.runtime.model.selection.selected == Vector(cell)
      )
      val selected = runOnFx(w.runtime.model)
      val label    = eyes4s.studio.app.vm
        .Labels(selected, eyes4s.studio.app.text.Messages.english)
        .place(Place.At(cell), current = true)
      assert(label.contains("All queries") && label.contains("D"), label)
      dispatch(fx, w, Intent.Explain(Place.At(cell)))
      val location = runOnFx(w.runtime.model.location)
      assert(location.trail.contains(Place.Summary(expected.id)), location)
      assert(location.trail.contains(Place.At(cell)), location)
      dispatch(
        fx,
        w,
        Intent.Navigate(
          Location(
            eyes4s.studio.core.document.Perspective.Explore,
            Vector(Place.At(StudioRef.Participant("P17")))
          )
        )
      )
      dispatch(fx, w, Intent.Navigate(location))
      loaded(fx, w)
      val returned = runOnFx(w.runtime.model)
      assertEquals(
        eyes4s.studio.app.compare.CompareSummary.reporting(returned),
        Some(expected.id)
      )
      assertEquals(
        eyes4s.studio.app.compare.CompareSummary.reportingSpec(returned.document, expected.id),
        Some(expected)
      )
      val source =
        runOnFx(w.summary.scaleProfile.plot).getOrElse(fail("no returned profile")).source
      assert(source.rows.take(4).exists(_.ref == cell), source.rows.take(4).map(_.ref))
      assert(
        source.rows
          .take(4)
          .forall(_.ref match
            case StudioRef.ReportCell(_, id, _, ReportGroup.Whole, ReportRole.Difference) =>
              id == expected.id
            case _ => false)
      )
      assertEquals(returned.location, location)
      val table = runOnFx(w.summary.participantTable.source)
        .getOrElse(fail("no returned participant table"))
      val lineRefs = source.rows.map(_.ref).collect {
        case ref @ StudioRef.ReportParticipant(
              _,
              _,
              scale,
              ReportGroup.Whole,
              ReportRole.Difference,
              _
            ) if scale == StoryModels.sigma2 =>
          ref
      }
      assertEquals(table.rows.map(_.ref).toSet, lineRefs.toSet)
      val person = table.rows.head.ref
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
      assertEquals(
        eyes4s.studio.app.compare.CompareSummary.reporting(explained),
        Some(expected.id)
      )
      assertEquals(
        eyes4s.studio.app.compare.CompareSummary.reportingSpec(explained.document, expected.id),
        Some(expected)
      )
    }

  }
