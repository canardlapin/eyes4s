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

package eyes4s.studio.app.driver

import eyes4s.studio.app.figures.{ComposerEffect, ComposerIntent, FigureComposer, NewPanel}

import cats.syntax.all.*
import eyes4s.studio.app.{AppModel, Intent, PlatformDialog, StoryModels, TrialItems}
import eyes4s.studio.app.nav.{DataSection, Location, Place, Provenance, SummaryScope}
import eyes4s.studio.app.vm.{FreshnessTone, JobsChipState}
import eyes4s.studio.core.backend.{Provenance as _, *}
import eyes4s.studio.core.bundle.BundleSamples
import eyes4s.studio.core.command.{Command, Reducer}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.execution.JobPhase
import eyes4s.studio.core.fixture.{MockStudy, StoryMoments}
import eyes4s.studio.core.headless.{HeadlessError, HeadlessSession}
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}

import scala.concurrent.{ExecutionContext, Future}

/** The E2E-01 golden journey (tickets S3.6 and S10.1) as named stages, so the
  * headless suites run it whole and the S10.1 suite inserts its checks
  * between the stages:
  *
  * new project → import the fixture → map columns → admit r3 → explore P17
  * enc_03 → Analysis rev 4 → run → Compare P17 ret_07 → summary → figure →
  * export → close → reopen.
  *
  * Stubbed, and recorded as such in the driver's record:
  *  - the import dialog: a platform dialog, answered with the fixture's
  *    sources; the fake's history already holds the fixture as r2 with rev 3
  *    and run 5 (FIXTURE.md), so the journey re-imports it as r3 there. A
  *    fresh project cannot reach the fixture's scored ids: the fake serves
  *    r3 only, and no command creates a first analysis revision yet (S7.x
  *    presets). The new-project state itself is checked first.
  *  - figure composing and export in [[Stages.figureAndClose]]; the S10.1
  *    suite composes and exports for real instead.
  *
  * The run is the journey's own: rev 4 on r3 is run 6 here (the story's run
  * 7 is its second run; there is no rerun command), and the fake serves the
  * fixture's scores for any completed run of rev 4 on r3.
  */
object GoldenJourney:
  import StoryModels.{ok, p17enc03, p17ret07, sigma2}

  /** The journey's stages, in order. */
  final case class Stages(
      newProject: Scenario[Future],
      importAndAdmit: Scenario[Future],
      explore: Scenario[Future],
      analysisAndRun: Scenario[Future],
      compare: Scenario[Future],
      summary: Scenario[Future],
      figureAndClose: Scenario[Future]
  ):
    def all: Scenario[Future] =
      newProject ++ importAndAdmit ++ explore ++ analysisAndRun ++ compare ++ summary ++
        figureAndClose

  given ExecutionContext = ExecutionContext.global

  val study   = ok(MockStudy.load)
  val fixture = study.summary
  val focus   = study.queries.find(q => q.participant == "P17" && q.trial == "ret_07").get

  val r2      = StoryMoments.r2
  val r3      = StoryMoments.r3
  val rev3    = StoryMoments.rev3
  val rev4    = StoryMoments.rev4
  val run6    = RunId(6)
  val project = StoryModels.project

  val reporting: ReportingId = StoryModels.reporting
  val remembered             = Response.Remembered
  val page                   = ok(PageRequest.first(PageRequest.MaximumSize))

  type S = Step[Future]

  def fail[A](step: String, expected: String, actual: Any): Either[DriverError, A] =
    Left(DriverError.Expectation(step, expected, actual.toString))

  def expect[A](step: String, expected: A, actual: A): Either[DriverError, Unit] =
    if expected == actual then Right(()) else fail(step, expected.toString, actual)

  def check(name: String)(p: StudioDriver => Either[DriverError, Unit]): S =
    Step.check[Future](name)(p)

  def sync(name: String)(f: StudioDriver => Either[DriverError, StudioDriver]): S =
    Step.pure[Future](name)(f)

  /** A backend read as a step's result; a refusal fails the step, typed. */
  def service[A](name: String)(
      fa: Future[Either[BackendError, A]]
  ): Future[Either[DriverError, A]] =
    fa.map(_.leftMap(e => DriverError.Service(name, ServiceError.Backend(e))))

  /** A fake control call as a step's result. */
  def control[A](name: String)(
      fa: Future[Either[HeadlessError, A]]
  ): Future[Either[DriverError, A]] =
    fa.map(_.leftMap(e => DriverError.Service(name, ServiceError.Headless(e))))

  /** Feed execution events until the driver's state satisfies `done`: the
    * events already published first, then each new one as it arrives. The
    * state, not a particular event, is awaited, since a settle may already
    * have fed the event.
    */
  def await(session: HeadlessSession, name: String)(done: StudioDriver => Boolean): S =
    def loop(d: StudioDriver): Future[Either[DriverError, StudioDriver]] =
      if done(d) then Future.successful(Right(d))
      else
        session.awaitEvent(_ => true).flatMap {
          case Left(e) =>
            Future.successful(Left(DriverError.Service(name, ServiceError.Headless(e))))
          case Right(events) => loop(d.feed(events))
        }
    Step(name, d => session.events.flatMap(events => loop(d.feed(events))))

  def run6Job(d: StudioDriver): Either[DriverError, JobId] =
    d.model.jobs.jobs
      .find(_.run == run6)
      .map(_.id)
      .toRight(
        DriverError.Expectation("run 6's job", "a tracked job", d.model.jobs.jobs.toString)
      )

  /** Explain one level further down: the child `pick` chooses among the
    * navigator's children of the trail's last place.
    */
  def drill(session: HeadlessSession, name: String)(
      pick: Vector[Place] => Option[Place]
  ): S =
    Step(
      name,
      d =>
        val scope = d.model.document.presentation.shownRun.map(SummaryScope(_, sigma2))
        scope match
          case None     => Future.successful(fail(name, "a shown run", "none"))
          case Some(sc) =>
            Provenance.children(session.navigator, d.model.location.trail.last, sc, page).map {
              case Left(e) => Left(DriverError.Service(name, ServiceError.Navigation(e)))
              case Right(children) =>
                pick(children)
                  .toRight(
                    DriverError.Expectation(name, "a child to explain", children.toString)
                  )
                  .flatMap(p => d.perform(name, Intent.Explain(p)))
            }
    )

  def crumbs(step: String, expected: String*): S =
    check(step)(d => expect(step, expected.toVector, d.crumbs))

  // -------------------------------------------------------------------------

  /** The fixture as the fake's history holds it before the re-import: t1
    * without its pending r3.
    */
  lazy val history: StudioDocument =
    ok(Reducer.step(StoryModels.t1, Command.DiscardDataset(r3)).map(_._1))

  def stages(session: HeadlessSession): GoldenJourney.Stages =
    val t1r3   = StoryModels.t1.dataset(r3).get
    val r2Spec = StoryModels.t1.dataset(r2).get

    val newProject = Scenario.of[Future](
      crumbs("new project: trail", "New project"),
      check("new project: badge")(d =>
        expect("badge", "No dataset · no analysis", d.context.freshness.text)
      ),
      Step.invoke[Future](eyes4s.studio.app.keys.CommandRegistry.importSources.id),
      Step.settle[Future](session),
      check("import dialog requested")(d =>
        expect(
          "dialog",
          Vector(PlatformDialog.ImportSources),
          d.records.collect { case DriverRecord.Dialog(x) => x }
        )
      )
    )

    val importAndAdmit = Scenario.of[Future](
      Step.stub[Future](
        "import dialog",
        "no platform dialog headless; the fake's history holds the fixture as r2 (FIXTURE.md)"
      ),
      sync("open the fixture's history")(d =>
        Right(d.openProject(AppModel.open(history, Some(project))))
      ),
      Step.command[Future](
        Command.ImportSources(
          Some(r2),
          r2Spec.sources,
          r2Spec.mapping,
          r2Spec.units,
          r2Spec.geometry,
          DeclaredAttributes.empty,
          None,
          // trials.csv mapped as r3 maps it: Block → occurrence (S5.4).
          t1r3.inventory
        )
      ),
      Step.intent[Future](
        "open column mapping",
        Intent.Navigate(
          Location(
            Perspective.Data,
            Vector(Place.Dataset(r3), Place.DataView(DataSection.ColumnMapping))
          )
        )
      ),
      crumbs("r3 is a draft", "Dataset r3 (draft)", "Column mapping"),
      // Map columns: Block → occurrence and onsets declared in ms (FIXTURE.md r3).
      Step.command[Future](Command.SetMapping(r3, t1r3.mapping)),
      Step.command[Future](Command.SetUnits(r3, DeclaredUnits(Some(TimeUnit.Milliseconds)))),
      check("r3 is the story's r3")(d =>
        expect(
          "r3",
          Some((t1r3.mapping, t1r3.units)),
          d.model.document.dataset(r3).map(s => (s.mapping, s.units))
        )
      ),
      Step.command[Future](Command.VerifyDataset(r3)),
      Step.settle[Future](session),
      check("admission summary = FIXTURE.md")(d =>
        d.admissions.lastOption match
          case None    => fail("admission", "a summary", d.records)
          case Some(a) =>
            for
              _ <- expect("inventory trials", Some(960), a.inventoryTrials)
              _ <- expect("admitted", 937, a.admitted)
              _ <- expect("quarantined", 17, a.quarantinedTrials + a.noFixations)
              _ <- expect("absent", Some(6), a.absent)
              _ <- expect("fixation records", 11520, a.fixationRecords)
              _ <- expect("source records", Some(11520), a.window.sourceRecords)
              _ <- expect("images missing", 2, a.missingImages.size)
              _ <- expect("dataset", r3, a.dataset)
            yield ()
      ),
      sync("admit r3")(d =>
        d.model.document.dataset(r3).map(_.decision) match
          case Some(AdmissionDecision.Verifying(content)) =>
            d.command(
              Command.Admit(
                r3,
                content,
                Some(eyes4s.plan.AdmissionDecision.ReviewExclusions),
                CoreBinding.unbound,
                CoreBinding.unbound
              )
            )
          case other => fail("admit", "r3 verifying", other)
      ),
      check("run 5 is stale once r3 is admitted")(d =>
        expect("tone", FreshnessTone.Stale, d.context.freshness.tone)
      ),
      Step(
        "load item labels from the ledger",
        d =>
          service("ledger")(session.ledger(r3, page)).map(_.flatMap { ledger =>
            expect("ledger trials", 960, ledger.page.total).as(
              d.dispatch(
                Intent.ItemsLoaded(TrialItems(ledger.entries.map(e => e.trial -> e.item).toMap))
              )
            )
          })
      )
    )

    val explore = Scenario.of[Future](
      Step.intent[Future](
        "explore P17 enc_03",
        Intent.Explain(Place.At(StudioRef.Trial(p17enc03)))
      ),
      check("in Explore")(d => expect("perspective", Perspective.Explore, d.model.perspective)),
      crumbs("explore trail", "P17", "enc_03"),
      Step(
        "enc_03 in the ledger and its fixations",
        d =>
          for
            ledger <- service("ledger")(session.ledger(r3, page))
            six    <- session.navigator.record(
              StudioRef.Fixation(p17enc03, ok(FixationIndex.of(6)))
            )
            last <- session.navigator.record(
              StudioRef.Fixation(p17enc03, ok(FixationIndex.of(13)))
            )
            beyond <- session.navigator.record(
              StudioRef.Fixation(p17enc03, ok(FixationIndex.of(14)))
            )
          yield
            for
              entries <- ledger.map(_.entries)
              entry   <- entries
                .find(_.trial == p17enc03)
                .toRight(DriverError.Expectation("enc_03", "in the ledger", "absent"))
              _ <- expect("item", "beach-042", entry.item)
              _ <- expect("disposition", TrialDisposition.Admitted, entry.disposition)
              _ <- expect("fixation 6", Right(StoryModels.record), six)
              _ <- expect("13 fixations", true, last.isRight && beyond.isLeft)
            yield d
      ),
      sync("select enc_03")(d =>
        d.perform(
          "select",
          StoryModels.select(d.model, "explore.trial-view", StudioRef.Trial(p17enc03))
        )
      ),
      check("status bar")(d => expect("selected", Some("P17 › enc_03"), d.status.selected))
    )

    val analysisAndRun = Scenario.of[Future](
      Step.command[Future](Command.StartDraft(rev3, Some(r3), Vector.empty)),
      Step.invoke[Future](eyes4s.studio.app.keys.CommandRegistry.reviewDraft.id),
      check("Analysis rev 4")(d =>
        for
          _ <- expect("perspective", Perspective.Analysis, d.model.perspective)
          _ <- expect("draft crumb", Some("Draft rev 4"), d.crumbs.lastOption)
        yield ()
      ),
      Step.command[Future](Command.SaveAndRun(None)),
      Step.settle[Future](session),
      check("rev 4 saved on r3; run 6 requested")(d =>
        for
          _ <- expect("rev 4", Some(r3), d.model.document.analysis(rev4).map(_.dataset))
          _ <- expect(
            "run 6",
            Some(RunLifecycle.Running),
            d.model.document.run(run6).map(_.state)
          )
        yield ()
      ),
      await(session, "run 6 accepted")(run6Job(_).isRight),
      Step(
        "hold run 6 at 8,969 pairs",
        d =>
          run6Job(d).fold(
            e => Future.successful(Left(e)),
            j => control("hold")(session.holdAtPairs(j, fixture.pairRowsPerScale)).map(_.as(d))
          )
      ),
      await(session, "run 6 comparing")(
        _.model.jobs.jobs.exists(j =>
          j.run == run6 && (j.phase match
            case JobPhase.Running(p) => p.pairs.done == fixture.pairRowsPerScale
            case _                   => false)
        )
      ),
      check("jobs chip: Run 6 · Comparing · 8,969 / 35,876 pairs")(d =>
        expect("chip", "Run 6 · Comparing · 8,969 / 35,876 pairs", d.appBar.jobs.text)
      ),
      Step(
        "complete run 6",
        d =>
          run6Job(d).fold(
            e => Future.successful(Left(e)),
            j => control("complete")(session.complete(j)).map(_.as(d))
          )
      ),
      await(session, "run 6 ready")(_.model.jobs.ready.exists(_.run == run6)),
      check("run 6 recorded and ready")(d =>
        for
          _ <- expect(
            "lifecycle",
            Some(RunLifecycle.Completed),
            d.model.document.run(run6).map(_.state)
          )
          _ <- expect("chip", JobsChipState.Ready, d.appBar.jobs.state)
        yield ()
      ),
      Step.invoke[Future](eyes4s.studio.app.keys.CommandRegistry.showRun.id),
      Step.settle[Future](session),
      check("run 6 shown and current")(d =>
        for
          _ <- expect("shown", Some(run6), d.model.document.presentation.shownRun)
          _ <- expect(
            "badge",
            "Analysis rev 4 · run 6 · data r3 · current",
            d.context.freshness.text
          )
        yield ()
      )
    )

    val remembered17 =
      StudioRef.ParticipantSummary(run6, reporting, sigma2, Some(remembered), "P17")
    val query = StudioRef.QueryContrast(run6, sigma2, p17ret07)

    val compare = Scenario.of[Future](
      Step.intent[Future]("Compare", Intent.SwitchPerspective(Perspective.Compare)),
      crumbs("summary root", "Summary · by retrieval response"),
      drill(session, "group Remembered")(_.find(_ == Place.Group(reporting, remembered))),
      drill(session, "participant P17")(_.find(_ == Place.At(remembered17))),
      drill(session, "query ret_07")(_.find(_ == Place.At(query))),
      crumbs(
        "query trail",
        "Summary · by retrieval response",
        "Remembered",
        "P17",
        "ret_07 · beach-042"
      ),
      Step(
        "P17 ret_07: M, B and D at 2° = fixture.json",
        d =>
          service("inspect")(
            session.inspect(run6, ResultAddress.ContrastRow(sigma2.value, p17ret07))
          ).map(_.flatMap {
            case Inspection.Contrast(_, m, b, dd) =>
              for
                _ <- expect("M", focus.m.map(_(2)), Some(m))
                _ <- expect("B", focus.b.map(_(2)), Some(b))
                _ <- expect("D", focus.d.map(_(2)), Some(dd))
                // FIXTURE.md: M 0.73, B 0.35, D 0.38 at 2°.
                _ <- expect("FIXTURE.md", (0.73, 0.35, 0.38), (m, b, dd))
              yield d
            case other => fail("inspect", "a contrast", other)
          })
      ),
      drill(session, "matched pair ret_07 × enc_03")(_.headOption),
      Step(
        "the matched pair's score is M",
        d =>
          service("pair")(
            session.inspect(
              run6,
              ResultAddress.PairRow(2, PairDesign.Matched, p17ret07, p17enc03)
            )
          ).map(_.flatMap {
            case Inspection.Pair(_, item, score) =>
              expect("pair", ("beach-042", focus.m.map(_(2))), (item, Some(score))).as(d)
            case other => fail("pair", "a pair", other)
          })
      ),
      drill(session, "map enc_03")(
        _.find(_ == Place.At(StudioRef.TrialMap(run6, sigma2, p17enc03)))
      ),
      drill(session, "fixation 6")(_.lift(5)),
      check("fixation crosses into Explore")(d =>
        expect("perspective", Perspective.Explore, d.model.perspective)
      ),
      drill(session, "record")(_.headOption),
      check("the whole trail")(d =>
        expect(
          "trail",
          "Summary · by retrieval response › Remembered › P17 › ret_07 · beach-042 › " +
            "pair ret_07 × enc_03 › map enc_03 · σ 2° › enc_03 · fixation 6 › fixations.csv record 7,214",
          d.trailText
        )
      )
    )

    val summary = Scenario.of[Future](
      Step.intent[Future]("the summary crumb", Intent.OpenCrumb(0)),
      check("summary layout")(d =>
        expect("perspective", Perspective.Compare, d.model.perspective)
      ),
      Step(
        "the summary facts and explicit reports agree with fixture.json",
        d =>
          val grouped = ok(StoryMoments.byResponse)
          val overall = ok(
            ReportingSpec.of(
              ok(ReportingId.of(reporting.value + "-overall")),
              "Overall",
              None,
              Vector.empty,
              None,
              ReportingWeight.ParticipantMeans
            )
          )
          val reads =
            for
              result <- service("result")(session.result(run6))
              groups <- service("grouped reports")(
                Future
                  .sequence((0 until 4).toVector.map(i => session.report(run6, grouped, i)))
                  .map(_.sequence)
              )
              whole <- service("overall reports")(
                Future
                  .sequence((0 until 4).toVector.map(i => session.report(run6, overall, i)))
                  .map(_.sequence)
              )
            yield
              for
                r  <- result
                gs <- groups
                ws <- whole
                _  <- expect("pair rows", 35876L, r.pairRows)
                _  <- expect("contrasts", QueryContrasts(480, 14, 9, 3, 454), r.contrasts)
                _  <- expect("eligible", 457, r.eligibleQueries)
                _  <- expect(
                  "participants",
                  fixture.participants.map(p =>
                    ParticipantCounts(
                      p.participant,
                      p.requested,
                      p.contributing,
                      p.failed,
                      p.noMatch,
                      p.notAdmitted
                    )
                  ),
                  r.participants
                )
                _ <- expect(
                  "grand D by scale",
                  fixture.grandDByScale,
                  ws.map(_.cell(None, ReportRole.Difference).get.estimate.get)
                    .map(v => math.rint(v * 100) / 100)
                )
                _ <- fixture.groups.traverse_ { expected =>
                  expect(
                    expected.label.label + " by scale",
                    expected.dByScale,
                    gs.map(_.cell(Some(expected.label), ReportRole.Difference).get.estimate.get)
                      .map(v => math.rint(v * 100) / 100)
                  )
                }
                _ <- expect(
                  "legacy grouping declares no level subtraction",
                  Vector.empty,
                  gs(2).contrasts
                )
              yield d
          reads
      ),
      Step.intent[Future]("Explain P17", Intent.Explain(Place.At(remembered17))),
      check("Explain P17 lands on the P17 group")(d =>
        expect("trail", "Summary · by retrieval response › Remembered › P17", d.trailText)
      )
    )

    val figureAndClose = Scenario.of[Future](
      sync("create Figure 3 on run 6 through Start figure with Participant D")(d =>
        val focused = d.perform(
          "open the matched pair",
          Intent.Explain(
            eyes4s.studio.app.nav.Place.At(
              eyes4s.studio.core.selection.StudioRef
                .Pair(run6, sigma2, PairDesign.Matched, p17ret07, p17enc03)
            )
          )
        )
        focused.flatMap { current =>
          val (_, effects) = FigureComposer.update(
            FigureComposer.empty,
            current.model,
            ComposerIntent.NewFigureWith(NewPanel.ParticipantD)
          )
          effects
            .collect { case ComposerEffect.App(i) => i }
            .foldLeft(Right(current): Either[DriverError, StudioDriver])((acc, i) =>
              acc.flatMap(_.perform("figure template control", i))
            )
        }
      ),
      check("Figure 3 binds run 6 and the spec")(d =>
        expect(
          "figure",
          Some((run6, reporting)),
          d.model.document.figures.lastOption.map(f => (f.run, f.reporting))
        )
      ),
      Step.stub[Future]("compose figure", "the figure composer is S9.x"),
      Step.stub[Future]("export", "export is S9.5"),
      Step.settle[Future](session),
      sync("close and reopen")(d =>
        d.reopen(BundleSamples.inputsFor(d.model.document)).flatMap { reopened =>
          for
            _ <- expect("document", d.model.document, reopened.model.document)
            _ <- expect("badge", d.context.freshness, reopened.context.freshness)
            _ <- expect("project", d.model.project, reopened.model.project)
          yield reopened
        }
      )
    )
    GoldenJourney.Stages(
      newProject,
      importAndAdmit,
      explore,
      analysisAndRun,
      compare,
      summary,
      figureAndClose
    )
