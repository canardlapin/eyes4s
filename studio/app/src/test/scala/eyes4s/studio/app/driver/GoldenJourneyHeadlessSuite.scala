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

import cats.syntax.all.*
import eyes4s.studio.app.{AppModel, Intent, PlatformDialog, StoryModels, TrialItems}
import eyes4s.studio.app.nav.{DataSection, Location, Place, Provenance, SummaryScope}
import eyes4s.studio.app.vm.{FreshnessTone, JobsChipState}
import eyes4s.studio.core.backend.{Provenance as _, *}
import eyes4s.studio.core.bundle.BundleSamples
import eyes4s.studio.core.command.{Command, Reducer}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.execution.JobPhase
import eyes4s.studio.core.fixture.{MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.{HeadlessError, HeadlessSession}
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}

/** E2E-01 headless (ticket S3.6): the golden journey through the driver on
  * the fake backend, asserting FIXTURE.md counts and fixture.json scores on
  * the way:
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
  *  - figure composing (S9.x) and export (S9.5), which do not exist yet; the
  *    figure's binding is a real `CreateFigure` command.
  *
  * The run is the journey's own: rev 4 on r3 is run 6 here (the story's run
  * 7 is its second run; there is no rerun command), and the fake serves the
  * fixture's scores for any completed run of rev 4 on r3.
  */
class GoldenJourneyHeadlessSuite extends munit.FunSuite:
  import StoryModels.{ok, p17enc03, p17ret07, sigma2}

  override val munitTimeout: Duration = 120.seconds

  private given ExecutionContext = ExecutionContext.global

  /** S3.6 acceptance: E2E-01 headless in under 60 s on the fake. */
  private val Budget: FiniteDuration = 60.seconds

  private val study   = ok(MockStudy.load)
  private val fixture = study.summary
  private val focus = study.queries.find(q => q.participant == "P17" && q.trial == "ret_07").get

  private val r2      = StoryMoments.r2
  private val r3      = StoryMoments.r3
  private val rev3    = StoryMoments.rev3
  private val rev4    = StoryMoments.rev4
  private val run6    = RunId(6)
  private val project = StoryModels.project

  private val reporting: ReportingId = StoryModels.reporting
  private val remembered             = Response.Remembered
  private val page                   = ok(PageRequest.first(PageRequest.MaximumSize))

  private type S = Step[Future]

  private def fail[A](step: String, expected: String, actual: Any): Either[DriverError, A] =
    Left(DriverError.Expectation(step, expected, actual.toString))

  private def expect[A](step: String, expected: A, actual: A): Either[DriverError, Unit] =
    if expected == actual then Right(()) else fail(step, expected.toString, actual)

  private def check(name: String)(p: StudioDriver => Either[DriverError, Unit]): S =
    Step.check[Future](name)(p)

  private def sync(name: String)(f: StudioDriver => Either[DriverError, StudioDriver]): S =
    Step.pure[Future](name)(f)

  /** A backend read as a step's result; a refusal fails the step, typed. */
  private def service[A](name: String)(
      fa: Future[Either[BackendError, A]]
  ): Future[Either[DriverError, A]] =
    fa.map(_.leftMap(e => DriverError.Service(name, ServiceError.Backend(e))))

  /** A fake control call as a step's result. */
  private def control[A](name: String)(
      fa: Future[Either[HeadlessError, A]]
  ): Future[Either[DriverError, A]] =
    fa.map(_.leftMap(e => DriverError.Service(name, ServiceError.Headless(e))))

  /** Feed execution events until the driver's state satisfies `done`: the
    * events already published first, then each new one as it arrives. The
    * state, not a particular event, is awaited, since a settle may already
    * have fed the event.
    */
  private def await(session: HeadlessSession, name: String)(done: StudioDriver => Boolean): S =
    def loop(d: StudioDriver): Future[Either[DriverError, StudioDriver]] =
      if done(d) then Future.successful(Right(d))
      else
        session.awaitEvent(_ => true).flatMap {
          case Left(e) =>
            Future.successful(Left(DriverError.Service(name, ServiceError.Headless(e))))
          case Right(events) => loop(d.feed(events))
        }
    Step(name, d => session.events.flatMap(events => loop(d.feed(events))))

  private def run6Job(d: StudioDriver): Either[DriverError, JobId] =
    d.model.jobs.jobs
      .find(_.run == run6)
      .map(_.id)
      .toRight(
        DriverError.Expectation("run 6's job", "a tracked job", d.model.jobs.jobs.toString)
      )

  /** Explain one level further down: the child `pick` chooses among the
    * navigator's children of the trail's last place.
    */
  private def drill(session: HeadlessSession, name: String)(
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

  private def crumbs(step: String, expected: String*): S =
    check(step)(d => expect(step, expected.toVector, d.crumbs))

  // -------------------------------------------------------------------------

  /** The fixture as the fake's history holds it before the re-import: t1
    * without its pending r3.
    */
  private lazy val history: StudioDocument =
    ok(Reducer.step(StoryModels.t1, Command.DiscardDataset(r3)).map(_._1))

  private def journey(session: HeadlessSession): Scenario[Future] =
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
          None
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
              _ <- expect("inventory trials", 960, a.inventoryTrials)
              _ <- expect("admitted", 937, a.admitted)
              _ <- expect("quarantined", 17, a.quarantinedTrials + a.noFixations)
              _ <- expect("absent", 6, a.absent)
              _ <- expect("fixation records", 11520, a.fixationRecords)
              _ <- expect("source records", Some(11520), a.window.sourceRecords)
              _ <- expect("images missing", 2, a.missingImages.size)
              _ <- expect("dataset", r3, a.dataset)
            yield ()
      ),
      sync("admit r3")(d =>
        d.model.document.dataset(r3).map(_.decision) match
          case Some(AdmissionDecision.Verifying(content)) =>
            d.command(Command.Admit(r3, content, CoreBinding.unbound, CoreBinding.unbound))
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
        "the summary = fixture.json",
        d =>
          service("result")(session.result(run6)).map(_.flatMap { r =>
            for
              _ <- expect("grand D", fixture.grandD, r.grandD)
              _ <- expect("grand D", 0.26, r.grandD)
              _ <- expect("by scale", fixture.grandDByScale, r.grandDByScale)
              _ <- expect("groups", fixture.groups, r.groups)
              _ <- expect(
                "Remembered",
                Some((24, 0.3)),
                r.groups.find(_.label == remembered).map(g => (g.n, g.d))
              )
              _ <- expect(
                "Forgotten",
                Some((24, 0.15)),
                r.groups.find(_.label == Response.Forgotten).map(g => (g.n, g.d))
              )
              _ <- expect("paired n", 24, r.pairedN)
              _ <- expect("pair rows", 35876L, r.pairRows)
              _ <- expect("contrasts", QueryContrasts(480, 14, 9, 3, 454), r.contrasts)
              _ <- expect("eligible", 457, r.eligibleQueries)
              _ <- expect("participants", fixture.participants, r.participants)
            yield d
          })
      ),
      Step.intent[Future]("Explain P17", Intent.Explain(Place.At(remembered17))),
      check("Explain P17 lands on the P17 group")(d =>
        expect("trail", "Summary · by retrieval response › Remembered › P17", d.trailText)
      )
    )

    val figureAndClose = Scenario.of[Future](
      sync("create Figure 3 on run 6")(d =>
        (for
          letter <- PanelLetter.of("A")
          scale  <- Sigma.of(2.0)
        yield Vector(
          PanelSpec(
            letter,
            "Participant D by response",
            PanelScale.At(scale),
            PanelSelection.AllQueries
          )
        ))
          .leftMap(e => DriverError.Expectation("figure", "a valid panel", e.message))
          .flatMap(panels => d.command(Command.CreateFigure(run6, reporting, panels)))
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
    newProject ++ importAndAdmit ++ explore ++ analysisAndRun ++ compare ++ summary ++ figureAndClose

  test("E2E-01 headless: the golden journey on the fake backend, in under 60 s") {
    val started = System.nanoTime()
    HeadlessSession.open(StoryMoment.T1).flatMap { session =>
      val driver = StudioDriver.open(StoryModels.firstRun)
      journey(session)
        .run(driver)
        .transformWith(result => session.close.transform(_ => result))
        .map { result =>
          val elapsed = (System.nanoTime() - started).nanos
          result match
            case Left(failure) => fail(failure.message)
            case Right(end)    =>
              assert(elapsed < Budget, s"E2E-01 took ${elapsed.toMillis} ms")
              assertEquals(
                end.records.collect { case DriverRecord.Stubbed(step, _) => step },
                Vector("import dialog", "compose figure", "export")
              )
              assertEquals(
                end.records.collect { case r: DriverRecord.Refused => r },
                Vector.empty
              )
        }
    }
  }
