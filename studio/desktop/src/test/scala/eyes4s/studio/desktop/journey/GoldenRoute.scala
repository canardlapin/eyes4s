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
import cats.instances.future.*
import cats.syntax.all.*
import eyes4s.plan.MapPlacement
import eyes4s.studio.app.driver.*
import eyes4s.studio.app.figures.*
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.figures.{MethodsReads, ReferenceReads}
import eyes4s.studio.core.fixture.{GoldenAssets, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.core.session.ProjectSession
import eyes4s.studio.desktop.figures.BundleFiles
import eyes4s.studio.desktop.platform.FileProjectStore

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*

/** The E2E-01 headless route (ticket S10.1), shared by [[GoldenJourneySuite]]
  * and the UI route's comparison ([[GoldenJourneyFxSuite]]): the journey
  * through the StudioDriver with its checks, what it exported, and the
  * project folder it saved. See [[GoldenJourneySuite]].
  */
object GoldenRoute:
  import GoldenJourney.*
  import StoryModels.{ok, p17enc03, p17ret07, sigma2}

  given ExecutionContext = ExecutionContext.global

  val doc     = FixtureDoc
  val library = GoldenLibrary.facts
  val run6    = RunId(6)
  val rev4    = StoryMoments.rev4

  def round(v: Double, places: Int = 2) = FixtureDoc.rounded(v, places)

  /** A FIXTURE.md phrase, required verbatim, and the value the journey saw. */
  def stated[A](step: String, phrase: String, expected: A, actual: A) =
    if !doc.states(phrase) then
      GoldenJourney.fail(step, s"FIXTURE.md to state '$phrase'", "it does not")
    else expect(step, expected, actual)

  /** A parity-checklist item, required verbatim, and whether the journey saw it. */
  def listed(step: String, phrase: String, seen: Boolean, shown: => String) =
    if !doc.lists(phrase) then
      GoldenJourney.fail(step, s"PARITY_CHECKLIST.md to list '$phrase'", "it does not")
    else expect(step, s"$phrase: shown", if seen then s"$phrase: shown" else shown)

  def all(checks: Either[DriverError, Unit]*): Either[DriverError, Unit] =
    checks.toVector.sequence_

  /** The journey's own steps, on session `s`; trial views from `views`. */
  final class Route(s: HeadlessSession, views: HeadlessSession):

    // -------------------------------------------------------------------------
    // Admission, against FIXTURE.md and eyes4s itself
    // -------------------------------------------------------------------------

    private val admission: S = check("admission = FIXTURE.md = eyes4s") { d =>
      d.admissions.lastOption match
        case None    => GoldenJourney.fail("admission", "a summary", d.records)
        case Some(a) =>
          val causes =
            a.quarantined.map(q => q.code.stripPrefix("quarantine.") -> q.trials).toMap
          val missing = a.missingImages.map(m => (m.item, m.participants, m.encodingTrials))
          all(
            stated(
              "inventory",
              "Inventory (trials.csv): 960 trials. Admitted 937, quarantined 17 " +
                "(duplicate-ordinals 4, no-fixations 5, overlap 6, rejected-records 2), absent 6",
              (Some(960), 937, 17, 5, Some(6)),
              (
                a.inventoryTrials,
                a.admitted,
                a.quarantinedTrials + a.noFixations,
                a.noFixations,
                a.absent
              )
            ),
            expect(
              "quarantine causes",
              Map("duplicate-ordinals" -> 4, "overlap" -> 6, "rejected-records" -> 2),
              causes
            ),
            stated(
              "records and items",
              "fixations.csv: 11520 records. Distinct items (stimulus images): 259; images found 257, missing 2.",
              (11520, 259, 257, 2),
              (a.fixationRecords, a.items, a.imagesFound, a.missingImages.size)
            ),
            stated(
              "missing images",
              "Missing images: forest-044.png (encoding trials of P01, P24: 2); " +
                "kitchen-081.png (encoding trials of P01, P24: 2).",
              Set(
                ("forest-044", Vector("P01", "P24"), 2),
                ("kitchen-081", Vector("P01", "P24"), 2)
              ),
              missing.toSet
            ),
            stated(
              "outside the window",
              "Fixation records outside the image frame (analysis window): 543 of 11,520, in 409 trials.",
              (543, 409, 0),
              (a.window.outsideWindow, a.window.trialsOutsideWindow, a.window.outsideScreen)
            ),
            // eyes4s itself, on the same files.
            expect(
              "eyes4s: inventory",
              (960, 937, causes, 5, 6, 11520, 259),
              (
                library.inventoryTrials,
                library.admitted,
                library.quarantined,
                library.noFixations,
                library.absent,
                library.fixationRecords,
                library.items
              )
            ),
            expect(
              "images",
              (library.imagesFound, library.missingImages),
              (a.imagesFound, a.missingImages.map(_.item).sorted)
            ),
            expect(
              "eyes4s: window",
              (a.window.outsideWindow, a.window.trialsOutsideWindow, a.window.outsideScreen),
              (
                library.outsideWindowRecords,
                library.outsideWindowTrials,
                library.outsideScreenRecords
              )
            )
          )
    }

    // -------------------------------------------------------------------------
    // The resolved design and the focus fixation, under rev 4 on r3
    // -------------------------------------------------------------------------

    private val design: S = Step(
      "rev 4's resolved design = FIXTURE.md",
      d =>
        service("preview")(s.preview(rev4)).map(_.flatMap { p =>
          all(
            stated(
              "eligible",
              "Eligible (computed) queries: 457 (454 contributing + 3 failed).",
              457,
              p.eligibleQueries
            ),
            stated(
              "requested",
              "Query contrasts (retrieval trials): {\"requested\": 480",
              480,
              p.requestedQueries
            ),
            stated(
              "candidates",
              "Cartesian candidate pairs before paging, per scale: 219,486",
              219486L,
              p.candidatePairsPerScale
            ),
            stated(
              "pair rows",
              "Pair rows: 8969 per scale, 35876 across 4 scales.",
              (8969L, 35876L, 4),
              (p.pairRowsPerScale, p.pairRows, p.scales.size)
            ),
            // eyes4s's own pairing of the admitted trials.
            expect(
              "eyes4s: design",
              (
                p.requestedQueries,
                p.eligibleQueries,
                p.candidatePairsPerScale,
                p.pairRowsPerScale
              ),
              (
                library.design.requested,
                library.design.eligible,
                library.design.candidatePairsPerScale,
                library.design.pairsPerScale
              )
            ),
            expect(
              "eyes4s: pair rows",
              p.pairRows,
              library.design.pairsPerScale * p.scales.size
            ),
            stated(
              "controls per query",
              "Controls: same participant, Encoding, other items, all admitted → 19 per query " +
                "(18 where one encoding trial was not admitted).",
              Set(18, 19),
              library.design.controlsPerQuery.keySet
            )
          ).as(d)
        })
    )

    private val focus: S = Step(
      "the focus fixation = FIXTURE.md = eyes4s",
      d => // The fake serves trial views from its story's documents: the
        // journey's rev 4 must be the story's rev 4 (t2) for them to apply.
        val story =
          StoryMoments.t2.toOption.flatMap(_.analysis(rev4)).map(a => (a.dataset, a.recipe))
        val ours = d.model.document.analysis(rev4).map(a => (a.dataset, a.recipe))

        val f = library.focus
        for
          page  <- service("records")(views.sourceRecords(rev4, 7214, 1))
          enc03 <- service("enc_03")(views.trialFixations(rev4, p17enc03))
          ret07 <- service("ret_07")(views.trialFixations(rev4, p17ret07))
        yield
          for
            _   <- expect("rev 4 is the story's rev 4", story, ours)
            p   <- page
            row <- p.rows.headOption.toRight(DriverError.Expectation("record", "7,214", "none"))
            e   <- enc03
            r   <- ret07
            served = (
              row.screen.map(s => (s.x, s.y)),
              row.image.map(i => (i.at.x, i.at.y)),
              row.degrees.map(g => (g.x, g.y)),
              row.onsetMs,
              row.durationMs
            )
            _ <- all(
              stated(
                "focus record",
                "Focus fixation: P17 enc_03 fixation 6 = fixations.csv record 7,214; image px [700, 300], " +
                  "screen px [1148, 456], degrees from image centre (x right, y up) [5.4, 2.4]; " +
                  "onset 2160 ms, duration 412 ms.",
                (
                  7214,
                  Some((1148.0, 456.0)),
                  Some((700.0, 300.0)),
                  Some((5.4, 2.4)),
                  Some(2160.0),
                  Some(412.0)
                ),
                (
                  row.record,
                  served._1,
                  served._2,
                  served._3.map((x, y) => (round(x, 1).toDouble, round(y, 1).toDouble)),
                  served._4,
                  served._5
                )
              ),
              expect(
                "eyes4s: focus",
                (
                  f.record,
                  Some(f.screen),
                  Some(f.image),
                  Some(f.degrees),
                  Some(f.onsetMs),
                  Some(f.durationMs)
                ),
                (row.record, served._1, served._2, served._3, served._4, served._5)
              ),
              stated(
                "focus outside",
                "Focus query outside window: ret_07 1 of 12 fixations · 4% of duration; " +
                  "enc_03 1 of 13 fixations · 3% of duration.",
                ((12, 1, 4), (13, 1, 3)),
                (window(r), window(e))
              ),
              windowCheck("enc_03", e, library.enc03),
              windowCheck("ret_07", r, library.ret07)
            )
          yield d
    )

    // A trial's served fixations outside the window and their share of its
    // fixation duration.
    private def outside(t: TrialFixations) =
      val out = t.fixations.filter(_.placement match
        case MapPlacement.OutsideWindow(_) => true
        case _                             => false)
      (out, out.map(_.durationMs).sum / t.fixations.map(_.durationMs).sum)

    /** How many fixations, how many outside, the outside percent of duration. */
    private def window(t: TrialFixations): (Int, Int, Int) =
      val (out, share) = outside(t)
      (t.fixations.size, out.size, (share * 100).round.toInt)

    /** The served fixations against eyes4s's own tally of the trial. */
    private def windowCheck(
        name: String,
        t: TrialFixations,
        tally: TrialWindow
    ): Either[DriverError, Unit] =
      val (out, share) = outside(t)
      expect(
        s"eyes4s: $name",
        (tally.fixations, tally.outside, tally.outsideShare.map(round(_, 6))),
        (t.fixations.size, out.size, Some(round(share, 6)))
      )

    // -------------------------------------------------------------------------
    // Compare: the focus query at every scale, its controls and the matched map
    // -------------------------------------------------------------------------

    private val query: S = Step(
      "P17 ret_07 at every scale = FIXTURE.md",
      d =>
        (0 until 4).toVector
          .traverse { i =>
            service(s"scale $i")(
              s.inspect(run6, ResultAddress.ContrastRow(i, p17ret07))
            ).map(_.flatMap {
              case Inspection.Contrast(_, m, b, dd) => Right((round(m), round(b), round(dd)))
              case other => GoldenJourney.fail("inspect", "a contrast", other)
            })
          }
          .map(_.sequence.flatMap { mbd =>
            all(
              expect("M by scale", doc.named("M/B/D by scale:", "M"), mbd.map(_._1)),
              expect("B by scale", doc.named("M/B/D by scale:", "B"), mbd.map(_._2)),
              expect("D by scale", doc.named("M/B/D by scale:", "D"), mbd.map(_._3))
            ).as(d)
          })
    )

    private val controls: S = Step(
      "19 controls at 2°, highest street-112 (enc_01) 0.61, mean 0.35",
      d =>
        val nav = s.navigator
        for
          refs <- ReferenceReads.read[Future](
            s.inspect(run6, _),
            nav.pairs,
            run6,
            sigma2,
            p17ret07
          )
          used <- nav.usedByCounts(StudioRef.TrialMap(run6, sigma2, p17enc03))
        yield
          for
            r <- refs.leftMap(e => DriverError.Expectation("references", "read", e.message))
            u <- used.leftMap(e => DriverError.Service("used by", ServiceError.Navigation(e)))
            top = r.controls.maxBy(_.score)
            _ <- all(
              stated(
                "controls",
                "19 controls at 2°, highest street-112 (enc_01) 0.61, mean 0.35.",
                (19, "street-112", "enc_01", round(0.61), round(0.35)),
                (r.controlMembers, top.item, top.reference.trial, round(top.score), round(r.b))
              ),
              stated(
                "used by",
                "enc_03 (beach-042) is used by ret_07 as the matched reference and by the 18 other " +
                  "admitted P17 queries as a control.",
                (1, 18),
                (u.asMatched, u.asControl)
              )
            )
          yield d
    )

    // -------------------------------------------------------------------------
    // The summary: every number of the participant table
    // -------------------------------------------------------------------------

    private val summary: S = Step(
      "the evaluated participant reports and frozen FIXTURE.md metadata",
      d =>
        val groupedSpec = ok(StoryMoments.byResponse)
        val overallSpec = ok(
          ReportingSpec.of(
            ok(ReportingId.of(reporting.value + "-overall")),
            "Overall",
            None,
            Vector.empty,
            None,
            ReportingWeight.ParticipantMeans
          )
        )
        for
          result  <- service("result")(s.result(run6))
          grouped <- service("grouped reports")(
            Future
              .sequence((0 until 4).toVector.map(i => s.report(run6, groupedSpec, i)))
              .map(_.sequence)
          )
          overall <- service("overall reports")(
            Future
              .sequence((0 until 4).toVector.map(i => s.report(run6, overallSpec, i)))
              .map(_.sequence)
          )
        yield
          for
            r      <- result
            gs     <- grouped
            ws     <- overall
            served <- r.participants.traverse { p =>
              def mean(view: ReportView, group: Option[Response], role: ReportRole) =
                view
                  .participant(group, role, p.participant)
                  .flatMap(_.value)
                  .toRight(
                    DriverError.Expectation(
                      "participant report",
                      "a served mean",
                      s"${p.participant} $group $role at scale ${view.scale}"
                    )
                  )
              def groupMean(group: Response) = for
                value   <- mean(gs(2), Some(group), ReportRole.Difference)
                queries <- gs(2)
                  .participant(Some(group), ReportRole.Difference, p.participant)
                  .map(_.queries)
                  .toRight(
                    DriverError
                      .Expectation("participant count", "a served count", p.participant)
                  )
              yield (round(value), queries)
              for
                m          <- mean(ws(2), None, ReportRole.Matched)
                b          <- mean(ws(2), None, ReportRole.Control)
                value      <- mean(ws(2), None, ReportRole.Difference)
                remembered <- groupMean(Response.Remembered)
                forgotten  <- groupMean(Response.Forgotten)
              yield FixtureRow(
                p.participant,
                p.requested,
                p.contributing,
                p.failed,
                p.noMatch,
                p.notAdmitted,
                round(m),
                round(b),
                round(value),
                remembered,
                forgotten
              )
            }
            _ <- r.participants.traverse_ { p =>
              Vector(ReportRole.Matched, ReportRole.Control, ReportRole.Difference).traverse_ {
                role =>
                  ws.indices.toVector.traverse_ { scale =>
                    Vector(None, Some(Response.Remembered), Some(Response.Forgotten))
                      .traverse_ { group =>
                        val view = group.fold(ws(scale))(_ => gs(scale))
                        expect(
                          s"stored queries: ${p.participant}/$group/$role/$scale",
                          Some(
                            round(
                              StoredQueryExpectations.mean(p.participant, role, scale, group),
                              12
                            )
                          ),
                          view
                            .participant(group, role, p.participant)
                            .flatMap(_.value)
                            .map(round(_, 12))
                        )
                      }
                  }
              }
            }
            _ <- all(
              expect("participant rows", 24, doc.participants.size),
              expect(
                "frozen participant metadata",
                doc.participants,
                StoredQueryExpectations.legacyParticipants
              ),
              expect(
                "evaluated participant display",
                StoredQueryExpectations.evaluatedParticipants,
                served
              ),
              expect(
                "grand D by scale",
                doc.list("By scale:"),
                ws.map(_.cell(None, ReportRole.Difference).get.estimate.get).map(round(_))
              ),
              expect(
                "Remembered by scale",
                doc.list("Remembered ["),
                gs.map(
                  _.cell(Some(Response.Remembered), ReportRole.Difference).get.estimate.get
                ).map(round(_))
              ),
              expect(
                "Forgotten by scale",
                doc.list("Forgotten ["),
                gs.map(_.cell(Some(Response.Forgotten), ReportRole.Difference).get.estimate.get)
                  .map(round(_))
              ),
              expect(
                "legacy grouping declares no level subtraction",
                Vector.empty,
                gs(2).contrasts
              ),
              stated(
                "group n range",
                "Per-group n range across participants (Remembered/Forgotten): [2, 17].",
                Some((2, 17)),
                gs(2).queryRange(ReportRole.Difference).map(v => (v.fewest, v.most))
              ),
              stated(
                "contributing",
                "\"no_match\": 9, \"failed\": 3, \"contributing\": 454}",
                QueryContrasts(480, 14, 9, 3, 454),
                r.contrasts
              ),
              expect(
                "eyes4s: queries",
                (library.design.requested, library.design.notAdmitted, library.design.noMatch),
                (r.contrasts.requested, r.contrasts.queryNotAdmitted, r.contrasts.noMatch)
              ),
              stated(
                "P05 failed",
                "P05's 3 failed queries: 11 of 11 fixations outside.",
                Vector(("P05", "ret_04", 11), ("P05", "ret_11", 11), ("P05", "ret_16", 11)),
                library.allOutside
              )
            )
          yield d
    )

    // The draft that adds σ 8°: rev 5's pair rows, then discarded.
    private val rev5: Scenario[Future] =
      val eight = ok(Sigma.of(8.0))
      Scenario.of[Future](
        sync("start rev 5 with σ 8°")(d =>
          d.model.document.analysis(rev4) match
            case None    => GoldenJourney.fail("rev 4", "saved", "absent")
            case Some(a) =>
              val after = ok(ScaleSet.of(a.recipe.scales.values :+ eight))
              d.command(
                Command
                  .StartDraft(rev4, None, Vector(RecipeChange.Scales(a.recipe.scales, after)))
              )
        ),
        // The draft is the story's rev 5 (t2): the fake previews it from there.
        check("rev 5 is the story's rev 5")(d =>
          expect(
            "rev 5",
            StoryMoments.t2.toOption
              .flatMap(_.draft)
              .map(x => (x.id, x.recipe(StoryModels.t2.analysis(rev4).get.recipe))),
            d.model.document.draft.map(x =>
              (x.id, x.recipe(d.model.document.analysis(rev4).get.recipe))
            )
          )
        ),
        Step(
          "rev 5's pair rows",
          d =>
            service("preview")(views.preview(StoryMoments.rev5)).map(
              _.flatMap(p =>
                stated(
                  "rev 5",
                  "Rev 5 (5 scales incl. 8°) pair rows: 44,845. Rev 4: 35,876.",
                  (5, 44845L),
                  (p.scales.size, p.pairRows)
                ).flatMap(_ =>
                  expect("eyes4s: rev 5", p.pairRows, library.design.pairsPerScale * 5)
                ).as(d)
              )
            )
        ),
        Step.invoke[Future](eyes4s.studio.app.keys.CommandRegistry.discardDraft.id),
        Step.intent[Future]("confirm the discard", Intent.Confirm),
        check("no draft")(d => expect("draft", None, d.model.document.draft.map(_.id)))
      )

    // -------------------------------------------------------------------------
    // Figure and export: composed, its bundle assembled as the desktop writes it
    // -------------------------------------------------------------------------

    /** The composer's reads, answered by the session; the export bundle's
      * request is captured, not written.
      */
    private def compose(
        c: FigureComposer,
        model: AppModel,
        effects: Vector[ComposerEffect],
        captured: Vector[BundleRequest]
    ): Future[(FigureComposer, Vector[BundleRequest])] =
      effects.foldLeft(Future.successful((c, captured))) { (acc, effect) =>
        acc.flatMap { (c, captured) =>
          def next(i: ComposerIntent) =
            val (n, more) = FigureComposer.update(c, model, i)
            compose(n, model, more, captured)
          effect match
            case ComposerEffect.RequestSummary(run) =>
              s.result(run)
                .flatMap(a =>
                  next(
                    ComposerIntent.SummaryRead(
                      run,
                      a.fold(
                        e => eyes4s.studio.app.compare.SummaryAnswer.Refused(e),
                        eyes4s.studio.app.compare.SummaryAnswer.Answered(_)
                      )
                    )
                  )
                )
            case ComposerEffect.RequestReport(run, spec, scale) =>
              s.report(run, spec, scale.value)
                .flatMap(answer =>
                  next(ComposerIntent.ReportRead(run, spec, scale, answer.leftMap(_.message)))
                )
            case ComposerEffect.RequestMethods(run, dataset) =>
              MethodsReads
                .read[Future](s.admission, s.queries, run, dataset)
                .flatMap(a =>
                  next(
                    ComposerIntent.Methods(MethodsIntent.FactsRead(run, a.leftMap(_.message)))
                  )
                )
            case ComposerEffect.RequestReferences(run, scale, q) =>
              ReferenceReads
                .read[Future](s.inspect(run, _), s.navigator.pairs, run, scale, q)
                .flatMap(a =>
                  next(ComposerIntent.ReferencesRead(run, scale, q, a.leftMap(_.message)))
                )
            case ComposerEffect.RequestDisplays(dataset) =>
              next(
                ComposerIntent.DisplaysRead(
                  dataset.id,
                  GoldenAssets
                    .registry(dataset)
                    .map(eyes4s.studio.app.explore.DisplaySource.Served(_))
                )
              )
            case ComposerEffect.RequestFixations(revision, trial) =>
              s.trialFixations(revision, trial)
                .flatMap(a =>
                  next(ComposerIntent.FixationsRead(revision, trial, a.leftMap(_.message)))
                )
            case ComposerEffect.RequestMap(run, scale, trial) =>
              s.mapGrid(run, scale.value, trial)
                .flatMap(a =>
                  next(ComposerIntent.MapRead(run, scale, trial, a.leftMap(_.message)))
                )
            case ComposerEffect.WriteBundle(request) =>
              Future.successful((c, captured :+ request))
            case _ => Future.successful((c, captured))
        }
      }

    /** The exported bundle's text files, by name. */
    @volatile var exported: Map[String, String] = Map.empty

    /** Every file of the exported bundle, by name, as written. */
    @volatile var bundle: Map[String, Vector[Byte]] = Map.empty

    /** The document the journey closed, and the project folder it saved. */
    @volatile var closed: Option[StudioDocument]         = None
    @volatile var savedFolder: Map[String, Vector[Byte]] = Map.empty

    private val figureAndExport: Scenario[Future] = Scenario.of[Future](
      // New figure, as the navigator's button asks the composer: Figure 3 on
      // the shown run, its two default panels.
      sync("New figure: Figure 3 on run 6")(d =>
        val (_, effects) =
          FigureComposer.update(FigureComposer.empty, d.model, ComposerIntent.NewFigure)
        effects
          .collect { case ComposerEffect.App(i) => i }
          .foldLeft(Right(d): Either[DriverError, StudioDriver])((acc, i) =>
            acc.flatMap(_.perform("New figure", i))
          )
          .flatMap(next =>
            expect(
              "Figure 3",
              Some((3, run6, StoryModels.reporting, Vector("A", "B"))),
              next.model.document.figures.lastOption
                .map(f => (f.id.number, f.run, f.reporting, f.panels.map(_.letter.value)))
            ).as(next)
          )
      ),
      sync("create Figure 4 through Start figure with and Add panel templates")(d =>
        // The user's pair choice supplies the encoding trial and query.
        val pair    = StudioRef.Pair(run6, sigma2, PairDesign.Matched, p17ret07, p17enc03)
        val focused = d.perform(
          "open the matched pair",
          Intent.Explain(eyes4s.studio.app.nav.Place.At(pair))
        )
        val actions = Vector(ComposerIntent.NewFigureWith(NewPanel.EncodingGaze)) ++
          Vector(
            NewPanel.RetrievalGaze,
            NewPanel.DensityMaps,
            NewPanel.ParticipantD,
            NewPanel.ScaleProfile
          )
            .map(ComposerIntent.AddPanelOf.apply)
        actions
          .foldLeft(focused) { (acc, action) =>
            acc.flatMap { current =>
              val (_, effects) =
                FigureComposer.update(FigureComposer.empty, current.model, action)
              effects
                .collect { case ComposerEffect.App(i) => i }
                .foldLeft(Right(current): Either[DriverError, StudioDriver])((next, i) =>
                  next.flatMap(_.perform("figure template control", i))
                )
            }
          }
          .flatMap(next =>
            StoryMoments.figure1
              .leftMap(e => DriverError.Expectation("board figure", "valid", e.message))
              .flatMap(expected =>
                expect(
                  "figure panels",
                  expected.panels,
                  next.model.document.figures.last.panels
                ).as(next)
              )
          )
      ),
      check("the figure binds run 6 and the spec")(d =>
        expect(
          "figure",
          Some((run6, StoryModels.reporting)),
          d.model.document.figures.lastOption.map(f => (f.run, f.reporting))
        )
      ),
      Step(
        "compose and export the bundle",
        d =>
          val figure = d.model.document.figures.last.id
          val model  = d
            .dispatch(
              Intent.Navigate(
                eyes4s.studio.app.nav.Location(
                  Perspective.Figures,
                  Vector(
                    eyes4s.studio.app.nav.Place.Figures,
                    eyes4s.studio.app.nav.Place.Figure(figure)
                  )
                )
              )
            )
            .model
          val (c0, e0) = FigureComposer.sync(FigureComposer.empty, model)
          for
            (c1, _) <- compose(c0, model, e0, Vector.empty)
            // The project snapshot, off by default, is turned on.
            (c1s, _) = FigureComposer.update(
              c1,
              model,
              ComposerIntent.ToggleBundle(BundleItem.Snapshot)
            )
            (c2, e2) = FigureComposer.update(c1s, model, ComposerIntent.ExportBundle)
            (_, req) <- compose(c2, model, e2, Vector.empty)
            project  <- req.headOption
              .filter(_.items.contains(BundleItem.Snapshot))
              .fold(
                Future.successful(Right(Map.empty): Either[String, Map[String, Vector[Byte]]])
              )(r => snapshot(d.model.document, r.includeImages))
            summary <- s.result(run6)
            rows    <- MethodsReads.queryRows[Future](s.queries, run6)
            pairs   <- summary.fold(
              e => Future.successful(Left(e.message)),
              sum =>
                MethodsReads
                  .pairRows[Future](s.pairRows, run6, sum.scales.size)
                  .map(_.left.map(_.message))
            )
          yield
            for
              request <- req.headOption.toRight(
                DriverError.Expectation("bundle", "a request", "none")
              )
              snap <- project.leftMap(why =>
                DriverError.Expectation("snapshot", "written", why)
              )
              _ <- expect("snapshot", true, snap.keySet.exists(_.startsWith("project/inputs/")))
              sum <- summary.leftMap(e =>
                DriverError.Service("result", ServiceError.Backend(e))
              )
              all_     <- rows.leftMap(e => DriverError.Expectation("rows", "read", e.message))
              allPairs <- pairs.leftMap(why => DriverError.Expectation("pairs", "read", why))
              report   <- request.participantScale
                .flatMap(scale =>
                  c2.reports.get((request.source.run.id, request.source.reporting, scale))
                )
                .toRight(
                  DriverError.Expectation(
                    "participant report",
                    "the exact requested scale",
                    "not cached"
                  )
                )
                .flatMap(
                  _.leftMap(why => DriverError.Expectation("participant report", "served", why))
                )
              files <- BundleFiles
                .assemble(
                  request,
                  sum,
                  all_,
                  allPairs,
                  // The window's stimuli (ShellFxSuite's), so both routes draw
                  // the gaze panels over the same images.
                  eyes4s.studio.desktop.figures.FigureExport.rasters(
                    request.page,
                    eyes4s.studio.desktop.trial.StimulusSource
                      .directory(eyes4s.studio.desktop.trial.GoldenTrials.stimuli)
                  ),
                  report = Some(report)
                )
                .leftMap(why => DriverError.Expectation("bundle", "assembled", why))
              text = files.collect {
                case (n, bytes) if !n.endsWith(".svg") => n -> String(Array.from(bytes), UTF_8)
              }.toMap
              page = FigureComposer.view(c2, model).page
              _ <- figureChecks(text, page)
            yield
              exported = text
              bundle = files.map((n, b) => n -> Vector.from(b)).toMap ++ snap
              d.dispatch(
                Intent.Navigate(
                  eyes4s.studio.app.nav.Location(
                    Perspective.Compare,
                    d.model.navigation.at(Perspective.Compare).trail
                  )
                )
              )
      )
    )

    /** The bundle's project snapshot, as the window's port writes it: the
      * project, as saved with its stored inputs, shared into `project/`
      * (ProjectSession.share), with the images when `images`.
      */
    private def snapshot(
        document: StudioDocument,
        images: Boolean
    ): Future[Either[String, Map[String, Vector[Byte]]]] =
      val base  = Files.createTempDirectory("golden-snapshot")
      val owner = ok(LockOwner.of("golden journey"))
      (for
        store   <- FileProjectStore.at[IO](base.resolve("journey.eyes"))
        lock    <- store.acquire(owner).map(_.leftMap(_.message))
        entries <- lock.flatTraverse(l =>
          (golden ++ stored)
            .traverse((kind, name, bytes) =>
              ProjectBundle.importInput(store, l, kind, name, bytes)
            )
            .map(_.sequence.leftMap(_.message))
            .flatTap(_ => store.release(l))
        )
        session <- entries.flatTraverse(in =>
          ProjectSession
            .create(store, owner, document, SharingOptions.complete, in)
            .map(_.leftMap(_.message))
        )
        copy   <- FileProjectStore.at[IO](base.resolve("project"))
        shared <- session.flatTraverse(
          _.share(
            copy,
            SharingOptions(
              Inclusion.Included,
              if images then Inclusion.Included else Inclusion.Withheld
            )
          ).map(_.leftMap(_.message))
        )
        _     <- session.toOption.traverse(_.close)
        files <- IO(folder(base.resolve("project")))
      yield shared.as(files.map((n, b) => s"project/$n" -> b)))
        .guarantee(IO(remove(base)))
        .unsafeToFuture()

    private def figureChecks(files: Map[String, String], page: Option[PageVM]) =
      val methods      = files.getOrElse("methods.md", "")
      val results      = files.getOrElse("results.csv", "").linesIterator.toVector
      val participants = files.getOrElse("participants.csv", "").linesIterator.toVector
      val panelC = page.toVector.flatMap(_.panels).find(_.letter.value == "C").map(_.toString)
      all(
        expect(
          "bundle files",
          Set("results.csv", "participants.csv", "comparisons.csv", "methods.md", "README.txt"),
          files.keySet
        ),
        // The methods text against FIXTURE.md, the parity checklist and eyes4s's
        // own tallies: the admitted trials' records and the share of their
        // fixation duration (the owner's decision on the S10.1 bead).
        listed(
          "methods: outside, as the checklist says",
          "543 of the 11,311 fixation records of admitted trials (4.8% of their fixation duration).",
          methods.contains(
            "543 of the 11,311 fixation records of admitted trials (4.8% of their fixation duration)"
          ),
          methods
        ),
        stated(
          "methods: eligible",
          "Eligible (computed) queries: 457",
          true,
          methods.contains("Of 480 retrieval queries, 457 were eligible")
        ),
        expect(
          "methods: outside",
          true,
          methods.contains(
            s"543 of the ${eyes4s.studio.app.text.Format.count(library.admittedRecords.toLong)} " +
              "fixation records of admitted trials " +
              s"(${round(library.outsideDurationShare * 100, 1)}% of their fixation duration), " +
              "in 409 trials"
          )
        ),
        // One row per query and scale; one per participant and group; each
        // after its header.
        expect("results.csv rows", 480 * 4, results.size - 1),
        expect("participants.csv rows", 24 * 2, participants.size - 1),
        listed(
          "panel C",
          "Panel C reads \"Matched 0.73 · highest of 19 controls street-112 0.61 · control mean B 0.35 · D +0.38\".",
          panelC.exists(
            _.contains(
              "Matched 0.73 · highest of 19 controls street-112 0.61 · control mean B 0.35 · D +0.38"
            )
          ),
          panelC.toString
        )
      )

    // -------------------------------------------------------------------------
    // Close and reopen: a real project folder, saved twice, byte-identical
    // -------------------------------------------------------------------------

    // The journey's temporary project folders, removed after the check.
    private def remove(root: Path): Unit =
      Files.walk(root).iterator.asScala.toVector.reverse.foreach(Files.deleteIfExists(_))

    private def folder(root: Path): Map[String, Vector[Byte]] =
      Files
        .walk(root)
        .iterator
        .asScala
        .filter(Files.isRegularFile(_))
        // Volatile: the store's lock files (FileProjectStore), named; the
        // empty .lock stays after a release.
        .filterNot(p =>
          Set(FileProjectStore.LockName, FileProjectStore.OwnerName)(p.getFileName.toString)
        )
        .map(p => root.relativize(p).toString -> Files.readAllBytes(p).toVector)
        .toMap

    /** An input's kind, name and bytes. */
    private type Input = (InputKind, String, IArray[Byte])

    /** The images Repair… stored in the project, in the order it stored them. */
    @volatile var stored: Vector[Input] = Vector.empty

    /** The golden inputs, from fixtures/studio-golden. */
    private def golden: Vector[Input] =
      Vector(SourceRole.Fixations -> "fixations.csv", SourceRole.Trials -> "trials.csv").map {
        (role, name) =>
          (
            InputKind.Source(role),
            name,
            IArray.unsafeFromArray(
              Files.readAllBytes(doc.root.resolve(s"fixtures/studio-golden/$name"))
            )
          )
      }

    /** Save `document` as a new project at `root`, its `inputs` stored. */
    private def saveTo(root: Path, document: StudioDocument, inputs: Vector[Input]) =
      for
        store <- FileProjectStore.at[IO](root)
        lock  <- store.acquire(ok(LockOwner.of("golden journey"))).map(_.leftMap(_.message))
        saved <- lock.flatTraverse { l =>
          inputs
            .traverse((kind, name, bytes) =>
              ProjectBundle.importInput(store, l, kind, name, bytes)
            )
            .map(_.sequence.leftMap(_.message))
            .flatMap(_.flatTraverse { entries =>
              ProjectBundle.encode(document, SharingOptions.complete, entries) match
                case Left(e)        => IO.pure(Left(e.message))
                case Right(encoded) =>
                  ProjectBundle.save(store, l, None, encoded).map(_.bimap(_.message, _ => ()))
            })
            .flatTap(_ => store.release(l))
        }
      yield saved

    /** The opened project's stored inputs, read back from its folder. */
    private def storedInputs(store: ProjectStore[IO], o: OpenedProject) =
      o.manifest.inputs
        .traverse { entry =>
          entry.path match
            case Some(path) =>
              store
                .read(path)
                .map(
                  _.bimap(
                    _.message,
                    bytes => (entry.kind, entry.name.getOrElse(path.value), bytes)
                  )
                )
            case None => IO.pure(Left(s"input ${entry.name} is not stored"))
        }
        .map(_.sequence)

    private val closeAndReopen: S = Step(
      "save, close, reopen from the folder, save again: byte-identical",
      d =>
        val base  = Files.createTempDirectory("golden-journey")
        val first = base.resolve("first.eyes")
        val again = base.resolve("again.eyes")
        (for
          saved  <- saveTo(first, d.model.document, golden ++ stored)
          store  <- FileProjectStore.at[IO](first)
          opened <- ProjectBundle.open(store).map(_.leftMap(_.message))
          // The second save takes its inputs from the opened folder.
          reopened <- opened.flatTraverse(o =>
            storedInputs(store, o).flatMap(
              _.flatTraverse(in => saveTo(again, o.document, in)).map(_.as(o))
            )
          )
          bytes <- IO((folder(first), folder(again)))
        yield (saved, reopened, bytes))
          .guarantee(IO(remove(base)))
          .unsafeToFuture()
          .map { (saved, reopened, bytes) =>
            for
              _ <- saved.leftMap(why => DriverError.Expectation("save", "saved", why))
              o <- reopened
                .leftMap(why => DriverError.Expectation("reopen and save again", "done", why))
              _ <- all(
                expect("reopened document", d.model.document, o.document),
                expect("files", true, bytes._1.keySet.exists(_.startsWith("inputs"))),
                expect("bytes", bytes._1, bytes._2)
              )
              _ = { closed = Some(d.model.document); savedFolder = bytes._1 }
              // The driver reopens the project from the folder's document.
              next = d.openProject(
                AppModel
                  .update(
                    AppModel.open(o.document, d.model.project),
                    Intent.ItemsLoaded(d.model.items)
                  )
                  ._1
              )
              _ <- all(
                expect("reopened model", d.model.document, next.model.document),
                expect("badge", d.context.freshness, next.context.freshness),
                expect("project", d.model.project, next.model.project)
              )
            yield next
          }
    )

    // -------------------------------------------------------------------------
    // Repair (S5.7) and linked selection (S6.6)
    // -------------------------------------------------------------------------

    /** Data › r3 › Sources: Repair… for each missing image, the located file
      * answered as the platform answers it (a stored file and its bytes'
      * digest), the relink the pane asks for dispatched; then every image of
      * the 259 is found.
      */
    private val repair: S = Step(
      "repair the missing images",
      d =>
        import eyes4s.studio.app.data.{SourcesEffect, SourcesIntent, SourcesPane, SourcesVM}
        import eyes4s.studio.app.explore.DisplaySource
        import eyes4s.studio.app.nav.{DataSection, Location, Place}
        import eyes4s.studio.core.assets.AssetFile
        val r3 = StoryMoments.r3
        val at = d.dispatch(
          Intent.Navigate(
            Location(
              Perspective.Data,
              Vector(Place.Dataset(r3), Place.DataView(DataSection.Sources))
            )
          )
        )
        val spec     = at.model.document.dataset(r3).get
        val (p0, e0) = SourcesPane.sync(SourcesPane.empty, at.model)
        val ask      = e0.collectFirst { case SourcesEffect.ReadRegistry(`spec`, n) => n }
        val served   =
          GoldenAssets.registry(spec).map(eyes4s.studio.app.explore.DisplaySource.Served(_))
        // A stand-in image the author picks: another stimulus's bytes.
        val bytes = IArray.unsafeFromArray(
          Files.readAllBytes(doc.root.resolve("fixtures/studio-golden/stimuli/beach-042.png"))
        )
        val sha = eyes4s.codec.ByteDigest.sha256(bytes)
        // Repair… until nothing is missing: each asks to locate one file.
        def loop(
            p: SourcesPane,
            d: StudioDriver,
            n: Int
        ): Either[DriverError, (SourcesPane, StudioDriver)] =
          if n > 3 then GoldenJourney.fail("repair", "at most 2 files", "more")
          else
            SourcesVM.of(p, d.model).missing match
              case None    => Right((p, d))
              case Some(_) =>
                val (asked, effects) = SourcesPane.update(p, d.model, SourcesIntent.Repair)
                effects.collectFirst { case SourcesEffect.Locate(`r3`, file) => file } match
                  case None       => GoldenJourney.fail("repair", "a file to locate", effects)
                  case Some(file) =>
                    val restored =
                      ok(AssetFile.of(file.value.stripSuffix(".png") + "_restored.png"))
                    // The platform stores the chosen file in the project, as
                    // the window's Repair… does (SourcesPaneHost).
                    stored = stored :+ ((InputKind.StimulusImage, restored.value, bytes))
                    val (located, more) =
                      SourcesPane.update(
                        asked,
                        d.model,
                        SourcesIntent.Located(r3, file, restored, sha)
                      )
                    more
                      .collect { case SourcesEffect.App(i) => i }
                      .foldLeft(Right(d): Either[DriverError, StudioDriver])((acc, i) =>
                        acc.flatMap(_.perform(s"relink ${file.value}", i))
                      )
                      .flatMap(next => loop(located, next, n + 1))
        Future.successful(for
          n <- ask.toRight(DriverError.Expectation("registry", "a read", e0.toString))
          before = SourcesPane
            .update(p0, at.model, SourcesIntent.RegistryRead(r3, n, served))
            ._1
          _ <- expect(
            "before",
            Some(s"${library.imagesFound} of ${library.items} images found"),
            SourcesVM.of(before, at.model).sources.lift(2).flatMap(_.count)
          )
          (after, repaired) <- loop(before, at, 0)
          vm = SourcesVM.of(after, repaired.model)
          _ <- all(
            expect(
              "after",
              Some("259 of 259 images found"),
              vm.sources.lift(2).flatMap(_.count)
            ),
            expect("relinks", 2, repaired.model.document.relinks.of(r3).size),
            expect("missing", None, vm.missing.map(_.title))
          )
        yield repaired)
    )

    /** Explore P17 enc_03 under run 6: selecting fixation 6 brings the trail
      * to it and its record 7,214, and Next selects fixation 7, record 7,215.
      */
    private val linked: S = Step(
      "linked selection: fixation 6 ↔ record 7,214 ↔ timeline",
      d =>
        import eyes4s.studio.app.explore.{
          ExploreLinked,
          ExploreTrialView,
          TrialViewEffect,
          TrialViewIntent,
          BackendAnswer
        }
        import eyes4s.studio.core.selection.FixationIndex
        val at =
          d.dispatch(Intent.Explain(eyes4s.studio.app.nav.Place.At(StudioRef.Trial(p17enc03))))
        val (synced, effects) = ExploreTrialView.sync(ExploreTrialView.empty, at.model)
        effects.collectFirst { case TrialViewEffect.RequestFixations(r, t, n) =>
          (r, t, n)
        } match
          case None =>
            Future.successful(GoldenJourney.fail("trial view", "a fixations read", effects))
          case Some((r, t, n)) =>
            views.trialFixations(r, t).map { a =>
              val answer =
                Right(a.fold(e => BackendAnswer.Refused(e.message), BackendAnswer.Answered(_)))
              val view = ExploreTrialView
                .update(synced, TrialViewIntent.FixationsRead(r, t, n, answer))
                ._1
              def fixation(i: Int) = StudioRef.Fixation(p17enc03, ok(FixationIndex.of(i)))
              def follow(d: StudioDriver) =
                ExploreLinked.follow(view, d.model).fold(Right(d))(i => d.perform("follow", i))
              for
                _  <- expect("revision", rev4, r)
                d1 <- at.perform(
                  "select fixation 6",
                  StoryModels.select(at.model, "explore.trial-view", fixation(6))
                )
                d2 <- follow(d1)
                _  <- stated(
                  "trail at fixation 6",
                  "P17 enc_03 fixation 6 = fixations.csv record 7,214",
                  Vector("enc_03 · fixation 6", "fixations.csv record 7,214"),
                  d2.crumbs.takeRight(2)
                )
                _    <- expect("eyes4s: record", library.focus.record, 7214)
                next <- ExploreLinked
                  .step(view, d2.model, 1)
                  .toRight(DriverError.Expectation("Next", "a fixation", "none"))
                _  <- expect("Next", fixation(7), next)
                d3 <- d2
                  .perform("Next", StoryModels.select(d2.model, "explore.trial-view", next))
                d4 <- follow(d3)
                _ <- expect("trail at fixation 7", "fixations.csv record 7,215", d4.crumbs.last)
              yield d4
            }
    )

    /** What only the run decides, from eyes4s itself (S3.7, S0.7b): the
      * journey's reopened dataset r3 and analysis rev 4, admitted and run
      * directly by eyes4s, give SCORES.json's M, B and D for every query at
      * every scale (to its 6-place rounding), FIXTURE.md's 454 contributing
      * and 3 failed of the 457 eligible, and groups of 2 to 17 queries.
      */
    private val libraryScores: S = Step.check[Future](
      "direct-library scores = SCORES.json = FIXTURE.md"
    ) { d =>
      import eyes4s.plan.{ResultInspection, ResultRef, ScaleContrast, StudyDesign}
      import eyes4s.studio.core.fixture.{GoldenCsv, GoldenScores}
      import eyes4s.studio.core.real.{RealAdmission, RealPrepared}
      val tolerance = 5e-7 + 1e-12
      val document  = closed.getOrElse(d.model.document)
      for
        spec <- document
          .dataset(StoryMoments.r3)
          .toRight(DriverError.Expectation("scores", "dataset r3", "none"))
        recipe <- document
          .analysis(rev4)
          .map(_.recipe)
          .toRight(DriverError.Expectation("scores", "analysis rev 4", "none"))
        registry <- GoldenAssets
          .registry(spec)
          .leftMap(e => DriverError.Expectation("scores", "the asset registry", e.toString))
        admitted <- RealAdmission
          .admit(spec, GoldenCsv.fixations, GoldenCsv.trials, registry)
          .leftMap(e => DriverError.Expectation("scores", "eyes4s admission", e.message))
        prepared <- RealPrepared
          .of(rev4, StoryMoments.r3, recipe, admitted)
          .leftMap(e => DriverError.Expectation("scores", "a prepared study", e.message))
        result <- prepared.work.run
          .leftMap(e => DriverError.Expectation("scores", "a completed run", e.toString))
        inspected <- ResultInspection
          .study(prepared.plan, result, admitted.input, Some(admitted.evidence))
          .leftMap(e => DriverError.Expectation("scores", "an inspection", e.toString))
        scores <- io.circe.parser
          .parse(GoldenScores.text)
          .leftMap(e => DriverError.Expectation("scores", "SCORES.json", e.message))
        queries = scores.hcursor.downField("queries").values.toVector.flatten
        keys    = admitted.input.trials.rows.map(_.key)
        checked <- queries.traverse { q =>
          val c                   = q.hcursor
          def text(field: String) = c.get[String](field).toOption.getOrElse("")
          val status              = text("status")
          if status != "contributing" then Right(None)
          else
            val key = keys.find(k =>
              k.participant == text("participant") && k.phase == "Retrieval" &&
                k.trial == text("trial")
            )
            key
              .toRight(DriverError.Expectation("scores", "a query key", text("trial")))
              .flatMap { k =>
                Vector("0.5", "1", "2", "4").zipWithIndex
                  .traverse { (sigma, i) =>
                    val scale                        = inspected.scales(i)
                    def reduced(design: StudyDesign) = scale
                      .reductions(design)
                      .get(ResultRef.Reduction(i, design, k))
                      .flatMap(_.outcome.toOption)
                      .map(_.value.value)
                    val difference = scale.contrast match
                      case ScaleContrast.Rows(rows) =>
                        rows
                          .get(ResultRef.ContrastRow(i, k))
                          .flatMap(_.outcome.toOption)
                          .map(_.value.value)
                      case _ => None
                    val pinned = c.downField("scales").downField(sigma)
                    Vector(
                      "M" -> reduced(StudyDesign.Matched),
                      "B" -> reduced(StudyDesign.Control),
                      "D" -> difference
                    ).traverse { (role, value) =>
                      val at = s"${text("participant")} ${text("trial")} σ $sigma $role"
                      (value, pinned.get[Double](role).toOption) match
                        case (Some(v), Some(p)) if math.abs(v - p) <= tolerance => Right(())
                        case other                                              =>
                          Left(DriverError.Expectation(at, "SCORES.json", other.toString))
                    }
                  }
                  .as(Some(text("participant") -> text("response")))
              }
        }
        contributing = checked.flatten
        failed       = queries.count(
          _.hcursor.get[String]("status").toOption.exists(_.startsWith("failed:"))
        )
        groups = contributing.groupBy(identity).values.map(_.size)
        _ <- all(
          stated(
            "contributing",
            "Eligible (computed) queries: 457 (454 contributing + 3 failed)",
            (457, 454, 3),
            (contributing.size + failed, contributing.size, failed)
          ),
          expect("group sizes", (2, 17), (groups.min, groups.max))
        )
      yield ()
    }

    // -------------------------------------------------------------------------

    val scenario: Scenario[Future] =
      val g = GoldenJourney.stages(s)
      g.newProject ++
        g.importAndAdmit ++ Scenario.of(admission, repair) ++
        g.explore ++
        g.analysisAndRun ++ Scenario.of(design, focus, linked) ++
        g.compare ++ Scenario.of(query, controls) ++
        g.summary ++ Scenario.of(summary) ++
        rev5 ++
        figureAndExport ++
        Scenario.of(Step.settle[Future](s), closeAndReopen, libraryScores)
