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
import eyes4s.studio.core.fixture.{GoldenAssets, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.figures.BundleFiles
import eyes4s.studio.desktop.platform.FileProjectStore

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*

/** E2E-01 (ticket S10.1): the golden journey through the headless driver, its
  * numbers held to FIXTURE.md and, where the library can say, to eyes4s run
  * directly on fixtures/studio-golden ([[GoldenLibrary]]):
  *
  * new project → import the fixture → map columns → admit r3 → explore P17
  * enc_03 → Analysis rev 4 → run → Compare P17 ret_07 → summary → figure →
  * export → close → reopen → identical.
  *
  * The figure is composed and its bundle exported through the composer and
  * the desktop's bundle assembly; the project is saved to a real project
  * folder, reopened, and saved again, and the two folders are byte-identical.
  *
  * Pending, each a named stub in the driver's record:
  *  - [[Pending.Repair]]: S5.7's Repair of the two missing images;
  *  - [[Pending.LinkedSelection]]: S6.6's linked selection in Explore;
  *  - [[Pending.LibraryScores]]: scores from eyes4s itself. The fake serves
  *    fixture.json's scores, not eyes4s's on these files (fixtures/
  *    studio-golden/README.md); S0.7b freezes eyes4s's as SCORES.json and S3.7
  *    runs the real backend. Until then the scores are held to FIXTURE.md.
  */
class GoldenJourneySuite extends munit.FunSuite:
  import GoldenJourney.*
  import StoryModels.{ok, p17enc03, p17ret07, sigma2}

  override val munitTimeout: Duration = 300.seconds

  private given ExecutionContext = ExecutionContext.global

  /** The steps a later ticket implements, by name, and why. */
  object Pending:
    val Repair: (String, String) = (
      "repair the missing images",
      "pending S5.7 (Assets in Data: Repair), not on main"
    )
    val LinkedSelection: (String, String) = (
      "linked selection: fixation 6 ↔ record 7,214 ↔ timeline",
      "pending S6.6 (Linked selection in Explore), not on main"
    )
    val LibraryScores: (String, String) = (
      "direct-library scores",
      "pending S0.7b (SCORES.json from eyes4s) and S3.7 (real backend)"
    )

  private val doc     = FixtureDoc
  private val library = GoldenLibrary.facts
  private val run6    = RunId(6)
  private val rev4    = StoryMoments.rev4

  private def round(v: Double, places: Int = 2) = FixtureDoc.rounded(v, places)

  /** A FIXTURE.md phrase, required verbatim, and the value the journey saw. */
  private def stated[A](step: String, phrase: String, expected: A, actual: A) =
    if !doc.states(phrase) then
      GoldenJourney.fail(step, s"FIXTURE.md to state '$phrase'", "it does not")
    else expect(step, expected, actual)

  /** A parity-checklist item, required verbatim, and whether the journey saw it. */
  private def listed(step: String, phrase: String, seen: Boolean, shown: => String) =
    if !doc.lists(phrase) then
      GoldenJourney.fail(step, s"PARITY_CHECKLIST.md to list '$phrase'", "it does not")
    else expect(step, s"$phrase: shown", if seen then s"$phrase: shown" else shown)

  private def all(checks: Either[DriverError, Unit]*): Either[DriverError, Unit] =
    checks.toVector.sequence_

  private def pending(p: (String, String)): S = Step.stub[Future](p._1, p._2)

  /** The journey's own steps, on session `s`. */
  private final class Journey(s: HeadlessSession, views: HeadlessSession):

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
              "Cartesian candidate pairs before paging, per scale: 230,400",
              230400L,
              p.candidatePairsPerScale
            ),
            stated(
              "pair rows",
              "Pair rows: 8969 per scale, 35876 across 4 scales.",
              (8969L, 35876L, 4),
              (p.pairRowsPerScale, p.pairRows, p.scales.size)
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
              windowCheck("enc_03", e, 13, 1, 3, library.enc03),
              windowCheck("ret_07", r, 12, 1, 4, library.ret07),
              stated(
                "focus outside",
                "Focus query outside window: ret_07 1 of 12 fixations · 4% of duration; " +
                  "enc_03 1 of 13 fixations · 3% of duration.",
                true,
                true
              )
            )
          yield d
    )

    /** A trial's served fixations against the window: how many, how many
      * outside, and the outside share of fixation duration, as FIXTURE.md
      * rounds it and as eyes4s tallies it.
      */
    private def windowCheck(
        name: String,
        t: TrialFixations,
        count: Int,
        outside: Int,
        percent: Int,
        tally: TrialWindow
    ): Either[DriverError, Unit] =
      val out = t.fixations.filter(_.placement match
        case MapPlacement.OutsideWindow(_) => true
        case _                             => false)
      val share = out.map(_.durationMs).sum / t.fixations.map(_.durationMs).sum
      all(
        expect(
          s"$name fixations",
          (count, outside, percent),
          (t.fixations.size, out.size, (share * 100).round.toInt)
        ),
        expect(
          s"eyes4s: $name",
          (tally.fixations, tally.outside, tally.outsideShare.map(round(_, 6))),
          (t.fixations.size, out.size, Some(round(share, 6)))
        )
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
              expect("M by scale", doc.list("M/B/D by scale: M"), mbd.map(_._1)),
              expect("B by scale", doc.list("0.86] B"), mbd.map(_._2)),
              expect("D by scale", doc.list("0.63] D"), mbd.map(_._3))
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
      "the participant table and group summaries = FIXTURE.md",
      d =>
        service("result")(s.result(run6)).map(_.flatMap { r =>
          def groupD(p: ParticipantSummary, label: Response) =
            p.groups.find(_.label == label).map(g => (round(g.d), g.n))
          val served = r.participants.map { p =>
            FixtureRow(
              p.participant,
              p.requested,
              p.contributing,
              p.failed,
              p.noMatch,
              p.notAdmitted,
              round(p.all.m),
              round(p.all.b),
              round(p.all.d),
              groupD(p, Response.Remembered).getOrElse((BigDecimal(-99), -1)),
              groupD(p, Response.Forgotten).getOrElse((BigDecimal(-99), -1))
            )
          }
          def byScale(label: Response) =
            r.groups.find(_.label == label).map(_.dByScale.map(round(_)))
          all(
            expect("participant table", doc.participants, served),
            expect("grand D by scale", doc.list("By scale:"), r.grandDByScale.map(round(_))),
            expect(
              "Remembered by scale",
              Some(doc.list("Remembered [")),
              byScale(Response.Remembered)
            ),
            expect(
              "Forgotten by scale",
              Some(doc.list("Forgotten [")),
              byScale(Response.Forgotten)
            ),
            stated(
              "group n range",
              "Per-group n range across participants (Remembered/Forgotten): [2, 17].",
              (2, 17),
              (r.groupNMinimum, r.groupNMaximum)
            ),
            stated(
              "contributing",
              "\"no_match\": 9, \"failed\": 3, \"contributing\": 454}",
              QueryContrasts(480, 14, 9, 3, 454),
              r.contrasts
            ),
            // P05's failed queries: every fixation outside the window, by eyes4s.
            stated(
              "P05 failed",
              "P05's 3 failed queries: 11 of 11 fixations outside.",
              Vector(("P05", "ret_04", 11), ("P05", "ret_11", 11), ("P05", "ret_16", 11)),
              library.allOutside
            )
          ).as(d)
        })
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
            case ComposerEffect.WriteBundle(request) =>
              Future.successful((c, captured :+ request))
            case _ => Future.successful((c, captured))
        }
      }

    /** The exported bundle's files, by name, kept for the reopen check. */
    @volatile var exported: Map[String, String] = Map.empty

    private val figureAndExport: Scenario[Future] = Scenario.of[Future](
      sync("create Figure 3 on run 6, panels A–E as the board's Figure 1")(d =>
        StoryMoments.figure1
          .leftMap(e => DriverError.Expectation("figure 1", "valid", e.message))
          .flatMap(f => d.command(Command.CreateFigure(run6, StoryModels.reporting, f.panels)))
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
            (c2, e2) = FigureComposer.update(c1, model, ComposerIntent.ExportBundle)
            (_, req) <- compose(c2, model, e2, Vector.empty)
            summary  <- s.result(run6)
            rows     <- MethodsReads.queryRows[Future](s.queries, run6)
          yield
            for
              request <- req.headOption.toRight(
                DriverError.Expectation("bundle", "a request", "none")
              )
              sum <- summary.leftMap(e =>
                DriverError.Service("result", ServiceError.Backend(e))
              )
              all_  <- rows.leftMap(e => DriverError.Expectation("rows", "read", e.message))
              files <- BundleFiles
                .assemble(request, sum, all_)
                .leftMap(why => DriverError.Expectation("bundle", "assembled", why))
              text = files.collect {
                case (n, bytes) if !n.endsWith(".svg") => n -> String(Array.from(bytes), UTF_8)
              }.toMap
              page = FigureComposer.view(c2, model).page
              _ <- figureChecks(text, page)
            yield
              exported = text
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

    private def figureChecks(files: Map[String, String], page: Option[PageVM]) =
      val methods      = files.getOrElse("methods.md", "")
      val results      = files.getOrElse("results.csv", "").linesIterator.toVector
      val participants = files.getOrElse("participants.csv", "").linesIterator.toVector
      val panelC = page.toVector.flatMap(_.panels).find(_.letter.value == "C").map(_.toString)
      all(
        expect(
          "bundle files",
          Set("results.csv", "participants.csv", "methods.md", "README.txt"),
          files.keySet
        ),
        // The methods text against FIXTURE.md and eyes4s's own tallies. The
        // checklist's "543 of 11,520 records (4.7%)" counts every record; the
        // generator counts the admitted trials' records and the share of their
        // fixation duration (reported as a parity deviation).
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
          "Panel C reads \"highest of 19 · B 0.35\".",
          // The panel words it "highest of 19 controls street-112 0.61 · control
          // mean B 0.35"; the board's numbers, in other words.
          panelC.exists(c => c.contains("highest of 19") && c.contains("B 0.35")),
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
        .filterNot(_.getFileName.toString.startsWith("."))
        .map(p => root.relativize(p).toString -> Files.readAllBytes(p).toVector)
        .toMap

    private def saveTo(root: Path, document: StudioDocument): IO[Either[String, Unit]] =
      val sources = Vector(
        SourceRole.Fixations -> "fixations.csv",
        SourceRole.Trials    -> "trials.csv"
      )
      for
        store <- FileProjectStore.at[IO](root)
        lock  <- store.acquire(ok(LockOwner.of("golden journey"))).map(_.leftMap(_.message))
        saved <- lock.flatTraverse { l =>
          sources
            .traverse { (role, name) =>
              val bytes = IArray.unsafeFromArray(
                Files.readAllBytes(doc.root.resolve(s"fixtures/studio-golden/$name"))
              )
              ProjectBundle.importInput(store, l, InputKind.Source(role), name, bytes)
            }
            .map(_.sequence.leftMap(_.message))
            .flatMap(_.flatTraverse { inputs =>
              ProjectBundle.encode(document, SharingOptions.complete, inputs) match
                case Left(e)        => IO.pure(Left(e.message))
                case Right(encoded) =>
                  ProjectBundle.save(store, l, None, encoded).map(_.bimap(_.message, _ => ()))
            })
            .flatTap(_ => store.release(l))
        }
      yield saved

    private val closeAndReopen: S = Step(
      "save, close, reopen, save again: byte-identical",
      d =>
        val base  = Files.createTempDirectory("golden-journey")
        val first = base.resolve("first.eyes")
        val again = base.resolve("again.eyes")
        (for
          saved    <- saveTo(first, d.model.document)
          store    <- FileProjectStore.at[IO](first)
          opened   <- ProjectBundle.open(store)
          reopened <- opened.traverse(o => saveTo(again, o.document).map(_.as(o)))
          bytes    <- IO((folder(first), folder(again)))
        yield (saved, opened, reopened, bytes))
          .guarantee(IO(remove(base)))
          .unsafeToFuture()
          .map { (saved, opened, reopened, bytes) =>
            for
              _ <- saved.leftMap(why => DriverError.Expectation("save", "saved", why))
              o <- opened.leftMap(DriverError.Bundle(_))
              _ <- reopened
                .leftMap(DriverError.Bundle(_))
                .flatMap(_.leftMap(why => DriverError.Expectation("save again", "saved", why)))
              _ <- all(
                expect("reopened document", d.model.document, o.document),
                expect("files", true, bytes._1.keySet.exists(_.startsWith("inputs"))),
                expect("bytes", bytes._1, bytes._2)
              )
              next <- d.reopen(BundleSamples.inputsFor(d.model.document))
              _    <- expect("reopened model", d.model.document, next.model.document)
            yield next
          }
    )

    // -------------------------------------------------------------------------

    val scenario: Scenario[Future] =
      val g = GoldenJourney.stages(s)
      g.newProject ++
        g.importAndAdmit ++ Scenario.of(admission, pending(Pending.Repair)) ++
        g.explore ++ Scenario.of(pending(Pending.LinkedSelection)) ++
        g.analysisAndRun ++ Scenario.of(design, focus) ++
        g.compare ++ Scenario.of(query, controls) ++
        g.summary ++ Scenario.of(summary) ++
        rev5 ++
        figureAndExport ++
        Scenario.of(closeAndReopen, pending(Pending.LibraryScores))

  test(
    "E2E-01: the golden journey, every FIXTURE.md number, eyes4s's counts, reopened identical"
  ) {
    (HeadlessSession.open(StoryMoment.T1), HeadlessSession.open(StoryMoment.T2)).tupled
      .flatMap { (session, views) =>
        val journey = Journey(session, views)
        journey.scenario
          .run(StudioDriver.open(StoryModels.firstRun))
          .transformWith(result => (session.close, views.close).tupled.transform(_ => result))
          .map {
            case Left(failure) => fail(failure.message)
            case Right(end)    =>
              assertEquals(
                end.records.collect { case DriverRecord.Stubbed(step, _) => step },
                Vector(
                  "import dialog",
                  Pending.Repair._1,
                  Pending.LinkedSelection._1,
                  Pending.LibraryScores._1
                )
              )
              assertEquals(
                end.records.collect { case r: DriverRecord.Refused => r },
                Vector.empty
              )
              assert(journey.exported.contains("methods.md"), journey.exported.keySet)
          }
      }
  }
