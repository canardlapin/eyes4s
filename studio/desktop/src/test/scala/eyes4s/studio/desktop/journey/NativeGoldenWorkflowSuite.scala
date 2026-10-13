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
import cats.syntax.all.*
import eyes4s.codec.{ByteDigest, CanonicalDigest}
import eyes4s.plan.{ResultInspection, ResultRef, TrialKeyDefinitions}
import eyes4s.studio.app.{AppModel, ProjectName}
import eyes4s.studio.app.compare.SummaryAnswer
import eyes4s.studio.app.figures.*
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.figures.{FigureSource, MethodsReads}
import eyes4s.studio.core.fixture.{GoldenScores, StoryMoments}
import eyes4s.studio.core.headless.NativeReads
import eyes4s.studio.core.real.{DatasetSources, RealAdmission, RealPrepared, RealStudyBackend}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}
import eyes4s.studio.core.session.ProjectSession
import eyes4s.studio.desktop.figures.{BundleFiles, BundleWriter}
import eyes4s.studio.desktop.platform.{FileProjectStore, TempDirs}
import eyes4s.studio.desktop.runtime.{DatasetSourceHosts, SessionPort}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import scala.concurrent.duration.*

/** One native scientific workflow, separate from the illustrative story journeys. */
class NativeGoldenWorkflowSuite extends munit.CatsEffectSuite:
  override val munitIOTimeout: Duration                = 300.seconds
  private def get[E, A](value: Either[E, A]): A        = value.fold(e => fail(s"$e"), identity)
  private def ok[E, A](value: IO[Either[E, A]]): IO[A] = value.flatMap(v => IO(get(v)))
  // SCORES.json stores six decimal places; include its quantization bound
  // and decimal-to-double rounding, as its native score qualification does.
  private val ScoresTolerance: Double = 5e-7 + 1e-12
  private val scale                   = get(ScaleIndex.of(2))
  private val owner                   = get(LockOwner.of("NativeGoldenWorkflowSuite"))
  private val golden                  = FixtureDoc.root.resolve("fixtures/studio-golden")

  private def rebuild(
      base: StudioDocument,
      datasets: Vector[DatasetRevisionSpec],
      analyses: Vector[AnalysisRevisionSpec],
      runs: Vector[RunRef],
      figures: Vector[FigureSpec],
      reporting: Vector[ReportingSpec]
  ): StudioDocument =
    get(
      StudioDocument.of(
        datasets,
        analyses,
        None,
        runs,
        reporting,
        figures,
        base.presentation,
        Vector.empty
      )
    )

  private lazy val declared: StudioDocument =
    val base     = get(StoryMoments.t2)
    val datasets = base.datasets.map { d =>
      if d.id != StoryMoments.r3 then d
      else
        d.copy(inventory =
          d.inventory.map(mapping =>
            get(
              InventoryMapping.withDisplays(
                mapping,
                Some(
                  DisplayColumns(
                    get(ColumnName.of("display_kind")),
                    Some(get(ColumnName.of("image_file")))
                  )
                )
              )
            )
          )
        )
    }
    val analyses = base.analyses.map(a =>
      a.copy(recipe =
        a.recipe.copy(layout = DefinitionRef.fromCore(TrialKeyDefinitions.trialLayout))
      )
    )
    val spec    = base.reporting.head
    val grouped = get(
      ReportingSpec.of(
        spec.id,
        spec.name,
        spec.groupBy,
        spec.filters,
        spec.minimumPerGroup,
        spec.weighting,
        Some(get(ReportingContrast.of("Remembered", "Forgotten")))
      )
    )
    val figures = base.figures.map { f =>
      if f.run != StoryMoments.run7 then f
      else
        get(
          FigureSpec.of(
            f.id,
            f.run,
            f.reporting,
            f.panels.filter(p => PanelTemplate.of(p).isInstanceOf[PanelTemplate.ParticipantD])
          )
        )
    }
    rebuild(base, datasets, analyses, base.runs, figures, grouped +: base.reporting.tail)

  private final case class Readback(
      summary: ResultSummary,
      rows: Vector[QueryRow],
      report: ReportView,
      source: SourceRecordPage,
      provenance: Provenance,
      files: Vector[(String, IArray[Byte])]
  )

  private def readback(document: StudioDocument, sources: DatasetSources[IO]): IO[Readback] =
    RealStudyBackend
      .resource[IO](document, sources)
      .flatMap { backend =>
        NativeReads.resource[IO](backend, backend.navigator).map(backend -> _)
      }
      .use { (backend, reads) =>
        val run    = StoryMoments.run7
        val spec   = document.reporting.head
        val figure = document.figures.find(_.run == run).getOrElse(fail("no native figure"))
        for
          summary <- ok(reads.result(run))
          rows    <- ok(MethodsReads.queryRows[IO](reads.queries, run))
          report  <- ok(reads.report(run, spec, scale.value))
          focus = rows
            .find(q => q.query.participant == "P17" && q.query.trial == "ret_07")
            .getOrElse(fail("no native focus query"))
          inspected <- ok(
            reads.inspect(run, ResultAddress.ContrastRow(scale.value, focus.query))
          )
          provenance <- ok(
            reads.provenance(run, ResultAddress.ContrastRow(scale.value, focus.query))
          )
          pairs <- ok(
            reads.navigator.pairs(
              StudioRef.QueryContrast(run, scale, focus.query),
              PairDesign.Matched,
              get(PageRequest.of(0, 1))
            )
          )
          pair = pairs.entries.headOption.getOrElse(fail("no matched pair"))
          maps      <- ok(reads.navigator.maps(pair))
          fixations <- ok(reads.navigator.fixations(maps.query, get(PageRequest.of(0, 1))))
          fixation = fixations.entries.headOption.getOrElse(fail("no native source fixation"))
          record <- ok(reads.navigator.record(fixation))
          number = record match
            case StudioRef.SourceRecord(_, _, _, record) => record.value
            case other => fail(s"unexpected native source reference $other")
          source   <- ok(backend.sourceRecords(StoryMoments.rev4, number, 1))
          pairRows <- ok(MethodsReads.pairRows[IO](reads.pairRows, run, summary.scales.size))
          model   = AppModel.open(document, Some(get(ProjectName.of("Native golden workflow"))))
          initial = FigureComposer.sync(FigureComposer.empty, model)
          reportReplies <- initial._2
            .collect { case ComposerEffect.RequestReport(run, spec, scale) =>
              (run, spec, scale)
            }
            .traverse { (run, spec, scale) =>
              ok(reads.report(run, spec, scale.value))
                .map(r => ComposerIntent.ReportRead(run, spec, scale, Right(r)))
            }
          composer = (ComposerIntent.SummaryRead(
            run,
            SummaryAnswer.Answered(summary)
          ) +: reportReplies)
            .foldLeft(initial._1)((c, i) => FigureComposer.update(c, model, i)._1)
          page = FigureComposer
            .view(composer, model)
            .page
            .getOrElse(fail("no native figure page"))
          _ = page.panels.foreach { panel =>
            panel.body match
              case PanelBody.Plot(plot) =>
                val refs   = plot.source.rows.map(_.ref)
                val people = refs.collect {
                  case StudioRef.ReportParticipant(
                        `run`,
                        _,
                        `scale`,
                        _,
                        ReportRole.Difference,
                        participant
                      ) =>
                    participant
                }.distinct
                assertEquals(people.size, 24)
                assert(
                  refs.forall {
                    case StudioRef
                          .ReportParticipant(`run`, _, `scale`, _, ReportRole.Difference, _) =>
                      true
                    case StudioRef.ReportCell(`run`, _, `scale`, _, ReportRole.Difference) =>
                      true
                    case _ => false
                  },
                  refs
                )
              case other => fail(s"native figure contains a placeholder: $other")
          }
          bound   = get(FigureSource.of(document, figure.id))
          request = BundleRequest(
            bound,
            page,
            ExportFormat.Svg,
            None,
            Vector(
              BundleItem.Figure,
              BundleItem.Results,
              BundleItem.Comparisons,
              BundleItem.Participants
            ),
            false,
            "native-golden-export",
            participantScale = Some(scale)
          )
          files = get(
            BundleFiles.assemble(request, summary, rows, pairRows, report = Some(report))
          )
          jobs <- backend.jobs
        yield
          assert(jobs.exists(_.run == run), jobs)
          assert(provenance.trail.exists(_.isInstanceOf[ProvenanceStep.Recomputed]))
          inspected match
            case Inspection.Contrast(_, m, b, d) =>
              focus.status match
                case QueryStatus.Contributing(ms, bs, ds) =>
                  assertEquals((m, b, d), (ms(scale.value), bs(scale.value), ds(scale.value)))
                case other => fail(s"focus did not contribute: $other")
            case other => fail(s"focus inspection was not native: $other")
          assertEquals(source.rows.head.ref, record)
          assert(source.rows.head.screen.nonEmpty)
          Readback(summary, rows, report, source, provenance, files)
      }

  private def storedSources(store: ProjectStore[IO]): Resource[IO, DatasetSources[IO]] =
    // SessionPort.close drains/cancels its queue; its documented contract
    // leaves ProjectSession open. These nested resources give each one
    // exactly one owner, with the port released before the project lock.
    for
      project <- Resource.make(ok(ProjectSession.open(store, owner)).map(_.session))(p =>
        ok(p.close)
      )
      port <- Resource.make(IO.blocking(SessionPort.start(project)))(p =>
        IO.blocking(p.close())
      )
    yield DatasetSourceHosts.stored(port)

  test(
    "native bytes and bound plan run, inspect, save/reopen and export identical scientific values"
  ) {
    TempDirs.resource("eyes4s-native-golden-workflow-").use { directory =>
      val sources = DatasetSourceHosts.golden(golden)
      val dataset = declared.dataset(StoryMoments.r3).getOrElse(fail("no native dataset"))
      for
        bytes  <- dataset.sources.entries.traverse(s => sources.bytes(dataset, s).map(s -> _))
        assets <- sources.assets(dataset).map(_.getOrElse(fail("no native registry")))
        text = bytes
          .map((s, b) =>
            s.role -> String(Array.from(b.getOrElse(fail(s"no ${s.path.value}"))), UTF_8)
          )
          .toMap
        admitted = get(
          RealAdmission
            .admit(dataset, text(SourceRole.Fixations), text(SourceRole.Trials), assets)
        )
        recipe = declared
          .analysis(StoryMoments.rev4)
          .getOrElse(fail("no saved analysis"))
          .recipe
        prepared = get(RealPrepared.of(StoryMoments.rev4, StoryMoments.r3, recipe, admitted))
        direct <- IO.blocking(get(prepared.work.run))
        planDigest   = get(prepared.plans.codec.digest(prepared.plan))
        resultDigest = get(prepared.results.codec.digest(direct))
        boundPlan    = CoreBinding.Bound(
          get(CanonicalDigest.parse[StudyPlanArtifact](planDigest.sha256.hex))
        )
        boundResult = CoreBinding.Bound(
          get(CanonicalDigest.parse[ResultArchiveArtifact](resultDigest.sha256.hex))
        )
        document = rebuild(
          declared,
          declared.datasets,
          declared.analyses
            .map(a => if a.id == StoryMoments.rev4 then a.copy(plan = boundPlan) else a),
          declared.runs
            .map(r => if r.id == StoryMoments.run7 then r.copy(archive = boundResult) else r),
          declared.figures,
          declared.reporting
        )
        first        <- readback(document, sources)
        store        <- FileProjectStore.at[IO](directory.resolve("native.eyes"))
        lock         <- ok(store.acquire(owner))
        inputEntries <- bytes.traverse { (source, held) =>
          ok(
            ProjectBundle.importInput(
              store,
              lock,
              InputKind.Source(source.role),
              source.path.value.split('/').last,
              held.getOrElse(fail("native source disappeared"))
            )
          )
        }
        imageEntries <- IO
          .blocking(TempDirs.files(golden.resolve("stimuli")))
          .flatMap(_.traverse { name =>
            IO.blocking(
              IArray.from(Files.readAllBytes(golden.resolve("stimuli").resolve(name)))
            ).flatMap(b =>
              ok(ProjectBundle.importInput(store, lock, InputKind.StimulusImage, name, b))
            )
          })
        encoded = get(
          ProjectBundle.encode(document, SharingOptions.complete, inputEntries ++ imageEntries)
        )
        _        <- ok(ProjectBundle.save(store, lock, None, encoded))
        _        <- ok(store.release(lock))
        reopened <- ok(ProjectBundle.open(store))
        verified <- ProjectBundle.checkInputs(store, reopened.manifest)
        second   <- storedSources(store).use(readback(reopened.document, _))
        exported <- IO.async_[Either[String, String]](done =>
          BundleWriter.write(
            directory.resolve("export"),
            second.files,
            None,
            answer => done(Right(answer))
          )
        )
      yield
        bytes.foreach((s, b) => assertEquals(ByteDigest.sha256(b.get), s.bytes))
        assertEquals(reopened.document, document)
        assert(reopened.science.verified)
        assert(verified.forall(_.isInstanceOf[InputStatus.Present]))
        assertEquals(second.summary, first.summary)
        assertEquals(second.rows, first.rows)
        assertEquals(second.report, first.report)
        assertEquals(second.source, first.source)
        assertEquals(second.provenance, first.provenance)
        assertEquals(
          second.files.map((name, data) => name -> ByteDigest.sha256(data)),
          first.files.map((name, data) => name -> ByteDigest.sha256(data))
        )
        assert(exported.isRight, exported)
        second.files.foreach { (name, expected) =>
          val actual = Files.readAllBytes(directory.resolve("export").resolve(name))
          assertEquals(ByteDigest.sha256(IArray.from(actual)), ByteDigest.sha256(expected))
        }
        val scores   = get(io.circe.parser.parse(GoldenScores.text)).hcursor
        val expected = get(scores.get[Vector[io.circe.Json]]("queries"))
          .map(q =>
            (
              get(q.hcursor.get[String]("participant")),
              get(q.hcursor.get[String]("trial")),
              get(q.hcursor.get[Int]("occurrence"))
            ) -> q
          )
          .toMap
        assertEquals(
          first.rows.size,
          get(scores.downField("counts").get[Int]("requestedQueries"))
        )
        assertEquals(
          first.summary.contrasts.queryNotAdmitted,
          get(scores.downField("counts").get[Int]("queriesNotAdmitted"))
        )
        assertEquals(
          first.summary.contrasts.failed,
          get(scores.downField("counts").get[Int]("failedQueries"))
        )
        assertEquals(
          first.summary.contrasts.noMatch,
          get(scores.downField("counts").get[Int]("noMatchQueries"))
        )
        assertEquals(
          first.summary.contrasts.contributing,
          get(scores.downField("counts").get[Int]("contributingQueries"))
        )
        assertEquals(
          first.rows.count(!_.status.isInstanceOf[QueryStatus.NotAdmitted]),
          expected.size
        )
        val inspection = get(
          ResultInspection.study(prepared.plan, direct, admitted.input, Some(admitted.evidence))
        )
        first.rows.foreach { row =>
          row.status match
            case QueryStatus.Contributing(ms, bs, ds) =>
              val stored =
                expected((row.query.participant, row.query.trial, row.query.occurrence)).hcursor
              val core = admitted.input.trials.rows
                .find(t =>
                  t.key.participant == row.query.participant && t.key.trial == row.query.trial && t.key.phase == row.query.phase.label && t.key.occurrence.value == row.query.occurrence
                )
                .getOrElse(fail("no direct query key"))
                .key
              Vector("0.5", "1", "2", "4").zipWithIndex.foreach { (sigma, index) =>
                val directRows = inspection.scales(index).contrast match
                  case eyes4s.plan.ScaleContrast.Rows(rows) => rows
                  case other => fail(s"direct contrast unavailable $other")
                val d = get(
                  directRows.get(ResultRef.ContrastRow(index, core)).toRight("no direct D")
                ).outcome.toOption.getOrElse(fail("no direct score")).value.value
                assertEquals(ds(index), d)
                val values = stored.downField("scales").downField(sigma)
                Vector("M" -> ms(index), "B" -> bs(index), "D" -> ds(index)).foreach {
                  (role, value) =>
                    val pinned = get(values.get[Double](role))
                    assert(
                      math.abs(value - pinned) <= ScoresTolerance,
                      s"${row.query.label} $sigma $role: $value vs $pinned"
                    )
                }
              }
            case status if status.isFailed =>
              assert(
                get(
                  expected(
                    (row.query.participant, row.query.trial, row.query.occurrence)
                  ).hcursor.get[String]("status")
                ).startsWith("failed:")
              )
            case QueryStatus.NoMatch(_) =>
              assertEquals(
                get(
                  expected(
                    (row.query.participant, row.query.trial, row.query.occurrence)
                  ).hcursor.get[String]("status")
                ),
                "no-match"
              )
            case _ => ()
        }
        // Compare the exported bytes with independent library evaluation,
        // including exact key sets, all three roles and explicit absences.
        import eyes4s.studio.core.importing.{CsvSniffer, Delimiter}
        val csv = second.files
          .collectFirst { case ("results.csv", data) => String(Array.from(data), UTF_8) }
          .getOrElse(fail("no results.csv exported"))
        val written = get(CsvSniffer.records("results.csv", csv, Delimiter.Comma))
        val column  = written.head.zipWithIndex.toMap
        val cells   = written.tail
        assertEquals(cells.size, first.rows.size * 4)
        val exportedKeys = cells.map { r =>
          def c(name: String) = r(column(name))
          (c("participant"), c("phase"), c("trial"), c("occurrence").toInt, c("scale").toInt)
        }
        val queryLedger = admitted.ledger.filter(_.trial.phase == recipe.phases.focal)
        val unmatched   = eyes4s.plan.UnmatchedReasons.of(
          prepared.plan.layout,
          prepared.plan.pairing,
          prepared.plan.referencePhase,
          prepared.counts.cardinality.unmatched,
          admitted.evidence.inventory.get
        )
        val requestedKeys = queryLedger.flatMap(row =>
          (0 until 4).map(index =>
            (
              row.trial.participant,
              row.trial.phase.label,
              row.trial.trial,
              row.trial.occurrence,
              index
            )
          )
        )
        assertEquals(exportedKeys.distinct.size, exportedKeys.size)
        assertEquals(exportedKeys.toSet, requestedKeys.toSet)
        var scored = 0
        cells.foreach { r =>
          def c(name: String) = r(column(name))
          val at              = s"${c("participant")} ${c("trial")} scale ${c("scale")}"
          val ledger          = queryLedger
            .find(e =>
              e.trial.participant == c("participant") &&
                e.trial.phase.label == c("phase") && e.trial.trial == c("trial") &&
                e.trial.occurrence == c("occurrence").toInt
            )
            .getOrElse(fail(s"$at: no admitted ledger entry"))
          assertEquals(c("item"), ledger.item, at)
          assertEquals(c("response"), ledger.response.get.label, at)
          assertEquals(c("sigma"), prepared.summary.scales(c("scale").toInt), at)
          val pinned = expected.get(
            (ledger.trial.participant, ledger.trial.trial, ledger.trial.occurrence)
          )
          val status = pinned
            .fold("not-admitted")(q => get(q.hcursor.get[String]("status")).takeWhile(_ != ':'))
          assertEquals(c("status"), status, at)
          val compared = status == "contributing" || status == "failed"
          assertEquals(c("matched_trial__valid"), compared.toString, at)
          assertEquals(c("controls__valid"), compared.toString, at)
          if compared then
            val q = pinned.get.hcursor
            assertEquals(c("matched_trial"), get(q.get[Vector[String]]("matched")).head, at)
            assertEquals(c("controls").toInt, get(q.get[Int]("controls")), at)
          else
            assertEquals(c("matched_trial"), "", at)
            assertEquals(c("controls"), "", at)
          assertEquals(c("reason__valid"), (status != "contributing").toString, at)
          assertEquals(c("reason").isEmpty, status == "contributing", at)
          val directReason = ledger.disposition match
            case TrialDisposition.Quarantined(cause) => Some(s"${cause.code}: ${cause.message}")
            case TrialDisposition.NoFixations        => Some("no-fixations")
            case TrialDisposition.Absent             => Some("absent")
            case TrialDisposition.Admitted if status == "contributing" => None
            case TrialDisposition.Admitted                             =>
              val core = admitted.input.trials.rows
                .find(t =>
                  t.key.participant == ledger.trial.participant &&
                    t.key.phase == ledger.trial.phase.label && t.key.trial == ledger.trial.trial &&
                    t.key.occurrence.value == ledger.trial.occurrence
                )
                .get
                .key
              val diagnostic = if status == "no-match" then
                eyes4s.plan.Diagnostic.of(
                  eyes4s.plan.StudyFinding
                    .UnmatchedFocal[eyes4s.plan.TrialKey, eyes4s.kernel.Unit2D.Px](
                      core,
                      unmatched.reason(core).get
                    )
                )
              else
                inspection.scales(c("scale").toInt).contrast match
                  case eyes4s.plan.ScaleContrast.Rows(rows) =>
                    rows
                      .get(ResultRef.ContrastRow(c("scale").toInt, core))
                      .get
                      .outcome
                      .swap
                      .toOption
                      .get
                  case other => fail(s"$at: no direct contrast failure: $other")
              Some(s"${diagnostic.code.render}: ${diagnostic.message}")
          assertEquals(c("reason"), directReason.getOrElse(""), at)
          if c("status") == "contributing" then
            scored += 1
            val core = admitted.input.trials.rows
              .find(t =>
                t.key.participant == c("participant") && t.key.phase == c("phase") &&
                  t.key.trial == c("trial") && t.key.occurrence.value == c("occurrence").toInt
              )
              .getOrElse(fail(s"$at: no direct query key"))
              .key
            val index      = c("scale").toInt
            val directRows = inspection.scales(index).contrast match
              case eyes4s.plan.ScaleContrast.Rows(rows) => rows
              case other => fail(s"direct contrast unavailable $other")
            val d = get(
              directRows.get(ResultRef.ContrastRow(index, core)).toRight(s"$at: no D")
            ).outcome.toOption
              .getOrElse(fail(s"$at: no direct score"))
              .value
              .value
            assertEquals((c("d__valid"), c("d").toDouble), ("true", d), at)
            Vector(
              "m" -> eyes4s.plan.StudyDesign.Matched,
              "b" -> eyes4s.plan.StudyDesign.Control
            ).foreach { (name, design) =>
              val reduction = get(
                inspection
                  .scales(index)
                  .reductions(design)
                  .get(ResultRef.Reduction(index, design, core))
                  .toRight(s"$at: no $name")
              )
              val value = get(reduction.outcome).value.value
              assertEquals((c(s"${name}__valid"), c(name).toDouble), ("true", value), at)
            }
            Vector("m", "b", "d").foreach { name =>
              assertEquals(
                (c(s"${name}_absence__valid"), c(s"${name}_absence")),
                ("false", ""),
                at
              )
            }
          else
            Vector("m", "b", "d").foreach { name =>
              assertEquals((c(s"${name}__valid"), c(name)), ("false", ""), at)
              assertEquals(
                (c(s"${name}_absence__valid"), c(s"${name}_absence")),
                ("true", c("status")),
                at
              )
            }
        }
        assertEquals(scored, first.summary.contrasts.contributing * 4)
        // comparisons.csv, as written: every scored pair is eyes4s's pair score.
        val keys = admitted.input.trials.rows
          .map(t =>
            (t.key.participant, t.key.phase, t.key.trial, t.key.occurrence.value) -> t.key
          )
          .toMap
        val pairCsv = second.files
          .collectFirst { case ("comparisons.csv", data) => String(Array.from(data), UTF_8) }
          .getOrElse(fail("no comparisons.csv exported"))
        val pairs         = get(CsvSniffer.records("comparisons.csv", pairCsv, Delimiter.Comma))
        val pcol          = pairs.head.zipWithIndex.toMap
        val directTallies = prepared.plan
          .windowTallies(admitted.input)
          .map((key, tally) => key -> get(tally))
          .toMap
        val writtenPairRefs = pairs.tail.map { r =>
          def c(name: String)                                       = r(pcol(name))
          def key(phase: String, trial: String, occurrence: String) =
            keys
              .get((c("participant"), c(phase), c(trial), c(occurrence).toInt))
              .getOrElse(fail(s"no direct key for ${r.mkString(",")}"))
          val index = c("scale").toInt
          assertEquals(c("sigma"), prepared.summary.scales(index))
          assertEquals(
            c("reference_item"),
            key("reference_phase", "reference_trial", "reference_occurrence").item
          )
          Vector(
            "query"     -> key("phase", "trial", "occurrence"),
            "reference" -> key("reference_phase", "reference_trial", "reference_occurrence")
          ).foreach { (role, key) =>
            val tally = directTallies.getOrElse(key, fail(s"no direct window tally for $key"))
            assertEquals(c(s"${role}_outside_window_count__valid"), "true")
            assertEquals(c(s"${role}_outside_window_count").toInt, tally.outsideWindow)
            assertEquals(c(s"${role}_outside_window_of__valid"), "true")
            assertEquals(c(s"${role}_outside_window_of").toInt, tally.total)
            assertEquals(
              c(s"${role}_outside_window_share__valid"),
              tally.outsideWindowShare.nonEmpty.toString
            )
            assertEquals(
              Option.when(c(s"${role}_outside_window_share").nonEmpty)(
                c(s"${role}_outside_window_share").toDouble
              ),
              tally.outsideWindowShare
            )
          }
          val design = c("design") match
            case "matched" => eyes4s.plan.StudyDesign.Matched
            case "control" => eyes4s.plan.StudyDesign.Control
            case other     => fail(s"unknown design $other")
          val ref = ResultRef.PairRow(
            index,
            design,
            key("phase", "trial", "occurrence"),
            key("reference_phase", "reference_trial", "reference_occurrence")
          )
          val outcome = get(
            inspection.scales(index).pairs(design).get(ref).toRight(s"no direct pair $ref")
          ).outcome
          outcome match
            case Right(score) =>
              assertEquals((c("reason__valid"), c("reason")), ("false", ""), ref.toString)
              assertEquals(
                (c("score__valid"), c("score").toDouble),
                ("true", score.value.value),
                ref.toString
              )
              assertEquals(
                (c("score_absence__valid"), c("score_absence")),
                ("false", ""),
                ref.toString
              )
            case Left(diagnostic) =>
              assertEquals((c("score__valid"), c("score")), ("false", ""), ref.toString)
              assertEquals(
                (c("score_absence__valid"), c("score_absence")),
                ("true", "failed"),
                ref.toString
              )
              assertEquals(c("reason__valid"), "true", ref.toString)
              assertEquals(
                c("reason"),
                s"${diagnostic.code.render}: ${diagnostic.message}",
                ref.toString
              )
          ref
        }
        val directPairRefs = inspection.scales.flatMap(s =>
          Vector(eyes4s.plan.StudyDesign.Matched, eyes4s.plan.StudyDesign.Control)
            .flatMap(d => s.pairs(d).references)
        )
        assert(directPairRefs.nonEmpty)
        assertEquals(writtenPairRefs.distinct.size, writtenPairRefs.size)
        assertEquals(writtenPairRefs.toSet, directPairRefs.toSet)

        val name   = get(eyes4s.results.CovariateName.of("response"))
        val levels = get(eyes4s.results.Levels.of(Vector("Forgotten", "Remembered")))
        val term   = eyes4s.results.LevelTerm.Categorical(name, levels)
        val schema = get(
          eyes4s.results.CovariateSchema.of(
            Vector(
              eyes4s.results.Covariate(name, eyes4s.results.CovariateType.Categorical(levels))
            )
          )
        )
        val reportSource = get(
          eyes4s.codec.ReportSources.study(prepared.plans, prepared.inputs, prepared.results)(
            prepared.plan,
            admitted.input,
            direct,
            Some(admitted.evidence),
            schema
          )
        )
        val reportSpec = get(
          eyes4s.results.ReportSpec.of(
            get(eyes4s.results.ReportId.of("independent-csv-oracle")),
            scale.value,
            get(
              eyes4s.results.ReportSelection
                .of(eyes4s.results.Role.values.toVector, Vector("value"))
            ),
            groupBy = Vector(eyes4s.results.Grouping.ByLevel(term)),
            reduce = eyes4s.results.ReducePolicy
              .ParticipantMeans(get(eyes4s.results.MinimumQueries.of(1))),
            contrast = Some(eyes4s.results.LevelContrast(term, "Remembered", "Forgotten"))
          )
        )
        val directReport   = get(eyes4s.results.Report.evaluate(reportSpec, reportSource))
        val participantCsv = second.files
          .collectFirst { case ("participants.csv", data) =>
            String(Array.from(data), UTF_8)
          }
          .getOrElse(fail("no participants.csv exported"))
        val participants =
          get(CsvSniffer.records("participants.csv", participantCsv, Delimiter.Comma))
        val participantColumns = participants.head.zipWithIndex.toMap
        val participantKeys    = participants.tail.map { row =>
          def c(name: String) = row(participantColumns(name))
          val participant     = c("participant")
          val group           = c("group")
          val ledger          = queryLedger.filter(_.trial.participant == participant)
          val scoredQueries   = expected.collect { case ((`participant`, _, _), query) =>
            query
          }.toVector
          val statuses = scoredQueries.map(q => get(q.hcursor.get[String]("status")))
          assertEquals(c("requested").toInt, ledger.size)
          assertEquals(c("contributing").toInt, statuses.count(_ == "contributing"))
          assertEquals(c("failed").toInt, statuses.count(_.startsWith("failed:")))
          assertEquals(c("no_match").toInt, statuses.count(_ == "no-match"))
          assertEquals(
            c("not_admitted").toInt,
            ledger.count(_.disposition != TrialDisposition.Admitted)
          )
          Vector(
            "m" -> eyes4s.results.Role.Matched,
            "b" -> eyes4s.results.Role.Control,
            "d" -> eyes4s.results.Role.Difference
          ).foreach { (name, role) =>
            val cell = directReport.cells
              .find(cell =>
                cell.role == role && cell.group.levels.contains(term.render -> group)
              )
              .getOrElse(fail(s"no direct $group $role cell"))
            val expected = cell.perParticipant
              .find(_.participant == participant)
              .getOrElse(fail(s"no direct $participant $group $role mean"))
            assertEquals(c(s"${name}__valid"), expected.value.isPresent.toString)
            assertEquals(
              Option.when(c(name).nonEmpty)(c(name).toDouble),
              expected.value.toOption
            )
            val absence = expected.value.absence.map {
              case eyes4s.results.Absence.EmptyGroup         => "empty-group"
              case eyes4s.results.Absence.BelowMinimum(_, _) => "below-minimum"
              case eyes4s.results.Absence.Unpaired           => "unpaired"
              case eyes4s.results.Absence.NotRecorded        => "not-recorded"
              case eyes4s.results.Absence.Unparsed           => "unparsed"
              case eyes4s.results.Absence.Failed(_, _)       => "failed"
              case eyes4s.results.Absence.Undefined(_)       => "undefined"
            }
            assertEquals(c(s"${name}_absence__valid"), absence.nonEmpty.toString)
            assertEquals(c(s"${name}_absence"), absence.getOrElse(""))
            if name == "d" then assertEquals(c("n").toInt, expected.queries)
          }
          participant -> group
        }
        val directParticipantKeys =
          directReport.cells.filter(_.role == eyes4s.results.Role.Difference).flatMap { cell =>
            val group = cell.group.levels.find(_._1 == term.render).get._2
            cell.perParticipant.map(p => p.participant -> group)
          }
        assertEquals(participantKeys.distinct.size, participantKeys.size)
        assertEquals(participantKeys.toSet, directParticipantKeys.toSet)
    }
  }
