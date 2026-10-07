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

package eyes4s.studio.desktop.figures

import cats.instances.future.*
import eyes4s.io.circe
import eyes4s.results.ResultCell
import eyes4s.studio.app.compare.SummaryAnswer
import eyes4s.studio.app.explore.DisplaySource
import eyes4s.studio.app.figures.*
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.{
  PairRowPage,
  QueryRow,
  QueryStatus,
  ReportRole,
  ReportView,
  ResultSummary,
  TrialTally
}
import eyes4s.studio.core.document.{PanelLetter, StudioDocument}
import eyes4s.studio.core.figures.{
  BundleTableError,
  BundleTables,
  MethodsReads,
  ReferenceReads,
  ReferenceScores
}
import eyes4s.studio.core.fixture.{GoldenAssets, MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.ScaleIndex

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.concurrent.{ExecutionContext, Future}

/** The export bundle (ticket S9.5; Figures.dc.html, Bundle) of story moment
  * t2's Figure 1 on the fake backend: the chosen files, the result tables
  * as eyes4s CSV, their values equal to what the page shows and to the
  * fixture, and a missing value never written as a zero.
  */
class ExportBundleSuite extends munit.FunSuite:
  import StoryMoments.{r3, run7}

  private given ExecutionContext = ExecutionContext.global

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private def t2: AppModel = StoryModels.t2Figures
  private val scale2       = ok(ScaleIndex.of(2))
  private val p17ret07     = MockStudy.key("P17", "ret_07")

  private final case class Served(
      summary: ResultSummary,
      rows: Vector[QueryRow],
      scores: ReferenceScores,
      composer: FigureComposer,
      pairs: Vector[PairRowPage],
      report: ReportView
  )

  /** Figure 1 with every read answered, and the run's query rows. */
  private def served: Future[Served] =
    for
      session <- HeadlessSession.open(StoryMoment.T2)
      summary <- session.result(run7)
      scores  <- ReferenceReads.read[Future](
        session.inspect(run7, _),
        session.navigator.pairs,
        run7,
        scale2,
        p17ret07
      )
      facts   <- MethodsReads.read[Future](session.admission, session.queries, run7, r3)
      rows    <- MethodsReads.queryRows[Future](session.queries, run7)
      pairs   <- MethodsReads.pairRows[Future](session.pairRows, run7, ok(summary).scales.size)
      reports <- Future.traverse(FigureComposer.sync(FigureComposer.empty, t2)._2.collect {
        case ComposerEffect.RequestReport(run, spec, scale) => (run, spec, scale)
      }) { (run, spec, scale) =>
        session
          .report(run, spec, scale.value)
          .map(r => ComposerIntent.ReportRead(run, spec, scale, r.left.map(_.message)))
      }
      _ <- session.close
    yield
      val registry = t2.document.dataset(r3).map(GoldenAssets.registry).getOrElse(Left("no r3"))
      val c        = (reports ++ Vector(
        ComposerIntent.SummaryRead(run7, SummaryAnswer.Answered(ok(summary))),
        ComposerIntent.ReferencesRead(run7, scale2, p17ret07, Right(ok(scores))),
        ComposerIntent.DisplaysRead(r3, registry.map(DisplaySource.Served(_))),
        ComposerIntent.Methods(MethodsIntent.FactsRead(run7, Right(ok(facts))))
      )).foldLeft(FigureComposer.sync(FigureComposer.empty, t2)._1)((c, i) =>
        FigureComposer.update(c, t2, i)._1
      )
      val source = request(c).source
      val report = c
        .reports(
          (
            run7,
            source.reporting,
            request(c).participantScale.getOrElse(fail("no reporting scale"))
          )
        )
        .fold(fail(_), identity)
      Served(ok(summary), ok(rows), ok(scores), c, ok(pairs), report)

  private def request(c: FigureComposer, model: AppModel = t2): BundleRequest =
    FigureComposer.update(c, model, ComposerIntent.ExportBundle)._2 match
      case Vector(ComposerEffect.WriteBundle(r)) => r
      case other                                 => fail(s"expected one bundle, got $other")

  private def files(
      s: Served,
      c: Option[FigureComposer] = None,
      model: AppModel = t2
  ): Map[String, String] =
    val r = request(c.getOrElse(s.composer), model)
    ok(BundleFiles.assemble(r, s.summary, s.rows, s.pairs, report = Some(s.report)))
      .map((n, b) => n -> String(Array.from(b), UTF_8))
      .toMap

  /** RFC 4180: a header, then records, as maps by column. */
  private def csv(text: String): Vector[Map[String, String]] =
    val records = Vector.newBuilder[Vector[String]]
    var field   = StringBuilder()
    var record  = Vector.newBuilder[String]
    var quoted  = false
    var i       = 0
    while i < text.length do
      val ch = text(i)
      if quoted then
        if ch == '"' && i + 1 < text.length && text(i + 1) == '"' then
          field += '"'; i += 1
        else if ch == '"' then quoted = false
        else field += ch
      else if ch == '"' then quoted = true
      else if ch == ',' then
        record += field.result(); field = StringBuilder()
      else if ch == '\r' then ()
      else if ch == '\n' then
        record += field.result(); field = StringBuilder()
        records += record.result(); record = Vector.newBuilder[String]
      else field += ch
      i += 1
    val all    = records.result()
    val header = all.head
    all.tail.map(r => header.zip(r).toMap)

  private def number(row: Map[String, String], column: String): Option[Double] =
    Option.when(row(s"${column}__valid") == "true")(row(column).toDouble)

  // --- The bundle's files ---------------------------------------------------------

  test("the bundle holds the chosen files the board lists") {
    served.map { s =>
      val vm = FigureComposer.view(s.composer, t2).bundle.getOrElse(fail("no bundle"))
      assertEquals(
        vm.rows.map(r => (r.file, r.detail, r.chosen, r.unavailable.isDefined)),
        Vector(
          ("figure-1.svg", "", true, false),
          ("results.csv", "480 queries × 4 σ", true, false),
          ("comparisons.csv", "35,876 pair rows", true, false),
          ("participants.csv", "24 × 2 groups at 2°", true, false),
          ("methods.md", "", true, false),
          ("project snapshot", "includes images", false, false)
        )
      )
      val r = request(s.composer)
      assertEquals(r.folder, "figure-1-bundle")
      assertEquals(r.participantScale, Some(scale2))
      assertEquals(
        r.items,
        Vector(
          BundleItem.Figure,
          BundleItem.Results,
          BundleItem.Comparisons,
          BundleItem.Participants,
          BundleItem.Methods
        )
      )
      assertEquals(
        files(s).keySet,
        Set(
          "figure-1.svg",
          "results.csv",
          "comparisons.csv",
          "participants.csv",
          "methods.md",
          "README.txt"
        )
      )
    }
  }

  test("an item turned off is not written; the snapshot is asked for when chosen") {
    served.map { s =>
      val c = Vector(
        ComposerIntent.ToggleBundle(BundleItem.Results),
        ComposerIntent.ToggleBundle(BundleItem.Snapshot),
        ComposerIntent.IncludeImages(false)
      ).foldLeft(s.composer)((c, i) => FigureComposer.update(c, t2, i)._1)
      val r = request(c)
      assert(!r.items.contains(BundleItem.Results), r.items)
      assert(r.items.contains(BundleItem.Snapshot), r.items)
      assert(!r.includeImages)
      assertEquals(
        files(s, Some(c)).keySet,
        Set("figure-1.svg", "comparisons.csv", "participants.csv", "methods.md", "README.txt")
      )
      val done = FigureComposer.update(c, t2, ComposerIntent.BundleExported(Right("/tmp/b")))._1
      assertEquals(
        FigureComposer.view(done, t2).bundle.flatMap(_.status),
        Some("Bundle exported to /tmp/b.")
      )
    }
  }

  test("README.txt lists the files and the binding; nothing chosen is left out") {
    served.map { s =>
      val readme = files(s)("README.txt")
      assert(
        readme.startsWith(
          """Figure 1 export bundle
            |run 7 · analysis rev 4 · data r3 · reporting “By retrieval response” · studio build eyes4s 0.1
            |reporting spec by-retrieval-response sha256:3a0ed363d486975f28f3eb990c0deb87c415924ebe4a301e638dae73221de5dd
            |participants.csv and reporting counts: σ 2° (scale index 2); the first participant panel's scale, or the first declared scale for a figure without one.
            |
            |Files:
            |- figure-1.svg
            |- results.csv
            |- comparisons.csv
            |- participants.csv
            |- methods.md
            |
            |Tables:
          |""".stripMargin
        ),
        readme
      )
      assert(!readme.contains("Not included"), readme)
    }
  }

  test("README.txt documents every column of every table written, from its schema") {
    served.map { s =>
      val all    = files(s)
      val readme = all("README.txt")
      assert(readme.contains("<column>__valid: true when the cell holds a value"), readme)
      for name <- Vector("results.csv", "comparisons.csv", "participants.csv") do
        val header = all(name).linesIterator.next().split(",").toVector
        // The documented columns, in order, are exactly the CSV's header.
        val section = readme
          .split("\n\n")
          .find(_.startsWith(s"$name ("))
          .getOrElse(fail(s"no section for $name in\n$readme"))
        val documented =
          section.linesIterator.drop(1).map(_.drop(2).takeWhile(_ != ' ')).toVector
        assertEquals(documented, header, name)
      // A label column names its labels; units and meanings come from the schema.
      assert(
        readme.contains(
          "- m_absence (text, label, may be missing; one of failed, no-match, not-admitted, " +
            "not-scored, empty-group): why m is missing"
        ),
        readme
      )
      assert(
        readme.contains(
          "- score_absence (text, label, may be missing; one of failed, not-served): " +
            "why score is missing"
        ),
        readme
      )
      assert(readme.contains("- d__valid (true or false): whether d holds a value"), readme)
      assert(readme.contains("- participant (text): participant id"), readme)
    }
  }

  /** results.csv's table digest for the fixture, recorded when its label
    * domain was settled (S9.5): pair absences must not change it.
    */
  private val ResultsDigest = "c032718a63e58d745850c70b35f038ab9ecfd9109ce4252c20e3d0834252149f"

  test(
    "results.csv pins explicit missing references; comparisons.csv has its own absence labels"
  ) {
    served.map { s =>
      val r       = request(s.composer)
      val results =
        BundleTables.results(r.source, s.summary, s.rows).fold(e => fail(e.message), identity)
      assertEquals(results.identity.hex, ResultsDigest)
      val pairs = BundleTables
        .comparisons(r.source, s.summary, s.pairs)
        .fold(e => fail(e.message), identity)
      assertEquals(
        pairs.columns.find(_.name == "score_absence").map(_.labels),
        Some(BundleTables.PairAbsences)
      )
      assertEquals(
        results.columns.find(_.name == "d_absence").map(_.labels),
        Some(BundleTables.Absences)
      )
      assert(!BundleTables.Absences.contains("not-served"))
    }
  }

  test("comparisons rows of a scale the run does not compute are refused, not labelled") {
    served.map { s =>
      val r     = request(s.composer)
      val wrong = s.pairs.take(1).map(_.copy(scale = s.summary.scales.size))
      assertEquals(
        BundleTables.comparisons(r.source, s.summary, wrong).left.map(_.message),
        Left(
          s"The comparisons table has rows of scale ${s.summary.scales.size}; the run computes " +
            s.summary.scaleLabels.mkString(", ") + "."
        )
      )
    }
  }

  // --- comparisons.csv ---------------------------------------------------------------

  test("comparisons.csv: every pair of the run at every scale, as the backend serves them") {
    served.map { s =>
      val rows = csv(files(s)("comparisons.csv"))
      assertEquals(rows.size.toLong, s.summary.pairRows)
      assertEquals(rows.size, 35876)
      assert(rows.forall(_("table_sha256").length == 64))
      assertEquals(
        rows.groupMapReduce(_("scale"))(_ => 1)(_ + _).values.toSet,
        Set(s.summary.pairRowsPerScale.toInt)
      )
      // The panel C query at 2°: its matched pair and every control pair the
      // page shows, with the same scores.
      val p17 = rows.filter(r =>
        r("participant") == "P17" && r("trial") == "ret_07" && r("scale") == "2"
      )
      val matched = p17.find(_("design") == "matched").get
      assertEquals(
        (matched("reference_trial"), number(matched, "score")),
        (s.scores.matched.reference.trial, Some(s.scores.matched.score))
      )
      s.scores.controls.foreach { c =>
        val row = p17
          .find(r => r("design") == "control" && r("reference_trial") == c.reference.trial)
          .getOrElse(fail(s"no control ${c.reference.label}"))
        assertEquals((row("reference_item"), number(row, "score")), (c.item, Some(c.score)))
      }
      assertEquals(p17.count(_("design") == "control"), s.scores.controlMembers)
    }
  }

  test("comparisons exports each served window tally and documents its duration share") {
    served.map { s =>
      val rows    = csv(files(s)("comparisons.csv"))
      val entries = s.pairs.flatMap(_.rows)
      assertEquals(rows.size, entries.size)
      rows.zip(entries).foreach { (row, entry) =>
        Vector("query" -> entry.queryWindow, "reference" -> entry.referenceWindow).foreach {
          (role, window) =>
            assertEquals(
              number(row, s"${role}_outside_window_count"),
              window.map(_.tally.outsideWindow.toDouble)
            )
            assertEquals(
              number(row, s"${role}_outside_window_of"),
              window.map(_.tally.total.toDouble)
            )
            assertEquals(
              number(row, s"${role}_outside_window_share"),
              window.flatMap(_.tally.outsideWindowShare)
            )
        }
      }
      val readme = files(s)("README.txt")
      assert(
        readme.contains("query outside-window fixation duration / total fixation duration")
      )
      assert(readme.contains("reference_outside_window_count__valid"))
    }
  }

  test("window exports distinguish count fractions, duration shares, zero and unavailable") {
    served.map { s =>
      import eyes4s.kernel.Span
      import eyes4s.plan.WindowTally
      val page                                                         = s.pairs.head
      val entry                                                        = page.rows.head
      def tally(n: Int, total: Int, duration: Long, allDuration: Long) =
        ok(
          WindowTally.of(
            0,
            n,
            total,
            Span.zero,
            Span.micros(duration),
            Span.micros(allDuration)
          )
        )
      val query = TrialTally(
        eyes4s.studio.core.selection.StudioRef.Trial(entry.query),
        tally(1, 4, 60, 100)
      )
      val zero = TrialTally(
        eyes4s.studio.core.selection.StudioRef.Trial(entry.reference),
        tally(0, 4, 0, 100)
      )
      val empty = zero.copy(tally = tally(0, 0, 0, 0))
      val rows  = Vector(
        entry.copy(queryWindow = Some(query), referenceWindow = Some(zero)),
        entry.copy(queryWindow = None, referenceWindow = Some(empty))
      )
      val table = ok(
        BundleTables.comparisons(
          request(s.composer).source,
          s.summary,
          Vector(page.copy(rows = rows))
        )
      )
      def cells(index: Int) = table.columns.map(_.name).zip(table.rows(index)).toMap
      val first             = cells(0)
      assertEquals(first("query_outside_window_count"), ResultCell.Integer(1))
      assertEquals(first("query_outside_window_of"), ResultCell.Integer(4))
      assertEquals(first("query_outside_window_share"), ResultCell.Number(0.6))
      assertEquals(first("reference_outside_window_share"), ResultCell.Number(0.0))
      val second = cells(1)
      assertEquals(second("query_outside_window_count"), ResultCell.Missing)
      assertEquals(second("query_outside_window_of"), ResultCell.Missing)
      assertEquals(second("query_outside_window_share"), ResultCell.Missing)
      assertEquals(second("reference_outside_window_count"), ResultCell.Integer(0))
      assertEquals(second("reference_outside_window_of"), ResultCell.Integer(0))
      assertEquals(second("reference_outside_window_share"), ResultCell.Missing)
    }
  }

  test("a pair without a score is missing with its absence and reason; never a zero") {
    served.map { s =>
      val rows = csv(files(s)("comparisons.csv"))
      rows.foreach { r =>
        val valid = r("score__valid") == "true"
        assertEquals(r("score").isEmpty, !valid, r)
        assertEquals(r("score_absence").isEmpty, valid, r)
      }
      val failed = rows.filter(_("score_absence") == "failed")
      assert(failed.nonEmpty)
      assert(failed.forall(_("reason").startsWith("study-failure.")), failed.head)
      assert(rows.exists(_("score_absence") == "not-served"))
    }
  }

  // --- Writing: whole or not at all --------------------------------------------------

  private def tempDir(): Path = Files.createTempDirectory("eyes4s-bundle-")

  private def written(
      target: Path,
      snapshot: Option[BundleWriter.Snapshot]
  ): Either[String, String] =
    var answer: Option[Either[String, String]] = None
    BundleWriter.write(
      target,
      Vector("a.txt" -> IArray.from("a".getBytes(UTF_8))),
      snapshot,
      a => answer = Some(a)
    )
    answer.getOrElse(fail("no answer"))

  test("a bundle appears whole, with its snapshot; nothing partial is left behind") {
    val parent = tempDir()
    val target = parent.resolve("figure-1-bundle")
    val result = written(
      target,
      Some((to, answer) =>
        Files.createDirectories(to)
        Files.writeString(to.resolve("project.json"), "{}")
        answer(Right(()))
      )
    )
    assertEquals(result, Right(target.toString))
    assert(Files.exists(target.resolve("a.txt")))
    assert(Files.exists(target.resolve("project/project.json")))
    assertEquals(Files.list(parent).count(), 1L)
  }

  test("a failed snapshot leaves no bundle and no partial folder") {
    val parent = tempDir()
    val target = parent.resolve("figure-1-bundle")
    val result = written(target, Some((_, answer) => answer(Left("the disk is full"))))
    assertEquals(result, Left("project snapshot: the disk is full"))
    assert(!Files.exists(target))
    assertEquals(Files.list(parent).count(), 0L)
    // An existing folder is never written into.
    Files.createDirectories(target)
    assert(written(target, None).left.exists(_.contains("already exists")))
  }

  // --- results.csv ----------------------------------------------------------------

  test("results.csv: every query at every scale, keyed, with status and reason") {
    served.map { s =>
      val rows = csv(files(s)("results.csv"))
      assertEquals(rows.size, 480 * 4)
      assert(rows.forall(_("table_sha256").length == 64))
      val byStatus = rows.groupMapReduce(_("status"))(_ => 1)(_ + _).view.mapValues(_ / 4).toMap
      val c        = s.summary.contrasts
      assertEquals(
        byStatus,
        Map(
          "contributing" -> c.contributing,
          "failed"       -> c.failed,
          "no-match"     -> c.noMatch,
          "not-admitted" -> c.queryNotAdmitted
        )
      )
      assert(rows.filter(_("status") != "contributing").forall(_("reason__valid") == "true"))
      assert(rows.filter(_("status") == "contributing").forall(_("reason__valid") == "false"))
    }
  }

  test("results.csv retains each scale failure and refuses an incomplete vector") {
    served.map { s =>
      val failed      = s.rows.find(_.status.isFailed).getOrElse(fail("no failed query"))
      val first       = failed.status.diagnosticAt(0).getOrElse(fail("no failure diagnostic"))
      val diagnostics = s.summary.scales.indices
        .map(scale =>
          first.copy(code = s"failure.scale-$scale", message = s"failure at scale $scale")
        )
        .toVector
      val row    = failed.copy(status = QueryStatus.FailedAtScales(diagnostics))
      val source = request(s.composer).source
      val table  = ok(BundleTables.results(source, s.summary, Vector(row)))
      val reason = table.columns.indexWhere(_.name == "reason")
      assertEquals(
        table.rows.map(_(reason)),
        diagnostics.map(d => ResultCell.Text(s"${d.code}: ${d.message}"))
      )
      assertEquals(
        BundleTables.results(
          source,
          s.summary,
          Vector(row.copy(status = QueryStatus.FailedAtScales(diagnostics.dropRight(1))))
        ),
        Left(
          BundleTableError.FailureScales(
            run7,
            row.query,
            diagnostics.size,
            diagnostics.size - 1
          )
        )
      )
    }
  }

  test("results.csv equals what the page shows: panel C's query at 2°") {
    served.map { s =>
      val row = csv(files(s)("results.csv"))
        .find(r => r("participant") == "P17" && r("trial") == "ret_07" && r("scale") == "2")
        .getOrElse(fail("no P17 ret_07 at scale 2"))
      assertEquals(row("sigma"), "2°")
      assertEquals(number(row, "m"), Some(s.scores.m))
      assertEquals(number(row, "b"), Some(s.scores.b))
      assertEquals(number(row, "d"), Some(s.scores.d))
      // The tile and the caption show the same numbers, formatted.
      val page = FigureComposer.view(s.composer, t2).page.getOrElse(fail("no page"))
      page.panels.find(_.letter == ok(PanelLetter.of("C"))).map(_.body) match
        case Some(PanelBody.Maps(vm)) =>
          assert(vm.caption.contains(s"B ${f"${s.scores.b}%.2f"}"), vm.caption)
        case other => fail(other.toString)
      assertEquals(row("controls"), "19")
    }
  }

  test("a missing score is an empty cell marked invalid, with its absence; never a zero") {
    served.map { s =>
      val rows = csv(files(s)("results.csv"))
      for column <- Vector("m", "b", "d") do
        rows.foreach { r =>
          val valid = r(s"${column}__valid") == "true"
          assertEquals(r(column).isEmpty, !valid, r)
          assertEquals(r(s"${column}_absence").isEmpty, valid, r)
        }
      val unadmitted = rows.filter(_("status") == "not-admitted")
      assert(unadmitted.nonEmpty)
      unadmitted.foreach { r =>
        assertEquals(r("d"), "")
        assertEquals(r("d_absence"), "not-admitted")
        assertEquals(r("controls__valid"), "false")
      }
      // The fixture's no-match rows are the run's.
      val fromRows = s.rows.count(_.status.isInstanceOf[QueryStatus.NoMatch])
      assertEquals(rows.count(_("status") == "no-match"), fromRows * 4)
      rows.filter(_("status") == "no-match").foreach { row =>
        assertEquals(row("matched_trial__valid"), "false")
        assertEquals(row("matched_trial"), "")
      }
    }
  }

  // --- participants.csv ------------------------------------------------------------

  test("participants.csv uses the explicitly served report scale") {
    served.map { s =>
      val rows = csv(files(s)("participants.csv"))
      assertEquals(rows.size, 24 * 2)
      rows.foreach { r =>
        val expected = s.report.participant(
          Some(eyes4s.studio.core.backend.Response(r("group"))),
          ReportRole.Difference,
          r("participant")
        )
        assertEquals(number(r, "d"), expected.flatMap(_.value), r)
      }
      val p17 = rows.find(r => r("participant") == "P17" && r("group") == "Forgotten").get
      assertEquals(p17("n"), "2")
      assertEquals(p17("requested"), "20")
      val table = ok(BundleTables.participants(request(s.composer).source, s.summary, s.report))
      assert(table.context.circe.hcursor.get[String]("means_scale").contains("2°"))
      // Equal values at another scale still carry that scale's declared identity.
      val sameValues = ok(
        BundleTables.participants(
          request(s.composer).source,
          s.summary,
          s.report.copy(scale = 1)
        )
      )
      assert(sameValues.context.circe.hcursor.get[String]("means_scale").contains("1°"))
    }
  }

  test("a group without queries is a row with n 0 and missing means, never zero") {
    served.map { s =>
      val source = request(s.composer).source
      val empty  = s.report.copy(participants =
        s.report.participants.filterNot(p =>
          p.participant == "P17" && p.group.exists(_.label == "Forgotten")
        )
      )
      val table = ok(BundleTables.participants(source, s.summary, empty))
      val rows  = csv(eyes4s.io.csv(table).encode)
      val p17   = rows.find(r => r("participant") == "P17" && r("group") == "Forgotten").get
      assertEquals(p17("n"), "0")
      assertEquals((p17("d"), p17("d__valid"), p17("d_absence")), ("", "false", "empty-group"))
    }
  }

  // --- methods.md ------------------------------------------------------------------

  test("methods.md is the text as shown, the author's edits included") {
    served.map { s =>
      val shown  = FigureComposer.view(s.composer, t2).methods.flatMap(_.text.toOption).get
      val edited = shown.replace(
        "D measures spatial correspondence, not sequential replay.",
        "D measures where gaze went, not when."
      )
      val (c, effects) = FigureComposer.update(
        s.composer,
        t2,
        ComposerIntent.Methods(MethodsIntent.Edit(edited))
      )
      val model  = AppModel.run(t2, effects.collect { case ComposerEffect.App(i) => i })._1
      val figure = request(s.composer).source.figure.id
      val draft  = model.document.figures
        .find(_.id == figure)
        .flatMap(_.methods)
        .getOrElse(fail("no draft"))
      assertEquals(draft.base, shown)
      assertEquals(draft.edited, edited)
      assertEquals(files(s)("methods.md"), shown + "\n")
      assertEquals(files(s, Some(c), model)("methods.md"), edited + "\n")

      val codec    = ok(StudioDocument.codec)
      val document = ok(codec.decode(ok(codec.encode(model.document))))
      val reopened = AppModel
        .update(AppModel.open(document, model.project), Intent.Navigate(model.location))
        ._1
      assertEquals(
        FigureComposer.view(FigureComposer.empty, reopened).methods.flatMap(_.text.toOption),
        Some(edited)
      )
      // Reuse only immutable run reads, with the author's state supplied by the reopened document.
      assertEquals(files(s, model = reopened)("methods.md"), edited + "\n")
    }
  }
