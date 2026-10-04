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
import eyes4s.studio.app.compare.SummaryAnswer
import eyes4s.studio.app.explore.DisplaySource
import eyes4s.studio.app.figures.*
import eyes4s.studio.app.plot.PlotValue
import eyes4s.studio.app.{AppModel, StoryModels}
import eyes4s.studio.core.backend.{QueryRow, QueryStatus, ResultSummary}
import eyes4s.studio.core.document.PanelLetter
import eyes4s.studio.core.figures.{BundleTables, MethodsReads, ReferenceReads, ReferenceScores}
import eyes4s.studio.core.fixture.{GoldenAssets, MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

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
      composer: FigureComposer
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
      facts <- MethodsReads.read[Future](session.admission, session.queries, run7, r3)
      rows  <- MethodsReads.queryRows[Future](session.queries, run7)
      _     <- session.close
    yield
      val registry = t2.document.dataset(r3).map(GoldenAssets.registry).getOrElse(Left("no r3"))
      val c        = Vector(
        ComposerIntent.SummaryRead(run7, SummaryAnswer.Answered(ok(summary))),
        ComposerIntent.ReferencesRead(run7, scale2, p17ret07, Right(ok(scores))),
        ComposerIntent.DisplaysRead(r3, registry.map(DisplaySource.Served(_))),
        ComposerIntent.Methods(MethodsIntent.FactsRead(run7, Right(ok(facts))))
      ).foldLeft(FigureComposer.sync(FigureComposer.empty, t2)._1)((c, i) =>
        FigureComposer.update(c, t2, i)._1
      )
      Served(ok(summary), ok(rows), ok(scores), c)

  private def request(c: FigureComposer): BundleRequest =
    FigureComposer.update(c, t2, ComposerIntent.ExportBundle)._2 match
      case Vector(ComposerEffect.WriteBundle(r)) => r
      case other                                 => fail(s"expected one bundle, got $other")

  private def files(s: Served, c: Option[FigureComposer] = None): Map[String, String] =
    val r = request(c.getOrElse(s.composer))
    ok(BundleFiles.assemble(r, s.summary, s.rows))
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

  test("the bundle holds the chosen files the board lists; comparisons.csv says why not") {
    served.map { s =>
      val vm = FigureComposer.view(s.composer, t2).bundle.getOrElse(fail("no bundle"))
      assertEquals(
        vm.rows.map(r => (r.file, r.detail, r.chosen, r.unavailable.isDefined)),
        Vector(
          ("figure-1.svg", "", true, false),
          ("results.csv", "480 queries × 4 σ", true, false),
          ("comparisons.csv", "35,876 pair rows", true, true),
          ("participants.csv", "24 × 2 groups", true, false),
          ("methods.md", "", true, false),
          ("project snapshot", "includes images", false, false)
        )
      )
      val r = request(s.composer)
      assertEquals(r.folder, "figure-1-bundle")
      assertEquals(
        r.items,
        Vector(
          BundleItem.Figure,
          BundleItem.Results,
          BundleItem.Participants,
          BundleItem.Methods
        )
      )
      assertEquals(
        files(s).keySet,
        Set("figure-1.svg", "results.csv", "participants.csv", "methods.md", "README.txt")
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
        Set("figure-1.svg", "participants.csv", "methods.md", "README.txt")
      )
      val done = FigureComposer.update(c, t2, ComposerIntent.BundleExported(Right("/tmp/b")))._1
      assertEquals(
        FigureComposer.view(done, t2).bundle.flatMap(_.status),
        Some("Bundle exported to /tmp/b.")
      )
    }
  }

  test(
    "README.txt lists the files, the binding, and comparisons.csv as not included, with why"
  ) {
    served.map { s =>
      assertNoDiff(
        files(s)("README.txt"),
        """Figure 1 export bundle
          |run 7 · analysis rev 4 · data r3 · reporting “By retrieval response” · studio build eyes4s 0.1
          |
          |Files:
          |- figure-1.svg
          |- results.csv
          |- participants.csv
          |- methods.md
          |
          |Not included:
          |- comparisons.csv: comparisons.csv needs the backend's pair-rows view, which it does not serve yet.
          |""".stripMargin
      )
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
    }
  }

  // --- participants.csv ------------------------------------------------------------

  test("participants.csv equals panel D's participant means, and the fixture's n") {
    served.map { s =>
      val rows = csv(files(s)("participants.csv"))
      assertEquals(rows.size, 24 * 2)
      val page = FigureComposer.view(s.composer, t2).page.getOrElse(fail("no page"))
      val d    = page.panels.find(_.letter == ok(PanelLetter.of("D"))).map(_.body) match
        case Some(PanelBody.Plot(vm)) => vm.source
        case other                    => fail(other.toString)
      val shown = d.rows.collect {
        case r @ PlotRowOf(StudioRef.ParticipantSummary(_, _, _, Some(group), participant)) =>
          (participant, group.label) -> r.values(2)
      }.toMap
      assertEquals(shown.size, 48)
      rows.foreach { r =>
        val onScreen = shown((r("participant"), r("group"))) match
          case PlotValue.Number(v) => Some(v)
          case _                   => None
        assertEquals(number(r, "d"), onScreen, r)
      }
      val p17 = rows.find(r => r("participant") == "P17" && r("group") == "Forgotten").get
      assertEquals(p17("n"), "2")
      assertEquals(p17("requested"), "20")
      assertEquals(BundleTables.meansScale(s.summary), "2°")
    }
  }

  test("a group without queries is a row with n 0 and missing means, never zero") {
    served.map { s =>
      val source = request(s.composer).source
      val empty  = s.summary.copy(participants =
        s.summary.participants.map(p =>
          if p.participant != "P17" then p
          else
            p.copy(groups =
              p.groups.map(g => if g.label.label == "Forgotten" then g.copy(n = 0) else g)
            )
        )
      )
      val table = ok(BundleTables.participants(source, empty))
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
      val c = FigureComposer
        .update(
          s.composer,
          t2,
          ComposerIntent.Methods(MethodsIntent.Edit(edited))
        )
        ._1
      assertEquals(files(s)("methods.md"), shown + "\n")
      assertEquals(files(s, Some(c))("methods.md"), edited + "\n")
    }
  }

  private object PlotRowOf:
    def unapply(r: eyes4s.studio.app.plot.PlotRow): Some[StudioRef] = Some(r.ref)
