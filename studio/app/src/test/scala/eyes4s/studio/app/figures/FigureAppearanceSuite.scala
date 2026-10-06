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

package eyes4s.studio.app.figures

import eyes4s.studio.app.compare.SummaryAnswer
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.plot.ParticipantLines
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.{ReportRole, ReportView, ResultSummary}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.engine.StudioBuild
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.figures.FigureSource
import eyes4s.studio.core.selection.ScaleIndex

import scala.concurrent.{ExecutionContext, Future}

/** The figure's appearance, captions and provenance stamp, headless (ticket
  * S9.2b; Figures.dc.html, inspector and page), at story moment t2.
  */
class FigureAppearanceSuite extends munit.FunSuite:
  import StoryMoments.run7

  private given ExecutionContext = ExecutionContext.global

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val figure2           = ok(FigureId.of(2))
  private def letter(l: String) = ok(PanelLetter.of(l))
  private def t2: AppModel      = StoryModels.t2Figures

  private def at(model: AppModel, figure: FigureId): AppModel =
    AppModel
      .update(
        model,
        Intent.Navigate(
          Location(Perspective.Figures, Vector(Place.Figures, Place.Figure(figure)))
        )
      )
      ._1

  private def page(c: FigureComposer, m: AppModel): PageVM =
    FigureComposer.view(c, m).page.getOrElse(fail("no page"))

  private def summary: Future[(ResultSummary, ReportView)] =
    for
      session <- HeadlessSession.open(StoryMoment.T2)
      s       <- session.result(run7)
      report  <- session.report(
        run7,
        ok(FigureSource.of(t2.document, ok(FigureId.of(1)))).reporting,
        2
      )
      _ <- session.close
    yield (ok(s), ok(report))

  private def withSummary(m: AppModel, s: ResultSummary, report: ReportView): FigureComposer =
    val synced = FigureComposer.sync(FigureComposer.empty, m)._1
    val result = FigureComposer
      .update(synced, m, ComposerIntent.SummaryRead(run7, SummaryAnswer.Answered(s)))
      ._1
    val spec = ok(FigureSource.of(m.document, ok(FigureId.of(1)))).reporting
    FigureComposer
      .update(
        result,
        m,
        ComposerIntent.ReportRead(run7, spec, ok(ScaleIndex.of(report.scale)), Right(report))
      )
      ._1

  private def panelD(c: FigureComposer, m: AppModel): PlotPanelVM =
    page(c, m).panels.find(_.letter == letter("D")).map(_.body) match
      case Some(PanelBody.Plot(vm)) => vm
      case other                    => fail(other.toString)

  // --- Caption numbers generated, not typed ---------------------------------------

  test("the figure caption's numbers are the binding's") {
    val c = FigureComposer.sync(FigureComposer.empty, t2)._1
    assertEquals(
      page(c, t2).caption,
      "Figure 1. Matched-minus-control spatial similarity of retrieval gaze (dataset r3, " +
        "analysis rev 4, run 7). Spatial correspondence, not sequential replay."
    )
    val two = at(t2, figure2)
    assert(
      page(c, two).caption.contains("(dataset r2, analysis rev 3, run 5)"),
      page(c, two).caption
    )
    // Rebound, Figure 2's caption follows its new binding.
    val rep     = t2.document.figures(1).reporting
    val rebound =
      AppModel.update(two, Intent.Dispatch(Command.BindFigure(figure2, run7, rep)))._1
    assert(
      page(c, rebound).caption.startsWith(
        "Figure 2. Matched-minus-control spatial similarity of retrieval gaze (dataset r3, " +
          "analysis rev 4, run 7)."
      ),
      page(c, rebound).caption
    )
  }

  test("panel D's caption carries the per-group n range the summary serves") {
    summary.map { (s, report) =>
      val d = panelD(withSummary(t2, s, report), t2)
      assertEquals(
        d.notes.last,
        "Each pair of dots is one participant; per participant, 2–17 queries per group. " +
          "Descriptive only: no intervals or tests."
      )
      // Other served numbers, other caption: nothing in it is typed.
      val other = report.copy(queryRanges =
        report.queryRanges.map(r =>
          if r.role == ReportRole.Difference then r.copy(fewest = 3, most = 11) else r
        )
      )
      assert(
        panelD(withSummary(t2, s, other), t2).notes.last.contains("3–11 queries per group")
      )
    }
  }

  // --- The stamp --------------------------------------------------------------------

  test("the stamp shows the eyes4s version, the run and the reporting spec") {
    val c = FigureComposer.sync(FigureComposer.empty, t2)._1
    assertEquals(
      page(c, t2).stamp,
      "Analysis rev 4 · run 7 (archive unbound) · data r3 · reporting “By retrieval " +
        "response” (sha256:3a0e…5dd) · studio build eyes4s " +
        StudioBuild.eyes4sBaseVersion
    )
    // The spec is edited in place, so its digest says which version a figure
    // was exported with (decision on bead S8.7): an edit changes it.
    val spec   = t2.document.reporting.head
    val pooled = eyes4s.studio.core.document.ReportingSpec
      .of(
        spec.id,
        spec.name,
        spec.groupBy,
        spec.filters,
        spec.minimumPerGroup,
        eyes4s.studio.core.document.ReportingWeight.PooledQueries
      )
      .fold(e => fail(e.message), identity)
    assertNotEquals(
      FigureCaption.specDigest(pooled, short = false),
      FigureCaption.specDigest(spec, short = false)
    )
    assert(FigureCaption.specDigest(spec, short = false).matches("sha256:[0-9a-f]{64}"))
    // Every field the spec reports with is in the digest: filters and the
    // minimum change it too.
    import eyes4s.studio.core.document.{MinimumPerGroup, ReportingFilter, ReportingSpec, Share}
    def edited(
        filters: Vector[ReportingFilter] = spec.filters,
        minimum: Option[MinimumPerGroup] = spec.minimumPerGroup
    ) = ReportingSpec
      .of(spec.id, spec.name, spec.groupBy, filters, minimum, spec.weighting)
      .fold(e => fail(e.message), identity)
    val filtered = edited(filters =
      Vector(
        ReportingFilter.OutsideWindowAtMost(Share.of(0.25).fold(e => fail(e.message), identity))
      )
    )
    val minimum = edited(minimum = MinimumPerGroup.of(3).toOption)
    val digests =
      Vector(spec, filtered, minimum).map(FigureCaption.specDigest(_, short = false))
    assertEquals(digests.distinct.size, 3)
    // The same spec digests alike.
    assertEquals(
      FigureCaption.specDigest(edited(), short = false),
      FigureCaption.specDigest(spec, short = false)
    )
    // The release line only: no commit or timestamp to regenerate on every load.
    assertEquals(StudioBuild.eyes4sBaseVersion, "0.1")
    assert(
      page(c, at(t2, figure2)).stamp
        .startsWith("Analysis rev 3 · run 5 (archive unbound) · data r2")
    )
  }

  // --- Appearance (view only) and export ---------------------------------------------

  test("text size, participant lines, panel width and images are the figure's, view only") {
    summary.map { (s, report) =>
      val c0   = withSummary(t2, s, report)
      val step = (c: FigureComposer, i: ComposerIntent) => FigureComposer.update(c, t2, i)._1
      assertEquals(page(c0, t2).textPt, 7)
      assertEquals(
        page(c0, t2).appearance.textSizes.map((_, l, chosen) => (l, chosen)),
        Vector(("6 pt", false), ("7 pt", true), ("8 pt", false))
      )
      val c1 = Vector(
        ComposerIntent.SetTextSize(FigureTextSize.Eight),
        ComposerIntent.SetParticipantLines(ParticipantLines.Hidden),
        ComposerIntent.SetPanelWidth(letter("D"), 94),
        ComposerIntent.IncludeImages(false)
      ).foldLeft(c0)(step)
      val p = page(c1, t2)
      assertEquals(p.textPt, 8)
      assertEquals(p.panels.find(_.letter == letter("D")).map(_.widthMm), Some(94))
      assertEquals(p.panels.find(_.letter == letter("C")).map(_.widthMm), Some(89))
      assertEquals(p.appearance.panelWidthMm, Some((letter("D"), 94, "94 mm")))
      assertEquals(p.appearance.includeImages, ("project snapshot includes images", false))
      val d = panelD(c1, t2)
      assertEquals(d.lines, ParticipantLines.Hidden)
      assert(d.notes.last.startsWith("Each dot is one participant's mean;"), d.notes.last)
      // The width stays on the page: at least 30 mm, at most the page.
      assertEquals(
        page(step(c1, ComposerIntent.SetPanelWidth(letter("D"), 5)), t2).panels(3).widthMm,
        30
      )
      assertEquals(
        page(step(c1, ComposerIntent.SetPanelWidth(letter("D"), 900)), t2).panels(3).widthMm,
        183
      )
      // None of it is science, and Figure 2 keeps its own appearance.
      assertEquals(page(c1, at(t2, figure2)).textPt, 7)
      assertEquals(t2.document.figures, StoryModels.t2Figures.document.figures)
    }
  }

  test("the figure typography: Plex Sans at the body size, letters at 8 pt semibold") {
    assertEquals(FigureType.BodyFace.javaFxFamily, "IBM Plex Sans")
    assertEquals(FigureType.LetterFace.javaFxFamily, "IBM Plex Sans SmBld")
    // 7 pt is 2.47 mm, 8.64 px at 3.5 px/mm.
    assertEqualsDouble(FigureType.px(7, 3.5), 7 * 25.4 / 72 * 3.5, 1e-12)
    assertEqualsDouble(FigureType.px(FigureType.LetterPt, 3.5), 9.8778, 1e-4)
  }

  test("the caption names the run's comparison method, with a generic fallback") {
    val cosine = t2.document.analyses.head.recipe.method
    assertEquals(FigureCaption.measure(cosine), "spatial similarity")
    val other = MethodSpec(ok(DefinitionRef.of("eyes4s.correlation", 1)), Vector.empty)
    assertEquals(FigureCaption.measure(other), "similarity by eyes4s.correlation@1")
  }
