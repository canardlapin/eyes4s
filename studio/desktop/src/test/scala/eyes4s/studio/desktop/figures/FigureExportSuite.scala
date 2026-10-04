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
import eyes4s.studio.app.{AppModel, StoryModels}
import eyes4s.studio.core.figures.ReferenceReads
import eyes4s.studio.core.fixture.{GoldenAssets, MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.ScaleIndex
import eyes4s.studio.viz.figure.FigureSvg

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import java.util.Base64
import scala.concurrent.{ExecutionContext, Future}

/** Figure export (ticket S9.3; Figures.dc.html, export) of story moment t2's
  * Figure 1 on the fake backend: the page laid out again for the target, its
  * fonts embedded, its disclosures kept, and its SVG held against a golden.
  */
class FigureExportSuite extends munit.FunSuite:
  import StoryMoments.{r3, run7}

  private given ExecutionContext = ExecutionContext.global

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val buildRoot: Path = Paths.get(
    String(
      getClass.getClassLoader
        .getResourceAsStream("eyes4s/studio/desktop/build-root.txt")
        .readAllBytes,
      UTF_8
    ).trim
  )

  private def t2: AppModel = StoryModels.t2Figures

  /** Figure 1's page with every panel read from the fake backend. */
  private def page(c: FigureComposer => FigureComposer = identity): Future[PageVM] =
    val scale2   = ok(ScaleIndex.of(2))
    val p17ret07 = MockStudy.key("P17", "ret_07")
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
      _ <- session.close
    yield
      val registry = t2.document.dataset(r3).map(GoldenAssets.registry).getOrElse(Left("no r3"))
      val loaded   = Vector(
        ComposerIntent.SummaryRead(run7, SummaryAnswer.Answered(ok(summary))),
        ComposerIntent.ReferencesRead(run7, scale2, p17ret07, Right(ok(scores))),
        ComposerIntent.DisplaysRead(r3, registry.map(DisplaySource.Served(_)))
      ).foldLeft(FigureComposer.sync(FigureComposer.empty, t2)._1)((x, i) =>
        FigureComposer.update(x, t2, i)._1
      )
      FigureComposer.view(c(loaded), t2).page.getOrElse(fail("no page"))

  private def svgOf(p: PageVM): String =
    ok(FigureSvg.render(p, ok(FigureFonts.bundled)))

  /** The SVG without its embedded font data, for a golden that stays readable. */
  private def withoutFonts(svg: String): String =
    svg.replaceAll("(?s)<defs><style>@font-face.*?</style></defs>", "<defs><style/></defs>")

  /** Every text run of the SVG, in order, joined by spaces. */
  private def text(svg: String): String =
    "<text[^>]*>([^<]*)</text>".r.findAllMatchIn(svg).map(_.group(1)).mkString(" ")

  private def unescape(s: String): String =
    s.replace("&amp;", "&").replace("&lt;", "<").replace("&gt;", ">").replace("&quot;", "\"")

  test("the exported SVG matches its golden (fonts aside)") {
    page().map { p =>
      val svg    = withoutFonts(svgOf(p))
      val golden = buildRoot.resolve("docs/studio/figures/golden/figure-1.svg")
      if sys.env.contains("EYES4S_UPDATE_GOLDENS") then
        Files.createDirectories(golden.getParent)
        Files.writeString(golden, svg, UTF_8): Unit
      assert(
        Files.exists(golden),
        s"missing $golden; run with EYES4S_UPDATE_GOLDENS=1 to write it"
      )
      assertNoDiff(svg, Files.readString(golden, UTF_8))
    }
  }

  test("the layout is rebuilt for the target: the page's journal width in millimetres") {
    for
      two <- page()
      one <- page(c =>
        FigureComposer.update(c, t2, ComposerIntent.SetWidth(PageWidth.SingleColumn))._1
      )
    yield
      def width(svg: String) =
        """<svg[^>]* width="([0-9.]+)"""".r.findFirstMatchIn(svg).map(_.group(1).toDouble)
      // 183 mm and 89 mm at 96 px per inch, in whole pixels.
      assertEquals(width(svgOf(two)), Some(math.ceil(183 / 25.4 * 96)))
      assertEquals(width(svgOf(one)), Some(math.ceil(89 / 25.4 * 96)))
      // Every panel is drawn from its template, with its letter, not captured.
      val drawn = unescape(text(svgOf(two)))
      Vector("A", "B", "C", "D", "E").foreach(l => assert(drawn.contains(l)))
      assert(drawn.contains("Matched 0.73 · highest of 19 controls street-112 0.61"), drawn)
  }

  test("the imagery disclosure survives export (E2E-10)") {
    page().map { p =>
      val drawn = unescape(text(svgOf(p)))
      assert(drawn.contains("Displayed: blank + fixation cross."), drawn)
      assert(drawn.contains("The remembered image was not shown."), drawn)
      // So do the generated caption and the provenance stamp.
      assert(drawn.contains("(dataset r3, analysis rev 4, run 7)"), drawn)
      assert(drawn.contains("studio build eyes4s"), drawn)
    }
  }

  test("every font the SVG names is embedded, byte for byte the bundled face") {
    page().map { p =>
      val svg      = svgOf(p)
      val families = FigureSvg.families(svg)
      assert(
        families.contains("IBM Plex Sans") && families.contains("IBM Plex Sans SmBld"),
        families
      )
      val embedded =
        """@font-face\{font-family:"([^"]+)";src:url\(data:font/ttf;base64,([^)]+)\)""".r
          .findAllMatchIn(svg)
          .map(m => m.group(1) -> Base64.getDecoder.decode(m.group(2)).toSeq)
          .toMap
      assertEquals(embedded.keySet, families.toSet)
      ok(FigureFonts.bundled).filter(f => families.contains(f.family)).foreach { f =>
        assertEquals(embedded(f.family), f.bytes.toSeq, f.family)
      }
    }
  }

  test("a format whose backend is not yet pinned says so") {
    page().map { p =>
      assert(FigureExport.render(ExportFormat.Pdf, p).isLeft)
    }
  }
