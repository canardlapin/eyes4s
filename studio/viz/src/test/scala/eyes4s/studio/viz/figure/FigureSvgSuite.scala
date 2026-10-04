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

package eyes4s.studio.viz.figure

/** The SVG export's font embedding (ticket S9.3): every family the text
  * names is embedded, and a family no supplied font provides is refused,
  * never left to the reader's system.
  */
class FigureSvgSuite extends munit.FunSuite:

  private def font(family: String) =
    EmbeddedFont.of(family, IArray[Byte](1, 2, 3)).fold(e => fail(e), identity)

  private val svg =
    """<svg xmlns="http://www.w3.org/2000/svg"><text font-family="Plex">a</text>""" +
      """<text font-family="Plex SmBld">b</text></svg>"""

  test("the families are read in first-use order") {
    assertEquals(FigureSvg.families(svg), Vector("Plex", "Plex SmBld"))
  }

  test("every named family is embedded once, inside the svg element") {
    val out = FigureSvg.embed(svg, Vector(font("Plex"), font("Plex SmBld"), font("Unused")))
    val css = out.fold(e => fail(e.message), identity)
    assert(css.startsWith("""<svg xmlns="http://www.w3.org/2000/svg"><defs><style>"""), css)
    assertEquals("@font-face".r.findAllMatchIn(css).size, 2)
    assert(!css.contains("Unused"), css)
  }

  test("no fonts at all is refused, naming the first family") {
    assertEquals(
      FigureSvg.embed(svg, Vector.empty),
      Left(FigureSvgError.MissingFont("Plex", Vector.empty))
    )
  }

  test("a partial set is refused, naming the family it lacks and what was supplied") {
    assertEquals(
      FigureSvg.embed(svg, Vector(font("Plex"))),
      Left(FigureSvgError.MissingFont("Plex SmBld", Vector("Plex")))
    )
    assertEquals(
      FigureSvgError.MissingFont("Plex SmBld", Vector("Plex")).message,
      "The figure uses the font Plex SmBld, which is not among the embedded fonts (Plex)."
    )
  }
