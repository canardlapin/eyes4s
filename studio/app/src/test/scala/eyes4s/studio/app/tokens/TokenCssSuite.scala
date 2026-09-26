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

package eyes4s.studio.app.tokens

/** The token source and the stylesheets generated from it (ticket S1.1). */
class TokenCssSuite extends munit.FunSuite:

  // `selector {` ... `}` blocks of a generated sheet, as selector -> declarations.
  private def blocks(css: String): Map[String, Map[String, String]] =
    val withoutComments = css.replaceAll("(?s)/\\*.*?\\*/", "")
    "(?s)([^{}]+)\\{([^{}]*)\\}".r
      .findAllMatchIn(withoutComments)
      .map { m =>
        val declarations = m
          .group(2)
          .split(";")
          .toList
          .map(_.trim)
          .filter(_.nonEmpty)
          .map { d =>
            val i = d.indexOf(':')
            d.substring(0, i).trim -> d.substring(i + 1).trim
          }
        m.group(1).trim.replaceAll("\\s+", " ") -> declarations.toMap
      }
      .toMap

  test("token names are unique across the three families") {
    val names = Tokens.all.map(_.cssName)
    assertEquals(names.distinct, names)
    assert(names.forall(_.matches("[a-z][a-z0-9-]*")), names)
  }

  test("colours render as the boards write them") {
    assertEquals(Colour.srgb(0x0e6e68).webCss, "#0E6E68")
    assertEquals(Colour.srgba(0x1c1e21, 35).webCss, "rgba(28,30,33,.35)")
    assertEquals(Colour.srgba(0x1c1e21, 35).javaFxCss, "rgba(28,30,33,0.35)")
    assertEquals(Colour.srgba(0x1c1e21, 5).webCss, "rgba(28,30,33,.05)")
    assertEquals(Colour.srgba(0x1c1e21, 50).javaFxCss, "rgba(28,30,33,0.5)")
    assertEquals(Colour.srgba(0x1c1e21, 0).javaFxCss, "rgba(28,30,33,0)")
    assertEquals(Colour.srgba(0x1c1e21, 100), Colour.srgb(0x1c1e21))
  }

  test("an out-of-range colour literal does not compile") {
    import scala.compiletime.testing.typeCheckErrors
    def messages(errors: List[scala.compiletime.testing.Error]): List[String] =
      errors.map(_.message)
    assertEquals(
      messages(typeCheckErrors("Colour.srgb(0x1000000)")),
      List("Colour.srgb: the literal must lie in 0x000000..0xFFFFFF")
    )
    assertEquals(
      messages(typeCheckErrors("Colour.srgb(-1)")),
      List("Colour.srgb: the literal must lie in 0x000000..0xFFFFFF")
    )
    assertEquals(
      messages(typeCheckErrors("Colour.srgba(0x000000, 101)")),
      List("Colour.srgba: alpha is a whole percent in 0..100")
    )
    assertEquals(
      messages(typeCheckErrors("Colour.srgba(0x1000000, 50)")),
      List("Colour.srgba: the literal must lie in 0x000000..0xFFFFFF")
    )
    // A colour cannot come from a non-constant, or bypass the literal check.
    assert(typeCheckErrors("{ val n = 3; Colour.srgb(n) }").nonEmpty)
    assert(typeCheckErrors("Colour(1, 2, 3, 100)").nonEmpty)
    assert(typeCheckErrors("Colour.fromChecked(0x1000000, 100)").nonEmpty)
    assertEquals(typeCheckErrors("Colour.srgba(0xffffff, 0)"), Nil)
  }

  test("each JavaFX sheet defines every token on .root and every stage variant") {
    Theme.values.foreach { theme =>
      val sheet = blocks(TokenCss.javaFx(theme))
      val root  = sheet(".root")
      Tokens.all.foreach { ref =>
        val expected = Tokens.resolve(ref, theme, Tokens.defaultStage).javaFxCss
        assertEquals(
          root.get(TokenCss.javaFxName(ref)),
          Some(expected),
          s"$theme ${ref.cssName}"
        )
      }
      StageVariant.values.foreach { stage =>
        val variant = sheet(s".${TokenCss.stageStyleClass(stage)}")
        assertEquals(variant.keySet, StageToken.values.map(t => s"-es-${t.cssName}").toSet)
        StageToken.values.foreach { t =>
          assertEquals(variant(s"-es-${t.cssName}"), Tokens.staged(stage, t).javaFxCss)
        }
      }
    }
  }

  test("the JavaFX sheets differ in themed tokens only") {
    val light     = blocks(TokenCss.javaFx(Theme.Light))
    val dark      = blocks(TokenCss.javaFx(Theme.Dark))
    val differing = light(".root").keySet.filter(k => light(".root")(k) != dark(".root")(k))
    assert(differing.forall(k => ThemedToken.values.exists(t => s"-es-${t.cssName}" == k)))
    assertEquals(light - ".root", dark - ".root")
  }

  test("the web sheet mirrors the boards' selectors") {
    val sheet = blocks(TokenCss.web)
    assertEquals(
      sheet.keySet,
      Set(".es", ".es.dark") ++ StageVariant.values.map { s =>
        s"[data-stage=${s.cssName}], .es[data-stage=${s.cssName}]"
      }
    )
    Tokens.all.foreach { ref =>
      val expected = Tokens.resolve(ref, Theme.Light, Tokens.defaultStage).webCss
      assertEquals(sheet(".es").get(TokenCss.webName(ref)), Some(expected), ref.cssName)
    }
    ThemedToken.values.foreach { t =>
      val ref = TokenRef.Themed(t)
      assertEquals(
        sheet(".es.dark").get(TokenCss.webName(ref)),
        Some(Tokens.themed(Theme.Dark, t).webCss)
      )
    }
    assertEquals(sheet(".es")("--sel-ring"), "0 0 0 2px #FFFFFF,0 0 0 4px #1C1E21")
    assertEquals(sheet(".es.dark")("--sel-ring"), "0 0 0 2px #000000,0 0 0 4px #ECEAE5")
  }

  test("the ramps are the palette tokens, in order") {
    assertEquals(
      Tokens.massRamp.stops,
      List(PaletteToken.Ramp0, PaletteToken.Ramp1, PaletteToken.Ramp2, PaletteToken.Ramp3)
        .map(Tokens.palette)
    )
    assertEquals(Tokens.differenceRamp.zero, Tokens.palette(PaletteToken.DivMid))
    // The mass ramp runs light to dark.
    val lightness =
      Tokens.massRamp.stops.map(c => Wcag.lightness(c).fold(e => fail(e.message), identity))
    assertEquals(lightness, lightness.sorted.reverse)
  }

  test("every background token is opaque") {
    val backgrounds = TokenUsage.text.map(_.background) ++ TokenUsage.marks.map(_.background)
    for
      ref   <- backgrounds.distinct
      theme <- Theme.values
      stage <- StageVariant.values
    do assert(Tokens.resolve(ref, theme, stage).isOpaque, ref.cssName)
  }
