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

/** The type scale and the stylesheets generated from it (ticket S1.2). */
class TypeScaleSuite extends munit.FunSuite:

  private def blocks(css: String): Map[String, Map[String, String]] =
    "(?s)([^{}]+)\\{([^{}]*)\\}".r
      .findAllMatchIn(css.replaceAll("(?s)/\\*.*?\\*/", ""))
      .map { m =>
        m.group(1).trim -> m
          .group(2)
          .split(";")
          .toList
          .map(_.trim)
          .filter(_.nonEmpty)
          .map { d =>
            val i = d.indexOf(':')
            d.substring(0, i).trim -> d.substring(i + 1).trim
          }
          .toMap
      }
      .toMap

  test("five sizes, with the spec's weights") {
    assertEquals(
      TypeSize.values.toList.map(s => (s.styleClass, s.px, s.weight.css)),
      List(
        ("t11", 11, 400),
        ("t12", 12, 400),
        ("t13", 13, 600),
        ("t16", 16, 400),
        ("t28", 28, 500)
      )
    )
    assertEquals(TypeScale.body, TypeSize.T12)
    assertEquals(TypeScale.numerals, TypeFamily.Sans)
  }

  test("seven static faces: Plex Sans 400/500/600, Plex Mono 400/500, Source Serif 4 400/600") {
    assertEquals(
      FontFace.values.toList.map(f => (f.family, f.weight.css)),
      List(
        (TypeFamily.Sans, 400),
        (TypeFamily.Sans, 500),
        (TypeFamily.Sans, 600),
        (TypeFamily.Mono, 400),
        (TypeFamily.Mono, 500),
        (TypeFamily.Serif, 400),
        (TypeFamily.Serif, 600)
      )
    )
    val names = FontFace.values.toList.map(_.javaFxFamily)
    assertEquals(names.distinct, names, "JavaFX selects a weight only by a distinct family")
  }

  test("a family without the size's weight uses its nearest face, the lighter on a tie") {
    assertEquals(TypeScale.face(TypeFamily.Sans, TypeSize.T13), FontFace.SansSemiBold)
    assertEquals(TypeScale.face(TypeFamily.Sans, TypeSize.T28), FontFace.SansMedium)
    assertEquals(TypeScale.face(TypeFamily.Mono, TypeSize.T13), FontFace.MonoMedium)
    assertEquals(TypeScale.face(TypeFamily.Mono, TypeSize.T12), FontFace.MonoRegular)
    assertEquals(TypeScale.face(TypeFamily.Serif, TypeSize.T28), FontFace.SerifRegular)
    assertEquals(TypeScale.face(TypeFamily.Serif, TypeSize.T13), FontFace.SerifSemiBold)
    TypeFamily.values.foreach { family =>
      TypeWeight.values.foreach(w => assertEquals(FontFace.nearest(family, w).family, family))
    }
  }

  test("the JavaFX sheet: body on the root, one class per size, family classes per size") {
    val css = blocks(TypeCss.javaFx)
    assertEquals(
      css(".root"),
      Map("-fx-font-family" -> "\"IBM Plex Sans\"", "-fx-font-size" -> "12px")
    )
    assertEquals(
      css(".t13"),
      Map("-fx-font-family" -> "\"IBM Plex Sans SmBld\"", "-fx-font-size" -> "13px")
    )
    assertEquals(
      css(".t28"),
      Map("-fx-font-family" -> "\"IBM Plex Sans Medm\"", "-fx-font-size" -> "28px")
    )
    assertEquals(css(".mono"), Map("-fx-font-family" -> "\"IBM Plex Mono\""))
    assertEquals(css(".mono.t13"), Map("-fx-font-family" -> "\"IBM Plex Mono Medm\""))
    assertEquals(css(".serif.t13"), Map("-fx-font-family" -> "\"Source Serif 4 Semibold\""))
    assertEquals(
      css.keySet,
      Set(".root", ".mono", ".serif") ++
        TypeSize.values.flatMap(s =>
          List("", ".mono", ".serif").map(f => s"$f.${s.styleClass}")
        ) ++ TypeHelper.values.map(h => s".${h.styleClass}")
    )
    val sizes = css.values.flatMap(_.get("-fx-font-size")).toSet
    assertEquals(sizes, TypeSize.values.map(s => s"${s.px}px").toSet)
    assert(!TypeCss.javaFx.contains("-fx-font-weight"), "JavaFX ignores weights below bold")
  }

  test("the web sheet sets weights on the typographic families") {
    val css = blocks(TypeCss.web)
    assertEquals(css(".t13"), Map("font-size" -> "13px", "font-weight" -> "600"))
    assertEquals(css(".t28"), Map("font-size" -> "28px", "font-weight" -> "500"))
    assert(css(".mono")("font-family").startsWith("\"IBM Plex Mono\""))
    assertEquals(css(".mono.t13"), Map("font-weight" -> "500"))
    assertEquals(css(".serif.t28"), Map("font-weight" -> "400"))
    assert(!css.contains(".mono.t12"), "only substituted weights get a rule")
  }

  test("the weight helpers: t11-strong 11/600, t12-label 12/500, t12-strong 12/600, mono-500") {
    assertEquals(
      TypeHelper.values.toList.map(h =>
        (h.styleClass, h.family, h.size.map(_.px), h.weight.css)
      ),
      List(
        ("t11-strong", TypeFamily.Sans, Some(11), 600),
        ("t12-label", TypeFamily.Sans, Some(12), 500),
        ("t12-strong", TypeFamily.Sans, Some(12), 600),
        ("mono-500", TypeFamily.Mono, None, 500)
      )
    )
    // Each helper names a weight its family bundles: no substitution.
    TypeHelper.values.foreach(h => assertEquals(h.face.weight, h.weight, h.styleClass))
    val fx = blocks(TypeCss.javaFx)
    assertEquals(
      fx(".t11-strong"),
      Map("-fx-font-family" -> "\"IBM Plex Sans SmBld\"", "-fx-font-size" -> "11px")
    )
    assertEquals(
      fx(".t12-label"),
      Map("-fx-font-family" -> "\"IBM Plex Sans Medm\"", "-fx-font-size" -> "12px")
    )
    assertEquals(
      fx(".t12-strong"),
      Map("-fx-font-family" -> "\"IBM Plex Sans SmBld\"", "-fx-font-size" -> "12px")
    )
    assertEquals(fx(".mono-500"), Map("-fx-font-family" -> "\"IBM Plex Mono Medm\""))
    // A helper follows the size classes, so it wins at equal specificity.
    val fxSheet = TypeCss.javaFx
    assert(fxSheet.indexOf(".mono-500 {") > fxSheet.indexOf(".t28 {"))
    assert(fxSheet.indexOf(".t11-strong {") > fxSheet.indexOf(".serif.t28 {"))
    val web = blocks(TypeCss.web)
    assertEquals(web(".t11-strong"), Map("font-size" -> "11px", "font-weight" -> "600"))
    assertEquals(web(".t12-label"), Map("font-size" -> "12px", "font-weight" -> "500"))
    assertEquals(web(".t12-strong"), Map("font-size" -> "12px", "font-weight" -> "600"))
    assertEquals(web(".mono-500")("font-weight"), "500")
    assert(web(".mono-500")("font-family").startsWith("\"IBM Plex Mono\""))
  }
