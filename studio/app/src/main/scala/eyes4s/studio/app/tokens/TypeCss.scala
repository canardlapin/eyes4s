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

/** Stylesheets generated from [[TypeScale]] (ticket S1.2).
  *
  * Pure text, like [[TokenCss]]: studio-desktop writes and checks the files.
  * The classes are the boards' own: `t11`, `t12`, `t13`, `t16` and `t28` set
  * a size and its weight; `mono` and `serif` set a family and combine with a
  * size class (`mono t13`). The scene root sets body text, Plex Sans at 12px.
  *
  * JavaFX selects weights by face family, not by `-fx-font-weight`
  * ([[FontFace]]), so each class names the face's own family. A size class
  * also sets the family, so `mono` belongs on the node that shows the numeral,
  * not on an ancestor.
  */
object TypeCss:

  private def quoted(family: String): String = "\"" + family + "\""

  private def block(selector: String, declarations: List[(String, String)]): String =
    declarations
      .map { case (name, value) => s"  $name: $value;\n" }
      .mkString(s"$selector {\n", "", "}\n")

  private def generatedNote: String =
    "/* Generated from eyes4s.studio.app.tokens.TypeScale (ticket S1.2). Do not edit:\n" +
      " * change TypeScale.scala and run `sbt studioTokens`. */\n"

  private def classSelector(family: TypeFamily, size: Option[TypeSize]): Option[String] =
    (family.styleClass.toList ++ size.map(_.styleClass).toList) match
      case Nil     => None
      case classes => Some(classes.map("." + _).mkString)

  // ---------------------------------------------------------------------------
  // JavaFX
  // ---------------------------------------------------------------------------

  /** The JavaFX stylesheet of the type scale; the same for both themes. */
  def javaFx: String =
    val root = block(
      ".root",
      List(
        "-fx-font-family" -> quoted(
          TypeScale.face(TypeFamily.Sans, TypeScale.body).javaFxFamily
        ),
        "-fx-font-size" -> s"${TypeScale.body.px}px"
      )
    )
    val sizes = TypeSize.values.toList.map { size =>
      block(
        s".${size.styleClass}",
        List(
          "-fx-font-family" -> quoted(TypeScale.face(TypeFamily.Sans, size).javaFxFamily),
          "-fx-font-size"   -> s"${size.px}px"
        )
      )
    }
    val families = TypeFamily.values.toList.filter(_.styleClass.isDefined).flatMap { family =>
      val plain = block(
        classSelector(family, None).mkString,
        List(
          "-fx-font-family" -> quoted(FontFace.nearest(family, TypeWeight.Regular).javaFxFamily)
        )
      )
      plain :: TypeSize.values.toList.map { size =>
        block(
          classSelector(family, Some(size)).mkString,
          List("-fx-font-family" -> quoted(TypeScale.face(family, size).javaFxFamily))
        )
      }
    }
    TokenCss.licenseHeader + generatedNote +
      "\n/* Eyes Studio type scale: five sizes, JavaFX. Body text is set on the root. */\n" +
      root + "\n" + sizes.mkString("\n") +
      "\n/* Families: numerals and identifiers in Plex Mono, methods prose in Source Serif 4. */\n" +
      families.mkString("\n")

  /** The file name JavaFX loads the type scale from. */
  val javaFxFileName: String = "studio-type.css"

  // ---------------------------------------------------------------------------
  // Web
  // ---------------------------------------------------------------------------

  /** The type scale as web CSS, under the boards' `.es` root. Checked in CI,
    * not shipped.
    */
  def web: String =
    def stack(family: TypeFamily): String = family match
      case TypeFamily.Sans =>
        quoted(family.webName) + ",-apple-system,\"Helvetica Neue\",sans-serif"
      case TypeFamily.Mono  => quoted(family.webName) + ",ui-monospace,monospace"
      case TypeFamily.Serif => quoted(family.webName) + ",Georgia,serif"
    val root = block(
      ".es",
      List(
        "font-family" -> stack(TypeFamily.Sans),
        "font-size"   -> s"${TypeScale.body.px}px",
        "font-weight" -> TypeScale.body.weight.css.toString
      )
    )
    val sizes = TypeSize.values.toList.map { size =>
      block(
        s".${size.styleClass}",
        List("font-size" -> s"${size.px}px", "font-weight" -> size.weight.css.toString)
      )
    }
    val families = TypeFamily.values.toList.filter(_.styleClass.isDefined).flatMap { family =>
      val plain =
        block(classSelector(family, None).mkString, List("font-family" -> stack(family)))
      // Only where the family lacks the size's weight: the nearest bundled one.
      val substituted = TypeSize.values.toList.flatMap { size =>
        val face = TypeScale.face(family, size)
        if face.weight == size.weight then Nil
        else
          List(
            block(
              classSelector(family, Some(size)).mkString,
              List("font-weight" -> face.weight.css.toString)
            )
          )
      }
      plain :: substituted
    }
    TokenCss.licenseHeader + generatedNote +
      "\n/* Eyes Studio type scale as web CSS, for a future web shell. */\n" +
      root + "\n" + sizes.mkString("\n") + "\n" + families.mkString("\n")
