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

import sbt._

/** Only the five sizes of the type scale in studio sources (DESIGN_SPEC section
  * 7, ticket S1.2).
  *
  * The scale is `eyes4s.studio.app.tokens.TypeSize`: 11, 12, 13, 16 and 28 px.
  * The build cannot link studio code, so [[allowedPx]] repeats it and
  * `TypeScaleSuite` (studio-desktop) fails if the two differ.
  *
  * The lint reads the main CSS, FXML and Scala sources of every studio project
  * and fails on a font size that is not one of the five, written in pixels:
  *
  *   - in CSS and FXML, a `-fx-font-size` or `font-size` value, and the size in
  *     a `-fx-font` or `font` shorthand (the token with a unit or a size
  *     keyword; a bare number there is a weight);
  *   - in Scala, the same inside inline style strings, and the literal size of
  *     `Font.font(...)`, `Font.loadFont(...)` and `new Font(...)`.
  *
  * Comments are ignored. [[selfTest]] plants violations and clean inputs;
  * `checkStudioTypeScale` (in `studioStyleCheck`) runs it before the scan.
  */
object TypeScaleLint {

  /** The five sizes, in px. Keep equal to `TypeSize.values.map(_.px)`. */
  val allowedPx: Seq[Int] = Seq(11, 12, 13, 16, 28)

  /** Studio source roots, relative to the build root. */
  val roots: Seq[String] = NoLiteralColourLint.roots

  /** File extensions the lint reads. */
  val extensions: Seq[String] = Seq("scala", "css", "fxml")

  /** One font size outside the scale, located by file and line. */
  final case class Violation(file: String, line: Int, size: String) {
    def render: String =
      s"$file:$line  '$size' is not one of ${allowedPx.map(_ + "px").mkString(", ")}"
  }

  /** True when a path under a studio root is main source the lint reads. */
  def inScope(relativePath: String): Boolean =
    roots.exists(r => relativePath.startsWith(r + "/")) &&
      relativePath.contains("/src/main/") &&
      !relativePath.contains("/target/") &&
      extensions.exists(e => relativePath.endsWith("." + e))

  private val sizeKeywords =
    Seq(
      "xx-small",
      "x-small",
      "small",
      "medium",
      "large",
      "x-large",
      "xx-large",
      "smaller",
      "larger"
    )

  private val unitNumber =
    "[+-]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:px|pt|pc|em|rem|ex|%|mm|cm|in)"

  // `-fx-font-size: 12px` / `font-size: 12px`: the whole value up to `;`, `}`
  // or the end of a string.
  private val sizeDeclaration =
    "(?i)(?<![\\w-])(?:-fx-)?font-size\\s*:\\s*([^;}\"\\n]+)".r

  // `-fx-font: 600 13px "IBM Plex Sans"` / `font: ...`.
  private val shorthand = "(?i)(?<![\\w-])(?:-fx-)?font\\s*:\\s*([^;}\\n]+)".r

  private val shorthandSize =
    ("(?i)(?<![\\w.-])(" + unitNumber + "|" + sizeKeywords.mkString("|") + ")(?![\\w-])").r

  // Font.font("IBM Plex Sans", 14) / Font.loadFont(in, 14) / new Font("x", 14):
  // the last argument, when it is a numeric literal.
  private val fontCall =
    "(?<![\\w$])(?:Font\\s*\\.\\s*(?:font|loadFont)|new\\s+(?:javafx\\.scene\\.text\\.)?Font)\\s*\\(([^()]*)\\)".r
  private val numericLiteral = "^[+-]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)[dDfF]?$".r

  private val cssComment = "(?s)/\\*.*?\\*/".r

  private def allowed(value: String): Boolean = {
    val v = value.trim.toLowerCase
    allowedPx.exists(px => v == s"${px}px" || v == s"$px.0px")
  }

  /** Every font size outside the scale in one file's text. */
  def scanSource(fileName: String, source: String): Seq[Violation] = {
    val scala    = fileName.endsWith(".scala")
    val readable =
      if (scala) NoLiteralColourLint.scalaWithoutComments(source)
      else if (fileName.endsWith(".fxml")) NoLiteralColourLint.xmlWithoutComments(source)
      else
        cssComment.replaceAllIn(
          source,
          m => java.util.regex.Matcher.quoteReplacement(m.matched.replaceAll("[^\\n]", " "))
        )
    def lineOf(offset: Int) = readable.take(offset).count(_ == '\n') + 1

    val declared = sizeDeclaration.findAllMatchIn(readable).collect {
      case m if !allowed(m.group(1)) => Violation(fileName, lineOf(m.start), m.group(1).trim)
    }
    val short = shorthand.findAllMatchIn(readable).flatMap { m =>
      val value = m.group(1).takeWhile(_ != '"') match {
        case "" => m.group(1)
        case v  => v
      }
      shorthandSize.findAllMatchIn(value).collect {
        case s if !allowed(s.group(1)) => Violation(fileName, lineOf(m.start), s.group(1))
      }
    }
    val calls =
      if (!scala) Iterator.empty
      else
        fontCall.findAllMatchIn(readable).flatMap { m =>
          val last = m.group(1).split(",").lastOption.map(_.trim).getOrElse("")
          if (numericLiteral.findFirstIn(last).isEmpty) None
          else {
            val px = last.stripSuffix("d").stripSuffix("D").stripSuffix("f").stripSuffix("F")
            if (allowedPx.exists(a => BigDecimal(px) == BigDecimal(a))) None
            else Some(Violation(fileName, lineOf(m.start), last))
          }
        }
    (declared ++ short ++ calls).toSeq.sortBy(v => (v.line, v.size))
  }

  /** Every font size outside the scale in the studio sources under `buildRoot`. */
  def scanTree(buildRoot: File): Seq[Violation] =
    roots.flatMap { root =>
      val dir = buildRoot / root
      if (!dir.exists) Nil
      else
        dir
          .**(new SimpleFileFilter(f => f.isFile))
          .get
          .map(f => (f, IO.relativize(buildRoot, f).getOrElse(f.getPath).replace('\\', '/')))
          .filter { case (_, rel) => inScope(rel) }
          .sortBy(_._2)
          .flatMap { case (f, rel) => scanSource(rel, IO.read(f)) }
    }

  /** Planted inputs: each violation must be found, each clean input must pass.
    * Returns the failures of the lint itself; empty when it works.
    */
  def selfTest: Seq[String] = {
    val violations: Seq[(String, String)] = Seq(
      "a.css"   -> ".x { -fx-font-size: 14px; }",
      "b.css"   -> ".x { -fx-font-size: 1.2em; }",
      "c.css"   -> ".x { -fx-font-size: 12pt; }",
      "d.css"   -> ".x { -fx-font: 600 15px \"IBM Plex Sans\"; }",
      "e.css"   -> ".x { -fx-font: larger \"IBM Plex Sans\"; }",
      "f.css"   -> ".x { font-size: 10px }",
      "g.fxml"  -> "<Label style=\"-fx-font-size: 9px\"/>",
      "H.scala" -> "node.setStyle(\"-fx-font-size: 20px;\")",
      "I.scala" -> "label.setFont(Font.font(\"IBM Plex Sans\", 14))",
      "J.scala" -> "Font.loadFont(in, 18.0)",
      "K.scala" -> "new Font(\"IBM Plex Mono\", 10d)"
    )
    val clean: Seq[(String, String)] = Seq(
      "a.css"   -> ".t13 { -fx-font-family: \"IBM Plex Sans SmBld\"; -fx-font-size: 13px; }",
      "b.css"   -> ".x { -fx-font: 600 13px \"IBM Plex Sans\"; }",
      "c.css"   -> "/* -fx-font-size: 14px; */ .x { -fx-font-size: 28px; }",
      "d.css"   -> ".x { -fx-font-weight: 600; -fx-font-family: \"IBM Plex Mono\"; }",
      "E.scala" -> "// setStyle(\"-fx-font-size: 20px\")\nval x = 1",
      "F.scala" -> "Font.loadFont(in, TypeScale.body.px.toDouble)",
      "G.scala" -> "Font.font(\"IBM Plex Sans\", 12)",
      "H.scala" -> "node.setStyle(\"-fx-font-size: 16px\")",
      "i.fxml"  -> "<!-- style=\"-fx-font-size: 9px\" --><Label/>"
    )
    val missed = violations.collect {
      case (file, text) if scanSource(file, text).isEmpty =>
        s"missed a planted violation in $file: $text"
    }
    val flagged = clean.flatMap { case (file, text) =>
      scanSource(file, text).map(v => s"flagged clean input $file: ${v.render}")
    }
    missed ++ flagged
  }
}
