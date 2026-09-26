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

/** No literal colour outside the token source (DESIGN_SPEC section 4, ticket
  * S1.1).
  *
  * Every Eyes Studio colour comes from `eyes4s.studio.app.tokens.Tokens`, which
  * generates the JavaFX stylesheets and the web custom properties. This lint
  * scans the main Scala, CSS and FXML sources of every studio project and
  * fails on:
  *
  *   - a hex colour (`#RGB`, `#RGBA`, `#RRGGBB`, `#RRGGBBAA`);
  *   - a CSS colour function (`rgb(`, `rgba(`, `hsl(`, `hsla(`, `hsb(`, `hsba(`);
  *   - a JavaFX colour factory or constant (`Color.rgb(`, `Color.web(`,
  *     `Color.color(`, `Color.hsb(`, `Color.gray(`, `Color.grayRgb(`,
  *     `Color.valueOf(`, `new Color(`, `Color.RED`, `Paint.valueOf(`);
  *     `Color.TRANSPARENT` is allowed;
  *   - a token-package constructor (`Colour.srgb(`, `Colour.srgba(`);
  *   - in Scala, a 24- or 32-bit colour int (`0xRRGGBB`, `0xAARRGGBB`) and a
  *     named colour in an inline style (`"-fx-fill: white"`);
  *   - in CSS, a named colour in a declaration value (`transparent` allowed).
  *
  * Comments are ignored. In CSS only declaration blocks are read, so an id
  * selector such as `#add` is not a colour; in Scala a hex-like selector
  * passed to `lookup`, `lookupAll` or `setId` is not one either. The token
  * source (`Tokens.scala`, `Colour.scala`), the files generated from it, and
  * nothing else are exempt ([[whitelist]]).
  *
  * [[selfTest]] plants violations and clean inputs; `checkStudioColours` runs
  * it before the real scan, so a rule that stops detecting fails the build.
  */
object NoLiteralColourLint {

  /** Studio source roots, relative to the build root. */
  val roots: Seq[String] = Seq("studio/core", "studio/app", "studio/viz", "studio/desktop")

  /** File extensions the lint reads. */
  val extensions: Seq[String] = Seq("scala", "css", "fxml")

  /** Paths exempt from the lint (a prefix ending in `/` exempts a directory),
    * each with its reason.
    */
  val whitelist: Seq[(String, String)] = Seq(
    "studio/app/src/main/scala/eyes4s/studio/app/tokens/Tokens.scala" ->
      "the token source: every colour and ramp",
    "studio/app/src/main/scala/eyes4s/studio/app/tokens/Colour.scala" ->
      "the colour type: its literal range checks and its CSS rendering",
    "studio/desktop/src/main/resources/eyes4s/studio/desktop/studio.css" ->
      "generated from the token source (TokenFiles)",
    "studio/desktop/src/main/resources/eyes4s/studio/desktop/studio-dark.css" ->
      "generated from the token source (TokenFiles)"
  )

  /** One literal colour, located by file and line. */
  final case class Violation(file: String, line: Int, literal: String) {
    def render: String = s"$file:$line  '$literal'"
  }

  /** True when `relativePath` (with `/` separators) is exempt. */
  def whitelisted(relativePath: String): Boolean =
    whitelist.exists { case (p, _) =>
      if (p.endsWith("/")) relativePath.startsWith(p) else relativePath == p
    }

  /** True when a path under a studio root is main source the lint reads. */
  def inScope(relativePath: String): Boolean =
    roots.exists(r => relativePath.startsWith(r + "/")) &&
      relativePath.contains("/src/main/") &&
      !relativePath.contains("/target/") &&
      extensions.exists(e => relativePath.endsWith("." + e))

  private val hexColour =
    "(?<![\\w#$&.\\]\\[])#(?:[0-9A-Fa-f]{8}|[0-9A-Fa-f]{6}|[0-9A-Fa-f]{3,4})(?![\\w-])".r
  private val cssFunction = "(?<![\\w-])(?:rgba?|hsla?|hsba?)\\s*\\(".r
  private val fxFactory   =
    "(?<![\\w$])Color\\s*\\.\\s*(?:rgb|web|color|hsb|gray|grayRgb|valueOf)\\s*\\(".r
  private val fxNew      = "(?<![\\w$])new\\s+(?:javafx\\.scene\\.paint\\.)?Color\\s*\\(".r
  private val fxConstant =
    "(?<![\\w$])Color\\s*\\.\\s*(?!TRANSPARENT(?![\\w$]))[A-Z][A-Z0-9_]+(?![\\w$])".r
  private val paintValue     = "(?<![\\w$])Paint\\s*\\.\\s*valueOf\\s*\\(".r
  private val tokenMint      = "(?<![\\w$])Colour\\s*\\.\\s*srgba?\\s*\\(".r
  private val commonPatterns =
    Seq(cssFunction, fxFactory, fxNew, fxConstant, paintValue, tokenMint)

  private val colourInt = "(?<![\\w.$])0[xX](?:[0-9A-Fa-f]{8}|[0-9A-Fa-f]{6})(?![\\w$])".r

  /** CSS named colours (CSS Color 4, which JavaFX also reads), except
    * `transparent`, which is the absence of a colour rather than a choice of one.
    */
  val namedColours: Seq[String] = Seq(
    "aliceblue",
    "antiquewhite",
    "aqua",
    "aquamarine",
    "azure",
    "beige",
    "bisque",
    "black",
    "blanchedalmond",
    "blue",
    "blueviolet",
    "brown",
    "burlywood",
    "cadetblue",
    "chartreuse",
    "chocolate",
    "coral",
    "cornflowerblue",
    "cornsilk",
    "crimson",
    "cyan",
    "darkblue",
    "darkcyan",
    "darkgoldenrod",
    "darkgray",
    "darkgreen",
    "darkgrey",
    "darkkhaki",
    "darkmagenta",
    "darkolivegreen",
    "darkorange",
    "darkorchid",
    "darkred",
    "darksalmon",
    "darkseagreen",
    "darkslateblue",
    "darkslategray",
    "darkslategrey",
    "darkturquoise",
    "darkviolet",
    "deeppink",
    "deepskyblue",
    "dimgray",
    "dimgrey",
    "dodgerblue",
    "firebrick",
    "floralwhite",
    "forestgreen",
    "fuchsia",
    "gainsboro",
    "ghostwhite",
    "gold",
    "goldenrod",
    "gray",
    "green",
    "greenyellow",
    "grey",
    "honeydew",
    "hotpink",
    "indianred",
    "indigo",
    "ivory",
    "khaki",
    "lavender",
    "lavenderblush",
    "lawngreen",
    "lemonchiffon",
    "lightblue",
    "lightcoral",
    "lightcyan",
    "lightgoldenrodyellow",
    "lightgray",
    "lightgreen",
    "lightgrey",
    "lightpink",
    "lightsalmon",
    "lightseagreen",
    "lightskyblue",
    "lightslategray",
    "lightslategrey",
    "lightsteelblue",
    "lightyellow",
    "lime",
    "limegreen",
    "linen",
    "magenta",
    "maroon",
    "mediumaquamarine",
    "mediumblue",
    "mediumorchid",
    "mediumpurple",
    "mediumseagreen",
    "mediumslateblue",
    "mediumspringgreen",
    "mediumturquoise",
    "mediumvioletred",
    "midnightblue",
    "mintcream",
    "mistyrose",
    "moccasin",
    "navajowhite",
    "navy",
    "oldlace",
    "olive",
    "olivedrab",
    "orange",
    "orangered",
    "orchid",
    "palegoldenrod",
    "palegreen",
    "paleturquoise",
    "palevioletred",
    "papayawhip",
    "peachpuff",
    "peru",
    "pink",
    "plum",
    "powderblue",
    "purple",
    "rebeccapurple",
    "red",
    "rosybrown",
    "royalblue",
    "saddlebrown",
    "salmon",
    "sandybrown",
    "seagreen",
    "seashell",
    "sienna",
    "silver",
    "skyblue",
    "slateblue",
    "slategray",
    "slategrey",
    "snow",
    "springgreen",
    "steelblue",
    "tan",
    "teal",
    "thistle",
    "tomato",
    "turquoise",
    "violet",
    "wheat",
    "white",
    "whitesmoke",
    "yellow",
    "yellowgreen"
  )

  private val namedAlternatives = namedColours.sortBy(-_.length).mkString("|")

  // A named colour as a word inside a CSS value (after the property's colon).
  private val cssNamed =
    ("(?i)(?<![\\w#.$-])(?:" + namedAlternatives + ")(?![\\w(-])").r

  // A named colour in an inline JavaFX style string: `-fx-fill: white`.
  private val inlineNamed =
    ("(?i)-fx-[\\w-]+\\s*:\\s*[^;\"]*?(?<![\\w#.$-])(?:" + namedAlternatives + ")(?![\\w(-])").r

  // A Scala string passed as a node selector: `lookup("#add")`.
  private val selectorCall =
    "(?:lookup|lookupAll|setId|querySelector|querySelectorAll)\\s*\\(\\s*\"[^\"\\n]*$".r

  // ---------------------------------------------------------------------------
  // Blanking: keep offsets and newlines, drop what is not colour-bearing
  // ---------------------------------------------------------------------------

  private def blankRange(sb: StringBuilder, from: Int, until: Int): Unit =
    (from until until).foreach(i => if (sb(i) != '\n') sb(i) = ' ')

  /** Scala source with `//` and nested `/* */` comments blanked. String
    * literals are kept: that is where a colour literal lives.
    */
  def scalaWithoutComments(source: String): String = {
    val out = new StringBuilder(source)
    val n   = source.length
    var i   = 0
    while (i < n) {
      if (source.startsWith("\"\"\"", i)) {
        val close = source.indexOf("\"\"\"", i + 3)
        i = if (close < 0) n else close + 3
      } else if (source(i) == '"') {
        var j = i + 1
        while (j < n && source(j) != '"' && source(j) != '\n')
          j += (if (source(j) == '\\') 2 else 1)
        i = j + 1
      } else if (source(i) == '\'' && i + 2 < n && source(i + 2) == '\'') i += 3
      else if (source(i) == '\'' && i + 1 < n && source(i + 1) == '\\') {
        val close = source.indexOf('\'', i + 2)
        i = if (close < 0 || close - i > 8) i + 1 else close + 1
      } else if (source.startsWith("//", i)) {
        val end = { val e = source.indexOf('\n', i); if (e < 0) n else e }
        blankRange(out, i, end); i = end
      } else if (source.startsWith("/*", i)) {
        var j     = i + 2
        var depth = 1
        while (depth > 0 && j < n) {
          if (source.startsWith("/*", j)) { depth += 1; j += 2 }
          else if (source.startsWith("*/", j)) { depth -= 1; j += 2 }
          else j += 1
        }
        blankRange(out, i, j); i = j
      } else i += 1
    }
    out.toString
  }

  /** CSS with comments and everything outside `{ ... }` declaration blocks
    * blanked, so selectors (`#id`) are never read as colours.
    */
  def cssDeclarationsOnly(source: String): String = {
    val out     = new StringBuilder(source)
    val n       = source.length
    var i       = 0
    var inBlock = false
    while (i < n) {
      if (source.startsWith("/*", i)) {
        val close = source.indexOf("*/", i + 2)
        val end   = if (close < 0) n else close + 2
        blankRange(out, i, end); i = end
      } else {
        val c = source(i)
        if (c == '{') inBlock = true
        else if (c == '}') inBlock = false
        else if (!inBlock && c != '\n') out(i) = ' '
        i += 1
      }
    }
    out.toString
  }

  /** FXML with `<!-- -->` comments blanked; attribute values stay readable. */
  def xmlWithoutComments(source: String): String = {
    val out = new StringBuilder(source)
    var i   = source.indexOf("<!--")
    while (i >= 0) {
      val close = source.indexOf("-->", i + 4)
      val end   = if (close < 0) source.length else close + 3
      blankRange(out, i, end)
      i = source.indexOf("<!--", end)
    }
    out.toString
  }

  /** Every literal colour in one file's text. */
  def scanSource(fileName: String, source: String): Seq[Violation] = {
    val readable =
      if (fileName.endsWith(".scala")) scalaWithoutComments(source)
      else if (fileName.endsWith(".fxml")) xmlWithoutComments(source)
      else cssDeclarationsOnly(source)
    val scala               = fileName.endsWith(".scala")
    val css                 = fileName.endsWith(".css")
    def lineOf(offset: Int) = readable.take(offset).count(_ == '\n') + 1
    def lineBefore(at: Int) = readable.substring(readable.lastIndexOf('\n', at - 1) + 1, at)
    def selector(at: Int)   = scala && selectorCall.findFirstIn(lineBefore(at)).isDefined
    val hexes     = hexColour.findAllMatchIn(readable).filterNot(m => selector(m.start)).toList
    val scalaOnly = if (scala) Seq(colourInt, inlineNamed) else Nil
    val others    = (commonPatterns ++ scalaOnly).flatMap(_.findAllMatchIn(readable)).toList
    val named     = if (css) cssNamed.findAllMatchIn(cssValuesOnly(readable)).toList else Nil
    (hexes ++ others ++ named)
      .map(m => Violation(fileName, lineOf(m.start), m.matched))
      .toSeq
      .sortBy(v => (v.line, v.literal))
  }

  /** CSS declaration text with property names and quoted strings blanked,
    * leaving only the values after each `:`.
    */
  def cssValuesOnly(declarations: String): String = {
    val out     = new StringBuilder(declarations)
    var inValue = false
    var quote   = 0.toChar
    declarations.indices.foreach { i =>
      val c = declarations(i)
      if (quote != 0) { if (c == quote) quote = 0; if (c != '\n') out(i) = ' ' }
      else if (c == '"' || c == '\'') { quote = c; out(i) = ' ' }
      else if (c == ':') { inValue = true; out(i) = ' ' }
      else if (c == ';' || c == '{' || c == '}') { inValue = false; out(i) = ' ' }
      else if (!inValue && c != '\n') out(i) = ' '
    }
    out.toString
  }

  /** Every literal colour in the studio sources under `buildRoot`. */
  def scanTree(buildRoot: File): Seq[Violation] =
    roots.flatMap { root =>
      val dir = buildRoot / root
      if (!dir.exists) Nil
      else
        dir
          .**(new SimpleFileFilter(f => f.isFile))
          .get
          .map(f => (f, IO.relativize(buildRoot, f).getOrElse(f.getPath).replace('\\', '/')))
          .filter { case (_, rel) => inScope(rel) && !whitelisted(rel) }
          .sortBy(_._2)
          .flatMap { case (f, rel) => scanSource(rel, IO.read(f)) }
    }

  // ---------------------------------------------------------------------------
  // Self-test
  // ---------------------------------------------------------------------------

  /** Planted literals every rule must find, clean inputs it must pass, and the
    * scope and whitelist decisions. Empty when every rule still bites.
    */
  def selfTest: Seq[String] = {
    val failures                                  = Seq.newBuilder[String]
    def expect(cond: Boolean, what: String): Unit = if (!cond) failures += what
    def show(s: String): String                   = s.replace('\n', ' ')

    def mustFind(file: String, planted: Seq[(String, String)]): Unit =
      planted.foreach { case (source, literal) =>
        val found = scanSource(file, source)
        expect(
          found.exists(_.literal == literal),
          s"missed '$literal' in $file: ${show(source)} (found ${found.map(_.literal)})"
        )
      }
    def mustPass(file: String, clean: Seq[String]): Unit =
      clean.foreach { source =>
        val found = scanSource(file, source)
        expect(found.isEmpty, s"flagged clean $file: ${show(source)} ($found)")
      }

    mustFind(
      "Planted.scala",
      Seq(
        "val c = \"#ECE9E2\""                              -> "#ECE9E2",
        "val c = \"#fff\""                                 -> "#fff",
        "val c = \"#1C1E2159\""                            -> "#1C1E2159",
        "node.setStyle(\"-fx-fill: #0E6E68;\")"            -> "#0E6E68",
        "node.setStyle(s\"-fx-fill: #0e6e68; $x\")"        -> "#0e6e68",
        "val s = \"-fx-text-fill: rgba(28,30,33,.35)\""    -> "rgba(",
        "val s = \"-fx-background-color: rgb(1, 2, 3)\""   -> "rgb(",
        "val s = \"hsl(120, 50%, 50%)\""                   -> "hsl(",
        "val p = Color.web(\"white\")"                     -> "Color.web(",
        "val p = Color.rgb(28, 30, 33)"                    -> "Color.rgb(",
        "val p = Color .color(0.1, 0.2, 0.3)"              -> "Color .color(",
        "val p = javafx.scene.paint.Color.hsb(1, 1, 1)"    -> "Color.hsb(",
        "val t = \"\"\"a\n  -fx-fill: #ABCDEF;\"\"\""      -> "#ABCDEF",
        "val c = Colour.srgb(0x1c1e21)"                    -> "Colour.srgb(",
        "val c = Colour.srgba(0x1c1e21, 35)"               -> "Colour.srgba(",
        "val argb = 0xFF1C1E21"                            -> "0xFF1C1E21",
        "val rgb = 0x1c1e21"                               -> "0x1c1e21",
        "val p = new Color(0.1, 0.2, 0.3, 1.0)"            -> "new Color(",
        "val p = new javafx.scene.paint.Color(0, 0, 0, 1)" -> "new javafx.scene.paint.Color(",
        "val p = Color.RED"                                -> "Color.RED",
        "val p = Color.DARKSLATEGRAY"                      -> "Color.DARKSLATEGRAY",
        "val p = Paint.valueOf(\"x\")"                     -> "Paint.valueOf(",
        "node.setStyle(\"-fx-text-fill: white;\")"         -> "-fx-text-fill: white",
        "node.lookup(\"#add\").setStyle(\"-fx-fill: #ECE9E2\")" -> "#ECE9E2"
      )
    )
    mustFind(
      "planted.css",
      Seq(
        ".root { -fx-background-color: #ECE9E2; }"                           -> "#ECE9E2",
        ".chip {\n  -fx-text-fill: #1c1e21;\n}"                              -> "#1c1e21",
        ".x { -fx-fill: rgba(0, 0, 0, 0.5); }"                               -> "rgba(",
        "#id .x { -fx-border-color: #abc #abc; }"                            -> "#abc",
        ".x { -fx-effect: dropshadow(gaussian, hsb(0,0%,0%), 2, 0, 0, 0); }" -> "hsb(",
        ".x { -fx-text-fill: white; }"                                       -> "white",
        ".x { -fx-border-color: -es-ink DarkSlateGray; }"                    -> "DarkSlateGray",
        ".x:hover {\n  -fx-background-color: -es-surface, red;\n}"           -> "red"
      )
    )
    mustFind(
      "planted.fxml",
      Seq(
        "<Label style=\"-fx-text-fill: #1C1E21;\"/>"           -> "#1C1E21",
        "<Region style=\"-fx-background-color: rgb(1,2,3)\"/>" -> "rgb("
      )
    )
    mustPass(
      "clean.fxml",
      Seq("<!-- <Label textFill=\"#FFFFFF\"/> --><Label styleClass=\"chip\"/>")
    )
    val lines = scanSource("Planted.scala", "package p\n\n// #FFFFFF\nval c = \"#FFFFFF\"\n")
    expect(lines.map(_.line) == Seq(4), s"misreported a line: $lines")

    mustPass(
      "Clean.scala",
      Seq(
        "// #ECE9E2 in a comment",
        "/* rgba(0,0,0,.5) /* nested #fff */ Color.web(\"x\") */ val x = 1",
        "/** See [[Seq#add]] and [[Foo#bead]]. */ val x = 1",
        "val q = '\\\"'; // #ECE9E2 after an escaped quote",
        "val issue = \"see issue #1234567\"",
        "val a = \"#face-id\"",
        "val t = Tokens.themed(theme, ThemedToken.Ink)",
        "val v = TokenCss.javaFxName(ref)",
        "def rgbToHex(x: Int) = x",
        "val m = myColor.rgb",
        "val fill = \"-fx-fill: -es-ink;\"",
        "val u = s\"$name#$id\"",
        "val c = '#'",
        "val n = node.lookup(\"#add\")",
        "val ns = scene.lookupAll(\"#fade .chip\")",
        "node.setId(\"#bead\")",
        "val p = Color.TRANSPARENT",
        "val mask = 0xff; val big = 0x1234567",
        "node.setStyle(\"-fx-font-weight: bold; -fx-fill: -es-ink;\")",
        "val t = Tokens.themed(Theme.Light, ThemedToken.Ink).webCss"
      )
    )
    mustPass(
      "clean.css",
      Seq(
        "#add { -fx-fill: -es-ink; }",
        "#fade, #bead .x { -fx-text-fill: -es-ink-2; }",
        "/* .root { -fx-fill: #ECE9E2; } */ .root { -fx-fill: -es-surface; }",
        ".x { -fx-background-color: -es-surface, -es-hairline; -fx-effect: none; }",
        ".x { -fx-background-color: transparent; -fx-border-color: TRANSPARENT; }",
        ".x { -fx-font-family: \"Tan Serif\"; -fx-alignment: center; }",
        ".x:hover { -fx-cursor: hand; -fx-fill: -es-tan; }",
        ".red, .white > .tan { -fx-fill: -es-ink; }"
      )
    )

    // Scope and whitelist.
    expect(
      inScope("studio/desktop/src/main/scala/eyes4s/studio/desktop/Shell.scala"),
      "desktop Scala out of scope"
    )
    expect(
      inScope("studio/desktop/src/main/resources/eyes4s/studio/desktop/shell.css"),
      "desktop CSS out of scope"
    )
    expect(
      inScope("studio/viz/src/main/scala/eyes4s/studio/viz/Scene.scala"),
      "viz Scala out of scope"
    )
    expect(
      !inScope("studio/desktop/src/test/scala/eyes4s/studio/desktop/X.scala"),
      "tests in scope"
    )
    expect(!inScope("studio/desktop/target/scala-3/X.scala"), "target in scope")
    expect(!inScope("kernel/src/main/scala/X.scala"), "library module in scope")
    expect(
      whitelisted("studio/app/src/main/scala/eyes4s/studio/app/tokens/Tokens.scala"),
      "token source not exempt"
    )
    expect(
      whitelisted("studio/desktop/src/main/resources/eyes4s/studio/desktop/studio-dark.css"),
      "generated CSS not exempt"
    )
    expect(
      whitelisted("studio/app/src/main/scala/eyes4s/studio/app/tokens/Colour.scala"),
      "colour type not exempt"
    )
    expect(
      !whitelisted("studio/app/src/main/scala/eyes4s/studio/app/tokens/TokenCss.scala"),
      "the rest of the token package exempt"
    )
    expect(
      !whitelisted("studio/desktop/src/main/resources/eyes4s/studio/desktop/stimulus/a.css"),
      "an undeclared directory exempt"
    )
    expect(
      !whitelisted("studio/desktop/src/main/resources/eyes4s/studio/desktop/shell.css"),
      "hand-written CSS exempt"
    )
    expect(
      !whitelisted("studio/app/src/main/scala/eyes4s/studio/app/tokensx/Other.scala"),
      "sibling of the token package exempt"
    )
    expect(
      !whitelisted("studio/desktop/src/main/resources/eyes4s/studio/desktop/studio.css.bak"),
      "prefix of a generated file exempt"
    )

    failures.result()
  }
}
