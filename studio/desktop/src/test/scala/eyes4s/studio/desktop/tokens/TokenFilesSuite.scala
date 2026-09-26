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

package eyes4s.studio.desktop.tokens

import eyes4s.studio.app.tokens.*
import javafx.css.{CssParser, Stylesheet}
import javafx.scene.paint.Color

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** The generated token files are current, load in JavaFX, and match the design
  * boards (ticket S1.1).
  */
class TokenFilesSuite extends munit.FunSuite:

  private def resourceText(name: String): String =
    val in = Option(getClass.getClassLoader.getResourceAsStream(name))
      .getOrElse(fail(s"missing classpath resource $name"))
    try String(in.readAllBytes(), UTF_8)
    finally in.close()

  // Written by the build (studioDesktop / Test / resourceGenerators).
  private lazy val buildRoot: Path =
    Paths.get(resourceText("eyes4s/studio/desktop/build-root.txt").trim)

  private def read(relative: String): String =
    String(Files.readAllBytes(buildRoot.resolve(relative)), UTF_8)

  // ---------------------------------------------------------------------------
  // The checked-in files are the generator's output
  // ---------------------------------------------------------------------------

  test("every generated token file is current") {
    val problems = TokenFiles.check(buildRoot)
    assert(problems.isEmpty, problems.map(_.message).mkString("\n", "\n", ""))
  }

  test("the check notices a missing or stale file") {
    val scratch = Files.createTempDirectory("eyes4s-token-files")
    try
      assertEquals(
        TokenFiles.check(scratch),
        TokenFiles.files.map(f => TokenFileProblem.Missing(f.relativePath))
      )
      assertEquals(TokenFiles.write(scratch), TokenFiles.files.map(_.relativePath))
      assertEquals(TokenFiles.check(scratch), Nil)
      assertEquals(TokenFiles.write(scratch), Nil)
      val web = TokenFiles.files.last.relativePath
      Files.write(scratch.resolve(web), (read(web) + "\n").getBytes(UTF_8))
      assertEquals(TokenFiles.check(scratch), List(TokenFileProblem.Stale(web)))
    finally Files.walk(scratch).iterator.asScala.toList.reverse.foreach(p => Files.delete(p))
  }

  test("the shipped stylesheets are on the classpath") {
    Theme.values.foreach { theme =>
      assertEquals(resourceText(TokenFiles.stylesheetResource(theme)), TokenCss.javaFx(theme))
    }
  }

  // ---------------------------------------------------------------------------
  // JavaFX reads every looked-up colour as the token's colour
  // ---------------------------------------------------------------------------

  private def parse(theme: Theme): Stylesheet =
    CssParser.errorsProperty.clear()
    val sheet  = CssParser().parse(TokenCss.javaFx(theme))
    val errors = CssParser.errorsProperty.asScala.toList.map(_.getMessage)
    assert(errors.isEmpty, s"$theme: ${errors.mkString("; ")}")
    sheet

  private def colours(sheet: Stylesheet, selector: String): Map[String, Color] =
    val rule = sheet.getRules.asScala
      .find(_.getSelectors.asScala.exists(_.toString == selector))
      .getOrElse(fail(s"no rule for $selector"))
    rule.getDeclarations.asScala.map { d =>
      d.getProperty -> (d.getParsedValue.convert(null) match
        case c: Color => c
        case other    => fail(s"${d.getProperty} parsed as $other, not a colour"))
    }.toMap

  private def fx(c: eyes4s.studio.app.tokens.Colour): Color =
    Color.rgb(c.red, c.green, c.blue, c.alphaPercent / 100.0)

  test("JavaFX parses each theme's looked-up colours to the token values") {
    Theme.values.foreach { theme =>
      val sheet    = parse(theme)
      val root     = colours(sheet, "*.root")
      val expected = Tokens.all.map { ref =>
        TokenCss.javaFxName(ref) -> fx(Tokens.resolve(ref, theme, Tokens.defaultStage))
      }.toMap
      assertEquals(root, expected, theme)
      StageVariant.values.foreach { stage =>
        assertEquals(
          colours(sheet, s"*.${TokenCss.stageStyleClass(stage)}"),
          StageToken.values.map { t =>
            TokenCss.javaFxName(TokenRef.Staged(t)) -> fx(Tokens.staged(stage, t))
          }.toMap,
          s"$theme $stage"
        )
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Board parity: docs/studio/design/Main.dc.html and System.dc.html
  // ---------------------------------------------------------------------------

  private def declarations(
      board: String,
      selector: String,
      first: String
  ): Map[String, String] =
    val pattern = (java.util.regex.Pattern.quote(selector) + "\\{(" +
      java.util.regex.Pattern.quote(first) + "[^}]*)\\}").r
    val body = pattern.findFirstMatchIn(board).getOrElse(fail(s"no $selector block")).group(1)
    body
      .split(";")
      .toList
      .filter(_.contains(':'))
      .map { d =>
        val i = d.indexOf(':')
        d.substring(0, i).trim.stripPrefix("--") -> d.substring(i + 1).trim
      }
      .toMap

  private def web(theme: Theme, stage: StageVariant): Map[String, String] =
    Tokens.all.map(ref => ref.cssName -> Tokens.resolve(ref, theme, stage).webCss).toMap +
      ("sel-ring" -> TokenCss.webSelectionRing(theme))

  // Where the token source departs from the boards, and why (see Tokens).
  private val deviations: Map[(String, String), String] = Map(
    ("mid", "on-stage") -> "white on #808080 is 3.95:1; captions need 4.5:1",
    ("light", "halo")   -> "a white halo on #F2F2F2 is 1.12:1"
  )

  test("tokens match the boards' token block, except the recorded deviations") {
    val main   = read("docs/studio/design/Main.dc.html")
    val system = read("docs/studio/design/System.dc.html")
    val light  = web(Theme.Light, StageVariant.Dark)
    val dark   = web(Theme.Dark, StageVariant.Dark)
    val boards = List(
      ("light", declarations(main, ".es", "--ground"), light),
      ("dark", declarations(main, ".es.dark", "--ground"), dark),
      ("palettes", declarations(system, ".es", "--ramp-0"), light)
    ) ++ List(StageVariant.Mid, StageVariant.Light).map { stage =>
      val v = stage.cssName
      (
        v,
        declarations(main, s"[data-stage=$v],.es[data-stage=$v]", "--stage"),
        web(Theme.Light, stage)
      )
    }
    val mismatches = for
      (block, board, ours) <- boards
      (name, value)        <- board.toList.sortBy(_._1)
      if !deviations.contains((block, name))
      if !ours.get(name).contains(value)
    yield s"$block --$name: board $value, tokens ${ours.getOrElse(name, "absent")}"
    assert(mismatches.isEmpty, mismatches.mkString("\n", "\n", ""))

    // Every token the boards do not name is a recorded addition.
    val named = boards.flatMap(_._2.keySet).toSet
    assertEquals(Tokens.all.map(_.cssName).filterNot(named), List("sel-ring-inner"))
    // The deviations are real: the boards still say otherwise.
    assertEquals(
      declarations(main, "[data-stage=mid],.es[data-stage=mid]", "--stage")("on-stage"),
      "#FFFFFF"
    )
    assertEquals(web(Theme.Light, StageVariant.Mid)("on-stage"), "#000000")
  }
