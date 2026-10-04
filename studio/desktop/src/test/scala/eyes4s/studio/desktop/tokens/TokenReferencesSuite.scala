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

import eyes4s.studio.app.tokens.{TokenCss, Tokens}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** Every `-es-*` looked-up colour a studio stylesheet names is a token, or
  * one a studio stylesheet declares (`-es-current`, the icons' currentColor)
  * (ticket S1.10): a misspelt name would only log "Could not resolve" at run
  * time and leave the control in modena's colours. Covers the hand-written
  * sheets (the modena mapping in studio-shell.css, studio-dock.css, the
  * panes') as well as the generated ones. No JavaFX.
  */
class TokenReferencesSuite extends munit.FunSuite:

  private lazy val root: Path =
    val in = getClass.getClassLoader.getResourceAsStream("eyes4s/studio/desktop/build-root.txt")
    try Paths.get(String(in.readAllBytes, UTF_8).trim)
    finally in.close()

  private lazy val sheets: Vector[Path] =
    Files
      .list(root.resolve(s"studio/desktop/src/main/resources/${TokenFiles.resourceDirectory}"))
      .iterator
      .asScala
      .filter(_.getFileName.toString.endsWith(".css"))
      .toVector
      .sortBy(_.getFileName.toString)

  private val reference = "-es-[a-z0-9-]+".r

  /** The `-es-*` names `css` uses, comments excluded. */
  def references(css: String): Set[String] =
    reference.findAllIn(css.replaceAll("(?s)/\\*.*?\\*/", "")).toSet

  private val tokens: Set[String] = Tokens.all.map(TokenCss.javaFxName).toSet

  private val declaration = "(-es-[a-z0-9-]+)\\s*:".r

  /** The `-es-*` names `css` declares itself. */
  def declared(css: String): Set[String] =
    declaration.findAllMatchIn(css.replaceAll("(?s)/\\*.*?\\*/", "")).map(_.group(1)).toSet

  test("every -es-* name in the studio's stylesheets is a token") {
    assert(sheets.exists(_.getFileName.toString == "studio-shell.css"), sheets)
    val texts = sheets.map(p => p -> String(Files.readAllBytes(p), UTF_8))
    val known = tokens ++ texts.flatMap((_, css) => declared(css))
    assertEquals(known.diff(tokens), Set("-es-current"))
    val unknown = texts.flatMap { (sheet, css) =>
      references(css)
        .diff(known)
        .toVector
        .sorted
        .map(name => s"${sheet.getFileName}: $name")
    }
    assertEquals(unknown, Vector.empty[String])
  }

  test("the check sees a misspelt name, and ignores one in a comment") {
    assertEquals(
      references("/* -es-nothing */ .x { -fx-text-fill: -es-inkx; }").diff(tokens),
      Set("-es-inkx")
    )
  }
