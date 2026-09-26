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

package eyes4s.studio.desktop.typography

import eyes4s.studio.app.tokens.*
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.harness.{SnapshotScale, StageSize, StudioFxSuite, StudioTheme}
import javafx.scene.control.Label
import javafx.scene.layout.VBox

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import java.security.MessageDigest

/** The bundled fonts load, match their recorded sources, and the type scale
  * reaches them through CSS (ticket S1.2).
  */
class FontLoadSuite extends StudioFxSuite:

  override protected def stageSize: StageSize = StageSize(480, 320)

  private def bytes(resource: String): Array[Byte] =
    val in = Option(getClass.getClassLoader.getResourceAsStream(resource))
      .getOrElse(fail(s"missing classpath resource $resource"))
    try in.readAllBytes()
    finally in.close()

  private def sha256(data: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(data).map(b => f"${b & 0xff}%02x").mkString

  test("Font.loadFont returns a font for every face, under the family the CSS names") {
    FontFace.values.foreach { face =>
      val font = StudioFonts.load(face).fold(p => fail(p.message), identity)
      assert(font != null, face)
      assertEquals(font.getFamily, face.javaFxFamily, face)
    }
    assertEquals(StudioFonts.loadAll(), Nil)
  }

  test("a face that JavaFX names differently is reported, not used") {
    val face = FontFace.SansMedium
    // The typographic family is what a web shell names; JavaFX reports the
    // legacy one, which is why FontFace records it.
    assertNotEquals(face.javaFxFamily, face.family.webName)
    assertEquals(
      FontLoadProblem.WrongFamily(face, "r", face.family.webName, face.javaFxFamily).message,
      s"font SansMedium: r loaded as family 'IBM Plex Sans Medm', not 'IBM Plex Sans'; " +
        "CSS naming 'IBM Plex Sans' would not find it"
    )
  }

  test("every face matches the SHA-256 recorded in SOURCES.md") {
    val sources = String(bytes(StudioFonts.sourcesResource), UTF_8)
    FontFace.values.foreach { face =>
      val digest = sha256(bytes(StudioFonts.resource(face)))
      val row    = sources.linesIterator
        .find(_.startsWith(s"| `${face.fileName}` |"))
        .getOrElse(fail(s"SOURCES.md has no row for ${face.fileName}"))
      assert(row.contains(s"`$digest`"), s"${face.fileName}: sha256 $digest, SOURCES.md: $row")
      assert(row.contains(s"| ${face.weight.css} |"), row)
    }
  }

  test("the SIL Open Font License ships with every family") {
    TypeFamily.values.foreach { family =>
      val text = String(bytes(StudioFonts.licenceResource(family)), UTF_8)
      assert(text.contains("SIL Open Font License, Version 1.1"), family)
      assert(text.contains("PERMISSION & CONDITIONS"), family)
    }
  }

  test("the type-scale lint allows exactly the TypeSize scale") {
    val buildRoot = Paths.get(String(bytes("eyes4s/studio/desktop/build-root.txt"), UTF_8).trim)
    val lint      =
      String(Files.readAllBytes(buildRoot.resolve("project/TypeScaleLint.scala")), UTF_8)
    val declared = "val allowedPx: Seq\\[Int\\] = Seq\\(([0-9, ]+)\\)".r
      .findFirstMatchIn(lint)
      .getOrElse(fail("TypeScaleLint.allowedPx not found"))
      .group(1)
      .split(",")
      .map(_.trim.toInt)
      .toList
    assertEquals(declared, TypeSize.values.toList.map(_.px))
  }

  fxStage.test("the type classes select the bundled faces at the five sizes") { fx =>
    assertEquals(StudioFonts.loadAll(), Nil)
    def label(classes: String*): Label =
      val l = Label("Pane title 0.73")
      l.getStyleClass.addAll(classes*)
      l
    val cases: List[(List[String], FontFace, Int)] =
      List(
        (Nil, FontFace.SansRegular, 12),
        (List("t11"), FontFace.SansRegular, 11),
        (List("t13"), FontFace.SansSemiBold, 13),
        (List("t16"), FontFace.SansRegular, 16),
        (List("t28"), FontFace.SansMedium, 28),
        (List("mono"), FontFace.MonoRegular, 12),
        (List("mono", "t13"), FontFace.MonoMedium, 13),
        (List("mono", "t28"), FontFace.MonoMedium, 28),
        (List("serif"), FontFace.SerifRegular, 12),
        (List("serif", "t13"), FontFace.SerifSemiBold, 13)
      )
    val labels = fx.runOnFx(cases.map { case (classes, _, _) => label(classes*) })
    val sheets = StudioStyles.stylesheets(Theme.Light).fold(e => fail(e.message), identity)
    fx.runOnFx(fx.scene.getStylesheets.setAll(sheets*)): Unit
    fx.show(fx.runOnFx(VBox(4, labels*)))
    cases.zip(labels).foreach { case ((classes, face, px), l) =>
      val font = fx.runOnFx(l.getFont)
      assertEquals((font.getFamily, font.getSize), (face.javaFxFamily, px.toDouble), classes)
    }
    fx.snapshot(StudioTheme.Light, List(SnapshotScale.X2)): Unit
  }
