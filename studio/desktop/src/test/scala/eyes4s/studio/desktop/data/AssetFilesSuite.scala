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

package eyes4s.studio.desktop.data

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

/** What Repair… accepts as a display image (S5.7 review): one path segment,
  * a bounded size, and bytes that decode as an image; and the platform's
  * chooser behind the seam (S10.1 review): its dialog's title and filter, and
  * how it reads the chosen file. Headless.
  */
class AssetFilesSuite extends munit.FunSuite:

  private val png = IArray.unsafeFromArray(
    Files.readAllBytes(
      eyes4s.studio.desktop.trial.GoldenTrials.stimuli.resolve("beach-042.png")
    )
  )

  test("an image is accepted under its name") {
    assertEquals(
      AssetFiles.check("forest_044.png", png).map(_._1.value),
      Right("forest_044.png")
    )
  }

  test("text, a file over the bound, and a bad name are refused, each named") {
    val text = IArray.from("not an image".getBytes(UTF_8))
    assertEquals(AssetFiles.check("a.png", text), Left(AssetFileRefusal.NotAnImage("a.png")))
    assertEquals(
      AssetFiles.check("big.png", png, limit = 10),
      Left(AssetFileRefusal.TooLarge("big.png", png.length.toLong, 10))
    )
    assert(AssetFiles.check("a/b.png", png).left.exists {
      case AssetFileRefusal.BadName("a/b.png", _) => true
      case _                                      => false
    })
    assertEquals(AssetFiles.MaxBytes, 64L * 1024 * 1024)
  }

  test("a decoder that throws is a typed refusal naming the file, not a dead thread") {
    val throwing: Array[Byte] => Boolean = _ => throw IllegalStateException("bad GIF block")
    assertEquals(
      AssetFiles.check("odd.gif", png, decode = throwing),
      Left(AssetFileRefusal.Unreadable("odd.gif", "bad GIF block"))
    )
    val io: Array[Byte] => Boolean = _ => throw java.io.IOException("truncated")
    assertEquals(
      AssetFiles.check("odd.gif", png, decode = io),
      Left(AssetFileRefusal.NotAnImage("odd.gif"))
    )
  }

  test("the platform chooser: titled for the missing file, over images") {
    eyes4s.studio.desktop.harness.StudioFxSuite.startToolkit()
    val chooser = eyes4s.studio.desktop.harness.StudioFxSuite.runOnFx(
      AssetFiles.dialog(eyes4s.studio.core.assets.AssetFile.of("forest-044.png").toOption.get)
    )
    assertEquals(chooser.getTitle, "Locate forest-044.png")
    import scala.jdk.CollectionConverters.*
    assertEquals(
      chooser.getExtensionFilters.asScala.toVector.map(f =>
        f.getDescription -> f.getExtensions.asScala.toVector
      ),
      Vector("Images" -> Vector("*.png", "*.jpg", "*.jpeg", "*.bmp", "*.gif"))
    )
  }

  test(
    "the platform chooser reads the chosen file under its name; over the bound or unreadable, refused"
  ) {
    val dir = Files.createTempDirectory("asset-files")
    try
      val chosen = dir.resolve("forest_044_restored.png")
      Files.write(chosen, Array.from(png))
      assertEquals(
        AssetFiles.read(chosen.toFile).map(_.map((n, b) => (n.value, Vector.from(b)))),
        Right(Some(("forest_044_restored.png", Vector.from(png))))
      )
      assertEquals(
        AssetFiles.read(chosen.toFile, limit = 10),
        Left(AssetFileRefusal.TooLarge("forest_044_restored.png", png.length.toLong, 10))
      )
      // A directory is no file to read.
      val folder = Files.createDirectory(dir.resolve("folder.png"))
      assert(AssetFiles.read(folder.toFile).left.exists {
        case AssetFileRefusal.Unreadable("folder.png", _) => true
        case _                                            => false
      })
    finally eyes4s.studio.desktop.platform.TempDirs.remove(dir)
  }
