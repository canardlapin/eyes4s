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

package eyes4s.studio.desktop.platform

import eyes4s.studio.core.document.{ColumnBinding, ColumnName, ColumnRole, TimeUnit}
import eyes4s.studio.core.importing.{ImportPreset, PresetName}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

/** Import presets on disk (ticket S5.2): saved presets load back, any name is
  * a safe file name, and a file that is not a preset is reported, never
  * dropped silently.
  */
class FilePresetStoreSuite extends munit.FunSuite:

  private def preset(name: String): ImportPreset =
    ImportPreset
      .of(
        PresetName.of(name).fold(e => fail(e.message), identity),
        Vector(
          ColumnBinding(
            ColumnRole.Ordinal,
            ColumnName.of("FixNum").fold(e => fail(e.message), identity)
          )
        ),
        Some(TimeUnit.Milliseconds)
      )
      .fold(e => fail(e.message), identity)

  test("saved presets load back; a name with separators is a safe file name") {
    val dir = Files.createTempDirectory("eyes4s-presets")
    try
      val store = FilePresetStore(dir.resolve("presets"))
      assertEquals(store.load._1.all, Vector.empty)
      val odd = preset("../lab/export: v2")
      assertEquals(store.save(preset("b")), Right(()))
      assertEquals(store.save(odd), Right(()))
      val (loaded, problems) = store.load
      assertEquals(problems, Vector.empty)
      assertEquals(loaded.all.toSet, Set(preset("b"), odd))
      assert(TempDirs.files(dir.resolve("presets")).forall(!_.contains("/")))
    finally TempDirs.remove(dir)
  }

  test("a file that is not a preset is reported with its path") {
    val dir = Files.createTempDirectory("eyes4s-presets")
    try
      val store = FilePresetStore(dir)
      assertEquals(store.save(preset("a")), Right(()))
      Files.writeString(dir.resolve("zz.json"), "{\"format\":\"other\"}", UTF_8)
      val (loaded, problems) = store.load
      assertEquals(loaded.names.map(_.value), Vector("a"))
      assertEquals(problems.size, 1)
      assert(problems.head.contains("zz.json is not an import preset"), problems.head)
    finally TempDirs.remove(dir)
  }
