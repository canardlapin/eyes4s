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

package eyes4s.studio.app.data

import eyes4s.studio.core.document.SourceRole
import eyes4s.studio.core.platform.HostPath
import munit.FunSuite

class FirstRunImportSuite extends FunSuite:
  private def file(name: String, directory: Boolean = false) =
    FirstRunFile(HostPath.of("/drop/" + name).toOption.get, name, directory)

  test("both table roles and the stimulus folder survive the drop unchanged and in order") {
    val files   = Vector(file("TRIALS.TSV"), file("fixations.csv"), file("images", true))
    val request = FirstRunImport.dropped(files).toOption.get
    assertEquals(
      request.files,
      Vector(SourceRole.Trials -> files(0).path, SourceRole.Fixations -> files(1).path)
    )
    assertEquals(request.imageFolder, Some(files(2).path))
    assertEquals(request.choose, None)
  }

  test(
    "duplicate roles and unsupported extras refuse the whole drop with every offending name"
  ) {
    val duplicate = FirstRunImport.dropped(Vector(file("a.csv"), file("b.tsv")))
    assertEquals(
      duplicate,
      Left(FirstRunDropProblem.RepeatedRole(SourceRole.Fixations, Vector("a.csv", "b.tsv")))
    )
    val unsupported = FirstRunImport.dropped(
      Vector(file("fixations.csv"), file("notes.pdf"), file("photo.png"))
    )
    assertEquals(
      unsupported,
      Left(FirstRunDropProblem.Unsupported(Vector("notes.pdf", "photo.png")))
    )
  }

  test("folder-only drops are explicit; several folders and an empty drop are refused") {
    assertEquals(FirstRunImport.dropped(Vector.empty), Left(FirstRunDropProblem.Empty))
    assertEquals(
      FirstRunImport.dropped(Vector(file("one", true), file("two", true))),
      Left(FirstRunDropProblem.SeveralFolders(Vector("one", "two")))
    )
    val images = file("one", true)
    assertEquals(
      FirstRunImport.dropped(Vector(images)).toOption.get.imageFolder,
      Some(images.path)
    )
  }
