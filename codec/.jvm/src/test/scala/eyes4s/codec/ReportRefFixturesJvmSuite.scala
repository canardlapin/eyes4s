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

package eyes4s.codec

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest

/** The pinned report-reference document is exactly the writer's output and
  * the portable mirror, byte for byte, with an independently known digest.
  */
class ReportRefFixturesJvmSuite extends munit.FunSuite:
  private val path: Path =
    Iterator
      .iterate(Paths.get(sys.props("user.dir")).toAbsolutePath.normalize)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve("codec/src/test/resources/values").resolve(ReportRefFixtures.file))
      .find(Files.isRegularFile(_))
      .getOrElse(fail(s"${ReportRefFixtures.file} not found"))

  test("the pinned report-reference document is the writer's output and the mirror") {
    val text = Files.readString(path, StandardCharsets.UTF_8)
    assertEquals(text, ReportRefFixtures.document.spaces2)
    assertEquals(text, ReportRefFixtures.versionOne)
  }

  test("the pinned report-reference document has its independently known digest") {
    // Computed outside eyes4s with `shasum -a 256`.
    val digest = MessageDigest
      .getInstance("SHA-256")
      .digest(Files.readAllBytes(path))
      .map(b => f"${b & 0xff}%02x")
      .mkString
    assertEquals(digest, "01760dea6ed747886cb3ea21371b8dee3336c15ab8ac9e88c7a10510da637298")
  }
