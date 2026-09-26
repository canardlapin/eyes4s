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

package eyes4s.io

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** The same acceptance tests over the canonical `fixtures/studio-golden`
  * tables, read unchanged: the importer joins `trials.csv` to
  * `fixations.csv`, so no test code touches either table.
  */
object StudioGolden:
  private def directory: Path =
    Iterator
      .iterate(Paths.get("").toAbsolutePath)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve("fixtures/studio-golden"))
      .find(Files.isDirectory(_))
      .getOrElse(throw new IllegalStateException("fixtures/studio-golden not found"))

  private def file(name: String): String =
    Files.readString(directory.resolve(name), StandardCharsets.UTF_8)

  lazy val source: StudioFixture.Source =
    StudioFixture.Source(file("fixations.csv"), file("trials.csv"))

/** The golden README states 209 records in quarantined trials, so 11,311
  * admitted of 11,520 (`tools/studio-fixture/verify_counts.py` recomputes it).
  */
class StudioGoldenJvmSuite extends StudioAcceptance(StudioGolden.source):
  override protected def rejectedRecords: Option[Int] = Some(209)
