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
  * tables. The fixation table carries no item, which is a trial attribute in
  * `trials.csv`; until the importer passes trial attributes through (UI-H),
  * this reader joins it onto each record by the trial key.
  */
object StudioGolden:
  private def directory: Path =
    Iterator
      .iterate(Paths.get("").toAbsolutePath)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve("fixtures/studio-golden"))
      .find(Files.isDirectory(_))
      .getOrElse(throw new IllegalStateException("fixtures/studio-golden not found"))

  private def table(name: String): Vector[Vector[String]] =
    Rfc4180
      .decode(Files.readString(directory.resolve(name), StandardCharsets.UTF_8))
      .fold(e => throw new IllegalStateException(e.message), identity)

  lazy val source: StudioFixture.Source =
    val trials                                                          = table("trials.csv")
    val fixations                                                       = table("fixations.csv")
    val th                                                              = trials.head
    def at(row: Vector[String], header: Vector[String], column: String) =
      row(header.indexOf(column))
    val identity = Vector("participant", "phase", "trial", "occurrence")
    val items    = trials.tail
      .map(r => identity.map(at(r, th, _)) -> at(r, th, "item"))
      .toMap
    val fh = fixations.head
    StudioFixture.Source(
      Rfc4180.encode(
        (fh :+ "item") +: fixations.tail.map(r => r :+ items(identity.map(at(r, fh, _))))
      ),
      trials.tail.map(r => (at(r, th, "participant"), at(r, th, "trial")))
    )

class StudioGoldenJvmSuite extends StudioAcceptance(StudioGolden.source)
