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

package eyes4s.plan

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** docs/DIAGNOSTICS.md carries the code table generated from the catalog and
  * the per-case samples, so the published table cannot drift. Set
  * EYES4S_WRITE_DIAGNOSTICS_DOC=1 to rewrite the generated section.
  */
class DiagnosticsDocJvmSuite extends munit.FunSuite:
  private val Begin    = "<!-- BEGIN GENERATED DIAGNOSTIC CODES -->"
  private val End      = "<!-- END GENERATED DIAGNOSTIC CODES -->"
  private val relative = Paths.get("docs/DIAGNOSTICS.md")

  private val path: Path =
    Iterator
      .iterate(Paths.get(sys.props("user.dir")).toAbsolutePath.normalize)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve(relative))
      .find(Files.isRegularFile(_))
      .getOrElse(fail(s"docs/DIAGNOSTICS.md not found from ${sys.props("user.dir")}"))

  /** The generated section: one table per family, in catalog order. */
  private def generated: String =
    val families = DiagnosticSamples.all.map { family =>
      val rows = family.labels.indices.map { ordinal =>
        val sample   = family.samples.find(_._1.ordinal == ordinal).map(_._1)
        val operands = sample.toVector
          .flatMap(_.productElementNames.toVector)
          .map(name => s"`$name`")
          .mkString(", ")
        s"| `${family.family.codes(ordinal).render}` | `${family.labels(ordinal)}` | $operands |"
      }
      (Vector(
        s"### `${family.family.name}` — `${family.enumName}`",
        "",
        "| Code | Case | Operands |",
        "|---|---|---|"
      ) ++ rows).mkString("\n")
    }
    families.mkString("\n\n")

  test("docs/DIAGNOSTICS.md contains exactly the generated code table") {
    val text  = Files.readString(path, StandardCharsets.UTF_8)
    val start = text.indexOf(Begin)
    val stop  = text.indexOf(End)
    assert(start >= 0 && stop > start, "generated-section markers are missing")
    val expected = s"$Begin\n\n$generated\n\n$End"
    val current  = text.substring(start, stop + End.length)
    if sys.env.get("EYES4S_WRITE_DIAGNOSTICS_DOC").contains("1") && current != expected then
      Files.writeString(
        path,
        text.substring(0, start) + expected + text.substring(stop + End.length),
        StandardCharsets.UTF_8
      )
      fail("Rewrote the generated section of docs/DIAGNOSTICS.md; review it and rerun.")
    else assertEquals(current, expected)
  }

  test("the generated section documents every catalog code exactly once") {
    val text    = Files.readString(path, StandardCharsets.UTF_8)
    val section = text.substring(text.indexOf(Begin), text.indexOf(End))
    val codes   = section.linesIterator
      .filter(_.startsWith("| `"))
      .map(_.split('`')(1))
      .toVector
    assertEquals(codes, DiagnosticCatalog.codes.map(_.render))
  }
