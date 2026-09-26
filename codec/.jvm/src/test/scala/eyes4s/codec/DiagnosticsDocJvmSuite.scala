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

import eyes4s.plan.*
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** docs/DIAGNOSTICS.md carries the code table generated from both catalogs
  * and their per-case samples, so the published table, which is also the list
  * of covered families, cannot drift. Set EYES4S_WRITE_DIAGNOSTICS_DOC=1 to
  * rewrite the generated section; the run then fails so the change is reviewed.
  */
class DiagnosticsDocJvmSuite extends munit.FunSuite:
  private val Begin    = "<!-- BEGIN GENERATED DIAGNOSTIC CODES -->"
  private val End      = "<!-- END GENERATED DIAGNOSTIC CODES -->"
  private val relative = Paths.get("docs/DIAGNOSTICS.md")
  private val all      = DiagnosticSamples.all ++ CodecDiagnosticSamples.all

  private val path: Path =
    Iterator
      .iterate(Paths.get(sys.props("user.dir")).toAbsolutePath.normalize)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve(relative))
      .find(Files.isRegularFile(_))
      .getOrElse(fail(s"docs/DIAGNOSTICS.md not found from ${sys.props("user.dir")}"))

  /** The generated section: one table per family, in catalog order. */
  private def generated: String =
    val families = all.map { family =>
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
    if sys.env
        .get("EYES4S_WRITE_DIAGNOSTICS_DOC")
        .orElse(sys.props.get("EYES4S_WRITE_DIAGNOSTICS_DOC"))
        .contains("1") && current != expected
    then
      Files.writeString(
        path,
        text.substring(0, start) + expected + text.substring(stop + End.length),
        StandardCharsets.UTF_8
      )
      fail("Rewrote the generated section of docs/DIAGNOSTICS.md; review it and rerun.")
    else assertEquals(current, expected)
  }

  test("the generated section documents every code of both catalogs exactly once") {
    val text    = Files.readString(path, StandardCharsets.UTF_8)
    val section = text.substring(text.indexOf(Begin), text.indexOf(End))
    val codes   = section.linesIterator
      .filter(_.startsWith("| `"))
      .map(_.split('`')(1))
      .toVector
    assertEquals(codes, CodecDiagnosticCatalog.all.flatMap(_.codes).map(_.render))
  }

  test("every error enum the document says is not cataloged is absent from the catalogs") {
    val text  = Files.readString(path, StandardCharsets.UTF_8)
    val start = text.indexOf("<!-- BEGIN NOT CATALOGED -->")
    val stop  = text.indexOf("<!-- END NOT CATALOGED -->")
    assert(start >= 0 && stop > start, "not-cataloged markers are missing")
    val excluded =
      "`([A-Za-z]+)`".r.findAllMatchIn(text.substring(start, stop)).map(_.group(1)).toSet
    val cataloged = all.map(_.enumName).toSet
    assert(excluded.nonEmpty)
    assertEquals(excluded.intersect(cataloged), Set.empty[String])
  }
