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

import eyes4s.plan.*
import io.circe.parser
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** The JVM-only Arrow family's samples, beside the portable ones. */
object ArrowDiagnosticSamples:
  import ArrowDiagnostics.given

  val all: Vector[FamilySamples] =
    Vector(DiagnosticSamples.generated[ArrowExportError]("ArrowExportError"))

/** Every public error enum of the shipped modules has a `Diagnose` instance.
  *
  * The enums are read from a fingerprint-checked compiler inventory. Audit runs
  * require their exact active candidate; ordinary tests use an explicitly prepared
  * local candidate or a committed inventory with valid provenance. Preparation is
  * not full audit qualification. The inventory covers the compiled public surface: every public enum whose name ends in `Error` or
  * `Failure`. The covered enums are the runtime classes of the sampled
  * families, each sampled through the instance `Diagnostic.of` finds, so an
  * enum without an instance, or an instance left out of the samples, fails
  * here. The same inventory shows that no public signature projects to
  * `Diagnostic[Any]`.
  */
class DiagnosticCoverageJvmSuite extends munit.FunSuite:
  // The evidence apparatus is test code: its enums are sampled by the catalog
  // suite but are not public API of a shipped module.
  private val all = (IoDiagnosticSamples.all ++ ArrowDiagnosticSamples.all)
    .filterNot(f => IoDiagnosticSamples.evidence(f.enumName))

  private val inventory: Map[String, Vector[String]] =
    val relative       = Paths.get("tools/api-audit/candidate.py")
    val selector: Path = Iterator
      .iterate(Paths.get(sys.props("user.dir")).toAbsolutePath.normalize)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve(relative))
      .find(Files.isRegularFile(_))
      .getOrElse(fail(s"$relative not found from ${sys.props("user.dir")}"))
    val process = new ProcessBuilder("python3", selector.toString)
      .redirectErrorStream(true)
      .start()
    val output =
      try new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8).trim
      finally process.getInputStream.close()
    assertEquals(process.waitFor(), 0, output)
    val path = Paths.get(output)
    val json = parser
      .parse(Files.readString(path, StandardCharsets.UTF_8))
      .fold(e => fail(s"inventory: $e"), identity)
    json.asObject
      .getOrElse(fail("inventory is not an object"))
      .toMap
      .map((key, row) => key -> row.asArray.toVector.flatten.map(_.asString.getOrElse("")))

  /** Public enums named as errors or failures, by fully qualified name. */
  private val errorEnums: Vector[String] =
    inventory.toVector
      .collect {
        case (key, row)
            if key
              .startsWith("class ") && row.lift(5).exists(_.endsWith("scala.reflect.Enum")) =>
          key.stripPrefix("class ")
      }
      .filter(name => name.endsWith("Error") || name.endsWith("Failure"))
      .sorted

  /** The enum each sampled family belongs to, from its runtime class. */
  private val covered: Set[String] =
    all.flatMap(_.samples.map(_._1.getClass.getName.takeWhile(_ != '$'))).toSet

  test("the inventory lists the public error enums of every shipped module") {
    assert(errorEnums.size > 100, errorEnums.size)
    Vector(
      "kernel",
      "core",
      "detect",
      "surface",
      "compare",
      "design",
      "plan",
      "results",
      "codec",
      "laws",
      "io"
    )
      .foreach(module =>
        assert(errorEnums.exists(_.startsWith(s"eyes4s.$module.")), s"no enum in $module")
      )
  }

  test("the retired EyeLink evidence apparatus is not public API of eyes4s-io") {
    val published = errorEnums.map(_.stripPrefix("eyes4s.io.")).toSet
    assertEquals(IoDiagnosticSamples.evidence.intersect(published), Set.empty[String])
  }

  test("every public error enum has a Diagnose instance and a sampled family") {
    assertEquals(errorEnums.filterNot(covered), Vector.empty)
  }

  test("every sampled family is a public enum of the inventory") {
    val enums = inventory.keySet.collect {
      case key if key.startsWith("class ") => key.stripPrefix("class ")
    }
    assertEquals(covered.filterNot(enums).toVector.sorted, Vector.empty)
  }

  test("no public signature projects to Diagnostic[Any]") {
    val offending = inventory.toVector.collect {
      case (key, row) if row.lift(5).exists(erasesKeys) => key
    }
    assertEquals(offending.sorted, Vector.empty)
    // The check itself: a keyed instance is found however deep its arguments.
    assert(erasesKeys("eyes4s.plan.Diagnose[eyes4s.plan.StudyFailure[K], scala.Any]"))
    assert(erasesKeys("(e: X): eyes4s.plan.Diagnostic[scala.Any]"))
    assert(!erasesKeys("eyes4s.plan.Diagnose[eyes4s.plan.StudyFailure[scala.Any], K]"))
  }

  /** Whether a signature names a diagnostic type whose key argument is `Any`:
    * the last top-level type argument of `Diagnostic`, `Diagnose`, `Locus`,
    * `Operand` or `SourceLink`.
    */
  private def erasesKeys(signature: String): Boolean =
    val heads = Vector("Diagnostic", "Diagnose", "Locus", "Operand", "SourceLink")
      .map(name => s"eyes4s.plan.$name[")
    heads.exists { head =>
      Iterator
        .iterate(signature.indexOf(head))(i => signature.indexOf(head, i + 1))
        .takeWhile(_ >= 0)
        .exists { at =>
          val start = at + head.length
          var depth = 1
          var index = start
          var last  = start
          while index < signature.length && depth > 0 do
            signature(index) match
              case '['               => depth += 1
              case ']'               => depth -= 1
              case ',' if depth == 1 => last = index + 1
              case _                 => ()
            index += 1
          signature.substring(last, index - 1).trim == "scala.Any"
        }
    }
