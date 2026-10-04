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

package eyes4s.laws

import eyes4s.kernel.UnitLabel
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** docs/METHOD_DESCRIPTORS.md carries the explicit bounds of every shipped
  * one-number field, generated from `RecipeParameters.forms`, so the
  * published table cannot drift from the fields. Set
  * `EYES4S_WRITE_DESCRIPTORS_DOC=1` to rewrite the generated section; the run
  * then fails so the change is reviewed.
  */
class MethodDescriptorsDocJvmSuite extends munit.FunSuite:
  private val Begin    = "<!-- BEGIN GENERATED FIELD BOUNDS -->"
  private val End      = "<!-- END GENERATED FIELD BOUNDS -->"
  private val relative = Paths.get("docs/METHOD_DESCRIPTORS.md")

  private val path: Path =
    Iterator
      .iterate(Paths.get(sys.props("user.dir")).toAbsolutePath.normalize)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve(relative))
      .find(Files.isRegularFile(_))
      .getOrElse(fail(s"$relative not found from ${sys.props("user.dir")}"))

  private val F = RecipeParameters.forms

  /** Every shipped one-number field, found by reflection over
    * `RecipeParameters.forms` so a new field cannot be left out: each public
    * accessor returning a [[NumericField]], and each unit-generic one in both
    * pixels and degrees. Sorted by id, then unit.
    */
  private val fields: Vector[FieldView] =
    val units  = Vector[UnitLabel[?]](summon[UnitLabel[Px]], summon[UnitLabel[Deg]])
    val fields = F.getClass.getMethods.toVector
      .filter(m => classOf[NumericField[?, ?, ?]].isAssignableFrom(m.getReturnType))
      .flatMap { m =>
        m.getParameterTypes.toVector match
          case Vector()                                => Vector(m.invoke(F))
          case Vector(t) if t == classOf[UnitLabel[?]] => units.map(m.invoke(F, _))
          case other => fail(s"forms.${m.getName} takes $other; extend this suite")
      }
      .map(_.asInstanceOf[NumericField[?, ?, ?]].view)
    fields.sortBy(v => (v.id.value, v.kind.toString))

  test("the table covers every shipped numeric field") {
    val ids = fields.map(_.id.value).toSet
    // The accessors added before CR6c; reflection must find at least these.
    val known = Set(
      "sigma",
      "sigmaX",
      "sigmaY",
      "residualLimitMicros",
      "thresholdDegPerSecond",
      "minimumDurationMicros",
      "extentWidthDeg",
      "extentHeightDeg",
      "etaXDegPerSecond",
      "etaYDegPerSecond",
      "minimumSamples",
      "interpolationGapMicros"
    )
    assert(known.subsetOf(ids), known -- ids)
    assertEquals(fields.count(_.id.value == "sigma"), 2)
  }

  private def row(view: FieldView): String = view.kind match
    case FieldKind.Numeric(q, shape, bounds) =>
      val unit = if q.symbol.isEmpty then "—" else s"`${q.symbol}`"
      s"| `${view.id}` | $unit | `$shape` | `${bounds.render}` |"
    case other => fail(s"${view.id} is not numeric: $other")

  private def generated: String =
    (Vector("| Field | Unit | Shape | Bounds |", "|---|---|---|---|") ++ fields.map(row))
      .mkString("\n")

  test("docs/METHOD_DESCRIPTORS.md contains exactly the generated bounds table") {
    val text  = Files.readString(path, StandardCharsets.UTF_8)
    val start = text.indexOf(Begin)
    val stop  = text.indexOf(End)
    assert(start >= 0 && stop > start, "generated-section markers are missing")
    val expected = s"$Begin\n\n$generated\n\n$End"
    val current  = text.substring(start, stop + End.length)
    if sys.env
        .get("EYES4S_WRITE_DESCRIPTORS_DOC")
        .orElse(sys.props.get("EYES4S_WRITE_DESCRIPTORS_DOC"))
        .contains("1") && current != expected
    then
      Files.writeString(
        path,
        text.substring(0, start) + expected + text.substring(stop + End.length),
        StandardCharsets.UTF_8
      )
      fail("Rewrote the generated section of docs/METHOD_DESCRIPTORS.md; review it and rerun.")
    else assertEquals(current, expected)
  }
