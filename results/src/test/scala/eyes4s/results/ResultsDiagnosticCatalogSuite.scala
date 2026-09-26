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

package eyes4s.results

import eyes4s.kernel.ContentHash
import eyes4s.plan.*

/** The results code table beside the plan's: total over the compiler's case
  * lists, one generated sample per case, every field kept, codes unique,
  * pinned on the JVM and Scala.js, and findings keyed to the trials,
  * participants and groups they concern.
  */
class ResultsDiagnosticCatalogSuite extends munit.FunSuite:
  import ResultsDiagnostics.given

  private val PinnedCount  = 50
  private val PinnedDigest = "5a453ed0d2d7f703"

  private val all       = DiagnosticSamples.all ++ ResultsDiagnosticSamples.all
  private val alignment = DiagnosticAlignment(
    all,
    ResultsDiagnosticSamples.structured,
    { case (_: StudyKey, Operand.Key(_)) => true }
  )

  test("every results family is sampled, in catalog order, through its own instance") {
    assertEquals(
      ResultsDiagnosticSamples.all.map(_.family.name),
      ResultsDiagnosticCatalog.families.map(_.name)
    )
    ResultsDiagnosticSamples.all.zip(ResultsDiagnosticCatalog.families).foreach {
      (sampled, family) => assert(sampled.family eq family, sampled.enumName)
    }
  }

  test("each family's labels are the compiler's cases, and every case projects its own code") {
    ResultsDiagnosticSamples.all.foreach { family =>
      assertEquals(family.family.labels, family.labels, family.enumName)
      assertEquals(
        family.samples.map(_._1.ordinal),
        family.labels.indices.toVector,
        family.enumName
      )
      family.samples.foreach { (sample, diagnostic) =>
        assertEquals(diagnostic.code, family.family.codes(sample.ordinal), s"$sample")
      }
    }
  }

  test("codes and family names are unique across the plan and results catalogs") {
    val families = all.map(_.family)
    val codes    = families.flatMap(_.codes).map(_.render)
    assertEquals(codes.distinct.size, codes.size)
    assertEquals(families.map(_.name).distinct.size, families.size)
  }

  test("every projection keeps every field, and every reachable error type is cataloged") {
    assertEquals(alignment.problems, Vector.empty)
  }

  test("the results code table is pinned and so identical on the JVM and Scala.js") {
    val rendered = ResultsDiagnosticCatalog.codes.map(_.render)
    assertEquals(rendered.size, PinnedCount)
    assertEquals(ContentHash.ofString(rendered.mkString("\n")).render, PinnedDigest)
    assertEquals(rendered.head, "report.empty-group")
  }

  test("report findings are warnings keyed to trials, participants and groups") {
    val key   = StudyKey("p1", "a", "recall")
    val group = GroupKey(Vector("covariate:memory" -> "Remembered"))
    val trial =
      Diagnostic.of(ReportFinding.UnknownPredicate(key, "window:outside-window-share"))
    assertEquals(trial.code.render, "report.unknown-predicate")
    assertEquals(trial.severity, DiagnosticSeverity.Warning)
    assertEquals(trial.subject, Vector(Locus.Trial(key)))
    assertEquals(trial.keys, Vector(key))
    val below = Diagnostic.of(ReportFinding.BelowMinimum("p2", group, Role.Difference, 1, 2))
    assertEquals(
      below.subject,
      Vector(Locus.Group(Vector("covariate:memory" -> "Remembered")), Locus.Participant("p2"))
    )
    assertEquals(
      below.operands.map(_._1),
      Vector("participant", "group", "role", "queries", "required")
    )
    val refused = Diagnostic.of(ReportError.StaleBinding("result", "sha256:a", "sha256:b"))
    assertEquals(refused.code.render, "report-error.stale-binding")
    assertEquals(refused.severity, DiagnosticSeverity.Error)
  }
