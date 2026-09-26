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

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

/** The generic finding and report that later recipe families share: the
  * artifact findings the report derives, severity, class and remedy by case,
  * and confirmation exactly as the dedicated reports confirm.
  */
class AnalysisReportSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)

  private type Input = StudyInput[StudyKey, Px]
  private val expected    = get(ArtifactRef.parse[Input]("0123456789abcdef"))
  private val other       = get(ArtifactRef.parse[Input]("fedcba9876543210"))
  private val k1          = StudyKey("p1", "a", "recall")
  private val k2          = StudyKey("p1", "b", "recall")
  private val description =
    Vector("scales" -> Vector[Provenance.Param](Provenance.Param.Num(1)))
  private val refusal: Diagnostic[StudyKey] = Diagnostic.of(PlanError.EmptyScales(0))
  private val cause: Diagnostic[StudyKey]   =
    Diagnostic.of(StudyFinding.UnmatchedFocal[StudyKey, Px](k2))

  private def report(
      available: Option[ArtifactRef[Input]],
      findings: AnalysisFinding[StudyKey]*
  ): AnalysisReport[StudyKey, Input] =
    AnalysisReport.of(
      RecipeFamily.FixationStudy,
      description,
      expected,
      available,
      findings.toVector,
      Vector(UncheckedAspect.PairComparison)
    )

  test("severity, class and remedy follow from the case") {
    val table = Vector(
      AnalysisFinding.MissingArtifact[StudyKey](expected) ->
        (Severity.Blocker, FindingClass.UnavailableInput, Remedy.SupplyReferencedArtifact),
      AnalysisFinding.ArtifactMismatch[StudyKey](expected, other) ->
        (Severity.Blocker, FindingClass.UnavailableInput, Remedy.RetargetPlanToAvailableInput),
      AnalysisFinding.Refused[StudyKey](refusal) ->
        (Severity.Blocker, FindingClass.InvalidSetting, Remedy.ReconcileMethodDescriptor),
      AnalysisFinding.DataDependent(cause, Vector(k1)) ->
        (Severity.Warning, FindingClass.DataDependent, Remedy.AcceptMissingObservation)
    )
    table.foreach { (finding, classification) =>
      val (severity, category, remedy) = classification
      assertEquals((finding.severity, finding.category, finding.remedy), classification)
      val d = Diagnostic.of(finding)
      assertEquals(d.code.family, "analysis-finding")
      assertEquals(d.category, Some(category))
      assertEquals(d.remedy, Some(remedy))
      val level =
        if severity == Severity.Blocker then DiagnosticSeverity.Error
        else DiagnosticSeverity.Warning
      assertEquals(d.severity, level)
    }
  }

  test("findings name their operands: artifacts, the carried cause and the failing trials") {
    val mismatch = Diagnostic.of(AnalysisFinding.ArtifactMismatch[StudyKey](expected, other))
    assertEquals(
      mismatch.operands,
      Vector(
        "expected" -> Operand.Artifact(expected.digest),
        "actual"   -> Operand.Artifact(other.digest)
      )
    )
    assertEquals(mismatch.subject, Vector(Locus.Artifact(expected.digest)))
    assert(mismatch.message.contains(expected.digest), mismatch.message)
    assert(mismatch.message.contains(other.digest), mismatch.message)
    val failing = AnalysisFinding.DataDependent(cause, Vector(k1))
    assertEquals(failing.keys, Vector(k1, k2))
    val d = Diagnostic.of(failing)
    assertEquals(d.operand("underlying"), Some(Operand.Cause(cause)))
    assertEquals(d.operand("trials"), Some(Operand.Keys(Vector(k1))))
    assertEquals(d.subject.head, Locus.Trials(Vector(k1)))
    assertEquals(d.affectedTrials, Vector(k1, k2))
    assert(failing.message.contains(cause.code.render), failing.message)
    assert(failing.message.contains(k1.toString), failing.message)
    val refused = AnalysisFinding.Refused[StudyKey](refusal)
    assertEquals(refused.keys, Vector.empty)
    assert(refused.message.contains(refusal.code.render), refused.message)
  }

  test("the report derives the artifact finding before the family's own") {
    val missing = report(None, AnalysisFinding.DataDependent(cause, Vector(k1)))
    assertEquals(missing.findings.head, AnalysisFinding.MissingArtifact[StudyKey](expected))
    assertEquals(missing.availability, Availability.Unavailable)
    val mismatched = report(Some(other))
    assertEquals(
      mismatched.findings,
      Vector(AnalysisFinding.ArtifactMismatch[StudyKey](expected, other))
    )
    val ready = report(Some(expected), AnalysisFinding.DataDependent(cause, Vector(k1)))
    assert(ready.ready)
    assertEquals(ready.warnings.size, 1)
    assertEquals(ready.affectedTrials, Vector(k1, k2))
    assertEquals(
      ready.diagnostics.map(_.code.render),
      Vector("analysis-finding.data-dependent")
    )
    assertEquals(ready.family, RecipeFamily.FixationStudy)
    assertEquals(ready.notChecked, Vector(UncheckedAspect.PairComparison))
  }

  test("confirm refuses a changed plan, a changed input and any blocker") {
    val ready = report(Some(expected))
    assertEquals(ready.confirm(description, expected), Right(()))
    val revised = Vector("scales" -> Vector[Provenance.Param](Provenance.Param.Num(2)))
    assertEquals(
      ready.confirm(revised, expected),
      Left(
        PreflightError.ChangedPlan(
          RecipeFamily.FixationStudy,
          PlanChange.between(description, revised)
        )
      )
    )
    assertEquals(
      ready.confirm(description, other),
      Left(PreflightError.ChangedInput(RecipeFamily.FixationStudy, expected, other))
    )
    val refused  = report(Some(expected), AnalysisFinding.Refused(refusal))
    val notReady = refused.confirm(description, expected)
    assertEquals(
      notReady,
      Left(PreflightError.NotReady(RecipeFamily.FixationStudy, refused.blockers))
    )
    assertEquals(
      notReady.left.toOption.map(Diagnostic.of(_).causes.map(_.code.render)),
      Some(Vector("analysis-finding.refused"))
    )
  }
