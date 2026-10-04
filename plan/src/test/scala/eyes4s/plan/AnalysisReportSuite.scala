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

import cats.data.NonEmptyVector
import eyes4s.core.*
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
    Diagnostic.of(StudyFinding.UnmatchedFocal[StudyKey, Px](k2, UnmatchedKind.Undetermined))

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
      AnalysisFinding.refused[PlanError, StudyKey](PlanError.EmptyScales(0)) ->
        (Severity.Blocker, FindingClass.InvalidSetting, Remedy.ReconcileMethodDescriptor),
      AnalysisFinding.DataDependent(cause, NonEmptyVector.one(k1)) ->
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
    val failing = AnalysisFinding.DataDependent(cause, NonEmptyVector.one(k1))
    assertEquals(failing.keys, Vector(k1, k2))
    val d = Diagnostic.of(failing)
    assertEquals(d.operand("underlying"), Some(Operand.Cause(cause)))
    assertEquals(d.operand("trials"), Some(Operand.Keys(Vector(k1))))
    assertEquals(d.subject.head, Locus.Trials(Vector(k1)))
    assertEquals(d.affectedTrials, Vector(k1, k2))
    assert(failing.message.contains(cause.code.render), failing.message)
    assert(failing.message.contains(k1.toString), failing.message)
    val refused = AnalysisFinding.refused[PlanError, StudyKey](PlanError.EmptyScales(0))
    assertEquals(
      Diagnostic.of(refused).operands,
      Vector(
        "underlying" -> Operand.Cause(refusal),
        "suggested"  -> Operand.Token("ReconcileMethodDescriptor")
      )
    )
    assertEquals(refused.keys, Vector.empty)
    assert(refused.message.contains(refusal.code.render), refused.message)
  }

  test("the report derives the artifact finding before the family's own") {
    val missing = report(None, AnalysisFinding.DataDependent(cause, NonEmptyVector.one(k1)))
    assertEquals(missing.findings.head, AnalysisFinding.MissingArtifact[StudyKey](expected))
    assertEquals(missing.availability, Availability.Unavailable)
    val mismatched = report(Some(other))
    assertEquals(
      mismatched.findings,
      Vector(AnalysisFinding.ArtifactMismatch[StudyKey](expected, other))
    )
    val ready =
      report(Some(expected), AnalysisFinding.DataDependent(cause, NonEmptyVector.one(k1)))
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
    val refused =
      report(
        Some(expected),
        AnalysisFinding.refused[PlanError, StudyKey](PlanError.EmptyScales(0))
      )
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

  test("a refusal's remedy follows its cause, as the dedicated families derive it") {
    val causes: Vector[(AnalysisFinding[StudyKey], Remedy)] = Vector(
      AnalysisFinding.refused[PlanError, StudyKey](PlanError.MissingAngularScale(1)) ->
        StudyFinding.Refused[StudyKey, Px](PlanError.MissingAngularScale(1)).remedy,
      AnalysisFinding.refused[PlanError, StudyKey](
        PlanError.MatchItemConflict(Vector(Vector("t")))
      ) ->
        StudyFinding
          .Refused[StudyKey, Px](PlanError.MatchItemConflict(Vector(Vector("t"))))
          .remedy,
      AnalysisFinding.refused[TemporalStudyError, StudyKey](
        TemporalStudyError.WindowNames(Vector.empty)
      ) ->
        TemporalFinding
          .Refused[StudyKey, Px](TemporalStudyError.WindowNames(Vector.empty))
          .remedy,
      AnalysisFinding.refused[RecordingPlanError, StudyKey](
        RecordingPlanError.MissingViewing(RecordingRef("r"))
      ) -> RecordingFinding
        .Refused(RecordingPlanError.MissingViewing(RecordingRef("r")))
        .remedy,
      AnalysisFinding.refused[StudyFinding[StudyKey, Px], StudyKey](
        StudyFinding.FrameMismatch(k1, GeometryError.NonFiniteSigma(1.5))
      ) -> Remedy.AlignFrame
    )
    causes.foreach { (finding, expected) =>
      assertEquals(finding.remedy, expected, finding)
      assertEquals(Diagnostic.of(finding).remedy, Some(expected), finding)
    }
    assertEquals(
      AnalysisFinding.refused[PlanError, StudyKey](PlanError.MissingAngularScale(1)).remedy,
      Remedy.ReviseScaleDeclaration
    )
    // A cause with no dedicated mapping takes the generic remedy only when asked.
    given Remedial[DescriptorError] = Remedial.fixed(Remedy.ReconcileMethodDescriptor)
    assertEquals(
      AnalysisFinding
        .refused[DescriptorError, StudyKey](DescriptorError.MissingMethod(DefinitionId.cosine))
        .remedy,
      Remedy.ReconcileMethodDescriptor
    )
  }

  test("a data-dependent finding names at least one trial, by type") {
    assert(
      compiletime.testing
        .typeCheckErrors("AnalysisFinding.DataDependent(cause, Vector.empty[StudyKey])")
        .nonEmpty
    )
    assertEquals(
      compiletime.testing.typeCheckErrors(
        "AnalysisFinding.DataDependent(cause, NonEmptyVector.one(k1))"
      ),
      Nil
    )
  }

  test("each Remedial instance states the remedy of its error type directly") {
    assertEquals(
      summon[Remedial[PlanError]].remedy(PlanError.MissingAngularScale(1)),
      Remedy.ReviseScaleDeclaration
    )
    assertEquals(
      summon[Remedial[TemporalStudyError]].remedy(TemporalStudyError.WindowNames(Vector.empty)),
      Remedy.ReviseWindow
    )
    assertEquals(
      summon[Remedial[RecordingPlanError]]
        .remedy(RecordingPlanError.MissingViewing(RecordingRef("r"))),
      Remedy.ReviseDetectorParameters
    )
    assertEquals(
      summon[Remedial[StudyFinding[StudyKey, Px]]]
        .remedy(StudyFinding.UnmatchedFocal(k1, UnmatchedKind.Undetermined)),
      Remedy.SupplyMatchedReference
    )
    assertEquals(
      Remedial
        .fixed[DescriptorError](Remedy.AlignClock)
        .remedy(DescriptorError.MissingMethod(DefinitionId.cosine)),
      Remedy.AlignClock
    )
  }

  test("a refusal whose cause states no remedy does not compile") {
    val missing = compiletime.testing.typeCheckErrors(
      "AnalysisFinding.refused[DescriptorError, StudyKey](DescriptorError.MissingMethod(DefinitionId.cosine))"
    )
    assert(missing.exists(_.message.contains("Remedial")), missing.map(_.message))
    assertEquals(
      compiletime.testing.typeCheckErrors(
        """{
          given Remedial[DescriptorError] = Remedial.fixed(Remedy.ReconcileMethodDescriptor)
          AnalysisFinding.refused[DescriptorError, StudyKey](
            DescriptorError.MissingMethod(DefinitionId.cosine)
          )
        }"""
      ),
      Nil
    )
  }
