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
import eyes4s.design.*
import eyes4s.kernel.*

/** The code table is total, unique, pinned, and every projection keeps every
  * field of its case. Runs unchanged on the JVM and Scala.js, so the pinned
  * digest also proves the codes are identical on both platforms.
  */
class DiagnosticCatalogSuite extends munit.FunSuite:
  private val all = DiagnosticSamples.all

  /** Rendered codes, one per line, pinned by count and portable digest. */
  private val PinnedCount  = 607
  private val PinnedDigest = "1246a9552674785a"

  /** The table before CR5: codes are only ever added, never changed or
    * removed, so taking away the codes CR5 added leaves exactly this table.
    */
  private val StableCount  = 445
  private val StableDigest = "fb7ffbd3d3db7f63"

  /** The codes CR5 added: the families appended after `inspection`, and
    * preflight's finding for a trial the initial-fixation policy empties.
    */
  private val Cr5Codes: Set[String] =
    DiagnosticCatalog.families
      .dropWhile(_ ne DiagnosticCatalog.inspection)
      .drop(1)
      .flatMap(_.codes.map(_.render))
      .toSet + "study-finding.no-fixation-kept"

  test(
    "every cataloged family is sampled, in catalog order, through its own Diagnose instance"
  ) {
    assertEquals(all.map(_.family.name), DiagnosticCatalog.families.map(_.name))
    all.zip(DiagnosticCatalog.families).foreach { case (sampled, family) =>
      assert(sampled.family eq family, sampled.enumName)
    }
  }

  test("each family's labels are exactly the compiler's cases, in declaration order") {
    all.foreach { family =>
      assertEquals(family.family.labels, family.labels, family.enumName)
    }
  }

  test("every case of every family is sampled and projects to its own catalog code") {
    all.foreach { family =>
      assertEquals(
        family.samples.map(_._1.ordinal).sorted,
        family.labels.indices.toVector,
        s"${family.enumName} samples must cover every case exactly once"
      )
      family.samples.foreach { case (sample, diagnostic) =>
        assertEquals(diagnostic.code, family.family.codes(sample.ordinal), s"$sample")
        assertEquals(diagnostic.code.family, family.family.name)
        assertEquals(
          diagnostic.code.name,
          DiagnosticFamily.slug(family.labels(sample.ordinal)),
          s"$sample"
        )
      }
    }
  }

  test("codes are unique across the catalog and none is uncatalogued") {
    val codes = DiagnosticCatalog.codes.map(_.render)
    assertEquals(codes.distinct.size, codes.size)
    assert(codes.forall(!_.contains("uncatalogued")))
    all.flatMap(_.samples).foreach { case (_, d) =>
      def walk(value: Diagnostic[Any]): Unit =
        assert(DiagnosticCatalog.codes.contains(value.code), value.code.render)
        value.causes.foreach(walk)
      walk(d)
    }
  }

  test("every projection keeps every field under its own name, with aligned values") {
    assertEquals(alignment.problems, Vector.empty)
  }

  test("a wrapped error of an unsampled family, or carried as a token, is detected") {
    val partial = DiagnosticAlignment(all.filterNot(_.family eq DiagnosticCatalog.geometry))
    val raw     = StudyFailure.Frame(DiagnosticSamples.k1, GeometryError.NonFiniteSigma(1.5))
    assertEquals(
      partial.uncataloged(raw),
      Vector("eyes4s.kernel.GeometryError")
    )
    val projected = Diagnostic.of(raw)
    assert(partial.aligned(raw, projected).nonEmpty)
    assert(alignment.uncataloged(raw).isEmpty)
    val tokenised = projected.copy(operands =
      projected.operands.updated(1, "underlying" -> Operand.Token("NonFiniteSigma(1.5)"))
    )
    assert(aligned(raw, tokenised).nonEmpty)
  }

  test("the rendered code table is pinned and so identical on the JVM and Scala.js") {
    assertEquals(
      DiagnosticCatalog.estimate.codes.map(_.render),
      Vector(
        "estimate.frame-mismatch",
        "estimate.no-mass",
        "estimate.degenerate-bandwidth",
        "estimate.surface",
        "estimate.degenerate-axis-bandwidth",
        "estimate.kernel-support-overflow"
      )
    )
    val rendered = DiagnosticCatalog.codes.map(_.render)
    val stable   = rendered.filterNot(Cr5Codes)
    assertEquals(stable.size, StableCount)
    assertEquals(ContentHash.ofString(stable.mkString("\n")).render, StableDigest)
    assertEquals(rendered.size, PinnedCount)
    assertEquals(ContentHash.ofString(rendered.mkString("\n")).render, PinnedDigest)
    assertEquals(
      rendered.take(3),
      Vector("plan.invalid-definition", "plan.invalid-artifact", "plan.missing-artifact")
    )
    assertEquals(
      DiagnosticCatalog.studyFailure.codes.map(_.render).last,
      "study-failure.initial-fixations"
    )
    assertEquals(
      DiagnosticFamily.slug("SynchronizationTargetIsSource"),
      "synchronization-target-is-source"
    )
  }

  test("errors name their failing object: trial keys, pairs, records, windows and scales") {
    val k1      = DiagnosticSamples.k1
    val k2      = DiagnosticSamples.k2
    val failure = Diagnostic.of(
      StudyFailure.Comparison(k1, k2, eyes4s.compare.CompareError.ZeroNorm("cosine", 0, 1))
    )
    assertEquals(failure.subject, Vector(Locus.Pair(k1, k2)))
    assertEquals(failure.keys, Vector(k1, k2))
    val nested = Diagnostic.of(
      StudyResultError.Scale(
        2,
        StudyResultError.FailureKey(
          k1,
          StudyFailure.Frame(k2, eyes4s.kernel.GeometryError.NonFiniteSigma(0))
        )
      )
    )
    assertEquals(nested.subject, Vector(Locus.Scale(2), Locus.Trial(k1)))
    assertEquals(nested.causes.map(_.code.render), Vector("study-result.failure-key"))
    val finding = Diagnostic.of(
      TemporalFinding.Repetition[StudyKey, Unit2D.Px]("rep", StudyFinding.UnmatchedFocal(k1))
    )
    assertEquals(finding.subject, Vector(Locus.Repetition("rep"), Locus.Trial(k1)))
    assertEquals(finding.severity, DiagnosticSeverity.Warning)
    assertEquals(finding.remedy, Some(Remedy.SupplyMatchedReference))
    assertEquals(finding.category, Some(FindingClass.DataDependent))
    val blocker = Diagnostic.of(
      StudyFinding.Refused[StudyKey, Unit2D.Px](PlanError.MissingArtifact("0123456789abcdef"))
    )
    assertEquals(blocker.severity, DiagnosticSeverity.Error)
    assertEquals(blocker.subject, Vector(Locus.Artifact("0123456789abcdef")))
    val record = Diagnostic.of(AdmissionError.QuarantineAdmitted(4, 7))
    assertEquals(record.subject, Vector(Locus.Record(4)))
    val window = Diagnostic.of(TemporalStudyError.InvalidWindow("late", 10L, 5L))
    assertEquals(window.subject, Vector(Locus.Window("late")))
    val quarantine = Diagnostic.of(
      AdmissionReason.Quarantined(Vector(2, 3), QuarantineCause.Overlap(1, "a", "b"))
    )
    assertEquals(quarantine.subject, Vector(Locus.Records(Vector(2, 3)), Locus.Fixation(1)))
    val incompatible = Diagnostic.of(
      ContrastError.Incompatible[StudyKey](
        NonEmptyVector.of(
          ContrastCompatibilityError
            .Orientation(ReductionOrientation.ByLeft, ReductionOrientation.EdgesOnce),
          ContrastCompatibilityError.Policy(FailurePolicy.RequireAll, FailurePolicy.RequireAll)
        )
      )
    )
    assertEquals(
      incompatible.causes.map(_.code.render),
      Vector("contrast-compatibility.orientation", "contrast-compatibility.policy")
    )
  }

  test("codes and operands carry the identity; the default message is not part of it") {
    val a = Diagnostic.of(ReductionError.FailedScores(DiagnosticSamples.k1, 1, 2))
    assertEquals(a.code.render, "reduction.failed-scores")
    assertEquals(
      a.operands,
      Vector(
        "key"        -> Operand.Key(DiagnosticSamples.k1),
        "successful" -> Operand.Integer(BigInt(1)),
        "failed"     -> Operand.Integer(BigInt(2))
      )
    )
    val relabelled = a.copy(message = "Autre langue")
    assertEquals(relabelled.code, a.code)
    assertEquals(relabelled.operands, a.operands)
  }

  test("the alignment check kills swapped, dropped and mis-coded projection mutants") {
    val raw      = ReductionError.FailedScores(DiagnosticSamples.k1, 1, 2)
    val faithful = Diagnostic.of(raw)
    assertEquals(aligned(raw, faithful), Vector.empty)
    val swapped = faithful.copy(operands =
      faithful.operands
        .updated(1, "successful" -> Operand.Integer(BigInt(2)))
        .updated(2, "failed" -> Operand.Integer(BigInt(1)))
    )
    assert(aligned(raw, swapped).nonEmpty)
    val dropped = faithful.copy(operands = faithful.operands.dropRight(1))
    assert(aligned(raw, dropped).nonEmpty)
    val nestedRaw = ReductionError.MeanFailure(
      DiagnosticSamples.k1,
      eyes4s.design.ScoreMeanError.EmptyValues("value")
    )
    val nested = Diagnostic.of(nestedRaw)
    assertEquals(aligned(nestedRaw, nested), Vector.empty)
    val miscoded = nested.copy(operands =
      nested.operands.updated(
        1,
        "underlying" -> Operand.Cause(
          nested.causes.head.copy(code = DiagnosticCatalog.scoreMean.codes(1))
        )
      )
    )
    assert(aligned(nestedRaw, miscoded).nonEmpty)
  }

  test("a wrapper keeps every locus of the error it wraps, so the innermost object is named") {
    // A stored-value mismatch names the row it was stored in, not the trial
    // the misplaced failure names; these are the only exceptions.
    val exempt = Set("study-result.failure-key", "study-result.pair-failure")
    def check(d: Diagnostic[Any]): Unit =
      d.operands.foreach {
        case (name, Operand.Cause(inner)) =>
          if !exempt.contains(d.code.render) then
            assert(
              inner.subject.forall(d.subject.contains),
              s"${d.code} drops ${inner.subject} of $name"
            )
          check(inner)
        case (_, Operand.Causes(inner)) => inner.foreach(check)
        case _                          => ()
      }
    all.flatMap(_.samples).foreach((_, d) => check(d))
    val nested = Diagnostic.of(
      RecordingPlanError.Core(
        "preprocess",
        eyes4s.core.CoreError.OfRecording(eyes4s.core.RecordingError.NonMonotonic(3, 10L, 5L))
      )
    )
    assertEquals(nested.subject, Vector(Locus.Sample(3)))
    val quarantined = Diagnostic.of(
      AdmissionReason.Quarantined(Vector(4, 5), QuarantineCause.Overlap(1, "a", "b"))
    )
    assertEquals(quarantined.subject, Vector(Locus.Records(Vector(4, 5)), Locus.Fixation(1)))
  }

  test("samples give same-typed fields distinct values, so a swapped operand is detected") {
    val repeating = all.flatMap(_.samples).collect {
      case (sample, _) if {
            // Scala.js cannot tell an integral Double from an Int at run
            // time, so both are compared as one numeric kind everywhere.
            val values = sample.productIterator.toVector.collect {
              case v: Int      => "Number"   -> v.toDouble.toString
              case v: Double   => "Number"   -> v.toString
              case v: Long     => "Long"     -> v.toString
              case v: String   => "String"   -> v
              case v: StudyKey => "StudyKey" -> v.toString
            }
            values.distinct.size != values.size
          } =>
        sample.toString
    }
    assertEquals(repeating, Vector.empty)
  }

  test("non-finite operands compare exactly: NaN equals NaN, -0.0 differs from 0.0") {
    val nan = Diagnostic.of(ScoreMeanError.NonFiniteValue("value", 2, Double.NaN))
    assertEquals(nan, Diagnostic.of(ScoreMeanError.NonFiniteValue("value", 2, Double.NaN)))
    assertEquals(Vector(nan, nan).distinct.size, 1)
    assertNotEquals(
      Diagnostic.of(DifferenceError.NonFiniteOperands("value", 0.0, Double.NaN)),
      Diagnostic.of(DifferenceError.NonFiniteOperands("value", -0.0, Double.NaN))
    )
    val range = Diagnostic.of(
      DescriptorError.InvalidComponent(
        "value",
        "x",
        eyes4s.compare.MeasureScale.Bounded(0.12345, 0.9)
      )
    )
    assertEquals(
      range.operand("range"),
      Some(
        Operand.Fields(
          Vector(
            "kind" -> Operand.Token("Bounded"),
            "lo"   -> Operand.Real(ExactDouble(0.12345)),
            "hi"   -> Operand.Real(ExactDouble(0.9))
          )
        )
      )
    )
  }

  test("every finding's diagnostic names exactly its typed affected trials") {
    val findings = all.filter(f => Set("study-finding", "temporal-finding")(f.family.name))
    assert(findings.size == 2)
    findings.flatMap(_.samples).foreach { (sample, diagnostic) =>
      val keys = sample match
        case f: StudyFinding[?, ?]    => f.keys
        case f: TemporalFinding[?, ?] => f.keys
        case other                    => fail(s"not a finding: $other")
      assertEquals(diagnostic.affectedTrials, keys.distinct, s"$sample")
    }
    val k1                                = DiagnosticSamples.k1
    val k2                                = DiagnosticSamples.k2
    val cardinality: Diagnostic[StudyKey] = Diagnostic.of(
      StudyFinding
        .MatchedCardinality[StudyKey, Unit2D.Px](k1, Vector(k2), MatchedReferences.RequireOne)
    )
    assertEquals(cardinality.affectedTrials, Vector(k1, k2))
  }

  test("a preflight refusal keeps its blockers' typed keys") {
    val k1       = DiagnosticSamples.k1
    val k2       = DiagnosticSamples.k2
    val blockers = Vector[PreflightFinding[StudyKey]](
      StudyFinding.UnmatchedFocalRefused[StudyKey, Unit2D.Px](k1),
      StudyFinding.MatchItemConflict[StudyKey, Unit2D.Px](Vector(k2, k1)),
      RecordingFinding.MissingViewing(eyes4s.core.RecordingRef("r"))
    )
    val refusal = PreflightError.NotReady(RecipeFamily.FixationStudy, blockers)
    assertEquals(refusal.affectedTrials, Vector(k1, k2))
    val diagnostic: Diagnostic[StudyKey] = Diagnostic.of(refusal)
    assertEquals(diagnostic.code.render, "preflight.not-ready")
    assertEquals(diagnostic.affectedTrials, Vector(k1, k2))
    assertEquals(
      diagnostic.causes.map(_.code.render),
      Vector(
        "study-finding.unmatched-focal-refused",
        "study-finding.match-item-conflict",
        "recording-finding.missing-viewing"
      )
    )
    assertEquals(
      Diagnostic.of(PreflightError.Refused(PlanError.EmptyScales(0))).affectedTrials,
      Vector.empty
    )
    assertEquals(PreflightError.Refused(PlanError.EmptyScales(0)).affectedTrials, Vector.empty)
  }

  test("every diagnostic is reported by EyesCore; a host reports its own checks") {
    all.flatMap(_.samples).foreach((_, d) => assertEquals(d.source, DiagnosticSource.EyesCore))
    val code = DiagnosticCode
      .host("studio-check", "matched-cardinality")
      .getOrElse(fail("a kebab-case code"))
    assertEquals(code.render, "studio-check.matched-cardinality")
    val check = Diagnostic.host[StudyKey](
      code,
      DiagnosticSeverity.Error,
      Vector(Locus.Trial(DiagnosticSamples.k1)),
      Vector("references" -> Operand.Keys(Vector(DiagnosticSamples.k2))),
      "Choose an occurrence."
    )
    assertEquals(check.source, DiagnosticSource.Host)
    assertEquals(check.affectedTrials, Vector(DiagnosticSamples.k1, DiagnosticSamples.k2))
    assertEquals(
      DiagnosticCode.host("Studio", "x"),
      Left(DiagnosticCodeError.InvalidFamily("Studio"))
    )
    assertEquals(
      DiagnosticCode.host("studio", "x y"),
      Left(DiagnosticCodeError.InvalidName("x y"))
    )
  }

  test("keys map through subject, operands, causes and source links") {
    val k1     = DiagnosticSamples.k1
    val k2     = DiagnosticSamples.k2
    val linked = Diagnostic
      .of(StudyFailure.Frame(k1, GeometryError.NonFiniteSigma(1.5)))
      .linked(
        Vector(
          SourceLink.Fixation(k2, 0),
          SourceLink.Missing(MissingSource.CollidingDigest("d", Vector(k1, k2)))
        )
      )
    val erased = linked.mapKeys(new ErasedKey(_))
    assertEquals(erased.keys, Vector(new ErasedKey(k1)))
    assertEquals(erased.narrow[StudyKey], Some(linked))
    assertEquals(erased.narrow[String], None)
    assertEquals(new ErasedKey(k1).narrow[StudyKey], Some(k1))
    assertEquals(new ErasedKey(k1).toString, k1.toString)
    assertEquals(new ErasedKey(k1).hashCode, k1.##)
    assertNotEquals(new ErasedKey(k1), new ErasedKey(k2))
    assertEquals(linked.mapKeys(_.participant).affectedTrials, Vector(k1.participant))
  }

  // ---------------------------------------------------------------- alignment

  private val alignment = DiagnosticAlignment(all)
  private def aligned(raw: scala.reflect.Enum, d: Diagnostic[Any]): Vector[String] =
    alignment.aligned(raw, d)
