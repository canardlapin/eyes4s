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

import eyes4s.compare.*
import eyes4s.core.Weight
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.EstimateError
import io.circe.{ACursor, Json}
import scala.compiletime.testing.typeCheckErrors

/** Completed results, with every failure family a fixation study records,
  * survive the archive exactly; malformed archives are refused by operand.
  */
class ResultCodecSuite extends munit.FunSuite:
  import StudyResultFixtures.*

  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private val cosine                        = StudyResultCodecs.cosine[Px]
  private val layout                        = StudyKey.layout(DefinitionId.studyLayout)
  private def sameSimilarity(a: Similarity, b: Similarity): Boolean = a.value == b.value
  private def sameDifference(a: SignedDifference, b: SignedDifference): Boolean =
    a.value == b.value
  private def key(p: String, s: String, phase: String) = StudyKey(p, s, phase)

  /** Edit one JSON location by path; array steps are integers. */
  private def edit(json: Json)(path: String*)(f: Json => Json): Json =
    def go(cursor: ACursor, rest: List[String]): ACursor = rest match
      case Nil          => cursor.withFocus(f)
      case head :: tail =>
        head.toIntOption match
          case Some(index) => go(cursor.downN(index), tail)
          case None        => go(cursor.downField(head), tail)
    go(json.hcursor, path.toList).top.getOrElse(fail(s"no JSON at ${path.mkString("/")}"))

  private def scale(index: Int)(rest: String*): Seq[String] =
    Seq("value", "scales", index.toString) ++ rest
  private def scale0(rest: String*): Seq[String] = scale(0)(rest*)

  /** Change a number, keeping an integer spelled as one (the only spelling decoders accept). */
  private def number(f: Double => Double): Json => Json =
    json =>
      val n     = json.asNumber.getOrElse(fail(s"not a number: $json"))
      val value = f(n.toDouble)
      if n.toLong.exists(_.toString == n.toString) && value.isWhole then
        Json.fromLong(value.toLong)
      else Json.fromDoubleOrNull(value)

  test(
    "successes mixed with pair and scale failures round-trip with exact counts, keys and failures"
  ) {
    Vector(requireAll, successfulOnly).foreach { policy =>
      val result  = get(cosinePlan(mixed, policy).run(mixed))
      val json    = get(cosine.codec.encode(result))
      val decoded = get(cosine.codec.decode(json))
      assert(ResultEquivalence.same(result, decoded)(sameSimilarity, sameDifference), policy)
      assertEquals(get(cosine.codec.encode(decoded)), json)

      val binned   = decoded.scales(0)
      val failing  = key("p1", "b", "recall")
      val focal    = key("p1", "a", "recall")
      val doubled  = key("p2", "a", "recall")
      val contrast = get(binned.contrast)
      val matched  = binned.analyses.matchedSource
      assert(contrast.matched eq binned.analyses.matched)
      assertEquals(binned.excludedPhases, Vector(key("p2", "d", "practice")))
      assertEquals(
        binned.estimation.collect { case (k, Left(f)) => k -> f },
        Vector(
          failing -> StudyFailure.Frame(
            failing,
            GeometryError.FrameMismatch(frame.id, otherFrame.id)
          )
        )
      )
      // Pair rows keep both source keys and the typed failure of the operand that failed.
      assertEquals(
        matched.rows.map(r => (r.left, r.right, r.result.isRight)),
        Vector(
          (focal, key("p1", "a", "encode"), true),
          (failing, key("p1", "b", "encode"), false)
        )
      )
      assertEquals(
        matched.rows(1).result.left.toOption,
        Some(StudyFailure.Frame(failing, GeometryError.FrameMismatch(frame.id, otherFrame.id)))
      )
      assertEquals(matched.diagnostics.selectedPairCount, 2)
      assertEquals(matched.diagnostics.eligiblePairCount, 2L)
      assertEquals(
        matched.diagnostics.ambiguous,
        // Duplicate indices address the focal operand, not the input rows.
        Vector(PairingAmbiguity.DuplicateLeft[StudyKey, StudyKey](doubled, Vector(2, 3)))
      )
      assertEquals(matched.diagnostics.unmatchedLeft, Vector.empty)
      assertEquals(
        matched.diagnostics.unmatchedRight,
        Vector(key("p2", "a", "encode"), key("p2", "c", "encode"))
      )
      assertEquals(binned.analyses.controlSource.rows.size, 2)
      assertEquals(
        binned.analyses.controlSource.diagnostics.unmatchedRight,
        Vector(key("p2", "a", "encode"), key("p2", "c", "encode"))
      )
      // Reductions retain per-key denominators and the typed reason a key failed.
      val expectedFailure = policy match
        case FailurePolicy.RequireAll        => ReductionError.FailedScores(failing, 0, 1)
        case FailurePolicy.SuccessfulOnly(m) =>
          ReductionError.InsufficientSuccessful(failing, m.value, 0, 1)
      assertEquals(
        contrast.matched.entries.map(r =>
          (r.key, r.result.left.toOption, r.successful, r.failed, r.contributing)
        ),
        Vector(
          (focal, None, 1, 0, 1),
          (failing, Some(expectedFailure), 0, 1, 0),
          (doubled, Some(ReductionError.AmbiguousKey(doubled, Vector(2, 3))), 0, 0, 0)
        )
      )
      assertEquals(contrast.matched.diagnostics.successfulPairCount, 1)
      assertEquals(contrast.matched.diagnostics.failedPairCount, 1)
      assertEquals(contrast.matched.diagnostics.contributionCount, 2)
      assertEquals(contrast.matched.diagnostics.reducedKeyCount, 1)
      assertEquals(contrast.matched.diagnostics.failedKeys, Vector(failing, doubled))
      assertEquals(contrast.matched.diagnostics.policy, policy)
      // Contrast rows keep both operands and the typed row failure.
      assertEquals(contrast.rows.map(_.key), Vector(focal, failing, doubled))
      assert(contrast.rows(0).difference.isRight)
      assertEquals(
        contrast.rows(1).difference.left.toOption,
        Some(
          ContrastRowError.ReductionFailures(
            failing,
            Some(expectedFailure),
            Some(expectedFailure)
          )
        )
      )
      assertEquals(contrast.rows(1).matched.map(_.selected), Some(1))
      // The narrow Gaussian fails every trial at estimation; the contrast still exists per key.
      val narrow = decoded.scales(2)
      assert(narrow.estimation.forall {
        case (k, Left(StudyFailure.Estimation(l, EstimateError.DegenerateBandwidth(_, _)))) =>
          k == l
        case (k, Left(StudyFailure.Frame(l, _))) => k == l && k == failing
        case _                                   => false
      })
      assert(get(narrow.contrast).rows.forall(_.difference.isLeft))
      assertEquals(get(narrow.contrast).matched.diagnostics.successfulPairCount, 0)
      // The smooth Gaussian estimates every in-frame trial with the smoothing step on record.
      assertEquals(decoded.scales(1).estimation.count(_._2.isRight), 8)
      decoded.scales(1).estimation.collect { case (_, Right(mass)) => mass }.foreach { mass =>
        assertEquals(mass.provenance.steps, scales(1).provenanceSteps)
      }
    }
  }

  test("a clean study archives the same contrast the plan computed, provenance included") {
    val result  = get(cosinePlan(clean, requireAll, scales.take(2)).run(clean))
    val json    = get(cosine.codec.encode(result))
    val decoded = get(cosine.codec.decode(json))
    assert(ResultEquivalence.same(result, decoded)(sameSimilarity, sameDifference))
    assertEquals(result.scales.size, 2)
    result.scales.zip(decoded.scales).foreach { case (a, b) =>
      assertEquals(get(b.contrast).matched.provenance, get(a.contrast).matched.provenance)
      assertEquals(
        get(b.contrast).matched.provenance.digest,
        get(a.contrast).matched.provenance.digest
      )
      b.estimation.zip(a.estimation).foreach {
        case ((_, Right(x)), (_, Right(y))) =>
          assertEquals(x.provenance.digest, y.provenance.digest)
        case (x, y) => fail(s"$x versus $y")
      }
    }
  }

  test(
    "a two-component extension score round-trips; missing schema, wrong components, non-finite score and conflicting provenance are refused"
  ) {
    val result  = get(twoComponentPlan(mixed, requireAll).run(mixed))
    val json    = get(twoComponentCodec.codec.encode(result))
    val decoded = get(twoComponentCodec.codec.decode(json))
    assert(ResultEquivalence.same(result, decoded)(_ == _, _ == _))
    assertEquals(get(twoComponentCodec.codec.encode(decoded)), json)
    val spec =
      get(decoded.scales(0).analyses.matchedSource.evaluation.specification.toRight("no spec"))
    assertEquals(spec.components, Vector("similarity", "distance"))
    assertEquals(
      spec.parameters.map(_._1),
      Vector("estimate.estimator", "method.gain", "weight")
    )
    val difference = get(get(decoded.scales(0).contrast).rows(0).difference)
    assertEqualsDouble(difference.distance.value, -gain * difference.similarity.value, 1e-12)

    assertEquals(
      StudyResultRegistry.empty[StudyKey, Px].decode(json).left.toOption,
      Some(CodecError.MissingResultCodec(twoComponentId))
    )
    val registry =
      get(StudyResultRegistry.empty[StudyKey, Px].register(twoComponentCodec.registration))
    val loaded = get(registry.decode(json))
    assertEquals(get(loaded.encode), json)
    assertEquals(
      registry.register(twoComponentCodec.registration).left.toOption,
      Some(CodecError.DuplicateResultCodec(twoComponentId))
    )
    // An archive declaring another score schema is refused before any row is read.
    val wrongScore = edit(json)("value", "scoreSchema")(_ => Wire.id(DefinitionId.similarity))
    assertEquals(
      twoComponentCodec.codec.decode(wrongScore).left.toOption,
      Some(CodecError.Schema(twoComponentScores.schema, DefinitionId.similarity))
    )

    val oneComponent = edit(json)(
      scale0("analyses", "matched", "source", "evaluation", "specification", "components")*
    )(_ => Json.arr(Json.fromString("similarity")))
    assertEquals(
      twoComponentCodec.codec.decode(oneComponent).left.toOption,
      Some(
        CodecError.Entry(
          "scales[0].analyses.matched.source",
          CodecError.ScoreComponents(Vector("similarity", "distance"), Vector("similarity"))
        )
      )
    )

    val nonFinite = edit(json)(
      scale0("analyses", "matched", "source", "rows", "0", "result", "score", "similarity")*
    )(_ => Json.fromString("NaN"))
    twoComponentCodec.codec.decode(nonFinite) match
      case Left(
            CodecError.Entry(
              "scales[0].analyses.matched.source.rows[0]",
              CodecError.Field("similarity", _, _)
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")

    val conflicting = edit(json)(
      scale0("analyses", "matched", "source", "provenance", "steps", "1", "params")*
    )(params => Json.arr(params.asArray.get.dropRight(1)*))
    twoComponentCodec.codec.decode(conflicting) match
      case Left(
            CodecError.Entry(
              "scales[0].analyses.matched.source",
              CodecError.Reconstruction(
                ReconstructionError.ProvenanceConflict("evaluatePairs", declared, derived)
              )
            )
          ) =>
        assertEquals(declared.steps(1).params.size, derived.steps(1).params.size - 1)
      case other => fail(s"unexpected $other")

    // The method parameter tampered consistently in the specification and both provenances
    // still contradicts the plan description the archive carries.
    val gainTampered = Vector("matched", "control")
      .flatMap(design =>
        Vector(
          scale0(
            "analyses",
            design,
            "source",
            "evaluation",
            "specification",
            "parameters",
            "1",
            "value"
          ),
          scale0(
            "analyses",
            design,
            "source",
            "provenance",
            "steps",
            "3",
            "params",
            "1",
            "value"
          ),
          scale0("analyses", design, "provenance", "steps", "3", "params", "1", "value")
        )
      )
      .foldLeft(json)((j, path) => edit(j)(path*)(number(_ + 1.0)))
    twoComponentCodec.codec.decode(gainTampered) match
      case Left(
            CodecError.Result(
              StudyResultError.Scale(
                0,
                StudyResultError.SpecificationParameters(StudyDesign.Matched, expected, found)
              )
            )
          ) =>
        assertEquals(
          expected.collectFirst { case ("method.gain", v) => v },
          Some(Provenance.Param.Num(gain))
        )
        assertEquals(
          found.collectFirst { case ("method.gain", v) => v },
          Some(Provenance.Param.Num(gain + 1))
        )
      case other => fail(s"unexpected $other")
  }

  test(
    "malformed archives name their operands: impossible denominators, orphan pairs, dropped failure rows and a partial run tagged complete"
  ) {
    val result = get(cosinePlan(mixed, requireAll).run(mixed))
    val json   = get(cosine.codec.encode(result))
    val focal  = key("p1", "a", "recall")

    val denominator = edit(json)(
      scale0("analyses", "matched", "entries", "0", "contributing")*
    )(_ => Json.fromInt(5))
    assertEquals(
      cosine.codec.decode(denominator).left.toOption,
      Some(
        CodecError.Entry(
          "scales[0].analyses.matched.entries[0]",
          CodecError.Reconstruction(ReconstructionError.Denominator(focal, 1, 0, 5))
        )
      )
    )

    // Entries, report and provenance bumped together are consistent with each other
    // but not with the pair rows the reduction regroups.
    val inflated = Vector(
      scale0("analyses", "matched", "entries", "0", "successful"),
      scale0("analyses", "matched", "entries", "0", "contributing"),
      scale0("analyses", "matched", "diagnostics", "contributionCount"),
      scale0("analyses", "matched", "provenance", "steps", "6", "params", "2", "value")
    ).foldLeft(json)((j, path) => edit(j)(path*)(number(_ + 1.0)))
    assertEquals(
      cosine.codec.decode(inflated).left.toOption,
      Some(
        CodecError.Entry(
          "scales[0].analyses.matched",
          CodecError.Reconstruction(ReconstructionError.KeyDenominator(focal, 1, 0, 2, 0))
        )
      )
    )

    // A pair whose focal key no reduced row carries is caught when the reduction is regrouped.
    val stranger = get(StudyCodecs.key(DefinitionId.studyKey).encode(key("p9", "z", "recall")))
    val orphanPair =
      edit(json)(scale0("analyses", "matched", "source", "rows", "0", "left")*)(_ => stranger)
    assertEquals(
      cosine.codec.decode(orphanPair).left.toOption,
      Some(
        CodecError.Entry(
          "scales[0].analyses.matched",
          CodecError.Reconstruction(
            ReconstructionError.KeyDomain(
              Vector(
                key("p9", "z", "recall"),
                key("p1", "b", "recall"),
                key("p2", "a", "recall")
              ),
              Vector(focal, key("p1", "b", "recall"), key("p2", "a", "recall"))
            )
          )
        )
      )
    )
    // An excluded key that no trial was estimated for.
    val orphanKey = edit(json)(scale0("excludedPhases", "0")*)(_ => stranger)
    assertEquals(
      cosine.codec.decode(orphanKey).left.toOption,
      Some(
        CodecError.Entry(
          "scales[0]",
          CodecError.Result(StudyResultError.OrphanKey(key("p9", "z", "recall")))
        )
      )
    )
    // A pair analysis over a trial the scale never estimated.
    val scale0Result = result.scales.head
    assertEquals(
      StudyScaleResult
        .reconstruct(
          scale0Result.estimate,
          scale0Result.estimation.filterNot(_._1 == focal),
          scale0Result.excludedPhases,
          scale0Result.analyses,
          scale0Result.contrast
        )
        .left
        .toOption,
      Some(StudyResultError.OrphanPair(focal, key("p1", "a", "encode")))
    )

    val dropped = edit(json)(scale0("analyses", "matched", "source", "rows")*)(rows =>
      Json.arr(rows.asArray.get.take(1)*)
    )
    assertEquals(
      cosine.codec.decode(dropped).left.toOption,
      Some(
        CodecError.Entry(
          "scales[0].analyses.matched.source",
          CodecError.Reconstruction(ReconstructionError.RowCount(2, 1))
        )
      )
    )

    val partial = edit(json)("value", "scales")(scales => Json.arr(scales.asArray.get.take(1)*))
    assertEquals(
      cosine.codec.decode(partial).left.toOption,
      Some(CodecError.Result(StudyResultError.ScaleCount(3, 1)))
    )
    val empty = edit(json)("value", "scales")(_ => Json.arr())
    assertEquals(
      cosine.codec.decode(empty).left.toOption,
      Some(CodecError.Result(StudyResultError.ScaleCount(3, 0)))
    )

    val foreignGrid = edit(json)(scale0("estimation", "0", "outcome", "grid")*)(_ =>
      Json.fromString("elsewhere")
    )
    assertEquals(
      cosine.codec.decode(foreignGrid).left.toOption,
      Some(
        CodecError.Entry(
          "scales[0].estimation[0]",
          CodecError.MissingIdentity("grid", "elsewhere")
        )
      )
    )

    // A temporal failure carries its typed error; an unknown error kind is
    // refused at the estimation it sits in.
    val estimated = get(
      json.hcursor
        .downField("value")
        .downField("scales")
        .downN(0)
        .downField("estimation")
        .downN(0)
        .get[Json]("key")
    )
    val epochless = Json.obj("kind" -> Json.fromString("epochless"))
    val temporal  = edit(json)(scale0("estimation", "0", "outcome")*)(_ =>
      Json.obj(
        "kind"    -> Json.fromString("failure"),
        "failure" -> Json.obj(
          "kind"  -> Json.fromString("temporal"),
          "key"   -> estimated,
          "error" -> epochless
        )
      )
    )
    assertEquals(
      cosine.codec.decode(temporal).left.toOption,
      Some(
        CodecError.Entry(
          "scales[0].estimation[0]",
          CodecError.Field("kind", epochless, "unknown temporal study error epochless")
        )
      )
    )

    val wrongSchema = json.mapObject(_.add("schema", Wire.id(DefinitionId.studyInput)))
    assertEquals(
      cosine.codec.decode(wrongSchema).left.toOption,
      Some(CodecError.Schema(DefinitionId.studyResult, DefinitionId.studyInput))
    )
  }

  test(
    "provenance is re-derived against the plan: inputs digest, estimator sigma and density steps"
  ) {
    val result = get(cosinePlan(mixed, requireAll).run(mixed))
    val json   = get(cosine.codec.encode(result))

    val foreignInputs = Vector(
      scale0("analyses", "matched", "source", "provenance", "inputs"),
      scale0("analyses", "matched", "provenance", "inputs")
    ).foldLeft(json)((j, path) => edit(j)(path*)(_ => Json.fromString("0000000000000000")))
    assertEquals(
      cosine.codec.decode(foreignInputs).left.toOption,
      Some(
        CodecError.Result(
          StudyResultError.Scale(
            0,
            StudyResultError.ProvenanceInputs(
              StudyDesign.Matched,
              "0000000000000000",
              mixed.reference.digest
            )
          )
        )
      )
    )

    // Sigma tampered consistently in the specification and both provenances of the
    // Gaussian scale still contradicts the estimator the description declares.
    val sigmaTampered = Vector("matched", "control")
      .flatMap(design =>
        Vector(
          scale(1)(
            "analyses",
            design,
            "source",
            "evaluation",
            "specification",
            "parameters",
            "2",
            "value"
          ),
          scale(1)(
            "analyses",
            design,
            "source",
            "provenance",
            "steps",
            "3",
            "params",
            "2",
            "value"
          ),
          scale(1)("analyses", design, "provenance", "steps", "3", "params", "2", "value")
        )
      )
      .foldLeft(json)((j, path) => edit(j)(path*)(number(_ + 0.2)))
    cosine.codec.decode(sigmaTampered) match
      case Left(
            CodecError.Result(
              StudyResultError.Scale(
                1,
                StudyResultError.SpecificationParameters(StudyDesign.Matched, expected, found)
              )
            )
          ) =>
        assertEquals(
          expected.collectFirst { case ("estimate.sigma", v) => v },
          Some(Provenance.Param.Num(0.5))
        )
        assertEquals(
          found.collectFirst { case ("estimate.sigma", v) => v },
          Some(Provenance.Param.Num(0.7))
        )
      case other => fail(s"unexpected $other")

    // A density whose smoothing step names another bandwidth than the scale's estimator.
    val massTampered = edit(json)(
      scale(1)(
        "estimation",
        "0",
        "outcome",
        "provenance",
        "steps",
        "0",
        "params",
        "1",
        "value"
      )*
    )(number(_ + 0.2))
    cosine.codec.decode(massTampered) match
      case Left(
            CodecError.Entry(
              "scales[1]",
              CodecError.Result(StudyResultError.MassProvenance(k, expected, found))
            )
          ) =>
        assertEquals(k: Any, key("p1", "a", "recall"): Any)
        assertEquals(expected, scales(1).provenanceSteps)
        assertEquals(
          found.head.params.collectFirst { case ("sigma", v) => v },
          Some(Provenance.Param.Num(0.7))
        )
      case other => fail(s"unexpected $other")

    // A described method that differs from the archived specification's method.
    val otherMethod =
      edit(json)("value", "description", "2", "values", "1", "value")(number(_ + 1.0))
    assertEquals(
      cosine.codec.decode(otherMethod).left.toOption,
      Some(CodecError.Schema(DefinitionId.cosine, get(DefinitionId.of("eyes4s.cosine", 2))))
    )
  }

  test(
    "checked reconstruction refuses foreign sources, a temporal failure and a mismatched identity"
  ) {
    val result   = get(cosinePlan(clean, requireAll, Vector(StudyEstimate.Binned())).run(clean))
    val scale    = result.scales.head
    val analyses = scale.analyses
    val masses   = Trials(scale.estimation.collect { case (k, Right(mass)) =>
      Trial(k, (), mass)
    })
    // A reduction over an undirected analysis is not a study reduction, whatever its keys.
    val undirected = evaluatePairs(
      pair(masses, Pairing.within[StudyKey].canonicalUndirected),
      clean.hash,
      Distribution.cosine[Px]
    ).meanByEndpoint(FailurePolicy.RequireAll)
    assertEquals(
      StudyAnalyses
        .of(analyses.matchedSource, undirected, analyses.controlSource, analyses.control)
        .left
        .toOption,
      Some(StudyResultError.SourceIdentity(StudyDesign.Matched))
    )
    assertEquals(
      StudyAnalyses
        .of(analyses.matchedSource, analyses.matched, analyses.controlSource, undirected)
        .left
        .toOption,
      Some(StudyResultError.SourceIdentity(StudyDesign.Control))
    )
    assertEquals(
      StudyAnalyses
        .of(analyses.controlSource, analyses.matched, analyses.matchedSource, analyses.control)
        .left
        .toOption,
      Some(StudyResultError.SourceIdentity(StudyDesign.Matched))
    )
    // A contrast over other reductions than the scale's own.
    val other =
      get(cosinePlan(clean, successfulOnly, Vector(StudyEstimate.Binned())).run(clean))
    assertEquals(
      StudyScaleResult
        .reconstruct(
          scale.estimate,
          scale.estimation,
          scale.excludedPhases,
          analyses,
          other.scales.head.contrast
        )
        .left
        .toOption,
      Some(StudyResultError.ContrastAnalyses(StudyDesign.Matched))
    )

    val k        = clean.trials.rows.head.key
    val temporal = get(
      StudyScaleResult.reconstruct(
        StudyEstimate.Binned(),
        scale.estimation.updated(
          0,
          k -> Left(StudyFailure.Temporal(k, TemporalStudyError.Weighting(Weight.Duration)))
        ),
        Vector.empty,
        analyses,
        scale.contrast
      )
    )
    val rebuilt =
      get(StudyResult.reconstruct(result.input, layout, result.description, Vector(temporal)))
    // A temporal failure has a wire form: it round-trips with its trial and typed error.
    val encoded = get(cosine.codec.encode(rebuilt))
    assertEquals(
      encoded.hcursor
        .downField("value")
        .downField("scales")
        .downN(0)
        .downField("estimation")
        .downN(0)
        .downField("outcome")
        .downField("failure")
        .get[Json]("error"),
      Right(
        Json.obj(
          "kind"   -> Json.fromString("weighting"),
          "weight" -> Json.fromString("Duration")
        )
      )
    )
    assertEquals(
      get(cosine.codec.decode(encoded)).scales.head.estimation.head,
      k -> Left(StudyFailure.Temporal(k, TemporalStudyError.Weighting(Weight.Duration)))
    )
    assertEquals(
      StudyResult
        .reconstruct(
          get(ArtifactRef.parse[StudyInput[StudyKey, Px]]("0000000000000000")),
          layout,
          result.description,
          result.scales
        )
        .left
        .toOption,
      Some(
        StudyResultError.InputMismatch(
          "0000000000000000",
          Vector(Provenance.Param.Text(clean.reference.digest))
        )
      )
    )
    assertEquals(
      StudyResult
        .reconstruct(result.input, layout, result.description, Vector.empty)
        .left
        .toOption,
      Some(StudyResultError.ScaleCount(1, 0))
    )
    val otherLayout = StudyKey.layout(get(DefinitionId.of("other.layout", 1)))
    assertEquals(
      StudyResult
        .reconstruct(result.input, otherLayout, result.description, result.scales)
        .left
        .toOption,
      Some(StudyResultError.LayoutMismatch(otherLayout.id, DefinitionId.studyLayout))
    )
    assertEquals(
      StudyScaleResult
        .reconstruct(
          StudyEstimate.Binned(),
          scale.estimation.updated(
            0,
            k -> Left(
              StudyFailure.Frame(key("x", "y", "z"), GeometryError.DegenerateGrid(0, 0))
            )
          ),
          Vector.empty,
          analyses,
          scale.contrast
        )
        .left
        .toOption,
      Some(
        StudyResultError.FailureKey(
          k,
          StudyFailure.Frame(key("x", "y", "z"), GeometryError.DegenerateGrid(0, 0))
        )
      )
    )
    // Phase discipline: a described focal phase the pairs do not respect.
    val swappedPhases = result.description.map {
      case ("phases", Vector(Provenance.Param.Text(f), Provenance.Param.Text(r))) =>
        "phases" -> Vector(Provenance.Param.Text(r), Provenance.Param.Text(f))
      case other => other
    }
    assertEquals(
      StudyResult.reconstruct(result.input, layout, swappedPhases, result.scales).left.toOption,
      Some(
        StudyResultError.Scale(
          0,
          StudyResultError.Phase(key("s1", "a", "recall"), "encode", "recall")
        )
      )
    )
    val otherPolicy = result.description.map {
      case ("failurePolicy", _) =>
        "failurePolicy" -> Vector(Provenance.Param.Text(successfulOnly.render))
      case other => other
    }
    assertEquals(
      StudyResult.reconstruct(result.input, layout, otherPolicy, result.scales).left.toOption,
      Some(
        StudyResultError.Scale(
          0,
          StudyResultError.Policy(StudyDesign.Matched, successfulOnly.render, requireAll)
        )
      )
    )
  }

  test("non-finite error operands survive the archive: infinities by identity, NaN by kind") {
    val result   = get(cosinePlan(mixed, requireAll, Vector(StudyEstimate.Binned())).run(mixed))
    val scale    = result.scales.head
    val analyses = scale.analyses
    val failing  = key("p1", "b", "recall")
    val excluded = key("p2", "d", "practice")
    val source   = analyses.matchedSource
    val rows     = source.rows.map {
      case PairScore(l, r, Left(_)) =>
        PairScore(
          l,
          r,
          Left(
            StudyFailure.Comparison(
              l,
              r,
              CompareError.InvalidScore(
                "cosine",
                ComparisonValueError.NonFiniteSimilarity(Double.NegativeInfinity)
              )
            )
          )
        )
      case row => row
    }
    val rebuiltSource = get(
      DirectedPairwiseAnalysis.reconstruct(
        rows,
        source.diagnostics,
        source.provenance,
        source.evaluation
      )
    )
    val rebuiltMatched = get(
      Analysis.reconstructByLeft(
        analyses.matched.entries,
        analyses.matched.diagnostics,
        analyses.matched.provenance,
        rebuiltSource
      )
    )
    val rebuiltAnalyses = get(
      StudyAnalyses.of(rebuiltSource, rebuiltMatched, analyses.controlSource, analyses.control)
    )
    val contrast = get(
      Contrast.reconstruct(
        rebuiltMatched,
        analyses.control,
        get(scale.contrast).rows,
        Vector("value")
      )(using
        layout.ordering
      )
    )
    val estimation = scale.estimation.map {
      case (k, _) if k == excluded =>
        k -> Left(
          StudyFailure.Frame(
            k,
            GeometryError.NonFiniteBounds(
              Double.NegativeInfinity,
              0.0,
              Double.PositiveInfinity,
              1.0
            )
          )
        )
      case other => other
    }
    val rebuiltScale = get(
      StudyScaleResult.reconstruct(
        scale.estimate,
        estimation,
        scale.excludedPhases,
        rebuiltAnalyses,
        Right(contrast)
      )
    )
    val rebuilt = get(
      StudyResult.reconstruct(result.input, layout, result.description, Vector(rebuiltScale))
    )
    val json    = get(cosine.codec.encode(rebuilt))
    val decoded = get(cosine.codec.decode(json))
    assert(ResultEquivalence.same(rebuilt, decoded)(sameSimilarity, sameDifference))
    assertEquals(get(cosine.codec.encode(decoded)), json)
    assertEquals(
      decoded.scales.head.analyses.matchedSource.rows(1).result.left.toOption,
      Some(
        StudyFailure.Comparison(
          failing,
          key("p1", "b", "encode"),
          CompareError.InvalidScore(
            "cosine",
            ComparisonValueError.NonFiniteSimilarity(Double.NegativeInfinity)
          )
        )
      )
    )
    // NaN is not equal to itself, so its round trip is checked by kind.
    val nan = edit(json)(scale0("estimation", "8", "outcome", "failure", "error", "xMin")*)(_ =>
      Json.fromString("NaN")
    )
    get(cosine.codec.decode(nan)).scales.head.estimation(8)._2 match
      case Left(StudyFailure.Frame(k, GeometryError.NonFiniteBounds(x0, _, x1, _))) =>
        assertEquals(k, excluded)
        assert(x0.isNaN)
        assertEquals(x1, Double.PositiveInfinity)
      case other => fail(s"unexpected $other")
    assertEquals(get(cosine.codec.encode(get(cosine.codec.decode(nan)))), nan)
    assert(get(ResultWire.readParam(ResultWire.param(Provenance.Param.Num(Double.NaN)))) match
      case Provenance.Param.Num(v) => v.isNaN
      case _                       => false)
  }

  test("the built-in registry decodes the cosine archive and refuses duplicates") {
    val result   = get(cosinePlan(mixed, successfulOnly).run(mixed))
    val json     = get(cosine.codec.encode(result))
    val registry = get(StudyResultRegistry.empty[StudyKey, Px].register(cosine.registration))
    val loaded   = get(registry.decode(json))
    assertEquals(loaded.result.scales.size, 3)
    assertEquals(get(loaded.encode), json)
    assertEquals(
      registry.register(cosine.registration).left.toOption,
      Some(CodecError.DuplicateResultCodec(DefinitionId.cosine))
    )
    assertEquals(
      StudyResultRegistry.empty[StudyKey, Px].decode(json).left.toOption,
      Some(CodecError.MissingResultCodec(DefinitionId.cosine))
    )
  }

  test(
    "score and difference codecs are typed: a difference codec cannot stand in for a score codec"
  ) {
    assert(
      typeCheckErrors(
        """import eyes4s.codec.*; import eyes4s.kernel.Unit2D.Px
           StudyCodecs.cosine[Px].results(StudyResultCodecs.signedDifference(), StudyResultCodecs.similarity())"""
      ).nonEmpty
    )
    assert(
      StudyResultCodecs
        .similarity()
        .decode(
          Json.obj(
            "schema" -> Wire.id(DefinitionId.similarity),
            "value"  -> Json.fromString("NaN")
          )
        )
        .isLeft
    )
  }
