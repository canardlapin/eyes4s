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
  private def sameSimilarity(a: Similarity, b: Similarity): Boolean = a.value == b.value
  private def sameDifference(a: SignedDifference, b: SignedDifference): Boolean =
    a.value == b.value
  private def key(p: String, s: String, phase: String) = StudyKey(p, s, phase)
  private def source(analysis: Analysis[StudyKey, ?])  =
    analysis.source
      .asInstanceOf[DirectedPairwiseAnalysis[StudyKey, StudyKey, StudyFailure[StudyKey], ?]]

  /** Edit one JSON location by path; array steps are integers. */
  private def edit(json: Json)(path: String*)(f: Json => Json): Json =
    def go(cursor: ACursor, rest: List[String]): ACursor = rest match
      case Nil          => cursor.withFocus(f)
      case head :: tail =>
        head.toIntOption match
          case Some(index) => go(cursor.downN(index), tail)
          case None        => go(cursor.downField(head), tail)
    go(json.hcursor, path.toList).top.getOrElse(fail(s"no JSON at ${path.mkString("/")}"))

  private def scale0(rest: String*): Seq[String] = Seq("value", "scales", "0") ++ rest

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
        source(contrast.matched).rows.map(r => (r.left, r.right, r.result.isRight)),
        Vector(
          (focal, key("p1", "a", "encode"), true),
          (failing, key("p1", "b", "encode"), false)
        )
      )
      assertEquals(
        source(contrast.matched).rows(1).result.left.toOption,
        Some(StudyFailure.Frame(failing, GeometryError.FrameMismatch(frame.id, otherFrame.id)))
      )
      assertEquals(contrast.matched.diagnostics.selectedPairCount, 2)
      assertEquals(contrast.matched.diagnostics.eligiblePairCount, 2L)
      assertEquals(
        source(contrast.matched).diagnostics.ambiguous,
        // Duplicate indices address the focal operand, not the input rows.
        Vector(PairingAmbiguity.DuplicateLeft[StudyKey, StudyKey](doubled, Vector(2, 3)))
      )
      assertEquals(source(contrast.matched).diagnostics.unmatchedLeft, Vector.empty)
      assertEquals(
        source(contrast.matched).diagnostics.unmatchedRight,
        Vector(key("p2", "a", "encode"), key("p2", "c", "encode"))
      )
      assertEquals(source(contrast.control).rows.size, 2)
      assertEquals(
        source(contrast.control).diagnostics.unmatchedRight,
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
      // The smooth Gaussian estimates every in-frame trial.
      assertEquals(decoded.scales(1).estimation.count(_._2.isRight), 8)
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
      get(get(decoded.scales(0).contrast).matched.evaluation.specification.toRight("no spec"))
    assertEquals(spec.components, Vector("similarity", "distance"))
    val difference = get(get(decoded.scales(0).contrast).rows(0).difference)
    assertEqualsDouble(difference.distance.value, -difference.similarity.value, 1e-12)

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
      scale0("contrast", "matched", "source", "evaluation", "specification", "components")*
    )(_ => Json.arr(Json.fromString("similarity")))
    assertEquals(
      twoComponentCodec.codec.decode(oneComponent).left.toOption,
      Some(
        CodecError.Entry(
          "scales[0].contrast.matched.source",
          CodecError.ScoreComponents(Vector("similarity", "distance"), Vector("similarity"))
        )
      )
    )

    val nonFinite = edit(json)(
      scale0("contrast", "matched", "source", "rows", "0", "result", "score", "similarity")*
    )(_ => Json.fromString("NaN"))
    twoComponentCodec.codec.decode(nonFinite) match
      case Left(
            CodecError.Entry(
              "scales[0].contrast.matched.source.rows[0]",
              CodecError.Field("similarity", _, _)
            )
          ) =>
        ()
      case other => fail(s"unexpected $other")

    val conflicting = edit(json)(
      scale0("contrast", "matched", "source", "provenance", "steps", "1", "params")*
    )(params => Json.arr(params.asArray.get.dropRight(1)*))
    twoComponentCodec.codec.decode(conflicting) match
      case Left(
            CodecError.Entry(
              "scales[0].contrast.matched.source",
              CodecError.Reconstruction(
                ReconstructionError.ProvenanceConflict("evaluatePairs", declared, derived)
              )
            )
          ) =>
        assertEquals(declared.steps(1).params.size, derived.steps(1).params.size - 1)
      case other => fail(s"unexpected $other")
  }

  test(
    "malformed archives name their operands: impossible denominators, orphan pairs, dropped failure rows and a partial run tagged complete"
  ) {
    val result = get(cosinePlan(mixed, requireAll).run(mixed))
    val json   = get(cosine.codec.encode(result))
    val focal  = key("p1", "a", "recall")

    val denominator = edit(json)(
      scale0("contrast", "matched", "entries", "0", "contributing")*
    )(_ => Json.fromInt(5))
    assertEquals(
      cosine.codec.decode(denominator).left.toOption,
      Some(
        CodecError.Entry(
          "scales[0].contrast.matched.entries[0]",
          CodecError.Reconstruction(ReconstructionError.Denominator(focal, 1, 0, 5))
        )
      )
    )

    val stranger = get(StudyCodecs.key(DefinitionId.studyKey).encode(key("p9", "z", "recall")))
    val orphan   =
      edit(json)(scale0("contrast", "matched", "source", "rows", "0", "left")*)(_ => stranger)
    assertEquals(
      cosine.codec.decode(orphan).left.toOption,
      Some(
        CodecError.Entry(
          "scales[0]",
          CodecError.Result(
            StudyResultError.OrphanPair(key("p9", "z", "recall"), key("p1", "a", "encode"))
          )
        )
      )
    )

    val dropped = edit(json)(scale0("contrast", "matched", "source", "rows")*)(rows =>
      Json.arr(rows.asArray.get.take(1)*)
    )
    assertEquals(
      cosine.codec.decode(dropped).left.toOption,
      Some(
        CodecError.Entry(
          "scales[0].contrast.matched.source",
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

    val wrongSchema = json.mapObject(_.add("schema", Wire.id(DefinitionId.studyInput)))
    assertEquals(
      cosine.codec.decode(wrongSchema).left.toOption,
      Some(CodecError.Schema(DefinitionId.studyResult, DefinitionId.studyInput))
    )
  }

  test(
    "checked reconstruction refuses a temporal failure in the fixation archive and a mismatched identity"
  ) {
    val plan   = cosinePlan(clean, requireAll, Vector(StudyEstimate.Binned()))
    val result = get(plan.run(clean))
    val k      = clean.trials.rows.head.key
    val scale  = get(
      StudyScaleResult.reconstruct[StudyKey, Px, Similarity, SignedDifference](
        StudyEstimate.Binned(),
        Vector(
          k -> Left(StudyFailure.Temporal(k, TemporalStudyError.Weighting(Weight.Duration)))
        ),
        Vector.empty,
        Left(ContrastError.EmptyDomain(0, 0))
      )
    )
    val rebuilt = get(StudyResult.reconstruct(result.input, result.description, Vector(scale)))
    assertEquals(
      cosine.codec.encode(rebuilt).left.toOption.map(_.message),
      Some(
        "At scales[0].estimation[0]: Cannot encode failure: temporal failures name windows and epochs that belong to the temporal result archive"
      )
    )
    assertEquals(
      StudyResult
        .reconstruct(
          get(ArtifactRef.parse[StudyInput[StudyKey, Px]]("0000000000000000")),
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
      StudyResult.reconstruct(result.input, result.description, Vector.empty).left.toOption,
      Some(StudyResultError.ScaleCount(1, 0))
    )
    assertEquals(
      StudyScaleResult
        .reconstruct[StudyKey, Px, Similarity, SignedDifference](
          StudyEstimate.Binned(),
          Vector(
            k -> Left(
              StudyFailure.Frame(key("x", "y", "z"), GeometryError.DegenerateGrid(0, 0))
            )
          ),
          Vector.empty,
          Left(ContrastError.EmptyDomain(0, 0))
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
    assertEquals(
      StudyResultCodecs
        .similarity()
        .decode(
          Json.obj(
            "schema" -> Wire.id(DefinitionId.similarity),
            "value"  -> Json.fromString("NaN")
          )
        )
        .isLeft,
      true
    )
  }
