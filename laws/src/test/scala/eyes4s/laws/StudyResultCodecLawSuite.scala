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

import eyes4s.codec.*
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy
import io.circe.Json
import org.scalacheck.{Gen, Test}

/** Published round-trip laws for completed fixation-study results, over
  * generated studies that mix successes with frame, estimation and pair
  * failures, duplicate keys, unmatched references and excluded phases, with
  * deliberate mutants that a lawful archive must not survive.
  */
class StudyResultCodecLawSuite extends munit.DisciplineSuite:
  private def checked[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)

  private val frame      = checked(Frame.screen("law-display", 2, 2))
  private val grid       = checked(Grid.over(frame, 2, 2))
  private val otherFrame = checked(Frame.screen("law-other", 2, 2))
  private val codec      = StudyResultCodecs.cosine[Px]
  private val sim        = (a: Similarity, b: Similarity) => a.value == b.value
  private val diff       = (a: SignedDifference, b: SignedDifference) => a.value == b.value
  private type Result = StudyResult[StudyKey, Px, Similarity, SignedDifference]
  private type Scale  = StudyScaleResult[StudyKey, Px, Similarity, SignedDifference]
  private val same = (a: Result, b: Result) => StudyResultEquivalence.same(a, b)(sim, diff)

  private def trial(key: StudyKey, in: Frame[Px], points: Vector[(Double, Double)]) =
    val clock = ClockId(s"${key.participant}/${key.stimulus}/${key.phase}")
    val fixes = points.zipWithIndex.map { case ((x, y), i) =>
      checked(
        Event.Fixation.withoutDispersion(
          checked(
            Interval.of(clock, Instant.micros(i * 2000L), Instant.micros(i * 2000L + 1000L))
          ),
          Pt[Px](x, y),
          1
        )
      )
    }
    Trial(key, (), checked(Scanpath.of(in, clock, IArray.from(fixes))))

  private val keys: Gen[StudyKey] = for
    p <- Gen.oneOf("p1", "p2")
    s <- Gen.oneOf("a", "b", "c")
    f <- Gen.frequency(4 -> "recall", 4 -> "encode", 1 -> "practice")
  yield StudyKey(p, s, f)

  private val trials: Gen[Trial[StudyKey, Unit, Scanpath[Px]]] = for
    key    <- keys
    in     <- Gen.frequency(9 -> frame, 1 -> otherFrame)
    n      <- Gen.choose(1, 3)
    points <- Gen.listOfN(n, Gen.zip(Gen.oneOf(0.5, 1.5), Gen.oneOf(0.5, 1.5)))
  yield trial(key, in, points.toVector)

  private val inputs: Gen[StudyInput[StudyKey, Px]] =
    Gen
      .choose(0, 8)
      .flatMap(n => Gen.listOfN(n, trials))
      .map(rows => StudyInput(Trials(rows.toVector)))

  private val policies: Gen[FailurePolicy] = Gen.oneOf(
    Gen.const(FailurePolicy.RequireAll),
    Gen.choose(1, 2).map(n => checked(FailurePolicy.successfulOnly(n)))
  )

  private val candidates: Vector[StudyEstimate[Px]] = Vector(
    StudyEstimate.Binned(),
    StudyEstimate.Gaussian(checked(Sigma.px(0.5)), EdgePolicy.Truncate),
    StudyEstimate.Gaussian(checked(Sigma.px(0.05)), EdgePolicy.Truncate)
  )
  private val estimates: Gen[Vector[StudyEstimate[Px]]] =
    Gen
      .someOf(candidates.indices)
      .suchThat(_.nonEmpty)
      .map(chosen => chosen.toVector.sorted.map(candidates))

  private val results: Gen[Result] = for
    input  <- inputs
    policy <- policies
    scales <- estimates
  yield checked(
    StudyPlan
      .cosine[Px](input.reference, grid, "recall", "encode", Weight.Duration, scales, policy)
      .flatMap(_.run(input))
  )

  checkAll("cosine study result", CodecLaws.roundTrip(codec.codec, results, same))

  /** Wrap a codec so that decoding applies a deliberate change to the value. */
  private def mutant[A](codec: VersionedCodec[A])(
      change: A => Either[CodecError, A]
  ): VersionedCodec[A] =
    VersionedCodec.checked[A](codec.schema)(a =>
      codec
        .encode(a)
        .flatMap(j =>
          j.hcursor.get[Json]("value").left.map(e => CodecError.Field("value", j, e.message))
        )
    )(raw =>
      codec
        .decode(
          Json.obj(
            "schema" -> Json.obj(
              "name"    -> Json.fromString(codec.schema.name),
              "version" -> Json.fromInt(codec.schema.version)
            ),
            "value" -> raw
          )
        )
        .flatMap(change)
    )

  private def killed[A](codec: VersionedCodec[A], gen: Gen[A], eq: (A, A) => Boolean): Boolean =
    CodecLaws.roundTrip(codec, gen, eq).all.properties.exists { case (_, prop) =>
      !Test.check(Test.Parameters.default.withMinSuccessfulTests(40), prop).passed
    }

  private def matchedOf(scale: Scale): Option[Analysis[StudyKey, Similarity]] =
    scale.contrast.toOption.map(_.matched)

  /** Rebuild a result with its first scale replaced through the checked constructors. */
  private def withScale(result: Result)(
      change: Scale => Either[CodecError, Scale]
  ): Either[CodecError, Result] =
    change(result.scales.head).flatMap(scale =>
      StudyResult
        .reconstruct(result.input, result.description, scale +: result.scales.tail)
        .left
        .map(e => CodecError.Result(e))
    )

  private def withMatched(scale: Scale)(
      change: Analysis[StudyKey, Similarity] => Either[
        CodecError,
        Analysis[StudyKey, Similarity]
      ]
  ): Either[CodecError, Scale] =
    val contrast = scale.contrast.toOption.get
    change(contrast.matched).flatMap(matched =>
      Contrast
        .reconstruct(matched, contrast.control, contrast.rows, Vector("value"))
        .left
        .map(e => CodecError.Reconstruction(e))
        .flatMap(c =>
          StudyScaleResult
            .reconstruct(scale.estimate, scale.estimation, scale.excludedPhases, Right(c))
            .left
            .map(e => CodecError.Result(e))
        )
    )

  private def source(analysis: Analysis[StudyKey, Similarity]) =
    analysis.source.asInstanceOf[DirectedPairwiseAnalysis[StudyKey, StudyKey, StudyFailure[
      StudyKey
    ], Similarity]]

  test(
    "published laws kill a dropped failure row, reordered or dropped provenance and an altered denominator"
  ) {
    val withFailedPair = results.suchThat(r =>
      matchedOf(r.scales.head).exists(_.source.rows.exists(_.result.isLeft))
    )
    val droppedFailure = mutant(codec.codec)(r =>
      withScale(r)(withMatched(_)(m =>
        val s = source(m)
        DirectedPairwiseAnalysis
          .reconstruct(
            s.rows.filterNot(_.result.isLeft),
            s.diagnostics,
            s.provenance,
            s.evaluation
          )
          .left
          .map(e => CodecError.Reconstruction(e))
          .flatMap(rebuilt =>
            Analysis
              .reconstruct(m.entries, m.diagnostics, m.provenance, rebuilt)
              .left
              .map(e => CodecError.Reconstruction(e))
          )
      ))
    )
    assert(killed(droppedFailure, withFailedPair, same))

    val withContrast        = results.suchThat(r => matchedOf(r.scales.head).isDefined)
    val reorderedProvenance = mutant(codec.codec)(r =>
      withScale(r)(withMatched(_)(m =>
        val s        = source(m)
        val reversed = Provenance(
          s.provenance.inputs,
          s.provenance.steps.map(step => Provenance.Step(step.operation, step.params.reverse))
        )
        DirectedPairwiseAnalysis
          .reconstruct(s.rows, s.diagnostics, reversed, s.evaluation)
          .left
          .map(e => CodecError.Reconstruction(e))
          .flatMap(rebuilt =>
            Analysis
              .reconstruct(m.entries, m.diagnostics, m.provenance, rebuilt)
              .left
              .map(e => CodecError.Reconstruction(e))
          )
      ))
    )
    assert(killed(reorderedProvenance, withContrast, same))

    val withMass        = results.suchThat(_.scales.head.estimation.exists(_._2.isRight))
    val droppedMassStep = mutant(codec.codec)(r =>
      withScale(r) { scale =>
        val estimation = scale.estimation.map {
          case (k, Right(mass)) if mass.provenance.steps.nonEmpty =>
            k -> Surface
              .mass(
                mass.grid,
                mass.values,
                Provenance(mass.provenance.inputs, mass.provenance.steps.dropRight(1))
              )
              .toOption
              .toRight(StudyFailure.Occupancy(k, SurfaceError.EmptyCollection("mutant")))
          case other => other
        }
        StudyScaleResult
          .reconstruct(scale.estimate, estimation, scale.excludedPhases, scale.contrast)
          .left
          .map(e => CodecError.Result(e))
      }
    )
    assert(killed(droppedMassStep, withMass, same))

    val withReduced =
      results.suchThat(r => matchedOf(r.scales.head).exists(_.entries.exists(_.result.isRight)))
    val alteredDenominator = mutant(codec.codec)(r =>
      withScale(r)(withMatched(_)(m =>
        val index = m.entries.indexWhere(_.result.isRight)
        val row   = m.entries(index)
        ReductionRow
          .reconstruct(row.key, row.result, row.successful, row.failed, row.contributing + 1)
          .left
          .map(e => CodecError.Reconstruction(e))
          .flatMap(changed =>
            Analysis
              .reconstruct(
                m.entries.updated(index, changed),
                m.diagnostics,
                m.provenance,
                m.source
              )
              .left
              .map(e => CodecError.Reconstruction(e))
          )
      ))
    )
    assert(killed(alteredDenominator, withReduced, same))

    // A consistent-looking alteration of successful and contributing counts is still refused,
    // because the report's contribution count no longer follows from the rows.
    val inflatedDenominator = mutant(codec.codec)(r =>
      withScale(r)(withMatched(_)(m =>
        val index = m.entries.indexWhere(_.result.isRight)
        val row   = m.entries(index)
        ReductionRow
          .reconstruct(
            row.key,
            row.result,
            row.successful + 1,
            row.failed,
            row.contributing + 1
          )
          .left
          .map(e => CodecError.Reconstruction(e))
          .flatMap(changed =>
            Analysis
              .reconstruct(
                m.entries.updated(index, changed),
                m.diagnostics,
                m.provenance,
                m.source
              )
              .left
              .map(e => CodecError.Reconstruction(e))
          )
      ))
    )
    assert(killed(inflatedDenominator, withReduced, same))
  }
