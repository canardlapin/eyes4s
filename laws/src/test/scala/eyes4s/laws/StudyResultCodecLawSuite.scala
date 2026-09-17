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
  private type Source =
    DirectedPairwiseAnalysis[StudyKey, StudyKey, StudyFailure[StudyKey], Similarity]
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

  /** A focal trial in a foreign frame with its matched reference (a failed
    * pair) and a clean matched pair (a successful pair and a reduced key), on
    * top of the random trials; every mutant below has something to alter.
    */
  private val guaranteed: Gen[StudyInput[StudyKey, Px]] = inputs.map(random =>
    StudyInput(
      Trials(
        Vector(
          trial(StudyKey("p1", "z", "recall"), otherFrame, Vector((0.5, 0.5))),
          trial(StudyKey("p1", "z", "encode"), frame, Vector((0.5, 0.5))),
          trial(StudyKey("p1", "y", "recall"), frame, Vector((0.5, 0.5), (1.5, 1.5))),
          trial(StudyKey("p1", "y", "encode"), frame, Vector((1.5, 1.5)))
        ) ++ random.trials.rows
      )
    )
  )

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
      .choose(1, 7)
      .map(mask =>
        candidates.indices.filter(i => (mask & (1 << i)) != 0).map(candidates).toVector
      )

  private def run(
      input: StudyInput[StudyKey, Px],
      policy: FailurePolicy,
      scales: Vector[StudyEstimate[Px]]
  ): Result =
    checked(
      StudyPlan
        .cosine[Px](input.reference, grid, "recall", "encode", Weight.Duration, scales, policy)
        .flatMap(_.run(input))
    )

  private val results: Gen[Result] = for
    input  <- inputs
    policy <- policies
    scales <- estimates
  yield run(input, policy, scales)

  /** Binned first, so the first scale has a density, a failed pair and a reduced key. */
  private val mutable: Gen[Result] = for
    input  <- guaranteed
    policy <- policies
    scales <- estimates
  yield run(
    input,
    policy,
    StudyEstimate.Binned() +: scales.filterNot(_ == StudyEstimate.Binned())
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

  /** A mutant is killed only by a falsified property, never by exhaustion. */
  private def killed[A](codec: VersionedCodec[A], gen: Gen[A], eq: (A, A) => Boolean): Boolean =
    CodecLaws.roundTrip(codec, gen, eq).all.properties.exists { case (_, prop) =>
      Test.check(Test.Parameters.default.withMinSuccessfulTests(40), prop).status match
        case Test.Failed(_, _) | Test.PropException(_, _, _) => true
        case _                                               => false
    }

  private def reconstruction[A](
      e: Either[ReconstructionError[StudyKey], A]
  ): Either[CodecError, A] =
    e.left.map(x => CodecError.Reconstruction(x))
  private def result[A](e: Either[StudyResultError[StudyKey], A]): Either[CodecError, A] =
    e.left.map(x => CodecError.Result(x))

  /** Rebuild a result with its first scale's matched analysis replaced through
    * the checked constructors; the contrast rows are kept as stored.
    */
  private def withMatched(r: Result)(
      change: (Source, Analysis[StudyKey, Similarity]) => Either[
        CodecError,
        (Source, Analysis[StudyKey, Similarity])
      ]
  ): Either[CodecError, Result] =
    val scale = r.scales.head
    for
      changed  <- change(scale.analyses.matchedSource, scale.analyses.matched)
      analyses <- result(
        StudyAnalyses.of(
          changed._1,
          changed._2,
          scale.analyses.controlSource,
          scale.analyses.control
        )
      )
      contrast <- scale.contrast match
        case Left(error) => Right(Left(error))
        case Right(c)    =>
          reconstruction(
            Contrast.reconstruct(analyses.matched, analyses.control, c.rows, Vector("value"))
          ).map(Right(_))
      rebuilt <- result(
        StudyScaleResult
          .reconstruct(
            scale.estimate,
            scale.estimation,
            scale.excludedPhases,
            analyses,
            contrast
          )
      )
      whole <- result(
        StudyResult.reconstruct(
          r.input,
          StudyKey.layout(DefinitionId.studyLayout),
          r.description,
          rebuilt +: r.scales.tail
        )
      )
    yield whole

  test(
    "published laws kill a dropped failure row, reordered or dropped provenance and an altered denominator"
  ) {
    val droppedFailure = mutant(codec.codec)(r =>
      withMatched(r) { (s, m) =>
        reconstruction(
          DirectedPairwiseAnalysis
            .reconstruct(
              s.rows.filterNot(_.result.isLeft),
              s.diagnostics,
              s.provenance,
              s.evaluation
            )
        ).flatMap(rebuilt =>
          reconstruction(
            Analysis.reconstructByLeft(m.entries, m.diagnostics, m.provenance, rebuilt)
          ).map(rebuilt -> _)
        )
      }
    )
    assert(killed(droppedFailure, mutable, same))

    val reorderedProvenance = mutant(codec.codec)(r =>
      withMatched(r) { (s, m) =>
        val reversed = Provenance(
          s.provenance.inputs,
          s.provenance.steps.map(step => Provenance.Step(step.operation, step.params.reverse))
        )
        reconstruction(
          DirectedPairwiseAnalysis.reconstruct(s.rows, s.diagnostics, reversed, s.evaluation)
        ).flatMap(rebuilt =>
          reconstruction(
            Analysis.reconstructByLeft(m.entries, m.diagnostics, m.provenance, rebuilt)
          ).map(rebuilt -> _)
        )
      }
    )
    assert(killed(reorderedProvenance, mutable, same))

    val droppedMassStep = mutant(codec.codec)(r =>
      val scale      = r.scales.head
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
      result(
        StudyScaleResult.reconstruct(
          scale.estimate,
          estimation,
          scale.excludedPhases,
          scale.analyses,
          scale.contrast
        )
      ).flatMap(rebuilt =>
        result(
          StudyResult.reconstruct(
            r.input,
            StudyKey.layout(DefinitionId.studyLayout),
            r.description,
            rebuilt +: r.scales.tail
          )
        )
      )
    )
    assert(killed(droppedMassStep, mutable, same))

    def bumped(by: Int): VersionedCodec[Result] = mutant(codec.codec)(r =>
      withMatched(r) { (s, m) =>
        val index = m.entries.indexWhere(_.result.isRight)
        val row   = m.entries(index)
        reconstruction(
          ReductionRow.reconstruct(
            row.key,
            row.result,
            row.successful + by,
            row.failed,
            row.contributing + 1
          )
        ).flatMap(changed =>
          reconstruction(
            Analysis.reconstructByLeft(
              m.entries.updated(index, changed),
              m.diagnostics,
              m.provenance,
              s
            )
          ).map(s -> _)
        )
      }
    )
    assert(killed(bumped(0), mutable, same))
    // Bumping successful and contributing together is consistent with the row alone,
    // but not with the source pairs the reduction regroups.
    assert(killed(bumped(1), mutable, same))
  }
