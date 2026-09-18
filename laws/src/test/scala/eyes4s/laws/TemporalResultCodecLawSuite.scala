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

/** Published round-trip laws for temporal result archives, over generated
  * temporal studies whose trials have fixations clipped by windows and
  * coverage gaps, anchors at zero, beyond JavaScript's exact integer range
  * and close enough to the Long maximum that a late window overflows,
  * missing epochs, both boundaries, one or two repetitions and binned and
  * Gaussian scales; with deliberate mutants that a lawful archive must not
  * survive.
  */
class TemporalResultCodecLawSuite extends munit.DisciplineSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)

  /** Generated operands satisfy every constructor invariant by construction,
    * so a `Left` here is a generator bug and surfaces as a failure.
    */
  private def sure[E, A](value: Either[E, A]): A =
    value.fold(
      e => throw new IllegalStateException(s"generator invariant violated: $e"),
      identity
    )

  private val frame = get(Frame.screen("law-temporal", 2, 2))
  private val grid  = get(Grid.over(frame, 2, 2))
  private val plans =
    new TemporalStudyCodec(
      get(DefinitionId.of("eyes4s.temporal-study", 1)),
      StudyCodecs.cosine[Px]
    )
  private val results =
    plans.results(StudyResultCodecs.similarity(), StudyResultCodecs.signedDifference())
  private type Result = TemporalStudyResult[StudyKey, Px, Unit, Similarity, SignedDifference]

  private val sim  = (a: Similarity, b: Similarity) => a.value == b.value
  private val diff = (a: SignedDifference, b: SignedDifference) => a.value == b.value
  private val same = (a: Result, b: Result) => TemporalResultEquivalence.same(a, b)(sim, diff)

  /** Anchors at zero, beyond 2^53, and near the Long maximum: there the last
    * window of the plan cannot be anchored without overflow.
    */
  private val anchors: Gen[Long] = Gen.frequency(
    4 -> Gen.const(0L),
    1 -> Gen.const(9007199254740993L),
    1 -> Gen.const(Long.MaxValue - 1050000L)
  )

  private def clock(key: StudyKey): ClockId =
    ClockId(s"law:${key.participant}/${key.stimulus}/${key.phase}")

  /** One trial: fixations over the first 900 ms after its anchor, and its
    * epoch (full coverage, or coverage with a gap), or none.
    */
  private def trial(
      key: StudyKey,
      withEpoch: Gen[Boolean]
  ): Gen[(Trial[StudyKey, Unit, Scanpath[Px]], Option[TrialEpoch])] =
    for
      anchor    <- anchors
      n         <- Gen.choose(1, 4)
      durations <- Gen.listOfN(n, Gen.oneOf(50000L, 100000L, 200000L))
      points    <- Gen.listOfN(n, Gen.zip(Gen.oneOf(0.5, 1.5), Gen.oneOf(0.5, 1.5)))
      epoch     <- withEpoch
      gapped    <- Gen.oneOf(false, true)
    yield
      val c      = clock(key)
      val onsets = durations.scanLeft(0L)((t, d) => t + d + 10000L)
      val fixes  = durations.zip(points).zip(onsets).map { case ((d, (x, y)), onset) =>
        sure(
          Event.Fixation.withoutDispersion(
            sure(
              Interval.of(c, Instant.micros(anchor + onset), Instant.micros(anchor + onset + d))
            ),
            Pt[Px](x, y),
            1
          )
        )
      }
      val coverage =
        if gapped then Vector((0L, 350000L), (500000L, 1000000L)) else Vector((0L, 1000000L))
      val trialEpoch = Option.when(epoch)(
        TrialEpoch(
          Instant.micros(anchor),
          sure(
            ObservedCoverage.of(
              c,
              coverage.map((a, b) =>
                sure(Interval.of(c, Instant.micros(anchor + a), Instant.micros(anchor + b)))
              )
            )
          )
        )
      )
      (Trial(key, (), sure(Scanpath.of(frame, c, IArray.from(fixes)))), trialEpoch)

  private val keys: Vector[StudyKey] = for
    p  <- Vector("p1", "p2")
    s  <- Vector("a", "b")
    ph <- Vector("encode", "recall", "retest")
  yield StudyKey(p, s, ph)

  private val windowSets: Vector[Vector[(String, Long, Long)]] = Vector(
    Vector(("early", 0L, 300000L)),
    Vector(("early", 0L, 300000L), ("late", 600000L, 950000L)),
    Vector(("early", 0L, 300000L), ("middle", 300000L, 600000L), ("outside", 950000L, 1100000L))
  )

  private def study(
      trials: Vector[(Trial[StudyKey, Unit, Scanpath[Px]], Option[TrialEpoch])],
      windows: Vector[(String, Long, Long)],
      twoRepetitions: Boolean,
      gaussian: Boolean,
      boundary: FixationBoundary
  ): Result =
    val ordered = trials.sortBy(_._1.key)
    val input   = sure(
      TemporalStudyInput.of(
        StudyInput(Trials(ordered.map(_._1))),
        ordered.flatMap((t, e) => e.map(t.key -> _))
      )
    )
    val estimates =
      StudyEstimate.Binned[Px]() +: Vector.fill(if gaussian then 1 else 0)(
        StudyEstimate.Gaussian(sure(Sigma.px(1)), EdgePolicy.Truncate)
      )
    val base = sure(
      StudyPlan.cosine[Px](
        input.study.reference,
        grid,
        "recall",
        "encode",
        Weight.Duration,
        estimates,
        FailurePolicy.RequireAll
      )
    )
    val repetitions =
      sure(RepetitionContrast.withinParticipant("recall-encode", "recall", "encode")) +:
        Vector.fill(if twoRepetitions then 1 else 0)(
          sure(RepetitionContrast.withinParticipant("retest-recall", "retest", "recall"))
        )
    val plan = sure(
      TemporalStudyPlan.of(
        base,
        input.reference,
        windows.map((name, from, until) =>
          sure(StudyWindow.of(name, sure(Window.of(Span.micros(from), Span.micros(until)))))
        ),
        repetitions,
        boundary
      )
    )
    sure(plan.run(input))

  private val studies: Gen[Result] = for
    count  <- Gen.choose(2, 7)
    chosen <- Gen.pick(count, keys)
    trials <- Gen.sequence[Vector[
      (Trial[StudyKey, Unit, Scanpath[Px]], Option[TrialEpoch])
    ], (Trial[StudyKey, Unit, Scanpath[Px]], Option[TrialEpoch])](
      chosen.toVector.map(k => trial(k, Gen.frequency(5 -> true, 1 -> false)))
    )
    windows  <- Gen.oneOf(windowSets)
    two      <- Gen.oneOf(false, true)
    gaussian <- Gen.oneOf(false, true)
    boundary <- Gen.oneOf(FixationBoundary.ClipDuration, FixationBoundary.FullyContained)
  yield study(trials, windows, two, gaussian, boundary)

  /** Matched and control pairs anchored at zero, and a trial without an
    * epoch, over two windows: every mutant below has something to alter.
    */
  private val mutable: Gen[Result] = for
    core <- Gen.sequence[Vector[
      (Trial[StudyKey, Unit, Scanpath[Px]], Option[TrialEpoch])
    ], (Trial[StudyKey, Unit, Scanpath[Px]], Option[TrialEpoch])](
      Vector("a", "b").flatMap(s =>
        Vector("recall", "encode").map(ph => trial(StudyKey("p1", s, ph), Gen.const(true)))
      )
    )
    missing  <- trial(StudyKey("p1", "a", "retest"), Gen.const(false))
    gaussian <- Gen.oneOf(false, true)
  yield study(
    core.map(realign) :+ missing,
    windowSets(1),
    true,
    gaussian,
    FixationBoundary.ClipDuration
  )

  /** A trial re-anchored at zero: its fixations and coverage start at zero. */
  private def realign(
      entry: (Trial[StudyKey, Unit, Scanpath[Px]], Option[TrialEpoch])
  ): (Trial[StudyKey, Unit, Scanpath[Px]], Option[TrialEpoch]) =
    val (row, _) = entry
    val c        = clock(row.key)
    val shift    = row.value.fixations.toVector.headOption.fold(0L)(_.span.onset.toMicros)
    val fixes    = row.value.fixations.toVector.map(f =>
      sure(
        Event.Fixation.withoutDispersion(
          sure(
            Interval.of(
              c,
              Instant.micros(f.span.onset.toMicros - shift),
              Instant.micros(f.span.offset.toMicros - shift)
            )
          ),
          f.centre,
          1
        )
      )
    )
    val epoch = TrialEpoch(
      Instant.micros(0L),
      sure(
        ObservedCoverage.of(
          c,
          Vector(sure(Interval.of(c, Instant.micros(0L), Instant.micros(1000000L))))
        )
      )
    )
    (Trial(row.key, (), sure(Scanpath.of(frame, c, IArray.from(fixes)))), Some(epoch))

  checkAll("temporal study result", CodecLaws.roundTrip(results.codec, studies, same))

  test("the mutable generator has a contrast difference, a missing epoch and two windows") {
    val result = get(mutable.sample.toRight("no sample"))
    assert(result.cells.size >= 2)
    assert(result.cells.head.occupancy.exists(_._2.isLeft))
    assert(result.cells.head.occupancy.exists(_._2.isRight))
    assert(
      result.cells.head.result.scales.head.contrast.exists(_.rows.exists(_.difference.isRight))
    )
  }

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

  /** A mutant is killed only by a falsified property, never by exhaustion or an exception. */
  private def killed[A](codec: VersionedCodec[A], gen: Gen[A], eq: (A, A) => Boolean): Boolean =
    CodecLaws.roundTrip(codec, gen, eq).all.properties.exists { case (_, prop) =>
      Test.check(Test.Parameters.default.withMinSuccessfulTests(40), prop).status match
        case Test.Failed(_, _) => true
        case _                 => false
    }

  private type Record = TemporalCellRecord[StudyKey, Px, Similarity, SignedDifference]

  private def records(result: Result): Vector[Record] =
    result.cells.map(c =>
      TemporalCellRecord(c.repetition.name, c.window.name, c.occupancy, c.result)
    )

  private def rebuilt(result: Result)(cells: Vector[Record]): Either[CodecError, Result] =
    TemporalStudyResult
      .reconstruct(result.plan, cells)
      .left
      .map(e => CodecError.TemporalResult(e))

  /** The first cell with one changed part. */
  private def first(result: Result)(change: Record => Either[CodecError, Record]) =
    val all = records(result)
    change(all.head).flatMap(cell => rebuilt(result)(cell +: all.tail))

  test(
    "published laws kill reordered cells, a forged epoch, a shifted window and a changed difference"
  ) {
    val reordered = mutant(results.codec)(r => rebuilt(r)(records(r).reverse))
    assert(killed(reordered, mutable, same))

    val forgedEpoch = mutant(results.codec)(r =>
      first(r)(cell =>
        Right(cell.copy(occupancy = cell.occupancy.map {
          case (k, Left(TemporalStudyError.MissingEpoch(_))) =>
            k -> Left(TemporalStudyError.MissingEpoch("0000000000000000"))
          case other => other
        }))
      )
    )
    assert(killed(forgedEpoch, mutable, same))

    val shifted = mutant(results.codec)(r =>
      first(r) { cell =>
        val index        = cell.occupancy.indexWhere(_._2.isRight)
        val (key, value) = cell.occupancy(index)
        value match
          case Right(o) =>
            (for
              moved <- Interval
                .of(
                  o.interval.clock,
                  Instant.micros(o.interval.onset.toMicros + 1),
                  Instant.micros(o.interval.offset.toMicros + 1)
                )
                .left
                .map(TemporalStudyError.Time.apply)
              rebuilt <- WindowOccupancy
                .reconstruct(
                  moved,
                  o.boundary,
                  o.measure.frame,
                  o.measure.positions,
                  o.observedMicros,
                  o.missingMicros,
                  o.fixationTimes
                )
                .left
                .map(TemporalStudyError.Occupancy.apply)
            yield cell.copy(occupancy = cell.occupancy.updated(index, key -> Right(rebuilt)))).left
              .map(CodecError.Temporal.apply)
          case Left(_) => Right(cell)
      }
    )
    assert(killed(shifted, mutable, same))

    val changedDifference = mutant(results.codec)(r =>
      first(r) { cell =>
        val scale = cell.result.scales.head
        scale.contrast match
          case Left(_)         => Right(cell)
          case Right(contrast) =>
            val index = contrast.rows.indexWhere(_.difference.isRight)
            val row   = contrast.rows(index)
            (for
              value <- row.difference.left
                .map(_ => CodecError.Field("difference", Json.Null, "failed"))
              moved <- SignedDifference
                .between(value.value + 0.5, 0.0)
                .left
                .map(e => CodecError.Field("difference", Json.Null, e.message))
              changed <- ContrastRow
                .reconstruct(row.key, row.matched, row.control, Right(moved))
                .left
                .map(e => CodecError.Reconstruction(e))
              rebuiltContrast <- Contrast
                .reconstruct(
                  contrast.matched,
                  contrast.control,
                  contrast.rows.updated(index, changed),
                  Vector("value")
                )
                .left
                .map(e => CodecError.Reconstruction(e))
              rebuiltScale <- StudyScaleResult
                .reconstruct(
                  scale.estimate,
                  scale.estimation,
                  scale.excludedPhases,
                  scale.analyses,
                  Right(rebuiltContrast)
                )
                .left
                .map(e => CodecError.Result(e))
              whole <- StudyResult
                .reconstruct(
                  cell.result.input,
                  r.plan.base.layout,
                  cell.result.description,
                  rebuiltScale +: cell.result.scales.tail,
                  r.plan.provenanceContext(r.cells.head.repetition, r.cells.head.window)
                )
                .left
                .map(e => CodecError.Result(e))
            yield cell.copy(result = whole))
      }
    )
    assert(killed(changedDifference, mutable, same))
  }
