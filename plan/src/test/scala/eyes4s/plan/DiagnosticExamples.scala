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
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.kernel.*
import scala.compiletime.{erasedValue, summonFrom, summonInline}
import scala.deriving.Mirror

/** A value of `A` for a seed. Different seeds give different values of every
  * scalar kind, so the fields of a generated case never repeat a value and a
  * swapped operand is detected. Integers and reals never coincide: a real is
  * always a seed plus one half.
  */
trait DiagnosticExample[A]:
  def apply(seed: Int): A

object DiagnosticExample extends DiagnosticExampleStructures:
  def of[A](f: Int => A): DiagnosticExample[A] =
    new DiagnosticExample[A]:
      def apply(seed: Int): A = f(seed)

  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => throw new AssertionError(s"$error"), identity)

  given DiagnosticExample[Int]          = of(identity)
  given DiagnosticExample[Long]         = of(seed => 1000L + seed)
  given DiagnosticExample[Double]       = of(seed => seed + 0.5)
  given DiagnosticExample[String]       = of(seed => s"s$seed")
  given DiagnosticExample[Char]         = of(seed => ('a' + seed % 26).toChar)
  given DiagnosticExample[Byte]         = of(seed => (seed % 100).toByte)
  given DiagnosticExample[BigInt]       = of(seed => BigInt(2000 + seed))
  given DiagnosticExample[BigDecimal]   = of(seed => BigDecimal(seed) + BigDecimal("0.25"))
  given DiagnosticExample[Boolean]      = of(seed => seed % 2 == 0)
  given DiagnosticExample[Span]         = of(seed => Span.micros(3000L + seed))
  given DiagnosticExample[Instant]      = of(seed => Instant.micros(4000L + seed))
  given DiagnosticExample[PositiveSpan] =
    of(seed => get(PositiveSpan.of(Span.micros(5000L + seed))))
  given DiagnosticExample[NonNegativeSpan] =
    of(seed => get(NonNegativeSpan.of(Span.micros(6000L + seed))))
  given DiagnosticExample[NonNegativeLong] = of(seed => get(NonNegativeLong.of(7000L + seed)))
  given DiagnosticExample[ContentHash]     = of(seed => ContentHash.ofString(s"hash-$seed"))
  given DiagnosticExample[FrameId]         = of(seed => FrameId(s"frame-$seed"))
  given DiagnosticExample[ClockId]         = of(seed => ClockId(s"clock-$seed"))
  given DiagnosticExample[GridId]          = of(seed => GridId(s"grid-$seed"))
  given DiagnosticExample[RecordingRef]    = of(seed => RecordingRef(s"recording-$seed"))
  given DiagnosticExample[DetectorRef]     = of(seed => DetectorRef(s"detector-$seed", "1"))
  given DiagnosticExample[DefinitionId]    = of(seed => get(DefinitionId.of(s"def-$seed", 1)))
  given DiagnosticExample[AlgorithmId]     = of(seed => get(AlgorithmId.from(s"alg-$seed")))
  given DiagnosticExample[SessionKey]      = of(seed => get(SessionKey.of(s"session-$seed")))
  given DiagnosticExample[PredictorId]     = of(seed => get(PredictorId.of(s"predictor-$seed")))
  given DiagnosticExample[StudyKey] = of(seed => StudyKey(s"p$seed", s"i$seed", "recall"))
  given DiagnosticExample[Interval] = of(seed =>
    get(Interval.of(ClockId(s"clock-$seed"), Instant.micros(seed), Instant.micros(seed + 10L)))
  )
  given DiagnosticExample[SampleRange] = of(seed => get(SampleRange.of(seed, seed + 3)))
  given DiagnosticExample[Window]      =
    of(seed => get(Window.of(Span.micros(-seed.toLong), Span.micros(seed + 20L))))
  given DiagnosticExample[TemporalSupport] =
    of(seed => get(TemporalSupport.fixed(Span.micros(8000L + seed))))
  given DiagnosticExample[Provenance] =
    of(seed => Provenance(ContentHash.ofString(s"inputs-$seed"), Vector.empty))
  given DiagnosticExample[Provenance.Param] = of(seed => Provenance.Param.Text(s"param-$seed"))
  given DiagnosticExample[DataRecord]       = of(seed => get(DataRecord.of(seed + 101)))
  given DiagnosticExample[CsvRecord]        = of(seed => get(CsvRecord.of(seed + 202)))
  given DiagnosticExample[SourceLine]       = of(seed => get(SourceLine.of(seed + 303L)))

  // Nested errors are given one explicit case each, so a generated family
  // never derives a chain of wrapped errors (which the compiler would refuse
  // as a diverging implicit search).
  given DiagnosticExample[GeometryError] = of(seed => GeometryError.NonFiniteSigma(seed + 0.5))
  given DiagnosticExample[RecordIdentityError] =
    of(seed => RecordIdentityError.CsvRecordNotPositive(-seed))
  given DiagnosticExample[SurfaceError] =
    of(seed => SurfaceError.LengthMismatch(seed, seed + 1))
  given DiagnosticExample[TimeError] =
    of(seed => TimeError.ReversedWindow(9000L + seed, 8000L + seed))
  given DiagnosticExample[TimelineError] =
    of(seed => TimelineError.BlankClock(s"clock-$seed"))
  given DiagnosticExample[eyes4s.compare.ComparisonValueError] =
    of(seed => eyes4s.compare.ComparisonValueError.NonFiniteSimilarity(seed + 0.5))
  given DiagnosticExample[eyes4s.compare.CompareError] =
    of(seed => eyes4s.compare.CompareError.ZeroNorm(s"measure-$seed", seed + 0.5, seed + 1.5))
  given DiagnosticExample[eyes4s.compare.ComparisonWorkError] =
    of(seed => eyes4s.compare.ComparisonWorkError.InvalidQuantum(seed))
  given DiagnosticExample[PairScheduleError] = of(PairScheduleError.InvalidQuantum(_))
  given DiagnosticExample[ScoreMeanError]    = of(seed => ScoreMeanError.EmptyValues(s"v$seed"))
  given DiagnosticExample[EvaluationSpecError] =
    of(seed => EvaluationSpecError.EmptyField(s"field-$seed", " "))
  given DiagnosticExample[LeastSquaresError]    = of(LeastSquaresError.RankTolerance(_))
  given DiagnosticExample[ReductionPolicyError] =
    of(ReductionPolicyError.NonPositiveMinimumSuccessful(_))
  given DiagnosticExample[ConfigurationError] =
    of(ConfigurationError.InsufficientRegularSamples(_))
  given DiagnosticExample[KinematicsError] =
    of(seed =>
      KinematicsError.InvalidSampling(ConfigurationError.InsufficientRegularSamples(seed))
    )
  given DiagnosticExample[CoreError] =
    of(seed => CoreError.OfTime(TimeError.ReversedWindow(9000L + seed, 8000L + seed)))
  given DiagnosticExample[RecordingError] =
    of(seed => RecordingError.NonMonotonic(seed, 9000L + seed, 8000L + seed))
  given DiagnosticExample[DetectionSupportError] = of(seed =>
    DetectionSupportError.EventSupportCountMismatch(
      RecordingRef(s"recording-$seed"),
      seed,
      seed + 1
    )
  )
  given DiagnosticExample[DetectionFailure] = of(seed =>
    DetectionFailure.Kinematics(
      KinematicsError.InvalidSampling(ConfigurationError.InsufficientRegularSamples(seed))
    )
  )
  given DiagnosticExample[DetectionResultError] = of(seed =>
    DetectionResultError.SourceSupport(
      RecordingRef(s"recording-$seed"),
      DetectorRef(s"detector-$seed", "1"),
      DetectionSupportError.InvalidSampleRange(seed, seed - 1)
    )
  )
  given DiagnosticExample[SyncEvidenceError] =
    of(seed => SyncEvidenceError.EmptyMarkId(s" $seed"))
  given DiagnosticExample[eyes4s.aoi.AoiError] =
    of(seed => eyes4s.aoi.AoiError.BlankId(s" $seed"))
  given DiagnosticExample[eyes4s.surface.DensityLookupError] =
    of(seed =>
      eyes4s.surface.DensityLookupError.Surface(SurfaceError.LengthMismatch(seed, seed + 1))
    )
  given DiagnosticExample[eyes4s.surface.DensityPointFailure] =
    of(seed => eyes4s.surface.DensityPointFailure.NonFinitePoint(seed + 0.5, seed + 1.5))
  given DiagnosticExample[eyes4s.compare.MapScaleFailure] =
    of(_ => eyes4s.compare.MapScaleFailure.MissingLeft)
  given DiagnosticExample[PlanError]          = of(seed => PlanError.EmptyScales(seed))
  given DiagnosticExample[TemporalStudyError] =
    of(seed => TemporalStudyError.InvalidWindow(s"window-$seed", 9000L + seed, 8000L + seed))
  given DiagnosticExample[RecordingPlanError] =
    of(seed => RecordingPlanError.InvalidSource(RecordingRef(s"recording-$seed")))
  given DiagnosticExample[InventoryError] =
    of(seed => InventoryError.Width(seed, seed + 1, seed + 2))
  given DiagnosticExample[QuarantineCause] =
    of(seed => QuarantineCause.Overlap(seed, s"[$seed,10)", s"[5,$seed)"))
  given DiagnosticExample[TemplateError] =
    of(seed => TemplateError.DuplicateKey(s"k$seed"))

  given option[A](using a: DiagnosticExample[A]): DiagnosticExample[Option[A]] =
    of(seed => Some(a(seed)))
  given vector[A](using a: DiagnosticExample[A]): DiagnosticExample[Vector[A]] =
    of(seed => Vector(a(seed * 10 + 1), a(seed * 10 + 2)))
  given set[A](using a: DiagnosticExample[A]): DiagnosticExample[Set[A]] =
    of(seed => Set(a(seed * 10 + 1), a(seed * 10 + 2)))
  given nonEmpty[A](using a: DiagnosticExample[A]): DiagnosticExample[NonEmptyVector[A]] =
    of(seed => NonEmptyVector.of(a(seed * 10 + 1), a(seed * 10 + 2)))

  /** One generated value of every case of `E`, in declaration order; field
    * `i` of a case takes seed `i + 1`.
    */
  inline def everyCase[E](using mirror: Mirror.SumOf[E]): Vector[E] =
    caseValues[mirror.MirroredElemTypes].asInstanceOf[Vector[E]]

  /** Four cases at a time, so a family of up to 128 cases stays within the
    * compiler's limit of successive inlines.
    */
  inline def caseValues[T <: Tuple]: Vector[Any] = inline erasedValue[T] match
    case _: EmptyTuple                 => Vector.empty
    case _: (a *: b *: c *: d *: rest) =>
      Vector[Any](product[a](0), product[b](0), product[c](0), product[d](0)) ++
        caseValues[rest]
    case _: (c *: rest) => product[c](0) +: caseValues[rest]

  inline def product[C](seed: Int): C = summonFrom { case mirror: Mirror.ProductOf[C] =>
    val values = fields[mirror.MirroredElemTypes](seed, 1)
    mirror.fromProduct(Tuple.fromArray(values.toArray[Any]))
  }

  inline def fields[T <: Tuple](seed: Int, index: Int): Vector[Any] =
    inline erasedValue[T] match
      case _: EmptyTuple  => Vector.empty
      case _: (f *: rest) =>
        summonInline[DiagnosticExample[f]](seed * 10 + index) +: fields[rest](seed, index + 1)

trait DiagnosticExampleStructures:
  /** A product from its fields' examples; a sum as its first case, so a
    * recursive error type (an error that wraps its own family) terminates.
    */
  inline given structure[A](using mirror: Mirror.Of[A]): DiagnosticExample[A] =
    inline mirror match
      case sum: Mirror.SumOf[A] =>
        DiagnosticExampleStructures.First[A](firstCase[sum.MirroredElemTypes])
      case product: Mirror.ProductOf[A] =>
        DiagnosticExampleStructures.Product[A](seed =>
          product.fromProduct(
            Tuple.fromArray(
              DiagnosticExample.fields[product.MirroredElemTypes](seed, 1).toArray
            )
          )
        )

  inline def firstCase[T <: Tuple]: Int => Any = inline erasedValue[T] match
    case _: (c *: _) => seed => DiagnosticExample.product[c](seed)

object DiagnosticExampleStructures:
  final class First[A](make: Int => Any) extends DiagnosticExample[A]:
    def apply(seed: Int): A = make(seed).asInstanceOf[A]

  final class Product[A](make: Int => A) extends DiagnosticExample[A]:
    def apply(seed: Int): A = make(seed)
