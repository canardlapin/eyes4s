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

package eyes4s.kernel

/** Whether common marks identify only an epoch offset or both offset and rate. */
enum SyncFitMode derives CanEqual:
  case OffsetOnly
  case Affine

  def render: String = this match
    case OffsetOnly => "offset-only"
    case Affine     => "affine"

/** One named occurrence observed on both timelines. */
final class SyncMark private (
    val id: String,
    val onSource: Instant,
    val onTarget: Instant
) derives CanEqual:
  override def equals(other: Any): Boolean = other match
    case mark: SyncMark =>
      id == mark.id && onSource == mark.onSource && onTarget == mark.onTarget
    case _ => false

  override def hashCode: Int =
    (id.hashCode * 31 + onSource.hashCode) * 31 + onTarget.hashCode

object SyncMark:
  def of(
      id: String,
      onSource: Instant,
      onTarget: Instant
  ): Either[SyncEvidenceError, SyncMark] =
    if id.trim.isEmpty then Left(SyncEvidenceError.EmptyMarkId(id))
    else Right(new SyncMark(id, onSource, onTarget))

/** Non-negative absolute residual eligible to remain in a fitted map. */
opaque type SyncResidualLimit = Span

object SyncResidualLimit:
  def of(limit: Span): Either[SyncEvidenceError, SyncResidualLimit] =
    if limit.isNegative then Left(SyncEvidenceError.NegativeResidualLimit(limit))
    else Right(limit)

  extension (limit: SyncResidualLimit) def span: Span = limit

/** Non-negative synchronization error or uncertainty magnitude. */
opaque type SyncErrorMagnitude = Span

object SyncErrorMagnitude:
  val zero: SyncErrorMagnitude = Span.zero

  def of(value: Span): Either[SyncEvidenceError, SyncErrorMagnitude] =
    if value.isNegative then Left(SyncEvidenceError.NegativeErrorMagnitude(value))
    else Right(value)

  private[kernel] def fromNonNegativeMicros(value: Long): SyncErrorMagnitude =
    Span.micros(value)

  extension (value: SyncErrorMagnitude)
    def span: Span       = value
    def toMicros: Long   = Span.toMicros(value)
    def toMillis: Double = Span.toMillis(value)
    def render: String   = Span.renderMilliseconds(Span.toMicros(value))

/** Signed target-clock error for one retained common mark. */
final case class SyncResidual private (
    mark: SyncMark,
    predictedTarget: Instant,
    error: Span
) derives CanEqual:
  def absolute: SyncErrorMagnitude =
    val micros = error.toMicros
    SyncErrorMagnitude.fromNonNegativeMicros(
      if micros == Long.MinValue then Long.MaxValue else math.abs(micros)
    )

object SyncResidual:
  private[kernel] def fromPrediction(mark: SyncMark, predictedTarget: Instant): SyncResidual =
    new SyncResidual(mark, predictedTarget, predictedTarget.until(mark.onTarget))

/** A mark excluded because its initial fitted residual exceeded the policy. */
final case class RejectedSyncMark private (
    mark: SyncMark,
    residual: SyncErrorMagnitude,
    limit: SyncResidualLimit
) derives CanEqual

object RejectedSyncMark:
  private[kernel] def fromResidual(
      residual: SyncResidual,
      limit: SyncResidualLimit
  ): RejectedSyncMark =
    new RejectedSyncMark(residual.mark, residual.absolute, limit)

/** Auditable evidence for an affine map between two nominal clocks.
  *
  * The map and all diagnostics come from the same retained observations. A
  * consumer can therefore inspect residuals and exclusions before accepting
  * the conversion rather than receiving an unexplained pair of coefficients.
  */
final class SyncEvidence private (
    val mode: SyncFitMode,
    val sync: Sync,
    val usedMarks: Vector[SyncMark],
    val residuals: Vector[SyncResidual],
    val rejectedMarks: Vector[RejectedSyncMark],
    val rootMeanSquareResidual: SyncErrorMagnitude,
    val maximumAbsoluteResidual: SyncErrorMagnitude,
    val uncertainty: SyncErrorMagnitude
):
  def source: ClockId = sync.from
  def target: ClockId = sync.to
  def scale: Double   = 1.0 + sync.drift
  def offset: Span    = sync.offset

  /** Convert a source-clock instant after this evidence has established its identity. */
  def apply(time: Instant): Instant = sync.unsafeInstant(time)

  /** Convert a source-clock interval with the ordinary nominal clock check. */
  def apply(interval: Interval): Either[TimeError, Interval] = sync(interval)

  /** Current constant uncertainty model, stated as a method so a later
    * heteroscedastic fit can vary it by time without changing call sites.
    */
  def uncertaintyAt(time: Instant): SyncErrorMagnitude =
    val _ = time
    uncertainty

  def render: String =
    s"${mode.render} ${sync.render}; used=${usedMarks.length}, " +
      s"rejected=${rejectedMarks.length}, rms=${rootMeanSquareResidual.render}, " +
      s"max=${maximumAbsoluteResidual.render}"

object SyncEvidence:

  /** Fit from ordered common marks, optionally excluding a first-pass residual
    * outlier set and refitting the retained marks once.
    */
  def fromCommonMarks(
      source: ClockId,
      target: ClockId,
      mode: SyncFitMode,
      marks: Vector[SyncMark],
      residualLimit: Option[SyncResidualLimit] = None
  ): Either[SyncEvidenceError, SyncEvidence] =
    val required = requiredMarks(mode)
    for
      _       <- validate(source, target, mode, marks)
      initial <- fit(source, target, mode, marks)
      initialResiduals = residualsFor(initial, marks)
      rejectedIndices  = residualLimit.fold(Set.empty[Int]) { limit =>
        initialResiduals.zipWithIndex.collect {
          case (residual, index) if residual.absolute.toMicros > limit.span.toMicros => index
        }.toSet
      }
      retained = marks.zipWithIndex.collect {
        case (mark, index) if !rejectedIndices.contains(index) => mark
      }
      _ <- Either.cond(
        retained.length >= required,
        (),
        SyncEvidenceError.TooFewRetainedMarks(
          source,
          target,
          marks.length,
          retained.length,
          required
        )
      )
      fitted <-
        if rejectedIndices.isEmpty then Right(initial) else fit(source, target, mode, retained)
      retainedResiduals = residualsFor(fitted, retained)
      rejected          = residualLimit.toVector.flatMap { limit =>
        marks.zipWithIndex.collect {
          case (mark, index) if rejectedIndices.contains(index) =>
            RejectedSyncMark.fromResidual(initialResiduals(index), limit)
        }
      }
      rms     = rootMeanSquare(retainedResiduals)
      maximum = retainedResiduals
        .map(_.absolute.toMicros)
        .maxOption
        .fold(SyncErrorMagnitude.zero)(SyncErrorMagnitude.fromNonNegativeMicros)
    yield new SyncEvidence(
      mode,
      fitted,
      retained,
      retainedResiduals,
      rejected,
      rms,
      maximum,
      maximum
    )

  private def validate(
      source: ClockId,
      target: ClockId,
      mode: SyncFitMode,
      marks: Vector[SyncMark]
  ): Either[SyncEvidenceError, Unit] =
    val required = requiredMarks(mode)
    if marks.length < required then
      Left(
        SyncEvidenceError.TooFewCommonMarks(
          source,
          target,
          marks.length,
          required
        )
      )
    else
      marks.indices
        .drop(1)
        .collectFirst {
          case secondIndex if marks.take(secondIndex).exists(_.id == marks(secondIndex).id) =>
            val id         = marks(secondIndex).id
            val firstIndex = marks.indexWhere(_.id == id)
            SyncEvidenceError.DuplicateMarkId(source, target, id, firstIndex, secondIndex)
        }
        .orElse(
          marks.indices.drop(1).collectFirst {
            case index
                if marks(index).onSource.toMicros <= marks(index - 1).onSource.toMicros =>
              SyncEvidenceError.NonIncreasingSourceMarks(
                source,
                target,
                index,
                marks(index - 1).onSource,
                marks(index).onSource
              )
          }
        )
        .orElse(
          marks.indices.drop(1).collectFirst {
            case index
                if marks(index).onTarget.toMicros <= marks(index - 1).onTarget.toMicros =>
              SyncEvidenceError.NonIncreasingTargetMarks(
                source,
                target,
                index,
                marks(index - 1).onTarget,
                marks(index).onTarget
              )
          }
        )
        .toLeft(())

  private def fit(
      source: ClockId,
      target: ClockId,
      mode: SyncFitMode,
      marks: Vector[SyncMark]
  ): Either[SyncEvidenceError, Sync] = mode match
    case SyncFitMode.OffsetOnly =>
      val meanOffset = marks.iterator
        .map(mark => mark.onSource.until(mark.onTarget).toMicros.toDouble)
        .sum / marks.length
      Sync
        .affine(source, target, Span.micros(math.round(meanOffset)), 0.0)
        .left
        .map(SyncEvidenceError.InvalidFittedSync(source, target, _))
    case SyncFitMode.Affine =>
      val sourceOrigin = marks.head.onSource.toMicros
      val targetOrigin = marks.head.onTarget.toMicros
      val xs           = marks.map(mark => (mark.onSource.toMicros - sourceOrigin).toDouble)
      val ys           = marks.map(mark => (mark.onTarget.toMicros - targetOrigin).toDouble)
      val meanX        = xs.sum / xs.length
      val meanY        = ys.sum / ys.length
      val variance     = xs.map(value => square(value - meanX)).sum
      if !variance.isFinite || variance <= 0.0 then
        Left(SyncEvidenceError.DegenerateSourceVariance(source, target, variance))
      else
        val covariance = xs
          .zip(ys)
          .map { case (x, y) =>
            (x - meanX) * (y - meanY)
          }
          .sum
        val scale  = covariance / variance
        val offset =
          targetOrigin.toDouble + meanY - scale * (sourceOrigin.toDouble + meanX)
        if !scale.isFinite || !offset.isFinite then
          Left(SyncEvidenceError.NonFiniteFit(source, target, scale, offset))
        else
          Sync
            .affine(source, target, Span.micros(math.round(offset)), scale - 1.0)
            .left
            .map(SyncEvidenceError.InvalidFittedSync(source, target, _))

  private def residualsFor(sync: Sync, marks: Vector[SyncMark]): Vector[SyncResidual] =
    marks.map { mark =>
      val predicted = sync.unsafeInstant(mark.onSource)
      SyncResidual.fromPrediction(mark, predicted)
    }

  private def rootMeanSquare(residuals: Vector[SyncResidual]): SyncErrorMagnitude =
    val meanSquare = residuals.iterator
      .map(residual => square(residual.error.toMicros.toDouble))
      .sum / residuals.length
    SyncErrorMagnitude.fromNonNegativeMicros(math.round(math.sqrt(meanSquare)))

  private def requiredMarks(mode: SyncFitMode): Int = mode match
    case SyncFitMode.OffsetOnly => 1
    case SyncFitMode.Affine     => 2

  private def square(value: Double): Double = value * value

enum SyncEvidenceError derives CanEqual:
  case EmptyMarkId(value: String)
  case NegativeResidualLimit(limit: Span)
  case NegativeErrorMagnitude(value: Span)
  case TooFewCommonMarks(
      source: ClockId,
      target: ClockId,
      available: Int,
      required: Int
  )
  case TooFewRetainedMarks(
      source: ClockId,
      target: ClockId,
      supplied: Int,
      retained: Int,
      required: Int
  )
  case DuplicateMarkId(
      source: ClockId,
      target: ClockId,
      id: String,
      firstIndex: Int,
      secondIndex: Int
  )
  case NonIncreasingSourceMarks(
      source: ClockId,
      target: ClockId,
      index: Int,
      previous: Instant,
      current: Instant
  )
  case NonIncreasingTargetMarks(
      source: ClockId,
      target: ClockId,
      index: Int,
      previous: Instant,
      current: Instant
  )
  case DegenerateSourceVariance(source: ClockId, target: ClockId, variance: Double)
  case NonFiniteFit(source: ClockId, target: ClockId, scale: Double, offsetMicros: Double)
  case InvalidFittedSync(source: ClockId, target: ClockId, underlying: TimeError)

  def message: String = this match
    case EmptyMarkId(value) =>
      s"A common synchronization mark requires a non-empty id, got value='$value'."
    case NegativeResidualLimit(limit) =>
      s"A synchronization residual limit must be non-negative, got limit=${limit.render}."
    case NegativeErrorMagnitude(value) =>
      s"A synchronization error magnitude must be non-negative, got value=${value.render}."
    case TooFewCommonMarks(source, target, available, required) =>
      s"Synchronization from '$source' to '$target' requires at least $required common marks, got available=$available."
    case TooFewRetainedMarks(source, target, supplied, retained, required) =>
      s"Synchronization from '$source' to '$target' retained only $retained of $supplied marks after rejection; required=$required."
    case DuplicateMarkId(source, target, id, firstIndex, secondIndex) =>
      s"Synchronization from '$source' to '$target' received duplicate mark id='$id' at indices $firstIndex and $secondIndex."
    case NonIncreasingSourceMarks(source, target, index, previous, current) =>
      s"Synchronization from '$source' to '$target' has non-increasing source marks at index=$index: previous=${previous.render}, current=${current.render}."
    case NonIncreasingTargetMarks(source, target, index, previous, current) =>
      s"Synchronization from '$source' to '$target' has non-increasing target marks at index=$index: previous=${previous.render}, current=${current.render}."
    case DegenerateSourceVariance(source, target, variance) =>
      s"Affine synchronization from '$source' to '$target' has degenerate source variance=$variance."
    case NonFiniteFit(source, target, scale, offset) =>
      s"Synchronization from '$source' to '$target' produced non-finite coefficients: scale=$scale, offsetMicros=$offset."
    case InvalidFittedSync(source, target, underlying) =>
      s"Synchronization from '$source' to '$target' produced an invalid affine map: ${underlying.message}"

end SyncEvidenceError
