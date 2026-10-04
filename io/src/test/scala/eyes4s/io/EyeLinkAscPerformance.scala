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

package eyes4s.io

import eyes4s.plan.Diagnose

/** A positive byte count used for parser and measurement budgets. */
final class AscByteBudget private (val bytes: Long)

object AscByteBudget:
  def of(operand: String, bytes: Long): Either[AscPerformanceValidationError, AscByteBudget] =
    if bytes <= 0L then Left(AscPerformanceValidationError.NonPositive(operand, bytes))
    else Right(new AscByteBudget(bytes))

/** Logical source-byte retention of the streaming framer.
  *
  * This is deliberately not presented as a JVM heap-size estimate. At one
  * parser step, source content consists of at most one retained partial line
  * plus one input chunk. Runtime object overhead and transient copies are
  * measured separately by the performance court.
  */
final class AscStreamingMemoryEnvelope private (
    val maximumLineBytes: AscByteBudget,
    val maximumReadChunkBytes: AscByteBudget,
    val maximumLogicalInputBytesPerStep: AscByteBudget
)

object AscStreamingMemoryEnvelope:
  def from(
      settings: AscStreamSettings
  ): Either[AscPerformanceValidationError, AscStreamingMemoryEnvelope] =
    for
      line  <- AscByteBudget.of("maximumLineBytes", settings.lineLimit.bytes.toLong)
      chunk <- AscByteBudget.of(
        "maximumReadChunkBytes",
        settings.readChunkSize.bytes.toLong
      )
      perStep <- AscByteBudget.of(
        "maximumLogicalInputBytesPerStep",
        line.bytes + chunk.bytes
      )
    yield new AscStreamingMemoryEnvelope(line, chunk, perStep)

/** Whether an execution discards emissions as they are consumed or retains
  * them as a materialized collection.
  */
enum AscPerformanceMode derives CanEqual:
  case Streaming
  case Materialized

/** The memory claim appropriate to a workflow. */
enum AscWorkflowMemoryEnvelope:
  case Streaming(parser: AscStreamingMemoryEnvelope)
  case Materialized(
      parser: AscStreamingMemoryEnvelope,
      inputBytes: AscByteBudget
  )

object AscWorkflowMemoryEnvelope:
  def forInput(
      mode: AscPerformanceMode,
      settings: AscStreamSettings,
      inputBytes: AscByteBudget
  ): Either[AscPerformanceValidationError, AscWorkflowMemoryEnvelope] =
    AscStreamingMemoryEnvelope.from(settings).map { parser =>
      mode match
        case AscPerformanceMode.Streaming    => AscWorkflowMemoryEnvelope.Streaming(parser)
        case AscPerformanceMode.Materialized =>
          AscWorkflowMemoryEnvelope.Materialized(parser, inputBytes)
    }

/** One measured performance result with its pass/fail thresholds stored as
  * data. A passing unit suite is not substituted for this evidence.
  */
final class AscPerformanceResult private (
    val measuredAtUtc: String,
    val sourceRevision: String,
    val sourceDigest: Sha256,
    val sourceDirty: Boolean,
    val hardware: String,
    val operatingSystem: String,
    val runtime: String,
    val profile: String,
    val workload: String,
    val nominalRateHz: Int,
    val durationSeconds: Long,
    val eyeLayout: AscEyeLayout,
    val messageRich: Boolean,
    val mode: AscPerformanceMode,
    val inputBytes: AscByteBudget,
    val physicalLines: Long,
    val emittedLines: Long,
    val lineBudget: AscByteBudget,
    val chunkBudget: AscByteBudget,
    val elapsedNanos: Long,
    val peakHeapDeltaBytes: Long,
    val allocatedBytes: Long,
    val jvmMaximumHeapBytes: Long,
    val minimumBytesPerSecond: Long,
    val maximumPeakHeapBytes: Long,
    val maximumAllocationPermille: Long
):
  val throughputBytesPerSecond: Long =
    ((BigDecimal(inputBytes.bytes) * BigDecimal(1000000000L)) /
      BigDecimal(elapsedNanos)).toLong

  val allocationBytesPerInputBytePermille: Long =
    ((BigDecimal(allocatedBytes) * BigDecimal(1000L)) /
      BigDecimal(inputBytes.bytes)).toLong

  val passed: Boolean =
    emittedLines == physicalLines &&
      throughputBytesPerSecond >= minimumBytesPerSecond &&
      peakHeapDeltaBytes <= maximumPeakHeapBytes &&
      jvmMaximumHeapBytes <= maximumPeakHeapBytes &&
      allocationBytesPerInputBytePermille <= maximumAllocationPermille

object AscPerformanceResult:
  def of(
      measuredAtUtc: String,
      sourceRevision: String,
      sourceDigest: Sha256,
      sourceDirty: Boolean,
      hardware: String,
      operatingSystem: String,
      runtime: String,
      profile: String,
      workload: String,
      nominalRateHz: Int,
      durationSeconds: Long,
      eyeLayout: AscEyeLayout,
      messageRich: Boolean,
      mode: AscPerformanceMode,
      inputBytes: Long,
      physicalLines: Long,
      emittedLines: Long,
      lineBudgetBytes: Long,
      chunkBudgetBytes: Long,
      elapsedNanos: Long,
      peakHeapDeltaBytes: Long,
      allocatedBytes: Long,
      jvmMaximumHeapBytes: Long,
      minimumBytesPerSecond: Long,
      maximumPeakHeapBytes: Long,
      maximumAllocationPermille: Long
  ): Either[AscPerformanceValidationError, AscPerformanceResult] =
    for
      measured <- nonBlank("measuredAtUtc", measuredAtUtc)
      revision <- nonBlank("sourceRevision", sourceRevision)
      machine  <- nonBlank("hardware", hardware)
      os       <- nonBlank("operatingSystem", operatingSystem)
      jvm      <- nonBlank("runtime", runtime)
      run      <- nonBlank("profile", profile)
      name     <- nonBlank("workload", workload)
      _        <- positiveInt("nominalRateHz", nominalRateHz)
      _        <- positive("durationSeconds", durationSeconds)
      bytes    <- AscByteBudget.of("inputBytes", inputBytes)
      _        <- positive("physicalLines", physicalLines)
      _        <- nonNegative("emittedLines", emittedLines)
      line     <- AscByteBudget.of("lineBudgetBytes", lineBudgetBytes)
      chunk    <- AscByteBudget.of("chunkBudgetBytes", chunkBudgetBytes)
      _        <- positive("elapsedNanos", elapsedNanos)
      _        <- nonNegative("peakHeapDeltaBytes", peakHeapDeltaBytes)
      _        <- nonNegative("allocatedBytes", allocatedBytes)
      _        <- positive("jvmMaximumHeapBytes", jvmMaximumHeapBytes)
      _        <- positive("minimumBytesPerSecond", minimumBytesPerSecond)
      _        <- positive("maximumPeakHeapBytes", maximumPeakHeapBytes)
      _        <- positive("maximumAllocationPermille", maximumAllocationPermille)
    yield new AscPerformanceResult(
      measured,
      revision,
      sourceDigest,
      sourceDirty,
      machine,
      os,
      jvm,
      run,
      name,
      nominalRateHz,
      durationSeconds,
      eyeLayout,
      messageRich,
      mode,
      bytes,
      physicalLines,
      emittedLines,
      line,
      chunk,
      elapsedNanos,
      peakHeapDeltaBytes,
      allocatedBytes,
      jvmMaximumHeapBytes,
      minimumBytesPerSecond,
      maximumPeakHeapBytes,
      maximumAllocationPermille
    )

  private def nonBlank(
      operand: String,
      value: String
  ): Either[AscPerformanceValidationError, String] =
    val normalized = value.trim
    if normalized.isEmpty then Left(AscPerformanceValidationError.Blank(operand, value))
    else Right(normalized)

  private def positive(
      operand: String,
      value: Long
  ): Either[AscPerformanceValidationError, Unit] =
    if value <= 0L then Left(AscPerformanceValidationError.NonPositive(operand, value))
    else Right(())

  private def positiveInt(
      operand: String,
      value: Int
  ): Either[AscPerformanceValidationError, Unit] = positive(operand, value.toLong)

  private def nonNegative(
      operand: String,
      value: Long
  ): Either[AscPerformanceValidationError, Unit] =
    if value < 0L then Left(AscPerformanceValidationError.Negative(operand, value))
    else Right(())

enum AscPerformanceValidationError derives CanEqual:
  case Blank(operand: String, value: String)
  case NonPositive(operand: String, value: Long)
  case Negative(operand: String, value: Long)
  case AllocationMeasurementUnavailable(operand: String, reason: String)

  def message: String = this match
    case Blank(operand, value) =>
      s"EyeLink performance operand='$operand' value='$value' is blank."
    case NonPositive(operand, value) =>
      s"EyeLink performance operand='$operand' value=$value is not positive."
    case Negative(operand, value) =>
      s"EyeLink performance operand='$operand' value=$value is negative."
    case AllocationMeasurementUnavailable(operand, reason) =>
      s"EyeLink performance operand='$operand' cannot be measured: $reason."

end AscPerformanceValidationError

// Test-scope evidence apparatus (CR9): its codes stay in IoDiagnosticCatalog,
// which only ever appends, and its Diagnose instance lives with the enum.
object AscPerformanceValidationError:
  given Diagnose[AscPerformanceValidationError, Nothing] =
    Diagnose.derived[AscPerformanceValidationError, Nothing](
      IoDiagnosticCatalog.ascPerformanceValidation
    )(_.message)
