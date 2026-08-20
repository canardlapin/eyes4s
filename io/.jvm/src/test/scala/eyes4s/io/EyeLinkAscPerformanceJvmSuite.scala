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

import scala.io.Source
import scala.util.Using

class EyeLinkAscPerformanceJvmSuite extends munit.FunSuite:
  private val settings = AscStreamSettings
    .of("performance-contract.asc", 1024, 256)
    .fold(error => fail(error.message), identity)

  test("streaming source-byte envelope is derived from byte budgets") {
    val envelope = AscStreamingMemoryEnvelope
      .from(settings)
      .fold(error => fail(error.message), identity)

    assertEquals(envelope.maximumLineBytes.bytes, 1024L)
    assertEquals(envelope.maximumReadChunkBytes.bytes, 256L)
    assertEquals(envelope.maximumLogicalInputBytesPerStep.bytes, 1280L)
    assert(AscByteBudget.of("heap", 0L).isLeft)
    assert(AscByteBudget.of("heap", -1L).isLeft)
  }

  test("materialized workflow reports input-proportional retention") {
    val input = AscByteBudget.of("input", 1000000L).fold(error => fail(error.message), identity)
    val streaming = AscWorkflowMemoryEnvelope
      .forInput(AscPerformanceMode.Streaming, settings, input)
      .fold(error => fail(error.message), identity)
    val materialized = AscWorkflowMemoryEnvelope
      .forInput(AscPerformanceMode.Materialized, settings, input)
      .fold(error => fail(error.message), identity)

    streaming match
      case AscWorkflowMemoryEnvelope.Streaming(parser) =>
        assertEquals(parser.maximumLogicalInputBytesPerStep.bytes, 1280L)
      case other => fail(s"expected bounded streaming envelope, found=$other")
    materialized match
      case AscWorkflowMemoryEnvelope.Materialized(parser, retainedInput) =>
        assertEquals(parser.maximumLogicalInputBytesPerStep.bytes, 1280L)
        assertEquals(retainedInput.bytes, 1000000L)
      case other => fail(s"expected input-proportional materialized envelope, found=$other")
  }

  test("performance result derives status from stored count, throughput, and heap thresholds") {
    def result(emitted: Long, minimum: Long, heap: Long, allocationPermille: Long = 1000L) =
      AscPerformanceResult
        .of(
          measuredAtUtc = "2026-08-15T00:00:00Z",
          sourceRevision = "revision",
          sourceDigest = Sha256.ofUtf8("sources"),
          sourceDirty = true,
          hardware = "test machine",
          operatingSystem = "test os",
          runtime = "test jvm",
          profile = "smoke",
          workload = "fixture",
          nominalRateHz = 1000,
          durationSeconds = 1L,
          eyeLayout = AscEyeLayout.Left,
          messageRich = false,
          mode = AscPerformanceMode.Streaming,
          inputBytes = 1000000L,
          physicalLines = 1000L,
          emittedLines = emitted,
          lineBudgetBytes = 1024L,
          chunkBudgetBytes = 256L,
          elapsedNanos = 100000000L,
          peakHeapDeltaBytes = heap,
          allocatedBytes = allocationPermille * 1000L,
          jvmMaximumHeapBytes = 256L * 1024L * 1024L,
          minimumBytesPerSecond = minimum,
          maximumPeakHeapBytes = 256L * 1024L * 1024L,
          maximumAllocationPermille = 2000L
        )
        .fold(error => fail(error.message), identity)

    assert(result(1000L, 1000000L, 1024L).passed)
    assert(!result(999L, 1000000L, 1024L).passed)
    assert(!result(1000L, 20000000L, 1024L).passed)
    assert(!result(1000L, 1000000L, 300L * 1024L * 1024L).passed)
    assert(!result(1000L, 1000000L, 1024L, allocationPermille = 3000L).passed)
  }

  test("smoke and scheduled workload matrices cover the declared performance dimensions") {
    val smoke     = EyeLinkAscPerformanceHarness.smokeWorkloads
    val scheduled = EyeLinkAscPerformanceHarness.scheduledWorkloads

    assertEquals(smoke.map(_.nominalRateHz).toSet, Set(250, 500, 1000, 2000))
    assert(smoke.exists(_.eyeLayout == AscEyeLayout.Left))
    assert(smoke.exists(_.eyeLayout == AscEyeLayout.Right))
    assert(smoke.exists(_.eyeLayout == AscEyeLayout.Binocular))
    assert(smoke.exists(_.messageEverySamples.nonEmpty))
    assert(smoke.exists(_.mode == AscPerformanceMode.Materialized))
    assert(smoke.exists(_.payload == EyeLinkPerformancePayload.OversizedLines))
    assert(
      scheduled.exists(workload =>
        workload.nominalRateHz == 2000 && workload.durationSeconds == 7200L
      )
    )
  }

  test("packaged baseline reconstructs validated passing performance results") {
    val resource = "/META-INF/eyes4s/eyelink/performance-baseline.tsv"
    val stream   = Option(getClass.getResourceAsStream(resource)).getOrElse(
      fail(s"missing packaged EyeLink performance baseline resource=$resource")
    )
    val lines = Using.resource(Source.fromInputStream(stream, "UTF-8"))(
      _.getLines().toVector
    )

    assertEquals(lines.length, 8)
    val results = lines.tail.map { line =>
      val fields = line.split("\t", -1).toVector
      assertEquals(fields.length, 30)
      assertEquals(fields(0), EyeLinkAscPerformanceHarness.schemaVersion)

      def long(index: Int, operand: String): Long =
        fields(index).toLongOption.getOrElse(
          fail(s"baseline operand=$operand value='${fields(index)}' is not a Long")
        )
      def integer(index: Int, operand: String): Int =
        fields(index).toIntOption.getOrElse(
          fail(s"baseline operand=$operand value='${fields(index)}' is not an Int")
        )
      def bool(index: Int, operand: String): Boolean = fields(index) match
        case "true"  => true
        case "false" => false
        case other   => fail(s"baseline operand=$operand value='$other' is not a Boolean")
      val layout = fields(12) match
        case "Left"      => AscEyeLayout.Left
        case "Right"     => AscEyeLayout.Right
        case "Binocular" => AscEyeLayout.Binocular
        case other       => fail(s"baseline eye_layout='$other' is unsupported")
      val mode = fields(14) match
        case "Streaming"    => AscPerformanceMode.Streaming
        case "Materialized" => AscPerformanceMode.Materialized
        case other          => fail(s"baseline mode='$other' is unsupported")

      val result = AscPerformanceResult
        .of(
          measuredAtUtc = fields(1),
          sourceRevision = fields(2),
          sourceDigest = Sha256
            .fromHex("performance source", fields(3))
            .fold(error => fail(error.message), identity),
          sourceDirty = bool(4, "source_dirty"),
          hardware = fields(5),
          operatingSystem = fields(6),
          runtime = fields(7),
          profile = fields(8),
          workload = fields(9),
          nominalRateHz = integer(10, "nominal_rate_hz"),
          durationSeconds = long(11, "duration_seconds"),
          eyeLayout = layout,
          messageRich = bool(13, "message_rich"),
          mode = mode,
          inputBytes = long(15, "input_bytes"),
          physicalLines = long(16, "physical_lines"),
          emittedLines = long(17, "emitted_lines"),
          lineBudgetBytes = long(18, "line_budget_bytes"),
          chunkBudgetBytes = long(19, "chunk_budget_bytes"),
          elapsedNanos = long(20, "elapsed_nanos"),
          peakHeapDeltaBytes = long(22, "peak_heap_delta_bytes"),
          allocatedBytes = long(23, "allocated_bytes"),
          jvmMaximumHeapBytes = long(25, "jvm_maximum_heap_bytes"),
          minimumBytesPerSecond = long(26, "minimum_bytes_per_second"),
          maximumPeakHeapBytes = long(27, "maximum_peak_heap_bytes"),
          maximumAllocationPermille = long(28, "maximum_allocation_permille")
        )
        .fold(error => fail(error.message), identity)
      assertEquals(result.throughputBytesPerSecond, long(21, "throughput_bytes_per_second"))
      assertEquals(
        result.allocationBytesPerInputBytePermille,
        long(24, "allocation_bytes_per_input_byte_permille")
      )
      assertEquals(result.passed, bool(29, "passed"))
      assert(result.passed)
      result
    }

    assert(results.forall(_.profile == "scheduled"))
    assert(results.forall(_.sourceDirty))
    assert(
      results.exists(result =>
        result.workload == "2000hz-binocular-endurance-2h" &&
          result.physicalLines == 14400004L
      )
    )
  }

end EyeLinkAscPerformanceJvmSuite
