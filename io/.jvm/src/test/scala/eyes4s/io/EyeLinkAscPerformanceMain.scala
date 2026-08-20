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

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import _root_.fs2.Chunk
import _root_.fs2.Stream

import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption
import java.time.Instant
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicLong

import scala.util.control.NonFatal

private[io] enum EyeLinkPerformancePayload:
  case Samples
  case OversizedLines

private[io] final case class EyeLinkPerformanceWorkload(
    id: String,
    nominalRateHz: Int,
    durationSeconds: Long,
    eyeLayout: AscEyeLayout,
    messageEverySamples: Option[Int],
    mode: AscPerformanceMode,
    payload: EyeLinkPerformancePayload,
    lineLimitBytes: Int,
    chunkBytes: Int
)

private[io] final case class EyeLinkPerformanceMetadata(
    measuredAtUtc: String,
    sourceRevision: String,
    sourceDigest: Sha256,
    sourceDirty: Boolean,
    hardware: String,
    operatingSystem: String,
    runtime: String,
    profile: String
)

private[io] object EyeLinkAscPerformanceHarness:
  val schemaVersion: String = "eyes4s-eyelink-performance-v1"

  val smokeWorkloads: Vector[EyeLinkPerformanceWorkload] = Vector(
    samples("250hz-monocular-left-60s", 250, 60, AscEyeLayout.Left),
    samples("500hz-monocular-right-60s", 500, 60, AscEyeLayout.Right),
    samples("1000hz-binocular-60s", 1000, 60, AscEyeLayout.Binocular),
    samples("2000hz-binocular-60s", 2000, 60, AscEyeLayout.Binocular),
    samples(
      "1000hz-binocular-message-rich-30s",
      1000,
      30,
      AscEyeLayout.Binocular,
      messageEverySamples = Some(10)
    ),
    samples(
      "1000hz-monocular-materialized-5s",
      1000,
      5,
      AscEyeLayout.Left,
      mode = AscPerformanceMode.Materialized
    ),
    EyeLinkPerformanceWorkload(
      "oversized-lines-64mib",
      250,
      1,
      AscEyeLayout.Left,
      None,
      AscPerformanceMode.Streaming,
      EyeLinkPerformancePayload.OversizedLines,
      64 * 1024,
      16 * 1024
    )
  )

  val scheduledWorkloads: Vector[EyeLinkPerformanceWorkload] = Vector(
    samples("250hz-monocular-left-10m", 250, 600, AscEyeLayout.Left),
    samples("500hz-monocular-right-10m", 500, 600, AscEyeLayout.Right),
    samples("1000hz-binocular-10m", 1000, 600, AscEyeLayout.Binocular),
    samples("2000hz-binocular-10m", 2000, 600, AscEyeLayout.Binocular),
    samples(
      "1000hz-binocular-message-rich-5m",
      1000,
      300,
      AscEyeLayout.Binocular,
      messageEverySamples = Some(10)
    ),
    samples(
      "2000hz-binocular-endurance-2h",
      2000,
      7200,
      AscEyeLayout.Binocular
    ),
    smokeWorkloads.last
  )

  def warmUp(): Unit =
    val workload = samples("warm-up", 1000, 1, AscEyeLayout.Left)
    val settings = AscStreamSettings
      .of("performance-warm-up.asc", workload.lineLimitBytes, workload.chunkBytes)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    generate(workload).stream
      .through(EyeLinkAscStreaming.pipe[IO](settings))
      .compile
      .drain
      .unsafeRunSync()

  def measure(
      metadata: EyeLinkPerformanceMetadata,
      workload: EyeLinkPerformanceWorkload
  ): Either[AscPerformanceValidationError, AscPerformanceResult] =
    val settings = AscStreamSettings
      .of(
        s"performance-${workload.id}.asc",
        workload.lineLimitBytes,
        workload.chunkBytes
      )
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val generated = generate(workload)

    forceGc()
    AllocationCounter.start().flatMap { allocations =>
      val memory       = ManagementFactory.getMemoryMXBean
      val baselineHeap = memory.getHeapMemoryUsage.getUsed
      val sampler      = HeapSampler.start(memory, baselineHeap)
      val started      = System.nanoTime()
      val emitted      = try
        workload.mode match
          case AscPerformanceMode.Streaming =>
            generated.stream
              .through(EyeLinkAscStreaming.pipe[IO](settings))
              .compile
              .fold(0L)((count, _) => count + 1L)
              .unsafeRunSync()
          case AscPerformanceMode.Materialized =>
            generated.stream
              .through(EyeLinkAscStreaming.pipe[IO](settings))
              .compile
              .toVector
              .map(_.length.toLong)
              .unsafeRunSync()
      catch
        case NonFatal(error) =>
          sampler.stop()
          allocations.stop()
          throw error
      val elapsed   = math.max(1L, System.nanoTime() - started)
      val peak      = sampler.stop()
      val delta     = math.max(0L, peak - baselineHeap)
      val allocated = allocations.stop()

      AscPerformanceResult.of(
        metadata.measuredAtUtc,
        metadata.sourceRevision,
        metadata.sourceDigest,
        metadata.sourceDirty,
        metadata.hardware,
        metadata.operatingSystem,
        metadata.runtime,
        metadata.profile,
        workload.id,
        workload.nominalRateHz,
        workload.durationSeconds,
        workload.eyeLayout,
        workload.messageEverySamples.nonEmpty,
        workload.mode,
        generated.inputBytes,
        generated.physicalLines,
        emitted,
        workload.lineLimitBytes.toLong,
        workload.chunkBytes.toLong,
        elapsed,
        delta,
        allocated,
        Runtime.getRuntime.maxMemory(),
        minimumBytesPerSecond = 1024L * 1024L,
        maximumPeakHeapBytes = 256L * 1024L * 1024L,
        maximumAllocationPermille = 512000L
      )
    }

  def render(results: Vector[AscPerformanceResult]): String =
    val header = Vector(
      "schema",
      "measured_at_utc",
      "source_revision",
      "source_sha256",
      "source_dirty",
      "hardware",
      "operating_system",
      "runtime",
      "profile",
      "workload",
      "nominal_rate_hz",
      "duration_seconds",
      "eye_layout",
      "message_rich",
      "mode",
      "input_bytes",
      "physical_lines",
      "emitted_lines",
      "line_budget_bytes",
      "chunk_budget_bytes",
      "elapsed_nanos",
      "throughput_bytes_per_second",
      "peak_heap_delta_bytes",
      "allocated_bytes",
      "allocation_bytes_per_input_byte_permille",
      "jvm_maximum_heap_bytes",
      "minimum_bytes_per_second",
      "maximum_peak_heap_bytes",
      "maximum_allocation_permille",
      "passed"
    ).mkString("\t")
    val rows = results.map { result =>
      Vector(
        schemaVersion,
        result.measuredAtUtc,
        result.sourceRevision,
        result.sourceDigest.hex,
        result.sourceDirty.toString,
        result.hardware,
        result.operatingSystem,
        result.runtime,
        result.profile,
        result.workload,
        result.nominalRateHz.toString,
        result.durationSeconds.toString,
        result.eyeLayout.toString,
        result.messageRich.toString,
        result.mode.toString,
        result.inputBytes.bytes.toString,
        result.physicalLines.toString,
        result.emittedLines.toString,
        result.lineBudget.bytes.toString,
        result.chunkBudget.bytes.toString,
        result.elapsedNanos.toString,
        result.throughputBytesPerSecond.toString,
        result.peakHeapDeltaBytes.toString,
        result.allocatedBytes.toString,
        result.allocationBytesPerInputBytePermille.toString,
        result.jvmMaximumHeapBytes.toString,
        result.minimumBytesPerSecond.toString,
        result.maximumPeakHeapBytes.toString,
        result.maximumAllocationPermille.toString,
        result.passed.toString
      ).mkString("\t")
    }
    (header +: rows).mkString("\n") + "\n"

  private final case class Generated(
      stream: Stream[IO, Byte],
      inputBytes: Long,
      physicalLines: Long
  )

  private def samples(
      id: String,
      nominalRateHz: Int,
      durationSeconds: Long,
      eyeLayout: AscEyeLayout,
      messageEverySamples: Option[Int] = None,
      mode: AscPerformanceMode = AscPerformanceMode.Streaming
  ): EyeLinkPerformanceWorkload =
    EyeLinkPerformanceWorkload(
      id,
      nominalRateHz,
      durationSeconds,
      eyeLayout,
      messageEverySamples,
      mode,
      EyeLinkPerformancePayload.Samples,
      lineLimitBytes = 64 * 1024,
      chunkBytes = 64 * 1024
    )

  private def generate(workload: EyeLinkPerformanceWorkload): Generated =
    workload.payload match
      case EyeLinkPerformancePayload.Samples        => sampleDocument(workload)
      case EyeLinkPerformancePayload.OversizedLines => oversizedDocument

  private def sampleDocument(workload: EyeLinkPerformanceWorkload): Generated =
    val layout = workload.eyeLayout match
      case AscEyeLayout.Left      => "LEFT"
      case AscEyeLayout.Right     => "RIGHT"
      case AscEyeLayout.Binocular => "LEFT RIGHT"
    val sample = workload.eyeLayout match
      case AscEyeLayout.Binocular => "0\t100.0\t200.0\t500\t300.0\t400.0\t600\n"
      case _                      => "0\t100.0\t200.0\t500\n"
    val prefix =
      s"START\t0\t$layout\tSAMPLES\nPUPIL\tAREA\nSAMPLES\tGAZE\t$layout\tRATE\t${workload.nominalRateHz}.00\n"
    val suffix         = "END\t1\n"
    val samples        = workload.nominalRateHz.toLong * workload.durationSeconds
    val samplesPerBody = 1000L
    val fullBodies     = samples / samplesPerBody
    val remainder      = (samples % samplesPerBody).toInt
    val fullBody       = body(sample, samplesPerBody.toInt, workload.messageEverySamples)
    val finalBody      = body(sample, remainder, workload.messageEverySamples)
    val stream         = bytes(prefix) ++
      bytes(fullBody).repeatN(fullBodies) ++
      bytes(finalBody) ++
      bytes(suffix)
    val messagesPerFullBody = workload.messageEverySamples.fold(0L) { every =>
      samplesPerBody / every.toLong
    }
    val remainderMessages = workload.messageEverySamples.fold(0L) { every =>
      remainder.toLong / every.toLong
    }
    val messages   = messagesPerFullBody * fullBodies + remainderMessages
    val inputBytes =
      prefix.getBytes(StandardCharsets.US_ASCII).length.toLong +
        fullBody.getBytes(StandardCharsets.US_ASCII).length.toLong * fullBodies +
        finalBody.getBytes(StandardCharsets.US_ASCII).length.toLong +
        suffix.getBytes(StandardCharsets.US_ASCII).length.toLong
    Generated(stream, inputBytes, samples + messages + 4L)

  private def body(
      sample: String,
      samples: Int,
      messageEverySamples: Option[Int]
  ): String =
    val builder = new java.lang.StringBuilder
    var index   = 1
    while index <= samples do
      builder.append(sample): Unit
      messageEverySamples.foreach { every =>
        if index % every == 0 then builder.append("MSG\t0 synthetic-throughput-marker\n"): Unit
      }
      index += 1
    builder.toString

  private def oversizedDocument: Generated =
    val lineBytes = 1024 * 1024
    val lines     = 64L
    val line      = ("X" * lineBytes) + "\n"
    Generated(
      bytes(line).repeatN(lines),
      (lineBytes.toLong + 1L) * lines,
      lines
    )

  private def bytes(value: String): Stream[IO, Byte] =
    if value.isEmpty then Stream.empty
    else
      Stream
        .chunk(Chunk.array(value.getBytes(StandardCharsets.US_ASCII)))
        .covary[IO]

  private def forceGc(): Unit =
    System.gc()
    Thread.sleep(50L)

  private final class HeapSampler private (
      running: AtomicBoolean,
      maximum: AtomicLong,
      thread: Thread
  ):
    def stop(): Long =
      running.set(false)
      thread.join()
      maximum.get()

  private object HeapSampler:
    def start(
        memory: java.lang.management.MemoryMXBean,
        initial: Long
    ): HeapSampler =
      val running = new AtomicBoolean(true)
      val maximum = new AtomicLong(initial)
      val thread  = new Thread(
        () =>
          while running.get() do
            val observed = memory.getHeapMemoryUsage.getUsed
            maximum.accumulateAndGet(observed, java.lang.Math.max)
            Thread.sleep(2L)
        ,
        "eyes4s-eyelink-heap-sampler"
      )
      thread.setDaemon(true)
      thread.start()
      new HeapSampler(running, maximum, thread)

  private final class AllocationCounter private (
      bean: com.sun.management.ThreadMXBean,
      before: Map[Long, Long]
  ):
    def stop(): Long =
      val after = AllocationCounter.snapshot(bean)
      after.iterator.map { case (thread, allocated) =>
        math.max(0L, allocated - before.getOrElse(thread, 0L))
      }.sum

  private object AllocationCounter:
    def start(): Either[AscPerformanceValidationError, AllocationCounter] =
      ManagementFactory.getThreadMXBean match
        case bean: com.sun.management.ThreadMXBean =>
          if !bean.isThreadAllocatedMemorySupported then
            Left(
              AscPerformanceValidationError.AllocationMeasurementUnavailable(
                "allocatedBytes",
                "the JVM does not support per-thread allocated-byte counters"
              )
            )
          else
            if !bean.isThreadAllocatedMemoryEnabled then
              bean.setThreadAllocatedMemoryEnabled(true)
            Right(new AllocationCounter(bean, snapshot(bean)))
        case other =>
          Left(
            AscPerformanceValidationError.AllocationMeasurementUnavailable(
              "allocatedBytes",
              s"thread bean '${other.getClass.getName}' does not expose HotSpot allocation counters"
            )
          )

    private def snapshot(bean: com.sun.management.ThreadMXBean): Map[Long, Long] =
      val threads = bean.getAllThreadIds
      val values  = bean.getThreadAllocatedBytes(threads)
      threads.iterator
        .zip(values.iterator)
        .collect { case (thread, value) if value >= 0L => thread -> value }
        .toMap

end EyeLinkAscPerformanceHarness

object EyeLinkAscPerformanceMain:
  def main(arguments: Array[String]): Unit =
    parse(arguments.toVector) match
      case Left(message) =>
        System.err.println(message)
        sys.exit(2)
      case Right(options) =>
        EyeLinkAscPerformanceHarness.warmUp()
        val workloads = options.profile match
          case "smoke"     => EyeLinkAscPerformanceHarness.smokeWorkloads
          case "scheduled" => EyeLinkAscPerformanceHarness.scheduledWorkloads
        val metadata = EyeLinkPerformanceMetadata(
          options.measuredAtUtc,
          options.sourceRevision,
          options.sourceDigest,
          options.sourceDirty,
          options.hardware,
          s"${System.getProperty("os.name")} ${System.getProperty("os.version")} ${System.getProperty("os.arch")}",
          s"${System.getProperty("java.vm.name")} ${System.getProperty("java.runtime.version")}",
          options.profile
        )
        val results = workloads.map(workload =>
          EyeLinkAscPerformanceHarness
            .measure(metadata, workload)
            .fold(
              error => throw new IllegalArgumentException(error.message),
              identity
            )
        )
        val rendered = EyeLinkAscPerformanceHarness.render(results)
        options.output match
          case Some(path) =>
            Files.writeString(
              path,
              rendered,
              StandardCharsets.UTF_8,
              StandardOpenOption.CREATE,
              StandardOpenOption.TRUNCATE_EXISTING,
              StandardOpenOption.WRITE
            ): Unit
          case None => print(rendered)
        if results.exists(!_.passed) then sys.exit(1)

  private final case class Options(
      profile: String,
      measuredAtUtc: String,
      sourceRevision: String,
      sourceDigest: Sha256,
      sourceDirty: Boolean,
      hardware: String,
      output: Option[Path]
  )

  private def parse(arguments: Vector[String]): Either[String, Options] =
    def value(flag: String): Option[String] =
      arguments.sliding(2).collectFirst { case Vector(`flag`, selected) => selected }

    val profile = value("--profile").getOrElse("smoke")
    if profile != "smoke" && profile != "scheduled" then
      Left(s"unsupported --profile value='$profile'; expected smoke or scheduled")
    else
      val revision = value("--source-revision").getOrElse("unrecorded")
      val digest   = value("--source-digest").getOrElse(Sha256.ofUtf8("unrecorded").hex)
      val measured = value("--measured-at").getOrElse(Instant.now().toString)
      val hardware = value("--hardware").getOrElse(
        s"${Runtime.getRuntime.availableProcessors()} processors"
      )
      val dirty = value("--source-dirty") match
        case None          => Right(true)
        case Some("true")  => Right(true)
        case Some("false") => Right(false)
        case Some(other)   =>
          Left(s"unsupported --source-dirty value='$other'; expected true or false")
      for
        parsedDigest <- Sha256.fromHex("--source-digest", digest).left.map(_.message)
        parsedDirty  <- dirty
      yield Options(
        profile,
        measured,
        revision,
        parsedDigest,
        parsedDirty,
        hardware,
        value("--output").map(Path.of(_))
      )

end EyeLinkAscPerformanceMain
