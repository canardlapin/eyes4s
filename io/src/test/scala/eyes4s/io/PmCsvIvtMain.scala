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

import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import java.security.MessageDigest
import java.util.Locale
import java.lang.management.ManagementFactory

/** Separate-process PM3.2 adapter. Invoked directly with java, outside sbt startup. */
object PmCsvIvtMain:
  private val source   = "steps-csv"
  private val clock    = ClockId("steps-500hz")
  private val validity = ValidityCodebook
    .of(tracked = Set("1"), lost = Set("0"))
    .fold(e => sys.error(e.message), identity)
  private val schema = DelimitedSchema
    .of(
      Delimiter.Comma,
      HeaderMode.FirstLine,
      TimeColumn("time_ms", TimestampUnit.Milliseconds),
      PositionColumns("x_px", "y_px", CoordinateUnit.Pixels),
      ValidityColumn("valid", validity),
      Some(PupilColumn("pupil", PupilUnit.Arbitrary))
    )
    .fold(e => sys.error(e.message), identity)
  private val display = Frame
    .screen("steps-display", 1920, 1080)
    .fold(e => sys.error(e.message), identity)
  private val viewing = Viewing
    .millimetres(600.0, 530.0, 298.0)
    .fold(e => sys.error(e.message), identity)
  private val angular = Frame
    .angular(
      "steps-angular",
      viewing.horizontalExtent.toDegrees,
      viewing.verticalExtent.toDegrees
    )
    .fold(e => sys.error(e.message), identity)
  private val threshold = IvtThreshold
    .of(
      Velocity
        .degPerSecond(30.0)
        .fold(e => sys.error(e.message), identity)
    )
    .fold(e => sys.error(e.message), identity)
  private val minimum = MinimumEventDuration
    .of(Span.millis(100))
    .fold(e => sys.error(e.message), identity)
  private val detector = Detectors.ivt(threshold, minimum, clock)

  private def format(value: Double): String =
    String.format(Locale.ROOT, "%.6f", Double.box(value))

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .map(b => f"${b & 0xff}%02x")
      .mkString

  private def execute(input: Path, output: Path): (Long, Int, Int, String, String) =
    val start    = System.nanoTime()
    val contents = Files.readString(input, StandardCharsets.UTF_8)
    val imported = Delimited
      .parse(source, contents, schema)
      .validate(display, clock, Rate.Irregular, Eye.Left)
    if !imported.isLosslessPartition || imported.rejectedCount != 0 ||
      imported.diagnostics.nonEmpty
    then
      sys.error(s"CSV admission failed: ${imported.diagnostics.map(_.message).mkString("; ")}")
    val recording = imported.recording.getOrElse(sys.error("CSV produced no recording"))
    val degrees   = recording
      .warp(Viewing.angularWarp(viewing, display, angular))
      .fold(e => sys.error(e.message), identity)
    val events = detector.runAll(degrees.samples.toVector)
    val lines  = new java.lang.StringBuilder(
      "onset_us,stop_us,samples,centre_x,centre_y,rms\n"
    )
    val native = new java.lang.StringBuilder("type,onset_us,stop_us\n")
    var count  = 0
    events.foreach {
      case Left(failure)                        => sys.error(failure.message)
      case Right(fixation: Event.Fixation[Deg]) =>
        native
          .append("fixation,")
          .append(fixation.span.onset.toMicros)
          .append(',')
          .append(fixation.span.offset.toMicros)
          .append('\n')
        val rms = fixation.dispersion.getOrElse(sys.error("fixation lacks RMS"))
        lines
          .append(fixation.span.onset.toMicros)
          .append(',')
          .append(fixation.span.offset.toMicros)
          .append(',')
          .append(fixation.sampleCount)
          .append(',')
          .append(format(fixation.centre.x))
          .append(',')
          .append(format(fixation.centre.y))
          .append(',')
          .append(format(rms.value))
          .append('\n')
        count += 1
      case Right(saccade: Event.Saccade[Deg]) =>
        native
          .append("saccade,")
          .append(saccade.span.onset.toMicros)
          .append(',')
          .append(saccade.span.offset.toMicros)
          .append('\n')
      case Right(_) => ()
    }
    Files.writeString(
      output,
      lines.toString,
      StandardCharsets.UTF_8,
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING
    )
    Files.writeString(
      Path.of(output.toString + ".native.csv"),
      native.toString,
      StandardCharsets.UTF_8,
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING
    )
    val elapsed     = System.nanoTime() - start
    val canonical   = sha256(Files.readAllBytes(output))
    val nativeTable = sha256(Files.readAllBytes(Path.of(output.toString + ".native.csv")))
    (elapsed, imported.acceptedCount, count, canonical, nativeTable)

  def main(args: Array[String]): Unit =
    if args.length != 4 && args.length != 5 then
      sys.error("usage: PmCsvIvtMain INPUT OUTPUT WARMUPS SAMPLES [diagnostic]")
    val input      = Path.of(args(0))
    val output     = Path.of(args(1))
    val warmups    = args(2).toInt
    val samples    = args(3).toInt
    val diagnostic = args.length == 5 && args(4) == "diagnostic"
    if args.length == 5 && !diagnostic then sys.error("unknown diagnostic mode")
    if warmups < 0 || samples < 1 then sys.error("invalid warmup/sample count")
    val readyNs       = System.nanoTime()
    val warmupResults = (0 until warmups).map(_ => execute(input, output)).toVector
    val threadBean    = ManagementFactory.getThreadMXBean
      .asInstanceOf[com.sun.management.ThreadMXBean]
    if diagnostic && !threadBean.isThreadAllocatedMemoryEnabled then
      threadBean.setThreadAllocatedMemoryEnabled(true)
    val currentThread    = Thread.currentThread().getId
    val beforeAllocation =
      if diagnostic then threadBean.getThreadAllocatedBytes(currentThread) else 0L
    val collectors    = ManagementFactory.getGarbageCollectorMXBeans
    def gcCount: Long = collectors.toArray.toVector
      .map {
        _.asInstanceOf[java.lang.management.GarbageCollectorMXBean].getCollectionCount
      }
      .filter(_ >= 0)
      .sum
    def gcMillis: Long = collectors.toArray.toVector
      .map {
        _.asInstanceOf[java.lang.management.GarbageCollectorMXBean].getCollectionTime
      }
      .filter(_ >= 0)
      .sum
    val beforeGcCount  = if diagnostic then gcCount else 0L
    val beforeGcMillis = if diagnostic then gcMillis else 0L
    val results        = (0 until samples).map(_ => execute(input, output)).toVector
    val allocated      = if diagnostic then
      threadBean.getThreadAllocatedBytes(currentThread) - beforeAllocation
    else 0L
    val collections = if diagnostic then gcCount - beforeGcCount else 0L
    val pauseNs     = if diagnostic then (gcMillis - beforeGcMillis) * 1000000L else 0L
    if (warmupResults ++ results).map(r => (r._2, r._3, r._4, r._5)).distinct.size != 1
    then sys.error("iteration output changed")
    val bytes       = Files.readAllBytes(output)
    val nativeBytes = Files.readAllBytes(Path.of(output.toString + ".native.csv"))
    if diagnostic then
      System.gc() // Diagnostic run only; never inside the timed interval.
      Thread.sleep(100)
    val retained =
      if diagnostic then ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed else 0L
    val diagnosticJson = if diagnostic then
      s",\"diagnostic\":{\"allocated_bytes\":$allocated," +
        s"\"retained_heap_bytes\":$retained,\"gc_pause_ns\":$pauseNs," +
        s"\"gc_collections\":$collections}"
    else ""
    println(
      s"{\"ready_ns\":$readyNs,\"iterations_ns\":[${results.map(_._1).mkString(",")}]," +
        s"\"accepted_rows\":${results.head._2},\"fixations\":${results.head._3}," +
        s"\"output_sha256\":\"${sha256(bytes)}\",\"output_bytes\":${bytes.length}" +
        s",\"native_sha256\":\"${sha256(nativeBytes)}\"" +
        s"$diagnosticJson}"
    )

end PmCsvIvtMain
