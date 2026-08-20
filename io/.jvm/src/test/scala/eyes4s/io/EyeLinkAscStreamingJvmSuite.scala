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
import cats.effect.Ref
import cats.effect.unsafe.implicits.global

import _root_.fs2.Chunk
import _root_.fs2.Stream
import _root_.fs2.io.file.Files

import java.lang.management.ManagementFactory

import scala.concurrent.duration.*

class EyeLinkAscStreamingJvmSuite extends munit.FunSuite:

  private def bytes(value: String): Vector[Byte] =
    value.toVector.map(_.toByte)

  private def settings: AscStreamSettings =
    AscStreamSettings.of("fixture.asc", 1024, 7).toOption.get

  test("JVM Files streaming closes its resource and matches the pure framer") {
    val input    = bytes("START 1 LEFT SAMPLES\r\nSAMPLES GAZE LEFT RATE 1000\n2 1 2 3\nEND 3")
    val fromFile = Files[IO].tempFile
      .use { path =>
        Stream
          .emits(input)
          .through(Files[IO].writeAll(path))
          .compile
          .drain *> EyeLinkAscStreaming.readPath[IO](path, settings).compile.toVector
      }
      .unsafeRunSync()
    val pure = EyeLinkAscFraming.whole(settings, IArray.from(input.toArray))

    def summary(values: Vector[AscFramingEmission]): Vector[(Long, Long, Vector[Byte])] =
      values.collect { case AscFramingEmission.Parsed(value) =>
        (value.source.number, value.source.byteOffset, value.source.bytes.toVector)
      }

    assertEquals(summary(fromFile), summary(pure))
  }

  test("task-level path import is observationally equal to byte import") {
    val input = bytes(
      "START 1 LEFT SAMPLES\nSAMPLES GAZE LEFT RATE 1000\nPUPIL AREA\n1 10 20 30\nEND 2\n"
    )
    val frame = eyes4s.kernel.Frame
      .screen("path-display", 100, 100)
      .fold(error => fail(error.message), identity)
    val config = EyeLinkAscSessionConfig
      .of(
        frame,
        eyes4s.kernel.ClockId("path-clock"),
        ConversionEvidencePolicy.Exploratory,
        AscBlinkReconciliationPolicy.PreserveSampleValidity,
        AscUnspecifiedPupilPolicy.RejectMeasuredValues
      )
      .fold(error => fail(error.message), identity)
    val array    = IArray.from(input.toArray)
    val origin   = EyeLinkAscOrigin.Unidentified(Sha256.ofBytes(array))
    val fromFile = Files[IO].tempFile
      .use { path =>
        Stream
          .emits(input)
          .through(Files[IO].writeAll(path))
          .compile
          .drain *> EyeLinkAscImport.readPath[IO](path, origin, settings, config)
      }
      .unsafeRunSync()
    val fromBytes = EyeLinkAscImport.fromBytes(array, origin, settings, config)

    assertEquals(
      EyeLinkAscCanonicalManifest.from(fromFile).canonical,
      EyeLinkAscCanonicalManifest.from(fromBytes).canonical
    )
  }

  test("two hundred thousand lines stream without downstream materialization") {
    val linesPerChunk = 1000
    val chunkCount    = 200
    val chunk         = Chunk.from(bytes("2 1 2 3\n" * linesPerChunk))
    val count         = Stream
      .emits(0 until chunkCount)
      .flatMap(_ => Stream.chunk(chunk))
      .through(EyeLinkAscStreaming.pipe[IO](settings))
      .compile
      .fold(0L) { (total, emission) =>
        emission match
          case AscFramingEmission.Parsed(_)   => total + 1L
          case AscFramingEmission.Rejected(_) => total
      }
      .unsafeRunSync()

    assertEquals(count, linesPerChunk.toLong * chunkCount.toLong)
  }

  test("downstream cancellation runs the upstream finalizer") {
    var finalized = false
    val line      = bytes("MSG 1 repeat\n")
    val source    = Stream
      .bracket(IO.unit)(_ => IO.delay { finalized = true })
      .flatMap(_ => Stream.emits(line).repeat)

    source
      .through(EyeLinkAscStreaming.pipe[IO](settings))
      .take(1)
      .compile
      .drain
      .unsafeRunSync()

    assert(finalized)
  }

  test("cancellation never flushes a partial physical line as complete") {
    val observed = (for
      captured <- Ref.of[IO, Vector[AscFramingEmission]](Vector.empty)
      fiber    <- (Stream.emits(bytes("PARTIAL")).covary[IO] ++ Stream.never[IO])
        .through(EyeLinkAscStreaming.pipe[IO](settings))
        .evalMap(value => captured.update(_ :+ value))
        .compile
        .drain
        .start
      _      <- IO.sleep(20.millis)
      _      <- fiber.cancel
      values <- captured.get
    yield values).unsafeRunSync()

    assertEquals(observed, Vector.empty)
  }

  test("repeated path cancellation does not leak file descriptors") {
    val operatingSystem = ManagementFactory.getOperatingSystemMXBean
    val before          = operatingSystem match
      case unix: com.sun.management.UnixOperatingSystemMXBean =>
        Some(unix.getOpenFileDescriptorCount)
      case _ => None

    Files[IO].tempFile
      .use { path =>
        val input =
          Stream.emits(bytes("MSG 1 cancel\n" * 1000)).through(Files[IO].writeAll(path))
        val cancelOnce = EyeLinkAscStreaming
          .readPath[IO](path, settings)
          .take(1)
          .compile
          .drain
        input.compile.drain *> (0 until 256).foldLeft(IO.unit)((run, _) => run *> cancelOnce)
      }
      .unsafeRunSync()

    System.gc()
    Thread.sleep(50L)
    (before, operatingSystem) match
      case (Some(initial), unix: com.sun.management.UnixOperatingSystemMXBean) =>
        val after = unix.getOpenFileDescriptorCount
        assert(
          after <= initial + 32L,
          s"repeated EyeLink path cancellation retained file descriptors: before=$initial after=$after"
        )
      case _ => assert(true)
  }

end EyeLinkAscStreamingJvmSuite
