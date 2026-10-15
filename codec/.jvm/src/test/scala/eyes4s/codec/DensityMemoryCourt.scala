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

package eyes4s.codec

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.*
import io.circe.Json
import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** Opt-in bounded-heap court. Write and read are deliberately separate JVMs.
  * The read directory contains only the saved result and verified packed chunks.
  */
object DensityMemoryCourt:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => throw new IllegalStateException(error.toString), identity)
  private val results  = StudyResultCodecs.cosine[Px]
  private val archives = new DensityArchiveCodec(results)

  private def phase[A](name: String)(run: => A): A =
    System.gc()
    val started     = System.nanoTime()
    val value       = run
    val elapsed     = (System.nanoTime() - started) / 1000000L
    val withGarbage = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed
    System.gc()
    val live = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed
    println(
      "DENSITY_MEMORY=" + Json
        .obj(
          "phase"                   -> Json.fromString(name),
          "elapsed_ms"              -> Json.fromLong(elapsed),
          "live_heap_bytes"         -> Json.fromLong(live),
          "heap_with_garbage_bytes" -> Json.fromLong(withGarbage),
          "max_heap_bytes"          -> Json.fromLong(Runtime.getRuntime.maxMemory()),
          "process_id"              -> Json.fromLong(ManagementFactory.getRuntimeMXBean.getPid)
        )
        .noSpaces
    )
    value

  def main(args: Array[String]): Unit =
    require(Runtime.getRuntime.maxMemory() <= 1073741824L, "run in a fresh JVM with -Xmx1g")
    require(args.length >= 2, "write|read DIRECTORY [PARTICIPANTS STIMULI NX NY]")
    val directory = Path.of(args(1))
    if args(0) == "write" then
      val _            = Files.createDirectories(directory)
      val participants = args.lift(2).fold(5)(_.toInt)
      val stimuli      = args.lift(3).fold(10)(_.toInt)
      val nx           = args.lift(4).fold(256)(_.toInt)
      val ny           = args.lift(5).fold(128)(_.toInt)
      val frame        =
        Frame.of[Px](FrameId("archive-court"), get(Bounds.of[Px](0, 0, 1024, 512)), YAxis.Down)
      val grid  = get(Grid.over(frame, nx, ny))
      val input = phase("input") {
        val trials = (for
          p        <- 0 until participants
          stimulus <- 0 until stimuli
          ph       <- 0 until 3
        yield
          val phase = Vector("encode", "recall", "recall2")(ph)
          val key   = StudyKey(s"p$p", s"item$stimulus", phase)
          val clock = ClockId(s"p$p/item$stimulus/$phase")
          val fixes = (0 until 32).map { index =>
            val onset = index * 160000L
            get(
              Event.Fixation.withoutDispersion(
                get(
                  Interval.of(
                    clock,
                    Instant.micros(onset),
                    Instant.micros(onset + 100000L + index * 911L)
                  )
                ),
                Pt[Px](
                  ((p * 113 + stimulus * 71 + ph * 37 + index * 29) % 1023) + .375,
                  ((p * 59 + stimulus * 17 + ph * 83 + index * 41)  % 511) + .625
                ),
                1
              )
            )
          }
          Trial(key, (), get(Scanpath.of(frame, clock, IArray.from(fixes))))
        ).toVector
        StudyInput(Trials(trials))
      }
      val plan = get(
        StudyPlan.of(
          input.reference,
          StudyKey.layout(DefinitionId.studyLayout),
          grid,
          "recall",
          "encode",
          Weight.Duration,
          Vector(4.0, 8.0, 12.0, 16.0).map(s =>
            StudyEstimate.Gaussian(get(Sigma.px(s)), EdgePolicy.Truncate)
          ),
          FailurePolicy.RequireAll,
          StudyMethod.cosine[Px](DefinitionId.cosine),
          ()
        )
      )
      val result   = phase("analysis")(get(plan.run(input)))
      val expected = phase("result-identity")(get(results.codec.digest(result)).sha256.hex)
      val bundle   = phase("pack")(get(archives.encode(result, DensityStorage.Packed)))
      val document = phase("save") {
        bundle.chunks.foreach { (_, chunk) =>
          val _ = Files.write(
            directory.resolve(chunk.ref.sha256.hex + ".bin"),
            chunk.bytes.asInstanceOf[Array[Byte]]
          )
        }
        val value = Json.obj(
          "format"   -> Json.fromString("eyes4s.density-memory-court/1"),
          "expected" -> Json.fromString(expected),
          "archive"  -> get(archives.codec.encode(bundle.archive)),
          "payloads" -> Json.arr(bundle.chunks.map((_, chunk) => PayloadRef.json(chunk.ref))*)
        )
        val _ = Files.writeString(
          directory.resolve("index.json"),
          value.noSpaces,
          StandardCharsets.UTF_8
        )
        value
      }
      val retained =
        phase("retained-result-identity")(get(results.codec.digest(result)).sha256.hex)
      require(retained == expected)
      println(
        "DENSITY_MEMORY_SUMMARY=" + Json
          .obj(
            "mode"                -> Json.fromString("write"),
            "trials"              -> Json.fromInt(participants * stimuli * 3),
            "fixations_per_trial" -> Json.fromInt(32),
            "scales"              -> Json.fromInt(result.scales.size),
            "nx"                  -> Json.fromInt(nx),
            "ny"                  -> Json.fromInt(ny),
            "packed_bytes"        -> Json.fromLong(bundle.chunks.map(_._2.ref.length).sum),
            "document_bytes"      -> Json.fromLong(
              document.noSpaces.getBytes(StandardCharsets.UTF_8).length.toLong
            ),
            "expected" -> Json.fromString(expected)
          )
          .noSpaces
      )
    else if args(0) == "read" then
      val document = phase("open")(
        get(io.circe.parser.parse(Files.readString(directory.resolve("index.json"))))
      )
      require(get(document.hcursor.get[String]("format")) == "eyes4s.density-memory-court/1")
      val chunks = phase("verify-payloads") {
        get(document.hcursor.get[Vector[Json]]("payloads")).map { entry =>
          val ref   = get(PayloadRef.read(entry))
          val bytes =
            IArray.from(Files.readAllBytes(directory.resolve(ref.sha256.hex + ".bin")))
          ref -> get(VerifiedPayload.verify(ref, bytes))
        }.toMap
      }
      val archive =
        phase("archive")(get(archives.codec.decode(get(document.hcursor.get[Json]("archive")))))
      val result = phase("restore")(get(archive.materialize(chunks.get)))
      val digest = phase("restored-identity")(get(results.codec.digest(result)).sha256.hex)
      require(digest == get(document.hcursor.get[String]("expected")))
      println(
        "DENSITY_MEMORY_SUMMARY=" + Json
          .obj(
            "mode"         -> Json.fromString("read"),
            "source_free"  -> Json.fromBoolean(true),
            "maps"         -> Json.fromInt(result.scales.map(_.estimation.size).sum),
            "packed_bytes" -> Json.fromLong(chunks.keysIterator.map(_.length).sum),
            "expected"     -> Json.fromString(digest)
          )
          .noSpaces
      )
    else throw new IllegalArgumentException("mode must be write or read")
