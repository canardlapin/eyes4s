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

package example

import cats.effect.IO
import eyes4s.codec.*
import eyes4s.fs2.*
import eyes4s.io.ArtifactFiles
import io.circe.Json

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** UI-G1 on the JVM: the recording and temporal routes' runs are written to
  * directories in the application's layout, resolved from them through
  * `ArtifactFiles`, and reloaded and rerun by a separate, freshly started JVM
  * ([[RouteReader]]) over the consumer's test classpath, which verify.py
  * checks holds only this consumer's classes and the packaged artifacts. The
  * reader shares no memory, registry or cache with this JVM; it reads only the
  * directory. A rerun on the JVM is bit for bit: every number of the result
  * (including the trigonometric angular warp and the Gaussian smoothing) and
  * the canonical archive's SHA-256.
  */
class FreshProcessRoutesJvmSuite extends munit.CatsEffectSuite:
  import JourneySetup.get

  override def munitIOTimeout: Duration = 10.minutes

  private val root = Files.createTempDirectory("eyes4s-routes-")

  override def afterAll(): Unit =
    // Only this suite's own temporary directory is removed.
    Using.resource(Files.walk(root))(_.iterator.asScala.toVector).reverse.foreach(Files.delete)

  private lazy val classpath: String =
    val stream = Option(getClass.getResourceAsStream("/example/fresh-process/classpath.txt"))
      .getOrElse(fail("the build did not generate the fresh-process classpath resource"))
    try String(stream.readAllBytes(), "UTF-8")
    finally stream.close()

  private val javaBinary = Paths.get(System.getProperty("java.home"), "bin", "java").toString

  private final case class Launch(pid: Long, receipt: Json):
    def field[A: io.circe.Decoder](name: String): A = get(receipt.hcursor.get[A](name))

  /** Run the reader in a new JVM; its output goes to a file, so a reader that
    * hangs is destroyed at the deadline rather than blocking a read.
    */
  private def launch(route: String, directory: Path): IO[Launch] = IO.blocking {
    val log     = Files.createTempFile(root, "reader-", ".log")
    val process = new ProcessBuilder(
      javaBinary,
      "-Xmx512m",
      "-cp",
      classpath,
      "example.RouteReader",
      route,
      directory.toString
    ).redirectErrorStream(true).redirectOutput(log.toFile).start()
    if !process.waitFor(5, TimeUnit.MINUTES) then
      process.destroyForcibly()
      fail(s"the reader did not finish within 5 minutes:\n${Files.readString(log)}")
    val output = Files.readString(log)
    val line   = output.linesIterator
      .find(_.startsWith(RouteReader.marker))
      .getOrElse(fail(s"the reader printed no receipt:\n$output"))
    assertEquals(process.exitValue, 0, output)
    Launch(process.pid, get(io.circe.parser.parse(line.drop(RouteReader.marker.length))))
  }

  private def store(directory: Path, saved: SavedManifest): IO[Unit] = IO.blocking {
    Files.createDirectories(directory)
    SavedRun.files(saved).foreach { (name, bytes) =>
      val _ = Files.write(directory.resolve(name), IArray.genericWrapArray(bytes).toArray)
    }
  }

  private def entry(saved: SavedManifest, name: String): ManifestEntry =
    get(saved.manifest.entry(get(ArtifactName.of(name))).toRight(s"no entry $name"))

  private def printed(reader: Launch): Unit =
    println(
      "EYES4S_FRESH_ROUTE=" + reader.receipt
        .deepMerge(
          Json.obj(
            "writer"   -> Json.fromLong(ProcessHandle.current.pid),
            "launched" -> Json.fromLong(reader.pid)
          )
        )
        .noSpaces
    )

  private def recording[P](label: String, run: RecordingCase[P]): Unit =
    test(
      s"$label: a recording run stored in a directory reloads in a separate JVM, bit for bit"
    ) {
      val directory = root.resolve(label)
      for
        (analysis, saved) <- run.saved
        events            <- run.events
        _                 <- store(directory, saved)
        local             <- RecordingJourney.load(
          run.route,
          saved.address,
          ArtifactFiles.directory[IO](directory, SavedRun.manifestFile)
        )
        reader <- launch(label, directory)
      yield
        assert(run.same(get(local).analysis, analysis), "the directory holds the analysis")
        assertEquals(reader.field[String]("outcome"), "completed", reader.receipt.noSpaces)
        assertEquals(reader.field[Long]("process"), reader.pid)
        assertNotEquals(reader.pid, ProcessHandle.current.pid)
        assertEquals(get(ByteDigest.parse(reader.field[String]("address"))), saved.address)
        val bits = RecordingJourney.fingerprint(analysis).map(java.lang.Long.toHexString)
        assertEquals(reader.field[Vector[String]]("archived_bits"), bits)
        assertEquals(reader.field[Vector[String]]("rerun_bits"), bits)
        assertEquals(
          get(ByteDigest.parse(reader.field[String]("rerun_sha256"))),
          entry(saved, RecordingJourney.resultEntry).sha256
        )
        assertEquals(reader.field[String]("run"), run.id.toString)
        assertEquals(reader.field[String]("input"), RecordingFixtures.input.reference.digest)
        assertEquals(
          reader.field[String]("recording"),
          RecordingFixtures.recording.contentHash.render
        )
        val progress = events.collect { case RunEvent.Advanced(p) => p }
        assertEquals(reader.field[Long]("steps"), progress.last.step)
        assertEquals(reader.field[Long]("total_units"), progress.last.totalUnits)
        assertEquals(
          reader.field[Vector[Vector[Int]]]("events"),
          RecordingFixtures.support.map((a, b) => Vector(a, b))
        )
        printed(reader)
    }

  private def temporal[K, P, S, D](label: String, t: TemporalCase[K, P, S, D]): Unit =
    test(
      s"$label: a temporal run stored in a directory reloads in a separate JVM, bit for bit"
    ) {
      val directory = root.resolve(label)
      for
        (result, saved) <- t.saved
        events          <- t.events
        _               <- store(directory, saved)
        local           <- TemporalJourney.load(
          t.route,
          saved.address,
          ArtifactFiles.directory[IO](directory, SavedRun.manifestFile)
        )
        reader <- launch(label, directory)
      yield
        assert(t.same(get(local).result, result), "the directory holds the result")
        assertEquals(reader.field[String]("outcome"), "completed", reader.receipt.noSpaces)
        assertEquals(reader.field[Long]("process"), reader.pid)
        assertNotEquals(reader.pid, ProcessHandle.current.pid)
        assertEquals(get(ByteDigest.parse(reader.field[String]("address"))), saved.address)
        val bits = t.bits(result).map(_.fold("-")(java.lang.Long.toHexString))
        assertEquals(reader.field[Vector[String]]("archived_bits"), bits)
        assertEquals(reader.field[Vector[String]]("rerun_bits"), bits)
        assert(
          bits.contains("-"),
          "the fingerprint marks the unobserved window's failures in place"
        )
        assertEquals(
          get(ByteDigest.parse(reader.field[String]("rerun_sha256"))),
          entry(saved, TemporalJourney.resultEntry).sha256
        )
        assertEquals(reader.field[String]("run"), t.id.toString)
        assertEquals(reader.field[String]("base"), t.base.reference.digest)
        assertEquals(reader.field[String]("input"), t.input.reference.digest)
        val progress = events.collect { case RunEvent.Advanced(p) => p }
        assertEquals(reader.field[Long]("steps"), progress.last.step)
        assertEquals(reader.field[Long]("total_units"), progress.last.totalUnits)
        // Two unobserved cells, each with 108 located failures.
        assertEquals(reader.field[Vector[String]]("failures").size, 2 * 108)
        printed(reader)
    }

  recording("recording-ivt", RecordingRun.ivt)
  recording("recording-lab", RecordingRun.lab)
  temporal("temporal-cosine", TemporalRun.cosine)
  temporal("temporal-scaled", TemporalRun.scaled)

  test("the separate reader refuses a changed archive and a missing payload by name") {
    val run       = RecordingRun.ivt
    val directory = root.resolve("perturbed")
    for
      (_, saved) <- run.saved
      _          <- store(directory, saved)
      _          <- IO.blocking {
        val result = directory.resolve(RecordingJourney.resultEntry)
        val bytes  = Files.readAllBytes(result)
        bytes(40) = (bytes(40) ^ 1).toByte
        val _ = Files.write(result, bytes)
        Files.delete(directory.resolve(s"${RecordingJourney.recordingEntry}.values"))
      }
      reader <- launch("recording-ivt", directory)
    yield
      assertEquals(reader.field[String]("outcome"), "refused", reader.receipt.noSpaces)
      assertEquals(
        reader
          .field[Vector[Json]]("errors")
          .map(e => get(e.hcursor.get[String]("code")) -> get(e.hcursor.get[String]("entry"))),
        Vector(
          "resolve.missing" -> s"${RecordingJourney.recordingEntry}.values",
          "resolve.digest"  -> RecordingJourney.resultEntry
        )
      )
  }

  test("a separate reader without the laboratory registration refuses its archive by method") {
    val run       = RecordingRun.lab
    val directory = root.resolve("unregistered")
    for
      (_, saved) <- run.saved
      _          <- store(directory, saved)
      reader     <- launch("recording-ivt", directory)
    yield
      assertEquals(reader.field[String]("outcome"), "refused", reader.receipt.noSpaces)
      assertEquals(
        reader
          .field[Vector[Json]]("errors")
          .map(e =>
            (
              get(e.hcursor.get[String]("code")),
              get(e.hcursor.get[Vector[String]]("causes")),
              get(e.hcursor.get[String]("entry"))
            )
          ),
        Vector(
          ("resolve.decode", Vector("codec.unsupported-schema"), RecordingJourney.planEntry),
          ("resolve.decode", Vector("codec.missing-result-codec"), RecordingJourney.resultEntry)
        )
      )
  }
