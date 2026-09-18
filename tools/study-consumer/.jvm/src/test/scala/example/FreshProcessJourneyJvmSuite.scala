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
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** UI-G0 on the JVM: the fixation journey's run is written to a directory
  * in the application's layout, resolved from it through `ArtifactFiles`,
  * and reloaded by a separate, freshly started JVM ([[JourneyReader]])
  * running over the consumer's test classpath, which the build writes as a
  * resource and verify.py checks contains only this consumer's classes and
  * packaged artifacts. The reader shares no memory, registry or cache with
  * this JVM; it reads only the directory.
  */
class FreshProcessJourneyJvmSuite extends munit.CatsEffectSuite:
  import JourneySetup.get

  override def munitIOTimeout: Duration = 10.minutes

  private val root = Files.createTempDirectory("eyes4s-journey-")

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

  /** Run the reader in a new JVM; its output goes to a file, so a reader
    * that hangs is destroyed at the deadline rather than blocking a read.
    */
  private def launch(route: String, directory: Path): IO[Launch] = IO.blocking {
    val log     = Files.createTempFile(root, "reader-", ".log")
    val process = new ProcessBuilder(
      javaBinary,
      "-Xmx512m",
      "-cp",
      classpath,
      "example.JourneyReader",
      route,
      directory.toString
    ).redirectErrorStream(true).redirectOutput(log.toFile).start()
    if !process.waitFor(5, TimeUnit.MINUTES) then
      process.destroyForcibly()
      fail(s"the reader did not finish within 5 minutes:\n${Files.readString(log)}")
    val output = Files.readString(log)
    val line   = output.linesIterator
      .find(_.startsWith(JourneyReader.marker))
      .getOrElse(fail(s"the reader printed no receipt:\n$output"))
    assertEquals(process.exitValue, 0, output)
    Launch(process.pid, get(io.circe.parser.parse(line.drop(JourneyReader.marker.length))))
  }

  /** Store a saved run under the application's file layout. */
  private def store(directory: Path, saved: SavedManifest): IO[Unit] = IO.blocking {
    Files.createDirectories(directory)
    FixationJourney.files(saved).foreach { (name, bytes) =>
      val _ = Files.write(directory.resolve(name), IArray.genericWrapArray(bytes).toArray)
    }
  }

  /** The reader's receipt agrees with a study this JVM stored: the process
    * that printed it is the child launched, it resolved the stored address,
    * and its rerun has the stored result's canonical digest and every number
    * and failure of `result` in place.
    */
  private def reconstructed[K, P, S, D](
      j: Journey[K, P, S, D],
      reader: Launch,
      saved: SavedManifest,
      result: StudyResult[K, Px, S, D]
  ): Unit =
    import JourneySetup.render
    assertEquals(reader.field[String]("outcome"), "completed", reader.receipt.noSpaces)
    assertEquals(reader.field[Long]("process"), reader.pid)
    assertEquals(get(ByteDigest.parse(reader.field[String]("address"))), saved.address)
    assertEquals(reader.field[Vector[String]]("archived_bits"), render(j.bits(result)))
    assertEquals(reader.field[Vector[String]]("rerun_bits"), render(j.bits(result)))
    val stored = get(
      saved.artifacts.find(_.name.value == FixationJourney.resultEntry).toRight("no result")
    )
    assertEquals(
      get(ByteDigest.parse(reader.field[String]("rerun_sha256"))),
      stored.entry.sha256
    )

  private def freshProcess[K, P, S, D](c: JourneyCase[K, P, S, D]): Unit =
    test(
      s"${c.name}: a run stored in a directory reloads in a separate JVM and reruns bit for bit"
    ) {
      val j         = Journey(c)
      val directory = root.resolve(c.name)
      for
        (result, saved) <- j.saved
        events          <- j.events
        _               <- store(directory, saved)
        // This JVM, reading the directory through the packaged file source.
        local <- FixationJourney.load(
          c.route(),
          saved.address,
          ArtifactFiles.directory[IO](directory, FixationJourney.manifestFile)
        )
        reader <- launch(c.name, directory)
      yield
        val study = get(local)
        assert(j.same(study.result, result), "the directory holds the run's result")
        reconstructed(j, reader, saved, result)
        assertEquals(
          reader.field[String]("run"),
          JourneySetup.render(StudyRunId.of(j.work, JourneySetup.quanta))
        )
        assertEquals(reader.field[Vector[String]]("failures"), Vector.empty)
        // Identity reconstruction: the semantic digest, exact typed keys and 64-bit times.
        assertEquals(reader.field[String]("input"), j.input.reference.digest)
        assertEquals(
          reader
            .field[Vector[Json]]("keys")
            .map(json => get(c.route().persistence.keys.decode(json))),
          j.input.trials.rows.map(_.key)
        )
        assertEquals(
          reader.field[Vector[String]]("late").map(_.toLong),
          JourneyFixtures.onsets("s2", "c", "recall")
        )
        // The same step sequence as this JVM's run, and the same drill-down.
        val progress = events.collect { case StudyEvent.Advanced(p) => p }
        assertEquals(reader.field[Long]("steps"), progress.last.step)
        assertEquals(reader.field[Long]("total_units"), progress.last.totalUnits)
        assertEquals(
          reader.field[Vector[Int]]("records"),
          JourneyFixtures.records("s1", "a", "encode")
        )
        println(
          "EYES4S_FRESH_PROCESS=" + reader.receipt
            .deepMerge(
              Json.obj(
                "writer"   -> Json.fromLong(ProcessHandle.current.pid),
                "launched" -> Json.fromLong(reader.pid)
              )
            )
            .noSpaces
        )
    }

  freshProcess(JourneyCases.cosine)
  freshProcess(JourneyCases.scaled)

  test("failures and their denominators survive a separate JVM's reload and rerun") {
    val j         = Journey(JourneyCases.cosine)
    val directory = root.resolve("failures")
    val fine      = j.plan(j.input, Vector(StudyEstimate.Binned(), JourneySetup.gaussian(0.1)))
    val work      = get(fine.prepare(j.input, JourneySetup.pairs))
    for
      outcome <- j.runner
        .start(work, JourneySetup.comparison, JourneySetup.quanta)
        .use(_.outcome)
      result = j.completed(outcome)
      saved  = get(FixationJourney.save(j.route, fine, j.input, j.ledger, result))
      _      <- store(directory, saved)
      reader <- launch(JourneyCases.cosine.name, directory)
    yield
      reconstructed(j, reader, saved, result)
      val failures = get(ResultInspection.study(fine, result, j.input, Some(j.ledger))).failures
      assertEquals(failures.size, 48)
      assertEquals(reader.field[Vector[String]]("failures"), failures.map(_.code.render))
      assert(j.bits(result).contains(None), "the fingerprint marks the failures in place")
  }

  test("the separate reader refuses a changed and a missing file by name") {
    val j         = Journey(JourneyCases.cosine)
    val directory = root.resolve("perturbed")
    for
      (_, saved) <- j.saved
      _          <- store(directory, saved)
      _          <- IO.blocking {
        val result = directory.resolve(FixationJourney.resultEntry)
        val bytes  = Files.readAllBytes(result)
        bytes(40) = (bytes(40) ^ 1).toByte
        val _ = Files.write(result, bytes)
        Files.delete(directory.resolve(FixationJourney.ledgerEntry))
      }
      reader <- launch(JourneyCases.cosine.name, directory)
    yield
      assertEquals(reader.field[String]("outcome"), "refused", reader.receipt.noSpaces)
      assertEquals(
        reader
          .field[Vector[Json]]("errors")
          .map(e => get(e.hcursor.get[String]("code")) -> get(e.hcursor.get[String]("entry"))),
        Vector(
          "resolve.missing" -> FixationJourney.ledgerEntry,
          "resolve.digest"  -> FixationJourney.resultEntry
        )
      )
  }
