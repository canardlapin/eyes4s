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

import eyes4s.codec.*
import io.circe.Json

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** Fresh-process reconstruction (UI-S6): a saved study is written by one
  * JVM and resolved, verified, re-executed and compared bit for bit by
  * another, freshly started JVM that shares nothing with the writer but the
  * files and the published code. Both are launched from this test's
  * classpath; neither is this JVM.
  *
  * The writer must reproduce the pinned v1 archives byte for byte, and the
  * reader must find, for the fixation study, a re-executed result whose
  * canonical encoding has the archived entry's exact SHA-256 and, for every
  * study, a result whose every double has the bits the writer computed. A
  * corrupted artifact, a missing registration, a consistently re-declared
  * numeric payload and a consistently re-declared dropped exclusion are each
  * refused by name in the reader, without affecting the other studies.
  */
class FreshProcessReconstructionJvmSuite extends munit.FunSuite:
  import FreshProcessHarness.{marker, refusalStatus}

  override val munitTimeout: Duration = 20.minutes

  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)

  /** Digests of the pinned v1 resource files, computed with `shasum -a 256`.
    * The pinned study-v1.json differs from the writer's output in whitespace
    * only, so the plan is compared as a JSON value instead.
    */
  private val pinned = Map(
    "input"          -> "dd9922646e8ced281d3ed8f63fb38f36774c775ef836d95a76b66ebc664c3b52",
    "ledger"         -> "0efe86cc9a1f297b1001925a05db9025845a7e8d9be05862744fc2fea8c9e0e7",
    "refused-ledger" -> "114585a745fa08bbb6acbbdb58590e6e29f16fd8e53aa2da7d5f77bccd7c5447",
    "result"         -> "d8429e2f980919fde450192e0485cfbcfc7920923fb45ef4863ea7923869d9f0"
  )

  private lazy val classpath: String =
    val stream = Option(getClass.getResourceAsStream("/eyes4s/fresh-process/classpath.txt"))
      .getOrElse(fail("the build did not generate the fresh-process classpath resource"))
    try String(stream.readAllBytes(), "UTF-8")
    finally stream.close()

  private val java = Paths.get(System.getProperty("java.home"), "bin", "java").toString

  private final case class Run(status: Int, receipt: Json, output: String):
    def pid: Long                 = get(receipt.hcursor.downField("process").get[Long]("pid"))
    def study(name: String): Json = get(receipt.hcursor.downField("studies").get[Json](name))
    def outcome(name: String): String = get(study(name).hcursor.get[String]("outcome"))
    def field(name: String, key: String): String = get(study(name).hcursor.get[String](key))

  /** Run the harness in a new JVM over this test's classpath. */
  private def launch(arguments: String*): Run =
    val command =
      Vector(java, "-Xmx1g", "-cp", classpath, "eyes4s.io.FreshProcessHarness") ++ arguments
    val process = new ProcessBuilder(command*).redirectErrorStream(true).start()
    val output  = String(process.getInputStream.readAllBytes(), "UTF-8")
    assert(process.waitFor(10, TimeUnit.MINUTES), s"the harness did not finish:\n$output")
    val line = output.linesIterator
      .find(_.startsWith(marker))
      .getOrElse(fail(s"the harness printed no receipt:\n$output"))
    Run(process.exitValue, get(io.circe.parser.parse(line.drop(marker.length))), output)

  private val root: Path  = Files.createTempDirectory("eyes4s-fresh-process-")
  private val saved: Path = root.resolve("saved")

  override def afterAll(): Unit =
    // Only this suite's own temporary directory is removed.
    Files.walk(root).iterator.asScala.toVector.reverse.foreach(Files.deleteIfExists)

  private lazy val written: Run = launch("write", saved.toString)
  private lazy val reader: Run  =
    val _ = written
    launch("read", saved.toString)

  /** A private copy of the saved studies for one perturbation. */
  private def copy(label: String): Path =
    val _      = written
    val target = root.resolve(label)
    Files.walk(saved).iterator.asScala.toVector.foreach { source =>
      val destination = target.resolve(saved.relativize(source).toString)
      if Files.isDirectory(source) then Files.createDirectories(destination)
      else Files.copy(source, destination, StandardCopyOption.REPLACE_EXISTING)
    }
    target

  private def errors(
      run: Run,
      study: String
  ): Vector[(String, String, Option[String], String)] =
    get(run.study(study).hcursor.get[Vector[Json]]("errors")).map { e =>
      (
        get(e.hcursor.get[String]("entry")),
        get(e.hcursor.get[String]("error")),
        get(e.hcursor.get[Option[String]]("underlying")),
        get(e.hcursor.get[String]("message"))
      )
    }

  test("a writer JVM saves three studies and reproduces the pinned v1 archives byte for byte") {
    assertEquals(written.status, 0, written.output)
    assertNotEquals(written.pid, ProcessHandle.current.pid)
    val entries = get(written.study("fixation").hcursor.get[Vector[Json]]("entries")).map(e =>
      get(e.hcursor.get[String]("name")) -> get(e.hcursor.get[String]("sha256"))
    )
    assertEquals(
      entries.map(_._1),
      Vector("plan", "input", "ledger", "refused-ledger", "result")
    )
    assertEquals(entries.toMap - "plan", pinned)
    def json(path: Path): Json =
      get(io.circe.parser.parse(String(Files.readAllBytes(path), "UTF-8")))
    val study =
      Option(getClass.getResource("/eyes4s/study-v1.json")).getOrElse(fail("study-v1.json"))
    assertEquals(json(saved.resolve("fixation").resolve("plan")), json(Paths.get(study.toURI)))
    FreshProcessHarness.studies.foreach { study =>
      assert(get(written.study(study).hcursor.get[Int]("doubles")) > 0, study)
      assert(Files.isRegularFile(saved.resolve(study).resolve("manifest.json")), study)
    }
  }

  test("a fresh JVM resolves, verifies, re-executes and matches every study bit for bit") {
    assertEquals(reader.status, 0, reader.output)
    assert(
      Set(ProcessHandle.current.pid, written.pid, reader.pid).size == 3,
      "writer, reader and test ran in three processes"
    )
    FreshProcessHarness.studies.foreach { study =>
      assertEquals(reader.outcome(study), "reconstructed", s"$study: ${reader.study(study)}")
      // Every double of the re-executed result has the bits the writer computed.
      assertEquals(
        reader.field(study, "fingerprint"),
        written.field(study, "fingerprint"),
        study
      )
      assertEquals(
        get(reader.study(study).hcursor.get[Int]("doubles")),
        get(written.study(study).hcursor.get[Int]("doubles")),
        study
      )
    }
    // The re-executed fixation result encodes to the archived bytes exactly.
    assertEquals(reader.field("fixation", "rerunSha256"), pinned("result"))
    assertEquals(reader.field("fixation", "archiveSha256"), pinned("result"))
    assertEquals(
      get(reader.study("fixation").hcursor.get[Vector[String]]("compared")),
      Vector("rerun", "loaded-rerun", "archive")
    )
    assertEquals(
      get(reader.study("recording").hcursor.get[Vector[String]]("resolved")),
      Vector(
        "recording-input",
        "recording",
        "recording.tMicros",
        "recording.support",
        "recording.lineage",
        "recording.values"
      )
    )
    assertEquals(
      get(reader.study("temporal").hcursor.get[Vector[String]]("resolved")),
      Vector("base", "temporal")
    )
  }

  test("the comparison is exact: one double's last bit differing from the writer's is caught") {
    val copied   = copy("one-bit")
    val expected = copied.resolve("temporal").resolve(FreshProcessHarness.expectedFile)
    val text     = String(Files.readAllBytes(expected), "UTF-8")
    val at       = text.indexOf("\"f64:") + "\"f64:".length + 15
    assert(at > 20, "the temporal fingerprint carries doubles")
    val flipped = text.charAt(at) match
      case '0' => '1'
      case _   => '0'
    Files.write(expected, (text.take(at) + flipped + text.drop(at + 1)).getBytes("UTF-8"))
    val run = launch("read", copied.toString)
    assertEquals(run.status, refusalStatus, run.output)
    assertEquals(run.outcome("temporal"), "failed")
    assert(run.field("temporal", "reason").contains("fingerprints differ"), run.output)
    assertEquals(run.outcome("fixation"), "reconstructed")
    assertEquals(run.outcome("recording"), "reconstructed")
  }

  test("a corrupted artifact is refused by its digest in a fresh JVM, and only its study") {
    val copied = copy("corrupted")
    val result = copied.resolve("fixation").resolve("result")
    val bytes  = Files.readAllBytes(result)
    bytes(bytes.length / 2) = (bytes(bytes.length / 2) ^ 0x01).toByte
    Files.write(result, bytes)
    val run = launch("read", copied.toString)
    assertEquals(run.status, refusalStatus, run.output)
    assertEquals(run.outcome("fixation"), "refused")
    errors(run, "fixation") match
      case Vector(("result", "Digest", None, message)) =>
        assert(message.contains(pinned("result")))
      case other => fail(s"unexpected $other")
    assertEquals(run.outcome("recording"), "reconstructed")
    assertEquals(run.outcome("temporal"), "reconstructed")
  }

  test("a fresh JVM without the result registration refuses the archive by name") {
    val run = launch("read", saved.toString, "--without-result-codec")
    assertEquals(run.status, refusalStatus, run.output)
    errors(run, "fixation") match
      case Vector(("result", "Decode", Some("MissingResultCodec"), message)) =>
        assert(message.contains("eyes4s.cosine@1"), message)
      case other => fail(s"unexpected $other")
    assertEquals(run.outcome("recording"), "reconstructed")
    assertEquals(run.outcome("temporal"), "reconstructed")
  }

  /** Replace entries' bytes and re-declare them, and every document that
    * references them, consistently in a new manifest: the forgery passes
    * every byte digest and must be refused by what the bytes mean.
    */
  private def redeclare(directory: Path, changed: Map[String, Array[Byte]]): Unit =
    val manifest = get(
      ScientificManifest.codec.parse(
        String(Files.readAllBytes(directory.resolve("manifest.json")), "UTF-8")
      )
    )
    val entries = manifest.entries.map { e =>
      changed.get(e.name.value).fold(e) { bytes =>
        Files.write(directory.resolve(e.name.value), bytes)
        get(
          ManifestEntry.of(
            e.name,
            e.role,
            e.schema,
            e.media,
            bytes.length.toLong,
            ByteDigest.sha256(IArray.unsafeFromArray(bytes)),
            e.identity,
            e.layout
          )
        )
      }
    }
    val forged = get(ScientificManifest.of(entries, manifest.relations))
    val bytes  = get(ScientificManifest.bytes(forged))
    Files.write(directory.resolve("manifest.json"), Array.tabulate(bytes.length)(bytes(_)))
    val _ = Files.write(
      directory.resolve("manifest.sha256"),
      ByteDigest.sha256(bytes).hex.getBytes("UTF-8")
    )

  test("one numeric payload changed and re-declared consistently is refused by its identity") {
    val copied    = copy("numeric")
    val directory = copied.resolve("recording")
    // The first x coordinate moves by one unit in the last place.
    val values = Files.readAllBytes(directory.resolve("recording.values"))
    values(0) = (values(0) ^ 0x01).toByte
    val digest   = ByteDigest.sha256(IArray.unsafeFromArray(values)).hex
    val document = String(Files.readAllBytes(directory.resolve("recording")), "UTF-8")
    val json     = get(io.circe.parser.parse(document))
    val cursor   = json.hcursor
      .downField("value")
      .downField("recording")
      .downField("samples")
      .downField("values")
      .downField("sha256")
    val rewritten = get(cursor.withFocus(_ => Json.fromString(digest)).top.toRight("cursor"))
    redeclare(
      directory,
      Map(
        "recording.values" -> values,
        "recording"        -> rewritten.spaces2.getBytes("UTF-8")
      )
    )
    val run = launch("read", copied.toString)
    assertEquals(run.status, refusalStatus, run.output)
    errors(run, "recording") match
      case Vector(("recording", "Decode", _, message)) =>
        assert(message.contains("2c826dc41ae25e67"), message)
      case other => fail(s"unexpected $other")
    assertEquals(run.outcome("fixation"), "reconstructed")
    assertEquals(run.outcome("temporal"), "reconstructed")
  }

  test("a dropped exclusion re-declared consistently is refused by the ledger it belonged to") {
    val copied    = copy("exclusion")
    val directory = copied.resolve("fixation")
    val ledger    = get(
      io.circe.parser.parse(
        String(Files.readAllBytes(directory.resolve("refused-ledger")), "UTF-8")
      )
    )
    val dropped = get(
      ledger.hcursor
        .downField("value")
        .downField("records")
        .withFocus(records => Json.arr(records.asArray.toVector.flatten.drop(1)*))
        .top
        .toRight("cursor")
    )
    redeclare(directory, Map("refused-ledger" -> dropped.spaces2.getBytes("UTF-8")))
    val run = launch("read", copied.toString)
    assertEquals(run.status, refusalStatus, run.output)
    errors(run, "fixation") match
      case Vector(("refused-ledger", "Decode", _, _)) => ()
      case other                                      => fail(s"unexpected $other")
    assertEquals(run.outcome("recording"), "reconstructed")
    assertEquals(run.outcome("temporal"), "reconstructed")
  }
