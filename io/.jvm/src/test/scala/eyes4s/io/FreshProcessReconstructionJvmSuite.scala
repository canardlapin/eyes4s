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
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Fresh-process reconstruction (UI-S6): a saved study is written by one
  * JVM and resolved, verified, re-executed and compared bit for bit by
  * another, freshly started JVM. Both are launched from this test's
  * classpath (the build's class directories: the library, the test code and
  * the pinned fixtures); neither is this JVM, and the reader shares no
  * memory, registry or cache with the writer and reads only the saved files.
  *
  * The writer must reproduce the pinned v1 archives byte for byte, and the
  * reader must find, for the fixation study, a re-executed result whose
  * canonical encoding has the archived entry's exact SHA-256 and, for every
  * study, a result whose every double has the bits the writer computed.
  *
  * Perturbations of private copies pin what verification establishes. Byte
  * corruption and a missing registration are refused by name. A forger who
  * re-declares digests consistently is refused only where the forged content
  * contradicts something re-derived: a semantic identity, a relation, a
  * ledger invariant or a plan's recorded input. Two ledger forgeries and a
  * forgery that also rewrites the plan pass every check; they are pinned as
  * such, because only a re-import of the source (for a ledger) or an
  * external record of the original result can reveal them.
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

  /** The doubles each result's fingerprint carries, as SAVED_STUDIES.md states. */
  private val doubles = Map("fixation" -> 308, "recording" -> 116, "temporal" -> 6178)

  /** The pinned monocular recording's identity: recording-input-v1's channels. */
  private val recordingHash = "2c826dc41ae25e67"

  private lazy val classpath: String =
    val stream = Option(getClass.getResourceAsStream("/eyes4s/fresh-process/classpath.txt"))
      .getOrElse(fail("the build did not generate the fresh-process classpath resource"))
    try String(stream.readAllBytes(), "UTF-8")
    finally stream.close()

  private def resourceBytes(name: String): Array[Byte] =
    val stream = Option(getClass.getResourceAsStream(s"/eyes4s/$name"))
      .getOrElse(fail(s"missing pinned fixture $name"))
    try stream.readAllBytes()
    finally stream.close()

  private val java = Paths.get(System.getProperty("java.home"), "bin", "java").toString

  private final case class Run(status: Int, receipt: Json, output: String):
    def pid: Long                 = get(receipt.hcursor.downField("process").get[Long]("pid"))
    def study(name: String): Json = get(receipt.hcursor.downField("studies").get[Json](name))
    def outcome(name: String): String = get(study(name).hcursor.get[String]("outcome"))
    def field(name: String, key: String): String = get(study(name).hcursor.get[String](key))
    def count(name: String): Int                 = get(study(name).hcursor.get[Int]("doubles"))

  private val root: Path  = Files.createTempDirectory("eyes4s-fresh-process-")
  private val saved: Path = root.resolve("saved")

  /** Run the harness in a new JVM over this test's classpath. Its output goes
    * to a file, so a child that hangs is destroyed at the deadline instead of
    * blocking a read.
    */
  private def launch(arguments: String*): Run =
    val log     = Files.createTempFile(root, "harness-", ".log")
    val command =
      Vector(java, "-Xmx1g", "-cp", classpath, "eyes4s.io.FreshProcessHarness") ++ arguments
    val process = new ProcessBuilder(command*)
      .redirectErrorStream(true)
      .redirectOutput(log.toFile)
      .start()
    if !process.waitFor(10, TimeUnit.MINUTES) then
      process.destroyForcibly()
      val _ = process.waitFor(30, TimeUnit.SECONDS)
      fail(s"the harness did not finish within 10 minutes:\n${Files.readString(log)}")
    val output = Files.readString(log)
    val line   = output.linesIterator
      .find(_.startsWith(marker))
      .getOrElse(fail(s"the harness printed no receipt:\n$output"))
    Run(process.exitValue, get(io.circe.parser.parse(line.drop(marker.length))), output)

  override def afterAll(): Unit =
    // Only this suite's own temporary directory is removed.
    Using.resource(Files.walk(root))(_.iterator.asScala.toVector).reverse.foreach(Files.delete)

  private lazy val written: Run = launch("write", saved.toString)
  private lazy val reader: Run  =
    val _ = written
    launch("read", saved.toString)

  /** A private copy of the saved studies for one perturbation. */
  private def copy(label: String): Path =
    val _      = written
    val target = root.resolve(label)
    Using.resource(Files.walk(saved))(_.iterator.asScala.toVector).foreach { source =>
      val destination = target.resolve(saved.relativize(source).toString)
      if Files.isDirectory(source) then Files.createDirectories(destination)
      else Files.copy(source, destination, StandardCopyOption.REPLACE_EXISTING)
    }
    target

  /** One reported resolve error: its entry, case, located path and typed leaf cause. */
  private final case class Refusal(entry: String, error: String, path: String, cause: String)

  private def refusals(run: Run, study: String): Vector[Refusal] =
    get(run.study(study).hcursor.get[Vector[Json]]("errors")).map { e =>
      Refusal(
        get(e.hcursor.get[String]("entry")),
        get(e.hcursor.get[String]("error")),
        get(e.hcursor.get[String]("path")),
        get(e.hcursor.get[String]("cause"))
      )
    }

  private def json(path: Path): Json =
    get(io.circe.parser.parse(Files.readString(path)))

  private def write(path: Path, bytes: Array[Byte]): Unit =
    val _ = Files.write(path, bytes)

  /** Replace entries' bytes, and optionally their declared semantic
    * identities, and re-declare them consistently in a new manifest under a
    * new address: the forgery passes every byte digest.
    */
  private def redeclare(
      directory: Path,
      changed: Map[String, Array[Byte]],
      identities: Map[String, ContentHash] = Map.empty
  ): Unit =
    val manifest = get(
      ScientificManifest.codec.parse(Files.readString(directory.resolve("manifest.json")))
    )
    val entries = manifest.entries.map { e =>
      changed.get(e.name.value).fold(e) { bytes =>
        write(directory.resolve(e.name.value), bytes)
        get(
          ManifestEntry.of(
            e.name,
            e.role,
            e.schema,
            e.media,
            bytes.length.toLong,
            ByteDigest.sha256(IArray.unsafeFromArray(bytes)),
            identities.get(e.name.value).orElse(e.identity),
            e.layout
          )
        )
      }
    }
    val forged = get(ScientificManifest.of(entries, manifest.relations))
    val bytes  = get(ScientificManifest.bytes(forged))
    write(directory.resolve("manifest.json"), Array.tabulate(bytes.length)(bytes(_)))
    write(directory.resolve("manifest.sha256"), ByteDigest.sha256(bytes).hex.getBytes("UTF-8"))

  // ---------------------------------------------------------------------------
  // Reconstruction
  // ---------------------------------------------------------------------------

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
    val study =
      Option(getClass.getResource("/eyes4s/study-v1.json")).getOrElse(fail("study-v1"))
    assertEquals(json(saved.resolve("fixation").resolve("plan")), json(Paths.get(study.toURI)))
    // The temporal plan the writer saved is the pinned temporal-study-v1 fixture, byte for byte.
    assertEquals(
      Files
        .readAllBytes(saved.resolve("temporal").resolve(FreshProcessHarness.planFile))
        .toVector,
      resourceBytes("temporal-study-v1.json").toVector
    )
    FreshProcessHarness.studies.foreach { study =>
      assertEquals(written.count(study), doubles(study), study)
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
      // The reader's own fingerprint digest and double count equal the writer's:
      // every double of the re-executed result has the bits the writer computed.
      assertEquals(
        reader.field(study, "fingerprint"),
        written.field(study, "fingerprint"),
        study
      )
      assertEquals(reader.count(study), doubles(study), study)
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

  test("the comparison is exact: one bit of one double differing from the writer's is caught") {
    val copied   = copy("one-bit")
    val expected = copied.resolve("temporal").resolve(FreshProcessHarness.expectedFile)
    val text     = Files.readString(expected)
    val at       = text.indexOf("\"f64:") + "\"f64:".length + 15
    assert(at > 20, "the temporal fingerprint carries doubles")
    // The last hexadecimal digit of the first double, with its lowest bit flipped.
    val digit   = Character.digit(text.charAt(at), 16)
    val flipped = Character.forDigit(digit ^ 1, 16)
    write(expected, (text.take(at) + flipped + text.drop(at + 1)).getBytes("UTF-8"))
    val run = launch("read", copied.toString)
    assertEquals(run.status, refusalStatus, run.output)
    assertEquals(run.outcome("temporal"), "failed")
    assertEquals(
      run.field("temporal", "reason"),
      "fingerprints differ from the writer's for rerun"
    )
    assertEquals(run.outcome("fixation"), "reconstructed")
    assertEquals(run.outcome("recording"), "reconstructed")
  }

  // ---------------------------------------------------------------------------
  // Corruption and missing registrations
  // ---------------------------------------------------------------------------

  test("a corrupted artifact is refused by its digest in a fresh JVM, and only its study") {
    val copied = copy("corrupted")
    val result = copied.resolve("fixation").resolve("result")
    val bytes  = Files.readAllBytes(result)
    bytes(bytes.length / 2) = (bytes(bytes.length / 2) ^ 0x01).toByte
    write(result, bytes)
    val actual = ByteDigest.sha256(IArray.unsafeFromArray(bytes)).hex
    val run    = launch("read", copied.toString)
    assertEquals(run.status, refusalStatus, run.output)
    assertEquals(run.outcome("fixation"), "refused")
    assertEquals(
      refusals(run, "fixation"),
      Vector(Refusal("result", "Digest", "", s"Digest(result,${pinned("result")},$actual)"))
    )
    assertEquals(run.outcome("recording"), "reconstructed")
    assertEquals(run.outcome("temporal"), "reconstructed")
  }

  test("a fresh JVM without the result registration refuses the archive by name") {
    val run = launch("read", saved.toString, "--without-result-codec")
    assertEquals(run.status, refusalStatus, run.output)
    assertEquals(
      refusals(run, "fixation"),
      Vector(
        Refusal("result", "Decode", "", "MissingResultCodec(DefinitionId(eyes4s.cosine,1))")
      )
    )
    assertEquals(run.outcome("recording"), "reconstructed")
    assertEquals(run.outcome("temporal"), "reconstructed")
  }

  // ---------------------------------------------------------------------------
  // Consistent forgeries of a numeric payload
  // ---------------------------------------------------------------------------

  /** The pinned recording with its first x coordinate one unit in the last
    * place higher, the forged recording input over it, and their identities.
    */
  private lazy val forged: (Recording[Px], RecordingInput[Px]) =
    val input = get(
      RecordingInputCodecs
        .input[Px]
        .parse(Files.readString(saved.resolve("recording").resolve("recording-input")))
    )
    val recording = get(input.monocular.toRight("monocular"))
    val first     = recording.samples(0)
    val moved     = first.gaze match
      case Gaze.Tracked(p, pupil) => Gaze.Tracked(Pt[Px](Math.nextUp(p.x), p.y), pupil)
      case other                  => fail(s"unexpected first sample $other")
    val changed = get(
      Recording.of(
        recording.frame,
        recording.clock,
        recording.rate,
        recording.eye,
        recording.pupilUnit,
        recording.samples.updated(0, first.copy(gaze = moved)),
        recording.samplingTolerance
      )
    )
    val changedInput = get(
      RecordingInput.of(
        input.source,
        RecordingChannels.Monocular(changed),
        input.viewing,
        input.synchronization
      )
    )
    (changed, changedInput)

  private def packedFiles(recording: Recording[Px]): Map[String, Array[Byte]] =
    val packed = get(StoredArtifact.packedRecording("recording", recording))
    (packed.recording +: packed.payloads)
      .map(a => a.name.value -> Array.tabulate(a.bytes.length)(a.bytes(_)))
      .toMap

  test("one packed double changed, with its payload digest re-declared, fails its identity") {
    val copied    = copy("numeric")
    val directory = copied.resolve("recording")
    // The lowest bit of the first x coordinate: exactly Math.nextUp(500.0).
    val values = Files.readAllBytes(directory.resolve("recording.values"))
    values(0) = (values(0) ^ 0x01).toByte
    assertEquals(values.toVector, packedFiles(forged._1)("recording.values").toVector)
    val document = json(directory.resolve("recording"))
    val digest   = ByteDigest.sha256(IArray.unsafeFromArray(values)).hex
    val sha      = document.hcursor
      .downField("value")
      .downField("recording")
      .downField("samples")
      .downField("values")
      .downField("sha256")
    val rewritten = get(sha.withFocus(_ => Json.fromString(digest)).top.toRight("cursor"))
    redeclare(
      directory,
      Map("recording.values" -> values, "recording" -> rewritten.spaces2.getBytes("UTF-8"))
    )
    val run = launch("read", copied.toString)
    assertEquals(run.status, refusalStatus, run.output)
    assertEquals(
      refusals(run, "recording"),
      Vector(
        Refusal(
          "recording",
          "Decode",
          "recording",
          s"InputIdentity($recordingHash,${forged._1.contentHash.render})"
        )
      )
    )
    assertEquals(run.outcome("fixation"), "reconstructed")
    assertEquals(run.outcome("temporal"), "reconstructed")
  }

  test("the recording re-packed and re-declared under its new identity fails its relation") {
    val copied    = copy("repacked")
    val directory = copied.resolve("recording")
    redeclare(directory, packedFiles(forged._1), Map("recording" -> forged._1.contentHash))
    val run = launch("read", copied.toString)
    assertEquals(run.status, refusalStatus, run.output)
    assertEquals(
      refusals(run, "recording"),
      Vector(
        Refusal(
          "recording-input",
          "Relation",
          "recording-of(input=recording-input, recording=recording)",
          s"RecordingIdentity(${forged._1.contentHash.render},$recordingHash)"
        )
      )
    )
  }

  test(
    "the recording input forged as well resolves, and the plan's recorded input refuses it"
  ) {
    val copied    = copy("forged-input")
    val directory = copied.resolve("recording")
    val input     = get(StoredArtifact.recordingInput("recording-input", forged._2))
    redeclare(
      directory,
      packedFiles(forged._1) +
        ("recording-input" -> Array.tabulate(input.bytes.length)(input.bytes(_))),
      Map("recording" -> forged._1.contentHash, "recording-input" -> forged._2.hash)
    )
    val run = launch("read", copied.toString)
    assertEquals(run.status, refusalStatus, run.output)
    // Every artifact verified; plan.json still records the original recording.
    assertEquals(run.outcome("recording"), "failed")
    assertEquals(run.field("recording", "reason"), "the plan disagrees with its input")
    assertEquals(
      run.field("recording", "cause"),
      s"Vector(Plan(Input(ArtifactMismatch($recordingHash,${forged._1.contentHash.render}))))"
    )
    assertEquals(run.outcome("fixation"), "reconstructed")
    assertEquals(run.outcome("temporal"), "reconstructed")
  }

  test(
    "pinned: a forgery that also rewrites the plan passes every check but the writer's record"
  ) {
    val copied    = copy("forged-plan")
    val directory = copied.resolve("recording")
    val input     = get(StoredArtifact.recordingInput("recording-input", forged._2))
    redeclare(
      directory,
      packedFiles(forged._1) +
        ("recording-input" -> Array.tabulate(input.bytes.length)(input.bytes(_))),
      Map("recording" -> forged._1.contentHash, "recording-input" -> forged._2.hash)
    )
    val plan = json(directory.resolve(FreshProcessHarness.planFile)).hcursor
      .downField("value")
      .downField("input")
      .withFocus(_ => Json.fromString(forged._1.contentHash.render))
      .top
      .get
    write(directory.resolve(FreshProcessHarness.planFile), plan.spaces2.getBytes("UTF-8"))
    val run = launch("read", copied.toString)
    assertEquals(run.status, refusalStatus, run.output)
    // A self-consistent different study: resolution, relations and the plan's
    // checks all pass, and it re-executes. Only the writer's fingerprint of the
    // original result, held outside the saved files, differs.
    assertEquals(run.outcome("recording"), "failed")
    assertEquals(
      run.field("recording", "reason"),
      "fingerprints differ from the writer's for rerun"
    )
  }

  // ---------------------------------------------------------------------------
  // Consistent forgeries of an exclusion
  // ---------------------------------------------------------------------------

  private def ledgerRecords(
      directory: Path
  )(change: Vector[Json] => Vector[Json]): Array[Byte] =
    json(directory.resolve("refused-ledger")).hcursor
      .downField("value")
      .downField("records")
      .withFocus(records => Json.arr(change(records.asArray.toVector.flatten)*))
      .top
      .get
      .spaces2
      .getBytes("UTF-8")

  private def record(entry: Json): Int = get(entry.hcursor.get[Int]("record"))

  test("record 2 dropped from the refused ledger breaks its quarantine scope") {
    val copied    = copy("exclusion")
    val directory = copied.resolve("fixation")
    redeclare(
      directory,
      Map("refused-ledger" -> ledgerRecords(directory)(_.filterNot(record(_) == 2)))
    )
    val run = launch("read", copied.toString)
    assertEquals(run.status, refusalStatus, run.output)
    // Records 3-5 are quarantined with record 2, which no longer exists.
    assertEquals(
      refusals(run, "fixation"),
      Vector(
        Refusal(
          "refused-ledger",
          "Decode",
          "",
          "Admission(QuarantineScope(3,Vector(2, 3, 4, 5)))"
        )
      )
    )
    assertEquals(run.outcome("recording"), "reconstructed")
    assertEquals(run.outcome("temporal"), "reconstructed")
  }

  test("pinned: record 2 dropped with the quarantine scope rewritten resolves cleanly") {
    val copied    = copy("rescoped")
    val directory = copied.resolve("fixation")
    val rescoped  = ledgerRecords(directory)(
      _.filterNot(record(_) == 2).map(entry =>
        entry.hcursor
          .downField("reason")
          .withFocus(reason =>
            if reason.hcursor.get[String]("kind").contains("quarantined") then
              reason.mapObject(_.add("records", Json.arr(Vector(3, 4, 5).map(Json.fromInt)*)))
            else reason
          )
          .top
          .getOrElse(entry) // an admitted record has no reason
      )
    )
    redeclare(directory, Map("refused-ledger" -> rescoped))
    val forgedLedger = get(
      StudyInputCodecs.study[Px].ledger.parse(String(rescoped, "UTF-8"))
    )
    assertEquals(forgedLedger.records.filterNot(_.isAdmitted).map(_.record), Vector(3, 4, 5))
    val run = launch("read", copied.toString)
    // Nothing binds a ledger's rejected rows: only a re-import of the source
    // file could show that record 2's exclusion is gone.
    assertEquals(run.status, 0, run.output)
    FreshProcessHarness.studies.foreach(study =>
      assertEquals(run.outcome(study), "reconstructed", study)
    )
  }
