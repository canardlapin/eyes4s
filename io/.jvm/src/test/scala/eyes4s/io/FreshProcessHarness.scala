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
import cats.syntax.all.*
import eyes4s.codec.*
import eyes4s.compare.Similarity
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*
import io.circe.Json

import java.nio.file.{Files, Path, Paths}

/** The two halves of the fresh-process reconstruction proof (UI-S6), run as
  * separate JVMs by `FreshProcessReconstructionJvmSuite`:
  *
  *   - `write <root>` decodes the pinned v1 fixtures, runs each saved study's
  *     plan, and writes three saved studies under `root` (`fixation`,
  *     `recording`, `temporal`): a manifest, its entries under their manifest
  *     names, the application's pointer to the manifest (`manifest.sha256`)
  *     and the writer's exact fingerprint of the result it computed;
  *   - `read <root>` reads nothing but those files, with registrations made
  *     explicitly on load: it resolves each manifest through the shipped
  *     `ArtifactFiles` directory source, which verifies every length, digest,
  *     schema, semantic identity and relation, re-executes the plan on the
  *     verified input, and compares the result with the archive: the
  *     re-encoded result must have the archived entry's exact SHA-256, and
  *     the fingerprints (every double's raw bits) of the re-executed result,
  *     the result of the plan as registered, the decoded archive and the
  *     writer's in-memory result must agree.
  *
  * Every study is a manifest of its plan, its input and its result archive:
  * the fixation study's `study-result@1`, the recording study's
  * `recording-result@1` and the temporal study's `temporal-result@1`, each
  * related to its plan and input. Both run from the build's class
  * directories: the library, this test code and the pinned fixtures are on
  * their classpath. The reader shares no memory, registry or cache with the
  * writer and reads none of the pinned fixtures, only the saved files;
  * running from published artifacts is the isolated consumer's job (G0/G1).
  *
  * Each run prints one line, `EYES4S_FRESH_PROCESS=<receipt>`, and exits 0
  * only if every study was written, or reconstructed and matched.
  */
object FreshProcessHarness:
  val marker        = "EYES4S_FRESH_PROCESS="
  val manifestFile  = "manifest.json"
  val addressFile   = "manifest.sha256"
  val expectedFile  = "expected-fingerprint.json"
  val studies       = Vector("fixation", "recording", "temporal")
  val refusalStatus = 3

  private def definition(name: String): Either[String, DefinitionId] =
    DefinitionId.of(name, 1).left.map(_.message)

  /** The conventional schemas of the two plan families without a built-in identity. */
  private def recordingCodec: Either[String, RecordingPlanCodec[IdtParameters]] =
    (
      definition("eyes4s.recording-plan"),
      definition("eyes4s.recording.idt"),
      definition("eyes4s.idt-parameters")
    ).mapN(RecordingCodecs.idt)

  private def temporalCodec
      : Either[String, TemporalStudyCodec[StudyKey, Px, Unit, Similarity, SignedDifference]] =
    definition("eyes4s.temporal-study").map(new TemporalStudyCodec(_, StudyCodecs.cosine[Px]))

  private def temporalResults(
      codec: TemporalStudyCodec[StudyKey, Px, Unit, Similarity, SignedDifference]
  ): TemporalResultCodec[StudyKey, Px, Unit, Similarity, SignedDifference] =
    codec.results(StudyResultCodecs.similarity(), StudyResultCodecs.signedDifference())

  private def message[E](e: E): String = e match
    case c: CodecError => c.message
    case p: PlanError  => p.message
    case other         => other.toString

  private def text(name: String): Either[String, String] =
    Option(getClass.getResourceAsStream(s"/eyes4s/$name"))
      .toRight(s"missing pinned fixture $name")
      .map { stream =>
        try String(stream.readAllBytes(), "UTF-8")
        finally stream.close()
      }

  private def process: Json = Json.obj(
    "pid"         -> Json.fromLong(ProcessHandle.current.pid),
    "startMillis" -> Json.fromLong(
      java.lang.management.ManagementFactory.getRuntimeMXBean.getStartTime
    ),
    "javaVersion" -> Json.fromString(System.getProperty("java.version"))
  )

  // ---------------------------------------------------------------------------
  // Writing
  // ---------------------------------------------------------------------------

  private final case class Saved(saved: SavedManifest, expected: ScientificFingerprint.Rendered)

  private def fixation: Either[String, Saved] =
    val studies = StudyCodecs.cosine[Px]
    val inputs  = StudyInputCodecs.study[Px]
    val results = StudyResultCodecs.cosine[Px]
    for
      plan     <- text("study-v1.json").flatMap(studies.codec.parse(_).left.map(message))
      input    <- text("study-input-v1.json").flatMap(inputs.input.parse(_).left.map(message))
      complete <- text("admission-ledger-complete-v1.json").flatMap(
        inputs.ledger.parse(_).left.map(message)
      )
      refused <- text("admission-ledger-v1.json").flatMap(
        inputs.ledger.parse(_).left.map(message)
      )
      result <- plan.run(input).left.map(message)
      saved  <- (for
        p <- StoredArtifact.plan("plan", studies, plan)
        i <- StoredArtifact.input("input", inputs, input)
        l <- StoredArtifact.ledger("ledger", inputs, complete)
        f <- StoredArtifact.ledger("refused-ledger", inputs, refused)
        r <- StoredArtifact.result("result", results, result)
        s <- SavedManifest.of(
          Vector(p, i, l, f, r),
          Vector(
            ManifestRelation.PlanInput(p.name, i.name),
            ManifestRelation.LedgerOf(l.name, i.name),
            ManifestRelation.ResultOf(r.name, p.name, i.name)
          )
        )
      yield s).left.map(message)
    yield Saved(saved, ScientificFingerprint.of(result))

  /** The pinned recording-result-v1 plan: I-DT over one degree square, two
    * milliseconds minimum and a 6 ms interpolation gap, whose declared
    * provenance is exactly the input's evidence.
    */
  private def recordingPlan(
      input: RecordingInput[Px],
      codec: RecordingPlanCodec[IdtParameters]
  ): Either[String, (Recording[Px], RecordingPlan[IdtParameters])] =
    for
      recording <- input.monocular.toRight("the pinned recording input is not monocular")
      sync      <- input.synchronization.toRight("the pinned recording input has no marks")
      area      <- Bounds
        .of[Px](0, 0, 800, 600)
        .left
        .map(_.message)
        .flatMap(b => RecordingArea.of("image", "Image", b).left.map(_.message))
      extent  <- Extent.of[Deg](1, 1).left.map(_.message)
      minimum <- MinimumEventDuration.of(Span.micros(2000)).left.map(_.message)
      gap     <- InterpolationGap.of(Span.micros(6000)).left.map(_.message)
      plan    <- RecordingPlan
        .of(
          ArtifactRef.of[Recording[Px]](recording.contentHash),
          input.source,
          recording.frame,
          recording.clock,
          sync.target,
          FrameId("angular"),
          input.viewing,
          sync.mode,
          sync.marks,
          sync.residualLimit,
          gap,
          Vector(area),
          codec.method,
          IdtParameters(extent, minimum)
        )
        .left
        .map(_.message)
      _ <- Either.cond(
        RecordingInput.disagreements(input, plan).isEmpty,
        (),
        s"the plan disagrees with its input: ${RecordingInput.disagreements(input, plan)}"
      )
    yield (recording, plan)

  private def recording: Either[String, Saved] =
    for
      codec <- recordingCodec
      input <- text("recording-input-v1.json").flatMap(
        RecordingInputCodecs.input[Px].parse(_).left.map(message)
      )
      built    <- recordingPlan(input, codec)
      analysis <- built._2.run(built._1).left.map(_.message)
      saved    <- (for
        i <- StoredArtifact.recordingInput("recording-input", input)
        r <- StoredArtifact.packedRecording("recording", built._1)
        p <- StoredArtifact.recordingPlan("recording-plan", codec, built._2)
        a <- StoredArtifact.recordingResult("recording-result", codec.results, analysis)
        s <- SavedManifest.of(
          Vector(i, r.recording) ++ r.payloads ++ Vector(p, a),
          (ManifestRelation.RecordingOf(i.name, r.recording.name) +: r.relations) ++ Vector(
            ManifestRelation.RecordingPlanInput(p.name, i.name),
            ManifestRelation.RecordingResultOf(a.name, p.name, i.name)
          )
        )
      yield s).left.map(message)
    yield Saved(saved, ScientificFingerprint.of(analysis))

  private def temporal: Either[String, Saved] =
    val inputs = StudyInputCodecs.study[Px]
    for
      codec <- temporalCodec
      input <- text("temporal-study-input-v1.json").flatMap(
        TemporalInputCodecs.study[Px]().input.parse(_).left.map(message)
      )
      plan   <- text("temporal-study-v1.json").flatMap(codec.codec.parse(_).left.map(message))
      result <- plan.run(input).left.map(_.message)
      saved  <- (for
        b <- StoredArtifact.input("base", inputs, input.study)
        t <- StoredArtifact.temporalInput(
          "temporal",
          TemporalInputCodecs.study[Px](StudyEmbedding.ByReference),
          input
        )
        p <- StoredArtifact.temporalPlan("temporal-plan", codec, plan)
        a <- StoredArtifact.temporalResult("temporal-result", temporalResults(codec), result)
        s <- SavedManifest.of(
          Vector(b, t, p, a),
          Vector(
            ManifestRelation.TemporalBase(t.name, b.name),
            ManifestRelation.TemporalPlanInput(p.name, t.name),
            ManifestRelation.TemporalResultOf(a.name, p.name, t.name)
          )
        )
      yield s).left.map(message)
    yield Saved(saved, ScientificFingerprint.of(result))

  private def store(directory: Path, study: Saved): Json =
    Files.createDirectories(directory)
    def put(name: String, bytes: Array[Byte]): Unit =
      val _ = Files.write(directory.resolve(name), bytes)
    put(manifestFile, Array.tabulate(study.saved.bytes.length)(study.saved.bytes(_)))
    put(addressFile, study.saved.address.hex.getBytes("UTF-8"))
    study.saved.artifacts.foreach(a =>
      put(a.name.value, Array.tabulate(a.bytes.length)(a.bytes(_)))
    )
    val expected = Json.obj(
      "doubles"     -> Json.fromInt(study.expected.doubles),
      "fingerprint" -> study.expected.json
    )
    put(expectedFile, expected.noSpaces.getBytes("UTF-8"))
    Json.obj(
      "address" -> Json.fromString(study.saved.address.hex),
      "entries" -> Json.arr(study.saved.manifest.entries.map { e =>
        Json.obj(
          "name"   -> Json.fromString(e.name.value),
          "sha256" -> Json.fromString(e.sha256.hex),
          "length" -> Json.fromLong(e.length)
        )
      }*),
      "doubles"     -> Json.fromInt(study.expected.doubles),
      "fingerprint" -> Json.fromString(
        ByteDigest.sha256(IArray.unsafeFromArray(study.expected.bytes)).hex
      )
    )

  def write(root: Path): Either[String, Json] =
    for
      f <- fixation
      r <- recording
      t <- temporal
    yield Json.obj(
      "role"    -> Json.fromString("writer"),
      "process" -> process,
      "studies" -> Json.obj(
        "fixation"  -> store(root.resolve("fixation"), f),
        "recording" -> store(root.resolve("recording"), r),
        "temporal"  -> store(root.resolve("temporal"), t)
      )
    )

  // ---------------------------------------------------------------------------
  // Reading in a fresh process
  // ---------------------------------------------------------------------------

  /** Registrations made explicitly on load: the cosine study, the I-DT
    * recording and the cosine temporal plan families, with their result
    * codecs unless `withoutResultCodec`, as a reader that forgot them would.
    * Recording plans register through the pixel unit witness.
    */
  private def decoders(
      withoutResultCodec: Boolean
  ): Either[String, ArtifactDecoders[StudyKey, Px]] =
    def registered[R](empty: R)(register: => Either[CodecError, R]): Either[CodecError, R] =
      if withoutResultCodec then Right(empty) else register
    (for
      recordings <- recordingCodec.left.map(reason =>
        CodecError.Unsupported("recording", reason)
      )
      temporals <- temporalCodec.left.map(reason => CodecError.Unsupported("temporal", reason))
      plans   <- StudyRegistry.empty[StudyKey, Px].register(StudyCodecs.cosine[Px].registration)
      inputs  <- StudyInputRegistry.empty[StudyKey, Px].register(StudyInputCodecs.study[Px])
      results <- registered(StudyResultRegistry.empty[StudyKey, Px])(
        StudyResultRegistry
          .empty[StudyKey, Px]
          .register(StudyResultCodecs.cosine[Px].registration)
      )
      recordingPlans   <- RecordingRegistry.empty.register(recordings.registration)
      recordingResults <- registered(RecordingResultRegistry.empty)(
        RecordingResultRegistry.empty.register(recordings.results.registration)
      )
      temporalPlans   <- TemporalRegistry.empty[StudyKey, Px].register(temporals.registration)
      temporalArchive <- registered(TemporalResultRegistry.empty[StudyKey, Px])(
        TemporalResultRegistry
          .empty[StudyKey, Px]
          .register(temporalResults(temporals).registration)
      )
    yield ArtifactDecoders
      .of(plans, inputs, results)
      .withRecordings(recordingPlans, recordingResults)
      .withTemporal(temporalPlans, temporalArchive)).left.map(message)

  /** A codec error's innermost cause and the entry path that locates it. */
  private def leaf(
      error: CodecError,
      path: Vector[String] = Vector.empty
  ): (String, CodecError) =
    error match
      case CodecError.Entry(at, inner) => leaf(inner, path :+ at)
      case other                       => (path.mkString("."), other)

  /** A refusal as data: every resolve error with its entry, its case, the
    * located leaf cause as its typed value, and its message.
    */
  private def refused(errors: Vector[ResolveError]): Json = Json.obj(
    "outcome" -> Json.fromString("refused"),
    "errors"  -> Json.arr(errors.map { e =>
      val (path, cause) = e match
        case ResolveError.Decode(_, codec) =>
          val (at, inner) = leaf(codec)
          (at, inner.toString)
        case ResolveError.Relation(relation, mismatch) => (relation.render, mismatch.toString)
        case other                                     => ("", other.toString)
      Json.obj(
        "entry"   -> e.entryName.fold(Json.Null)(n => Json.fromString(n.value)),
        "error"   -> Json.fromString(e.productPrefix),
        "path"    -> Json.fromString(path),
        "cause"   -> Json.fromString(cause),
        "message" -> Json.fromString(e.message)
      )
    }*)
  )

  /** Why a verified study could not be rerun or matched; `cause` is the typed
    * value the check refused with, when there is one.
    */
  private final case class Failure(reason: String, cause: Option[String] = None)

  private def failed(failure: Failure): Json = Json.obj(
    "outcome" -> Json.fromString("failed"),
    "reason"  -> Json.fromString(failure.reason),
    "cause"   -> failure.cause.fold(Json.Null)(Json.fromString)
  )

  private def check[E](refusals: Vector[E], reason: String): Either[Failure, Unit] =
    Either.cond(refusals.isEmpty, (), Failure(reason, Some(refusals.toString)))

  extension [A](value: Either[String, A])
    private def failing: Either[Failure, A] =
      value.left.map(Failure(_))

  private def readText(path: Path): Either[String, String] =
    Either
      .catchNonFatal(String(Files.readAllBytes(path), "UTF-8"))
      .left
      .map(e => s"$path: ${e.getMessage}")

  private def resolve(
      directory: Path,
      decoders: ArtifactDecoders[StudyKey, Px]
  ): Either[Json, ResolvedManifest[StudyKey, Px]] =
    (for
      hex     <- readText(directory.resolve(addressFile)).map(_.trim)
      address <- ByteDigest.parse(hex).left.map(_.message)
    yield address) match
      case Left(reason)   => Left(failed(Failure(reason)))
      case Right(address) =>
        ArtifactFiles
          .resolve[IO, StudyKey, Px](address, directory, manifestFile, decoders)
          .unsafeRunSync()
          .left
          .map(errors => refused(errors.toVector))

  private def expected(directory: Path): Either[String, (Json, Int)] =
    for
      raw     <- readText(directory.resolve(expectedFile))
      json    <- io.circe.parser.parse(raw).left.map(_.message)
      print   <- json.hcursor.get[Json]("fingerprint").left.map(_.message)
      doubles <- json.hcursor.get[Int]("doubles").left.map(_.message)
    yield (print, doubles)

  /** The rerun's fingerprints against the writer's. The receipt reports the
    * digest and double count of the reader's own first rendering, so a match
    * is two independently computed values agreeing.
    */
  private def compared(
      directory: Path,
      named: Vector[(String, ScientificFingerprint.Rendered)]
  ): Either[Failure, Json] =
    expected(directory).failing.flatMap { (print, doubles) =>
      val mismatched = named.collect {
        case (name, r) if r.json != print || r.doubles != doubles => name
      }
      val own = named.head._2
      Either.cond(
        mismatched.isEmpty,
        Json.obj(
          "outcome"     -> Json.fromString("reconstructed"),
          "doubles"     -> Json.fromInt(own.doubles),
          "compared"    -> Json.arr(named.map((n, _) => Json.fromString(n))*),
          "fingerprint" -> Json.fromString(
            ByteDigest.sha256(IArray.unsafeFromArray(own.bytes)).hex
          )
        ),
        Failure(s"fingerprints differ from the writer's for ${mismatched.mkString(", ")}")
      )
    }

  private def name(value: String): Either[String, ArtifactName] =
    ArtifactName.of(value).left.map(_.message)

  private def rerunFixation(
      directory: Path,
      resolved: ResolvedManifest[StudyKey, Px]
  ): Either[Failure, Json] =
    val studies = StudyCodecs.cosine[Px]
    val results = StudyResultCodecs.cosine[Px]
    for
      planName   <- name("plan").failing
      inputName  <- name("input").failing
      resultName <- name("result").failing
      loaded     <- resolved.plan(planName).toRight(Failure("no plan"))
      input      <- resolved.input(inputName).toRight(Failure("no input"))
      archive    <- resolved.result(resultName).toRight(Failure("no result"))
      entry      <- resolved.manifest.entry(resultName).toRight(Failure("no result entry"))
      // The typed plan the application's registration stands for: the verified
      // plan re-encoded and read through the typed codec, with the same description.
      json  <- loaded.encode.left.map(message).failing
      typed <- studies.codec.decode(json).left.map(message).failing
      _     <- check(
        PlanChange.between(loaded.description, typed.description),
        "typed plan differs"
      )
      _         <- check(typed.prerequisites(Some(input)), "prerequisites refused")
      rerun     <- typed.run(input).left.map(message).failing
      loadedRun <- loaded.run(input).left.map(message).failing
      encoded   <- results.codec.encode(rerun).left.map(message).failing
      bytes  = encoded.spaces2.getBytes("UTF-8")
      digest = ByteDigest.sha256(IArray.unsafeFromArray(bytes))
      _ <- Either.cond(
        digest == entry.sha256 && bytes.length.toLong == entry.length,
        (),
        Failure(
          s"the re-executed result encodes to ${digest.hex} (${bytes.length} bytes), " +
            s"not the archived ${entry.sha256.hex} (${entry.length} bytes)"
        )
      )
      receipt <- compared(
        directory,
        Vector(
          "rerun"        -> ScientificFingerprint.of(rerun),
          "loaded-rerun" -> ScientificFingerprint.of(loadedRun),
          "archive"      -> ScientificFingerprint.of(archive.result)
        )
      )
    yield receipt.deepMerge(
      Json.obj(
        "archiveSha256" -> Json.fromString(entry.sha256.hex),
        "rerunSha256"   -> Json.fromString(digest.hex)
      )
    )

  /** The re-executed result against the archived entry: its canonical
    * encoding (the UTF-8 of the pretty-printed document) must have the
    * archived entry's exact length and SHA-256.
    */
  private def matchesArchive(
      entry: ManifestEntry,
      encoded: Either[CodecError, Json]
  ): Either[Failure, ByteDigest] =
    encoded.left.map(message).failing.flatMap { json =>
      val bytes  = json.spaces2.getBytes("UTF-8")
      val digest = ByteDigest.sha256(IArray.unsafeFromArray(bytes))
      Either.cond(
        digest == entry.sha256 && bytes.length.toLong == entry.length,
        digest,
        Failure(
          s"the re-executed result encodes to ${digest.hex} (${bytes.length} bytes), " +
            s"not the archived ${entry.sha256.hex} (${entry.length} bytes)"
        )
      )
    }

  private def archived(entry: ManifestEntry, digest: ByteDigest): Json = Json.obj(
    "archiveSha256" -> Json.fromString(entry.sha256.hex),
    "rerunSha256"   -> Json.fromString(digest.hex)
  )

  private def rerunRecording(
      directory: Path,
      resolved: ResolvedManifest[StudyKey, Px]
  ): Either[Failure, Json] =
    for
      codec      <- recordingCodec.failing
      inputName  <- name("recording-input").failing
      planName   <- name("recording-plan").failing
      resultName <- name("recording-result").failing
      input      <- resolved.recordingInput(inputName).toRight(Failure("no recording input"))
      loaded     <- resolved.recordingPlan(planName).toRight(Failure("no recording plan"))
      archive    <- resolved.recordingResult(resultName).toRight(Failure("no recording result"))
      entry      <- resolved.manifest.entry(resultName).toRight(Failure("no result entry"))
      recording  <- input.monocular.toRight(Failure("the recording input is binocular"))
      // The typed plan the application's registration stands for: the verified
      // plan re-encoded and read through the typed codec, with the same description.
      json  <- loaded.encode.left.map(message).failing
      typed <- codec.codec.decode(json).left.map(message).failing
      _     <- check(
        PlanChange.between(loaded.description, typed.description),
        "typed plan differs"
      )
      _ <- check(
        RecordingInput.disagreements(input, typed),
        "the plan disagrees with its input"
      )
      rerun     <- typed.run(recording).left.map(_.message).failing
      loadedRun <- loaded.run(input).left.map(_.message).failing
      digest    <- matchesArchive(entry, codec.results.codec.encode(rerun))
      receipt   <- compared(
        directory,
        Vector(
          "rerun"        -> ScientificFingerprint.of(rerun),
          "loaded-rerun" -> ScientificFingerprint.of(loadedRun),
          "archive"      -> ScientificFingerprint.of(archive.analysis)
        )
      )
    yield receipt.deepMerge(archived(entry, digest))

  private def rerunTemporal(
      directory: Path,
      resolved: ResolvedManifest[StudyKey, Px]
  ): Either[Failure, Json] =
    for
      codec      <- temporalCodec.failing
      inputName  <- name("temporal").failing
      planName   <- name("temporal-plan").failing
      resultName <- name("temporal-result").failing
      input      <- resolved.temporalInput(inputName).toRight(Failure("no temporal input"))
      loaded     <- resolved.temporalPlan(planName).toRight(Failure("no temporal plan"))
      archive    <- resolved.temporalResult(resultName).toRight(Failure("no temporal result"))
      entry      <- resolved.manifest.entry(resultName).toRight(Failure("no result entry"))
      json       <- loaded.encode.left.map(message).failing
      typed      <- codec.codec.decode(json).left.map(message).failing
      _          <- check(
        PlanChange.between(loaded.description, typed.description),
        "typed plan differs"
      )
      _         <- check(typed.prerequisites(Some(input)), "prerequisites refused")
      rerun     <- typed.run(input).left.map(_.message).failing
      loadedRun <- loaded.run(input).left.map(_.message).failing
      digest    <- matchesArchive(entry, temporalResults(codec).codec.encode(rerun))
      receipt   <- compared(
        directory,
        Vector(
          "rerun"        -> ScientificFingerprint.of(rerun),
          "loaded-rerun" -> ScientificFingerprint.of(loadedRun),
          "archive"      -> ScientificFingerprint.of(archive.result)
        )
      )
    yield receipt.deepMerge(archived(entry, digest))

  def read(root: Path, withoutResultCodec: Boolean): Either[String, Json] =
    decoders(withoutResultCodec).map { registered =>
      def study(
          label: String,
          rerun: (Path, ResolvedManifest[StudyKey, Px]) => Either[Failure, Json]
      ): Json =
        val directory = root.resolve(label)
        val started   = System.nanoTime
        resolve(directory, registered) match
          case Left(outcome)   => outcome
          case Right(resolved) =>
            val entries = resolved.manifest.entries.map(e => Json.fromString(e.name.value))
            rerun(directory, resolved)
              .fold(failed, identity)
              .deepMerge(
                Json.obj(
                  "resolved" -> Json.arr(entries*),
                  "millis"   -> Json.fromLong((System.nanoTime - started) / 1000000L)
                )
              )
      Json.obj(
        "role"    -> Json.fromString("reader"),
        "process" -> process,
        "studies" -> Json.obj(
          "fixation"  -> study("fixation", rerunFixation),
          "recording" -> study("recording", rerunRecording),
          "temporal"  -> study("temporal", rerunTemporal)
        )
      )
    }

  def main(arguments: Array[String]): Unit =
    val outcome = arguments.toVector match
      case Vector("write", root)                          => write(Paths.get(root))
      case Vector("read", root)                           => read(Paths.get(root), false)
      case Vector("read", root, "--without-result-codec") => read(Paths.get(root), true)
      case other                                          =>
        Left(s"usage: write <root> | read <root> [--without-result-codec]; got $other")
    outcome match
      case Left(reason) =>
        println(marker + Json.obj("error" -> Json.fromString(reason)).noSpaces)
        System.exit(2)
      case Right(receipt) =>
        println(marker + receipt.noSpaces)
        val complete = receipt.hcursor
          .downField("studies")
          .focus
          .flatMap(_.asObject)
          .forall(_.values.forall { s =>
            s.hcursor.get[String]("outcome").toOption.forall(_ == "reconstructed")
          })
        System.exit(if complete then 0 else refusalStatus)
