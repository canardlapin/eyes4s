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
import eyes4s.surface.EdgePolicy
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
  *   - `read <root>` starts from nothing but those files and explicit
  *     registrations: it resolves each manifest through the shipped
  *     `ArtifactFiles` directory source, which verifies every length, digest,
  *     schema, semantic identity and relation, re-executes the plan on the
  *     verified input, and compares the result with the archive: the
  *     re-encoded result must have the archived entry's exact SHA-256, and
  *     the fingerprints (every double's raw bits) of the re-executed result,
  *     the decoded archive and the writer's in-memory result must agree.
  *
  * Recording and temporal plans have no manifest role yet, so their plans
  * travel beside the manifest in `plan.json` and are checked against the
  * verified inputs by the plans' own prerequisites; their results have no
  * archive codec, so the writer's fingerprint is the archive they are
  * compared with.
  *
  * Each run prints one line, `EYES4S_FRESH_PROCESS=<receipt>`, and exits 0
  * only if every study was written, or reconstructed and matched.
  */
object FreshProcessHarness:
  val marker        = "EYES4S_FRESH_PROCESS="
  val manifestFile  = "manifest.json"
  val addressFile   = "manifest.sha256"
  val planFile      = "plan.json"
  val expectedFile  = "expected-fingerprint.json"
  val studies       = Vector("fixation", "recording", "temporal")
  val refusalStatus = 3

  private def definition(name: String): Either[String, DefinitionId] =
    DefinitionId.of(name, 1).left.map(_.message)

  /** The conventional schemas of the two plan families without a built-in identity. */
  private def recordingCodec: Either[String, RecordingPlanCodec[IvtParameters]] =
    (
      definition("eyes4s.recording-plan"),
      definition("eyes4s.recording.ivt"),
      definition("eyes4s.ivt-parameters")
    ).mapN(RecordingCodecs.ivt)

  private def temporalCodec
      : Either[String, TemporalStudyCodec[StudyKey, Px, Unit, Similarity, SignedDifference]] =
    definition("eyes4s.temporal-study").map(new TemporalStudyCodec(_, StudyCodecs.cosine[Px]))

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

  private final case class Saved(
      saved: SavedManifest,
      expected: ScientificFingerprint.Rendered,
      plan: Option[Json]
  )

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
    yield Saved(saved, ScientificFingerprint.of(result), None)

  /** An I-VT plan whose declared provenance is exactly the input's evidence. */
  private def recordingPlan(
      input: RecordingInput[Px],
      codec: RecordingPlanCodec[IvtParameters]
  ): Either[String, (Recording[Px], RecordingPlan[IvtParameters])] =
    for
      recording <- input.monocular.toRight("the pinned recording input is not monocular")
      sync      <- input.synchronization.toRight("the pinned recording input has no marks")
      area      <- Bounds
        .of[Px](0, 0, 800, 600)
        .left
        .map(_.message)
        .flatMap(b => RecordingArea.of("image", "Image", b).left.map(_.message))
      threshold <- Velocity
        .perSecond[Deg](30)
        .left
        .map(_.message)
        .flatMap(v => IvtThreshold.of(v).left.map(_.message))
      minimum <- MinimumEventDuration.of(Span.micros(2000)).left.map(_.message)
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
          InterpolationGap.none,
          Vector(area),
          codec.method,
          IvtParameters(threshold, minimum)
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
      document <- codec.codec.encode(built._2).left.map(message)
      saved    <- (for
        i <- StoredArtifact.recordingInput("recording-input", input)
        r <- StoredArtifact.packedRecording("recording", built._1)
        s <- SavedManifest.of(
          Vector(i, r.recording) ++ r.payloads,
          ManifestRelation.RecordingOf(i.name, r.recording.name) +: r.relations
        )
      yield s).left.map(message)
    yield Saved(saved, ScientificFingerprint.of(analysis), Some(document))

  /** The temporal fixture's plan: both repetitions, every window, the binned
    * and Gaussian scales, over the pinned input with its missing epoch and
    * its anchor beyond JavaScript's exact integer range.
    */
  private def temporalPlan(
      input: TemporalStudyInput[StudyKey, Px]
  ): Either[String, TemporalStudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference]] =
    for
      frame <- input.study.trials.rows.headOption.map(_.value.frame).toRight("no trials")
      grid  <- Grid.over(frame, 2, 2).left.map(_.message)
      sigma <- Sigma.px(1).left.map(_.message)
      base  <- StudyPlan
        .cosine(
          input.study.reference,
          grid,
          "recall",
          "encode",
          Weight.Duration,
          Vector(StudyEstimate.Binned(), StudyEstimate.Gaussian(sigma, EdgePolicy.Truncate)),
          FailurePolicy.RequireAll
        )
        .left
        .map(message)
      windows <- TemporalFixtures.windows.traverse { (name, from, until) =>
        Window
          .of(Span.micros(from), Span.micros(until))
          .left
          .map(_.message)
          .flatMap(w => StudyWindow.of(name, w).left.map(_.message))
      }
      repeats <- TemporalFixtures.repetitions.traverse { (name, focal, reference) =>
        RepetitionContrast.withinParticipant(name, focal, reference).left.map(_.message)
      }
      plan <- TemporalStudyPlan
        .of(base, input.reference, windows, repeats, FixationBoundary.ClipDuration)
        .left
        .map(_.message)
    yield plan

  private def temporal: Either[String, Saved] =
    val inputs = StudyInputCodecs.study[Px]
    for
      codec <- temporalCodec
      input <- text("temporal-study-input-v1.json").flatMap(
        TemporalInputCodecs.study[Px]().input.parse(_).left.map(message)
      )
      plan     <- temporalPlan(input)
      result   <- plan.run(input).left.map(_.message)
      document <- codec.codec.encode(plan).left.map(message)
      saved    <- (for
        b <- StoredArtifact.input("base", inputs, input.study)
        t <- StoredArtifact.temporalInput(
          "temporal",
          TemporalInputCodecs.study[Px](StudyEmbedding.ByReference),
          input
        )
        s <- SavedManifest.of(
          Vector(b, t),
          Vector(ManifestRelation.TemporalBase(t.name, b.name))
        )
      yield s).left.map(message)
    yield Saved(saved, ScientificFingerprint.of(result), Some(document))

  private def store(directory: Path, study: Saved): Json =
    Files.createDirectories(directory)
    def put(name: String, bytes: Array[Byte]): Unit =
      val _ = Files.write(directory.resolve(name), bytes)
    put(manifestFile, Array.tabulate(study.saved.bytes.length)(study.saved.bytes(_)))
    put(addressFile, study.saved.address.hex.getBytes("UTF-8"))
    study.saved.artifacts.foreach(a =>
      put(a.name.value, Array.tabulate(a.bytes.length)(a.bytes(_)))
    )
    study.plan.foreach(plan => put(planFile, plan.spaces2.getBytes("UTF-8")))
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

  /** Registrations made explicitly on load; `withoutResultCodec` leaves the
    * cosine result codec unregistered, as a reader that forgot it would.
    */
  private def decoders(
      withoutResultCodec: Boolean
  ): Either[String, ArtifactDecoders[StudyKey, Px]] =
    (for
      plans   <- StudyRegistry.empty[StudyKey, Px].register(StudyCodecs.cosine[Px].registration)
      inputs  <- StudyInputRegistry.empty[StudyKey, Px].register(StudyInputCodecs.study[Px])
      results <-
        if withoutResultCodec then Right(StudyResultRegistry.empty[StudyKey, Px])
        else
          StudyResultRegistry
            .empty[StudyKey, Px]
            .register(StudyResultCodecs.cosine[Px].registration)
    yield ArtifactDecoders.of(plans, inputs, results)).left.map(message)

  /** A refusal as data: every resolve error with its entry and its case names. */
  private def refused(errors: Vector[ResolveError]): Json = Json.obj(
    "outcome" -> Json.fromString("refused"),
    "errors"  -> Json.arr(errors.map { e =>
      val underlying = e match
        case ResolveError.Decode(_, codec) => Some(codec.productPrefix)
        case _                             => None
      Json.obj(
        "entry"      -> e.entryName.fold(Json.Null)(n => Json.fromString(n.value)),
        "error"      -> Json.fromString(e.productPrefix),
        "underlying" -> underlying.fold(Json.Null)(Json.fromString),
        "message"    -> Json.fromString(e.message)
      )
    }*)
  )

  private def failed(reason: String): Json =
    Json.obj("outcome" -> Json.fromString("failed"), "reason" -> Json.fromString(reason))

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
      case Left(reason)   => Left(failed(reason))
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

  /** The rerun's fingerprints against the writer's, as the receipt reports them. */
  private def compared(
      directory: Path,
      named: Vector[(String, ScientificFingerprint.Rendered)]
  ): Either[String, Json] =
    expected(directory).flatMap { (print, doubles) =>
      val mismatched = named.collect { case (name, r) if r.json != print => name }
      Either.cond(
        mismatched.isEmpty && named.forall(_._2.doubles == doubles),
        Json.obj(
          "outcome"     -> Json.fromString("reconstructed"),
          "doubles"     -> Json.fromInt(doubles),
          "compared"    -> Json.arr(named.map((n, _) => Json.fromString(n))*),
          "fingerprint" -> Json.fromString(
            ByteDigest.sha256(IArray.unsafeFromArray(print.noSpaces.getBytes("UTF-8"))).hex
          )
        ),
        s"fingerprints differ from the writer's for ${mismatched.mkString(", ")}"
      )
    }

  private def name(value: String): Either[String, ArtifactName] =
    ArtifactName.of(value).left.map(_.message)

  private def rerunFixation(
      directory: Path,
      resolved: ResolvedManifest[StudyKey, Px]
  ): Either[String, Json] =
    val studies = StudyCodecs.cosine[Px]
    val results = StudyResultCodecs.cosine[Px]
    for
      planName   <- name("plan")
      inputName  <- name("input")
      resultName <- name("result")
      loaded     <- resolved.plan(planName).toRight("no plan")
      input      <- resolved.input(inputName).toRight("no input")
      archive    <- resolved.result(resultName).toRight("no result")
      entry      <- resolved.manifest.entry(resultName).toRight("no result entry")
      // The typed plan the application's registration stands for: the verified
      // plan re-encoded and read through the typed codec, with the same description.
      json  <- loaded.encode.left.map(message)
      typed <- studies.codec.decode(json).left.map(message)
      _     <- Either.cond(typed.description == loaded.description, (), "typed plan differs")
      _ <- Either.cond(typed.prerequisites(Some(input)).isEmpty, (), "prerequisites refused")
      rerun     <- typed.run(input).left.map(message)
      loadedRun <- loaded.run(input).left.map(message)
      encoded   <- results.codec.encode(rerun).left.map(message)
      bytes  = encoded.spaces2.getBytes("UTF-8")
      digest = ByteDigest.sha256(IArray.unsafeFromArray(bytes))
      _ <- Either.cond(
        digest == entry.sha256 && bytes.length.toLong == entry.length,
        (),
        s"the re-executed result encodes to ${digest.hex} (${bytes.length} bytes), " +
          s"not the archived ${entry.sha256.hex} (${entry.length} bytes)"
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

  private def rerunRecording(
      directory: Path,
      resolved: ResolvedManifest[StudyKey, Px]
  ): Either[String, Json] =
    for
      codec     <- recordingCodec
      registry  <- RecordingRegistry.empty.register(codec.registration).left.map(message)
      inputName <- name("recording-input")
      recName   <- name("recording")
      input     <- resolved.recordingInput(inputName).toRight("no recording input")
      recording <- resolved.recording(recName).toRight("no recording").flatMap {
        case RecordingChannels.Monocular(r) => Right(r)
        case RecordingChannels.Binocular(_) => Left("the recording is binocular")
      }
      raw    <- readText(directory.resolve(planFile))
      json   <- io.circe.parser.parse(raw).left.map(_.message)
      loaded <- registry.decode(json).left.map(message)
      _      <- Either.cond(
        RecordingInput.disagreements(input, loaded.plan).isEmpty,
        (),
        s"the plan disagrees with its input: ${RecordingInput.disagreements(input, loaded.plan)}"
      )
      _ <- Either.cond(
        loaded.plan.prerequisites(Some(recording)).isEmpty,
        (),
        s"prerequisites refused: ${loaded.plan.prerequisites(Some(recording))}"
      )
      analysis <- loaded.plan.run(recording).left.map(_.message)
      receipt  <- compared(directory, Vector("rerun" -> ScientificFingerprint.of(analysis)))
    yield receipt

  private def rerunTemporal(
      directory: Path,
      resolved: ResolvedManifest[StudyKey, Px]
  ): Either[String, Json] =
    for
      codec <- temporalCodec
      named <- name("temporal")
      input <- resolved.temporalInput(named).toRight("no temporal input")
      raw   <- readText(directory.resolve(planFile))
      plan  <- codec.codec.parse(raw).left.map(message)
      _     <- Either.cond(plan.prerequisites(Some(input)).isEmpty, (), "prerequisites refused")
      result  <- plan.run(input).left.map(_.message)
      receipt <- compared(directory, Vector("rerun" -> ScientificFingerprint.of(result)))
    yield receipt

  def read(root: Path, withoutResultCodec: Boolean): Either[String, Json] =
    decoders(withoutResultCodec).map { registered =>
      def study(
          label: String,
          rerun: (Path, ResolvedManifest[StudyKey, Px]) => Either[String, Json]
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
