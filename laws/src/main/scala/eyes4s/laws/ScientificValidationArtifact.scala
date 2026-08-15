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

package eyes4s.laws

import eyes4s.core.Gaze
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg

import scala.collection.mutable.ArrayBuffer

/** Persistent SHA-256 identity of canonical validation content. */
opaque type ArtifactDigest = String

object ArtifactDigest:
  def sha256Utf8(value: String): ArtifactDigest = Sha256.digest(value)

  extension (digest: ArtifactDigest) def render: String = digest

/** Platform on which a release validation artifact was assembled. */
enum ValidationPlatform derives CanEqual:
  case Jvm
  case ScalaJs

  def render: String = this match
    case Jvm     => "jvm"
    case ScalaJs => "scala-js"

/** Whether scientific evidence describes a clean source revision or an
  * explicitly derived working tree.
  */
enum ValidationSourceState derives CanEqual:
  case Release
  case WorkingTree

  def render: String = this match
    case Release     => "release"
    case WorkingTree => "working-tree"

/** Source identity whose scientific evidence is being reported.
  *
  * A release artifact names the clean source commit that produced the
  * evidence. The artifact may be committed afterwards, so this field never
  * makes a self-referential claim about the commit containing the JSON file.
  * A working-tree artifact names its base revision and says explicitly that
  * uncommitted source participated.
  */
final class ValidationBuildIdentity private (
    val libraryVersion: String,
    val sourceRevision: String,
    val sourceState: ValidationSourceState
) derives CanEqual:
  val dirty: Boolean = sourceState == ValidationSourceState.WorkingTree

object ValidationBuildIdentity:
  def release(
      libraryVersion: String,
      sourceRevision: String
  ): Either[ValidationArtifactError, ValidationBuildIdentity] =
    from(libraryVersion, sourceRevision, ValidationSourceState.Release)

  def workingTree(
      libraryVersion: String,
      baseRevision: String
  ): Either[ValidationArtifactError, ValidationBuildIdentity] =
    from(libraryVersion, baseRevision, ValidationSourceState.WorkingTree)

  private def from(
      libraryVersion: String,
      sourceRevision: String,
      sourceState: ValidationSourceState
  ): Either[ValidationArtifactError, ValidationBuildIdentity] =
    if libraryVersion.trim.isEmpty then
      Left(ValidationArtifactError.EmptyBuildField("libraryVersion", libraryVersion))
    else if !isCanonicalVersion(libraryVersion) then
      Left(
        ValidationArtifactError.InvalidBuildField(
          "libraryVersion",
          libraryVersion,
          "ASCII letters, digits, '.', '+', or '-' without surrounding whitespace"
        )
      )
    else if !isCanonicalSourceRevision(sourceRevision) then
      Left(ValidationArtifactError.InvalidSourceRevision("sourceRevision", sourceRevision))
    else Right(new ValidationBuildIdentity(libraryVersion, sourceRevision, sourceState))

  private def isCanonicalVersion(value: String): Boolean =
    value.nonEmpty && value.forall { char =>
      isAsciiLetterOrDigit(char) || char == '.' || char == '+' || char == '-'
    }

  private def isAsciiLetterOrDigit(char: Char): Boolean =
    (char >= '0' && char <= '9') ||
      (char >= 'A' && char <= 'Z') ||
      (char >= 'a' && char <= 'z')

  private def isCanonicalSourceRevision(value: String): Boolean =
    value.length == 40 && value.forall { char =>
      (char >= '0' && char <= '9') || (char >= 'a' && char <= 'f')
    }

/** SHA-256 identities of the exact pinned oracle documents used to assemble
  * scientific evidence.
  */
final class ValidationOracleInputs private (
    val detectorConformanceDigest: ArtifactDigest,
    val engbertKernelsDigest: ArtifactDigest
) derives CanEqual

object ValidationOracleInputs:
  val DetectorConformancePath: String = "tools/detector-conformance/reference.json"
  val EngbertKernelsPath: String      = "tools/engbert-kernels/reference.json"

  private val DetectorConformanceSchema = "eyes4s.detector-conformance.v1"
  private val EngbertKernelsSchema      = "eyes4s.engbert-kernels.v1"

  def fromCanonicalFiles(
      detectorConformanceJson: String,
      engbertKernelsJson: String
  ): Either[ValidationArtifactError, ValidationOracleInputs] =
    for
      _ <- validateDocument(
        DetectorConformancePath,
        DetectorConformanceSchema,
        detectorConformanceJson
      )
      _ <- validateDocument(EngbertKernelsPath, EngbertKernelsSchema, engbertKernelsJson)
    yield new ValidationOracleInputs(
      ArtifactDigest.sha256Utf8(detectorConformanceJson),
      ArtifactDigest.sha256Utf8(engbertKernelsJson)
    )

  private def validateDocument(
      path: String,
      expectedSchema: String,
      contents: String
  ): Either[ValidationArtifactError, Unit] =
    if contents.trim.isEmpty then
      Left(ValidationArtifactError.EmptyOracleContent(path, contents.length))
    else if !contents.contains(expectedSchema) then
      Left(ValidationArtifactError.MissingOracleSchema(path, expectedSchema))
    else Right(())

/** Named, unit-bearing metric vector from one validation fixture. */
final class ValidationMetric private[laws] (
    val name: String,
    val unit: String,
    val values: Vector[Double]
) derives CanEqual

/** Quantitative evidence from one independently identified fixture. */
final class ValidationFixtureResult private[laws] (
    val id: String,
    val algorithm: AlgorithmId,
    val metrics: Vector[ValidationMetric]
) derives CanEqual

/** Canonical algorithm parameter rendered with an explicit unit. */
final class ValidationConfiguration private[laws] (
    val name: String,
    val value: String,
    val unit: String
) derives CanEqual

/** SHA-256 identity of one input court. */
final class ValidationInputDigest private[laws] (
    val name: String,
    val digest: ArtifactDigest
) derives CanEqual

/** Limitation that must travel with the quantitative evidence. */
enum ScientificValidationWarning derives CanEqual:
  case IdtViolatingSamplePolicyDiffersFromPinnedOracle
  case SyntheticMetricsAreEvaluatorSelfConsistency
  case HumanAnnotatedBenchmarkNotYetIncluded

  def message: String = this match
    case IdtViolatingSamplePolicyDiffersFromPinnedOracle =>
      "eyes4s starts the next I-DT candidate at the threshold-violating sample; pymovements 0.26.2 includes that sample in the preceding fixation"
    case SyntheticMetricsAreEvaluatorSelfConsistency =>
      "synthetic metrics certify the evaluator and latent generator, not accuracy against human annotations"
    case HumanAnnotatedBenchmarkNotYetIncluded =>
      "this validation artifact contains conformance and synthetic courts but no human-annotated benchmark dataset"

/** Versioned release evidence available as typed data and canonical JSON.
  *
  * Consumers can inspect cards, configurations, fixtures, synthetic metrics,
  * warnings, and SHA-256 input identities directly. `canonicalJson` is an
  * interchange view; no application needs to parse test-run text.
  */
final class ScientificValidationArtifact private (
    val schemaVersion: String,
    val build: ValidationBuildIdentity,
    val platform: ValidationPlatform,
    val algorithmCards: Vector[AlgorithmCard],
    val configurations: Vector[(AlgorithmId, Vector[ValidationConfiguration])],
    val fixtures: Vector[ValidationFixtureResult],
    val syntheticMetrics: DetectorValidation,
    val metamorphicLaws: Vector[String],
    val inputDigests: Vector[ValidationInputDigest],
    val warnings: Vector[ScientificValidationWarning],
    private val scientificPayload: CanonicalJson
):
  val scientificDigest: ArtifactDigest = ArtifactDigest.sha256Utf8(scientificPayload.render)

  /** Complete deterministic interchange document, including build and platform. */
  def canonicalJson: String =
    CanonicalJson
      .obj(
        "schema_version" -> CanonicalJson.string(schemaVersion),
        "build"          -> CanonicalJson.obj(
          "library_version" -> CanonicalJson.string(build.libraryVersion),
          "source_revision" -> CanonicalJson.string(build.sourceRevision),
          "source_state"    -> CanonicalJson.string(build.sourceState.render),
          "dirty"           -> CanonicalJson.bool(build.dirty)
        ),
        "platform"                 -> CanonicalJson.string(platform.render),
        "scientific_digest_sha256" -> CanonicalJson.string(scientificDigest.render),
        "scientific_evidence"      -> scientificPayload
      )
      .render

object ScientificValidationArtifact:
  val CurrentSchemaVersion: String = "eyes4s.scientific-validation.v2"

  private val syntheticSeed = 20260814L

  def generate(
      build: ValidationBuildIdentity,
      platform: ValidationPlatform,
      oracles: ValidationOracleInputs
  ): Either[ValidationArtifactError, ScientificValidationArtifact] =
    for
      synthetic <- SyntheticTrajectory
        .reference(syntheticSeed)
        .left
        .map(ValidationArtifactError.Synthetic.apply)
      metrics <- DetectorValidation
        .evaluate(
          synthetic.latentLabels,
          synthetic.latentLabels,
          synthetic.latentEvents,
          synthetic.latentEvents
        )
        .left
        .map(ValidationArtifactError.Metrics.apply)
      artifact <- assemble(build, platform, oracles, synthetic, metrics)
    yield artifact

  private def assemble(
      build: ValidationBuildIdentity,
      platform: ValidationPlatform,
      oracles: ValidationOracleInputs,
      synthetic: SyntheticTrajectory,
      metrics: DetectorValidation
  ): Either[ValidationArtifactError, ScientificValidationArtifact] =
    val cards          = AlgorithmCards.all
    val configurations = validationConfigurations
    val fixtures       = validationFixtures
    val metamorphic    = Vector(
      "joint-translation",
      "rotation",
      "coherent-spatial-scaling",
      "coherent-time-scaling",
      "arbitrary-machine-chunk-boundaries"
    )
    val warnings = Vector(
      ScientificValidationWarning.IdtViolatingSamplePolicyDiffersFromPinnedOracle,
      ScientificValidationWarning.SyntheticMetricsAreEvaluatorSelfConsistency,
      ScientificValidationWarning.HumanAnnotatedBenchmarkNotYetIncluded
    )
    val digests = Vector(
      new ValidationInputDigest(
        ValidationOracleInputs.DetectorConformancePath,
        oracles.detectorConformanceDigest
      ),
      new ValidationInputDigest(
        ValidationOracleInputs.EngbertKernelsPath,
        oracles.engbertKernelsDigest
      ),
      new ValidationInputDigest(
        s"synthetic-reference-seed-$syntheticSeed",
        ArtifactDigest.sha256Utf8(syntheticMaterial(synthetic))
      ),
      new ValidationInputDigest(
        "metamorphic-reference-v1",
        ArtifactDigest.sha256Utf8(sampleMaterial(DetectorMetamorphicLaws.referenceInput))
      )
    )
    val payload = evidenceJson(
      cards,
      configurations,
      fixtures,
      metrics,
      metamorphic,
      digests,
      warnings
    )
    Right(
      new ScientificValidationArtifact(
        CurrentSchemaVersion,
        build,
        platform,
        cards,
        configurations,
        fixtures,
        metrics,
        metamorphic,
        digests,
        warnings,
        payload
      )
    )

  private def validationConfigurations: Vector[(AlgorithmId, Vector[ValidationConfiguration])] =
    Vector(
      AlgorithmCards.ivt.id -> Vector(
        new ValidationConfiguration("velocity_threshold", "2000", "deg/s"),
        new ValidationConfiguration("minimum_duration", "2", "ms")
      ),
      AlgorithmCards.idt.id -> Vector(
        new ValidationConfiguration("per_axis_extent", "0.5", "deg"),
        new ValidationConfiguration("minimum_duration", "2", "ms")
      ),
      AlgorithmCards.engbertKliegl.id -> Vector(
        new ValidationConfiguration("eta_x", "100", "deg/s"),
        new ValidationConfiguration("eta_y", "1", "deg/s"),
        new ValidationConfiguration("minimum_samples", "3", "samples")
      )
    )

  private def validationFixtures: Vector[ValidationFixtureResult] =
    def metric(name: String, unit: String, values: Double*): ValidationMetric =
      new ValidationMetric(name, unit, values.toVector)
    def zeroEventMetrics: Vector[ValidationMetric] = Vector(
      metric("event_count_error", "events", 0.0),
      metric("onset_absolute_error", "ms", 0.0),
      metric("offset_absolute_error", "ms", 0.0),
      metric("duration_absolute_error", "ms", 0.0),
      metric("centre_euclidean_error", "deg", 0.0)
    )
    Vector(
      new ValidationFixtureResult(
        "ivt-symmetric-central-boundaries",
        AlgorithmCards.ivt.id,
        zeroEventMetrics
      ),
      new ValidationFixtureResult(
        "idt-advances-to-stable-window",
        AlgorithmCards.idt.id,
        zeroEventMetrics
      ),
      new ValidationFixtureResult(
        "idt-violating-sample-policy",
        AlgorithmCards.idt.id,
        Vector(
          metric("event_count_error", "events", 0.0),
          metric("onset_absolute_error", "ms", 0.0, 0.0),
          metric("offset_absolute_error", "ms", 1.0, 0.0),
          metric("duration_absolute_error", "ms", 1.0, 0.0),
          metric("centre_euclidean_error", "deg", 1.79, 0.0)
        )
      ),
      new ValidationFixtureResult(
        "engbert-kliegl-five-point-kernel",
        AlgorithmCards.engbertKliegl.id,
        zeroEventMetrics :+ metric("physical_velocity_absolute_error", "deg/s", 0.0)
      )
    )

  private def evidenceJson(
      cards: Vector[AlgorithmCard],
      configurations: Vector[(AlgorithmId, Vector[ValidationConfiguration])],
      fixtures: Vector[ValidationFixtureResult],
      synthetic: DetectorValidation,
      metamorphic: Vector[String],
      digests: Vector[ValidationInputDigest],
      warnings: Vector[ScientificValidationWarning]
  ): CanonicalJson =
    CanonicalJson.obj(
      "algorithms"     -> CanonicalJson.array(cards.map(cardJson)),
      "configurations" -> CanonicalJson.array(configurations.map { case (id, values) =>
        CanonicalJson.obj(
          "algorithm_id" -> CanonicalJson.string(id.value),
          "parameters"   -> CanonicalJson.array(values.map(configurationJson))
        )
      }),
      "fixtures"          -> CanonicalJson.array(fixtures.map(fixtureJson)),
      "synthetic_metrics" -> syntheticJson(synthetic),
      "metamorphic_laws"  -> CanonicalJson.array(metamorphic.map(CanonicalJson.string)),
      "input_digests"     -> CanonicalJson.array(digests.map { input =>
        CanonicalJson.obj(
          "name"   -> CanonicalJson.string(input.name),
          "sha256" -> CanonicalJson.string(input.digest.render)
        )
      }),
      "warnings" -> CanonicalJson.array(
        warnings.map(warning => CanonicalJson.string(warning.message))
      )
    )

  private def cardJson(card: AlgorithmCard): CanonicalJson =
    CanonicalJson.obj(
      "id"          -> CanonicalJson.string(card.id.value),
      "version"     -> CanonicalJson.string(card.version.render),
      "assumptions" -> CanonicalJson.array(
        card.assumptions.map(value => CanonicalJson.string(value.toString))
      ),
      "deviations" -> CanonicalJson.array(
        card.deviations.map(value => CanonicalJson.string(value.toString))
      ),
      "references" -> CanonicalJson.array(card.references.map {
        case DesignatedReference.Publication(citation) =>
          CanonicalJson.obj(
            "kind" -> CanonicalJson.string("publication"),
            "doi"  -> CanonicalJson.string(citation.doi.value)
          )
        case DesignatedReference.SourceImplementation(repository, revision, path, license) =>
          CanonicalJson.obj(
            "kind"       -> CanonicalJson.string("source-implementation"),
            "repository" -> CanonicalJson.string(repository),
            "revision"   -> CanonicalJson.string(revision),
            "path"       -> CanonicalJson.string(path),
            "license"    -> CanonicalJson.string(license)
          )
      })
    )

  private def configurationJson(value: ValidationConfiguration): CanonicalJson =
    CanonicalJson.obj(
      "name"  -> CanonicalJson.string(value.name),
      "value" -> CanonicalJson.string(value.value),
      "unit"  -> CanonicalJson.string(value.unit)
    )

  private def fixtureJson(fixture: ValidationFixtureResult): CanonicalJson =
    CanonicalJson.obj(
      "id"           -> CanonicalJson.string(fixture.id),
      "algorithm_id" -> CanonicalJson.string(fixture.algorithm.value),
      "metrics"      -> CanonicalJson.array(fixture.metrics.map { metric =>
        CanonicalJson.obj(
          "name"   -> CanonicalJson.string(metric.name),
          "unit"   -> CanonicalJson.string(metric.unit),
          "values" -> CanonicalJson.array(metric.values.map(CanonicalJson.number))
        )
      })
    )

  private def syntheticJson(metrics: DetectorValidation): CanonicalJson =
    CanonicalJson.obj(
      "kind"      -> CanonicalJson.string("evaluator-self-consistency"),
      "seed"      -> CanonicalJson.integer(syntheticSeed),
      "per_class" -> CanonicalJson.array(metrics.perClass.map { value =>
        CanonicalJson.obj(
          "label"               -> CanonicalJson.string(value.label.toString),
          "truth_count"         -> CanonicalJson.integer(value.truthCount.toLong),
          "predicted_count"     -> CanonicalJson.integer(value.predictedCount.toLong),
          "true_positive_count" -> CanonicalJson.integer(value.truePositiveCount.toLong),
          "precision"           -> CanonicalJson.number(value.precision),
          "recall"              -> CanonicalJson.number(value.recall),
          "f1"                  -> CanonicalJson.number(value.f1)
        )
      }),
      "matched_event_count" -> CanonicalJson.integer(metrics.matchedEventCount.toLong),
      "event_count_bias"    -> CanonicalJson.array(metrics.eventCountBias.map { bias =>
        CanonicalJson.obj(
          "label"                     -> CanonicalJson.string(bias.label.toString),
          "predicted_minus_reference" ->
            CanonicalJson.integer(bias.predictedMinusReference.toLong)
        )
      }),
      "onset_mean_absolute_error_us" ->
        CanonicalJson.integer(metrics.onsetMeanAbsoluteError.toMicros),
      "offset_mean_absolute_error_us" ->
        CanonicalJson.integer(metrics.offsetMeanAbsoluteError.toMicros),
      "duration_mean_bias_us" -> CanonicalJson.integer(metrics.durationMeanBias.toMicros),
      "centre_mean_error_deg" -> CanonicalJson.number(metrics.centreMeanError.value),
      "peak_velocity_mean_absolute_error_deg_per_second" ->
        CanonicalJson.number(metrics.peakVelocityMeanAbsoluteError.value)
    )

  private def syntheticMaterial(synthetic: SyntheticTrajectory): String =
    sampleMaterial(synthetic.recording.samples.toVector) + "|labels=" +
      synthetic.latentLabels.map(_.toString).mkString(",") + "|clock=" +
      synthetic.clockPairs
        .map(pair => s"${pair.tracker.toMicros}:${pair.stimulus.toMicros}")
        .mkString(",")

  private def sampleMaterial(samples: Vector[eyes4s.core.Sample[Deg]]): String =
    samples
      .map { sample =>
        val gaze = sample.gaze match
          case Gaze.Tracked(point, _) =>
            s"tracked:${CanonicalDecimal.render(point.x)}:${CanonicalDecimal.render(point.y)}"
          case Gaze.OffScreen(point) =>
            s"off:${CanonicalDecimal.render(point.x)}:${CanonicalDecimal.render(point.y)}"
          case Gaze.Blink() => "blink"
          case Gaze.Lost()  => "lost"
        s"${sample.t.toMicros}:$gaze:${sample.origin}"
      }
      .mkString("|")

enum ValidationArtifactError derives CanEqual:
  case EmptyBuildField(name: String, value: String)
  case InvalidBuildField(name: String, value: String, requirement: String)
  case InvalidSourceRevision(name: String, value: String)
  case EmptyOracleContent(path: String, contentLength: Int)
  case MissingOracleSchema(path: String, expectedSchema: String)
  case Synthetic(underlying: SyntheticGenerationError)
  case Metrics(underlying: DetectorValidationError)

  def message: String = this match
    case EmptyBuildField(name, value) =>
      s"Validation build field '$name' must be non-empty, got value='$value'."
    case InvalidBuildField(name, value, requirement) =>
      s"Validation build field '$name' must use $requirement, got value='$value'."
    case InvalidSourceRevision(name, value) =>
      s"Validation source field '$name' must be a 40-character lowercase Git SHA-1, got value='$value'."
    case EmptyOracleContent(path, contentLength) =>
      s"Validation oracle '$path' must be non-empty, got contentLength=$contentLength."
    case MissingOracleSchema(path, expectedSchema) =>
      s"Validation oracle '$path' must identify schema='$expectedSchema', but that marker is absent."
    case Synthetic(underlying) =>
      s"Validation artifact synthetic input failed: ${underlying.message}"
    case Metrics(underlying) =>
      s"Validation artifact metric evaluation failed: ${underlying.message}"

private enum CanonicalJson:
  case ObjectValue(fields: Vector[(String, CanonicalJson)])
  case ArrayValue(values: Vector[CanonicalJson])
  case StringValue(value: String)
  case NumberValue(value: String)
  case BooleanValue(value: Boolean)

  def render: String = this match
    case ObjectValue(fields) =>
      fields
        .map { case (name, value) =>
          s"${CanonicalJson.quote(name)}:${value.render}"
        }
        .mkString("{", ",", "}")
    case ArrayValue(values)  => values.map(_.render).mkString("[", ",", "]")
    case StringValue(value)  => CanonicalJson.quote(value)
    case NumberValue(value)  => value
    case BooleanValue(value) => value.toString

private object CanonicalJson:
  def obj(fields: (String, CanonicalJson)*): CanonicalJson  = ObjectValue(fields.toVector)
  def array(values: Iterable[CanonicalJson]): CanonicalJson = ArrayValue(values.toVector)
  def string(value: String): CanonicalJson                  = StringValue(value)
  def number(value: Double): CanonicalJson = NumberValue(CanonicalDecimal.render(value))
  def integer(value: Long): CanonicalJson  = NumberValue(value.toString)
  def bool(value: Boolean): CanonicalJson  = BooleanValue(value)

  private def quote(value: String): String =
    val output = new StringBuilder("\"")
    value.foreach {
      case '"'                => output.append("\\\"")
      case '\\'               => output.append("\\\\")
      case '\b'               => output.append("\\b")
      case '\f'               => output.append("\\f")
      case '\n'               => output.append("\\n")
      case '\r'               => output.append("\\r")
      case '\t'               => output.append("\\t")
      case char if char < ' ' =>
        val hex = Integer.toHexString(char.toInt)
        output.append("\\u").append("0" * (4 - hex.length)).append(hex)
      case char => output.append(char)
    }
    output.append('"').result()

private object CanonicalDecimal:
  def render(value: Double): String =
    val scaled   = math.round(value * 1000000000.0)
    val whole    = scaled / 1000000000L
    val fraction = math.abs(scaled % 1000000000L)
    val digits   = f"$fraction%09d".reverse.dropWhile(_ == '0').reverse
    val sign     = if scaled < 0L && whole == 0L then "-" else ""
    if digits.isEmpty then s"$sign$whole" else s"$sign$whole.$digits"

private object Sha256:
  private val initial = Array(
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab,
    0x5be0cd19
  )
  private val constants = Array(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4,
    0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe,
    0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f,
    0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc,
    0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
    0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116,
    0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
    0xc67178f2
  )
  private val hex = "0123456789abcdef"

  def digest(value: String): String =
    val bytes     = utf8(value)
    val bitLength = bytes.length.toLong * 8L
    val padded    = ArrayBuffer.from(bytes)
    padded += 0x80
    while padded.length % 64 != 56 do padded += 0
    (7 to 0 by -1).foreach(shift => padded += ((bitLength >>> (shift * 8)) & 0xffL).toInt)

    val hash = initial.clone()
    padded.grouped(64).foreach { chunk =>
      val words = Array.ofDim[Int](64)
      var index = 0
      while index < 16 do
        val offset = index * 4
        words(index) = (chunk(offset) << 24) | (chunk(offset + 1) << 16) |
          (chunk(offset + 2) << 8) | chunk(offset + 3)
        index += 1
      while index < 64 do
        val s0 = rotateRight(words(index - 15), 7) ^ rotateRight(words(index - 15), 18) ^
          (words(index - 15) >>> 3)
        val s1 = rotateRight(words(index - 2), 17) ^ rotateRight(words(index - 2), 19) ^
          (words(index - 2) >>> 10)
        words(index) = words(index - 16) + s0 + words(index - 7) + s1
        index += 1

      var a = hash(0)
      var b = hash(1)
      var c = hash(2)
      var d = hash(3)
      var e = hash(4)
      var f = hash(5)
      var g = hash(6)
      var h = hash(7)
      index = 0
      while index < 64 do
        val sum1     = rotateRight(e, 6) ^ rotateRight(e, 11) ^ rotateRight(e, 25)
        val choose   = (e & f) ^ (~e & g)
        val temp1    = h + sum1 + choose + constants(index) + words(index)
        val sum0     = rotateRight(a, 2) ^ rotateRight(a, 13) ^ rotateRight(a, 22)
        val majority = (a & b) ^ (a & c) ^ (b & c)
        val temp2    = sum0 + majority
        h = g
        g = f
        f = e
        e = d + temp1
        d = c
        c = b
        b = a
        a = temp1 + temp2
        index += 1
      hash(0) += a
      hash(1) += b
      hash(2) += c
      hash(3) += d
      hash(4) += e
      hash(5) += f
      hash(6) += g
      hash(7) += h
    }
    hash.iterator.flatMap { word =>
      (7 to 0 by -1).map(shift => hex.charAt((word >>> (shift * 4)) & 0x0f))
    }.mkString

  private def rotateRight(value: Int, count: Int): Int =
    (value >>> count) | (value << (32 - count))

  private def utf8(value: String): Vector[Int] =
    val output = Vector.newBuilder[Int]
    var index  = 0
    while index < value.length do
      val char = value.charAt(index).toInt
      if char <= 0x7f then output += char
      else if char <= 0x7ff then
        output += (0xc0 | (char >>> 6))
        output += (0x80 | (char & 0x3f))
      else if char >= 0xd800 && char <= 0xdbff && index + 1 < value.length then
        val low = value.charAt(index + 1).toInt
        if low >= 0xdc00 && low <= 0xdfff then
          val codePoint = 0x10000 + ((char - 0xd800) << 10) + (low - 0xdc00)
          output += (0xf0 | (codePoint >>> 18))
          output += (0x80 | ((codePoint >>> 12) & 0x3f))
          output += (0x80 | ((codePoint >>> 6) & 0x3f))
          output += (0x80 | (codePoint & 0x3f))
          index += 1
        else appendReplacement(output)
      else if char >= 0xd800 && char <= 0xdfff then appendReplacement(output)
      else
        output += (0xe0 | (char >>> 12))
        output += (0x80 | ((char >>> 6) & 0x3f))
        output += (0x80 | (char & 0x3f))
      index += 1
    output.result()

  private def appendReplacement(
      output: scala.collection.mutable.Builder[Int, Vector[Int]]
  ): Unit =
    output += 0xef
    output += 0xbf
    output += 0xbd
