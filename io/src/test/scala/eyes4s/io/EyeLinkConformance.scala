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

import eyes4s.plan.Diagnose
import cats.data.NonEmptyVector

/** Identity of one side of a scientific comparison. */
final class EyeLinkConformanceOperand private (
    val fixtureId: String,
    val artifactId: String,
    val digest: Sha256
):
  override def toString: String = s"$artifactId[fixture=$fixtureId,digest=${digest.hex}]"

object EyeLinkConformanceOperand:
  private val IdPattern = "[a-z0-9][a-z0-9.-]*".r

  def create(
      fixtureId: String,
      artifactId: String,
      digest: Sha256
  ): Either[EyeLinkConformanceError, EyeLinkConformanceOperand] =
    val fixture  = fixtureId.trim
    val artifact = artifactId.trim
    if !IdPattern.matches(fixture) then
      Left(
        EyeLinkConformanceError.InvalidOperand(
          artifact,
          "fixture_id",
          fixture,
          "[a-z0-9][a-z0-9.-]*"
        )
      )
    else if !IdPattern.matches(artifact) then
      Left(
        EyeLinkConformanceError.InvalidOperand(
          artifact,
          "artifact_id",
          artifact,
          "[a-z0-9][a-z0-9.-]*"
        )
      )
    else Right(new EyeLinkConformanceOperand(fixture, artifact, digest))

/** Value state retained during comparison. Missing and omitted are distinct:
  * an oracle that omitted a field did not certify its value.
  */
final class EyeLinkConformanceValue private (
    val presence: EyeLinkOraclePresence,
    val value: String,
    val detail: String
):
  private[io] def canonical: String =
    s"${presence.token}:${value.length}:$value:${detail.length}:$detail"

object EyeLinkConformanceValue:
  def value(value: String): EyeLinkConformanceValue =
    new EyeLinkConformanceValue(EyeLinkOraclePresence.Value, value, "")

  def missing(reason: String): Either[EyeLinkConformanceError, EyeLinkConformanceValue] =
    absent(EyeLinkOraclePresence.Missing, reason)

  def omitted(reason: String): Either[EyeLinkConformanceError, EyeLinkConformanceValue] =
    absent(EyeLinkOraclePresence.Omitted, reason)

  private def absent(
      presence: EyeLinkOraclePresence,
      reason: String
  ): Either[EyeLinkConformanceError, EyeLinkConformanceValue] =
    val normalized = reason.trim
    if normalized.isEmpty then
      Left(
        EyeLinkConformanceError.InvalidAbsentValue(presence.token, reason, "nonblank reason")
      )
    else Right(new EyeLinkConformanceValue(presence, "", normalized))

/** One uniquely named scientific fact. */
final class EyeLinkConformanceFact private (
    val fieldPath: String,
    val observed: EyeLinkConformanceValue
)

object EyeLinkConformanceFact:
  private val PathPattern = "[a-z][a-z0-9_.-]*".r

  def create(
      fieldPath: String,
      observed: EyeLinkConformanceValue
  ): Either[EyeLinkConformanceError, EyeLinkConformanceFact] =
    val path = fieldPath.trim
    if !PathPattern.matches(path) then
      Left(EyeLinkConformanceError.InvalidFieldPath(path, "[a-z][a-z0-9_.-]*"))
    else Right(new EyeLinkConformanceFact(path, observed))

/** Canonical field set for one implementation or oracle. */
final class EyeLinkConformanceManifest private (
    val operand: EyeLinkConformanceOperand,
    val facts: Vector[EyeLinkConformanceFact]
):
  lazy val byPath: Map[String, EyeLinkConformanceValue] =
    facts.map(fact => fact.fieldPath -> fact.observed).toMap

  lazy val canonical: String =
    val body =
      facts.map(fact => s"${fact.fieldPath}\t${fact.observed.canonical}").mkString("\n")
    s"eyes4s-eyelink-conformance-v1\n$body\n"

  lazy val scientificDigest: Sha256 = Sha256.ofUtf8(canonical)

object EyeLinkConformanceManifest:
  def create(
      operand: EyeLinkConformanceOperand,
      facts: Vector[EyeLinkConformanceFact]
  ): Either[NonEmptyVector[EyeLinkConformanceError], EyeLinkConformanceManifest] =
    val sorted     = facts.sortBy(_.fieldPath)
    val duplicates = sorted
      .groupBy(_.fieldPath)
      .collect {
        case (path, values) if values.length > 1 =>
          path -> values.length
      }
      .toVector
      .sortBy(_._1)
      .map { case (path, count) =>
        EyeLinkConformanceError.DuplicateField(operand.toString, path, count)
      }
    val errors =
      Option
        .when(sorted.isEmpty)(EyeLinkConformanceError.EmptyManifest(operand.toString))
        .toVector ++ duplicates
    NonEmptyVector.fromVector(errors) match
      case Some(errors) => Left(errors)
      case None         => Right(new EyeLinkConformanceManifest(operand, sorted))

  /** Project the lossless eyes4s canonical result to a deliberate semantic
    * field set. Byte locations, line terminators, and source digests are not
    * semantic and must be opted into by the caller if they are relevant.
    */
  def fromCanonical(
      fixtureId: String,
      artifactId: String,
      manifest: EyeLinkAscCanonicalManifest,
      include: String => Boolean
  ): Either[NonEmptyVector[EyeLinkConformanceError], EyeLinkConformanceManifest] =
    val selected   = manifest.entries.filter(entry => include(entry._1))
    val digestBody = selected.map { case (path, value) =>
      s"${path.length}:$path${value.length}:$value\n"
    }.mkString
    for
      operand <- EyeLinkConformanceOperand
        .create(fixtureId, artifactId, Sha256.ofUtf8(digestBody))
        .left
        .map(NonEmptyVector.one)
      facts <- selected.foldLeft[Either[NonEmptyVector[EyeLinkConformanceError], Vector[
        EyeLinkConformanceFact
      ]]](Right(Vector.empty)) { case (acc, (path, value)) =>
        for
          values <- acc
          fact   <- EyeLinkConformanceFact
            .create(path, EyeLinkConformanceValue.value(value))
            .left
            .map(NonEmptyVector.one)
        yield values :+ fact
      }
      result <- create(operand, facts)
    yield result

/** Explicit equality policy for one field. Exact is the default. */
enum EyeLinkFieldTolerance:
  case Exact
  case Decimal private[io] (
      name: String,
      absolute: BigDecimal,
      relative: BigDecimal,
      unit: String,
      rationale: String
  )

  def token: String = this match
    case Exact                                      => "exact"
    case Decimal(name, absolute, relative, unit, _) =>
      s"decimal:$name:absolute=$absolute:relative=$relative:unit=$unit"

object EyeLinkFieldTolerance:
  def decimal(
      name: String,
      absolute: BigDecimal,
      relative: BigDecimal,
      unit: String,
      rationale: String
  ): Either[EyeLinkConformanceError, EyeLinkFieldTolerance] =
    val normalizedName      = name.trim
    val normalizedUnit      = unit.trim
    val normalizedRationale = rationale.trim
    if normalizedName.isEmpty then
      Left(EyeLinkConformanceError.InvalidTolerance(name, "name", name, "nonblank"))
    else if absolute < 0 || relative < 0 || (absolute == 0 && relative == 0) then
      Left(
        EyeLinkConformanceError.InvalidTolerance(
          normalizedName,
          "bounds",
          s"absolute=$absolute,relative=$relative",
          "nonnegative with at least one positive bound"
        )
      )
    else if normalizedUnit.isEmpty then
      Left(EyeLinkConformanceError.InvalidTolerance(normalizedName, "unit", unit, "nonblank"))
    else if normalizedRationale.isEmpty then
      Left(
        EyeLinkConformanceError.InvalidTolerance(
          normalizedName,
          "rationale",
          rationale,
          "nonblank"
        )
      )
    else
      Right(
        EyeLinkFieldTolerance.Decimal(
          normalizedName,
          absolute,
          relative,
          normalizedUnit,
          normalizedRationale
        )
      )

/** A single auditable disagreement. */
final class EyeLinkConformanceMismatch private[io] (
    val fixtureId: String,
    val leftArtifact: String,
    val rightArtifact: String,
    val fieldPath: String,
    val left: Option[EyeLinkConformanceValue],
    val right: Option[EyeLinkConformanceValue],
    val tolerance: EyeLinkFieldTolerance,
    val reason: String
):
  def message: String =
    s"EyeLink conformance fixture='$fixtureId' left='$leftArtifact' right='$rightArtifact' " +
      s"field='$fieldPath' tolerance='${tolerance.token}' failed: $reason; " +
      s"left=${render(left)}, right=${render(right)}."

  private def render(value: Option[EyeLinkConformanceValue]): String = value match
    case None        => "absent-field"
    case Some(found) => found.canonical

/** Successful comparison; every common and one-sided field was examined. */
final class EyeLinkConformanceComparison private[io] (
    val fixtureId: String,
    val left: EyeLinkConformanceOperand,
    val right: EyeLinkConformanceOperand,
    val comparedFields: Int,
    val tolerances: Map[String, EyeLinkFieldTolerance]
)

object EyeLinkConformance:
  def compare(
      left: EyeLinkConformanceManifest,
      right: EyeLinkConformanceManifest,
      tolerances: Map[String, EyeLinkFieldTolerance] = Map.empty
  ): Either[NonEmptyVector[EyeLinkConformanceMismatch], EyeLinkConformanceComparison] =
    val fixture    = left.operand.fixtureId
    val paths      = (left.byPath.keySet ++ right.byPath.keySet).toVector.sorted
    val mismatches =
      if fixture != right.operand.fixtureId then
        Vector(
          new EyeLinkConformanceMismatch(
            fixture,
            left.operand.toString,
            right.operand.toString,
            "fixture.id",
            Some(EyeLinkConformanceValue.value(fixture)),
            Some(EyeLinkConformanceValue.value(right.operand.fixtureId)),
            EyeLinkFieldTolerance.Exact,
            "operands name different fixtures"
          )
        )
      else
        paths.flatMap { path =>
          val policy = tolerances.getOrElse(path, EyeLinkFieldTolerance.Exact)
          mismatch(fixture, left, right, path, policy)
        }
    NonEmptyVector.fromVector(mismatches) match
      case Some(values) => Left(values)
      case None         =>
        Right(
          new EyeLinkConformanceComparison(
            fixture,
            left.operand,
            right.operand,
            paths.length,
            tolerances
          )
        )

  private def mismatch(
      fixture: String,
      left: EyeLinkConformanceManifest,
      right: EyeLinkConformanceManifest,
      path: String,
      tolerance: EyeLinkFieldTolerance
  ): Option[EyeLinkConformanceMismatch] =
    val observedLeft  = left.byPath.get(path)
    val observedRight = right.byPath.get(path)
    val reason        = (observedLeft, observedRight) match
      case (None, _) => Some("field is absent from the left artifact")
      case (_, None) => Some("field is absent from the right artifact")
      case (Some(a), Some(b)) if a.presence != b.presence =>
        Some(s"presence differs (${a.presence.token} versus ${b.presence.token})")
      case (Some(a), Some(b)) if a.presence != EyeLinkOraclePresence.Value =>
        Option.when(a.detail != b.detail)(
          s"absence reason differs ('${a.detail}' versus '${b.detail}')"
        )
      case (Some(a), Some(b)) => compareValues(a.value, b.value, tolerance)

    reason.map(value =>
      new EyeLinkConformanceMismatch(
        fixture,
        left.operand.toString,
        right.operand.toString,
        path,
        observedLeft,
        observedRight,
        tolerance,
        value
      )
    )

  private def compareValues(
      left: String,
      right: String,
      tolerance: EyeLinkFieldTolerance
  ): Option[String] = tolerance match
    case EyeLinkFieldTolerance.Exact =>
      Option.when(left != right)("values are not exactly equal")
    case value: EyeLinkFieldTolerance.Decimal =>
      (decimalOption(left), decimalOption(right)) match
        case (Some(a), Some(b)) =>
          val difference = (a - b).abs
          val scale      = a.abs.max(b.abs)
          val limit      = value.absolute + value.relative * scale
          Option.when(difference > limit)(
            s"decimal difference=$difference exceeds limit=$limit"
          )
        case _ => Some("named decimal tolerance was applied to a non-decimal value")

  private def decimalOption(value: String): Option[BigDecimal] =
    scala.util.Try(BigDecimal(value)).toOption

/** Outcome of an evidence source. Missing is not failure and never counts as a
  * pass; it means the corresponding external implementation was unavailable.
  */
enum EyeLinkConformanceStatus(val token: String) derives CanEqual:
  case ComparedPass    extends EyeLinkConformanceStatus("compared-pass")
  case ComparedFail    extends EyeLinkConformanceStatus("compared-fail")
  case MissingExternal extends EyeLinkConformanceStatus("missing-external")
  case Unsupported     extends EyeLinkConformanceStatus("unsupported")

final class EyeLinkPortableConformanceRow private (
    val fixtureId: String,
    val eyes4sDigest: Sha256,
    val ascOracle: String,
    val ascStatus: EyeLinkConformanceStatus,
    val ascDigest: Option[Sha256],
    val edfOracle: String,
    val edfStatus: EyeLinkConformanceStatus,
    val edfDigest: Option[Sha256],
    val metamorphicAssertions: Int,
    val limitation: String
)

object EyeLinkPortableConformanceRow:
  def create(
      fixtureId: String,
      eyes4sDigest: Sha256,
      ascOracle: String,
      ascStatus: EyeLinkConformanceStatus,
      ascDigest: Option[Sha256],
      edfOracle: String,
      edfStatus: EyeLinkConformanceStatus,
      edfDigest: Option[Sha256],
      metamorphicAssertions: Int,
      limitation: String
  ): Either[EyeLinkConformanceError, EyeLinkPortableConformanceRow] =
    val fixture    = fixtureId.trim
    val oracle     = ascOracle.trim
    val edf        = edfOracle.trim
    val disclosure = limitation.trim
    if fixture.isEmpty then
      Left(
        EyeLinkConformanceError.InvalidSummaryRow(fixture, "fixture_id", fixture, "nonblank")
      )
    else
      evidenceError(fixture, "asc", oracle, ascStatus, ascDigest)
        .orElse(evidenceError(fixture, "edf", edf, edfStatus, edfDigest)) match
        case Some(error) => Left(error)
        case None        =>
          if metamorphicAssertions < 0 then
            Left(
              EyeLinkConformanceError.InvalidSummaryRow(
                fixture,
                "metamorphic_assertions",
                metamorphicAssertions.toString,
                "nonnegative"
              )
            )
          else if disclosure.isEmpty then
            Left(
              EyeLinkConformanceError.InvalidSummaryRow(
                fixture,
                "limitation",
                limitation,
                "nonblank"
              )
            )
          else
            Right(
              new EyeLinkPortableConformanceRow(
                fixture,
                eyes4sDigest,
                oracle,
                ascStatus,
                ascDigest,
                edf,
                edfStatus,
                edfDigest,
                metamorphicAssertions,
                disclosure
              )
            )

  private def evidenceError(
      fixture: String,
      source: String,
      oracle: String,
      status: EyeLinkConformanceStatus,
      digest: Option[Sha256]
  ): Option[EyeLinkConformanceError] =
    if oracle.isEmpty then
      Some(
        EyeLinkConformanceError.InvalidSummaryRow(
          fixture,
          s"${source}_oracle",
          oracle,
          "nonblank"
        )
      )
    else
      status match
        case EyeLinkConformanceStatus.ComparedPass | EyeLinkConformanceStatus.ComparedFail =>
          Option.when(digest.isEmpty)(
            EyeLinkConformanceError.InvalidSummaryRow(
              fixture,
              s"${source}_digest",
              "absent",
              s"present for ${status.token}"
            )
          )
        case EyeLinkConformanceStatus.MissingExternal | EyeLinkConformanceStatus.Unsupported =>
          Option.when(digest.nonEmpty)(
            EyeLinkConformanceError.InvalidSummaryRow(
              fixture,
              s"${source}_digest",
              "present",
              s"absent for ${status.token}"
            )
          )

final class EyeLinkPortableConformanceSummary private (
    val rows: Vector[EyeLinkPortableConformanceRow]
):
  /** Every comparison that was actually run passed. Missing external tools do
    * not become failures, but neither do they become coverage.
    */
  val availableComparisonsPassed: Boolean =
    rows.nonEmpty && rows.forall(row =>
      row.ascStatus != EyeLinkConformanceStatus.ComparedFail &&
        row.edfStatus != EyeLinkConformanceStatus.ComparedFail
    )

  val ascOracleCoverageComplete: Boolean =
    rows.nonEmpty && rows.forall(_.ascStatus == EyeLinkConformanceStatus.ComparedPass)

  val vendorEdfCertified: Boolean =
    rows.nonEmpty && rows.forall(row =>
      row.edfStatus == EyeLinkConformanceStatus.ComparedPass && row.edfDigest.nonEmpty
    )

  lazy val renderTsv: String =
    val header =
      "schema\tfixture\teyes4s_sha256\tasc_oracle\tasc_status\tasc_sha256\tedf_oracle\tedf_status\tedf_sha256\tmetamorphic_assertions\tlimitation"
    val body = rows.sortBy(_.fixtureId).map { row =>
      Vector(
        "eyes4s-eyelink-portable-conformance-v1",
        row.fixtureId,
        row.eyes4sDigest.hex,
        escape(row.ascOracle),
        row.ascStatus.token,
        row.ascDigest.fold("")(_.hex),
        escape(row.edfOracle),
        row.edfStatus.token,
        row.edfDigest.fold("")(_.hex),
        row.metamorphicAssertions.toString,
        escape(row.limitation)
      ).mkString("\t")
    }
    (header +: body).mkString("\n") + "\n"

  lazy val digest: Sha256 = Sha256.ofUtf8(renderTsv)

  private def escape(value: String): String =
    value.replace("%", "%25").replace("\t", "%09").replace("\r", "%0d").replace("\n", "%0a")

object EyeLinkPortableConformanceSummary:
  def create(
      rows: Vector[EyeLinkPortableConformanceRow]
  ): Either[NonEmptyVector[EyeLinkConformanceError], EyeLinkPortableConformanceSummary] =
    val duplicates = rows
      .groupBy(_.fixtureId)
      .collect {
        case (fixture, values) if values.length > 1 =>
          fixture -> values.length
      }
      .toVector
      .sortBy(_._1)
      .map { case (fixture, count) =>
        EyeLinkConformanceError.DuplicateSummaryFixture(fixture, count)
      }
    val errors =
      duplicates ++ Option.when(rows.isEmpty)(EyeLinkConformanceError.EmptySummary).toVector
    NonEmptyVector.fromVector(errors) match
      case Some(values) => Left(values)
      case None => Right(new EyeLinkPortableConformanceSummary(rows.sortBy(_.fixtureId)))

/** Named transformations in the portable metamorphic court. */
enum EyeLinkMetamorphicTransformation(val token: String) derives CanEqual:
  case NewlineNormalization extends EyeLinkMetamorphicTransformation("newline-normalization")
  case StructuralWhitespace extends EyeLinkMetamorphicTransformation("structural-whitespace")
  case ByteRepartition      extends EyeLinkMetamorphicTransformation("byte-repartition")
  case ConversionSubset     extends EyeLinkMetamorphicTransformation("conversion-subset")
  case GazeTranslation      extends EyeLinkMetamorphicTransformation("gaze-translation")
  case BlockConcatenation   extends EyeLinkMetamorphicTransformation("block-concatenation")

enum EyeLinkMetamorphicSupport:
  case Supported(transformation: EyeLinkMetamorphicTransformation)
  case Unsupported(
      transformation: EyeLinkMetamorphicTransformation,
      fixtureId: String,
      coordinateMode: AscCoordinateMode,
      reason: String
  )

  def message: String = this match
    case Supported(transformation) =>
      s"EyeLink metamorphic transformation='${transformation.token}' is supported."
    case Unsupported(transformation, fixtureId, coordinateMode, reason) =>
      s"EyeLink metamorphic transformation='${transformation.token}' fixture='$fixtureId' " +
        s"coordinate-mode='$coordinateMode' is unsupported: $reason."

object EyeLinkMetamorphicSupport:
  def assess(
      transformation: EyeLinkMetamorphicTransformation,
      fixtureId: String,
      coordinateMode: AscCoordinateMode
  ): EyeLinkMetamorphicSupport =
    transformation match
      case EyeLinkMetamorphicTransformation.GazeTranslation
          if coordinateMode != AscCoordinateMode.Gaze =>
        EyeLinkMetamorphicSupport.Unsupported(
          transformation,
          fixtureId,
          coordinateMode,
          "translation equivariance is defined only for screen GAZE coordinates"
        )
      case _ => EyeLinkMetamorphicSupport.Supported(transformation)

enum EyeLinkConformanceError derives CanEqual:
  case InvalidOperand(artifact: String, field: String, actual: String, expected: String)
  case InvalidAbsentValue(presence: String, actual: String, expected: String)
  case InvalidFieldPath(actual: String, expected: String)
  case DuplicateField(operand: String, fieldPath: String, count: Int)
  case EmptyManifest(operand: String)
  case InvalidTolerance(tolerance: String, field: String, actual: String, expected: String)
  case InvalidSummaryRow(fixture: String, field: String, actual: String, expected: String)
  case DuplicateSummaryFixture(fixture: String, count: Int)
  case EmptySummary

  def message: String = this match
    case InvalidOperand(artifact, field, actual, expected) =>
      s"EyeLink conformance operand='$artifact' field='$field' was '$actual'; expected $expected."
    case InvalidAbsentValue(presence, actual, expected) =>
      s"EyeLink conformance presence='$presence' reason was '$actual'; expected $expected."
    case InvalidFieldPath(actual, expected) =>
      s"EyeLink conformance field path='$actual' is invalid; expected $expected."
    case DuplicateField(operand, fieldPath, count) =>
      s"EyeLink conformance operand='$operand' has field='$fieldPath' $count times; expected once."
    case EmptyManifest(operand) =>
      s"EyeLink conformance operand='$operand' has no facts; a vacuous comparison is forbidden."
    case InvalidTolerance(tolerance, field, actual, expected) =>
      s"EyeLink tolerance='$tolerance' field='$field' was '$actual'; expected $expected."
    case InvalidSummaryRow(fixture, field, actual, expected) =>
      s"EyeLink conformance summary fixture='$fixture' field='$field' was '$actual'; expected $expected."
    case DuplicateSummaryFixture(fixture, count) =>
      s"EyeLink conformance summary fixture='$fixture' occurs $count times; expected once."
    case EmptySummary =>
      "EyeLink conformance summary has no fixture rows; expected at least one."

end EyeLinkConformanceError

// Test-scope evidence apparatus (CR9): its codes stay in IoDiagnosticCatalog,
// which only ever appends, and its Diagnose instance lives with the enum.
object EyeLinkConformanceError:
  given Diagnose[EyeLinkConformanceError, Nothing] =
    Diagnose.derived[EyeLinkConformanceError, Nothing](IoDiagnosticCatalog.eyeLinkConformance)(
      _.message
    )
