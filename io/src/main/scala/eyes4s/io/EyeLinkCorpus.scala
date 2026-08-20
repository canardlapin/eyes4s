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

import cats.data.NonEmptyVector

/** Scientific origin of an EyeLink conformance fixture. */
enum EyeLinkCorpusKind(val token: String) derives CanEqual:
  case Synthetic           extends EyeLinkCorpusKind("synthetic")
  case RedistributableReal extends EyeLinkCorpusKind("redistributable-real")
  case PrivateRealPair     extends EyeLinkCorpusKind("private-real-pair")

/** Whether fixture bytes are present in the public corpus. */
enum EyeLinkCorpusAvailability(val token: String) derives CanEqual:
  case Included   extends EyeLinkCorpusAvailability("included")
  case DigestOnly extends EyeLinkCorpusAvailability("digest-only")
  case Planned    extends EyeLinkCorpusAvailability("planned")

/** Recorded legal disposition of fixture bytes. */
enum EyeLinkCorpusLicense(val token: String) derives CanEqual:
  case ProjectGeneratedApache2 extends EyeLinkCorpusLicense("project-generated-apache-2.0")
  case RedistributionGranted   extends EyeLinkCorpusLicense("redistribution-granted")
  case PrivateNoRedistribution extends EyeLinkCorpusLicense("private-no-redistribution")
  case PermissionPending       extends EyeLinkCorpusLicense("permission-pending")

/** Recorded privacy disposition of fixture bytes. */
enum EyeLinkCorpusPrivacy(val token: String) derives CanEqual:
  case NoHumanData extends EyeLinkCorpusPrivacy("no-human-data")
  case DeidentifiedForRedistribution
      extends EyeLinkCorpusPrivacy("deidentified-for-redistribution")
  case PrivateRestricted extends EyeLinkCorpusPrivacy("private-restricted")
  case ReviewPending     extends EyeLinkCorpusPrivacy("review-pending")

/** Tracker family recorded by a fixture or acquisition plan. */
enum EyeLinkCorpusHardware(val token: String) derives CanEqual:
  case Synthetic         extends EyeLinkCorpusHardware("synthetic")
  case EyeLink1000Family extends EyeLinkCorpusHardware("eyelink-1000-family")
  case PortableDuo       extends EyeLinkCorpusHardware("portable-duo")
  case EyeLink3          extends EyeLinkCorpusHardware("eyelink-3")
  case Legacy            extends EyeLinkCorpusHardware("legacy")
  case Unknown           extends EyeLinkCorpusHardware("unknown")

/** Eye layout present in one fixture. `MixedBlocks` means that blocks differ. */
enum EyeLinkCorpusEyeLayout(val token: String) derives CanEqual:
  case Left        extends EyeLinkCorpusEyeLayout("left")
  case Right       extends EyeLinkCorpusEyeLayout("right")
  case Binocular   extends EyeLinkCorpusEyeLayout("binocular")
  case MixedBlocks extends EyeLinkCorpusEyeLayout("mixed-blocks")

/** Tracking layout present in one fixture. */
enum EyeLinkCorpusTracking(val token: String) derives CanEqual:
  case HeadFixed   extends EyeLinkCorpusTracking("head-fixed")
  case Remote      extends EyeLinkCorpusTracking("remote")
  case MixedBlocks extends EyeLinkCorpusTracking("mixed-blocks")

/** Native coordinate columns present in one fixture. */
enum EyeLinkCorpusCoordinates(val token: String) derives CanEqual:
  case Gaze extends EyeLinkCorpusCoordinates("gaze")
  case Href extends EyeLinkCorpusCoordinates("href")
  case Raw  extends EyeLinkCorpusCoordinates("raw")

/** One validated, immutable corpus row.
  *
  * The constructor is private because a row is scientific evidence. In
  * particular, planned acquisitions cannot carry digests, included files
  * cannot be private, and real acquired data must carry a complete declared
  * EDF2ASC receipt. These are representation invariants, not caller policy.
  */
final class EyeLinkCorpusFixture private (
    val id: String,
    val kind: EyeLinkCorpusKind,
    val availability: EyeLinkCorpusAvailability,
    val license: EyeLinkCorpusLicense,
    val privacy: EyeLinkCorpusPrivacy,
    val permissionReference: String,
    val localPath: Option[String],
    val hardware: EyeLinkCorpusHardware,
    val firmware: Option[String],
    val samplingRatesHz: Vector[Int],
    val eyeLayout: EyeLinkCorpusEyeLayout,
    val tracking: EyeLinkCorpusTracking,
    val coordinateModes: Vector[EyeLinkCorpusCoordinates],
    val conversionReceipt: Option[Edf2AscReceipt],
    val ascDigest: Option[Sha256],
    val expectedCapabilities: Vector[EyeLinkCapability],
    val manifestLine: Int
):
  def isPubliclyLoadable: Boolean =
    availability == EyeLinkCorpusAvailability.Included &&
      kind != EyeLinkCorpusKind.PrivateRealPair &&
      localPath.nonEmpty

  def containsAcquiredRealData: Boolean =
    kind != EyeLinkCorpusKind.Synthetic && availability != EyeLinkCorpusAvailability.Planned

  def capabilityIds: Vector[String] = expectedCapabilities.map(_.id)

  override def toString: String = s"EyeLinkCorpusFixture($id,$kind,$availability)"

object EyeLinkCorpusFixture:
  private val IdPattern      = "[a-z0-9][a-z0-9-]*".r
  private val CorpusRates    = Set(250, 500, 1000, 2000)
  private val HardwareClaims = Map(
    EyeLinkCorpusHardware.EyeLink1000Family -> "hardware-1000-family",
    EyeLinkCorpusHardware.PortableDuo       -> "hardware-portable-duo",
    EyeLinkCorpusHardware.EyeLink3          -> "hardware-eyelink-3",
    EyeLinkCorpusHardware.Legacy            -> "hardware-legacy"
  )

  def create(
      source: String,
      manifestLine: Int,
      id: String,
      kind: EyeLinkCorpusKind,
      availability: EyeLinkCorpusAvailability,
      license: EyeLinkCorpusLicense,
      privacy: EyeLinkCorpusPrivacy,
      permissionReference: String,
      localPath: Option[String],
      hardware: EyeLinkCorpusHardware,
      firmware: Option[String],
      samplingRatesHz: Vector[Int],
      eyeLayout: EyeLinkCorpusEyeLayout,
      tracking: EyeLinkCorpusTracking,
      coordinateModes: Vector[EyeLinkCorpusCoordinates],
      conversionReceipt: Option[Edf2AscReceipt],
      ascDigest: Option[Sha256],
      expectedCapabilityIds: Vector[String]
  ): Either[EyeLinkCorpusError, EyeLinkCorpusFixture] =
    val normalizedId         = id.trim
    val normalizedPermission = permissionReference.trim
    val normalizedFirmware   = firmware.map(_.trim)
    val normalizedPath       = localPath.map(_.trim)
    val rates                = samplingRatesHz.distinct.sorted
    val coordinates          = coordinateModes.distinct.sortBy(_.token)
    val capabilityIds        = expectedCapabilityIds.map(_.trim).distinct.sorted
    val capabilities         = capabilityIds.flatMap(EyeLinkSupport.find)
    val unknownCapabilities  = capabilityIds.filterNot(id => EyeLinkSupport.find(id).nonEmpty)
    val rejectedCapabilities = capabilities.filter(
      _.disposition == EyeLinkDisposition.RejectExplicitly
    )

    def invalid(detail: String): Left[EyeLinkCorpusError, EyeLinkCorpusFixture] =
      Left(EyeLinkCorpusError.InvalidFixture(source, manifestLine, normalizedId, detail))

    if IdPattern.matches(normalizedId) == false then
      invalid("fixture id must match [a-z0-9][a-z0-9-]*")
    else if normalizedPermission.isEmpty then invalid("permission reference is blank")
    else if normalizedFirmware.exists(_.isEmpty) then invalid("firmware is present but blank")
    else if rates.isEmpty then invalid("at least one sampling rate is required")
    else if rates.exists(rate => !CorpusRates.contains(rate)) then
      invalid(s"sampling rates must be drawn from ${CorpusRates.toVector.sorted.mkString(",")}")
    else if coordinates.isEmpty then invalid("at least one coordinate mode is required")
    else if capabilityIds.isEmpty then invalid("at least one expected capability is required")
    else if unknownCapabilities.nonEmpty then
      invalid(s"unknown support capabilities=${unknownCapabilities.mkString(",")}")
    else if rejectedCapabilities.nonEmpty then
      invalid(
        s"explicitly rejected capabilities=${rejectedCapabilities.map(_.id).mkString(",")}"
      )
    else if !capabilityIds.contains("source-asc") then
      invalid("every portable corpus row must declare source-asc")
    else
      validatePath(source, manifestLine, normalizedId, normalizedPath).flatMap { _ =>
        validateEvidenceShape(
          source,
          manifestLine,
          normalizedId,
          kind,
          availability,
          license,
          privacy,
          normalizedPath,
          hardware,
          conversionReceipt,
          ascDigest
        ).flatMap { _ =>
          validateMetadataClaims(
            source,
            manifestLine,
            normalizedId,
            kind,
            hardware,
            rates,
            eyeLayout,
            tracking,
            coordinates,
            capabilityIds
          ).map { _ =>
            new EyeLinkCorpusFixture(
              normalizedId,
              kind,
              availability,
              license,
              privacy,
              normalizedPermission,
              normalizedPath,
              hardware,
              normalizedFirmware,
              rates,
              eyeLayout,
              tracking,
              coordinates,
              conversionReceipt,
              ascDigest,
              capabilities.sortBy(_.id),
              manifestLine
            )
          }
        }
      }

  private def validatePath(
      source: String,
      line: Int,
      fixtureId: String,
      path: Option[String]
  ): Either[EyeLinkCorpusError, Unit] =
    path match
      case Some(value)
          if value.isEmpty || value.startsWith("/") || value.startsWith("\\") ||
            value.contains("\\") || value.split('/').contains("..") =>
        Left(EyeLinkCorpusError.UnsafeLocalPath(source, line, fixtureId, value))
      case _ => Right(())

  private def validateEvidenceShape(
      source: String,
      line: Int,
      id: String,
      kind: EyeLinkCorpusKind,
      availability: EyeLinkCorpusAvailability,
      license: EyeLinkCorpusLicense,
      privacy: EyeLinkCorpusPrivacy,
      localPath: Option[String],
      hardware: EyeLinkCorpusHardware,
      receipt: Option[Edf2AscReceipt],
      ascDigest: Option[Sha256]
  ): Either[EyeLinkCorpusError, Unit] =
    def invalid(detail: String): Left[EyeLinkCorpusError, Unit] =
      Left(EyeLinkCorpusError.InvalidFixture(source, line, id, detail))

    availability match
      case EyeLinkCorpusAvailability.Planned =>
        if localPath.nonEmpty || receipt.nonEmpty || ascDigest.nonEmpty then
          invalid("planned acquisition cannot carry a path, digest, or conversion receipt")
        else if license != EyeLinkCorpusLicense.PermissionPending ||
          privacy != EyeLinkCorpusPrivacy.ReviewPending
        then invalid("planned acquisition must remain permission-pending and review-pending")
        else if kind == EyeLinkCorpusKind.Synthetic then
          invalid(
            "synthetic fixtures are generated now rather than represented as planned acquisitions"
          )
        else if hardware == EyeLinkCorpusHardware.Synthetic then
          invalid("planned real acquisition cannot use synthetic hardware")
        else Right(())

      case EyeLinkCorpusAvailability.Included =>
        if localPath.isEmpty || ascDigest.isEmpty then
          invalid("included fixture requires a safe local path and ASC digest")
        else
          kind match
            case EyeLinkCorpusKind.Synthetic =>
              if license != EyeLinkCorpusLicense.ProjectGeneratedApache2 ||
                privacy != EyeLinkCorpusPrivacy.NoHumanData ||
                hardware != EyeLinkCorpusHardware.Synthetic || receipt.nonEmpty
              then
                invalid(
                  "included synthetic fixture must be Apache-2.0, contain no human data, use synthetic hardware, and omit converter evidence"
                )
              else Right(())
            case EyeLinkCorpusKind.RedistributableReal =>
              if license != EyeLinkCorpusLicense.RedistributionGranted ||
                privacy != EyeLinkCorpusPrivacy.DeidentifiedForRedistribution || receipt.isEmpty
              then
                invalid(
                  "included real fixture requires documented redistribution, deidentification, and a conversion receipt"
                )
              else if hardware == EyeLinkCorpusHardware.Synthetic then
                invalid("included real fixture cannot use synthetic hardware")
              else validateReceiptDigest(source, line, id, receipt, ascDigest)
            case EyeLinkCorpusKind.PrivateRealPair =>
              invalid("private real data cannot be included in the public corpus")

      case EyeLinkCorpusAvailability.DigestOnly =>
        if kind != EyeLinkCorpusKind.PrivateRealPair || localPath.nonEmpty ||
          license != EyeLinkCorpusLicense.PrivateNoRedistribution ||
          privacy != EyeLinkCorpusPrivacy.PrivateRestricted || receipt.isEmpty || ascDigest.isEmpty
        then
          invalid(
            "digest-only evidence must be a private, non-redistributable real EDF/ASC pair with no local path and a complete conversion receipt"
          )
        else if receipt.flatMap(_.edfDigest).isEmpty then
          invalid("private real EDF/ASC pair requires both EDF and ASC digests")
        else if hardware == EyeLinkCorpusHardware.Synthetic then
          invalid("private real evidence cannot use synthetic hardware")
        else validateReceiptDigest(source, line, id, receipt, ascDigest)

  private def validateReceiptDigest(
      source: String,
      line: Int,
      id: String,
      receipt: Option[Edf2AscReceipt],
      ascDigest: Option[Sha256]
  ): Either[EyeLinkCorpusError, Unit] =
    (receipt, ascDigest) match
      case (Some(value), Some(expected)) if value.ascDigest != expected =>
        Left(
          EyeLinkCorpusError.InvalidFixture(
            source,
            line,
            id,
            s"receipt ASC digest=${value.ascDigest.hex} differs from row ASC digest=${expected.hex}"
          )
        )
      case _ => Right(())

  private def validateMetadataClaims(
      source: String,
      line: Int,
      id: String,
      kind: EyeLinkCorpusKind,
      hardware: EyeLinkCorpusHardware,
      rates: Vector[Int],
      eyeLayout: EyeLinkCorpusEyeLayout,
      tracking: EyeLinkCorpusTracking,
      coordinates: Vector[EyeLinkCorpusCoordinates],
      capabilities: Vector[String]
  ): Either[EyeLinkCorpusError, Unit] =
    val requiredClaims = Vector.newBuilder[String]
    rates.foreach(rate => requiredClaims += s"rate-$rate")
    eyeLayout match
      case EyeLinkCorpusEyeLayout.Left        => requiredClaims += "eye-left"
      case EyeLinkCorpusEyeLayout.Right       => requiredClaims += "eye-right"
      case EyeLinkCorpusEyeLayout.Binocular   => requiredClaims += "eye-binocular"
      case EyeLinkCorpusEyeLayout.MixedBlocks => ()
    tracking match
      case EyeLinkCorpusTracking.HeadFixed   => requiredClaims += "tracking-head-fixed"
      case EyeLinkCorpusTracking.Remote      => requiredClaims += "tracking-remote"
      case EyeLinkCorpusTracking.MixedBlocks => ()
    coordinates.foreach {
      case EyeLinkCorpusCoordinates.Gaze => requiredClaims += "coordinates-gaze"
      case EyeLinkCorpusCoordinates.Href => requiredClaims += "coordinates-href"
      case EyeLinkCorpusCoordinates.Raw  => requiredClaims += "coordinates-raw"
    }
    HardwareClaims.get(hardware).foreach(requiredClaims += _)
    val absent = requiredClaims.result().distinct.filterNot(capabilities.contains)
    val syntheticHardwareClaim =
      kind == EyeLinkCorpusKind.Synthetic && capabilities.exists(_.startsWith("hardware-"))
    val syntheticConverterClaim =
      kind == EyeLinkCorpusKind.Synthetic && capabilities.contains("converter-receipt")

    if absent.nonEmpty then
      Left(
        EyeLinkCorpusError.InvalidFixture(
          source,
          line,
          id,
          s"metadata is not reflected by expected capabilities=${absent.mkString(",")}"
        )
      )
    else if syntheticHardwareClaim || syntheticConverterClaim then
      Left(
        EyeLinkCorpusError.InvalidFixture(
          source,
          line,
          id,
          "synthetic fixtures cannot claim hardware or EDF2ASC conversion evidence"
        )
      )
    else Right(())

end EyeLinkCorpusFixture

/** Strength of corpus evidence for one capability or pair of dimensions. */
enum EyeLinkCorpusCoverageStatus derives CanEqual:
  case Missing
  case PlannedOnly
  case SyntheticOnly
  case RealEvidenceAvailable

final class EyeLinkCorpusCapabilityCoverage private[io] (
    val capability: EyeLinkCapability,
    val fixtureIds: Vector[String],
    val status: EyeLinkCorpusCoverageStatus
)

final class EyeLinkCorpusDimensionPair private[io] (
    val first: String,
    val second: String,
    val fixtureIds: Vector[String],
    val status: EyeLinkCorpusCoverageStatus
):
  override def toString: String = s"$first + $second"

/** Computed design coverage. This reports evidence strength; it never upgrades
  * the support contract to `Validated` by itself.
  */
final class EyeLinkCorpusCoverage private[io] (
    val capabilities: Vector[EyeLinkCorpusCapabilityCoverage],
    val dimensionPairs: Vector[EyeLinkCorpusDimensionPair]
):
  val missingRequiredCapabilities: Vector[EyeLinkCapability] =
    capabilities.collect {
      case coverage
          if coverage.capability.isReleaseRequirement &&
            coverage.status == EyeLinkCorpusCoverageStatus.Missing =>
        coverage.capability
    }

  val unvalidatedRequiredCapabilities: Vector[EyeLinkCapability] =
    capabilities.collect {
      case coverage
          if coverage.capability.isReleaseRequirement &&
            coverage.status != EyeLinkCorpusCoverageStatus.RealEvidenceAvailable =>
        coverage.capability
    }

  val unrepresentedDimensionPairs: Vector[EyeLinkCorpusDimensionPair] =
    dimensionPairs.filter(_.status == EyeLinkCorpusCoverageStatus.Missing)

  val unvalidatedDimensionPairs: Vector[EyeLinkCorpusDimensionPair] =
    dimensionPairs.filter(_.status != EyeLinkCorpusCoverageStatus.RealEvidenceAvailable)

/** Versioned, deterministic corpus manifest. */
final class EyeLinkCorpusManifest private (
    val corpusVersion: String,
    val supportContractVersion: String,
    val fixtures: Vector[EyeLinkCorpusFixture]
):
  lazy val coverage: EyeLinkCorpusCoverage = EyeLinkCorpusManifest.coverage(fixtures)

  lazy val publiclyLoadable: Vector[EyeLinkCorpusFixture] =
    fixtures.filter(_.isPubliclyLoadable)

  lazy val containsPrivateBytes: Boolean =
    fixtures.exists(fixture =>
      fixture.kind == EyeLinkCorpusKind.PrivateRealPair && fixture.localPath.nonEmpty
    )

  def renderTsv: String = EyeLinkCorpusManifest.render(this)

object EyeLinkCorpusManifest:
  val schemaVersion: Int = 1

  val columns: Vector[String] = Vector(
    "fixture_id",
    "kind",
    "availability",
    "license",
    "privacy",
    "permission_ref",
    "local_path",
    "hardware",
    "firmware",
    "sampling_rates_hz",
    "eye_layout",
    "tracking_mode",
    "coordinate_modes",
    "converter_name",
    "converter_digest",
    "converter_version",
    "converter_options",
    "converter_platform",
    "converter_recovery",
    "edf_digest",
    "asc_digest",
    "expected_capabilities"
  )

  def parseTsv(
      source: String,
      input: String
  ): Either[NonEmptyVector[EyeLinkCorpusError], EyeLinkCorpusManifest] =
    val lines = input.replace("\r\n", "\n").split("\n", -1).toVector
    lines match
      case preamble +: header +: rows =>
        parsePreamble(source, preamble).flatMap { case (corpusVersion, contractVersion) =>
          if header.split("\t", -1).toVector != columns then
            Left(
              NonEmptyVector.one(
                EyeLinkCorpusError.InvalidHeader(
                  source,
                  2,
                  columns.mkString("\t"),
                  header
                )
              )
            )
          else parseRows(source, corpusVersion, contractVersion, rows)
        }
      case _ =>
        Left(
          NonEmptyVector.one(
            EyeLinkCorpusError.InvalidPreamble(source, input.take(160))
          )
        )

  private def parsePreamble(
      source: String,
      value: String
  ): Either[NonEmptyVector[EyeLinkCorpusError], (String, String)] =
    value.split("\t", -1).toVector match
      case Vector("# eyes4s-eyelink-corpus", schema, corpusVersion, contractVersion)
          if schema == schemaVersion.toString && corpusVersion.trim.nonEmpty &&
            contractVersion == EyeLinkSupport.contractVersion =>
        Right((corpusVersion, contractVersion))
      case _ => Left(NonEmptyVector.one(EyeLinkCorpusError.InvalidPreamble(source, value)))

  private def parseRows(
      source: String,
      corpusVersion: String,
      contractVersion: String,
      rows: Vector[String]
  ): Either[NonEmptyVector[EyeLinkCorpusError], EyeLinkCorpusManifest] =
    val parsed = rows.zipWithIndex.collect {
      case (row, index) if row.nonEmpty && !row.startsWith("#") =>
        parseRow(source, index + 3, row)
    }
    val rowErrors        = parsed.collect { case Left(error) => error }
    val fixtures         = parsed.collect { case Right(fixture) => fixture }
    val structuralErrors = duplicateErrors(source, fixtures)
    NonEmptyVector.fromVector(rowErrors ++ structuralErrors) match
      case Some(errors)             => Left(errors)
      case None if fixtures.isEmpty =>
        Left(NonEmptyVector.one(EyeLinkCorpusError.EmptyManifest(source)))
      case None =>
        Right(new EyeLinkCorpusManifest(corpusVersion, contractVersion, fixtures.sortBy(_.id)))

  private def parseRow(
      source: String,
      line: Int,
      row: String
  ): Either[EyeLinkCorpusError, EyeLinkCorpusFixture] =
    row.split("\t", -1).toVector match
      case Vector(
            idRaw,
            kindRaw,
            availabilityRaw,
            licenseRaw,
            privacyRaw,
            permissionRaw,
            localPathRaw,
            hardwareRaw,
            firmwareRaw,
            ratesRaw,
            eyeRaw,
            trackingRaw,
            coordinatesRaw,
            converterNameRaw,
            converterDigestRaw,
            converterVersionRaw,
            converterOptionsRaw,
            converterPlatformRaw,
            converterRecoveryRaw,
            edfDigestRaw,
            ascDigestRaw,
            capabilitiesRaw
          ) =>
        for
          id           <- decode(source, line, "fixture_id", idRaw)
          kind         <- enumValue(source, line, "kind", kindRaw, EyeLinkCorpusKind.values)
          availability <- enumValue(
            source,
            line,
            "availability",
            availabilityRaw,
            EyeLinkCorpusAvailability.values
          )
          license <- enumValue(
            source,
            line,
            "license",
            licenseRaw,
            EyeLinkCorpusLicense.values
          )
          privacy <- enumValue(
            source,
            line,
            "privacy",
            privacyRaw,
            EyeLinkCorpusPrivacy.values
          )
          permission <- decode(source, line, "permission_ref", permissionRaw)
          localPath  <- optionalDecoded(source, line, "local_path", localPathRaw)
          hardware   <- enumValue(
            source,
            line,
            "hardware",
            hardwareRaw,
            EyeLinkCorpusHardware.values
          )
          firmware   <- optionalDecoded(source, line, "firmware", firmwareRaw)
          rateTokens <- list(source, line, "sampling_rates_hz", ratesRaw)
          rates      <- parseRates(source, line, rateTokens)
          eye        <- enumValue(
            source,
            line,
            "eye_layout",
            eyeRaw,
            EyeLinkCorpusEyeLayout.values
          )
          tracking <- enumValue(
            source,
            line,
            "tracking_mode",
            trackingRaw,
            EyeLinkCorpusTracking.values
          )
          coordinateTokens <- list(source, line, "coordinate_modes", coordinatesRaw)
          coordinates      <- enumValues(
            source,
            line,
            "coordinate_modes",
            coordinateTokens,
            EyeLinkCorpusCoordinates.values
          )
          ascDigest <- optionalDigest(source, line, "asc_digest", ascDigestRaw)
          receipt   <- parseReceipt(
            source,
            line,
            converterNameRaw,
            converterDigestRaw,
            converterVersionRaw,
            converterOptionsRaw,
            converterPlatformRaw,
            converterRecoveryRaw,
            edfDigestRaw,
            ascDigest
          )
          capabilities <- list(source, line, "expected_capabilities", capabilitiesRaw)
          fixture      <- EyeLinkCorpusFixture.create(
            source,
            line,
            id,
            kind,
            availability,
            license,
            privacy,
            permission,
            localPath,
            hardware,
            firmware,
            rates,
            eye,
            tracking,
            coordinates,
            receipt,
            ascDigest,
            capabilities
          )
        yield fixture
      case values =>
        Left(EyeLinkCorpusError.WrongFieldCount(source, line, columns.length, values.length))

  private def parseReceipt(
      source: String,
      line: Int,
      converterNameRaw: String,
      converterDigestRaw: String,
      converterVersionRaw: String,
      converterOptionsRaw: String,
      converterPlatformRaw: String,
      converterRecoveryRaw: String,
      edfDigestRaw: String,
      ascDigest: Option[Sha256]
  ): Either[EyeLinkCorpusError, Option[Edf2AscReceipt]] =
    val rawFields = Vector(
      converterNameRaw,
      converterDigestRaw,
      converterVersionRaw,
      converterOptionsRaw,
      converterPlatformRaw,
      converterRecoveryRaw,
      edfDigestRaw
    )
    if rawFields.forall(_.isEmpty) then Right(None)
    else if converterNameRaw.isEmpty || converterDigestRaw.isEmpty ||
      converterPlatformRaw.isEmpty || converterRecoveryRaw.isEmpty || ascDigest.isEmpty
    then Left(EyeLinkCorpusError.PartialConverterEvidence(source, line))
    else
      ascDigest match
        case None      => Left(EyeLinkCorpusError.PartialConverterEvidence(source, line))
        case Some(asc) =>
          for
            name      <- decode(source, line, "converter_name", converterNameRaw)
            digest    <- requiredDigest(source, line, "converter_digest", converterDigestRaw)
            version   <- optionalDecoded(source, line, "converter_version", converterVersionRaw)
            options   <- listAllowEmpty(source, line, "converter_options", converterOptionsRaw)
            platform  <- decode(source, line, "converter_platform", converterPlatformRaw)
            recovery  <- recoveryValue(source, line, converterRecoveryRaw)
            edfDigest <- optionalDigest(source, line, "edf_digest", edfDigestRaw)
            converter <- Edf2AscConverter
              .of(name, digest, version)
              .left
              .map(error =>
                EyeLinkCorpusError.InvalidConverterEvidence(source, line, error.message)
              )
            arguments <- Edf2AscArguments
              .of(options)
              .left
              .map(error =>
                EyeLinkCorpusError.InvalidConverterEvidence(source, line, error.message)
              )
            receipt <- Edf2AscReceipt
              .declared(edfDigest, asc, converter, arguments, platform, recovery)
              .left
              .map(error =>
                EyeLinkCorpusError.InvalidConverterEvidence(source, line, error.message)
              )
          yield Some(receipt)

  private def duplicateErrors(
      source: String,
      fixtures: Vector[EyeLinkCorpusFixture]
  ): Vector[EyeLinkCorpusError] =
    val duplicateIds = fixtures
      .groupBy(_.id)
      .values
      .filter(_.size > 1)
      .toVector
      .flatMap {
        case first +: remaining =>
          Vector(
            EyeLinkCorpusError.DuplicateFixtureId(
              source,
              first.id,
              (first +: remaining).map(_.manifestLine).sorted
            )
          )
        case _ => Vector.empty
      }
    val duplicatePaths = fixtures
      .flatMap(fixture => fixture.localPath.map(_ -> fixture))
      .groupBy(_._1)
      .values
      .filter(_.size > 1)
      .toVector
      .flatMap {
        case first +: remaining =>
          Vector(
            EyeLinkCorpusError.DuplicateLocalPath(
              source,
              first._1,
              (first +: remaining).map(_._2.id).sorted
            )
          )
        case _ => Vector.empty
      }
    duplicateIds ++ duplicatePaths

  private def coverage(fixtures: Vector[EyeLinkCorpusFixture]): EyeLinkCorpusCoverage =
    val capabilityCoverage = EyeLinkSupport.capabilities.map { capability =>
      val matching = fixtures.filter(_.capabilityIds.contains(capability.id))
      new EyeLinkCorpusCapabilityCoverage(
        capability,
        matching.map(_.id),
        status(matching)
      )
    }
    val dimensions = EyeLinkSupport.required.map(_.dimension).distinct.sorted
    val pairs      = dimensions.zipWithIndex.flatMap { case (first, index) =>
      dimensions.drop(index + 1).map { second =>
        val matching = fixtures.filter { fixture =>
          val fixtureDimensions = fixture.expectedCapabilities.map(_.dimension).toSet
          fixtureDimensions.contains(first) && fixtureDimensions.contains(second)
        }
        new EyeLinkCorpusDimensionPair(first, second, matching.map(_.id), status(matching))
      }
    }
    new EyeLinkCorpusCoverage(capabilityCoverage, pairs)

  private def status(fixtures: Vector[EyeLinkCorpusFixture]): EyeLinkCorpusCoverageStatus =
    if fixtures.exists(_.containsAcquiredRealData) then
      EyeLinkCorpusCoverageStatus.RealEvidenceAvailable
    else if fixtures.exists(fixture =>
        fixture.kind == EyeLinkCorpusKind.Synthetic &&
          fixture.availability == EyeLinkCorpusAvailability.Included
      )
    then EyeLinkCorpusCoverageStatus.SyntheticOnly
    else if fixtures.nonEmpty then EyeLinkCorpusCoverageStatus.PlannedOnly
    else EyeLinkCorpusCoverageStatus.Missing

  private def render(manifest: EyeLinkCorpusManifest): String =
    val preamble = Vector(
      "# eyes4s-eyelink-corpus",
      schemaVersion.toString,
      manifest.corpusVersion,
      manifest.supportContractVersion
    ).mkString("\t")
    val rows = manifest.fixtures.sortBy(_.id).map(renderFixture)
    (preamble +: columns.mkString("\t") +: rows).mkString("\n") + "\n"

  private def renderFixture(fixture: EyeLinkCorpusFixture): String =
    val receipt = fixture.conversionReceipt
    Vector(
      encode(fixture.id),
      fixture.kind.token,
      fixture.availability.token,
      fixture.license.token,
      fixture.privacy.token,
      encode(fixture.permissionReference),
      fixture.localPath.map(encode).getOrElse(""),
      fixture.hardware.token,
      fixture.firmware.map(encode).getOrElse(""),
      renderList(fixture.samplingRatesHz.map(_.toString)),
      fixture.eyeLayout.token,
      fixture.tracking.token,
      renderList(fixture.coordinateModes.map(_.token)),
      receipt.map(value => encode(value.converter.name)).getOrElse(""),
      receipt.map(_.converter.executableDigest.hex).getOrElse(""),
      receipt.flatMap(_.converter.reportedVersion).map(encode).getOrElse(""),
      receipt.map(value => renderList(value.arguments.values)).getOrElse(""),
      receipt.map(value => encode(value.platform)).getOrElse(""),
      receipt.map(value => recoveryToken(value.recovery)).getOrElse(""),
      receipt.flatMap(_.edfDigest).map(_.hex).getOrElse(""),
      fixture.ascDigest.map(_.hex).getOrElse(""),
      renderList(fixture.capabilityIds)
    ).mkString("\t")

  private def enumValue[A](
      source: String,
      line: Int,
      field: String,
      raw: String,
      values: Array[A]
  )(using token: A => String): Either[EyeLinkCorpusError, A] =
    decode(source, line, field, raw).flatMap { decoded =>
      values.find(value => token(value) == decoded) match
        case Some(value) => Right(value)
        case None        =>
          Left(
            EyeLinkCorpusError.InvalidValue(
              source,
              line,
              field,
              decoded,
              values.map(token).mkString(",")
            )
          )
    }

  private given Conversion[EyeLinkCorpusKind, String]         = _.token
  private given Conversion[EyeLinkCorpusAvailability, String] = _.token
  private given Conversion[EyeLinkCorpusLicense, String]      = _.token
  private given Conversion[EyeLinkCorpusPrivacy, String]      = _.token
  private given Conversion[EyeLinkCorpusHardware, String]     = _.token
  private given Conversion[EyeLinkCorpusEyeLayout, String]    = _.token
  private given Conversion[EyeLinkCorpusTracking, String]     = _.token
  private given Conversion[EyeLinkCorpusCoordinates, String]  = _.token

  private def enumValues[A](
      source: String,
      line: Int,
      field: String,
      tokens: Vector[String],
      values: Array[A]
  )(using token: A => String): Either[EyeLinkCorpusError, Vector[A]] =
    tokens.foldLeft(Right(Vector.empty): Either[EyeLinkCorpusError, Vector[A]]) {
      case (accumulated, value) =>
        for
          current <- accumulated
          parsed  <- enumValue(source, line, field, encode(value), values)
        yield current :+ parsed
    }

  private def recoveryValue(
      source: String,
      line: Int,
      raw: String
  ): Either[EyeLinkCorpusError, Edf2AscRecovery] =
    decode(source, line, "converter_recovery", raw).flatMap {
      case "normal"   => Right(Edf2AscRecovery.Normal)
      case "failsafe" => Right(Edf2AscRecovery.Failsafe)
      case value      =>
        Left(
          EyeLinkCorpusError.InvalidValue(
            source,
            line,
            "converter_recovery",
            value,
            "normal,failsafe"
          )
        )
    }

  private def recoveryToken(value: Edf2AscRecovery): String = value match
    case Edf2AscRecovery.Normal   => "normal"
    case Edf2AscRecovery.Failsafe => "failsafe"

  private def parseRates(
      source: String,
      line: Int,
      values: Vector[String]
  ): Either[EyeLinkCorpusError, Vector[Int]] =
    values.foldLeft(Right(Vector.empty): Either[EyeLinkCorpusError, Vector[Int]]) {
      case (accumulated, value) =>
        for
          current <- accumulated
          parsed  <- value.toIntOption.toRight(
            EyeLinkCorpusError.InvalidValue(source, line, "sampling_rates_hz", value, "integer")
          )
        yield current :+ parsed
    }

  private def requiredDigest(
      source: String,
      line: Int,
      field: String,
      raw: String
  ): Either[EyeLinkCorpusError, Sha256] =
    decode(source, line, field, raw).flatMap(value =>
      Sha256
        .fromHex(s"$source:$line:$field", value)
        .left
        .map(error => EyeLinkCorpusError.InvalidDigest(source, line, field, error.message))
    )

  private def optionalDigest(
      source: String,
      line: Int,
      field: String,
      raw: String
  ): Either[EyeLinkCorpusError, Option[Sha256]] =
    if raw.isEmpty then Right(None)
    else requiredDigest(source, line, field, raw).map(Some(_))

  private def optionalDecoded(
      source: String,
      line: Int,
      field: String,
      raw: String
  ): Either[EyeLinkCorpusError, Option[String]] =
    if raw.isEmpty then Right(None) else decode(source, line, field, raw).map(Some(_))

  private def list(
      source: String,
      line: Int,
      field: String,
      raw: String
  ): Either[EyeLinkCorpusError, Vector[String]] =
    if raw.isEmpty then
      Left(EyeLinkCorpusError.InvalidValue(source, line, field, raw, "nonempty list"))
    else listAllowEmpty(source, line, field, raw)

  private def listAllowEmpty(
      source: String,
      line: Int,
      field: String,
      raw: String
  ): Either[EyeLinkCorpusError, Vector[String]] =
    if raw.isEmpty then Right(Vector.empty)
    else
      raw
        .split(",", -1)
        .toVector
        .foldLeft(
          Right(Vector.empty): Either[EyeLinkCorpusError, Vector[String]]
        ) { case (accumulated, token) =>
          for
            current <- accumulated
            value   <- decode(source, line, field, token)
            _       <- Either.cond(
              value.nonEmpty,
              (),
              EyeLinkCorpusError.InvalidValue(source, line, field, token, "nonblank list item")
            )
          yield current :+ value
        }

  private def decode(
      source: String,
      line: Int,
      field: String,
      raw: String
  ): Either[EyeLinkCorpusError, String] =
    val output                            = new java.lang.StringBuilder(raw.length)
    var index                             = 0
    var error: Option[EyeLinkCorpusError] = None
    while index < raw.length && error.isEmpty do
      if raw.charAt(index) != '%' then
        output.append(raw.charAt(index))
        index += 1
      else if index + 2 >= raw.length then
        error = Some(EyeLinkCorpusError.InvalidEscape(source, line, field, index, raw))
      else
        raw.substring(index, index + 3).toUpperCase match
          case "%09" => output.append('\t'); index += 3
          case "%0A" => output.append('\n'); index += 3
          case "%0D" => output.append('\r'); index += 3
          case "%25" => output.append('%'); index += 3
          case "%2C" => output.append(','); index += 3
          case _     =>
            error = Some(EyeLinkCorpusError.InvalidEscape(source, line, field, index, raw))
    error.toLeft(output.toString)

  private def encode(value: String): String =
    val output = new java.lang.StringBuilder(value.length)
    var index  = 0
    while index < value.length do
      value.charAt(index) match
        case '\t' => output.append("%09")
        case '\n' => output.append("%0A")
        case '\r' => output.append("%0D")
        case '%'  => output.append("%25")
        case ','  => output.append("%2C")
        case char => output.append(char)
      index += 1
    output.toString

  private def renderList(values: Vector[String]): String = values.map(encode).mkString(",")

end EyeLinkCorpusManifest

enum EyeLinkCorpusError derives CanEqual:
  case InvalidPreamble(source: String, actual: String)
  case InvalidHeader(source: String, line: Int, expected: String, actual: String)
  case WrongFieldCount(source: String, line: Int, expected: Int, actual: Int)
  case InvalidEscape(source: String, line: Int, field: String, index: Int, value: String)
  case InvalidValue(source: String, line: Int, field: String, value: String, expected: String)
  case InvalidDigest(source: String, line: Int, field: String, detail: String)
  case PartialConverterEvidence(source: String, line: Int)
  case InvalidConverterEvidence(source: String, line: Int, detail: String)
  case InvalidFixture(source: String, line: Int, fixtureId: String, detail: String)
  case UnsafeLocalPath(source: String, line: Int, fixtureId: String, path: String)
  case DuplicateFixtureId(source: String, fixtureId: String, lines: Vector[Int])
  case DuplicateLocalPath(source: String, path: String, fixtureIds: Vector[String])
  case EmptyManifest(source: String)

  def message: String = this match
    case InvalidPreamble(source, actual) =>
      s"EyeLink corpus source='$source' has invalid preamble='$actual'."
    case InvalidHeader(source, line, expected, actual) =>
      s"EyeLink corpus source='$source' line=$line has header='$actual'; expected='$expected'."
    case WrongFieldCount(source, line, expected, actual) =>
      s"EyeLink corpus source='$source' line=$line has fields=$actual; expected=$expected."
    case InvalidEscape(source, line, field, index, value) =>
      s"EyeLink corpus source='$source' line=$line field='$field' has invalid escape at index=$index in value='$value'."
    case InvalidValue(source, line, field, value, expected) =>
      s"EyeLink corpus source='$source' line=$line field='$field' value='$value' is invalid; expected=$expected."
    case InvalidDigest(source, line, field, detail) =>
      s"EyeLink corpus source='$source' line=$line field='$field' has invalid digest: $detail"
    case PartialConverterEvidence(source, line) =>
      s"EyeLink corpus source='$source' line=$line has partial EDF2ASC evidence; name, executable digest, platform, recovery, and ASC digest are required together."
    case InvalidConverterEvidence(source, line, detail) =>
      s"EyeLink corpus source='$source' line=$line has invalid EDF2ASC evidence: $detail"
    case InvalidFixture(source, line, fixtureId, detail) =>
      s"EyeLink corpus source='$source' line=$line fixture='$fixtureId' is invalid: $detail."
    case UnsafeLocalPath(source, line, fixtureId, path) =>
      s"EyeLink corpus source='$source' line=$line fixture='$fixtureId' local path='$path' must be a safe relative POSIX path."
    case DuplicateFixtureId(source, fixtureId, lines) =>
      s"EyeLink corpus source='$source' repeats fixture='$fixtureId' at lines=${lines.mkString(",")}."
    case DuplicateLocalPath(source, path, fixtureIds) =>
      s"EyeLink corpus source='$source' reuses local path='$path' for fixtures=${fixtureIds.mkString(",")}."
    case EmptyManifest(source) =>
      s"EyeLink corpus source='$source' contains no fixture rows."

end EyeLinkCorpusError
