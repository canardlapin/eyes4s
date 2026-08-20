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

/** Whether the converter ran in its ordinary or corruption-recovery mode. */
enum Edf2AscRecovery derives CanEqual:
  case Normal
  case Failsafe

/** How eyes4s learned that conversion completed.
  *
  * A declaration is not relabelled as an observed subprocess execution. Both
  * can carry complete scientific evidence, but downstream reports retain the
  * distinction.
  */
enum ConversionAuthority derives CanEqual:
  case ObservedProcess
  case UserDeclared

/** Named policy for deciding whether conversion evidence is sufficient. */
enum ConversionEvidencePolicy derives CanEqual:
  case Release
  case Exploratory

/** Validated identity of the converter executable used to produce ASC. */
final class Edf2AscConverter private (
    val name: String,
    val executableDigest: Sha256,
    val reportedVersion: Option[String]
):
  override def toString: String =
    s"$name@${reportedVersion.getOrElse("unreported")}:${executableDigest.hex}"

object Edf2AscConverter:
  def of(
      name: String,
      executableDigest: Sha256,
      reportedVersion: Option[String]
  ): Either[Edf2AscProvenanceError, Edf2AscConverter] =
    val normalizedName    = name.trim
    val normalizedVersion = reportedVersion.map(_.trim)
    if normalizedName.isEmpty then Left(Edf2AscProvenanceError.BlankConverterName(name))
    else
      normalizedVersion match
        case Some(value) if value.isEmpty =>
          Left(Edf2AscProvenanceError.BlankConverterVersion(normalizedName))
        case _ =>
          Right(new Edf2AscConverter(normalizedName, executableDigest, normalizedVersion))

/** Exact scientific option vector, excluding input and output display paths.
  *
  * File identity comes from digests. Keeping paths out of the canonical
  * encoding makes relocation scientifically irrelevant while retaining every
  * option that can change EDF2ASC output.
  */
final class Edf2AscArguments private (val values: Vector[String]):
  def canonical: String = values.map(value => s"${value.length}:$value").mkString

object Edf2AscArguments:
  def of(values: Vector[String]): Either[Edf2AscProvenanceError, Edf2AscArguments] =
    values.zipWithIndex
      .collectFirst {
        case (value, index) if value.trim.isEmpty =>
          Edf2AscProvenanceError.BlankConverterArgument(index)
        case (value, index)
            if value.exists(character =>
              character == '\u0000' || character == '\n' || character == '\r'
            ) =>
          Edf2AscProvenanceError.InvalidConverterArgument(index, value)
      }
      .toLeft(new Edf2AscArguments(values))

/** Complete evidence for one successful conversion. */
final class Edf2AscReceipt private (
    val edfDigest: Option[Sha256],
    val ascDigest: Sha256,
    val converter: Edf2AscConverter,
    val arguments: Edf2AscArguments,
    val platform: String,
    val recovery: Edf2AscRecovery,
    val authority: ConversionAuthority
):
  lazy val canonicalDigest: Sha256 = Sha256.ofUtf8(canonicalEncoding)

  private def canonicalEncoding: String =
    val fields = Vector(
      "eyes4s.edf2asc.receipt.v1",
      edfDigest.map(_.hex).getOrElse("absent"),
      ascDigest.hex,
      converter.name,
      converter.executableDigest.hex,
      converter.reportedVersion.getOrElse("unreported"),
      arguments.canonical,
      platform,
      recovery.toString,
      authority.toString
    )
    fields.map(value => s"${value.length}:$value").mkString

object Edf2AscReceipt:
  def observed(
      edfDigest: Option[Sha256],
      ascDigest: Sha256,
      converter: Edf2AscConverter,
      arguments: Edf2AscArguments,
      platform: String,
      exitCode: Int,
      recovery: Edf2AscRecovery
  ): Either[Edf2AscProvenanceError, Edf2AscReceipt] =
    if exitCode != 0 then
      Left(Edf2AscProvenanceError.UnsuccessfulConversion(exitCode, ascDigest))
    else
      create(
        edfDigest,
        ascDigest,
        converter,
        arguments,
        platform,
        recovery,
        ConversionAuthority.ObservedProcess
      )

  /** Record a conversion performed outside eyes4s without pretending that the
    * library observed the process.
    */
  def declared(
      edfDigest: Option[Sha256],
      ascDigest: Sha256,
      converter: Edf2AscConverter,
      arguments: Edf2AscArguments,
      platform: String,
      recovery: Edf2AscRecovery
  ): Either[Edf2AscProvenanceError, Edf2AscReceipt] =
    create(
      edfDigest,
      ascDigest,
      converter,
      arguments,
      platform,
      recovery,
      ConversionAuthority.UserDeclared
    )

  private def create(
      edfDigest: Option[Sha256],
      ascDigest: Sha256,
      converter: Edf2AscConverter,
      arguments: Edf2AscArguments,
      platform: String,
      recovery: Edf2AscRecovery,
      authority: ConversionAuthority
  ): Either[Edf2AscProvenanceError, Edf2AscReceipt] =
    val normalizedPlatform = platform.trim
    if normalizedPlatform.isEmpty then Left(Edf2AscProvenanceError.BlankPlatform(platform))
    else
      Right(
        new Edf2AscReceipt(
          edfDigest,
          ascDigest,
          converter,
          arguments,
          normalizedPlatform,
          recovery,
          authority
        )
      )

/** Source identity even when the original conversion receipt is unavailable. */
enum EyeLinkAscOrigin derives CanEqual:
  case Converted(receipt: Edf2AscReceipt)
  case Unidentified(digest: Sha256)

  def ascDigest: Sha256 = this match
    case Converted(receipt)   => receipt.ascDigest
    case Unidentified(digest) => digest

  def assess(
      policy: ConversionEvidencePolicy
  ): Either[Edf2AscProvenanceError, ConversionEvidenceReport] =
    this match
      case Converted(receipt) =>
        if policy == ConversionEvidencePolicy.Release && receipt.edfDigest.isEmpty then
          Left(Edf2AscProvenanceError.MissingEdfDigest(receipt.ascDigest))
        else
          val warnings = Vector.newBuilder[ConversionEvidenceWarning]
          if receipt.authority == ConversionAuthority.UserDeclared then
            warnings += ConversionEvidenceWarning.UserDeclaredConversion(receipt.ascDigest)
          if receipt.edfDigest.isEmpty then
            warnings += ConversionEvidenceWarning.MissingEdfDigest(receipt.ascDigest)
          if receipt.recovery == Edf2AscRecovery.Failsafe then
            warnings += ConversionEvidenceWarning.FailsafeRecovery(receipt.ascDigest)
          Right(new ConversionEvidenceReport(this, warnings.result(), receipt.canonicalDigest))
      case Unidentified(digest) =>
        policy match
          case ConversionEvidencePolicy.Release =>
            Left(Edf2AscProvenanceError.MissingConversionReceipt(digest))
          case ConversionEvidencePolicy.Exploratory =>
            Right(
              new ConversionEvidenceReport(
                this,
                Vector(ConversionEvidenceWarning.UnidentifiedConversion(digest)),
                Sha256.ofUtf8(s"eyes4s.edf2asc.unidentified.v1:${digest.hex}")
              )
            )

/** Evidence accepted under one named policy, with every weakening retained. */
final class ConversionEvidenceReport private[io] (
    val origin: EyeLinkAscOrigin,
    val warnings: Vector[ConversionEvidenceWarning],
    val canonicalDigest: Sha256
)

enum ConversionEvidenceWarning derives CanEqual:
  case UserDeclaredConversion(ascDigest: Sha256)
  case MissingEdfDigest(ascDigest: Sha256)
  case FailsafeRecovery(ascDigest: Sha256)
  case UnidentifiedConversion(ascDigest: Sha256)

  def message: String = this match
    case UserDeclaredConversion(digest) =>
      s"ASC digest=${digest.hex} has user-declared conversion evidence; eyes4s did not observe the process."
    case MissingEdfDigest(digest) =>
      s"ASC digest=${digest.hex} has no corresponding EDF digest."
    case FailsafeRecovery(digest) =>
      s"ASC digest=${digest.hex} was produced using EDF2ASC failsafe recovery."
    case UnidentifiedConversion(digest) =>
      s"ASC digest=${digest.hex} has no identified EDF2ASC conversion receipt."

end ConversionEvidenceWarning

enum Edf2AscProvenanceError derives CanEqual:
  case BlankConverterName(value: String)
  case BlankConverterVersion(converter: String)
  case BlankConverterArgument(index: Int)
  case InvalidConverterArgument(index: Int, value: String)
  case BlankPlatform(value: String)
  case UnsuccessfulConversion(exitCode: Int, ascDigest: Sha256)
  case MissingEdfDigest(ascDigest: Sha256)
  case MissingConversionReceipt(ascDigest: Sha256)

  def message: String = this match
    case BlankConverterName(value) =>
      s"EDF2ASC converter name='$value' is blank."
    case BlankConverterVersion(converter) =>
      s"EDF2ASC converter='$converter' has a blank reported version."
    case BlankConverterArgument(index) =>
      s"EDF2ASC scientific argument[$index] is blank."
    case InvalidConverterArgument(index, value) =>
      s"EDF2ASC scientific argument[$index]='$value' contains a NUL or line break."
    case BlankPlatform(value) =>
      s"EDF2ASC platform='$value' is blank."
    case UnsuccessfulConversion(exitCode, digest) =>
      s"EDF2ASC conversion for ASC digest=${digest.hex} exited with code=$exitCode."
    case MissingEdfDigest(digest) =>
      s"Release conversion evidence for ASC digest=${digest.hex} requires the source EDF digest."
    case MissingConversionReceipt(digest) =>
      s"Release conversion evidence for ASC digest=${digest.hex} requires an identified EDF2ASC receipt."

end Edf2AscProvenanceError
