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

package eyes4s.detect

import eyes4s.core.*
import eyes4s.kernel.*

/** Stable identity of a named scientific algorithm. */
opaque type AlgorithmId = String

object AlgorithmId:
  def from(value: String): Either[AlgorithmMetadataError, AlgorithmId] =
    if value.trim.nonEmpty then Right(value)
    else Left(AlgorithmMetadataError.EmptyAlgorithmId(value))

  private[detect] def literal(value: String): AlgorithmId = value

  extension (id: AlgorithmId) def value: String = id

/** A DOI retained in canonical bare form, without a resolver prefix. */
opaque type Doi = String

object Doi:
  def from(value: String): Either[AlgorithmMetadataError, Doi] =
    val canonical = value.trim
    if canonical.startsWith("10.") && canonical.contains("/") then Right(canonical)
    else Left(AlgorithmMetadataError.InvalidDoi(value))

  private[detect] def literal(value: String): Doi = value

  extension (doi: Doi)
    def value: String = doi
    def url: String   = s"https://doi.org/$doi"

/** Validated semantic version for one implementation identity. */
final class AlgorithmVersion private (
    val major: Int,
    val minor: Int,
    val patch: Int
) derives CanEqual:
  def render: String = s"$major.$minor.$patch"

  override def equals(other: Any): Boolean = other match
    case version: AlgorithmVersion =>
      major == version.major && minor == version.minor && patch == version.patch
    case _ => false

  override def hashCode: Int = (major * 31 + minor) * 31 + patch

object AlgorithmVersion:
  def of(
      major: Int,
      minor: Int,
      patch: Int
  ): Either[AlgorithmMetadataError, AlgorithmVersion] =
    if major >= 0 && minor >= 0 && patch >= 0 then
      Right(new AlgorithmVersion(major, minor, patch))
    else Left(AlgorithmMetadataError.InvalidVersion(major, minor, patch))

  private[detect] def literal(major: Int, minor: Int, patch: Int): AlgorithmVersion =
    new AlgorithmVersion(major, minor, patch)

/** Primary publication defining or identifying an algorithm. */
final class Citation private (
    val authors: Vector[String],
    val year: Int,
    val title: String,
    val doi: Doi
) derives CanEqual:
  override def equals(other: Any): Boolean = other match
    case citation: Citation =>
      authors == citation.authors && year == citation.year && title == citation.title &&
      doi == citation.doi
    case _ => false

  override def hashCode: Int =
    ((authors.hashCode * 31 + year) * 31 + title.hashCode) * 31 + doi.hashCode

object Citation:
  def of(
      authors: Vector[String],
      year: Int,
      title: String,
      doi: Doi
  ): Either[AlgorithmMetadataError, Citation] =
    if authors.isEmpty || authors.exists(_.trim.isEmpty) then
      Left(AlgorithmMetadataError.InvalidAuthors(authors))
    else if year <= 0 then Left(AlgorithmMetadataError.InvalidYear(year))
    else if title.trim.isEmpty then Left(AlgorithmMetadataError.EmptyCitationTitle(title))
    else Right(new Citation(authors, year, title, doi))

  private[detect] def literal(
      authors: Vector[String],
      year: Int,
      title: String,
      doi: Doi
  ): Citation = new Citation(authors, year, title, doi)

/** Whether detection can proceed incrementally before the input ends. */
enum DetectorExecution derives CanEqual:
  case Streaming
  case StreamingAfterWholeRecordingPreparation

/** A precondition under which a detector's scientific identity is valid. */
enum DetectorAssumption derives CanEqual:
  case StrictlyIncreasingTimestamps
  case RegularSampling
  case VisualAngleCoordinates
  case PhysicalVelocityThreshold
  case FixedThresholdsSupplied
  case MinimumDurationSupplied
  case InvalidObservationsBreakCandidates
  case MonocularSignal

/** A deliberate implementation choice relative to a designated publication
  * or source implementation.
  */
enum DetectorDeviation derives CanEqual:
  case SymmetricThreeSampleVelocity
  case EndpointClassInheritance
  case ShortRunsRemainUnclassified
  case HalfOpenEventIntervals
  case PerAxisBoundingBoxThreshold
  case DiagonalDispersionSummary
  case LinearTimeExtremaQueue
  case ViolatingSampleStartsNextCandidate
  case PublishedFivePointVelocity
  case ThresholdEstimationIsSeparate
  case PublicationMedianSquareThreshold
  case ToolboxBoundaryVelocitiesOmitted
  case InclusiveCandidatePeakVelocity
  case BinocularOverlapNotImplemented
  case SourceSupportIsRetained

/** External evidence against which an implementation is checked. */
enum DesignatedReference derives CanEqual:
  case Publication(citation: Citation)
  case SourceImplementation(
      repository: String,
      revision: String,
      path: String,
      license: String
  )

/** Versioned scientific identity attached to every paper-named detector. */
final class AlgorithmCard private (
    val id: AlgorithmId,
    val version: AlgorithmVersion,
    val name: String,
    val citations: Vector[Citation],
    val assumptions: Vector[DetectorAssumption],
    val deviations: Vector[DetectorDeviation],
    val execution: DetectorExecution,
    val references: Vector[DesignatedReference]
) derives CanEqual:
  def detectorRef: DetectorRef = DetectorRef(id.value, version.render)

  override def equals(other: Any): Boolean = other match
    case card: AlgorithmCard =>
      id == card.id && version == card.version && name == card.name &&
      citations == card.citations && assumptions == card.assumptions &&
      deviations == card.deviations && execution == card.execution &&
      references == card.references
    case _ => false

  override def hashCode: Int =
    Vector(
      id.hashCode,
      version.hashCode,
      name.hashCode,
      citations.hashCode,
      assumptions.hashCode,
      deviations.hashCode,
      execution.hashCode,
      references.hashCode
    ).hashCode

object AlgorithmCard:
  def of(
      id: AlgorithmId,
      version: AlgorithmVersion,
      name: String,
      citations: Vector[Citation],
      assumptions: Vector[DetectorAssumption],
      deviations: Vector[DetectorDeviation],
      execution: DetectorExecution,
      references: Vector[DesignatedReference]
  ): Either[AlgorithmMetadataError, AlgorithmCard] =
    if name.trim.isEmpty then Left(AlgorithmMetadataError.EmptyAlgorithmName(name))
    else if citations.isEmpty then Left(AlgorithmMetadataError.NoCitations(id))
    else if assumptions.isEmpty then Left(AlgorithmMetadataError.NoAssumptions(id))
    else if references.isEmpty then Left(AlgorithmMetadataError.NoReferences(id))
    else
      Right(
        new AlgorithmCard(
          id,
          version,
          name,
          citations,
          assumptions,
          deviations,
          execution,
          references
        )
      )

  private[detect] def literal(
      id: AlgorithmId,
      version: AlgorithmVersion,
      name: String,
      citations: Vector[Citation],
      assumptions: Vector[DetectorAssumption],
      deviations: Vector[DetectorDeviation],
      execution: DetectorExecution,
      references: Vector[DesignatedReference]
  ): AlgorithmCard =
    new AlgorithmCard(
      id,
      version,
      name,
      citations,
      assumptions,
      deviations,
      execution,
      references
    )

/** A runnable event machine that cannot be separated from its algorithm card. */
final class EventDetector[U <: Unit2D] private[detect] (
    val card: AlgorithmCard,
    val machine: Machine[Sample[U], DetectionEmission[U]],
    val configuration: Vector[(String, Provenance.Param)]
):
  def runAll(input: Iterable[Sample[U]]): Vector[DetectionEmission[U]] =
    machine.runAll(input)

/** Canonical cards for the detector implementations shipped by eyes4s. */
object AlgorithmCards:
  private val pymovementsRepository = "https://github.com/aeye-lab/pymovements"
  private val pymovementsRevision   = "6753fdf8b81da40b890576dd6b369edb81243b06"

  private val salvucciGoldberg = Citation.literal(
    Vector("Dario D. Salvucci", "Joseph H. Goldberg"),
    2000,
    "Identifying fixations and saccades in eye-tracking protocols",
    Doi.literal("10.1145/355017.355028")
  )

  private val engbertKlieglCitation = Citation.literal(
    Vector("Ralf Engbert", "Reinhold Kliegl"),
    2003,
    "Microsaccades uncover the orientation of covert attention",
    Doi.literal("10.1016/S0042-6989(03)00084-1")
  )

  val ivt: AlgorithmCard = AlgorithmCard.literal(
    AlgorithmId.literal("eyes4s.detect.ivt"),
    AlgorithmVersion.literal(1, 0, 0),
    "Velocity-threshold identification",
    Vector(salvucciGoldberg),
    Vector(
      DetectorAssumption.StrictlyIncreasingTimestamps,
      DetectorAssumption.VisualAngleCoordinates,
      DetectorAssumption.PhysicalVelocityThreshold,
      DetectorAssumption.MinimumDurationSupplied,
      DetectorAssumption.InvalidObservationsBreakCandidates,
      DetectorAssumption.MonocularSignal
    ),
    Vector(
      DetectorDeviation.SymmetricThreeSampleVelocity,
      DetectorDeviation.EndpointClassInheritance,
      DetectorDeviation.ShortRunsRemainUnclassified,
      DetectorDeviation.HalfOpenEventIntervals,
      DetectorDeviation.SourceSupportIsRetained
    ),
    DetectorExecution.Streaming,
    Vector(
      DesignatedReference.Publication(salvucciGoldberg),
      DesignatedReference.SourceImplementation(
        pymovementsRepository,
        pymovementsRevision,
        "src/pymovements/events/detection/ivt.py",
        "MIT"
      )
    )
  )

  val idt: AlgorithmCard = AlgorithmCard.literal(
    AlgorithmId.literal("eyes4s.detect.idt"),
    AlgorithmVersion.literal(1, 0, 0),
    "Dispersion-threshold identification",
    Vector(salvucciGoldberg),
    Vector(
      DetectorAssumption.StrictlyIncreasingTimestamps,
      DetectorAssumption.MinimumDurationSupplied,
      DetectorAssumption.InvalidObservationsBreakCandidates,
      DetectorAssumption.MonocularSignal
    ),
    Vector(
      DetectorDeviation.PerAxisBoundingBoxThreshold,
      DetectorDeviation.DiagonalDispersionSummary,
      DetectorDeviation.LinearTimeExtremaQueue,
      DetectorDeviation.ViolatingSampleStartsNextCandidate,
      DetectorDeviation.ShortRunsRemainUnclassified,
      DetectorDeviation.HalfOpenEventIntervals,
      DetectorDeviation.SourceSupportIsRetained
    ),
    DetectorExecution.Streaming,
    Vector(
      DesignatedReference.Publication(salvucciGoldberg),
      DesignatedReference.SourceImplementation(
        pymovementsRepository,
        pymovementsRevision,
        "src/pymovements/events/detection/idt.py",
        "MIT"
      )
    )
  )

  val engbertKliegl: AlgorithmCard = AlgorithmCard.literal(
    AlgorithmId.literal("eyes4s.detect.engbert-kliegl"),
    AlgorithmVersion.literal(1, 0, 0),
    "Engbert-Kliegl microsaccade detection",
    Vector(engbertKlieglCitation),
    Vector(
      DetectorAssumption.StrictlyIncreasingTimestamps,
      DetectorAssumption.RegularSampling,
      DetectorAssumption.VisualAngleCoordinates,
      DetectorAssumption.FixedThresholdsSupplied,
      DetectorAssumption.InvalidObservationsBreakCandidates,
      DetectorAssumption.MonocularSignal
    ),
    Vector(
      DetectorDeviation.PublishedFivePointVelocity,
      DetectorDeviation.ThresholdEstimationIsSeparate,
      DetectorDeviation.PublicationMedianSquareThreshold,
      DetectorDeviation.ToolboxBoundaryVelocitiesOmitted,
      DetectorDeviation.InclusiveCandidatePeakVelocity,
      DetectorDeviation.BinocularOverlapNotImplemented,
      DetectorDeviation.HalfOpenEventIntervals,
      DetectorDeviation.SourceSupportIsRetained
    ),
    DetectorExecution.StreamingAfterWholeRecordingPreparation,
    Vector(
      DesignatedReference.Publication(engbertKlieglCitation),
      DesignatedReference.SourceImplementation(
        "https://github.com/lschwetlick/EngbertMicrosaccadeToolbox",
        "a3eba6e9f1464c953c81fbd87944ba7678c2cf64",
        "EngbertMicrosaccadeToolbox/microsac_detection.py",
        "GPL-3.0"
      )
    )
  )

  val all: Vector[AlgorithmCard] = Vector(ivt, idt, engbertKliegl)

/** A metadata constructor rejected an invalid local invariant. */
enum AlgorithmMetadataError derives CanEqual:
  case EmptyAlgorithmId(value: String)
  case InvalidDoi(value: String)
  case InvalidVersion(major: Int, minor: Int, patch: Int)
  case InvalidAuthors(authors: Vector[String])
  case InvalidYear(year: Int)
  case EmptyCitationTitle(value: String)
  case EmptyAlgorithmName(value: String)
  case NoCitations(id: AlgorithmId)
  case NoAssumptions(id: AlgorithmId)
  case NoReferences(id: AlgorithmId)

  def message: String = this match
    case EmptyAlgorithmId(value) =>
      s"Algorithm identity must be non-empty, got id='$value'."
    case InvalidDoi(value) =>
      s"A DOI must use canonical bare form beginning with '10.' and contain '/', got doi='$value'."
    case InvalidVersion(major, minor, patch) =>
      s"Algorithm version components must be non-negative, got $major.$minor.$patch."
    case InvalidAuthors(authors) =>
      s"A citation needs one or more non-empty author names, got authors=${authors.mkString("[", ", ", "]")}."
    case InvalidYear(year) =>
      s"A citation year must be positive, got year=$year."
    case EmptyCitationTitle(value) =>
      s"A citation title must be non-empty, got title='$value'."
    case EmptyAlgorithmName(value) =>
      s"An algorithm name must be non-empty, got name='$value'."
    case NoCitations(id) =>
      s"Algorithm '${id.value}' must name at least one primary citation."
    case NoAssumptions(id) =>
      s"Algorithm '${id.value}' must name at least one input assumption."
    case NoReferences(id) =>
      s"Algorithm '${id.value}' must name at least one designated reference."

end AlgorithmMetadataError
