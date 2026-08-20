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

/** What the EyeLink release contract promises to do with one capability.
  *
  * This is deliberately separate from validation state. A capability can be a
  * release requirement while still awaiting its conformance fixture; it is not
  * advertised as supported until its evidence is [[EyeLinkEvidence.Validated]].
  */
enum EyeLinkDisposition derives CanEqual:
  case RequiredForRelease
  case PreserveWithoutInterpretation
  case RejectExplicitly

/** Current evidence for one row of the EyeLink support matrix. */
enum EyeLinkEvidence derives CanEqual:
  case Planned
  case Validated
  case NotRequired

/** One immutable row in the executable EyeLink support matrix.
  *
  * Construction is private so a row cannot claim planned or validated support
  * without at least one evidence reference. The public matrix is the product
  * contract; third-party values are not accepted as proof of eyes4s support.
  */
final class EyeLinkCapability private (
    val id: String,
    val dimension: String,
    val value: String,
    val disposition: EyeLinkDisposition,
    val evidence: EyeLinkEvidence,
    val evidenceRefs: Vector[String],
    val rationale: String
):
  def isReleaseRequirement: Boolean = disposition == EyeLinkDisposition.RequiredForRelease

  def isValidated: Boolean = evidence == EyeLinkEvidence.Validated

  def isAdvertisable: Boolean = isReleaseRequirement && isValidated

  override def toString: String =
    s"EyeLinkCapability($id,$disposition,$evidence)"

object EyeLinkCapability:
  private[io] def planned(
      id: String,
      dimension: String,
      value: String,
      fixture: String,
      rationale: String
  ): EyeLinkCapability =
    new EyeLinkCapability(
      id,
      dimension,
      value,
      EyeLinkDisposition.RequiredForRelease,
      EyeLinkEvidence.Planned,
      Vector(fixture),
      rationale
    )

  private[io] def preserve(
      id: String,
      dimension: String,
      value: String,
      reference: String,
      rationale: String
  ): EyeLinkCapability =
    new EyeLinkCapability(
      id,
      dimension,
      value,
      EyeLinkDisposition.PreserveWithoutInterpretation,
      EyeLinkEvidence.NotRequired,
      Vector(reference),
      rationale
    )

  private[io] def reject(
      id: String,
      dimension: String,
      value: String,
      reference: String,
      rationale: String
  ): EyeLinkCapability =
    new EyeLinkCapability(
      id,
      dimension,
      value,
      EyeLinkDisposition.RejectExplicitly,
      EyeLinkEvidence.NotRequired,
      Vector(reference),
      rationale
    )

/** The versioned support target for the first EyeLink release.
  *
  * A row marked `RequiredForRelease` is a test obligation, not a current
  * support claim. [[releaseReady]] remains false until a generated scientific
  * validation artifact replaces every planned obligation with validated
  * evidence. This prevents documentation from outrunning conformance.
  */
object EyeLinkSupport:
  val contractVersion: String = "0.6-draft.1"

  private def planned(
      id: String,
      dimension: String,
      value: String,
      fixture: String,
      rationale: String
  ): EyeLinkCapability =
    EyeLinkCapability.planned(id, dimension, value, fixture, rationale)

  private def preserve(
      id: String,
      dimension: String,
      value: String,
      reference: String,
      rationale: String
  ): EyeLinkCapability =
    EyeLinkCapability.preserve(id, dimension, value, reference, rationale)

  private def reject(
      id: String,
      dimension: String,
      value: String,
      reference: String,
      rationale: String
  ): EyeLinkCapability =
    EyeLinkCapability.reject(id, dimension, value, reference, rationale)

  val capabilities: Vector[EyeLinkCapability] = Vector(
    reject(
      "source-edf",
      "source format",
      "binary EDF",
      "SR-ASC-GUIDE",
      "EDF is vendor-controlled binary data; the portable API requires EDF2ASC output."
    ),
    planned(
      "source-asc",
      "source format",
      "EDF2ASC text",
      "asc-basic-gaze",
      "ASC is the portable EyeLink ingestion boundary."
    ),
    planned(
      "converter-receipt",
      "conversion",
      "identified EDF2ASC invocation",
      "conversion-receipt",
      "The EDF and ASC digests, converter identity, options, platform, and failsafe state are provenance."
    ),
    planned(
      "content-samples",
      "conversion content",
      "samples",
      "asc-samples-only",
      "Samples-only conversion is a supported vendor mode."
    ),
    planned(
      "content-events",
      "conversion content",
      "events",
      "asc-events-only",
      "Events-only conversion remains useful native tracker evidence."
    ),
    planned(
      "content-samples-events",
      "conversion content",
      "samples and events",
      "asc-samples-events",
      "Combined conversion is the preferred scientific workflow."
    ),
    planned(
      "eye-left",
      "eye layout",
      "monocular left",
      "asc-left-gaze",
      "The source eye identity must survive materialization."
    ),
    planned(
      "eye-right",
      "eye layout",
      "monocular right",
      "asc-right-gaze",
      "Right-eye data must not be placed in a left-eye column or selected implicitly."
    ),
    planned(
      "eye-binocular",
      "eye layout",
      "binocular",
      "asc-binocular-gaze",
      "Both eyes and their shared sample index must survive import."
    ),
    planned(
      "coordinates-gaze",
      "coordinate mode",
      "GAZE screen coordinates",
      "asc-basic-gaze",
      "Only configured GAZE coordinates may materialize as pixel positions."
    ),
    preserve(
      "coordinates-href",
      "coordinate mode",
      "HREF",
      "asc-href-preserved",
      "HREF is retained natively but is not relabelled as screen pixels."
    ),
    preserve(
      "coordinates-raw",
      "coordinate mode",
      "RAW or PUPIL camera coordinates",
      "asc-raw-preserved",
      "Camera-space values are retained without inventing a screen frame."
    ),
    planned(
      "tracking-head-fixed",
      "tracking mode",
      "head fixed",
      "asc-head-fixed",
      "The common head-fixed layout is release-critical."
    ),
    planned(
      "tracking-remote",
      "tracking mode",
      "remote with head target",
      "asc-remote-htarget",
      "Remote target fields alter the sample layout and must be explicit."
    ),
    planned(
      "rate-250",
      "sampling rate",
      "250 Hz",
      "asc-rate-250",
      "Rate evidence is checked against timestamps rather than trusted by label."
    ),
    planned(
      "rate-500",
      "sampling rate",
      "500 Hz",
      "asc-rate-500",
      "Rate evidence is checked against timestamps rather than trusted by label."
    ),
    planned(
      "rate-1000",
      "sampling rate",
      "1000 Hz",
      "asc-rate-1000",
      "Rate evidence is checked against timestamps rather than trusted by label."
    ),
    planned(
      "rate-2000",
      "sampling rate",
      "2000 Hz",
      "asc-rate-2000",
      "Sub-millisecond timestamps and converter precision require a dedicated fixture."
    ),
    planned(
      "pupil-area",
      "pupil representation",
      "area",
      "asc-pupil-area",
      "Area and diameter are not related by a scale factor and must remain distinct."
    ),
    planned(
      "pupil-diameter",
      "pupil representation",
      "diameter",
      "asc-pupil-diameter",
      "Diameter is accepted only when declared by source metadata or import configuration."
    ),
    planned(
      "pupil-undocumented",
      "pupil representation",
      "arbitrary or unknown",
      "asc-pupil-arbitrary",
      "An unknown native representation is retained as arbitrary units, never guessed."
    ),
    planned(
      "optional-gazeres",
      "optional sample field",
      "GAZERES",
      "asc-optional-gazeres",
      "Horizontal and vertical pixels-per-degree values are preserved."
    ),
    planned(
      "optional-velocity",
      "optional field",
      "velocity",
      "asc-optional-velocity",
      "Vendor velocity is native evidence and is not confused with eyes4s kinematics."
    ),
    planned(
      "optional-input-button-status",
      "optional sample field",
      "input, button, and status",
      "asc-optional-io-status",
      "Digital and tracker-status fields are retained when declared."
    ),
    planned(
      "native-events",
      "record type",
      "fixation, saccade, and blink events",
      "asc-native-events",
      "EyeLink online classifications remain vendor-native observations."
    ),
    planned(
      "messages",
      "record type",
      "MSG with optional offset",
      "asc-message-offsets",
      "Logged time, effective time, and exact payload are all retained."
    ),
    planned(
      "multiple-blocks",
      "recording structure",
      "multiple START/END blocks",
      "asc-multiple-blocks",
      "Configuration belongs to a block and may change between blocks."
    ),
    planned(
      "unknown-records",
      "forward compatibility",
      "unknown line token",
      "asc-unknown-record",
      "Unknown well-formed records are preserved rather than silently dropped."
    ),
    preserve(
      "failsafe-output",
      "conversion recovery",
      "EDF2ASC failsafe output",
      "asc-failsafe-warning",
      "Recovered content is parsed with an explicit warning and never presented as an ordinary clean source."
    ),
    preserve(
      "hardware-legacy",
      "hardware generation",
      "EyeLink I and II",
      "asc-legacy-unvalidated",
      "Legacy records are retained, but no hardware-wide support claim is made without fixtures."
    ),
    planned(
      "hardware-1000-family",
      "hardware generation",
      "EyeLink 1000 and 1000 Plus",
      "asc-real-1000-family",
      "The widely deployed 1000 family is release-critical."
    ),
    planned(
      "hardware-portable-duo",
      "hardware generation",
      "EyeLink Portable Duo",
      "asc-real-portable-duo",
      "Portable Duo supplies the required 2000 Hz and remote-layout evidence."
    ),
    planned(
      "hardware-eyelink-3",
      "hardware generation",
      "EyeLink 3",
      "asc-real-eyelink-3",
      "The current hardware generation must be represented before a broad current-support claim."
    )
  )

  val required: Vector[EyeLinkCapability] = capabilities.filter(_.isReleaseRequirement)

  val preservedOnly: Vector[EyeLinkCapability] =
    capabilities.filter(_.disposition == EyeLinkDisposition.PreserveWithoutInterpretation)

  val explicitlyRejected: Vector[EyeLinkCapability] =
    capabilities.filter(_.disposition == EyeLinkDisposition.RejectExplicitly)

  val releaseReady: Boolean = required.nonEmpty && required.forall(_.isValidated)

  def find(id: String): Option[EyeLinkCapability] = capabilities.find(_.id == id)

end EyeLinkSupport
