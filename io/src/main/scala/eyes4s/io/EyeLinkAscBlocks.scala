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

import eyes4s.kernel.Detector
import eyes4s.kernel.Machine

import scala.util.Try

enum AscEyeLayout derives CanEqual:
  case Left
  case Right
  case Binocular

enum AscCoordinateMode derives CanEqual:
  case Gaze
  case HeadReference
  case RawPupil

enum AscOptionalColumn derives CanEqual:
  case Resolution
  case Velocity
  case Input
  case Buttons
  case Status
  case HeadTarget

enum AscPupilRepresentation derives CanEqual:
  case Area
  case Diameter
  case Undocumented(token: String)
  case Unspecified

/** Positive decimal evidence read from ASC without a binary floating-point
  * round trip.
  */
final class AscPositiveDecimal private (val value: BigDecimal):
  override def toString: String = value.toString

object AscPositiveDecimal:
  private[io] def parse(token: String): Option[AscPositiveDecimal] =
    Try(BigDecimal(token)).toOption.filter(_ > 0).map(new AscPositiveDecimal(_))

/** Parsed SAMPLES or EVENTS layout. Missing required members remain explicit;
  * they are never filled from a previous block or inferred from record width.
  */
final class AscDataLayout private[io] (
    val declaredAt: AscSourceLine,
    val coordinateMode: Option[AscCoordinateMode],
    val eyeLayout: Option[AscEyeLayout],
    val rateHz: Option[AscPositiveDecimal],
    val trackingEvidence: Option[String],
    val filterEvidence: Option[String],
    val optionalColumns: Set[AscOptionalColumn],
    val unrecognizedTokens: Vector[String]
):
  def isRemote: Boolean = optionalColumns.contains(AscOptionalColumn.HeadTarget)

  private[io] def signature: String =
    Vector(
      coordinateMode.toString,
      eyeLayout.toString,
      rateHz.map(_.value.toString).toString,
      trackingEvidence.toString,
      filterEvidence.toString,
      optionalColumns.toVector.map(_.toString).sorted.mkString(","),
      unrecognizedTokens.mkString("\u001f")
    ).mkString("\u001e")

/** Configuration snapshot effective for a subsequent record in one block. */
final class AscBlockConfiguration private[io] (
    val revision: Int,
    val samples: Option[AscDataLayout],
    val events: Option[AscDataLayout],
    val prescaler: Option[AscPositiveDecimal],
    val prescalerDeclaredAt: Option[AscSourceLine],
    val velocityPrescaler: Option[AscPositiveDecimal],
    val velocityPrescalerDeclaredAt: Option[AscSourceLine],
    val pupil: Option[AscPupilRepresentation],
    val pupilDeclaredAt: Option[AscSourceLine]
):
  private[io] def withSamples(value: AscDataLayout): AscBlockConfiguration =
    new AscBlockConfiguration(
      revision + 1,
      Some(value),
      events,
      prescaler,
      prescalerDeclaredAt,
      velocityPrescaler,
      velocityPrescalerDeclaredAt,
      pupil,
      pupilDeclaredAt
    )

  private[io] def withEvents(value: AscDataLayout): AscBlockConfiguration =
    new AscBlockConfiguration(
      revision + 1,
      samples,
      Some(value),
      prescaler,
      prescalerDeclaredAt,
      velocityPrescaler,
      velocityPrescalerDeclaredAt,
      pupil,
      pupilDeclaredAt
    )

  private[io] def withPrescaler(
      value: AscPositiveDecimal,
      source: AscSourceLine
  ): AscBlockConfiguration =
    new AscBlockConfiguration(
      revision + 1,
      samples,
      events,
      Some(value),
      Some(source),
      velocityPrescaler,
      velocityPrescalerDeclaredAt,
      pupil,
      pupilDeclaredAt
    )

  private[io] def withVelocityPrescaler(
      value: AscPositiveDecimal,
      source: AscSourceLine
  ): AscBlockConfiguration =
    new AscBlockConfiguration(
      revision + 1,
      samples,
      events,
      prescaler,
      prescalerDeclaredAt,
      Some(value),
      Some(source),
      pupil,
      pupilDeclaredAt
    )

  private[io] def withPupil(
      value: AscPupilRepresentation,
      source: AscSourceLine
  ): AscBlockConfiguration =
    new AscBlockConfiguration(
      revision + 1,
      samples,
      events,
      prescaler,
      prescalerDeclaredAt,
      velocityPrescaler,
      velocityPrescalerDeclaredAt,
      Some(value),
      Some(source)
    )

object AscBlockConfiguration:
  private[io] val Empty: AscBlockConfiguration =
    new AscBlockConfiguration(0, None, None, None, None, None, None, None, None)

final class AscBlockStart private[io] (
    val blockNumber: Long,
    val source: AscSourceLine,
    val trackerTimeToken: Option[String],
    val declaredEyeLayout: Option[AscEyeLayout],
    val declaresSamples: Boolean,
    val declaresEvents: Boolean,
    val unrecognizedTokens: Vector[String]
)

final class AscBlockSnapshot private[io] (
    val start: AscBlockStart,
    val configuration: AscBlockConfiguration
)

enum AscBlockRequirement derives CanEqual:
  case RecordOutsideBlock(source: String, line: Long, record: String)
  case SampleConfigurationMissing(source: String, line: Long, block: Long)
  case EventConfigurationMissing(source: String, line: Long, block: Long, record: String)
  case CoordinateModeMissing(source: String, line: Long, block: Long, stream: String)
  case EyeLayoutMissing(source: String, line: Long, block: Long, stream: String)
  case RateEvidenceMissing(source: String, line: Long, block: Long, stream: String)
  case ContentNotDeclared(source: String, line: Long, block: Long, content: String)
  case MalformedKnownRecord(source: String, line: Long, block: Long, record: String)

  def message: String = this match
    case RecordOutsideBlock(source, line, record) =>
      s"ASC source='$source' line=$line record=$record occurs outside a START/END block."
    case SampleConfigurationMissing(source, line, block) =>
      s"ASC source='$source' line=$line block=$block has a sample before SAMPLES configuration."
    case EventConfigurationMissing(source, line, block, record) =>
      s"ASC source='$source' line=$line block=$block record=$record occurs before EVENTS configuration."
    case CoordinateModeMissing(source, line, block, stream) =>
      s"ASC source='$source' line=$line block=$block stream=$stream has no coordinate mode."
    case EyeLayoutMissing(source, line, block, stream) =>
      s"ASC source='$source' line=$line block=$block stream=$stream has no eye layout."
    case RateEvidenceMissing(source, line, block, stream) =>
      s"ASC source='$source' line=$line block=$block stream=$stream has no positive RATE evidence."
    case ContentNotDeclared(source, line, block, content) =>
      s"ASC source='$source' line=$line block=$block contains $content not declared by START."
    case MalformedKnownRecord(source, line, block, record) =>
      s"ASC source='$source' line=$line block=$block contains malformed known record=$record."

end AscBlockRequirement

enum AscBlockOutcome derives CanEqual:
  case StartEyeDeclarationMissing(source: String, line: Long, block: Long)
  case StartContentDeclarationMissing(source: String, line: Long, block: Long)
  case ConfigurationOutsideBlock(source: String, line: Long, record: String)
  case UnexpectedEnd(source: String, line: Long)
  case DuplicateConfiguration(
      source: String,
      line: Long,
      block: Long,
      record: String,
      previousLine: Long,
      changed: Boolean
  )
  case InvalidConfigurationValue(
      source: String,
      line: Long,
      block: Long,
      field: String,
      value: String
  )
  case MissingConfigurationValue(source: String, line: Long, block: Long, field: String)
  case ConflictingConfigurationValues(
      source: String,
      line: Long,
      block: Long,
      field: String,
      values: Vector[String]
  )
  case NonAsciiConfigurationField(source: String, line: Long, block: Long, index: Int)
  case NonAsciiStartField(source: String, line: Long, block: Long, index: Int)
  case StartAndConfigurationEyeMismatch(
      source: String,
      line: Long,
      block: Long,
      stream: String,
      start: AscEyeLayout,
      configured: AscEyeLayout
  )
  case ConfigurationContentNotDeclared(
      source: String,
      line: Long,
      block: Long,
      stream: String
  )

  def message: String = this match
    case StartEyeDeclarationMissing(source, line, block) =>
      s"ASC source='$source' line=$line block=$block START has no LEFT or RIGHT declaration."
    case StartContentDeclarationMissing(source, line, block) =>
      s"ASC source='$source' line=$line block=$block START declares neither SAMPLES nor EVENTS."
    case ConfigurationOutsideBlock(source, line, record) =>
      s"ASC source='$source' line=$line configuration=$record occurs outside a START/END block."
    case UnexpectedEnd(source, line) =>
      s"ASC source='$source' line=$line END has no open START block."
    case DuplicateConfiguration(source, line, block, record, previousLine, changed) =>
      s"ASC source='$source' line=$line block=$block repeats configuration=$record from line=$previousLine with changed=$changed."
    case InvalidConfigurationValue(source, line, block, field, value) =>
      s"ASC source='$source' line=$line block=$block field=$field has invalid value='$value'."
    case MissingConfigurationValue(source, line, block, field) =>
      s"ASC source='$source' line=$line block=$block field=$field has no following value."
    case ConflictingConfigurationValues(source, line, block, field, values) =>
      s"ASC source='$source' line=$line block=$block field=$field has conflicting values=${values.mkString("[", ",", "]")}."
    case NonAsciiConfigurationField(source, line, block, index) =>
      s"ASC source='$source' line=$line block=$block has non-ASCII configuration field index=$index."
    case NonAsciiStartField(source, line, block, index) =>
      s"ASC source='$source' line=$line block=$block has non-ASCII START field index=$index."
    case StartAndConfigurationEyeMismatch(source, line, block, stream, start, configured) =>
      s"ASC source='$source' line=$line block=$block stream=$stream eye layout=$configured disagrees with START eye layout=$start."
    case ConfigurationContentNotDeclared(source, line, block, stream) =>
      s"ASC source='$source' line=$line block=$block has $stream configuration not declared by START."

end AscBlockOutcome

/** One input line with the block snapshot effective at that line. */
final class AscBlockLine private[io] (
    val line: AscLineResult,
    val block: Option[AscBlockSnapshot],
    val requirements: Vector[AscBlockRequirement],
    val outcomes: Vector[AscBlockOutcome]
)

enum AscBlockClosureReason:
  case EndRecord(source: AscSourceLine)
  case ReplacedByNestedStart(source: AscSourceLine)
  case EndOfInput

final class AscClosedBlock private[io] (
    val block: AscBlockSnapshot,
    val reason: AscBlockClosureReason
)

enum AscBlockEmission:
  case Line(value: AscBlockLine)
  case Closed(value: AscClosedBlock)

/** Stateful interpretation of block configuration without parsing scientific
  * sample or event values.
  */
object EyeLinkAscBlocks:

  private final case class State(nextBlock: Long, open: Option[AscBlockSnapshot])

  val machine: Machine[AscLineResult, AscBlockEmission] =
    Machine(
      new Detector[State, AscLineResult, AscBlockEmission]:
        def init: State = State(1L, None)

        def step(
            state: State,
            input: AscLineResult
        ): (State, Vector[AscBlockEmission]) =
          input.record match
            case AscRecord.Boundary(AscBoundaryKind.Start, fields) =>
              val (opened, outcomes) = openBlock(state.nextBlock, input.source, fields)
              val closed             = state.open.map(previous =>
                AscBlockEmission.Closed(
                  new AscClosedBlock(
                    previous,
                    AscBlockClosureReason.ReplacedByNestedStart(input.source)
                  )
                )
              )
              val line = AscBlockEmission.Line(
                new AscBlockLine(input, Some(opened), Vector.empty, outcomes)
              )
              (State(state.nextBlock + 1L, Some(opened)), closed.toVector :+ line)

            case AscRecord.Boundary(AscBoundaryKind.End, _) =>
              state.open match
                case None =>
                  val line = new AscBlockLine(
                    input,
                    None,
                    Vector.empty,
                    Vector(
                      AscBlockOutcome.UnexpectedEnd(input.source.source, input.source.number)
                    )
                  )
                  (state, Vector(AscBlockEmission.Line(line)))
                case Some(open) =>
                  val line = AscBlockEmission.Line(
                    new AscBlockLine(input, Some(open), Vector.empty, Vector.empty)
                  )
                  val closed = AscBlockEmission.Closed(
                    new AscClosedBlock(open, AscBlockClosureReason.EndRecord(input.source))
                  )
                  (state.copy(open = None), Vector(line, closed))

            case AscRecord.Configuration(kind, fields) =>
              state.open match
                case None =>
                  val outcome = AscBlockOutcome.ConfigurationOutsideBlock(
                    input.source.source,
                    input.source.number,
                    kind.toString
                  )
                  val line = new AscBlockLine(input, None, Vector.empty, Vector(outcome))
                  (state, Vector(AscBlockEmission.Line(line)))
                case Some(open) =>
                  val (updated, outcomes) = configure(open, kind, fields)
                  val line = new AscBlockLine(input, Some(updated), Vector.empty, outcomes)
                  (state.copy(open = Some(updated)), Vector(AscBlockEmission.Line(line)))

            case AscRecord.MalformedKnown(kind, _) =>
              val requirements = state.open match
                case Some(open) =>
                  Vector(
                    AscBlockRequirement.MalformedKnownRecord(
                      input.source.source,
                      input.source.number,
                      open.start.blockNumber,
                      kind.toString
                    )
                  )
                case None =>
                  Vector(
                    AscBlockRequirement.RecordOutsideBlock(
                      input.source.source,
                      input.source.number,
                      kind.toString
                    )
                  )
              val line = new AscBlockLine(input, state.open, requirements, Vector.empty)
              (state, Vector(AscBlockEmission.Line(line)))

            case record =>
              val requirements = requirementsFor(record, input.source, state.open)
              val line         = new AscBlockLine(input, state.open, requirements, Vector.empty)
              (state, Vector(AscBlockEmission.Line(line)))

        def flush(state: State): Vector[AscBlockEmission] =
          state.open.toVector.map(open =>
            AscBlockEmission.Closed(
              new AscClosedBlock(open, AscBlockClosureReason.EndOfInput)
            )
          )
    )

  private def openBlock(
      number: Long,
      source: AscSourceLine,
      fields: AscFields
  ): (AscBlockSnapshot, Vector[AscBlockOutcome]) =
    val values       = asciiValues(fields)
    val trackerTime  = values.headOption.flatten
    val tail         = values.drop(1).flatten.map(upperAscii)
    val eyes         = eyeLayout(tail)
    val samples      = tail.contains("SAMPLES")
    val events       = tail.contains("EVENTS")
    val recognized   = Set("LEFT", "RIGHT", "SAMPLES", "EVENTS")
    val unrecognized = tail.filterNot(recognized)
    val start        = new AscBlockStart(
      number,
      source,
      trackerTime,
      eyes,
      samples,
      events,
      unrecognized
    )
    val outcomes = Vector.newBuilder[AscBlockOutcome]
    values.zipWithIndex.foreach {
      case (None, index) =>
        outcomes += AscBlockOutcome.NonAsciiStartField(
          source.source,
          source.number,
          number,
          index
        )
      case _ => ()
    }
    if eyes.isEmpty then
      outcomes += AscBlockOutcome.StartEyeDeclarationMissing(
        source.source,
        source.number,
        number
      )
    if !samples && !events then
      outcomes += AscBlockOutcome.StartContentDeclarationMissing(
        source.source,
        source.number,
        number
      )
    (
      new AscBlockSnapshot(start, AscBlockConfiguration.Empty),
      outcomes.result()
    )

  private def configure(
      block: AscBlockSnapshot,
      kind: AscConfigurationKind,
      fields: AscFields
  ): (AscBlockSnapshot, Vector[AscBlockOutcome]) =
    kind match
      case AscConfigurationKind.Samples   => configureData(block, kind, fields, sample = true)
      case AscConfigurationKind.Events    => configureData(block, kind, fields, sample = false)
      case AscConfigurationKind.Prescaler =>
        configurePositive(
          block,
          kind,
          fields,
          _.prescaler,
          _.prescalerDeclaredAt,
          (configuration, value, source) => configuration.withPrescaler(value, source)
        )
      case AscConfigurationKind.VelocityPrescaler =>
        configurePositive(
          block,
          kind,
          fields,
          _.velocityPrescaler,
          _.velocityPrescalerDeclaredAt,
          (configuration, value, source) => configuration.withVelocityPrescaler(value, source)
        )
      case AscConfigurationKind.Pupil => configurePupil(block, fields)

  private def configureData(
      block: AscBlockSnapshot,
      kind: AscConfigurationKind,
      fields: AscFields,
      sample: Boolean
  ): (AscBlockSnapshot, Vector[AscBlockOutcome]) =
    val parsed    = parseLayout(block.start.blockNumber, fields)
    val previous  = if sample then block.configuration.samples else block.configuration.events
    val duplicate = previous.map(old =>
      duplicateOutcome(
        block,
        kind,
        old.declaredAt,
        old.signature != parsed.layout.signature,
        fields.source
      )
    )
    val updatedConfiguration = if sample then block.configuration.withSamples(parsed.layout)
    else block.configuration.withEvents(parsed.layout)
    val updated     = new AscBlockSnapshot(block.start, updatedConfiguration)
    val consistency = Vector.newBuilder[AscBlockOutcome]
    val stream      = if sample then "samples" else "events"
    val declared    = if sample then block.start.declaresSamples else block.start.declaresEvents
    if !declared then
      consistency += AscBlockOutcome.ConfigurationContentNotDeclared(
        fields.source.source,
        fields.source.number,
        block.start.blockNumber,
        stream
      )
    for
      startEye      <- block.start.declaredEyeLayout
      configuredEye <- parsed.layout.eyeLayout
      if startEye != configuredEye
    do
      consistency += AscBlockOutcome.StartAndConfigurationEyeMismatch(
        fields.source.source,
        fields.source.number,
        block.start.blockNumber,
        stream,
        startEye,
        configuredEye
      )
    (updated, duplicate.toVector ++ parsed.outcomes ++ consistency.result())

  private final case class ParsedLayout(
      layout: AscDataLayout,
      outcomes: Vector[AscBlockOutcome]
  )

  private def parseLayout(block: Long, fields: AscFields): ParsedLayout =
    val values   = asciiValues(fields)
    val outcomes = Vector.newBuilder[AscBlockOutcome]
    values.zipWithIndex.foreach {
      case (None, index) =>
        outcomes += AscBlockOutcome.NonAsciiConfigurationField(
          fields.source.source,
          fields.source.number,
          block,
          index
        )
      case _ => ()
    }
    val tokens         = values.map(_.map(upperAscii))
    var index          = 0
    val coordinates    = Vector.newBuilder[(String, AscCoordinateMode)]
    var left           = false
    var right          = false
    val rates          = Vector.newBuilder[(String, Option[AscPositiveDecimal])]
    val trackingValues = Vector.newBuilder[String]
    val filterValues   = Vector.newBuilder[String]
    var optional       = Set.empty[AscOptionalColumn]
    val unrecognized   = Vector.newBuilder[String]

    def valueAfter(field: String): Option[String] =
      if index + 1 >= tokens.length then
        outcomes += AscBlockOutcome.MissingConfigurationValue(
          fields.source.source,
          fields.source.number,
          block,
          field
        )
        None
      else
        index += 1
        tokens(index) match
          case Some(value) => Some(value)
          case None        => None

    while index < tokens.length do
      tokens(index) match
        case None         => ()
        case Some("GAZE") => coordinates += "GAZE" -> AscCoordinateMode.Gaze
        case Some("HREF") => coordinates += "HREF" -> AscCoordinateMode.HeadReference
        case Some(value @ ("RAW" | "PUPIL")) =>
          coordinates += value -> AscCoordinateMode.RawPupil
        case Some("LEFT")  => left = true
        case Some("RIGHT") => right = true
        case Some("RATE")  =>
          valueAfter("RATE").foreach(value => rates += value -> AscPositiveDecimal.parse(value))
        case Some("TRACKING") => valueAfter("TRACKING").foreach(trackingValues += _)
        case Some("FILTER")   => valueAfter("FILTER").foreach(filterValues += _)
        case Some("RES")      => optional += AscOptionalColumn.Resolution
        case Some("VEL")      => optional += AscOptionalColumn.Velocity
        case Some("INPUT")    => optional += AscOptionalColumn.Input
        case Some("BUTTONS")  => optional += AscOptionalColumn.Buttons
        case Some("STATUS")   => optional += AscOptionalColumn.Status
        case Some("HTARGET")  => optional += AscOptionalColumn.HeadTarget
        case Some(value)      => unrecognized += value
      index += 1

    val coordinateValues    = coordinates.result()
    val distinctCoordinates = coordinateValues.map(_._2).distinct
    val coordinate          = distinctCoordinates match
      case Vector(value)                         => Some(value)
      case values if values.lengthCompare(1) > 0 =>
        outcomes += AscBlockOutcome.ConflictingConfigurationValues(
          fields.source.source,
          fields.source.number,
          block,
          "coordinate",
          AscDiagnosticText.bounded(coordinateValues.map(_._1))
        )
        None
      case _ => None

    val rateValues = rates.result()
    rateValues.foreach { case (value, parsed) =>
      if parsed.isEmpty then
        outcomes += AscBlockOutcome.InvalidConfigurationValue(
          fields.source.source,
          fields.source.number,
          block,
          "RATE",
          AscDiagnosticText.bounded(value)
        )
    }
    val validRates = rateValues.flatMap(_._2).map(_.value).distinct
    val rate       =
      if rateValues.lengthCompare(1) > 0 || validRates.lengthCompare(1) > 0 then
        outcomes += AscBlockOutcome.ConflictingConfigurationValues(
          fields.source.source,
          fields.source.number,
          block,
          "RATE",
          AscDiagnosticText.bounded(rateValues.map(_._1))
        )
        None
      else rateValues.headOption.flatMap(_._2)

    def singleton(field: String, values: Vector[String]): Option[String] =
      values match
        case Vector(value)                             => Some(value)
        case repeated if repeated.lengthCompare(1) > 0 =>
          outcomes += AscBlockOutcome.ConflictingConfigurationValues(
            fields.source.source,
            fields.source.number,
            block,
            field,
            AscDiagnosticText.bounded(repeated)
          )
          None
        case _ => None

    val tracking = singleton("TRACKING", trackingValues.result())
    val filter   = singleton("FILTER", filterValues.result())

    ParsedLayout(
      new AscDataLayout(
        fields.source,
        coordinate,
        eyeLayout(left, right),
        rate,
        tracking,
        filter,
        optional,
        unrecognized.result()
      ),
      outcomes.result()
    )

  private def configurePositive(
      block: AscBlockSnapshot,
      kind: AscConfigurationKind,
      fields: AscFields,
      previous: AscBlockConfiguration => Option[AscPositiveDecimal],
      previousSource: AscBlockConfiguration => Option[AscSourceLine],
      update: (
          AscBlockConfiguration,
          AscPositiveDecimal,
          AscSourceLine
      ) => AscBlockConfiguration
  ): (AscBlockSnapshot, Vector[AscBlockOutcome]) =
    val value = fields.token(0).flatMap(_.ascii)
    value.flatMap(AscPositiveDecimal.parse) match
      case None =>
        val outcome = value match
          case Some(actual) =>
            AscBlockOutcome.InvalidConfigurationValue(
              fields.source.source,
              fields.source.number,
              block.start.blockNumber,
              kind.toString,
              AscDiagnosticText.bounded(actual)
            )
          case None =>
            AscBlockOutcome.MissingConfigurationValue(
              fields.source.source,
              fields.source.number,
              block.start.blockNumber,
              kind.toString
            )
        (block, Vector(outcome))
      case Some(parsed) =>
        val duplicate = previous(block.configuration).flatMap(prior =>
          previousSource(block.configuration).map(source =>
            duplicateOutcome(block, kind, source, prior.value != parsed.value, fields.source)
          )
        )
        val updated = new AscBlockSnapshot(
          block.start,
          update(block.configuration, parsed, fields.source)
        )
        (updated, duplicate.toVector)

  private def configurePupil(
      block: AscBlockSnapshot,
      fields: AscFields
  ): (AscBlockSnapshot, Vector[AscBlockOutcome]) =
    fields.token(0).flatMap(_.ascii).map(upperAscii) match
      case None =>
        (
          block,
          Vector(
            AscBlockOutcome.MissingConfigurationValue(
              fields.source.source,
              fields.source.number,
              block.start.blockNumber,
              "PUPIL"
            )
          )
        )
      case Some(value) =>
        val pupil = value match
          case "AREA"     => AscPupilRepresentation.Area
          case "DIAMETER" => AscPupilRepresentation.Diameter
          case other      => AscPupilRepresentation.Undocumented(other)
        val duplicate = block.configuration.pupil.flatMap(previous =>
          block.configuration.pupilDeclaredAt.map(source =>
            duplicateOutcome(
              block,
              AscConfigurationKind.Pupil,
              source,
              previous != pupil,
              fields.source
            )
          )
        )
        val updated = new AscBlockSnapshot(
          block.start,
          block.configuration.withPupil(pupil, fields.source)
        )
        (updated, duplicate.toVector)

  private def duplicateOutcome(
      block: AscBlockSnapshot,
      kind: AscConfigurationKind,
      previous: AscSourceLine,
      changed: Boolean,
      current: AscSourceLine
  ): AscBlockOutcome =
    AscBlockOutcome.DuplicateConfiguration(
      current.source,
      current.number,
      block.start.blockNumber,
      kind.toString,
      previous.number,
      changed
    )

  private def requirementsFor(
      record: AscRecord,
      source: AscSourceLine,
      block: Option[AscBlockSnapshot]
  ): Vector[AscBlockRequirement] =
    block match
      case None =>
        record match
          case AscRecord.Sample(_) | AscRecord.NativeEvent(_, _) | AscRecord.Button(_) |
              AscRecord.Input(_) | AscRecord.LostData(_) =>
            Vector(
              AscBlockRequirement.RecordOutsideBlock(
                source.source,
                source.number,
                recordName(record)
              )
            )
          case _ => Vector.empty
      case Some(snapshot) =>
        record match
          case AscRecord.Sample(_) => sampleRequirements(snapshot, source)
          case AscRecord.NativeEvent(_, _) | AscRecord.Button(_) | AscRecord.Input(_) |
              AscRecord.LostData(_) =>
            eventRequirements(snapshot, source, recordName(record))
          case _ => Vector.empty

  private def sampleRequirements(
      block: AscBlockSnapshot,
      source: AscSourceLine
  ): Vector[AscBlockRequirement] =
    val output = Vector.newBuilder[AscBlockRequirement]
    if !block.start.declaresSamples then
      output += AscBlockRequirement.ContentNotDeclared(
        source.source,
        source.number,
        block.start.blockNumber,
        "samples"
      )
    block.configuration.samples match
      case None =>
        output += AscBlockRequirement.SampleConfigurationMissing(
          source.source,
          source.number,
          block.start.blockNumber
        )
      case Some(layout) =>
        output ++= layoutRequirements(block, source, "samples", layout)
    output.result()

  private def eventRequirements(
      block: AscBlockSnapshot,
      source: AscSourceLine,
      record: String
  ): Vector[AscBlockRequirement] =
    val output = Vector.newBuilder[AscBlockRequirement]
    if !block.start.declaresEvents then
      output += AscBlockRequirement.ContentNotDeclared(
        source.source,
        source.number,
        block.start.blockNumber,
        "events"
      )
    block.configuration.events match
      case None =>
        output += AscBlockRequirement.EventConfigurationMissing(
          source.source,
          source.number,
          block.start.blockNumber,
          record
        )
      case Some(layout) =>
        output ++= layoutRequirements(block, source, "events", layout)
    output.result()

  private def layoutRequirements(
      block: AscBlockSnapshot,
      source: AscSourceLine,
      stream: String,
      layout: AscDataLayout
  ): Vector[AscBlockRequirement] =
    val output = Vector.newBuilder[AscBlockRequirement]
    if layout.coordinateMode.isEmpty then
      output += AscBlockRequirement.CoordinateModeMissing(
        source.source,
        source.number,
        block.start.blockNumber,
        stream
      )
    if layout.eyeLayout.isEmpty then
      output += AscBlockRequirement.EyeLayoutMissing(
        source.source,
        source.number,
        block.start.blockNumber,
        stream
      )
    if layout.rateHz.isEmpty then
      output += AscBlockRequirement.RateEvidenceMissing(
        source.source,
        source.number,
        block.start.blockNumber,
        stream
      )
    output.result()

  private def eyeLayout(tokens: Vector[String]): Option[AscEyeLayout] =
    eyeLayout(tokens.contains("LEFT"), tokens.contains("RIGHT"))

  private def eyeLayout(left: Boolean, right: Boolean): Option[AscEyeLayout] =
    (left, right) match
      case (true, true)   => Some(AscEyeLayout.Binocular)
      case (true, false)  => Some(AscEyeLayout.Left)
      case (false, true)  => Some(AscEyeLayout.Right)
      case (false, false) => None

  private def asciiValues(fields: AscFields): Vector[Option[String]] =
    fields.tokens.map(_.ascii)

  private def upperAscii(value: String): String =
    value.map { character =>
      if character >= 'a' && character <= 'z' then (character - ('a' - 'A')).toChar
      else character
    }

  private def recordName(record: AscRecord): String = record match
    case AscRecord.Blank                   => "Blank"
    case AscRecord.Comment(_)              => "Comment"
    case AscRecord.Sample(_)               => "Sample"
    case AscRecord.Message(_)              => "Message"
    case AscRecord.Boundary(kind, _)       => kind.toString
    case AscRecord.Configuration(kind, _)  => kind.toString
    case AscRecord.NativeEvent(kind, _)    => kind.toString
    case AscRecord.Button(_)               => "Button"
    case AscRecord.Input(_)                => "Input"
    case AscRecord.LostData(_)             => "LostData"
    case AscRecord.Unknown(_, _)           => "Unknown"
    case AscRecord.MalformedKnown(kind, _) => s"MalformedKnown($kind)"

end EyeLinkAscBlocks
