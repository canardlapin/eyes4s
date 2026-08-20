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
import eyes4s.kernel.Frame
import eyes4s.kernel.Machine
import eyes4s.kernel.Pt
import eyes4s.kernel.Unit2D

import scala.util.Try

/** The column order implemented by the sample parser. Naming the convention
  * lets oracle manifests bind evidence to the exact layout rather than to an
  * informal claim that an ASC file was parsed.
  */
enum AscSampleLayoutConvention derives CanEqual:
  case Edf2AscCanonicalV1

enum AscDotGazePolicy derives CanEqual:
  case MissingNativeNotBlink

enum AscZeroPupilPolicy derives CanEqual:
  case MissingNative

enum AscRecordedEye derives CanEqual:
  case Left
  case Right

enum AscNativePair derives CanEqual:
  case MissingDot
  case Values(x: BigDecimal, y: BigDecimal)

enum AscNativeScalar derives CanEqual:
  case MissingDot
  case Value(value: BigDecimal)

enum AscNativeInteger derives CanEqual:
  case MissingDot
  case Value(value: BigInt)

enum AscPupilObservation derives CanEqual:
  case MissingDot
  case MissingZero
  case Measured(value: BigDecimal)

final class AscNativePupil private[io] (
    val representation: AscPupilRepresentation,
    val observation: AscPupilObservation
)

final class AscNativeCoordinates private[io] (
    val mode: AscCoordinateMode,
    val position: AscNativePair
)

final class AscNativeEyeSample private[io] (
    val eye: AscRecordedEye,
    val coordinates: AscNativeCoordinates,
    val pupil: AscNativePupil,
    val velocity: Option[AscNativePair]
):
  /** Convert only configured GAZE coordinates into a typed pixel position and
    * classify it against the supplied frame. Missing native gaze remains
    * missing; this operation never invents a blink.
    */
  def pixelPosition(
      frame: Frame[Unit2D.Px]
  ): Either[AscSampleMaterializationError, AscPixelPosition] =
    coordinates.mode match
      case mode @ (AscCoordinateMode.HeadReference | AscCoordinateMode.RawPupil) =>
        Left(
          AscSampleMaterializationError.CoordinateModeIsNotScreenPixels(
            eye,
            mode,
            frame.id.name
          )
        )
      case AscCoordinateMode.Gaze =>
        coordinates.position match
          case AscNativePair.MissingDot   => Right(AscPixelPosition.MissingNativeGaze)
          case AscNativePair.Values(x, y) =>
            val xDouble = x.toDouble
            val yDouble = y.toDouble
            if !(xDouble.isFinite && yDouble.isFinite) then
              Left(
                AscSampleMaterializationError.PixelValueOutsideDoubleRange(
                  eye,
                  x.toString,
                  y.toString,
                  frame.id.name
                )
              )
            else
              val point = Pt[Unit2D.Px](xDouble, yDouble)
              if frame.contains(point) then Right(AscPixelPosition.InsideFrame(point))
              else Right(AscPixelPosition.OffSurface(point))

enum AscPixelPosition derives CanEqual:
  case MissingNativeGaze
  case InsideFrame(point: Pt[Unit2D.Px])
  case OffSurface(point: Pt[Unit2D.Px])

enum AscSampleMaterializationError derives CanEqual:
  case CoordinateModeIsNotScreenPixels(
      eye: AscRecordedEye,
      mode: AscCoordinateMode,
      frame: String
  )
  case PixelValueOutsideDoubleRange(
      eye: AscRecordedEye,
      x: String,
      y: String,
      frame: String
  )

  def message: String = this match
    case CoordinateModeIsNotScreenPixels(eye, mode, frame) =>
      s"EyeLink eye=$eye coordinate mode=$mode cannot materialize in pixel frame='$frame'."
    case PixelValueOutsideDoubleRange(eye, x, y, frame) =>
      s"EyeLink eye=$eye coordinates=($x,$y) exceed pixel frame='$frame' numeric range."

end AscSampleMaterializationError

final class AscHeadTarget private[io] (
    val position: AscNativePair,
    val distance: AscNativeScalar,
    val flags: String
)

/** One source sample with native EyeLink semantics and no selected-eye
  * projection.
  */
final class AscNativeSample private[io] (
    val convention: AscSampleLayoutConvention,
    val dotGazePolicy: AscDotGazePolicy,
    val zeroPupilPolicy: AscZeroPupilPolicy,
    val source: AscSourceLine,
    val blockNumber: Long,
    val timestamp: BigDecimal,
    val left: Option[AscNativeEyeSample],
    val right: Option[AscNativeEyeSample],
    val resolution: Option[AscNativePair],
    val input: Option[AscNativeInteger],
    val buttons: Option[AscNativeInteger],
    val status: Option[AscNativeInteger],
    val headTarget: Option[AscHeadTarget]
)

enum AscDiagnosticSeverity derives CanEqual:
  case Warning
  case Error

enum AscSampleDiagnostic:
  case NotSample(source: String, line: Long, record: String)
  case BlockRequirement(requirement: AscBlockRequirement)
  case SampleBlockMissing(source: String, line: Long)
  case SampleLayoutMissing(source: String, line: Long, block: Long)
  case UnexpectedFieldCount(
      source: String,
      line: Long,
      block: Long,
      expected: Int,
      actual: Int,
      convention: AscSampleLayoutConvention
  )
  case NonAsciiField(source: String, line: Long, block: Long, index: Int)
  case InvalidDecimal(
      source: String,
      line: Long,
      block: Long,
      field: String,
      index: Int,
      value: String
  )
  case NegativeTimestamp(source: String, line: Long, block: Long, value: String)
  case PartialMissingPair(
      source: String,
      line: Long,
      block: Long,
      field: String,
      x: String,
      y: String
  )
  case NegativePupil(
      source: String,
      line: Long,
      block: Long,
      eye: AscRecordedEye,
      value: String
  )
  case InvalidPupil(
      source: String,
      line: Long,
      block: Long,
      eye: AscRecordedEye,
      value: String
  )
  case InvalidInteger(
      source: String,
      line: Long,
      block: Long,
      field: String,
      index: Int,
      value: String
  )
  case PupilRepresentationUndeclared(source: String, line: Long, block: Long)
  case NonIncreasingTimestamp(
      source: String,
      line: Long,
      block: Long,
      previous: String,
      current: String
  )

  def severity: AscDiagnosticSeverity = this match
    case PupilRepresentationUndeclared(_, _, _) => AscDiagnosticSeverity.Warning
    case _                                      => AscDiagnosticSeverity.Error

  def message: String = this match
    case NotSample(source, line, record) =>
      s"ASC source='$source' line=$line record=$record is not a sample row."
    case BlockRequirement(requirement)    => requirement.message
    case SampleBlockMissing(source, line) =>
      s"ASC source='$source' line=$line sample has no recording block context."
    case SampleLayoutMissing(source, line, block) =>
      s"ASC source='$source' line=$line block=$block sample has no parsed SAMPLES layout."
    case UnexpectedFieldCount(source, line, block, expected, actual, convention) =>
      s"ASC source='$source' line=$line block=$block sample convention=$convention has fields=$actual, expected=$expected."
    case NonAsciiField(source, line, block, index) =>
      s"ASC source='$source' line=$line block=$block sample field index=$index is not ASCII."
    case InvalidDecimal(source, line, block, field, index, value) =>
      s"ASC source='$source' line=$line block=$block sample field=$field index=$index has invalid decimal='$value'."
    case NegativeTimestamp(source, line, block, value) =>
      s"ASC source='$source' line=$line block=$block sample timestamp=$value is negative."
    case PartialMissingPair(source, line, block, field, x, y) =>
      s"ASC source='$source' line=$line block=$block sample field=$field is partially missing: x='$x', y='$y'."
    case NegativePupil(source, line, block, eye, value) =>
      s"ASC source='$source' line=$line block=$block eye=$eye has negative pupil='$value'; zero has a distinct native-missing case."
    case InvalidPupil(source, line, block, eye, value) =>
      s"ASC source='$source' line=$line block=$block eye=$eye has invalid pupil decimal='$value'."
    case InvalidInteger(source, line, block, field, index, value) =>
      s"ASC source='$source' line=$line block=$block sample field=$field index=$index has invalid integer='$value'."
    case PupilRepresentationUndeclared(source, line, block) =>
      s"ASC source='$source' line=$line block=$block has no PUPIL representation declaration; native pupil remains Unspecified."
    case NonIncreasingTimestamp(source, line, block, previous, current) =>
      s"ASC source='$source' line=$line block=$block sample timestamp=$current is not later than previous=$previous."

end AscSampleDiagnostic

final class AscSampleParseResult private[io] (
    val line: AscBlockLine,
    val sample: Option[AscNativeSample],
    val diagnostics: Vector[AscSampleDiagnostic]
):
  def hasErrors: Boolean = diagnostics.exists(_.severity == AscDiagnosticSeverity.Error)

enum AscSampleEmission:
  case Parsed(value: AscSampleParseResult)
  case Preserved(value: AscBlockEmission)

object EyeLinkAscSamples:

  private final case class State(lastByBlock: Map[Long, BigDecimal])

  /** Parse samples and validate timestamp order independently within each
    * recording block. All non-sample block emissions are preserved.
    */
  val machine: Machine[AscBlockEmission, AscSampleEmission] =
    Machine(
      new Detector[State, AscBlockEmission, AscSampleEmission]:
        def init: State = State(Map.empty)

        def step(
            state: State,
            input: AscBlockEmission
        ): (State, Vector[AscSampleEmission]) =
          input match
            case AscBlockEmission.Closed(value) =>
              (
                State(state.lastByBlock - value.block.start.blockNumber),
                Vector(AscSampleEmission.Preserved(input))
              )
            case AscBlockEmission.Line(line) =>
              line.line.record match
                case AscRecord.Sample(_) =>
                  val parsed = parse(line)
                  parsed.sample match
                    case None         => (state, Vector(AscSampleEmission.Parsed(parsed)))
                    case Some(sample) =>
                      state.lastByBlock.get(sample.blockNumber) match
                        case Some(previous) if sample.timestamp <= previous =>
                          val diagnostic = AscSampleDiagnostic.NonIncreasingTimestamp(
                            sample.source.source,
                            sample.source.number,
                            sample.blockNumber,
                            AscDiagnosticText.bounded(previous.toString),
                            AscDiagnosticText.bounded(sample.timestamp.toString)
                          )
                          val invalid = new AscSampleParseResult(
                            line,
                            None,
                            parsed.diagnostics :+ diagnostic
                          )
                          (state, Vector(AscSampleEmission.Parsed(invalid)))
                        case _ =>
                          (
                            State(
                              state.lastByBlock.updated(sample.blockNumber, sample.timestamp)
                            ),
                            Vector(AscSampleEmission.Parsed(parsed))
                          )
                case _ => (state, Vector(AscSampleEmission.Preserved(input)))

        def flush(state: State): Vector[AscSampleEmission] = Vector.empty
    )

  /** Stateless row parsing. Use [[machine]] when timestamp-order evidence is
    * required.
    */
  def parse(line: AscBlockLine): AscSampleParseResult =
    val source = line.line.source
    line.line.record match
      case AscRecord.Sample(fields) =>
        val inherited = line.requirements.map(AscSampleDiagnostic.BlockRequirement.apply)
        line.block match
          case None =>
            new AscSampleParseResult(
              line,
              None,
              inherited :+ AscSampleDiagnostic.SampleBlockMissing(source.source, source.number)
            )
          case Some(block) =>
            block.configuration.samples match
              case None =>
                new AscSampleParseResult(
                  line,
                  None,
                  inherited :+ AscSampleDiagnostic.SampleLayoutMissing(
                    source.source,
                    source.number,
                    block.start.blockNumber
                  )
                )
              case Some(layout)
                  if inherited.exists(_.severity == AscDiagnosticSeverity.Error) =>
                new AscSampleParseResult(line, None, inherited)
              case Some(layout) => parseConfigured(line, fields, block, layout, inherited)
      case other =>
        new AscSampleParseResult(
          line,
          None,
          Vector(
            AscSampleDiagnostic.NotSample(
              source.source,
              source.number,
              recordKind(other)
            )
          )
        )

  def expectedFieldCount(layout: AscDataLayout): Int =
    val eyes = layout.eyeLayout match
      case Some(AscEyeLayout.Binocular) => 2
      case Some(_)                      => 1
      case None                         => 0
    1 + eyes * 3 +
      (if layout.optionalColumns.contains(AscOptionalColumn.Velocity) then eyes * 2 else 0) +
      (if layout.optionalColumns.contains(AscOptionalColumn.Resolution) then 2 else 0) +
      (if layout.optionalColumns.contains(AscOptionalColumn.Input) then 1 else 0) +
      (if layout.optionalColumns.contains(AscOptionalColumn.Buttons) then 1 else 0) +
      (if layout.optionalColumns.contains(AscOptionalColumn.Status) then 1 else 0) +
      (if layout.optionalColumns.contains(AscOptionalColumn.HeadTarget) then 4 else 0)

  private def parseConfigured(
      line: AscBlockLine,
      fields: AscFields,
      block: AscBlockSnapshot,
      layout: AscDataLayout,
      inherited: Vector[AscSampleDiagnostic]
  ): AscSampleParseResult =
    val source      = line.line.source
    val blockNumber = block.start.blockNumber
    val values      = fields.tokens.map(_.ascii)
    val diagnostics = Vector.newBuilder[AscSampleDiagnostic]
    diagnostics ++= inherited
    values.zipWithIndex.foreach {
      case (None, index) =>
        diagnostics += AscSampleDiagnostic.NonAsciiField(
          source.source,
          source.number,
          blockNumber,
          index
        )
      case _ => ()
    }
    val expected = expectedFieldCount(layout)
    if values.length != expected then
      diagnostics += AscSampleDiagnostic.UnexpectedFieldCount(
        source.source,
        source.number,
        blockNumber,
        expected,
        values.length,
        AscSampleLayoutConvention.Edf2AscCanonicalV1
      )
    if values.exists(_.isEmpty) || values.length != expected then
      new AscSampleParseResult(line, None, diagnostics.result())
    else
      val tokens    = values.flatten
      val cursor    = new Cursor(source, blockNumber, tokens, diagnostics)
      val timestamp = cursor.decimal("timestamp")
      timestamp.foreach { value =>
        if value < 0 then
          diagnostics += AscSampleDiagnostic.NegativeTimestamp(
            source.source,
            source.number,
            blockNumber,
            AscDiagnosticText.bounded(value.toString)
          )
      }
      val representation = block.configuration.pupil.getOrElse {
        diagnostics += AscSampleDiagnostic.PupilRepresentationUndeclared(
          source.source,
          source.number,
          blockNumber
        )
        AscPupilRepresentation.Unspecified
      }

      def eye(value: AscRecordedEye): Option[AscNativeEyeSample] =
        val position = cursor.pair(s"$value coordinates")
        val pupil    = cursor.pupil(value, representation)
        for
          coordinates <- position
          pupilValue  <- pupil
          mode        <- layout.coordinateMode
        yield new AscNativeEyeSample(
          value,
          new AscNativeCoordinates(mode, coordinates),
          pupilValue,
          None
        )

      val eyeOrder = layout.eyeLayout match
        case Some(AscEyeLayout.Left)      => Vector(AscRecordedEye.Left)
        case Some(AscEyeLayout.Right)     => Vector(AscRecordedEye.Right)
        case Some(AscEyeLayout.Binocular) => Vector(AscRecordedEye.Left, AscRecordedEye.Right)
        case None                         => Vector.empty
      val baseEyes   = eyeOrder.map(recordedEye => recordedEye -> eye(recordedEye))
      val velocities =
        if layout.optionalColumns.contains(AscOptionalColumn.Velocity) then
          eyeOrder
            .map(recordedEye => recordedEye -> cursor.pair(s"$recordedEye velocity"))
            .toMap
        else Map.empty[AscRecordedEye, Option[AscNativePair]]
      val completedEyes = baseEyes.map { case (recordedEye, base) =>
        if layout.optionalColumns.contains(AscOptionalColumn.Velocity) then
          for
            sample   <- base
            velocity <- velocities.getOrElse(recordedEye, None)
          yield new AscNativeEyeSample(
            sample.eye,
            sample.coordinates,
            sample.pupil,
            Some(velocity)
          )
        else base
      }

      val resolution = Option
        .when(
          layout.optionalColumns.contains(AscOptionalColumn.Resolution)
        )(cursor.pair("resolution"))
        .flatten
      val input = Option
        .when(layout.optionalColumns.contains(AscOptionalColumn.Input))(
          cursor.integer("input")
        )
        .flatten
      val buttons = Option
        .when(layout.optionalColumns.contains(AscOptionalColumn.Buttons))(
          cursor.integer("buttons")
        )
        .flatten
      val status = Option
        .when(layout.optionalColumns.contains(AscOptionalColumn.Status))(
          cursor.integer("status")
        )
        .flatten
      val headTarget =
        if layout.optionalColumns.contains(AscOptionalColumn.HeadTarget) then
          for
            position <- cursor.pair("head-target position")
            distance <- cursor.scalar("head-target distance")
            flags    <- cursor.raw("head-target flags")
          yield new AscHeadTarget(position, distance, flags)
        else None

      val allDiagnostics = diagnostics.result()
      val hasErrors      = allDiagnostics.exists(_.severity == AscDiagnosticSeverity.Error)
      val sample         =
        if hasErrors || timestamp.exists(_ < 0) then None
        else
          timestamp.map(value =>
            new AscNativeSample(
              AscSampleLayoutConvention.Edf2AscCanonicalV1,
              AscDotGazePolicy.MissingNativeNotBlink,
              AscZeroPupilPolicy.MissingNative,
              source,
              blockNumber,
              value,
              completedEyes.flatten.find(_.eye == AscRecordedEye.Left),
              completedEyes.flatten.find(_.eye == AscRecordedEye.Right),
              resolution,
              input,
              buttons,
              status,
              headTarget
            )
          )
      new AscSampleParseResult(line, sample, allDiagnostics)

  private final class Cursor(
      source: AscSourceLine,
      block: Long,
      values: Vector[String],
      diagnostics: scala.collection.mutable.Builder[AscSampleDiagnostic, Vector[
        AscSampleDiagnostic
      ]]
  ):
    private var index = 0

    def raw(field: String): Option[String] =
      val at = index
      index += 1
      values.lift(at) match
        case value @ Some(_) => value
        case None            =>
          diagnostics += AscSampleDiagnostic.InvalidDecimal(
            source.source,
            source.number,
            block,
            field,
            at,
            "<absent>"
          )
          None

    def decimal(field: String): Option[BigDecimal] =
      val at = index
      raw(field).flatMap(value =>
        Try(BigDecimal(value)).toOption match
          case value @ Some(_) => value
          case None            =>
            diagnostics += AscSampleDiagnostic.InvalidDecimal(
              source.source,
              source.number,
              block,
              field,
              at,
              AscDiagnosticText.bounded(value)
            )
            None
      )

    def scalar(field: String): Option[AscNativeScalar] =
      val at = index
      raw(field).flatMap {
        case "."   => Some(AscNativeScalar.MissingDot)
        case value =>
          Try(BigDecimal(value)).toOption match
            case Some(parsed) => Some(AscNativeScalar.Value(parsed))
            case None         =>
              diagnostics += AscSampleDiagnostic.InvalidDecimal(
                source.source,
                source.number,
                block,
                field,
                at,
                AscDiagnosticText.bounded(value)
              )
              None
      }

    def pair(field: String): Option[AscNativePair] =
      val xIndex = index
      val x      = raw(s"$field x")
      val yIndex = index
      val y      = raw(s"$field y")
      (x, y) match
        case (Some("."), Some("."))    => Some(AscNativePair.MissingDot)
        case (Some("."), Some(yValue)) =>
          diagnostics += AscSampleDiagnostic.PartialMissingPair(
            source.source,
            source.number,
            block,
            field,
            ".",
            AscDiagnosticText.bounded(yValue)
          )
          None
        case (Some(xValue), Some(".")) =>
          diagnostics += AscSampleDiagnostic.PartialMissingPair(
            source.source,
            source.number,
            block,
            field,
            AscDiagnosticText.bounded(xValue),
            "."
          )
          None
        case (Some(xValue), Some(yValue)) =>
          val parsedX = parseDecimal(field, xIndex, xValue)
          val parsedY = parseDecimal(field, yIndex, yValue)
          for xDecimal <- parsedX; yDecimal <- parsedY
          yield AscNativePair.Values(xDecimal, yDecimal)
        case _ => None

    def pupil(
        eye: AscRecordedEye,
        representation: AscPupilRepresentation
    ): Option[AscNativePupil] =
      raw(s"$eye pupil").flatMap {
        case "."   => Some(new AscNativePupil(representation, AscPupilObservation.MissingDot))
        case value =>
          Try(BigDecimal(value)).toOption match
            case Some(parsed) if parsed == 0 =>
              Some(new AscNativePupil(representation, AscPupilObservation.MissingZero))
            case Some(parsed) if parsed > 0 =>
              Some(new AscNativePupil(representation, AscPupilObservation.Measured(parsed)))
            case Some(_) =>
              diagnostics += AscSampleDiagnostic.NegativePupil(
                source.source,
                source.number,
                block,
                eye,
                AscDiagnosticText.bounded(value)
              )
              None
            case None =>
              diagnostics += AscSampleDiagnostic.InvalidPupil(
                source.source,
                source.number,
                block,
                eye,
                AscDiagnosticText.bounded(value)
              )
              None
      }

    def integer(field: String): Option[AscNativeInteger] =
      val at = index
      raw(field).flatMap {
        case "."   => Some(AscNativeInteger.MissingDot)
        case value =>
          Try(BigInt(value)).toOption match
            case Some(parsed) => Some(AscNativeInteger.Value(parsed))
            case None         =>
              diagnostics += AscSampleDiagnostic.InvalidInteger(
                source.source,
                source.number,
                block,
                field,
                at,
                AscDiagnosticText.bounded(value)
              )
              None
      }

    private def parseDecimal(field: String, at: Int, value: String): Option[BigDecimal] =
      Try(BigDecimal(value)).toOption match
        case parsed @ Some(_) => parsed
        case None             =>
          diagnostics += AscSampleDiagnostic.InvalidDecimal(
            source.source,
            source.number,
            block,
            field,
            at,
            AscDiagnosticText.bounded(value)
          )
          None

  private def recordKind(record: AscRecord): String = record match
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

end EyeLinkAscSamples
