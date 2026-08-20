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

import eyes4s.kernel.Frame

class EyeLinkAscSamplesSuite extends munit.FunSuite:

  private def ascii(value: String): IArray[Byte] =
    IArray.tabulate(value.length)(index => value.charAt(index).toByte)

  private def inputs(values: String*): Vector[AscLineResult] =
    values.zipWithIndex.map { case (value, index) =>
      EyeLinkAscLexer.parse(
        AscSourceLine
          .of("samples.asc", index + 1L, index * 100L, ascii(value))
          .toOption
          .get
      )
    }.toVector

  private def parsed(values: String*): Vector[AscSampleParseResult] =
    EyeLinkAscBlocks.machine
      .andThen(EyeLinkAscSamples.machine)
      .runAll(inputs(values*))
      .collect { case AscSampleEmission.Parsed(value) => value }

  test("left GAZE samples retain exact values and frame-check pixel materialization") {
    val result = parsed(
      "START 1 LEFT SAMPLES",
      "PUPIL AREA",
      "SAMPLES GAZE LEFT RATE 1000",
      "2.25 10.5 20.25 30",
      "3.25 110 20 31",
      "END 4"
    )
    val inside  = result(0).sample.get.left.get
    val outside = result(1).sample.get.left.get
    val frame   = Frame.screen("display", 100, 100).toOption.get

    assertEquals(result(0).sample.map(_.timestamp), Some(BigDecimal("2.25")))
    assertEquals(inside.coordinates.mode, AscCoordinateMode.Gaze)
    assertEquals(
      inside.coordinates.position,
      AscNativePair.Values(BigDecimal("10.5"), BigDecimal("20.25"))
    )
    assertEquals(inside.pupil.representation, AscPupilRepresentation.Area)
    assertEquals(inside.pupil.observation, AscPupilObservation.Measured(BigDecimal(30)))
    assertEquals(result(0).sample.get.right, None)
    assert(inside.pixelPosition(frame).exists(_.isInstanceOf[AscPixelPosition.InsideFrame]))
    assert(outside.pixelPosition(frame).exists(_.isInstanceOf[AscPixelPosition.OffSurface]))
  }

  test("right dot gaze and zero pupil remain native missing data, never blink") {
    val result = parsed(
      "START 1 RIGHT SAMPLES",
      "PUPIL DIAMETER",
      "SAMPLES GAZE RIGHT RATE 500",
      "2 . . 0",
      "END 3"
    ).head
    val eye   = result.sample.get.right.get
    val frame = Frame.screen("display", 100, 100).toOption.get

    assertEquals(eye.coordinates.position, AscNativePair.MissingDot)
    assertEquals(eye.pupil.representation, AscPupilRepresentation.Diameter)
    assertEquals(eye.pupil.observation, AscPupilObservation.MissingZero)
    assertEquals(eye.pixelPosition(frame), Right(AscPixelPosition.MissingNativeGaze))
    assertEquals(
      result.sample.map(_.dotGazePolicy),
      Some(AscDotGazePolicy.MissingNativeNotBlink)
    )
    assertEquals(result.sample.map(_.zeroPupilPolicy), Some(AscZeroPupilPolicy.MissingNative))
    assertEquals(result.diagnostics, Vector.empty)
  }

  test("binocular samples preserve both eyes and every declared optional field") {
    val result = parsed(
      "START 1 LEFT RIGHT SAMPLES",
      "PUPIL ARBITRARY_VENDOR_UNIT",
      "SAMPLES RAW LEFT RIGHT RATE 2000 VEL RES INPUT BUTTONS STATUS HTARGET",
      "2 1 2 3 4 5 6 0.1 0.2 0.3 0.4 30 31 7 8 9 40 41 42 MANC",
      "END 3"
    ).head.sample.get

    assertEquals(result.left.map(_.eye), Some(AscRecordedEye.Left))
    assertEquals(result.right.map(_.eye), Some(AscRecordedEye.Right))
    assertEquals(
      result.left.flatMap(_.velocity),
      Some(AscNativePair.Values(BigDecimal("0.1"), BigDecimal("0.2")))
    )
    assertEquals(
      result.right.flatMap(_.velocity),
      Some(AscNativePair.Values(BigDecimal("0.3"), BigDecimal("0.4")))
    )
    assertEquals(result.resolution, Some(AscNativePair.Values(BigDecimal(30), BigDecimal(31))))
    assertEquals(result.input, Some(AscNativeInteger.Value(BigInt(7))))
    assertEquals(result.buttons, Some(AscNativeInteger.Value(BigInt(8))))
    assertEquals(result.status, Some(AscNativeInteger.Value(BigInt(9))))
    assertEquals(
      result.headTarget.map(_.position),
      Some(AscNativePair.Values(BigDecimal(40), BigDecimal(41)))
    )
    assertEquals(result.headTarget.map(_.distance), Some(AscNativeScalar.Value(BigDecimal(42))))
    assertEquals(result.headTarget.map(_.flags), Some("MANC"))
    assertEquals(
      result.left.map(_.pupil.representation),
      Some(AscPupilRepresentation.Undocumented("ARBITRARY_VENDOR_UNIT"))
    )
  }

  test("HREF and RAW positions cannot be relabelled as screen pixels") {
    val href = parsed(
      "START 1 LEFT SAMPLES",
      "SAMPLES HREF LEFT RATE 1000",
      "2 1 2 3",
      "END 3"
    ).head.sample.get.left.get
    val raw = parsed(
      "START 1 LEFT SAMPLES",
      "SAMPLES RAW LEFT RATE 1000",
      "2 1 2 3",
      "END 3"
    ).head.sample.get.left.get
    val frame = Frame.screen("display", 100, 100).toOption.get

    assert(
      href
        .pixelPosition(frame)
        .left
        .exists(_.isInstanceOf[AscSampleMaterializationError.CoordinateModeIsNotScreenPixels])
    )
    assert(
      raw
        .pixelPosition(frame)
        .left
        .exists(_.isInstanceOf[AscSampleMaterializationError.CoordinateModeIsNotScreenPixels])
    )
  }

  test("invalid width, partial gaze, negative pupil, and invalid integers are named") {
    val results = parsed(
      "START 1 LEFT SAMPLES",
      "PUPIL AREA",
      "SAMPLES GAZE LEFT RATE 1000 INPUT",
      "2 1 2",
      "3 . 2 4 1",
      "4 1 2 -1 1",
      "5 1 2 3 nope",
      "6 1 2 nope 1",
      "END 7"
    )

    assert(
      results(0).diagnostics.exists(_.isInstanceOf[AscSampleDiagnostic.UnexpectedFieldCount])
    )
    assert(
      results(1).diagnostics.exists(_.isInstanceOf[AscSampleDiagnostic.PartialMissingPair])
    )
    assert(results(2).diagnostics.exists(_.isInstanceOf[AscSampleDiagnostic.NegativePupil]))
    assert(results(3).diagnostics.exists(_.isInstanceOf[AscSampleDiagnostic.InvalidInteger]))
    assert(results(4).diagnostics.exists(_.isInstanceOf[AscSampleDiagnostic.InvalidPupil]))
    assert(results.forall(_.sample.isEmpty))
    assert(results.flatMap(_.diagnostics).forall(_.message.contains("source='samples.asc'")))
  }

  test("timestamp order is strict within a block and resets between blocks") {
    val results = parsed(
      "START 1 LEFT SAMPLES",
      "SAMPLES GAZE LEFT RATE 1000",
      "10 1 2 3",
      "9 1 2 3",
      "END 11",
      "START 1 RIGHT SAMPLES",
      "SAMPLES GAZE RIGHT RATE 1000",
      "1 4 5 6",
      "END 2"
    )

    assert(results(0).sample.nonEmpty)
    assert(results(1).sample.isEmpty)
    assert(
      results(1).diagnostics.exists(_.isInstanceOf[AscSampleDiagnostic.NonIncreasingTimestamp])
    )
    assert(results(2).sample.nonEmpty)
    assertEquals(results(2).sample.map(_.blockNumber), Some(2L))
  }

  test("an invalid binocular eye does not shift later optional columns") {
    val result = parsed(
      "START 1 LEFT RIGHT SAMPLES",
      "PUPIL AREA",
      "SAMPLES GAZE LEFT RIGHT RATE 1000 VEL RES STATUS",
      "2 . 2 3 4 5 6 0.1 0.2 0.3 0.4 30 31 9",
      "END 3"
    ).head

    assert(result.sample.isEmpty)
    assertEquals(
      result.diagnostics.count(_.isInstanceOf[AscSampleDiagnostic.PartialMissingPair]),
      1
    )
    assert(!result.diagnostics.exists(_.isInstanceOf[AscSampleDiagnostic.InvalidInteger]))
    assert(!result.diagnostics.exists(_.isInstanceOf[AscSampleDiagnostic.UnexpectedFieldCount]))
  }

  test("undeclared pupil representation remains Unspecified with a warning") {
    val result = parsed(
      "START 1 LEFT SAMPLES",
      "SAMPLES GAZE LEFT RATE 1000",
      "2 1 2 3",
      "END 3"
    ).head

    assertEquals(
      result.sample.flatMap(_.left).map(_.pupil.representation),
      Some(AscPupilRepresentation.Unspecified)
    )
    assert(
      result.diagnostics.exists(
        _.isInstanceOf[AscSampleDiagnostic.PupilRepresentationUndeclared]
      )
    )
    assert(!result.hasErrors)
  }

  test("generated eye, coordinate, and optional-column combinations have one exact shape") {
    val layouts       = Vector("LEFT", "RIGHT", "LEFT RIGHT")
    val modes         = Vector("GAZE", "HREF", "RAW")
    val optionalNames = Vector("VEL", "RES", "INPUT", "BUTTONS", "STATUS", "HTARGET")
    val flags         = (0 until (1 << optionalNames.length)).map { mask =>
      optionalNames.zipWithIndex
        .collect { case (name, index) if (mask & (1 << index)) != 0 => name }
        .mkString(" ")
    }

    for
      layout <- layouts
      mode   <- modes
      flag   <- flags
    do
      val eyeCount = if layout == "LEFT RIGHT" then 2 else 1
      val fields   = Vector.newBuilder[String]
      fields += "2"
      (0 until eyeCount).foreach(_ => fields ++= Vector("1", "2", "3"))
      if flag.contains("VEL") then (0 until eyeCount).foreach(_ => fields ++= Vector("4", "5"))
      if flag.contains("RES") then fields ++= Vector("6", "7")
      if flag.contains("INPUT") then fields += "8"
      if flag.contains("BUTTONS") then fields += "9"
      if flag.contains("STATUS") then fields += "10"
      if flag.contains("HTARGET") then fields ++= Vector("11", "12", "13", "M")
      val row    = fields.result().mkString(" ")
      val result = parsed(
        s"START 1 $layout SAMPLES",
        "PUPIL AREA",
        s"SAMPLES $mode $layout RATE 1000 $flag",
        row,
        "END 3"
      ).head
      assert(
        result.sample.nonEmpty,
        clue =
          s"layout=$layout mode=$mode flags=$flag row=$row diagnostics=${result.diagnostics.map(_.message)}"
      )
  }

end EyeLinkAscSamplesSuite
