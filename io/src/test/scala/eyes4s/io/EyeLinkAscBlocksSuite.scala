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

import eyes4s.kernel.Machine

class EyeLinkAscBlocksSuite extends munit.FunSuite:

  private def ascii(value: String): IArray[Byte] =
    IArray.tabulate(value.length)(index => value.charAt(index).toByte)

  private def lines(values: String*): Vector[AscLineResult] =
    values.zipWithIndex.map { case (value, index) =>
      val source = AscSourceLine
        .of(
          "block.asc",
          index + 1L,
          values.take(index).map(_.length + 1L).sum,
          ascii(value),
          terminator = AscLineTerminator.LineFeed
        )
        .toOption
        .get
      EyeLinkAscLexer.parse(source)
    }.toVector

  private def blockLines(output: Vector[AscBlockEmission]): Vector[AscBlockLine] =
    output.collect { case AscBlockEmission.Line(value) => value }

  private def closed(output: Vector[AscBlockEmission]): Vector[AscClosedBlock] =
    output.collect { case AscBlockEmission.Closed(value) => value }

  private def driveChunks[I, O](machine: Machine[I, O], chunks: Vector[Vector[I]]): Vector[O] =
    var state  = machine.detector.init
    val output = Vector.newBuilder[O]
    chunks.foreach(_.foreach { input =>
      val (next, emitted) = machine.detector.step(state, input)
      state = next
      output ++= emitted
    })
    output ++= machine.detector.flush(state)
    output.result()

  private def summary(output: Vector[AscBlockEmission]): Vector[String] =
    output.map {
      case AscBlockEmission.Line(value) =>
        val block = value.block.map { snapshot =>
          val config = snapshot.configuration
          Vector(
            snapshot.start.blockNumber.toString,
            snapshot.start.declaredEyeLayout.toString,
            snapshot.start.declaresSamples.toString,
            snapshot.start.declaresEvents.toString,
            config.revision.toString,
            config.samples.flatMap(_.eyeLayout).toString,
            config.events.flatMap(_.eyeLayout).toString
          ).mkString(":")
        }
        s"line:${value.line.source.number}:$block:${value.requirements.map(_.message)}:${value.outcomes.map(_.message)}"
      case AscBlockEmission.Closed(value) =>
        s"closed:${value.block.start.blockNumber}:${value.reason.getClass.getName}"
    }

  test("monocular left samples-only configuration is explicit") {
    val input = lines(
      "START 100 LEFT SAMPLES",
      "PRESCALER 1",
      "VPRESCALER 1",
      "PUPIL AREA",
      "SAMPLES GAZE LEFT RATE 1000 TRACKING CR FILTER 2 RES VEL INPUT BUTTONS STATUS",
      "101 10 20 30",
      "END 102"
    )
    val output = EyeLinkAscBlocks.machine.runAll(input)
    val sample = blockLines(output)(5)
    val config = sample.block.get.configuration

    assertEquals(sample.requirements, Vector.empty)
    assertEquals(sample.block.get.start.declaredEyeLayout, Some(AscEyeLayout.Left))
    assertEquals(config.samples.flatMap(_.coordinateMode), Some(AscCoordinateMode.Gaze))
    assertEquals(config.samples.flatMap(_.eyeLayout), Some(AscEyeLayout.Left))
    assertEquals(config.samples.flatMap(_.rateHz).map(_.value), Some(BigDecimal(1000)))
    assertEquals(config.pupil, Some(AscPupilRepresentation.Area))
    assertEquals(config.prescaler.map(_.value), Some(BigDecimal(1)))
    assertEquals(config.velocityPrescaler.map(_.value), Some(BigDecimal(1)))
    assertEquals(
      config.samples.map(_.optionalColumns),
      Some(
        Set(
          AscOptionalColumn.Resolution,
          AscOptionalColumn.Velocity,
          AscOptionalColumn.Input,
          AscOptionalColumn.Buttons,
          AscOptionalColumn.Status
        )
      )
    )
    assertEquals(closed(output).length, 1)
    assert(closed(output).head.reason.isInstanceOf[AscBlockClosureReason.EndRecord])
  }

  test("monocular right events-only configuration is explicit") {
    val input = lines(
      "START 1 RIGHT EVENTS",
      "EVENTS HREF RIGHT RATE 500 TRACKING CR FILTER 1 RES",
      "SFIX R 2",
      "EFIX R 2 3",
      "END 4"
    )
    val output = EyeLinkAscBlocks.machine.runAll(input)
    val event  = blockLines(output)(2)
    val layout = event.block.get.configuration.events.get

    assertEquals(event.requirements, Vector.empty)
    assertEquals(layout.eyeLayout, Some(AscEyeLayout.Right))
    assertEquals(layout.coordinateMode, Some(AscCoordinateMode.HeadReference))
    assertEquals(layout.rateHz.map(_.value), Some(BigDecimal(500)))
  }

  test("binocular combined remote layout retains head-target and unknown evidence") {
    val input = lines(
      "START 1 LEFT RIGHT SAMPLES EVENTS",
      "SAMPLES GAZE LEFT RIGHT RATE 2000 TRACKING CR FILTER 2 HTARGET STATUS FUTURE_SAMPLE_FLAG",
      "EVENTS GAZE LEFT RIGHT RATE 2000 TRACKING CR FILTER 2 RES FUTURE_EVENT_FLAG",
      "2 1 2 3 4 5 6",
      "ESACC L 2 3",
      "END 4"
    )
    val output = EyeLinkAscBlocks.machine.runAll(input)
    val sample = blockLines(output)(3)
    val config = sample.block.get.configuration

    assertEquals(sample.requirements, Vector.empty)
    assertEquals(config.samples.flatMap(_.eyeLayout), Some(AscEyeLayout.Binocular))
    assertEquals(config.events.flatMap(_.eyeLayout), Some(AscEyeLayout.Binocular))
    assert(config.samples.exists(_.isRemote))
    assertEquals(config.samples.map(_.unrecognizedTokens), Some(Vector("FUTURE_SAMPLE_FLAG")))
    assertEquals(config.events.map(_.unrecognizedTokens), Some(Vector("FUTURE_EVENT_FLAG")))
  }

  test("records before sufficient configuration carry named requirements") {
    val input = lines(
      "99 1 2 3",
      "START 100 LEFT SAMPLES EVENTS",
      "101 1 2 3",
      "SAMPLES LEFT RATE nope",
      "102 1 2 3",
      "SFIX L 102",
      "END 103"
    )
    val output = blockLines(EyeLinkAscBlocks.machine.runAll(input))

    assert(
      output(0).requirements.exists(_.isInstanceOf[AscBlockRequirement.RecordOutsideBlock])
    )
    assert(
      output(2).requirements.exists(
        _.isInstanceOf[AscBlockRequirement.SampleConfigurationMissing]
      )
    )
    assert(output(3).outcomes.exists(_.isInstanceOf[AscBlockOutcome.InvalidConfigurationValue]))
    assert(
      output(4).requirements.exists(_.isInstanceOf[AscBlockRequirement.CoordinateModeMissing])
    )
    assert(
      output(4).requirements.exists(_.isInstanceOf[AscBlockRequirement.RateEvidenceMissing])
    )
    assert(
      output(5).requirements.exists(
        _.isInstanceOf[AscBlockRequirement.EventConfigurationMissing]
      )
    )
    assert(output.flatMap(_.requirements).forall(_.message.contains("source='block.asc'")))
  }

  test("conflicting configuration evidence and START disagreement are not guessed") {
    val input = lines(
      "START 1 LEFT SAMPLES",
      "SAMPLES GAZE HREF RIGHT RATE 500 RATE 1000 TRACKING CR TRACKING PUPIL FILTER 1 FILTER 2",
      "2 1 2 3",
      "END 3"
    )
    val output            = blockLines(EyeLinkAscBlocks.machine.runAll(input))
    val configurationLine = output(1)
    val sampleLine        = output(2)

    assertEquals(
      configurationLine.block.flatMap(_.configuration.samples).flatMap(_.coordinateMode),
      None
    )
    assertEquals(
      configurationLine.block.flatMap(_.configuration.samples).flatMap(_.rateHz),
      None
    )
    assert(
      configurationLine.outcomes.count(
        _.isInstanceOf[AscBlockOutcome.ConflictingConfigurationValues]
      ) >= 4
    )
    assert(
      configurationLine.outcomes.exists(
        _.isInstanceOf[AscBlockOutcome.StartAndConfigurationEyeMismatch]
      )
    )
    assert(
      sampleLine.requirements.exists(_.isInstanceOf[AscBlockRequirement.CoordinateModeMissing])
    )
    assert(
      sampleLine.requirements.exists(_.isInstanceOf[AscBlockRequirement.RateEvidenceMissing])
    )
  }

  test("multiple blocks change layout without state leakage") {
    val input = lines(
      "START 1 LEFT SAMPLES",
      "SAMPLES GAZE LEFT RATE 1000",
      "2 1 2 3",
      "END 3",
      "START 10 RIGHT SAMPLES",
      "11 4 5 6",
      "SAMPLES HREF RIGHT RATE 250",
      "12 4 5 6",
      "END 13"
    )
    val output             = blockLines(EyeLinkAscBlocks.machine.runAll(input))
    val secondBeforeConfig = output(5)
    val secondAfterConfig  = output(7)

    assertEquals(secondBeforeConfig.block.map(_.start.blockNumber), Some(2L))
    assert(
      secondBeforeConfig.requirements.exists(
        _.isInstanceOf[AscBlockRequirement.SampleConfigurationMissing]
      )
    )
    assertEquals(
      secondAfterConfig.block.flatMap(_.configuration.samples).flatMap(_.eyeLayout),
      Some(AscEyeLayout.Right)
    )
    assertEquals(
      secondAfterConfig.block.flatMap(_.configuration.samples).flatMap(_.coordinateMode),
      Some(AscCoordinateMode.HeadReference)
    )
  }

  test("unexpected end, duplicate configuration, nested start, and truncation are explicit") {
    val input = lines(
      "END 0",
      "SAMPLES GAZE LEFT RATE 1000",
      "START 1 LEFT SAMPLES",
      "SAMPLES GAZE LEFT RATE 1000",
      "SAMPLES GAZE LEFT RATE 500",
      "START 10 RIGHT EVENTS",
      "EVENTS GAZE RIGHT RATE 500"
    )
    val output = EyeLinkAscBlocks.machine.runAll(input)
    val rows   = blockLines(output)
    val closes = closed(output)

    assert(rows(0).outcomes.exists(_.isInstanceOf[AscBlockOutcome.UnexpectedEnd]))
    assert(rows(1).outcomes.exists(_.isInstanceOf[AscBlockOutcome.ConfigurationOutsideBlock]))
    rows(4).outcomes.collectFirst { case value: AscBlockOutcome.DuplicateConfiguration =>
      value
    } match
      case Some(AscBlockOutcome.DuplicateConfiguration(_, _, _, _, previousLine, changed)) =>
        assertEquals(previousLine, 4L)
        assert(changed)
      case _ => fail("expected changed duplicate SAMPLES configuration")
    assertEquals(closes.length, 2)
    assert(closes.head.reason.isInstanceOf[AscBlockClosureReason.ReplacedByNestedStart])
    assertEquals(closes.last.reason, AscBlockClosureReason.EndOfInput)
  }

  test("whole input and arbitrary line chunks are observationally equal through flush") {
    val input = lines(
      "START 1 LEFT RIGHT SAMPLES EVENTS",
      "SAMPLES GAZE LEFT RIGHT RATE 1000 HTARGET",
      "EVENTS GAZE LEFT RIGHT RATE 1000",
      "2 1 2 3 4 5 6",
      "SFIX L 2",
      "START 10 RIGHT SAMPLES",
      "SAMPLES GAZE RIGHT RATE 500",
      "11 1 2 3"
    )
    val whole     = summary(EyeLinkAscBlocks.machine.runAll(input))
    val chunkings = Vector(
      input.map(Vector(_)),
      Vector(input.take(1), input.slice(1, 5), input.drop(5)),
      Vector(Vector.empty, input.take(4), Vector.empty, input.drop(4)),
      Vector(input)
    )

    chunkings.foreach(chunks =>
      assertEquals(summary(driveChunks(EyeLinkAscBlocks.machine, chunks)), whole)
    )
  }

end EyeLinkAscBlocksSuite
