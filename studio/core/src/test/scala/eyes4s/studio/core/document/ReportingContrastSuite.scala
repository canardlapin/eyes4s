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

package eyes4s.studio.core.document

import eyes4s.codec.CodecError
import eyes4s.studio.core.command.{Command, CommandJournal, JournalEntry, JournalLine}
import io.circe.syntax.*
import io.circe.Json

/** Persisted subtraction direction cannot come from row or label order. */
class ReportingContrastSuite extends munit.FunSuite:
  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private val id                           = ok(ReportingId.of("direction"))
  private val response                     = ok(Covariate.of("response"))
  private val forward                      = ok(ReportingContrast.of("Remembered", "Forgotten"))
  private val reverse                      = ok(ReportingContrast.of("Forgotten", "Remembered"))

  private def spec(contrast: Option[ReportingContrast]) = ok(
    ReportingSpec.of(
      id,
      "Direction",
      Some(response),
      Vector.empty,
      None,
      ReportingWeight.ParticipantMeans,
      contrast
    )
  )

  private def document(reporting: ReportingSpec) =
    val d = DocumentSamples.t1
    ok(
      StudioDocument.of(
        d.datasets,
        d.analyses,
        d.draft,
        d.runs,
        Vector(
          ok(
            ReportingSpec.of(
              d.reporting.head.id,
              reporting.name,
              reporting.groupBy,
              reporting.filters,
              reporting.minimumPerGroup,
              reporting.weighting,
              reporting.contrast
            )
          )
        ),
        d.figures,
        d.presentation,
        d.jobs
      )
    )

  test("operands are nonblank and distinct, and a contrast needs grouping") {
    assert(ReportingContrast.of(" ", "Forgotten").isLeft)
    assert(ReportingContrast.of("Remembered", "").isLeft)
    assertEquals(
      ReportingContrast.of("Remembered", "Remembered"),
      Left(DocumentError.ContrastOperands("Remembered", "Remembered"))
    )
    assert(
      ReportingSpec
        .of(
          id,
          "Direction",
          None,
          Vector.empty,
          None,
          ReportingWeight.ParticipantMeans,
          Some(forward)
        )
        .isLeft
    )
    assert(
      io.circe.parser
        .decode[ReportingContrast]("""{"minuend":"Remembered","subtrahend":"Remembered"}""")
        .isLeft
    )
  }

  test("spec version 2 pins direction while old grouped specs stay without a contrast") {
    val ladder  = ok(ReportingSpec.ladder)
    val legacy  = spec(None)
    val next    = spec(Some(forward))
    val encoded = ok(ladder.codec.encode(next))
    assertEquals(encoded.hcursor.downField("schema").get[Int]("version"), Right(2))
    assertEquals(
      encoded.hcursor.downField("value").get[Json]("contrast"),
      Right(
        Json.obj(
          "minuend"    -> Json.fromString("Remembered"),
          "subtrahend" -> Json.fromString("Forgotten")
        )
      )
    )
    assertEquals(ladder.codec.decode(encoded), Right(next))
    val old = ok(ladder.codec.encode(legacy))
    assertEquals(old.hcursor.downField("schema").get[Int]("version"), Right(1))
    assert(!old.hcursor.downField("value").downField("contrast").succeeded)
    assertEquals(ladder.lift(old).flatMap(ladder.codec.decode), Right(legacy))
    assert(ladder.readAt(ladder.versions.head, next.asJson).isLeft)
    assert(ladder.writeAt(ladder.versions.head, next).isLeft)
    assertEquals(ReportingSpec.digest(legacy), ReportingSpec.digest(spec(None)))
    assertNotEquals(ReportingSpec.digest(next), ReportingSpec.digest(spec(Some(reverse))))
  }

  test("journal version 3 preserves operands and previous envelopes cannot drop them") {
    val ladder = ok(CommandJournal.ladder)
    val line   =
      JournalLine.Entry(1, JournalEntry.Apply(Command.PutReporting(spec(Some(forward)))))
    val encoded = ok(ladder.codec.encode(line))
    assertEquals(encoded.hcursor.downField("schema").get[Int]("version"), Right(3))
    assertEquals(ladder.codec.decode(encoded), Right(line))
    ladder.versions.take(2).foreach { version =>
      assert(ladder.writeAt(version, line).isLeft)
      assert(ladder.readAt(version, line.asJson).isLeft)
    }
    val legacy = JournalLine.Entry(1, JournalEntry.Apply(Command.PutReporting(spec(None))))
    val old    = ok(ladder.codec.encode(legacy))
    assertEquals(old.hcursor.downField("schema").get[Int]("version"), Right(1))
    assertEquals(ladder.lift(old).flatMap(ladder.codec.decode), Right(legacy))
  }

  test("document version 6 and science version 2 keep explicit operands through reopen") {
    val next    = document(spec(Some(forward)))
    val encoded = ok(StudioDocument.encode(next))
    assertEquals(encoded.hcursor.downField("schema").get[Int]("version"), Right(6))
    assertEquals(StudioDocument.decode(encoded), Right(next))
    val ladder = ok(StudioDocument.ladder)
    val old    = ok(ladder.upTo(ladder.versions(4)))
    assert(old.codec.decode(encoded).isLeft)
    old.versions.foreach(v => assert(old.writeAt(v, next).isLeft))
    val relabelled = encoded.hcursor
      .downField("schema")
      .downField("version")
      .withFocus(_ => Json.fromInt(5))
      .top
      .get
    assertEquals(
      StudioDocument.decode(relabelled),
      Left(
        CodecError.Unsupported("studio document", "explicit contrast operands need version 6")
      )
    )
    val science       = ok(ScienceContent.codec)
    val storedScience = ok(science.encode(next.science))
    assertEquals(storedScience.hcursor.downField("schema").get[Int]("version"), Right(2))
    assertEquals(science.decode(storedScience), Right(next.science))
    assertNotEquals(
      StudioDocument.scienceDigest(next),
      StudioDocument.scienceDigest(document(spec(Some(reverse))))
    )
    val legacy = document(spec(None))
    assertEquals(
      ok(ladder.lift(ok(StudioDocument.encode(legacy)))).hcursor
        .downField("value")
        .get[Vector[ReportingSpec]]("reporting"),
      Right(legacy.reporting)
    )
    assertEquals(
      ok(science.encode(legacy.science)).hcursor
        .downField("schema")
        .get[Int]("version"),
      Right(1)
    )
  }
