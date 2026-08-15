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

import eyes4s.core.*
import eyes4s.kernel.*

import scala.compiletime.testing.typeCheckErrors

class DelimitedSuite extends munit.FunSuite:

  private val validity = ValidityCodebook
    .of(tracked = Set("1"), lost = Set("0"), blink = Set("blink"), offScreen = Set("off"))
    .toOption
    .get

  private val schema = DelimitedSchema
    .of(
      Delimiter.Comma,
      HeaderMode.FirstLine,
      TimeColumn("timestamp", TimestampUnit.Milliseconds),
      PositionColumns("BPOGX", "BPOGY", CoordinateUnit.Pixels),
      ValidityColumn("LPV", validity),
      Some(PupilColumn("LPD", PupilUnit.Arbitrary)),
      markers = Vector("marker"),
      missingTokens = Set("", "NA"),
      columnMissingTokens = Map("LPD" -> Set("0"))
    )
    .toOption
    .get

  private val frame = Frame.screen("adserp-display", 1280, 1024).toOption.get
  private val clock = ClockId("adserp-unix")

  /** Derived conformance excerpt from the public AdSERP pupil-data example.
    *
    * The first native row and column vocabulary come from the project README:
    * https://github.com/kayhan-latifzadeh/AdSERP (MIT; DOI
    * 10.1145/3726302.3730325). Subsequent rows deliberately exercise native
    * missingness, a malformed quote, a bad number, a reordered timestamp, and
    * an off-surface observation. The identical checked fixture lives at
    * `src/test/resources/eyes4s/io/public-sample.csv`.
    */
  private val publicFixture =
    """|timestamp,BPOGX,BPOGY,LPD,LPV,marker
       |1671196599208,676,204,15.85818,1,trial-start
       |1671196599210,680,207,,1,
       |1671196599212,,,,0,
       |1671196599214,not-a-number,210,15.0,1,
       |1671196599216,"broken,213,15.0,1,
       |1671196599209,690,220,14.5,1,late
       |1671196599220,1400,300,14.8,1,off-screen
       |""".stripMargin

  test("SHA-256 uses canonical UTF-8 and standard known-answer vectors") {
    assertEquals(
      Sha256.ofUtf8("").hex,
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    )
    assertEquals(
      Sha256.ofUtf8("abc").hex,
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    )
    assertEquals(
      Sha256.ofUtf8("é").hex,
      "4a99557e4033c3539de2eb65472017cad5f9557f7a0625a09f1c3f6e2ba69c4c"
    )
    assertEquals(
      Sha256.ofUtf8("😀").hex,
      "f0443a342c5ef54783a111b51ba56c938e474c32324d90c3a60c9c8e3a37e2d9"
    )
    assertEquals(
      Sha256.ofUtf8(publicFixture).hex,
      "21604b397c6c8b8a95bb481dcc768582babd9f1923a8425644742c445ba91de3"
    )
  }

  test("the public fixture retains syntax diagnostics and exact physical lines") {
    val raw = Delimited.parse(
      "adserp-derived.csv",
      publicFixture,
      schema,
      Vector("dataset" -> "AdSERP", "doi" -> "10.1145/3726302.3730325")
    )

    assertEquals(raw.header, Vector("timestamp", "BPOGX", "BPOGY", "LPD", "LPV", "marker"))
    assertEquals(raw.rows.map(_.sourceLine), (2 to 8).toVector)
    assertEquals(
      raw.sourceDigest.hex,
      "21604b397c6c8b8a95bb481dcc768582babd9f1923a8425644742c445ba91de3"
    )
    assertEquals(raw.nativeMetadata.head, "dataset" -> "AdSERP")
    assert(
      raw.diagnostics.exists {
        case DelimitedDiagnostic.MalformedQuotedField("adserp-derived.csv", 6, _, _) => true
        case _                                                                       => false
      }
    )
    assert(
      raw.diagnostics.contains(
        DelimitedDiagnostic.WrongFieldCount("adserp-derived.csv", 6, 6, 2)
      )
    )
  }

  test("validation partitions every public-fixture row without hiding failures") {
    val imported = Delimited
      .parse("adserp-derived.csv", publicFixture, schema)
      .validate(frame, clock, Rate.Irregular, Eye.Left)

    assertEquals(imported.acceptedRows.map(_.sourceLine), Vector(2, 3, 4, 8))
    assertEquals(imported.rejectedRows.map(_.row.sourceLine), Vector(5, 6, 7))
    assertEquals(imported.acceptedCount, 4)
    assertEquals(imported.rejectedCount, 3)
    assert(imported.isLosslessPartition)
    assertEquals(imported.recording.map(_.size), Some(4))
    assertEquals(imported.recording.flatMap(_.pupilUnit), Some(PupilUnit.Arbitrary))
    assertEquals(imported.frameSpec, frame.spec)
    assertEquals(imported.clock, clock)
    assertEquals(
      imported.clockSpec,
      DelimitedClockSpec(
        clock,
        TimestampUnit.Milliseconds,
        TimestampRounding.NearestMicrosecond
      )
    )
    assertEquals(
      imported.acceptedRows.head.sample.t,
      Instant.micros(1671196599208000L)
    )
    assertEquals(imported.acceptedRows(2).sample.gaze, Gaze.Lost[Unit2D.Px]())
    assert(
      imported.acceptedRows.last.sample.gaze match
        case Gaze.OffScreen(point) => point == Pt[Unit2D.Px](1400.0, 300.0)
        case _                     => false
    )
    assert(
      imported.rejectedRows.head.diagnostics.exists {
        case DelimitedDiagnostic.InvalidNumber(_, 5, "BPOGX", "not-a-number", "pixels") =>
          true
        case _ => false
      }
    )
    assert(
      imported.rejectedRows.last.diagnostics.contains(
        DelimitedDiagnostic.NonIncreasingTimestamp(
          "adserp-derived.csv",
          7,
          4,
          Instant.micros(1671196599212000L),
          Instant.micros(1671196599209000L)
        )
      )
    )
  }

  test("quoted delimiters, doubled quotes, CRLF, and markers parse deterministically") {
    val source =
      "timestamp,BPOGX,BPOGY,LPD,LPV,marker\r\n" +
        "1,10,20,3,1,\"trial, \"\"one\"\"\"\r\n"
    val imported = Delimited
      .parse("quoted.csv", source, schema)
      .validate(frame, clock, Rate.Irregular, Eye.Left)

    assertEquals(imported.rejectedCount, 0)
    assertEquals(imported.acceptedRows.head.sourceLine, 2)
    assertEquals(
      imported.acceptedRows.head.markers,
      Vector("marker" -> Some("trial, \"one\""))
    )
  }

  test("supplied TSV headers and decimal seconds use the declared unit and rounding") {
    val tsvSchema = DelimitedSchema
      .of(
        Delimiter.Tab,
        HeaderMode.Supplied(Vector("t", "x", "y", "valid")),
        TimeColumn("t", TimestampUnit.Seconds),
        PositionColumns("x", "y", CoordinateUnit.Normalized),
        ValidityColumn("valid", validity),
        None,
        missingTokens = Set("")
      )
      .toOption
      .get
    val unitFrame = Frame.unitSquare("normalized-stimulus").toOption.get
    val imported  = Delimited
      .parse("sample.tsv", "0.0015\t0.25\t0.75\t1", tsvSchema)
      .validate(unitFrame, clock, Rate.Irregular, Eye.Cyclopean)

    assertEquals(imported.acceptedRows.head.sourceLine, 1)
    assertEquals(imported.acceptedRows.head.sample.t, Instant.micros(1500L))
    assertEquals(
      imported.acceptedRows.head.sample.gaze,
      Gaze.Tracked(Pt[Unit2D.Norm](0.25, 0.75), None)
    )
  }

  test("numeric field diagnostics accumulate instead of stopping at the first failure") {
    val source =
      "timestamp,BPOGX,BPOGY,LPD,LPV,marker\n" +
        "bad,bad-x,bad-y,-1,1,"
    val imported = Delimited
      .parse("many-errors.csv", source, schema)
      .validate(frame, clock, Rate.Irregular, Eye.Left)
    val diagnostics = imported.rejectedRows.head.diagnostics

    assertEquals(diagnostics.length, 4)
    assertEquals(
      diagnostics.collect { case DelimitedDiagnostic.InvalidNumber(_, _, column, _, _) =>
        column
      },
      Vector("timestamp", "BPOGX", "BPOGY")
    )
    assert(diagnostics.exists(_.isInstanceOf[DelimitedDiagnostic.InvalidPupil]))
  }

  test("column-specific sentinels do not erase the validity meaning of the same token") {
    val source =
      "timestamp,BPOGX,BPOGY,LPD,LPV,marker\n" +
        "1,,,0,0,"
    val imported = Delimited
      .parse("column-sentinel.csv", source, schema)
      .validate(frame, clock, Rate.Irregular, Eye.Left)

    assertEquals(imported.rejectedCount, 0)
    assertEquals(imported.acceptedRows.head.sample.gaze, Gaze.Lost[Unit2D.Px]())
  }

  test("missing and duplicate headers reject all rows while retaining them") {
    val source   = "timestamp,BPOGX,BPOGX,LPD,LPV,marker\n1,10,20,3,1,start"
    val imported = Delimited
      .parse("bad-header.csv", source, schema)
      .validate(frame, clock, Rate.Irregular, Eye.Left)

    assertEquals(imported.acceptedCount, 0)
    assertEquals(imported.rejectedRows.map(_.row.sourceLine), Vector(2))
    assert(imported.isLosslessPartition)
    assert(imported.diagnostics.exists(_.isInstanceOf[DelimitedDiagnostic.DuplicateHeader]))
    assert(
      imported.diagnostics.contains(
        DelimitedDiagnostic.MissingRequiredColumn(
          "bad-header.csv",
          "BPOGY",
          Vector("timestamp", "BPOGX", "BPOGX", "LPD", "LPV", "marker")
        )
      )
    )
  }

  test("malformed and empty sources remain diagnostic values rather than throws") {
    val hostile = Vector("", "\n", "\"", "a,b\n\"unterminated", "a\u0000b")
    hostile.zipWithIndex.foreach { case (contents, index) =>
      val imported = Delimited
        .parse(s"hostile-$index.csv", contents, schema)
        .validate(frame, clock, Rate.Irregular, Eye.Left)
      assert(imported.recording.isEmpty)
      assert(imported.diagnostics.nonEmpty)
      assert(imported.isLosslessPartition)
    }
  }

  test("schema constructors reject ambiguous meanings and columns") {
    assert(
      ValidityCodebook
        .of(tracked = Set("1"), lost = Set("1"))
        .left
        .exists(_.isInstanceOf[DelimitedSchemaError.AmbiguousValidityToken])
    )
    assert(
      DelimitedSchema
        .of(
          Delimiter.Comma,
          HeaderMode.FirstLine,
          TimeColumn("same", TimestampUnit.Microseconds),
          PositionColumns("same", "y", CoordinateUnit.Pixels),
          ValidityColumn("valid", validity),
          None
        )
        .left
        .exists(_.isInstanceOf[DelimitedSchemaError.DuplicateLogicalColumn])
    )
    assert(
      DelimitedSchema
        .of(
          Delimiter.Comma,
          HeaderMode.FirstLine,
          TimeColumn("t", TimestampUnit.Microseconds),
          PositionColumns("x", "y", CoordinateUnit.Pixels),
          ValidityColumn("valid", validity),
          None,
          missingTokens = Set("0")
        )
        .left
        .exists(_.isInstanceOf[DelimitedSchemaError.MissingValidityOverlap])
    )
  }

  test("law-bearing schema, codebook, digest, and import constructors are private") {
    val errors = typeCheckErrors("""
      import eyes4s.io.*
      import eyes4s.core.*
      import eyes4s.kernel.*
      val codebook = new ValidityCodebook(Set.empty, Set.empty, Set.empty, Set.empty)
      val schema = new DelimitedSchema[Unit2D.Px](
        Delimiter.Comma,
        HeaderMode.FirstLine,
        TimeColumn("", TimestampUnit.Microseconds),
        PositionColumns("", "", CoordinateUnit.Pixels),
        ValidityColumn("", codebook),
        None,
        Vector.empty,
        Set.empty,
        Map.empty
      )
      val digest = new Sha256(IArray.empty[Byte])
    """)
    assert(errors.nonEmpty)
  }

end DelimitedSuite
