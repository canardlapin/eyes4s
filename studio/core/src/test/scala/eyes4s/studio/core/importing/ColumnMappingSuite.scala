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

package eyes4s.studio.core.importing

import eyes4s.plan.{AttributeColumn, AttributeKind}
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.document.*
import io.circe.parser.decode
import io.circe.syntax.*

/** Column roles, declared units, attribute pass-through and presets (ticket
  * S5.2), on the golden fixture's header and the board's header
  * (Data.dc.html, column mapping).
  */
class ColumnMappingSuite extends munit.FunSuite:
  import ColumnRole.*

  private def name(s: String): ColumnName =
    ColumnName.of(s).fold(e => fail(e.message), identity)

  private def ok[A](e: Either[PresetError, A]): A = e.fold(err => fail(err.message), identity)

  private def preview(file: String, text: String): CsvPreview =
    CsvSniffer.sniff(file, text).fold(e => fail(e.message), identity)

  /** The first records of fixtures/studio-golden/fixations.csv, verbatim. */
  val golden: String =
    """|participant,phase,trial,occurrence,ordinal,x,y,onset_ms,duration_ms,sample_count
       |P01,Encoding,enc_01,1,1,570.4,609.3,70,80,40
       |P01,Encoding,enc_01,1,2,571.8,835.1,178,314,157
       |P01,Encoding,enc_01,1,3,1153.5,188.3,520,80,40
       |P01,Encoding,enc_01,1,4,1289.6,704.7,667,298,149
       |P01,Encoding,enc_01,1,5,736.0,420.2,1015,106,53
       |""".stripMargin

  /** The board's fixations.csv (Data.dc.html), with a lab column the
    * studio has no role for.
    */
  val board: String =
    """|Subject,Phase,TrialID,Block,Image,FixNum,FixX,FixY,FixStart,FixDur,NSamples,Pupil
       |P01,enc,enc_01,1,beach-007,1,1030,456,0,198,99,3.1
       |P01,enc,enc_01,1,beach-007,2,812,603,214,262,131,3.2
       |P01,enc,enc_01,1,beach-007,3,930,512,498,341,170,3.0
       |P01,enc,enc_01,1,beach-007,4,1301,388,861,175,87,3.3
       |""".stripMargin

  def roles(d: MappingDraft): Vector[(String, Option[ColumnRole])] =
    d.columns.map((c, choice) => (c.name.value, choice.roleOption))

  // --- sniffing ----------------------------------------------------------------

  test("the golden file sniffs as comma-separated with its ten columns and first records") {
    val p = preview("fixations.csv", golden)
    assertEquals(p.delimiter, Delimiter.Comma)
    assertEquals(p.header.map(_.value).size, 10)
    assertEquals(p.records, 5)
    assertEquals(
      p.column(name("x")).map(_.samples),
      Some(Vector("570.4", "571.8", "1153.5", "1289.6"))
    )
    assertEquals(p.ragged, Vector.empty)
  }

  test("tab and semicolon separators, quotes, CRLF and a byte-order mark") {
    val tsv = preview("f.tsv", "﻿a\tb\tc\r\n1\t\"x\ty\"\t3\r\n4\t5\t6\r\n")
    assertEquals(tsv.delimiter, Delimiter.Tab)
    assertEquals(tsv.header.map(_.value), Vector("a", "b", "c"))
    assertEquals(tsv.column(name("b")).map(_.samples), Some(Vector("x\ty", "5")))
    val semi = preview("f.csv", "a;b\n\"say \"\"hi\"\"\";2\n")
    assertEquals(semi.delimiter, Delimiter.Semicolon)
    assertEquals(semi.column(name("a")).map(_.samples), Some(Vector("say \"hi\"")))
  }

  test("a record of the wrong width is returned as data, never dropped") {
    val p = preview("f.csv", "a,b,c\n1,2,3\n4,5,6\n7,8,9\n10,11,12\n13,14\n")
    assertEquals(p.records, 5)
    assertEquals(p.ragged, Vector(RaggedRecord(5, 3, 2)))
  }

  test("a ragged first record is data: the file still previews, samples stay aligned") {
    val p = preview("f.csv", "a,b,c\n1,2\n4,5,6\n7,8,9\n")
    assertEquals(p.delimiter, Delimiter.Comma)
    assertEquals(p.ragged, Vector(RaggedRecord(1, 3, 2)))
    // The short record shows a blank cell, so record 2's value is the second sample.
    assertEquals(p.column(name("c")).map(_.samples), Some(Vector("", "6", "9")))
  }

  test("sniff errors name the file and what failed") {
    assertEquals(CsvSniffer.sniff("f.csv", ""), Left(SniffError.Empty("f.csv")))
    assertEquals(
      CsvSniffer.sniff("f.csv", "single\n1\n"),
      Left(SniffError.NoDelimiter("f.csv", "single"))
    )
    assertEquals(
      CsvSniffer.sniff("f.csv", "a,,c\n1,2,3\n"),
      Left(SniffError.BlankColumn("f.csv", 2))
    )
    assertEquals(
      CsvSniffer.sniff("f.csv", "a,b,a\n1,2,3\n"),
      Left(SniffError.RepeatedColumn("f.csv", "a", Vector(1, 3)))
    )
    assertEquals(
      CsvSniffer.sniff("f.csv", "a,b\n1,\"open\n"),
      Left(SniffError.UnterminatedQuote("f.csv", 1))
    )
  }

  // --- proposals ------------------------------------------------------------------

  test("the board's header gets the board's roles; the unknown column is an attribute") {
    val d = MappingDraft.proposed(preview("fixations.csv", board))
    assertEquals(
      roles(d),
      Vector(
        "Subject"  -> Some(Participant),
        "Phase"    -> Some(Phase),
        "TrialID"  -> Some(Trial),
        "Block"    -> Some(Occurrence),
        "Image"    -> Some(Item),
        "FixNum"   -> Some(Ordinal),
        "FixX"     -> Some(X),
        "FixY"     -> Some(Y),
        "FixStart" -> Some(Onset),
        "FixDur"   -> Some(Duration),
        "NSamples" -> Some(SampleCount),
        "Pupil"    -> None
      )
    )
  }

  test("time units are never inferred, even from a column named onset_ms") {
    val d = MappingDraft.proposed(preview("fixations.csv", golden))
    assertEquals(d.time, None)
    assertEquals(
      d.issues,
      Vector(
        MappingError.TimeUnitUndeclared("fixations.csv", name("onset_ms"), name("duration_ms"))
      )
    )
    assert(d.resolve.isLeft)
    val declared = d.declare(Some(TimeUnit.Milliseconds))
    assertEquals(
      declared.resolve.map(_.units),
      Right(DeclaredUnits(Some(TimeUnit.Milliseconds)))
    )
  }

  // --- required roles ---------------------------------------------------------------

  test("a missing required role is a typed error naming the columns that could take it") {
    val d = MappingDraft
      .proposed(preview("fixations.csv", golden))
      .declare(Some(TimeUnit.Milliseconds))
      .choose(name("ordinal"), ColumnChoice.Attribute)
      .fold(e => fail(e.message), identity)
    assertEquals(
      d.issues,
      Vector(MappingError.MissingRole("fixations.csv", Ordinal, Vector(name("ordinal"))))
    )
    val message = d.issues.head.message
    assert(message.contains("ordinal role"), message)
    assert(message.contains("unassigned columns: ordinal"), message)
  }

  test("every required role is enforced: ordinal, sample count, positions, times, identity") {
    val all =
      MappingDraft.proposed(preview("fixations.csv", golden)).declare(Some(TimeUnit.Seconds))
    val header   = all.preview.header
    val stripped = header.foldLeft(all)((d, c) =>
      d.choose(c, ColumnChoice.Attribute).fold(e => fail(e.message), identity)
    )
    assertEquals(
      stripped.issues.collect { case MappingError.MissingRole(_, role, _) => role },
      ColumnRole.required
    )
  }

  test("a role on two columns names both columns") {
    val d = MappingDraft
      .proposed(preview("fixations.csv", golden))
      .declare(Some(TimeUnit.Milliseconds))
      .choose(name("sample_count"), ColumnChoice.Role(Ordinal))
      .fold(e => fail(e.message), identity)
    assert(
      d.issues.contains(
        MappingError
          .RepeatedRole("fixations.csv", Ordinal, Vector(name("ordinal"), name("sample_count")))
      ),
      d.issues.toString
    )
  }

  test("a role on a column of the wrong kind names the column, record and value") {
    val d = MappingDraft
      .proposed(preview("fixations.csv", golden))
      .declare(Some(TimeUnit.Milliseconds))
      .choose(name("phase"), ColumnChoice.Role(X))
      .flatMap(_.choose(name("x"), ColumnChoice.Attribute))
      .fold(e => fail(e.message), identity)
    val mismatch = MappingError.ValueMismatch(
      "fixations.csv",
      name("phase"),
      X,
      1,
      "Encoding",
      ValueKind.Number
    )
    assert(d.issues.contains(mismatch), d.issues.toString)
    assertEquals(mismatch.pointsAt, Some(name("phase")))
    assert(mismatch.message.contains("column phase"), mismatch.message)
    assert(mismatch.message.contains("record 1"), mismatch.message)
  }

  test("choosing a role for a column the file lacks is refused, naming it") {
    val d = MappingDraft.proposed(preview("fixations.csv", golden))
    assertEquals(
      d.choose(name("pupil"), ColumnChoice.Role(X)),
      Left(MappingError.UnknownColumn("fixations.csv", name("pupil")))
    )
  }

  // --- attribute pass-through (UI-H) ------------------------------------------------

  test("unknown columns pass through as eyes4s attribute declarations, kept as text") {
    val resolved = MappingDraft
      .proposed(preview("fixations.csv", board))
      .declare(Some(TimeUnit.Milliseconds))
      .resolve
      .fold(es => fail(es.toVector.map(_.message).mkString("; ")), identity)
    assertEquals(resolved.attributes, Vector(AttributeColumn("Pupil", AttributeKind.Text)))
    assertEquals(resolved.mapping.column(Ordinal), Some(name("FixNum")))
    assertEquals(resolved.mapping.column(Item), Some(name("Image")))
    // Every header column is either bound or declared: nothing is dropped.
    assertEquals(
      resolved.mapping.bindings.size + resolved.attributes.size,
      preview("fixations.csv", board).header.size
    )
  }

  // --- presets ----------------------------------------------------------------------------

  test("a preset re-applies to a second file by column name, whatever the column order") {
    // Decisions no suggestion makes: Block is an attribute, not the occurrence.
    val first = MappingDraft
      .proposed(preview("fixations.csv", board))
      .declare(Some(TimeUnit.Milliseconds))
      .choose(name("Block"), ColumnChoice.Attribute)
      .fold(e => fail(e.message), identity)
    val preset =
      ok(first.preset(PresetName.of("Lab EyeLink export").fold(e => fail(e.message), identity)))
    // The second session's export: columns reordered, one more lab column.
    val second =
      """|FixNum,Subject,TrialID,Phase,Block,Image,FixX,FixY,FixStart,FixDur,NSamples,Pupil,Notes
         |1,P02,enc_01,enc,1,street-118,700,500,0,200,100,3.0,ok
         |""".stripMargin
    val applied = preset
      .applyTo(preview("session2.csv", second))
      .fold(es => fail(es.toVector.map(_.message).mkString("; ")), identity)
    val resolved =
      applied.resolve.fold(es => fail(es.toVector.map(_.message).mkString("; ")), identity)
    val original =
      first.resolve.fold(es => fail(es.toVector.map(_.message).mkString("; ")), identity)
    assertEquals(resolved.mapping, original.mapping)
    assertEquals(resolved.mapping.column(Occurrence), None)
    assertEquals(resolved.units, original.units)
    assertEquals(
      resolved.attributes.map(_.name),
      Vector("Block", "Pupil", "Notes")
    )
    // Without the preset the second file gets suggestions and no time unit.
    assertNotEquals(MappingDraft.proposed(applied.preview), applied)
  }

  test("a preset whose column the second file lacks names that column and role") {
    val preset = MappingDraft
      .proposed(preview("fixations.csv", board))
      .declare(Some(TimeUnit.Milliseconds))
      .preset(PresetName.of("lab").fold(e => fail(e.message), identity))
      .fold(e => fail(e.message), identity)
    val renamed = board.replace("NSamples", "Samples")
    assertEquals(
      preset.applyTo(preview("session2.csv", renamed)).left.map(_.toVector),
      Left(
        Vector(
          MappingError.ColumnAbsent(
            "session2.csv",
            MappingOrigin.Preset(preset.name),
            name("NSamples"),
            SampleCount
          )
        )
      )
    )
  }

  test("a preset never binds a role or a column twice; the stored form is checked too") {
    val n      = PresetName.of("dup").fold(e => fail(e.message), identity)
    val twice  = Vector(ColumnBinding(Ordinal, name("a")), ColumnBinding(Ordinal, name("b")))
    val shared =
      Vector(ColumnBinding(Ordinal, name("a")), ColumnBinding(SampleCount, name("a")))
    assertEquals(
      ImportPreset.of(n, twice, None),
      Left(PresetError.RepeatedRole("dup", "ordinal", Vector("a", "b")))
    )
    assertEquals(
      ImportPreset.of(n, shared, None),
      Left(PresetError.RepeatedColumn("dup", "a", Vector("ordinal", "sample count")))
    )
    // A draft with a role on two columns cannot be saved as a preset.
    val repeated = MappingDraft
      .proposed(preview("fixations.csv", golden))
      .choose(name("sample_count"), ColumnChoice.Role(Ordinal))
      .fold(e => fail(e.message), identity)
    assert(repeated.preset(n).isLeft)
    val stored = io.circe.Json.obj(
      "format"   -> ImportPreset.format.asJson,
      "version"  -> ImportPreset.version.asJson,
      "name"     -> "dup".asJson,
      "bindings" -> twice.asJson,
      "time"     -> io.circe.Json.Null
    )
    assert(decode[ImportPreset](stored.noSpaces).isLeft)
  }

  test("presets round-trip through JSON and keep distinct names") {
    val preset = MappingDraft
      .proposed(preview("fixations.csv", golden))
      .declare(Some(TimeUnit.Milliseconds))
      .preset(PresetName.of("golden").fold(e => fail(e.message), identity))
      .fold(e => fail(e.message), identity)
    assertEquals(decode[ImportPreset](preset.asJson.noSpaces), Right(preset))
    val presets = ImportPresets.of(Vector(preset)).fold(e => fail(e.message), identity)
    assertEquals(presets.add(preset), Left(PresetError.RepeatedName("golden")))
    assertEquals(
      presets.find("nope"),
      Left(PresetError.UnknownPreset("nope", Vector("golden")))
    )
    assertEquals(PresetName.of("  "), Left(PresetError.BlankName))
    assert(
      decode[ImportPreset](
        preset.asJson.deepMerge(io.circe.Json.obj("version" -> 9.asJson)).noSpaces
      ).isLeft
    )
  }

  test("a dataset's own mapping re-applies to its file") {
    val d = MappingDraft
      .proposed(preview("fixations.csv", golden))
      .declare(Some(TimeUnit.Milliseconds))
    val resolved = d.resolve.fold(es => fail(es.head.message), identity)
    val again    = MappingDraft
      .ofDataset(d.preview, DatasetRevision(3), resolved.mapping, resolved.units)
      .fold(es => fail(es.head.message), identity)
    assertEquals(again, d)
  }

  // --- trial metadata ------------------------------------------------------------------------

  test("trials.csv: identity roles required, positions refused, extras pass through") {
    val trials =
      """|participant,phase,trial,occurrence,item,display_kind,image_file,response
         |P01,Encoding,enc_01,1,beach-007,image,beach-007.png,
         |""".stripMargin
    val d = TrialMetadataDraft.proposed(preview("trials.csv", trials))
    assertEquals(d.issues, Vector.empty)
    assertEquals(d.attributes.map(_.name), Vector("display_kind", "image_file"))
    assertEquals(
      d.choose(name("display_kind"), ColumnChoice.Role(X)),
      Left(MappingError.RoleNotRead("trials.csv", name("display_kind"), X))
    )
    val noPhase =
      d.choose(name("phase"), ColumnChoice.Attribute).fold(e => fail(e.message), identity)
    assertEquals(
      noPhase.issues,
      Vector(
        MappingError.MissingRole(
          "trials.csv",
          Phase,
          Vector(name("phase"), name("display_kind"), name("image_file"))
        )
      )
    )
  }

  // --- geometry -------------------------------------------------------------------------------

  test("typed geometry parses, and a bad field names itself") {
    val fields = GeometryFields("1920", "1080", "448", "156", "1024", "768", "35")
    assertEquals(fields.parse.map(_.image.render), Right("1024×768 px at (448, 156)"))
    assertEquals(
      fields.set(GeometryField.ImageWidth, "wide").parse,
      Left(GeometryInputError.NotAWholeNumber(GeometryField.ImageWidth, "wide"))
    )
    assert(fields.set(GeometryField.ImageLeft, "1000").parse.isLeft)
    val round = fields.parse.map(GeometryFields.of)
    assertEquals(round, Right(fields))
  }

  test("a read source carries its path, SHA-256 and preview") {
    val bytes = IArray.from(golden.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    val read  = SniffedSource.read(SourceRole.Fixations, "inputs/fixations.csv", bytes)
    assertEquals(read.map(_.preview.file), Right("fixations.csv"))
    assertEquals(read.map(_.bytes), Right(eyes4s.codec.ByteDigest.sha256(bytes)))
    assert(SniffedSource.read(SourceRole.Fixations, "/abs.csv", bytes).isLeft)
  }
