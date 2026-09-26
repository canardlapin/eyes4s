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

import scala.compiletime.testing.typeCheckErrors

class EyeLinkAscRecordsSuite extends munit.FunSuite:

  private def ascii(value: String): IArray[Byte] =
    IArray.tabulate(value.length)(index => value.charAt(index).toByte)

  private def line(value: String, number: Long = 1L): AscSourceLine =
    AscSourceLine.of("fixture.asc", number, 0L, ascii(value)).toOption.get

  private def bytes(values: Vector[Byte]): IArray[Byte] = IArray.from(values.toArray)

  test("source-line construction proves location, budget, and terminator invariants") {
    assert(AscSourceLine.of(" ", 1L, 0L, ascii("MSG 1"), AscLineLimit.Default).isLeft)
    assert(AscSourceLine.of("x.asc", 0L, 0L, ascii("MSG 1"), AscLineLimit.Default).isLeft)
    assert(AscSourceLine.of("x.asc", 1L, -1L, ascii("MSG 1"), AscLineLimit.Default).isLeft)
    assert(AscLineLimit.of(0).isLeft)
    val limit = AscLineLimit.of(3).toOption.get
    assertEquals(
      AscSourceLine.of("x.asc", 2L, 9L, ascii("1234"), limit),
      Left(AscSourceLineError.LineTooLong("x.asc", 2L, 3, 4))
    )
    assert(
      AscSourceLine
        .of("x.asc", 2L, 9L, ascii("MSG 1\n"), AscLineLimit.Default)
        .left
        .exists(_.isInstanceOf[AscSourceLineError.EmbeddedLineTerminator])
    )
  }

  test("known record tokens are classified case-insensitively") {
    val examples = Vector(
      "msg 1 payload"        -> classOf[AscRecord.Message],
      "START 1 LEFT SAMPLES" -> classOf[AscRecord.Boundary],
      "end 2"                -> classOf[AscRecord.Boundary],
      "SAMPLES GAZE LEFT"    -> classOf[AscRecord.Configuration],
      "events GAZE LEFT"     -> classOf[AscRecord.Configuration],
      "PRESCALER 1"          -> classOf[AscRecord.Configuration],
      "VPRESCALER 1"         -> classOf[AscRecord.Configuration],
      "SFIX L 1"             -> classOf[AscRecord.NativeEvent],
      "EFIX L 1 2"           -> classOf[AscRecord.NativeEvent],
      "SSACC R 1"            -> classOf[AscRecord.NativeEvent],
      "ESACC R 1 2"          -> classOf[AscRecord.NativeEvent],
      "SBLINK L 1"           -> classOf[AscRecord.NativeEvent],
      "EBLINK L 1 2"         -> classOf[AscRecord.NativeEvent],
      "FIXUPDATE L 1 2"      -> classOf[AscRecord.NativeEvent],
      "SACUPDATE L 1 2"      -> classOf[AscRecord.NativeEvent],
      "BUTTON 1 2"           -> classOf[AscRecord.Button],
      "INPUT 1 2"            -> classOf[AscRecord.Input],
      "LOST_DATA_EVENT 1"    -> classOf[AscRecord.LostData]
    )

    examples.zipWithIndex.foreach { case ((source, expected), index) =>
      val result = EyeLinkAscLexer.parse(line(source, index + 1L))
      assertEquals(result.record.getClass, expected, clue = source)
      assertEquals(result.diagnostics, Vector.empty, clue = source)
    }
  }

  test("numeric-leading lines remain sample candidates for the block parser") {
    val integer = EyeLinkAscLexer.parse(line("1234 100.0 200.0 500"))
    val decimal = EyeLinkAscLexer.parse(line("1234.5 100.0 200.0 500"))
    val invalid = EyeLinkAscLexer.parse(line("1234x 100.0 200.0 500"))

    assert(integer.record.isInstanceOf[AscRecord.Sample])
    assert(decimal.record.isInstanceOf[AscRecord.Sample])
    assert(invalid.record.isInstanceOf[AscRecord.Sample])
  }

  test("blank, comment, and unknown lines are represented rather than dropped") {
    val blank   = EyeLinkAscLexer.parse(line(" \t  "))
    val comment = EyeLinkAscLexer.parse(line("  ** CONVERTED FROM example.edf"))
    val unknown = EyeLinkAscLexer.parse(line("FUTURE 1 alpha beta"))

    assertEquals(blank.record, AscRecord.Blank)
    assert(comment.record.isInstanceOf[AscRecord.Comment])
    unknown.record match
      case AscRecord.Unknown(token, payload) =>
        assertEquals(token.ascii, Some("FUTURE"))
        assertEquals(payload.ascii, Some("1 alpha beta"))
      case other => fail(s"expected unknown record, found=$other")
  }

  test("message payload bytes remain exact after syntactic separators") {
    val utf8 = bytes(
      ascii("MSG\t100\ttrial caf").toVector ++
        Vector(0xc3.toByte, 0xa9.toByte) ++
        ascii(" ").toVector
    )
    val source = AscSourceLine.of("unicode.asc", 7L, 40L, utf8).toOption.get
    val result = EyeLinkAscLexer.parse(source)

    result.record match
      case AscRecord.Message(fields) =>
        assertEquals(fields.token(0).flatMap(_.ascii), Some("100"))
        assertEquals(fields.token(0).get.bytes.toVector, ascii("100").toVector)
        val payload = fields.remainderAfter(0).get.bytes.toVector
        assertEquals(payload.take(6), ascii("trial ").toVector)
        assert(payload.contains(0xc3.toByte))
        assert(payload.contains(0xa9.toByte))
        assertEquals(payload.last, ' '.toByte)
      case other => fail(s"expected message, found=$other")
  }

  test("malformed known records retain their classification and bounded diagnostic") {
    val result = EyeLinkAscLexer.parse(line("ESACC R", 22L))

    result.record match
      case AscRecord.MalformedKnown(
            AscKnownRecord.NativeEvent(AscNativeEventKind.SaccadeEnd),
            fields
          ) =>
        assertEquals(fields.size, 1)
      case other => fail(s"expected malformed ESACC, found=$other")
    assertEquals(result.diagnostics.length, 1)
    assert(result.diagnostics.head.message.contains("source='fixture.asc'"))
    assert(result.diagnostics.head.message.contains("line=22"))
  }

  test("a non-ASCII structural token is retained with an explicit diagnostic") {
    val bytes  = IArray(0xc3.toByte, 0xa9.toByte, ' '.toByte, '1'.toByte)
    val result = EyeLinkAscLexer.parse(
      AscSourceLine.of("non-ascii.asc", 3L, 10L, bytes).toOption.get
    )

    assert(result.record.isInstanceOf[AscRecord.Unknown])
    assertEquals(result.diagnostics.length, 1)
    assert(result.diagnostics.head.isInstanceOf[AscLexicalDiagnostic.InvalidRecordToken])
  }

  test("the explicit encoding policy recognizes only a first-line UTF-8 BOM") {
    val bom   = Vector(0xef.toByte, 0xbb.toByte, 0xbf.toByte)
    val first = AscSourceLine
      .of("bom.asc", 1L, 0L, bytes(bom ++ ascii("MSG 1 ready").toVector))
      .toOption
      .get
    val later = AscSourceLine
      .of("bom.asc", 2L, 20L, bytes(bom ++ ascii("MSG 2 bad").toVector))
      .toOption
      .get
    val decoded = EyeLinkAscLexer.parse(first)
    val invalid = EyeLinkAscLexer.parse(later)

    assertEquals(decoded.encoding, AscEncodingPolicy.PrintableAsciiStructurePreservePayload)
    assert(decoded.record.isInstanceOf[AscRecord.Message])
    assertEquals(first.bytes.take(3).toVector, bom)
    assert(invalid.record.isInstanceOf[AscRecord.Unknown])
    assert(invalid.diagnostics.head.isInstanceOf[AscLexicalDiagnostic.InvalidRecordToken])
  }

  test("LF, CRLF, and unterminated final lines retain their physical delimiter") {
    val lf = AscSourceLine
      .of("line-endings.asc", 1L, 0L, ascii("MSG 1"), terminator = AscLineTerminator.LineFeed)
      .toOption
      .get
    val crlf = AscSourceLine
      .of(
        "line-endings.asc",
        2L,
        6L,
        ascii("MSG 2"),
        terminator = AscLineTerminator.CarriageReturnLineFeed
      )
      .toOption
      .get
    val eof = AscSourceLine.of("line-endings.asc", 3L, 13L, ascii("MSG 3")).toOption.get

    assertEquals(lf.terminator, AscLineTerminator.LineFeed)
    assertEquals(crlf.terminator, AscLineTerminator.CarriageReturnLineFeed)
    assertEquals(eof.terminator, AscLineTerminator.EndOfFile)
    assert(
      Vector(lf, crlf, eof).forall(line =>
        EyeLinkAscLexer.parse(line).record.isInstanceOf[AscRecord.Message]
      )
    )
  }

  test("long message payloads are retained while diagnostic rendering stays bounded") {
    val payload = "x" * 65536
    val source  = line(s"MSG 1 $payload")
    val result  = EyeLinkAscLexer.parse(source)

    result.record match
      case AscRecord.Message(fields) =>
        assertEquals(
          fields.remainderAfter(0).flatMap(_.ascii).map(_.length),
          Some(payload.length)
        )
      case other => fail(s"expected long message, found=$other")
    assert(result.source.diagnosticExcerpt.length <= 163)
  }

  test("hostile byte lines remain total and diagnostic excerpts are bounded") {
    val hostile = Vector(
      IArray.empty[Byte],
      IArray.fill(1000)(0.toByte),
      IArray.fill(1000)(0xff.toByte),
      ascii("MSG"),
      ascii("START"),
      ascii("999999999999999999999999999999x")
    )

    hostile.zipWithIndex.foreach { case (bytes, index) =>
      val source = AscSourceLine.of("hostile.asc", index + 1L, 0L, bytes).toOption.get
      val result = EyeLinkAscLexer.parse(source)
      assertEquals(result.source, source)
      assert(result.source.diagnosticExcerpt.length <= 163)
    }
  }

  test("source-line and token constructors are not public escape hatches") {
    val errors = typeCheckErrors("""
      import eyes4s.io.*
      val line = new AscSourceLine("", 0L, -1L, IArray.empty)
      val token = new AscToken(3, 1, IArray.empty)
      val payload = new AscPayload(IArray.empty)
      val fields = new AscFields(line, Vector(token))
    """)

    assert(errors.nonEmpty)
  }

end EyeLinkAscRecordsSuite
