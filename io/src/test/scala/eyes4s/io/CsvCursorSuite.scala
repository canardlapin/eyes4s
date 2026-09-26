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

class CsvCursorSuite extends munit.FunSuite:
  private def drain(text: String, budget: Int): Either[TidyCsvError, Vector[Vector[String]]] =
    @annotation.tailrec
    def loop(
        cursor: CsvCursor,
        rows: Vector[Vector[String]]
    ): Either[TidyCsvError, Vector[Vector[String]]] =
      cursor.advance(budget) match
        case Left(error) => Left(error)
        case Right(page) =>
          assert(page.rows.size <= budget)
          page.next match
            case Some(next) =>
              assert(next.index > cursor.index)
              loop(next, rows ++ page.rows)
            case None => Right(rows ++ page.rows)
    loop(CsvCursor.start(text), Vector.empty)

  test("record chunks preserve quoted newlines, escapes, empty fields and Unicode") {
    val rows = Vector(
      Vector("a", "b"),
      Vector("line\nbreak", "a\"b"),
      Vector("", "λ,界"),
      Vector("last", "")
    )
    val encoded = Rfc4180.encode(rows)
    Vector(1, 2, 3, 10).foreach(n => assertEquals(drain(encoded, n), Right(rows)))
    assertEquals(drain(encoded.dropRight(2), 1), Right(rows))
  }

  test("cursor snapshots can be advanced again without changing their result") {
    val cursor = CsvCursor.start("h\n1\n2\n3\n")
    val first  = cursor.advance(2)
    assertEquals(cursor.advance(2), first)
    val next = first.toOption.get.next.get
    assertEquals(next.advance(1), next.advance(1))
    assertEquals(cursor.index, 0)
  }

  test("chunk boundaries preserve the pinned malformed and terminal record behavior") {
    val examples: Vector[(String, Either[TidyCsvError, Vector[Vector[String]]])] = Vector(
      ""            -> Right(Vector.empty),
      "\n"          -> Right(Vector(Vector(""))),
      "a,b"         -> Right(Vector(Vector("a", "b"))),
      "a,b\r\n"     -> Right(Vector(Vector("a", "b"))),
      "\"\""        -> Right(Vector.empty),
      "\"\"\n"      -> Right(Vector(Vector(""))),
      "a,\"b\n"     -> Left(TidyCsvError.UnterminatedQuotedField(5)),
      "\"a\"z"      -> Left(TidyCsvError.MalformedCsv(3, 'z')),
      "a\"b"        -> Left(TidyCsvError.MalformedCsv(1, '"')),
      "x\rY"        -> Right(Vector(Vector("x\rY"))),
      "a\n\"b\"x\n" -> Left(TidyCsvError.MalformedCsv(5, 'x'))
    )
    examples.foreach { (text, expected) =>
      Vector(1, 2, 7).foreach(n => assertEquals(drain(text, n), expected, s"$text / $n"))
      assertEquals(Rfc4180.decode(text), expected)
    }
  }
