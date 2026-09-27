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

package eyes4s.studio.app.plot

import eyes4s.studio.app.text.Format
import eyes4s.studio.core.selection.StudioRef

/** A plot's value source (S4.5a): what it refuses, each refusal naming its
  * operands, and the one formatter the plot and the table both write with.
  */
class PlotSourceSuite extends munit.FunSuite:

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private def id(v: String) = right(ColumnId.of(v))
  private val participant   = PlotColumn(id("participant"), "Participant", ColumnFormat.Label)
  private val d             = PlotColumn(id("d"), "Mean D", ColumnFormat.Signed(2))
  private val n             = PlotColumn(id("n"), "Contributing", ColumnFormat.Count)
  private def p(i: Int): StudioRef       = StudioRef.Participant(s"P$i")
  private def row(i: Int, v: PlotValue*) = PlotRow(p(i), PlotValue.Text(s"P$i") +: v.toVector)
  private def num(v: Double)             = PlotValue.Number(v)

  test("a valid source keeps its rows in order and writes every value in its format") {
    val s = right(
      PlotSource(
        "Participant D",
        Vector(participant, d, n),
        Vector(
          row(17, num(0.38), num(19)),
          row(3, num(-0.081), num(21400)),
          row(5, PlotValue.Missing, num(0))
        )
      )
    )
    assertEquals(s.rows.map(_.ref), Vector(p(17), p(3), p(5)))
    assertEquals(s.cells(0), Some(Vector("P17", "+0.38", "19")))
    assertEquals(s.cells(1), Some(Vector("P3", s"${Format.Minus}0.08", "21,400")))
    assertEquals(s.cells(2), Some(Vector("P5", PlotSource.MissingText, "0")))
    assertEquals(s.rowText(0), Some("Participant P17, Mean D +0.38, Contributing 19"))
    assertEquals(s.text(0, 1), Some("+0.38"))
    // A bad index is a missing value of the lookup, never a throw.
    assertEquals((s.text(3, 0), s.text(0, 3), s.text(-1, 0)), (None, None, None))
    assertEquals((s.cells(3), s.rowText(-1)), (None, None))
    assertEquals(s.rowOf(p(3)), Some(1))
    assertEquals(s.number(1, id("d")), Some(-0.081))
    assertEquals(s.number(2, id("d")), None, "missing is not zero")
  }

  test("a source with no rows is valid and lists nothing") {
    assertEquals(
      right(PlotSource("Empty", Vector(participant), Vector.empty)).rows,
      Vector.empty
    )
  }

  test("every refusal names what it refused") {
    def refused(columns: Vector[PlotColumn], rows: Vector[PlotRow]): PlotSourceError =
      PlotSource("Caption", columns, rows).fold(identity, s => fail(s"accepted $s"))
    val cases: Vector[(PlotSourceError, String)] = Vector(
      refused(Vector.empty, Vector.empty) -> "Caption",
      refused(Vector(d, d), Vector.empty) -> "'d'",
      refused(
        Vector(participant, d),
        Vector(row(1, num(0.1)), row(1, num(0.2)))
      )                                               -> "rows 0 and 1",
      refused(Vector(participant, d), Vector(row(1))) -> "1 values for 2 columns",
      refused(Vector(participant, d), Vector(row(1, num(Double.NaN))))      -> "NaN",
      refused(Vector(participant, d), Vector(row(1, PlotValue.Text("x"))))  -> "text 'x'",
      refused(Vector(participant), Vector(PlotRow(p(1), Vector(num(2.5))))) -> "number 2.5",
      refused(Vector(PlotColumn(id("m"), "M", ColumnFormat.Decimal(-1))), Vector.empty) -> "-1",
      refused(Vector(participant, n), Vector(row(1, num(2.5)))) -> "2.5 is not a whole"
    )
    cases.foreach { (error, operand) =>
      assert(error.message.contains(operand), s"${error.message} does not name $operand")
    }
    assertEquals(
      cases.map(_._1.ordinal).distinct.size,
      cases.size,
      "each refusal is its own case"
    )
    assert(ColumnId.of("  ").isLeft)
  }

  test("a count beyond 2^53 is refused, naming its row, column and value") {
    val beyond = PlotSource("Counts", Vector(participant, n), Vector(row(1, num(1e20))))
    assertEquals(beyond, Left(PlotSourceError.CountOutOfRange(p(1), "n", 1e20)))
    assertEquals(
      PlotSource("Counts", Vector(participant, n), Vector(row(1, num(-1e20)))),
      Left(PlotSourceError.CountOutOfRange(p(1), "n", -1e20))
    )
    beyond.left.foreach(e => assert(e.message.contains("column 'n'"), e.message))
    // 2^53 itself is exact: the table writes the value the plot places.
    val edge = right(
      PlotSource("Counts", Vector(participant, n), Vector(row(1, num(PlotSource.MaxCount))))
    )
    assertEquals(edge.text(0, 1), Some(Format.count(9007199254740992L)))
  }
