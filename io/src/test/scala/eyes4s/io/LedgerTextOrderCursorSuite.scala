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

import eyes4s.design.SampleQuantum

class LedgerTextOrderCursorSuite extends munit.FunSuite:
  import LedgerExecutionLimitFixtures.*
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val expected                          =
    LedgerResourceLocation(LedgerResourceSource.ExpectedLedger, Some(2), Some(1))
  private val actual = LedgerResourceLocation(LedgerResourceSource.Primary, Some(2), Some(1))
  private def start(left: String, right: String) = get(
    LedgerTextOrderCursor.start(left, right, limits(), expected, actual)
  )
  private def drain(initial: LedgerTextOrderCursor, budget: Int): (Int, Long) =
    val q = get(SampleQuantum.of(budget))
    @annotation.tailrec
    def loop(cursor: LedgerTextOrderCursor, total: Long): (Int, Long) = cursor.advance(q) match
      case LedgerTextOrderStep.More(units, next) =>
        assert(units > 0 && units <= budget)
        loop(next, total + units)
      case LedgerTextOrderStep.Done(units, order) =>
        assert(units > 0 && units <= budget)
        (order, total + units)
    loop(initial, 0)

  test(
    "exhaustive short texts agree with String.compareTo, including NUL and surrogate code units"
  ) {
    val alphabet = Vector('\u0000', 'a', '\ud800', '\udfff')
    val levels   = (0 until 3).scanLeft(Vector(""))((previous, _) =>
      previous.flatMap(prefix => alphabet.map(c => prefix + c))
    )
    val values = levels.flatten
    for left <- values; right <- values do
      val initial = start(left, right)
      val one     = drain(initial, 1)
      assertEquals(one._1, left.compareTo(right))
      Vector(2, 7).foreach(b => assertEquals(drain(initial, b), one))
  }

  test("large equal prefixes yield and stop at the first unequal code unit or end") {
    val prefix   = "p" * 20000
    val examples =
      Vector((prefix, prefix, 0), (prefix + "a", prefix + "b", -1), (prefix, prefix + "a", -1))
    examples.foreach { (left, right, order) =>
      Vector(1, 17, 1024).foreach(b =>
        assertEquals(drain(start(left, right), b), (order, 20001L))
      )
    }
    assertEquals(drain(start("a" + prefix, "b" + prefix), 1), (-1, 1L))
    val initial = start(prefix, prefix)
    val q       = get(SampleQuantum.of(1))
    assertEquals(initial.advance(q), initial.advance(q))
    assertEquals(initial.index, 0)
  }

  test("both field bounds are checked before comparison and refusals locate the operand") {
    val envelope = limits(LedgerResource.FieldCodeUnits -> 2L)
    assert(LedgerTextOrderCursor.start("aa", "aa", envelope, expected, actual).isRight)
    assertEquals(
      LedgerTextOrderCursor.start("aaa", "bbb", envelope, expected, actual),
      Left(LedgerResourceError.Exceeded(LedgerResource.FieldCodeUnits, 2, BigInt(3), expected))
    )
    assertEquals(
      LedgerTextOrderCursor.start("aa", "bbb", envelope, expected, actual),
      Left(LedgerResourceError.Exceeded(LedgerResource.FieldCodeUnits, 2, BigInt(3), actual))
    )
  }
