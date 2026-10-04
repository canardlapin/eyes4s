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
import eyes4s.kernel.ContentHash

class LedgerTextHashCursorSuite extends munit.FunSuite:
  import LedgerExecutionLimitFixtures.*
  private val at = LedgerResourceLocation(LedgerResourceSource.Primary, Some(2), Some(3))
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private def drain(text: String, budget: Int): (ContentHash, Long) =
    val q = get(SampleQuantum.of(budget))
    @annotation.tailrec
    def loop(cursor: LedgerTextHashCursor, total: Long): (ContentHash, Long) =
      cursor.advance(q) match
        case LedgerTextHashStep.More(units, next) =>
          assert(units > 0 && units <= budget)
          loop(next, total + units)
        case LedgerTextHashStep.Done(units, hash) =>
          assert(units > 0 && units <= budget)
          (hash, total + units)
    loop(get(LedgerTextHashCursor.start(text, limits(), at)), 0)

  test("hash and exact character work are invariant across quanta, including one large field") {
    Vector("", "abc", "A\u0000B", "\ud83d\ude42", "\ud800", "\udfff", "\u03b1\u6f22" * 20000)
      .foreach { text =>
        val expected = (ContentHash.ofString(text), text.length.toLong + 1)
        Vector(1, 2, 17, 1024).foreach(b => assertEquals(drain(text, b), expected))
      }
    assertEquals(drain("\ud83d\ude42", 1)._1.render, "58786a97d4bd9778")
  }

  test("field bounds are checked before hashing and repeated advances preserve snapshots") {
    val text = "\ud83d\ude42"
    assert(
      LedgerTextHashCursor.start(text, limits(LedgerResource.FieldCodeUnits -> 2L), at).isRight
    )
    assertEquals(
      LedgerTextHashCursor.start(text, limits(LedgerResource.FieldCodeUnits -> 1L), at),
      Left(LedgerResourceError.Exceeded(LedgerResource.FieldCodeUnits, 1, BigInt(2), at))
    )
    val cursor = get(LedgerTextHashCursor.start(text, limits(), at))
    val q      = get(SampleQuantum.of(1))
    assertEquals(cursor.advance(q), cursor.advance(q))
    assertEquals(cursor.index, 0)
    assertEquals(cursor.hash, ContentHash.empty)
  }
