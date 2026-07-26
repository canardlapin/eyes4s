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

package eyes4s.design

import scala.compiletime.testing.typeCheckErrors

class TrialsSuite extends munit.FunSuite:

  final case class Key(subject: String, image: String, occasion: Int) derives CanEqual
  final case class Meta(correct: Boolean, note: Option[String]) derives CanEqual

  private val rows = Vector(
    Trial(Key("s1", "a", 1), Meta(true, Some("first")), 10),
    Trial(Key("s1", "a", 2), Meta(false, None), 20),
    Trial(Key("s2", "b", 1), Meta(true, Some("third")), 30)
  )

  private val trials = Trials(rows)

  test("typed keys and opaque metadata are retained in order") {
    assertEquals(trials.rows, rows)
    assertEquals(trials.size, 3)
    assert(trials.nonEmpty)
    assert(!trials.isEmpty)
  }

  test("empty and singleton construction are total") {
    assertEquals(Trials.empty[Key, Meta, Int].rows, Vector.empty)
    assertEquals(
      Trials.one(Key("s3", "c", 1), Meta(true, None), 42).rows,
      Vector(Trial(Key("s3", "c", 1), Meta(true, None), 42))
    )
  }

  test("indexed lookup is total") {
    assertEquals(trials.get(0), Some(rows.head))
    assertEquals(trials.get(-1), None)
    assertEquals(trials.get(trials.size), None)
  }

  test("filtering is explicit and preserves complete selected rows") {
    val selected = trials.filter(_.key.subject == "s1")
    assertEquals(selected.rows, rows.take(2))

    val occasion = trials.filterKey(_.occasion == 1)
    assertEquals(occasion.rows, Vector(rows(0), rows(2)))
  }

  test("mapV changes only values and preserves cardinality") {
    val mapped = trials.mapV(value => s"value-$value")

    assertEquals(
      mapped.rows,
      rows.map(row => Trial(row.key, row.meta, s"value-${row.value}"))
    )
    assertEquals(mapped.size, trials.size)
  }

  test("traverseV returns complete failed operands and ordered successes") {
    val (successful, failures) =
      trials.traverseV(value => Either.cond(value != 20, value / 10, "rejected twenty"))

    assertEquals(
      successful.rows,
      Vector(
        Trial(rows(0).key, rows(0).meta, 1),
        Trial(rows(2).key, rows(2).meta, 3)
      )
    )
    assertEquals(
      failures,
      Vector(TrialTransformFailure(1, rows(1), "rejected twenty"))
    )
    assertEquals(failures.head.key, rows(1).key)
    assertEquals(successful.size + failures.size, trials.size)
  }

  test("repeated keys remain data for a pairing operation to diagnose") {
    val duplicate = rows(0).copy(value = 99)
    val repeated  = Trials(rows :+ duplicate)

    assertEquals(repeated.size, 4)
    assertEquals(repeated.rows.count(_.key == rows(0).key), 2)
  }

  test("there is no stringly column-selection API") {
    val errors = typeCheckErrors("""
      import eyes4s.design.*
      val trials = Trials.one(("s1", "a"), (), 1)
      trials.filterBy("subject", "s1")
    """)

    assert(errors.nonEmpty, "Trials exposed a string-named column API")
  }

end TrialsSuite
