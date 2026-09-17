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

package eyes4s.kernel

import munit.ScalaCheckSuite
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.{forAll, propBoolean}

/** The chunked driver against `runAll`: for any cut of any finite input, a
  * [[MachineCursor]] driven to completion yields exactly what `runAll` yields,
  * and flushes exactly once. `runAll` is the oracle throughout; the cursor is
  * never compared with itself.
  */
class MachineCursorSuite extends ScalaCheckSuite:

  /** Groups consecutive equal values into runs, holding one at the end. */
  private def runs: Machine[Int, (Int, Int)] =
    Machine(
      new Detector[Option[(Int, Int)], Int, (Int, Int)]:
        def init: Option[(Int, Int)]                                                      = None
        def step(s: Option[(Int, Int)], i: Int): (Option[(Int, Int)], Vector[(Int, Int)]) =
          s match
            case Some((v, n)) if v == i => (Some((v, n + 1)), Vector.empty)
            case Some(done)             => (Some((i, 1)), Vector(done))
            case None                   => (Some((i, 1)), Vector.empty)
        def flush(s: Option[(Int, Int)]): Vector[(Int, Int)] = s.toVector
    )

  /** Reports only at flush, so a composite's flush order is observable. */
  private def countAll[A]: Machine[A, Int] =
    Machine(
      new Detector[Int, A, Int]:
        def init: Int                              = 0
        def step(s: Int, i: A): (Int, Vector[Int]) = (s + 1, Vector.empty)
        def flush(s: Int): Vector[Int]             = Vector(s)
    )

  /** The same machine with its flushes counted. */
  private final class Counted[I, O](inner: Machine[I, O]):
    var flushes                = 0
    var steps                  = 0
    val machine: Machine[I, O] = Machine(
      new Detector[inner.S, I, O]:
        def init: inner.S                                = inner.detector.init
        def step(s: inner.S, i: I): (inner.S, Vector[O]) =
          steps += 1
          inner.detector.step(s, i)
        def flush(s: inner.S): Vector[O] =
          flushes += 1
          inner.detector.flush(s)
    )

  private val machines: Vector[(String, Machine[Int, ?])] = Vector(
    "runs"                         -> runs,
    "countAll"                     -> countAll[Int],
    "identity"                     -> Machine.identity[Int],
    "filter evens"                 -> Machine.filter[Int](_ % 2 == 0),
    "runs andThen lift"            -> runs.andThen(Machine.lift[(Int, Int), Int](_._2)),
    "runs andThen countAll"        -> runs.andThen(countAll[(Int, Int)]),
    "runs andThen runs of lengths" ->
      runs.andThen(Machine.lift[(Int, Int), Int](_._2)).andThen(runs)
  )

  private val inputs: Gen[Vector[Int]] =
    Gen.choose(0, 40).flatMap(n => Gen.listOfN(n, Gen.choose(0, 3)).map(_.toVector))

  private def quanta(length: Int): Vector[Int] =
    Vector(1, 2, 3, 7, length + 1, 2 * length + 5).distinct

  /** One machine at every quantum against the `runAll` oracle. */
  private def agrees[O](name: String, m: Machine[Int, O], input: Vector[Int]): Vector[Prop] =
    val array    = IArray.from(input)
    val expected = m.runAll(input)
    quanta(input.size).map { q =>
      val counted = new Counted(m)
      val output  = MachineCursor.complete(MachineCursor.of(counted.machine, array), q)
      Prop(output == expected) :| s"$name at quantum $q: $output != $expected" &&
      Prop(counted.flushes == 1) :| s"$name at quantum $q flushed ${counted.flushes} times" &&
      Prop(counted.steps == input.size) :| s"$name at quantum $q stepped ${counted.steps}"
    }

  property("driving a cursor to completion at any quantum is runAll, flushing exactly once") {
    forAll(inputs) { input =>
      Prop.all(machines.flatMap { case (name, m) => agrees(name, m, input) }*)
    }
  }

  property("every page feeds min(quantum, remaining) inputs and only the last page flushes") {
    forAll(inputs, Gen.choose(1, 9)) { (input, q) =>
      val counted       = new Counted(runs)
      var cursor        = MachineCursor.of(counted.machine, IArray.from(input))
      var pages         = Vector.empty[Int]
      var done          = false
      var flushedOnMore = false
      var output        = Vector.empty[(Int, Int)]
      while !done do
        cursor.advance(q) match
          case MachinePage.More(units, next) =>
            pages :+= units
            flushedOnMore ||= counted.flushes > 0
            cursor = next
          case MachinePage.Done(units, out) =>
            pages :+= units
            output = out
            done = true
      val expectedPages =
        if input.isEmpty then Vector(0)
        else Vector.fill(input.size / q)(q) ++ Vector(input.size % q).filter(_ > 0)
      (pages == expectedPages) :| s"pages $pages != $expectedPages" &&
      (!flushedOnMore) :| "a More page flushed" &&
      (output == runs.runAll(input)) :| "output" &&
      (counted.flushes == 1) :| s"flushes ${counted.flushes}"
    }
  }

  test("empty input: one Done page of zero units that flushes once and yields the flush") {
    val counted = new Counted(countAll[Int])
    MachineCursor.of(counted.machine, IArray.empty[Int]).advance(3) match
      case MachinePage.Done(units, output) =>
        assertEquals((units, output, counted.flushes), (0, Vector(0), 1))
      case other => fail(s"expected Done, got $other")
  }

  // -------------------------------------------------------------------------
  // Deliberate mutants: a driver that flushes twice and one that drops what
  // earlier pages emitted both disagree with runAll, so the property above
  // is sensitive to exactly the faults it is meant to exclude.
  // -------------------------------------------------------------------------

  /** A chunked driver with an injected fault, written against the same detector. */
  private def mutant[I, O](
      m: Machine[I, O],
      input: Vector[I],
      q: Int,
      doubleFlush: Boolean,
      dropOnCut: Boolean
  ): Vector[O] =
    var s        = m.detector.init
    var emitted  = Vector.empty[O]
    var consumed = 0
    while consumed < input.size do
      val until = math.min(consumed + q, input.size)
      val out   = Vector.newBuilder[O]
      (consumed until until).foreach { i =>
        val (ns, o) = m.detector.step(s, input(i))
        s = ns
        out ++= o
      }
      emitted =
        (if dropOnCut && until < input.size then Vector.empty else emitted) ++ out.result()
      consumed = until
    val flushed = m.detector.flush(s)
    emitted ++ flushed ++ (if doubleFlush then m.detector.flush(s) else Vector.empty)

  test("a double-flushing driver and an emission-dropping driver both fail the oracle") {
    val input = Vector(1, 1, 2, 2, 2, 3, 3, 1)
    Vector(runs -> "runs", runs.andThen(countAll[(Int, Int)]) -> "runs andThen countAll")
      .foreach { case (m, name) =>
        val expected = m.runAll(input)
        assertEquals(
          MachineCursor.complete(MachineCursor.of(m, IArray.from(input)), 3),
          expected
        )
        assertNotEquals(
          mutant(m, input, 3, doubleFlush = true, dropOnCut = false),
          expected,
          name
        )
      }
    val expected = runs.runAll(input)
    assertNotEquals(mutant(runs, input, 3, doubleFlush = false, dropOnCut = true), expected)
    // The faults are the only difference: with both off the mutant is the oracle.
    assertEquals(mutant(runs, input, 3, doubleFlush = false, dropOnCut = false), expected)
  }
