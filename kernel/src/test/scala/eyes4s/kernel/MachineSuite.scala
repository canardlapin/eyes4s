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

class MachineSuite extends munit.FunSuite:

  /** Groups consecutive equal values into runs, emitting each when it ends.
    *
    * A miniature of what an event detector does: accumulate while a condition
    * holds, emit when it stops, and hold one incomplete result at the end.
    */
  private def runsOf[A]: Machine[A, (A, Int)] =
    Machine(
      new Detector[Option[(A, Int)], A, (A, Int)]:
        def init: Option[(A, Int)] = None

        def step(s: Option[(A, Int)], i: A): (Option[(A, Int)], Vector[(A, Int)]) =
          s match
            case Some((v, n)) if v == i => (Some((v, n + 1)), Vector.empty)
            case Some(done)             => (Some((i, 1)), Vector(done))
            case None                   => (Some((i, 1)), Vector.empty)

        def flush(s: Option[(A, Int)]): Vector[(A, Int)] = s.toVector
    )

  private def runs: Machine[Int, (Int, Int)] = runsOf[Int]

  /** Counts everything it sees and reports only at flush. A downstream stage
    * with pending state of its own, which is what makes the flush ORDER
    * observable.
    */
  private def countAll[A]: Machine[A, Int] =
    Machine(
      new Detector[Int, A, Int]:
        def init: Int                              = 0
        def step(s: Int, i: A): (Int, Vector[Int]) = (s + 1, Vector.empty)
        def flush(s: Int): Vector[Int]             = Vector(s)
    )

  private val double = Machine.lift[Int, Int](_ * 2)
  private val evens  = Machine.filter[Int](_ % 2 == 0)

  private val input = List(1, 1, 2, 2, 2, 3)

  // -------------------------------------------------------------------------
  // Running
  // -------------------------------------------------------------------------

  test("a machine accumulates and emits, flushing what is still in progress") {
    assertEquals(runs.runAll(input), Vector((1, 2), (2, 3), (3, 1)))
  }

  test("without the flush the final result would be lost") {
    // Driving by hand, stopping short of flush, loses the last run -- which is
    // exactly the behaviour a live stream has, and why runAll and a streaming
    // driver agree on finite input only.
    val d   = runs.detector
    var s   = d.init
    val out = Vector.newBuilder[(Int, Int)]
    input.foreach { i =>
      val (ns, o) = d.step(s, i)
      s = ns
      out ++= o
    }
    assertEquals(out.result(), Vector((1, 2), (2, 3)))
    assertEquals(d.flush(s), Vector((3, 1)))
  }

  test("an empty input still flushes, and yields nothing") {
    assertEquals(runs.runAll(Nil), Vector.empty)
  }

  // -------------------------------------------------------------------------
  // Composition
  // -------------------------------------------------------------------------

  test("composition feeds one machine's output into the next") {
    val pipeline = evens.andThen(double)
    assertEquals(pipeline.runAll(input), Vector(4, 4, 4))
  }

  test("a composite flushes upstream first, then feeds that through downstream") {
    // The ordering that is easy to get wrong: the upstream flush emits a final
    // run, which the downstream must still see before flushing itself.
    val counted = runs.andThen(Machine.lift[(Int, Int), Int](_._2))
    assertEquals(counted.runAll(input), Vector(2, 3, 1))
  }

  test("a downstream stage sees the upstream flush before flushing itself") {
    // runs emits three runs, the last of them only at flush. countAll reports
    // its tally only at ITS flush. If the composite flushed downstream first,
    // the tally would be 2 -- so this number is the ordering, measured.
    assertEquals(runs.andThen(countAll).runAll(input), Vector(3))
  }

  // -------------------------------------------------------------------------
  // Category laws, observationally
  // -------------------------------------------------------------------------

  test("identity is a left and right unit of composition") {
    val viaLeft  = Machine.identity[Int].andThen(runs)
    val viaRight = runs.andThen(Machine.identity[(Int, Int)])
    assertEquals(viaLeft.runAll(input), runs.runAll(input))
    assertEquals(viaRight.runAll(input), runs.runAll(input))
  }

  test("composition is associative, observationally") {
    val a = evens
    val b = double
    val c = Machine.lift[Int, String](_.toString)

    val left  = a.andThen(b).andThen(c)
    val right = a.andThen(b.andThen(c))
    assertEquals(left.runAll(input), right.runAll(input))
  }

  test("the laws cannot be structural, and this is why") {
    // id.andThen(f) has state (Unit, S) where f has state S. Isomorphic, not
    // equal -- and making them equal would mean erasing the composition the
    // type records. So the laws are stated over behaviour.
    val composed = Machine.identity[Int].andThen(runs)
    assertNotEquals(composed: Any, runs: Any)
    assertEquals(composed.runAll(input), runs.runAll(input))
  }

  test("the Category instance agrees with andThen") {
    val C          = summon[cats.arrow.Category[Machine]]
    val viaCompose = C.compose(double, evens)
    assertEquals(viaCompose.runAll(input), evens.andThen(double).runAll(input))
    assertEquals(C.id[Int].runAll(input), input.toVector)
  }

  // -------------------------------------------------------------------------
  // A four-stage chain, to show the state type stays hidden
  // -------------------------------------------------------------------------

  test("a long chain composes without its state type leaking into the signature") {
    val pipeline: Machine[Int, String] =
      Machine
        .filter[Int](_ > 0)
        .andThen(Machine.lift[Int, Int](_ * 10))
        .andThen(runsOf[Int])
        .andThen(Machine.lift[(Int, Int), String]((v, n) => s"$v x$n"))

    assertEquals(pipeline.runAll(List(1, 1, 2)), Vector("10 x2", "20 x1"))
  }

end MachineSuite
