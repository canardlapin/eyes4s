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

import cats.arrow.Category

/** A pure incremental transducer: state in, state and output out.
  *
  * ==One definition, two runtimes==
  *
  * This is the shape that makes offline analysis and online tracking the same
  * code. A detector written as a state machine has no opinion about where its
  * input comes from: [[Machine.runAll]] drives it over a collection in a pure
  * module with no effect dependency, and `eyes4s-fs2` drives the identical
  * value over a live stream. Nothing else in this space offers one
  * implementation for both, and the reason is usually that detection was
  * written as a loop over an array.
  *
  * ==Why flush exists==
  *
  * A detector accumulating a fixation has one in progress when the input ends.
  * `flush` is where it emits. On a finite input the driver always calls it, so
  * the final event is never lost; on an unbounded live stream it never runs,
  * which is correct -- the fixation genuinely has not ended -- and is why
  * `runAll` and a streaming driver agree on finite input only.
  *
  * @tparam S the machine's internal state, existentially hidden by [[Machine]]
  */
trait Detector[S, -I, +O]:
  def init: S

  /** Consume one input, producing the next state and whatever is now complete. */
  def step(s: S, i: I): (S, Vector[O])

  /** Emit anything still in progress when the input ends. */
  def flush(s: S): Vector[O]

object Detector:

  /** Emits its input unchanged. The identity of composition. */
  def identity[A]: Detector[Unit, A, A] =
    new Detector[Unit, A, A]:
      def init: Unit                             = ()
      def step(s: Unit, i: A): (Unit, Vector[A]) = ((), Vector(i))
      def flush(s: Unit): Vector[A]              = Vector.empty

  /** A stateless transformation. */
  def lift[A, B](f: A => B): Detector[Unit, A, B] =
    new Detector[Unit, A, B]:
      def init: Unit                             = ()
      def step(s: Unit, i: A): (Unit, Vector[B]) = ((), Vector(f(i)))
      def flush(s: Unit): Vector[B]              = Vector.empty

  /** Keeps only inputs satisfying a predicate. */
  def filter[A](p: A => Boolean): Detector[Unit, A, A] =
    new Detector[Unit, A, A]:
      def init: Unit                             = ()
      def step(s: Unit, i: A): (Unit, Vector[A]) =
        ((), if p(i) then Vector(i) else Vector.empty)
      def flush(s: Unit): Vector[A] = Vector.empty

end Detector

/** A [[Detector]] with its state type hidden.
  *
  * ==Why the existential==
  *
  * Composition pairs two state types, so a chain of four stages has state
  * `(((A, B), C), D)`. Exposing that would make the type of a pipeline depend
  * on how it was assembled, and every signature accepting a pipeline would have
  * to name a shape nobody cares about. Hiding it also gives composition a
  * well-kinded `Category` instance, which the raw three-parameter `Detector`
  * cannot have.
  */
sealed abstract class Machine[I, O]:
  type S

  /** A `val`, not a `def`, so that `m.S` is a stable path a driver can name
    * without casting. A driver in another module has to hold the state
    * between chunks, and the only honest way to type that is through this
    * machine's own type member.
    */
  val detector: Detector[S, I, O]

  /** Feed one machine's output into another. */
  def andThen[P](that: Machine[O, P]): Machine[I, P] =
    val f = this
    val g = that
    Machine(
      new Detector[(f.S, g.S), I, P]:
        def init: (f.S, g.S) = (f.detector.init, g.detector.init)

        def step(s: (f.S, g.S), i: I): ((f.S, g.S), Vector[P]) =
          val (fs, mid) = f.detector.step(s._1, i)
          val (gs, out) = drive(s._2, mid)
          ((fs, gs), out)

        def flush(s: (f.S, g.S)): Vector[P] =
          // The upstream flush can produce input the downstream must see before
          // it flushes in turn. Getting this order wrong loses the final event
          // of every composed pipeline, silently.
          val (gs, out) = drive(s._2, f.detector.flush(s._1))
          out ++ g.detector.flush(gs)

        private def drive(gs0: g.S, items: Vector[O]): (g.S, Vector[P]) =
          var gs  = gs0
          val buf = Vector.newBuilder[P]
          items.foreach { o =>
            val (ns, out) = g.detector.step(gs, o)
            gs = ns
            buf ++= out
          }
          (gs, buf.result())
    )

  /** Run to completion over a finite input, flushing at the end.
    *
    * Lives here, in a pure module, with no effect dependency.
    */
  def runAll(in: Iterable[I]): Vector[O] =
    var s   = detector.init
    val buf = Vector.newBuilder[O]
    in.foreach { i =>
      val (ns, out) = detector.step(s, i)
      s = ns
      buf ++= out
    }
    buf ++= detector.flush(s)
    buf.result()

end Machine

object Machine:

  def apply[St, I, O](d: Detector[St, I, O]): Machine[I, O] =
    new Machine[I, O]:
      type S = St
      val detector: Detector[St, I, O] = d

  def identity[A]: Machine[A, A]                = Machine(Detector.identity[A])
  def lift[A, B](f: A => B): Machine[A, B]      = Machine(Detector.lift(f))
  def filter[A](p: A => Boolean): Machine[A, A] = Machine(Detector.filter(p))

  /** Composition forms a category.
    *
    * ==Stated as observational equality==
    *
    * The laws hold over the *output sequences* `runAll` produces, not over the
    * machines themselves. They could not hold structurally: composing pairs the
    * state types, so `id.andThen(f)` has state `(Unit, S)` where `f` has `S`.
    * Those are isomorphic and not equal, and no amount of care makes them equal
    * without erasing the composition that the type records.
    *
    * A law suite that compared machines structurally would therefore fail for a
    * correct implementation, and the temptation would be to weaken it until it
    * passed. Comparing behaviour is both true and the thing a caller depends on.
    */
  given Category[Machine] with
    def id[A]: Machine[A, A] = Machine.identity[A]
    def compose[A, B, C](f: Machine[B, C], g: Machine[A, B]): Machine[A, C] =
      g.andThen(f)

end Machine
