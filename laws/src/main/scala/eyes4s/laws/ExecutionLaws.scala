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

package eyes4s.laws

import eyes4s.compare.ComparisonQuantum
import eyes4s.design.{PairQuantum, SampleQuantum, WorkQuanta}
import eyes4s.plan.{SegmentTotal, Stepwise, WorkStep}

import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.typelevel.discipline.Laws

/** The execution contract every bounded plan family satisfies, stated over
  * [[eyes4s.plan.Stepwise]] alone so it needs no effect system: a family is
  * conformant when driving its cursor step by step, at any quanta and any
  * sequence of quanta, is deterministic, ends in exactly one terminal step,
  * agrees with the pure run, visits its counted segments contiguously, and
  * states each segment's total once and truthfully. The runner-side half of
  * the contract (one terminal event, cancellation between steps, first commit
  * wins, defects surface) is effectful and lives in the `eyes4s-fs2` tests.
  *
  * A family supplies its [[Family]] evidence and a generator of cursors; the
  * shipped study, recording and temporal families are checked in the module's
  * own suite, which also shows each law killing a deliberate mutant. A
  * downstream family that implements `Stepwise` runs the same rule set.
  *
  * The pure oracle is the family's own run of the plan that produced the
  * cursor (`PreparedStudy.run`, `RecordingPlan.run`,
  * `PreparedTemporalStudy.run` for the shipped families), supplied
  * separately from the `Stepwise` instance under test so that an instance
  * which completes early or differently cannot vouch for itself.
  */
object ExecutionLaws extends Laws:

  /** One family's evidence for the laws: how a stage is counted, what a
    * segment's total is as its first step begins (given the cursor about to
    * take it), the pure run of the plan a generated cursor came from, when
    * two results or two errors are the same, and a step budget within which
    * every generated cursor must end. Results are compared with `sameResult`
    * because scientific results carry provenance and arrays that have no
    * useful universal equality.
    */
  final class Family[C, Stage, Segment, E, R](
      val segment: Stage => Segment,
      val total: (Segment, C) => SegmentTotal,
      val pure: C => Either[E, R],
      val sameResult: (R, R) => Boolean,
      val sameError: (E, E) => Boolean,
      val stepBudget: Int
  )(using val stepwise: Stepwise[C, Stage, E, R])

  /** One recorded step: the stage and segment the runner would report, the
    * step's own units and the total the family stated for the segment from
    * the cursor that took the step.
    */
  final case class Step[Stage, Segment](
      stage: Stage,
      segment: Segment,
      units: Int,
      total: SegmentTotal
  )

  /** A run's trace: every step, the `Done` step included, and its end. `end`
    * is `None` only when the step budget was exhausted before a terminal step.
    */
  final case class Trace[Stage, Segment, E, R](
      steps: Vector[Step[Stage, Segment]],
      end: Option[Either[E, R]]
  ):
    /** Contiguous blocks of one segment, in run order, with their unit sums and stated totals. */
    def blocks: Vector[(Segment, Long, Vector[SegmentTotal])] =
      steps
        .foldLeft(Vector.empty[(Segment, Long, Vector[SegmentTotal])]) { (acc, step) =>
          acc.lastOption match
            case Some((segment, units, totals)) if segment == step.segment =>
              acc.init :+ (segment, units + step.units, totals :+ step.total)
            case _ => acc :+ (step.segment, step.units.toLong, Vector(step.total))
        }

    /** The work each segment block charged, which the laws require to be cut-invariant. */
    def accounting: Vector[(Segment, Long)] =
      blocks.map((segment, units, _) => (segment, units))

  /** Drive a cursor to its end, cutting step `i` with `quanta(i)`, recording
    * exactly what the runner reports: a `More` step's own stage, a `Done`
    * step's stage as the cursor named it before advancing, and each step's
    * segment total from the cursor that took the step.
    */
  def trace[C, Stage, Segment, E, R](
      family: Family[C, Stage, Segment, E, R],
      cursor: C,
      quanta: Int => WorkQuanta
  ): Trace[Stage, Segment, E, R] =
    val steps = Vector.newBuilder[Step[Stage, Segment]]
    @annotation.tailrec
    def loop(cursor: C, index: Int): Option[Either[E, R]] =
      if index >= family.stepBudget then None
      else
        def record(stage: Stage, units: Int): Unit =
          val segment = family.segment(stage)
          steps += Step(stage, segment, units, family.total(segment, cursor))
        family.stepwise.advance(cursor, quanta(index)) match
          case Left(error)                              => Some(Left(error))
          case Right(WorkStep.More(stage, units, next)) =>
            record(stage, units)
            loop(next, index + 1)
          case Right(WorkStep.Done(units, result)) =>
            record(family.stepwise.stage(cursor), units)
            Some(Right(result))
    val end = loop(cursor, 0)
    Trace(steps.result(), end)

  /** The pure oracle: the family's own run of the plan the cursor came from. */
  def oracle[C, Stage, Segment, E, R](
      family: Family[C, Stage, Segment, E, R],
      cursor: C
  ): Either[E, R] = family.pure(cursor)

  /** Quanta drawn from the given positive values, `WorkQuanta.default` included. */
  def quanta(pairs: Seq[Int], comparison: Seq[Int], samples: Seq[Int]): Gen[WorkQuanta] =
    def pick[A](values: Seq[Int], of: Int => Either[?, A], default: A): Gen[A] =
      Gen.oneOf(values.flatMap(v => of(v).toOption) :+ default)
    Gen.oneOf(
      Gen.const(WorkQuanta.default),
      for
        p <- pick(pairs, PairQuantum.of, PairQuantum.default)
        c <- pick(comparison, ComparisonQuantum.of, ComparisonQuantum.default)
        s <- pick(samples, SampleQuantum.of, SampleQuantum.default)
      yield WorkQuanta(p, c, s)
    )

  /** The rule set. `cursors` should cover the family's designated fixtures:
    * fully matched and unmatched schedules, several scales, chunk-spanning
    * events, and whatever else changes the step sequence. `quanta` should
    * include the finest cut of every quantum and the default.
    */
  def conformance[C, Stage, Segment, E, R](
      family: Family[C, Stage, Segment, E, R],
      cursors: Gen[C],
      quanta: Gen[WorkQuanta]
  ): RuleSet =
    new SimpleRuleSet("execution", laws(family, cursors, quanta)*)

  /** The laws as named properties, so a suite can also run each one alone
    * against a mutant and record which laws kill it.
    */
  def laws[C, Stage, Segment, E, R](
      family: Family[C, Stage, Segment, E, R],
      cursors: Gen[C],
      quanta: Gen[WorkQuanta]
  ): Vector[(String, Prop)] =
    val sequences: Gen[Vector[WorkQuanta]] =
      Gen.choose(1, 4).flatMap(n => Gen.listOfN(n, quanta).map(_.toVector))

    // Two exhausted traces have the same (absent) end: termination is the
    // terminal law's concern, determinism compares what was produced.
    def same(a: Option[Either[E, R]], b: Option[Either[E, R]]): Boolean = (a, b) match
      case (Some(Right(x)), Some(Right(y))) => family.sameResult(x, y)
      case (Some(Left(x)), Some(Left(y)))   => family.sameError(x, y)
      case (None, None)                     => true
      case _                                => false

    def cycled(sequence: Vector[WorkQuanta]): Int => WorkQuanta =
      index => sequence(index % sequence.size)

    Vector(
      "the same cursor at the same quanta yields the same steps and the same end" ->
        forAll(cursors, quanta) { (cursor, q) =>
          val first  = trace(family, cursor, _ => q)
          val second = trace(family, cursor, _ => q)
          (first.steps == second.steps) :| s"steps differ: ${first.steps} versus ${second.steps}" &&
          same(first.end, second.end) :| "ends differ"
        },
      "a run ends in exactly one terminal step within the family's step budget" ->
        forAll(cursors, quanta) { (cursor, q) =>
          val run = trace(family, cursor, _ => q)
          run.end.isDefined :| s"no terminal step within ${family.stepBudget} steps" &&
          run.steps.nonEmpty :| "a run has at least its terminal step"
        },
      "completion at any quanta is the pure run" ->
        forAll(cursors, quanta) { (cursor, q) =>
          val run = trace(family, cursor, _ => q)
          same(run.end, Some(oracle(family, cursor))) :| s"end differs from the pure run at $q"
        },
      "every sequence of quanta yields the pure run" ->
        forAll(cursors, sequences) { (cursor, sequence) =>
          val run = trace(family, cursor, cycled(sequence))
          same(run.end, Some(oracle(family, cursor))) :|
            s"end differs from the pure run under $sequence"
        },
      "units are non-negative and each segment is visited in one contiguous block" ->
        forAll(cursors, quanta) { (cursor, q) =>
          val run      = trace(family, cursor, _ => q)
          val segments = run.blocks.map(_._1)
          run.steps.forall(_.units >= 0) :| "a step charged negative units" &&
          (segments == segments.distinct) :| s"a segment was revisited: $segments"
        },
      "a segment's total is stated once, from its first step to its last" ->
        forAll(cursors, quanta) { (cursor, q) =>
          val run = trace(family, cursor, _ => q)
          Prop.all(run.blocks.map { (segment, _, totals) =>
            (totals.distinct.size == 1) :| s"$segment stated ${totals.distinct}"
          }*)
        },
      "an Exact total is met and an AtMost total is never exceeded" ->
        forAll(cursors, quanta) { (cursor, q) =>
          val run = trace(family, cursor, _ => q)
          Prop.all(run.blocks.map { (segment, units, totals) =>
            totals.head match
              case SegmentTotal.Exact(n) =>
                (units == n) :| s"$segment charged $units of Exact($n)"
              case SegmentTotal.AtMost(n) =>
                (units <= n) :| s"$segment charged $units over AtMost($n)"
              case SegmentTotal.Unknown => Prop.passed
          }*)
        },
      "the work each segment charges is a property of the cursor, not of its cuts" ->
        forAll(cursors, quanta, sequences) { (cursor, q, sequence) =>
          val fixed = trace(family, cursor, _ => q).accounting
          val cut   = trace(family, cursor, cycled(sequence)).accounting
          (fixed == cut) :| s"accounting at $q: $fixed; under $sequence: $cut"
        }
    )
