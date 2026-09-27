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
  * sequence of quanta, is deterministic, ends in exactly one terminal step
  * (a result or a typed failure), agrees with the family's reference run,
  * visits its counted segments contiguously, and states each segment's
  * total once and truthfully. The runner-side half of the contract (one
  * terminal event, cancellation between steps, first commit wins, defects
  * surface) is effectful and lives in the `eyes4s-fs2` tests.
  *
  * A family supplies its [[Family]] evidence and a generator of cursors; the
  * shipped study, recording and temporal families are checked in the module's
  * own suite, which also shows each law killing a deliberate mutant. A
  * downstream family that implements `Stepwise` runs the same rule set.
  *
  * What the reference run proves depends on where it comes from. For the
  * shipped families it is the plan's own `run` (`PreparedStudy.run`,
  * `RecordingPlan.run`, `PreparedTemporalStudy.run`), and that `run` drives
  * the same cursor at [[WorkQuanta.default]]. The completion laws then say
  * that every cut, and every sequence of cuts, reaches the default-quanta
  * result: cut invariance, not scientific correctness. They are kept
  * separate from the `Stepwise` instance under test, so an instance that
  * completes early or differently cannot vouch for itself, but they share
  * its science. Scientific correctness needs an independent oracle, an
  * expectation computed without the cursor (a pinned reference fixture, for
  * example); pass it as `independent` and the rule set also requires every
  * cut to satisfy it.
  */
object ExecutionLaws extends Laws:

  /** One family's evidence for the laws: how a stage is counted, what a
    * segment's total is as its first step begins (given the cursor about to
    * take it), the reference run of the plan a generated cursor came from,
    * when two results or two errors are the same, and a step budget within
    * which every generated cursor must end. Results are compared with
    * `sameResult` because scientific results carry provenance and arrays that
    * have no useful universal equality.
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

  /** A run's trace: every step that completed, the `Done` step included,
    * and its end. A failing advance records no step: its work was not
    * committed. `end` is `None` only when the step budget was exhausted
    * before a terminal step.
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

    /** Whether the run ended in a typed failure. */
    def failed: Boolean = end.exists(_.isLeft)

    /** The blocks that ran to their end. A failed run's last block is cut
      * short by the failure, and how much of it completed depends on the
      * cuts, so it is not one of them.
      */
    def completedBlocks: Vector[(Segment, Long, Vector[SegmentTotal])] =
      if failed then blocks.dropRight(1) else blocks

    /** The work each completed segment block charged, which the laws require to be cut-invariant. */
    def accounting: Vector[(Segment, Long)] =
      completedBlocks.map((segment, units, _) => (segment, units))

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

  /** The reference run: the family's own run of the plan the cursor came from. */
  def oracle[C, Stage, Segment, E, R](
      family: Family[C, Stage, Segment, E, R],
      cursor: C
  ): Either[E, R] = family.pure(cursor)

  /** Every quantum at its smallest value: the most finely cut run any
    * family admits. The cut laws always compare against it, so a fault that
    * appears only at the finest cut is found on every generated case.
    */
  val finest: WorkQuanta =
    (PairQuantum.of(1), ComparisonQuantum.of(1), SampleQuantum.of(1)) match
      case (Right(pairs), Right(comparison), Right(samples)) =>
        WorkQuanta(pairs, comparison, samples)
      case _ => WorkQuanta.default // unreachable: 1 is a valid value of every quantum

  /** Quanta drawn from the given positive values, with [[finest]] and
    * `WorkQuanta.default` included explicitly.
    */
  def quanta(pairs: Seq[Int], comparison: Seq[Int], samples: Seq[Int]): Gen[WorkQuanta] =
    def pick[A](values: Seq[Int], of: Int => Either[?, A], default: A): Gen[A] =
      Gen.oneOf(values.flatMap(v => of(v).toOption) :+ default)
    Gen.oneOf(
      Gen.const(WorkQuanta.default),
      Gen.const(finest),
      for
        p <- pick(pairs, PairQuantum.of, PairQuantum.default)
        c <- pick(comparison, ComparisonQuantum.of, ComparisonQuantum.default)
        s <- pick(samples, SampleQuantum.of, SampleQuantum.default)
      yield WorkQuanta(p, c, s)
    )

  /** The rule set. `cursors` should cover the family's designated fixtures:
    * fully matched and unmatched schedules, several scales, chunk-spanning
    * events, a run that fails, and whatever else changes the step sequence.
    * `independent`, when given, is an oracle computed without the cursor
    * that every run's end must satisfy.
    */
  def conformance[C, Stage, Segment, E, R](
      family: Family[C, Stage, Segment, E, R],
      cursors: Gen[C],
      quanta: Gen[WorkQuanta],
      independent: Option[Either[E, R] => Prop] = None
  ): RuleSet =
    new SimpleRuleSet("execution", laws(family, cursors, quanta, independent)*)

  /** The laws as named properties, so a suite can also run each one alone
    * against a mutant and record which laws kill it.
    */
  def laws[C, Stage, Segment, E, R](
      family: Family[C, Stage, Segment, E, R],
      cursors: Gen[C],
      quanta: Gen[WorkQuanta],
      independent: Option[Either[E, R] => Prop] = None
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

    def reference(cursor: C): Option[Either[E, R]] = Some(oracle(family, cursor))

    val core = Vector(
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
          // A completed run records its Done step; a run whose first advance
          // fails ends lawfully with no step at all.
          run.end.isDefined :| s"no terminal step within ${family.stepBudget} steps" &&
          (run.failed || run.steps.nonEmpty) :| "a completed run recorded no terminal step"
        },
      "completion at any quanta is the reference run" ->
        forAll(cursors, quanta) { (cursor, q) =>
          val expected = reference(cursor)
          same(trace(family, cursor, _ => q).end, expected) :|
            s"end differs from the reference run at $q" &&
            same(trace(family, cursor, _ => finest).end, expected) :|
            "end differs from the reference run at the finest cut"
        },
      "every sequence of quanta yields the reference run" ->
        forAll(cursors, sequences) { (cursor, sequence) =>
          val run = trace(family, cursor, cycled(sequence))
          same(run.end, reference(cursor)) :|
            s"end differs from the reference run under $sequence"
        },
      "units are non-negative and each segment is visited in one contiguous block" ->
        forAll(cursors, quanta) { (cursor, q) =>
          val run      = trace(family, cursor, _ => q)
          val segments = run.blocks.map(_._1)
          run.steps.forall(_.units >= 0) :| "a step charged negative units" &&
          (segments == segments.distinct) :| s"a segment was revisited: $segments"
        },
      "a segment's total is position-independent: every step of the segment states the same one" ->
        forAll(cursors, quanta) { (cursor, q) =>
          val run = trace(family, cursor, _ => q)
          Prop.all(run.blocks.map { (segment, _, totals) =>
            (totals.distinct.size == 1) :| s"$segment stated ${totals.distinct}"
          }*)
        },
      "an Exact total is met and an AtMost total is never exceeded" ->
        forAll(cursors, quanta) { (cursor, q) =>
          val run  = trace(family, cursor, _ => q)
          val last = run.blocks.size - 1
          Prop.all(run.blocks.zipWithIndex.map { case ((segment, units, totals), index) =>
            // The block a failure cut short is held to its bound, not its total.
            val cutShort = run.failed && index == last
            totals.head match
              case SegmentTotal.Exact(n) if cutShort =>
                (units <= n) :| s"$segment, cut short by a failure, charged $units over Exact($n)"
              case SegmentTotal.Exact(n) =>
                (units == n) :| s"$segment charged $units of Exact($n)"
              case SegmentTotal.AtMost(n) =>
                (units <= n) :| s"$segment charged $units over AtMost($n)"
              case SegmentTotal.Unknown | SegmentTotal.Counting => Prop.passed
          }*)
        },
      "the work each segment charges is a property of the cursor, not of its cuts" ->
        forAll(cursors, quanta, sequences) { (cursor, q, sequence) =>
          val runs = Vector(
            s"$q"        -> trace(family, cursor, _ => q),
            "finest"     -> trace(family, cursor, _ => finest),
            s"$sequence" -> trace(family, cursor, cycled(sequence))
          )
          val (_, first) = runs.head
          Prop.all(runs.tail.map { (label, run) =>
            // A failure may land a block earlier or later depending on the
            // cuts; the blocks both runs completed must agree.
            val agreed =
              if first.failed || run.failed then
                first.accounting.zip(run.accounting).forall(_ == _)
              else first.accounting == run.accounting
            agreed :| s"accounting at $q: ${first.accounting}; at $label: ${run.accounting}"
          }*)
        }
    )
    core ++ independent.toVector.map(expect =>
      "completion at any sequence of quanta satisfies the independent oracle" ->
        forAll(cursors, sequences) { (cursor, sequence) =>
          trace(family, cursor, cycled(sequence)).end match
            case Some(end) => expect(end) :| s"under $sequence"
            case None      => Prop.falsified :| "no terminal step"
        }
    )
