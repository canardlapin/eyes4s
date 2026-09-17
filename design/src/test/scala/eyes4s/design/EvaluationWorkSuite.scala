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

import eyes4s.compare.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

import scala.compiletime.testing.typeCheckErrors

/** Bounded pair evaluation, reduction and contrast against the whole API. */
class EvaluationWorkSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  final case class Key(subject: String, item: String) derives CanEqual
  private given KeyDigest[Key] = KeyDigest.derived[Key]
  private given Ordering[Key]  = Ordering.by(k => (k.subject, k.item))
  private val subject          = Projection.named[Key, String]("subject")(_.subject)
  private val item             = Projection.named[Key, String]("item")(_.item)
  private val controls         =
    Pairing.between[Key, Key].sameOn(subject, subject).differentOn(item, item).all

  private val grid  = get(Grid.over(get(Frame.screen("work", 2, 2)), 2, 2))
  private val other = get(Grid.over(get(Frame.screen("work", 4, 4)), 2, 2))
  private def mass(g: Grid[Px], values: Double*): Mass[Px] =
    get(Surface.mass(g, IArray.from(values), Provenance.raw(ContentHash.empty)))

  private val cosine = Distribution.cosine[Px]
  private val info   = EvaluationInfo.comparison(cosine)

  private val leftKeys  = Vector(Key("p", "a"), Key("p", "b"), Key("p", "c"), Key("q", "a"))
  private val rightKeys = Vector(Key("p", "a"), Key("p", "b"), Key("p", "c"))
  private val left: Vector[Either[String, Mass[Px]]] = Vector(
    Right(mass(grid, 0.5, 0.5, 0.0, 0.0)),
    Left("left b was not estimated"),
    Right(mass(grid, 0.0, 0.0, 0.5, 0.5)),
    Right(mass(grid, 0.25, 0.25, 0.25, 0.25))
  )
  private val right: Vector[Either[String, Mass[Px]]] = Vector(
    Right(mass(grid, 0.5, 0.0, 0.5, 0.0)),
    Right(mass(grid, 0.25, 0.25, 0.25, 0.25)),
    Right(mass(other, 0.25, 0.25, 0.25, 0.25))
  )
  private val schedule = get(
    DirectedPairSchedule.exhaustive(leftKeys, rightKeys, controls.relation)
  )

  private def operands(
      pair: ScheduledPair[Key, Key]
  ): Either[String, (Mass[Px], Mass[Px])] =
    left(pair.leftIndex).flatMap(a => right(pair.rightIndex).map(b => (a, b)))

  private def bounded(budget: ComparisonBudget = ComparisonBudget.default) =
    PairEvaluation.Bounded[Key, Key, Mass[Px], Mass[Px], String, Similarity](
      cosine,
      budget,
      operands,
      (_, error) => error.message
    )

  private val whole = PairEvaluation.Whole[Key, Key, String, Similarity](pair =>
    operands(pair).flatMap { case (a, b) => cosine.compare(a, b).left.map(_.message) }
  )

  private def drive(
      cursor: EvaluationCursor[Key, Key, String, Similarity],
      quanta: WorkQuanta
  ): Either[
    EvaluationWorkError,
    (Int, DirectedPairwiseAnalysis[Key, Key, String, Similarity])
  ] =
    val bound = math.max(quanta.pairs.value, quanta.comparison.value)
    @annotation.tailrec
    def loop(
        cursor: EvaluationCursor[Key, Key, String, Similarity],
        steps: Int
    ): Either[
      EvaluationWorkError,
      (Int, DirectedPairwiseAnalysis[Key, Key, String, Similarity])
    ] =
      cursor.advance(quanta) match
        case Left(error)                             => Left(error)
        case Right(EvaluationPage.More(units, next)) =>
          assert(units >= 0 && units <= bound, clue((units, bound)))
          assert(next.completedPairs >= cursor.completedPairs)
          loop(next, steps + 1)
        case Right(EvaluationPage.Done(units, analysis)) =>
          assertEquals(units, 0)
          assertEquals(analysis.rows.size, cursor.completedPairs)
          Right((steps, analysis))
    loop(cursor, 0)

  private val quanta =
    for
      pairs      <- Vector(1, 3, 1024)
      comparison <- Vector(1, 2, 4, 1 << 16)
    yield WorkQuanta(get(PairQuantum.of(pairs)), get(ComparisonQuantum.of(comparison)))

  test("bounded evaluation at every quantum matches whole evaluation and the legacy pair API") {
    val legacy = evaluatePairs(
      pair(
        Trials(leftKeys.zip(left).map((k, v) => Trial(k, (), v))),
        Trials(rightKeys.zip(right).map((k, v) => Trial(k, (), v))),
        controls
      ),
      ContentHash.empty,
      info
    )((a: Either[String, Mass[Px]], b: Either[String, Mass[Px]]) =>
      a.flatMap(x => b.flatMap(y => cosine.compare(x, y).left.map(_.message)))
    )
    assertEquals(legacy.rows.size, 6)
    assertEquals(legacy.rows.count(_.result.isLeft), 3)
    assert(legacy.rows.exists(_.result.left.exists(_.contains("left b was not estimated"))))
    val viaWhole = get(evaluateScheduled(schedule, ContentHash.empty, info)(whole.evaluate))
    assertEquals(viaWhole, legacy)

    quanta.foreach { q =>
      val (boundedSteps, viaBounded) =
        get(drive(EvaluationWork.start(schedule, ContentHash.empty, info, bounded()), q))
      assertEquals(viaBounded, legacy, clue(q))
      val (wholeSteps, viaPaged) =
        get(drive(EvaluationWork.start(schedule, ContentHash.empty, info, whole), q))
      assertEquals(viaPaged, legacy, clue(q))
      assert(boundedSteps >= wholeSteps, clue((boundedSteps, wholeSteps)))
      Vector(FailurePolicy.RequireAll, get(FailurePolicy.successfulOnly(1))).foreach { policy =>
        val reference = legacy.meanByLeft(policy)
        val reduced   = reduce(viaBounded.meanByLeftWork(policy), q.pairs)
        assertEquals(reduced.entries, reference.entries, clue((q, policy)))
        assertEquals(reduced.diagnostics, reference.diagnostics)
        assertEquals(reduced.provenance, reference.provenance)
      }
    }
    // The finest cut visits one cell per comparison step: four pairs with
    // operands, four cells each, plus one step per pair, page and completion.
    val finest = WorkQuanta(get(PairQuantum.of(1)), get(ComparisonQuantum.of(1)))
    assert(
      get(
        drive(EvaluationWork.start(schedule, ContentHash.empty, info, bounded()), finest)
      )._1 > 16
    )
  }

  private def reduce[K, S](
      cursor: ReductionCursor[K, S],
      quantum: PairQuantum
  ): Analysis[K, S] =
    @annotation.tailrec
    def loop(cursor: ReductionCursor[K, S]): Analysis[K, S] =
      cursor.advance(quantum) match
        case ReductionPage.More(units, next) =>
          assert(units > 0)
          assert(next.reducedKeys >= cursor.reducedKeys)
          loop(next)
        case ReductionPage.Done(_, analysis) => analysis
    loop(cursor)

  test("an over-budget comparison is refused with its operands before any pair completes") {
    val cursor = EvaluationWork.start(
      schedule,
      ContentHash.empty,
      info,
      bounded(get(ComparisonBudget.of(3)))
    )
    assertEquals(
      drive(cursor, WorkQuanta.default).left.toOption,
      Some(EvaluationWorkError.Comparison(ComparisonWorkError.WorkBudget("cosine", 4, 3)))
    )
    assert(
      drive(cursor, WorkQuanta.default).isLeft
    ) // the cursor is immutable; re-driving agrees
    assert(
      get(
        drive(
          EvaluationWork.start(
            schedule,
            ContentHash.empty,
            info,
            bounded(get(ComparisonBudget.of(4)))
          ),
          WorkQuanta.default
        )
      )._2.rows.nonEmpty
    )
  }

  test("reduction and contrast cursors agree with whole reductions, including overflow rows") {
    val spec = get(
      EvaluationSpec.of(
        "doubles",
        "1",
        Vector.empty,
        Vector("value"),
        EvaluationGeometry.Independent,
        EvaluationTime.OrderFree
      )
    )
    val scored = EvaluationInfo("doubles", EvaluationScale.Unitless, Some(spec))
    val keys   = Vector(Key("p", "a"), Key("p", "b"), Key("p", "c"), Key("p", "d"))
    val refs   = Vector(Key("p", "x"), Key("p", "y"))
    def analysis(scores: Map[Key, Either[String, Double]]) =
      get(
        evaluateScheduled(
          get(DirectedPairSchedule.exhaustive(keys, refs, controls.relation)),
          ContentHash.empty,
          scored
        )(pair => scores(pair.left))
      )
    val matched = analysis(
      Map(
        Key("p", "a") -> Right(1e308),
        Key("p", "b") -> Right(0.5),
        Key("p", "c") -> Right(Double.PositiveInfinity),
        Key("p", "d") -> Left("failed")
      )
    )
    val control = analysis(
      Map(
        Key("p", "a") -> Right(-1e308),
        Key("p", "b") -> Right(0.25),
        Key("p", "c") -> Right(0.0),
        Key("p", "d") -> Right(0.0)
      )
    )
    Vector(FailurePolicy.RequireAll, get(FailurePolicy.successfulOnly(1))).foreach { policy =>
      val m = matched.meanByLeft(policy)
      val c = control.meanByLeft(policy)
      assertEquals(m.entries.map(_.key), keys)
      m.entries.find(_.key == Key("p", "c")).map(_.result) match
        case Some(
              Left(ReductionError.MeanFailure(_, ScoreMeanError.NonFiniteValue(_, 0, v)))
            ) =>
          assert(v.isPosInfinity)
        case other => fail(s"expected a non-finite reduction failure, got $other")
      val whole = get(contrast(m, c))
      whole.rows.find(_.key == Key("p", "a")).map(_.difference) match
        case Some(
              Left(ContrastRowError.Arithmetic(_, DifferenceError.NonFiniteDifference(_, a, b)))
            ) =>
          assertEquals((a, b), (1e308, -1e308))
        case other => fail(s"expected an overflow row, got $other")
      assertEquals(
        whole.rows.find(_.key == Key("p", "b")).map(_.difference.map(_.value)),
        Some(Right(0.25))
      )
      Vector(1, 2, 3, 1024).foreach { quantum =>
        val q     = get(PairQuantum.of(quantum))
        val paged = get(
          contrastWork(
            reduce(matched.meanByLeftWork(policy), q),
            reduce(control.meanByLeftWork(policy), q)
          )
        )
        val result = drain(paged, q)
        assertEquals(result.rows.map(_.key), whole.rows.map(_.key))
        assertEquals(result.rows.map(_.difference), whole.rows.map(_.difference))
        assertEquals(result.rows.map(_.matched), whole.rows.map(_.matched))
        assertEquals(result.rows.map(_.control), whole.rows.map(_.control))
        assertEquals(result.matched.entries, whole.matched.entries)
        assertEquals(result.control.provenance, whole.control.provenance)
      }
    }
  }

  private def drain[K, S, D](cursor: ContrastCursor[K, S, D], quantum: PairQuantum) =
    @annotation.tailrec
    def loop(cursor: ContrastCursor[K, S, D]): Contrast[K, S, D] =
      cursor.advance(quantum) match
        case ContrastPage.More(units, next) =>
          assert(units > 0 && units <= quantum.value)
          assert(next.contrastedKeys == cursor.contrastedKeys + units)
          loop(next)
        case ContrastPage.Done(units, contrast) =>
          assert(units <= quantum.value)
          contrast
    loop(cursor)

  test("bounded pair evaluation cannot be claimed for a whole comparison or a closure") {
    assert(
      typeCheckErrors("""
        import eyes4s.compare.*
        import eyes4s.design.*
        import eyes4s.kernel.*
        import eyes4s.kernel.Unit2D.Px
        PairEvaluation.Bounded[String, String, Mass[Px], Mass[Px], String, MeasureDistance](
          Distribution.totalVariation[Px],
          ComparisonBudget.default,
          _ => Left("unused"),
          (_, e) => e.message
        )
      """).nonEmpty,
      "a whole synchronous metric was accepted as bounded pair evaluation"
    )
    assert(
      typeCheckErrors("""
        import eyes4s.compare.*
        import eyes4s.design.*
        import eyes4s.kernel.*
        import eyes4s.kernel.Unit2D.Px
        val closure: (Mass[Px], Mass[Px]) => Either[CompareError, Similarity] =
          (a, b) => Distribution.cosine[Px].compare(a, b)
        PairEvaluation.Bounded[String, String, Mass[Px], Mass[Px], String, Similarity](
          closure,
          ComparisonBudget.default,
          _ => Left("unused"),
          (_, e) => e.message
        )
      """).nonEmpty,
      "a closure was accepted as bounded pair evaluation"
    )
  }
