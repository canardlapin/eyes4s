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

import eyes4s.compare.*
import eyes4s.design.*
import eyes4s.kernel.*
import org.scalacheck.Gen
import scala.compiletime.testing.typeCheckErrors

class ContrastSuite extends munit.DisciplineSuite:
  private val NumericalTolerance                = Tolerance(absolute = 1e-12, relative = 0.0)
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private def scalar(d: SignedDifference): Vector[Double] = Vector(d.value)
  private def multi(s: MultiMatchScore): Vector[Double]   =
    Vector(s.shape, s.direction, s.length, s.position, s.duration)
  private def delta(s: MultiMatchDifference): Vector[Double] =
    Vector(s.shape.value, s.direction.value, s.length.value, s.position.value, s.duration.value)
  private val bounded = Gen.choose(-100.0, 100.0)
  private val score   =
    Gen.choose(0.0, 1.0).map(x => get(MultiMatchScore.of(x, 1 - x, x / 2, x / 3, x / 4)))
  private val high = get(MultiMatchScore.of(0.9, 0.8, 0.7, 0.6, 0.5))
  private val low  = get(MultiMatchScore.of(0.3, 0.5, 0.7, 0.9, 0.9))

  checkAll(
    "double",
    ContrastLaws.subtraction(
      Contrastable[Double, SignedDifference],
      bounded,
      (x: Double) => Vector(x),
      scalar,
      (0.75, 0.25, Vector(0.5)),
      NumericalTolerance
    )
  )
  checkAll(
    "similarity",
    ContrastLaws.subtraction(
      Contrastable[Similarity, SignedDifference],
      bounded.map(x => get(Similarity.of(x))),
      (x: Similarity) => Vector(x.value),
      scalar,
      (get(Similarity.of(0.75)), get(Similarity.of(0.25)), Vector(0.5)),
      NumericalTolerance
    )
  )
  checkAll(
    "distance",
    ContrastLaws.subtraction(
      Contrastable[MeasureDistance, SignedDifference],
      Gen.choose(0.0, 100.0).map(x => get(MeasureDistance.of(x))),
      (x: MeasureDistance) => Vector(x.value),
      scalar,
      (get(MeasureDistance.of(0.25)), get(MeasureDistance.of(0.75)), Vector(-0.5)),
      NumericalTolerance
    )
  )
  checkAll(
    "multimatch",
    ContrastLaws.subtraction(
      Contrastable[MultiMatchScore, MultiMatchDifference],
      score,
      multi,
      delta,
      (high, low, Vector(0.6, 0.3, 0.0, -0.3, -0.4)),
      NumericalTolerance
    )
  )

  private def spec(
      method: String = "test.scalar",
      revision: String = "1",
      parameters: Vector[(String, Provenance.Param)] = Vector.empty,
      components: Vector[String] = Vector("value"),
      geometry: EvaluationGeometry = EvaluationGeometry.Independent,
      time: EvaluationTime = EvaluationTime.OrderFree
  ): EvaluationSpec = get(
    EvaluationSpec.of(method, revision, parameters, components, geometry, time)
  )

  private def info(s: EvaluationSpec = spec()): EvaluationInfo =
    EvaluationInfo("test scalar", EvaluationScale.Unitless, Some(s))

  private def evaluated(
      keys: Vector[String] = Vector("a", "b"),
      values: Vector[Double] = Vector(1.0, 3.0),
      metadata: EvaluationInfo = info(),
      inputs: String = "first",
      failOn: Set[Int] = Set.empty
  ): DirectedPairwiseAnalysis[String, Int, String, Double] =
    val left  = Trials(keys.map(key => Trial(key, (), 0.0)))
    val right = Trials(values.zipWithIndex.map { case (value, index) =>
      Trial(index, (), (index, value))
    })
    evaluatePairs(
      pair(left, right, Pairing.between[String, Int].all),
      ContentHash.ofString(inputs),
      metadata
    ) { (_, right) =>
      Either.cond(!failOn(right._1), right._2, s"reference ${right._1} rejected")
    }

  private def reduced(
      metadata: EvaluationInfo = info(),
      keys: Vector[String] = Vector("a", "b")
  ): Analysis[String, Double] =
    evaluated(keys = keys, metadata = metadata).meanByLeft(FailurePolicy.RequireAll)

  private def issues(
      result: Either[ContrastError[String], Contrast[String, Double, SignedDifference]]
  ): Vector[ContrastCompatibilityError] =
    result match
      case Left(ContrastError.Incompatible(errors)) => errors.toVector
      case other => fail(s"Expected incompatible operands, got $other")

  test("key union preserves missing operands, canonical order, and distinct input provenance") {
    val m = evaluated(keys = Vector("c", "a"), values = Vector(4.0), inputs = "matched")
      .meanByLeft(FailurePolicy.RequireAll)
    val c = evaluated(keys = Vector("b", "a"), inputs = "control")
      .meanByLeft(FailurePolicy.RequireAll)
    val result = get(contrast(m, c))
    assertEquals(result.rows.map(_.key), Vector("a", "b", "c"))
    assertEquals(get(result.rows(0).difference).value, 2.0)
    assertEquals(
      result.rows(1).difference,
      Left(ContrastRowError.MissingOperands("b", Vector(ContrastOperand.Matched)))
    )
    assertEquals(
      result.rows(2).difference,
      Left(ContrastRowError.MissingOperands("c", Vector(ContrastOperand.Control)))
    )
    assertEquals(result.matched.provenance.inputs, ContentHash.ofString("matched"))
    assertEquals(result.control.provenance.inputs, ContentHash.ofString("control"))
    assertEquals(result.rows(0).matched.map(_.contributing), Some(1))
    assertEquals(result.rows(0).control.map(_.contributing), Some(2))
    assertEquals(result.matched.source.rows.size, 2)
  }

  test("both failed reductions and original pair failures survive") {
    val m   = evaluated(failOn = Set(0)).meanByLeft(FailurePolicy.RequireAll)
    val c   = evaluated(failOn = Set(1)).meanByLeft(FailurePolicy.RequireAll)
    val row = get(contrast(m, c)).rows(0)
    assertEquals(
      row.difference,
      Left(
        ContrastRowError.ReductionFailures(
          "a",
          Some(ReductionError.FailedScores("a", 1, 1)),
          Some(ReductionError.FailedScores("a", 1, 1))
        )
      )
    )
    assertEquals(row.matched.map(_.contributing), Some(0))
    assertEquals(row.control.map(_.failed), Some(1))
    assertEquals(m.source.rows.count(_.result.isLeft), 2)
  }

  test("successful-only denominators count actual contributions and policy mismatches reject") {
    val one        = get(FailurePolicy.successfulOnly(1))
    val two        = get(FailurePolicy.successfulOnly(2))
    val evaluatedM = evaluated(failOn = Set(0))
    val m          = evaluatedM.meanByLeft(one)
    val c          = evaluated().meanByLeft(one)
    val row        = get(contrast(m, c)).rows(0)
    assertEquals(get(row.difference).value, 1.0)
    assertEquals(
      row.matched.map(r => (r.selected, r.successful, r.failed, r.contributing)),
      Some((2, 1, 1, 1))
    )
    assert(issues(contrast(m, evaluated().meanByLeft(two))).exists {
      case ContrastCompatibilityError.Policy(`one`, `two`) => true; case _ => false
    })
    assert(issues(contrast(m, evaluatedM.meanByLeft(FailurePolicy.RequireAll))).exists {
      case ContrastCompatibilityError.Policy(_, _) => true; case _ => false
    })
  }

  test("ambiguous focal keys retain occurrence indices and never produce a difference") {
    val m      = reduced(keys = Vector("a", "a"))
    val result = get(contrast(m, reduced()))
    assertEquals(
      result.rows.head.matched.map(_.result),
      Some(Left(ReductionError.AmbiguousKey("a", Vector(0, 1))))
    )
    assert(result.rows.head.difference.isLeft)
  }

  test("method, revision, parameters, scale, and component mismatches reject") {
    val base = reduced()
    Vector(
      spec(method = "other"),
      spec(revision = "2"),
      spec(parameters = Vector("sigma" -> Provenance.Param.Num(2.0)))
    ).foreach { other =>
      assert(issues(contrast(base, reduced(info(other)))).exists {
        case ContrastCompatibilityError.Method(_, _) => true; case _ => false
      })
    }
    assert(issues(contrast(base, reduced(info().copy(scale = EvaluationScale.Count)))).exists {
      case ContrastCompatibilityError.Scale(_, _) => true; case _ => false
    })
    assert(issues(contrast(base, reduced(info(spec(components = Vector("shape")))))).exists {
      case ContrastCompatibilityError.Components(_, _, _) => true; case _ => false
    })
    assert(
      issues(contrast(base, reduced(EvaluationInfo("unknown", EvaluationScale.Unitless))))
        .exists {
          case ContrastCompatibilityError.MissingSpecification(ContrastOperand.Control, _) =>
            true;
          case _ => false
        }
    )
  }

  test("orientation is checked before any keyed subtraction") {
    val trials = Trials(Vector(Trial("a", (), 1.0)))
    val p      = evaluatePairs(
      pair(trials, trials, Pairing.between[String, String].all),
      ContentHash.empty,
      info()
    )((a, b) => Right(a + b))
    assert(
      issues(
        contrast(
          p.meanByLeft(FailurePolicy.RequireAll),
          p.meanByRight(FailurePolicy.RequireAll)
        )
      ).exists {
        case ContrastCompatibilityError
              .Orientation(ReductionOrientation.ByLeft, ReductionOrientation.ByRight) =>
          true
        case _ => false
      }
    )
  }

  test("geometry identity, structure, discretisation and time use Agreement") {
    val frame   = get(Frame.screen("display", 2, 2))
    val other   = get(Frame.screen("other", 2, 2))
    val corrupt = get(Frame.screen("display", 3, 2))
    val grid    = get(Grid.over(frame, 2, 2))
    val base    = reduced(info(spec(geometry = EvaluationGeometry.onGrid(grid))))
    Vector(other, corrupt).foreach { f =>
      assert(
        issues(
          contrast(
            base,
            reduced(info(spec(geometry = EvaluationGeometry.onGrid(get(Grid.over(f, 2, 2))))))
          )
        ).exists {
          case ContrastCompatibilityError.Frames(_) => true; case _ => false
        }
      )
    }
    assert(
      issues(
        contrast(
          base,
          reduced(info(spec(geometry = EvaluationGeometry.onGrid(get(Grid.over(frame, 4, 4))))))
        )
      ).exists { case ContrastCompatibilityError.Grids(_) => true; case _ => false }
    )
    assert(issues(contrast(base, reduced())).exists {
      case ContrastCompatibilityError.SpatialConvention(_, _) => true; case _ => false
    })
    assert(
      issues(
        contrast(reduced(), reduced(info(spec(time = EvaluationTime.RelativeMicroseconds))))
      ).exists { case ContrastCompatibilityError.Time(_, _) => true; case _ => false }
    )
    val m = reduced(info(spec(time = EvaluationTime.SharedClock(ClockId("m")))))
    val c = reduced(info(spec(time = EvaluationTime.SharedClock(ClockId("c")))))
    assert(issues(contrast(m, c)).exists {
      case ContrastCompatibilityError.Clocks(
            TimeError.ClockMismatch(ClockId("m"), ClockId("c"))
          ) =>
        true;
      case _ => false
    })
  }

  test("empty domain and non-discriminating key order are explicit errors") {
    assertEquals(
      contrast(reduced(keys = Vector.empty), reduced(keys = Vector.empty)),
      Left(ContrastError.EmptyDomain[String](0, 0))
    )
    val badOrder = new Ordering[String]:
      def compare(a: String, b: String): Int = 0
    assert(
      contrast(reduced(), reduced())(using
        Contrastable[Double, SignedDifference],
        badOrder
      ).isLeft
    )
  }

  test(
    "finite subtraction overflow names operands, and nonfinite inputs cannot enter a signed value"
  ) {
    assertEquals(
      SignedDifference.between(Double.MaxValue, -Double.MaxValue),
      Left(DifferenceError.NonFiniteDifference("value", Double.MaxValue, -Double.MaxValue))
    )
    assert(SignedDifference.between(Double.NaN, 1.0).isLeft)
    assert(SignedDifference.between(1.0, Double.PositiveInfinity).isLeft)
    val m = evaluated(values = Vector(Double.MaxValue)).meanByLeft(FailurePolicy.RequireAll)
    val c = evaluated(values = Vector(-Double.MaxValue)).meanByLeft(FailurePolicy.RequireAll)
    assertEquals(
      get(contrast(m, c)).rows.head.difference,
      Left(
        ContrastRowError.Arithmetic(
          "a",
          DifferenceError.NonFiniteDifference("value", Double.MaxValue, -Double.MaxValue)
        )
      )
    )
  }

  test("method declarations validate names, finite parameters and ordered component identity") {
    assert(
      EvaluationSpec
        .of(
          "",
          "1",
          Vector.empty,
          Vector("value"),
          EvaluationGeometry.Independent,
          EvaluationTime.OrderFree
        )
        .isLeft
    )
    assert(
      EvaluationSpec
        .of(
          "m",
          "1",
          Vector("x" -> Provenance.Param.Num(Double.NaN)),
          Vector("value"),
          EvaluationGeometry.Independent,
          EvaluationTime.OrderFree
        )
        .isLeft
    )
    assert(
      EvaluationSpec
        .of(
          "m",
          "1",
          Vector("x" -> Provenance.Param.Num(1.0), "x" -> Provenance.Param.Num(2.0)),
          Vector("value"),
          EvaluationGeometry.Independent,
          EvaluationTime.OrderFree
        )
        .isLeft
    )
    assert(
      EvaluationSpec
        .of(
          "m",
          "1",
          Vector.empty,
          Vector("value", "value"),
          EvaluationGeometry.Independent,
          EvaluationTime.OrderFree
        )
        .isLeft
    )
    val params = Vector("z" -> Provenance.Param.Num(1.0), "a" -> Provenance.Param.Flag(true))
    val m      = reduced(info(spec(parameters = params)))
    val c      = reduced(info(spec(parameters = params.reverse)))
    assert(contrast(m, c).isRight)
    assertEquals(m.provenance.digest, c.provenance.digest)
    assertNotEquals(m.provenance.digest, reduced().provenance.digest)
  }

  test("signed outputs cannot be substituted for bounded scores or raw doubles") {
    assert(
      typeCheckErrors("""import eyes4s.design.*; val x: SignedDifference = 0.5""").nonEmpty
    )
    assert(
      typeCheckErrors(
        """import eyes4s.design.*; import eyes4s.compare.*; def f(d: MultiMatchDifference): MultiMatchScore = d"""
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        """import eyes4s.design.*; import eyes4s.compare.*; summon[Contrastable[MultiMatchScore, MultiMatchScore]]"""
      ).nonEmpty
    )
  }

  test("erased spatial metadata retains the unit witness even when identity and bounds match") {
    val pixels     = get(Frame.screen("unit-check", 1, 1))
    val normalized = get(Frame.unitSquare("unit-check"))
    val m          = reduced(info(spec(geometry = EvaluationGeometry.inFrame(pixels))))
    val c          = reduced(info(spec(geometry = EvaluationGeometry.inFrame(normalized))))
    assert(issues(contrast(m, c)).exists {
      case ContrastCompatibilityError.SpatialConvention(_, _) => true; case _ => false
    })
  }

  test("no selected scores and a one-sided failure preserve all focal rows") {
    val m      = evaluated(values = Vector.empty).meanByLeft(FailurePolicy.RequireAll)
    val c      = reduced()
    val result = get(contrast(m, c))
    assertEquals(result.rows.size, 2)
    assertEquals(
      result.rows.head.matched.map(_.result),
      Some(Left(ReductionError.NoSelectedScores("a")))
    )
    assertEquals(
      result.rows.head.difference,
      Left(
        ContrastRowError.ReductionFailures(
          "a",
          Some(ReductionError.NoSelectedScores("a")),
          None
        )
      )
    )
    assert(result.rows.head.control.exists(_.result.isRight))
  }
