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

class RepetitionSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)
  private val CosineTolerance               = 1e-12
  private val grid          = get(Grid.over(get(Frame.screen("repetition", 2, 2)), 2, 2))
  private val participant   = Projection.named[String, String]("participant")(_.split("/")(0))
  private val stimulus      = Projection.named[String, String]("stimulus")(_.split("/")(1))
  private val occasion      = Projection.named[String, String]("occasion")(_.split("/")(2))
  private val inputs        = ContentHash.ofString("repetition-fixture")
  private val comparison    = Distribution.cosine[Px]
  private val specification = get(
    EvaluationSpec.of(
      "cosine",
      "1",
      Vector.empty,
      Vector("value"),
      EvaluationGeometry.onGrid(grid),
      EvaluationTime.OrderFree
    )
  )
  private val trials = Trials(RepetitionReference.rows.map { r =>
    val mass = get(
      Surface
        .intensity(grid, IArray.from(r.weights), Provenance.raw(inputs))
        .flatMap(_.normalised)
    )
    Trial(r.id, (), mass)
  })
  private def design(selection: Selection = Selection.All) =
    RepetitionDesign.withinParticipant(participant, stimulus, occasion, selection)

  test("public repetition agrees with independent rational pairs and per-focal contrasts") {
    val result  = design().evaluate(trials, inputs, comparison, specification)
    val reduced = get(result.contrast[SignedDifference](FailurePolicy.RequireAll))
    assertEquals(
      result.matched.rows.map(r => r.left -> r.right),
      RepetitionReference.rows.flatMap(r => r.targets.map(r.id -> _))
    )
    assertEquals(
      result.controls.rows.map(r => r.left -> r.right),
      RepetitionReference.rows.flatMap(r => r.controls.map(r.id -> _))
    )
    assertEquals(result.matched.diagnostics.eligiblePairCount, 12L)
    assertEquals(result.controls.diagnostics.eligiblePairCount, 24L)
    reduced.rows.foreach { r =>
      val expected = RepetitionReference.rows.find(_.id == r.key).get
      assertEqualsDouble(get(r.matched.get.result).value, expected.target, CosineTolerance)
      assertEqualsDouble(get(r.control.get.result).value, expected.control, CosineTolerance)
      assertEqualsDouble(
        get(r.difference).value,
        expected.target - expected.control,
        CosineTolerance
      )
      assertEquals(r.matched.get.contributing, 1)
      assertEquals(r.control.get.contributing, 2)
    }
  }

  test("pinned eyesim phase-only means describe a different, cross-participant estimand") {
    val same      = Pairing.within[String].sameOn(occasion).excludingSelf.directed
    val other     = Pairing.within[String].differentOn(occasion).excludingSelf.directed
    val sameMeans =
      evaluatePairs(pair(trials, same), inputs, comparison).meanByLeft(FailurePolicy.RequireAll)
    val otherMeans = evaluatePairs(pair(trials, other), inputs, comparison).meanByLeft(
      FailurePolicy.RequireAll
    )
    RepetitionReference.rows.foreach { expected =>
      val s = sameMeans.entries.find(_.key == expected.id).get
      val o = otherMeans.entries.find(_.key == expected.id).get
      assertEqualsDouble(get(s.result).value, expected.referenceSame, CosineTolerance)
      assertEqualsDouble(get(o.result).value, expected.referenceOther, CosineTolerance)
      assertEquals(s.contributing, 5)
      assertEquals(o.contributing, 6)
    }
    assert(
      RepetitionReference.rows.exists(r =>
        math.abs(r.target - r.referenceSame) > CosineTolerance
      )
    )
  }

  test("finite-cap controls exclude targets before sampling and are stable under reordering") {
    def selected(cap: Int, t: Trials[String, Unit, Mass[Px]]) =
      design(
        Selection.BottomK(get(PairLimit.of(cap)), Seed(19), SampleId("repetition-controls"))
      )
        .evaluate(t, inputs, comparison, specification)
    val one = selected(1, trials)
    val two = selected(2, trials)
    assertEquals(one.controls.diagnostics.eligiblePairCount, 24L)
    assertEquals(one.controls.diagnostics.selectedPairCount, 12)
    assertEquals(one.controls.rows.groupBy(_.left).values.map(_.size).toSet, Set(1))
    val edges = one.controls.rows.map(r => r.left -> r.right).toSet
    assert(edges.subsetOf(two.controls.rows.map(r => r.left -> r.right).toSet))
    assertEquals(
      edges,
      selected(1, Trials(trials.rows.reverse)).controls.rows.map(r => r.left -> r.right).toSet
    )
    one.controls.rows.foreach { r =>
      val expected = RepetitionReference.rows.find(_.id == r.left).get
      assert(expected.controls.contains(r.right))
      assert(!expected.targets.contains(r.right))
    }
  }

  test("duplicate full keys are excluded in full; missing and empty strata stay explicit") {
    val duplicated = Trials(trials.rows :+ trials.rows.head)
    val result     = design().evaluate(duplicated, inputs, comparison, specification)
    assert(result.matched.diagnostics.ambiguous.nonEmpty)
    assert(
      result.matched.rows.forall(r =>
        r.left != trials.rows.head.key && r.right != trials.rows.head.key
      )
    )
    assert(result.matched.diagnostics.unmatchedLeft.contains("s1/a/recall"))
    val single =
      design().evaluate(Trials(trials.rows.take(1)), inputs, comparison, specification)
    assertEquals(single.matched.rows, Vector.empty)
    assertEquals(single.matched.diagnostics.unmatchedLeft, Vector(trials.rows.head.key))
    val empty =
      design().evaluate(Trials(trials.rows.take(0)), inputs, comparison, specification)
    assertEquals(empty.controls.diagnostics.eligiblePairCount, 0L)
  }

  test("failed comparisons remain pair outcomes and retain realized reduction denominators") {
    val failing = new Compare[Mass[Px], Mass[Px], Similarity]:
      val info                                                                = comparison.info
      def compare(a: Mass[Px], b: Mass[Px]): Either[CompareError, Similarity] =
        if b eq trials.rows.head.value then
          Left(CompareError.ConstantInput("injected", CompareOperand.Right))
        else comparison.compare(a, b)
    val result = design().evaluate(trials, inputs, failing, specification)
    assertEquals(result.controls.rows.size, 24)
    val strict     = get(result.contrast[SignedDifference](FailurePolicy.RequireAll))
    val permissive =
      get(result.contrast[SignedDifference](get(FailurePolicy.successfulOnly(1))))
    val strictRow = strict.control.entries.find(_.key == "s1/b/recall").get
    val accepted  = permissive.control.entries.find(_.key == "s1/b/recall").get
    assert(strictRow.result.isLeft)
    assertEquals(
      (accepted.selected, accepted.successful, accepted.failed, accepted.contributing),
      (2, 1, 1, 1)
    )
  }

  test("convenience evaluation is identical to explicit pair and evaluate composition") {
    val d      = design()
    val info   = EvaluationInfo.comparison(comparison, specification)
    val result = d.evaluate(trials, inputs, comparison, specification)
    val manual = evaluatePairs(pair(trials, d.controls), inputs, info)(comparison.compare)
    assertEquals(result.controls.rows, manual.rows)
    assertEquals(result.controls.diagnostics, manual.diagnostics)
    assertEquals(result.controls.provenance, manual.provenance)
  }
