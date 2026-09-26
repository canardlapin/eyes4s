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

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.compare.Distribution

class FiniteControlReferenceSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val ControlTolerance                  = 1e-12
  final case class Key(occurrence: String, matched: String, stratum: String) derives CanEqual
  private given KeyDigest[Key] = KeyDigest.derived[Key]
  private val matchKey         = Projection.named[Key, String]("matched")(_.matched)
  private val stratum          = Projection.named[Key, String]("stratum")(_.stratum)
  private val relation         =
    Pairing.between[Key, Key].sameOn(stratum, stratum).differentOn(matchKey, matchKey)
  private val grid = get(Grid.over(get(Frame.screen("controls", 2, 1)), 2, 1))
  private def mass(values: Vector[Double]): Mass[Px] =
    val v = IArray.from(values.map(_ / values.sum))
    get(Surface.mass(grid, v, Provenance.raw(ContentHash.of(v))))
  private val rows = FiniteControlReference.sources.map(r =>
    Trial(Key(r.key, r.matched, r.stratum), (), mass(r.values))
  )
  private val trials = Trials(rows)

  test(
    "finite native controls exclude every true-match copy, retain multiplicity and count before selection"
  ) {
    Vector(1, 20).foreach { cap =>
      val design   = get(relation.bottomK(cap, Seed(42), SampleId("finite-controls")))
      val paired   = pair(trials, trials, design)
      val eligible = rows.map(focal =>
        rows.count(r =>
          r.key.stratum == focal.key.stratum && r.key.matched != focal.key.matched
        )
      )
      assertEquals(paired.eligiblePairCount, eligible.map(_.toLong).sum)
      rows.zip(eligible).foreach { (focal, n) =>
        val selected = paired.pairs.filter(_._1.key == focal.key).map(_._2)
        assertEquals(selected.size, math.min(cap, n))
        assert(
          selected.forall(r =>
            r.key.matched != focal.key.matched && r.key.stratum == focal.key.stratum
          )
        )
      }
      val reversed = pair(Trials(rows.reverse), Trials(rows.reverse), design)
      assertEquals(
        paired.pairs.map((l, r) => l.key -> r.key).toSet,
        reversed.pairs.map((l, r) => l.key -> r.key).toSet
      )
    }
    val exhaustive =
      pair(trials, trials, get(relation.bottomK(20, Seed(42), SampleId("finite-controls"))))
    assertEquals(
      exhaustive.pairs.filter(_._1.key.occurrence == "b1").map(_._2.key.occurrence).toSet,
      Set("a1", "a2")
    )
    assert(relation.bottomK(0, Seed(42), SampleId("finite-controls")).isLeft)
    assert(relation.bottomK(-1, Seed(42), SampleId("finite-controls")).isLeft)
  }

  test(
    "pinned R draws reproduce analytic means but preserve sample-before-exclude divergence"
  ) {
    val references = FiniteControlReference.sources.groupBy(_.matched)
    val order      = FiniteControlReference.sources.map(_.matched).distinct
    FiniteControlReference.results.foreach { r =>
      if r.cap == 0 then
        assertEquals(r.count, None); assertEquals(r.mean, None)
      else
        assertEquals(r.count, Some(r.selected.size))
        val focal  = rows.find(_.key.occurrence == r.key).get.value
        val scores = r.selected.map(index =>
          get(
            Distribution
              .cosine[Px]
              .compare(focal, mass(references(order(index - 1)).head.values))
          ).value
        )
        if scores.isEmpty then assertEquals(r.mean, None)
        else assertEqualsDouble(r.mean.get, scores.sum / scores.size, ControlTolerance)
    }
    val r = FiniteControlReference.results.find(r => r.cap == 20 && r.key == "a1").get
    assertEquals(r.count, Some(2))
    assertEqualsDouble(r.mean.get, 0.5, ControlTolerance)
    val native = pair(
      trials,
      trials,
      get(relation.bottomK(20, Seed(42), SampleId("finite-controls")))
    ).pairs.filter(_._1.key.occurrence == "a1")
    assertEquals(native.size, 1)
    assertEqualsDouble(
      get(Distribution.cosine[Px].compare(native.head._1.value, native.head._2.value)).value,
      0.0,
      ControlTolerance
    )
    assert(FiniteControlReference.results.exists(r => r.cap == 1 && r.count.contains(0)))
  }

  test("seeds can change eligible subsets; duplicate occurrence keys remain ambiguity data") {
    val selections = (1L to 12L).map { seed =>
      pair(
        trials,
        trials,
        get(relation.bottomK(1, Seed(seed), SampleId("finite-controls")))
      ).pairs.filter(_._1.key.occurrence == "b1").map(_._2.key.occurrence).toSet
    }
    assert(selections.distinct.size > 1)
    val ambiguous = pair(
      trials,
      Trials(rows :+ rows.head),
      get(relation.bottomK(20, Seed(42), SampleId("finite-controls")))
    )
    assert(ambiguous.ambiguous.nonEmpty)
    assert(!ambiguous.pairs.exists(_._2.key.occurrence == "a1"))
  }
