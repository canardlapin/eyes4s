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

package eyes4s.repetitionconsumer

import eyes4s.compare.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

class RepetitionMethodsSuite extends munit.FunSuite:
  private def get[E, A](x: Either[E, A]): A = x.fold(e => fail(e.toString), identity)
  private val MeanTolerance                 = 1e-12
  private val grid      = get(Grid.over(get(Frame.screen("repetition-methods", 2, 2)), 2, 2))
  private val rows      = RepetitionMethodsReference.rows
  private val byKey     = rows.map(r => r.key -> r).toMap
  private val condition = Projection.named[String, String]("occasion")(k => byKey(k).occasion)
  private val inputs    = ContentHash.ofString("repetition-methods")
  private def mass(v: Vector[Double]) = get(
    Surface.mass(grid, IArray.from(v), Provenance.raw(ContentHash.of(IArray.from(v))))
  )
  private val trials    = Trials(rows.map(r => Trial(r.key, (), mass(r.values))))
  private val scaleMaps = rows.map { r =>
    r.key -> get(
      MapScales.of(
        Vector(get(Sigma.px(1)) -> mass(r.scale1), get(Sigma.px(2)) -> mass(r.values))
      )
    )
  }.toMap
  private val successful = get(FailurePolicy.successfulOnly(1))

  RepetitionMethodsReference.methods.foreach { (method, expected) =>
    test(
      s"$method: directed same/other-condition means and endpoints agree with the independent Cartesian oracle and R"
    ) {
      val instance = get(MapSimilarityMethod.fromReference(method)).instance[Px]
      Vector(true, false).foreach { same =>
        val design = if same then
          Pairing.within[String].sameOn(condition).excludingSelf.directed
        else Pairing.within[String].differentOn(condition).excludingSelf.directed
        val pairs = pair(trials, design)
        val want  = for
          a <- rows; b <- rows if a.key != b.key && ((a.occasion == b.occasion) == same)
        yield a.key -> b.key
        assertEquals(pairs.pairs.map((a, b) => a.key -> b.key), want)
        val analysis = evaluatePairs(pairs, inputs, instance)
        val reduced  = analysis.meanByLeft(FailurePolicy.RequireAll)
        reduced.entries.foreach { row =>
          val target = expected.find(_.key == row.key).get
          val value  = if same then target.same else target.other
          value match
            case Some(v) => assertEqualsDouble(get(row.result).value, v, MeanTolerance)
            case None    => assert(row.result.isLeft)
          assertEquals(row.selected, want.count(_._1 == row.key))
          assertEquals(row.contributing, row.successful)
        }
        assertEquals(analysis.diagnostics.selectedPairCount, want.size)
        val duplicate =
          evaluatePairs(pair(Trials(trials.rows :+ trials.rows.head), design), inputs, instance)
        assert(duplicate.diagnostics.ambiguous.nonEmpty)
        assert(!duplicate.rows.exists(r => r.left == rows.head.key || r.right == rows.head.key))
        assert(
          evaluatePairs(
            pair(Trials(trials.rows.take(1)), design),
            inputs,
            instance
          ).rows.isEmpty
        )
      }
    }
    test(
      s"$method: nested available-scale mean preserves every row and differs from pooled averaging when support varies"
    ) {
      val vocabulary = get(MapSimilarityMethod.fromReference(method))
      rows.foreach { focal =>
        Vector(true, false).foreach { same =>
          val candidates =
            rows.filter(r => r.key != focal.key && ((r.occasion == focal.occasion) == same))
          val comparisons = candidates.map { other =>
            RepetitionAggregation.scales(
              MapComparison.scales(scaleMaps(focal.key), scaleMaps(other.key), vocabulary),
              successful
            )
          }
          val nested = RepetitionAggregation.comparisons(comparisons, successful)
          val target = expected.find(_.key == focal.key).get
          val value  = if same then target.nestedSame else target.nestedOther
          value match
            case Some(v) => assertEqualsDouble(get(nested.result).value, v, MeanTolerance)
            case None    => assert(nested.result.isLeft)
          assertEquals(nested.requested, candidates.size)
          assertEquals(nested.requestedScales, 2 * candidates.size)
          assertEquals(nested.successfulScales, comparisons.map(_.source.contributing).sum)
          if same then
            target.pooledSame.foreach { pooled =>
              if value.exists(v => math.abs(v - pooled) > 1e-3) then
                assert(math.abs(get(nested.result).value - pooled) > 1e-3)
            }
        }
      }
    }
  }
  test("strict scale and comparison policies retain failures and all empty cases") {
    val a          = scaleMaps(rows.head.key); val b = scaleMaps(rows(1).key)
    val comparison = MapComparison.scales(a, b, MapSimilarityMethod.Pearson)
    val strict     = RepetitionAggregation.scales(comparison, FailurePolicy.RequireAll)
    val partial    = RepetitionAggregation.scales(comparison, successful)
    assertEquals(strict.requested, 2); assertEquals(strict.successful, 1);
    assertEquals(strict.contributing, 0)
    assert(strict.result.isLeft); assertEquals(partial.contributing, 1)
    assertEquals(partial.source.rows.size, 2)
    val nested =
      RepetitionAggregation.comparisons(Vector(strict, partial), FailurePolicy.RequireAll)
    assertEquals(nested.requested, 2); assertEquals(nested.successful, 1);
    assertEquals(nested.contributing, 0)
    assert(nested.result.isLeft)
    assert(
      RepetitionAggregation
        .comparisons(Vector.empty[RepetitionScaleMean[Px]], successful)
        .result
        .isLeft
    )
    assert(
      RepetitionAggregation
        .scales(comparison, get(FailurePolicy.successfulOnly(2)))
        .result
        .isLeft
    )
    assert(MapSimilarityMethod.fromReference("emd").isLeft)
  }
