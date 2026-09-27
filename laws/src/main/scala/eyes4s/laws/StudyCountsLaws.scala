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
import eyes4s.design.{PairQuantum, WorkQuanta}
import eyes4s.plan.*
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** Exact integer accounting against caller-supplied independent schedule counts. */
object StudyCountsLaws extends Laws:
  final case class Case[K](
      cursor: CountCursor[K],
      matched: Long,
      controls: Long,
      queries: Long,
      maps: Long,
      scales: Int
  )

  def counts[K](cases: Gen[Case[K]]): RuleSet =
    def run(c: Case[K], n: Int) =
      PairQuantum.of(n).flatMap { quantum =>
        Stepwise.complete(c.cursor, WorkQuanta(quantum, ComparisonQuantum.default))
      }
    new DefaultRuleSet(
      name = "studyCounts",
      parent = None,
      "independent matched and control accounting" -> forAll(cases) { c =>
        run(c, 1).exists { result =>
          result.matched.eligiblePairs == c.matched &&
          result.controls.eligiblePairs == c.controls &&
          result.pairRowsPerScale == c.matched + c.controls &&
          result.totalPairs == (c.matched + c.controls) * c.scales
        }
      },
      "query and map accounting" -> forAll(cases) { c =>
        run(c, 3).exists { result =>
          result.eligibleQueries == c.queries && result.totalMaps == c.maps * c.scales
        }
      },
      "counts are invariant to page size" -> forAll(cases, Gen.chooseNum(1, 20)) { (c, n) =>
        (run(c, 1), run(c, n)) match
          case (Right(a), Right(b)) =>
            a.totalPairs == b.totalPairs && a.totalMaps == b.totalMaps &&
            a.matched.unmatchedFocal == b.matched.unmatchedFocal &&
            a.controls.unmatchedReferences == b.controls.unmatchedReferences &&
            a.cardinality.multiple == b.cardinality.multiple &&
            a.cardinality.unmatched == b.cardinality.unmatched
          case _ => false
      }
    )
