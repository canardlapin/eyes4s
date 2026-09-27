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

import eyes4s.plan.*
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** Exact integer laws for progress; no floating-point tolerance applies. */
object StageMeterLaws extends Laws:
  def bounds: RuleSet =
    val counts = Gen.chooseNum(0L, Long.MaxValue - 1L)
    val kind   = StageKind.Comparing
    val unit   = CountUnit.Pairs
    new DefaultRuleSet(
      name = "stageMeter",
      parent = None,
      "completed <= total" -> forAll(counts) { n =>
        Vector(SegmentTotal.Exact(n), SegmentTotal.AtMost(n)).forall { total =>
          StageMeter.of(kind, unit, n, total).exists(_.done == n) &&
          StageMeter.of(kind, unit, n + 1L, total).isLeft
        }
      },
      "negative completed counts refused" -> forAll(Gen.chooseNum(Long.MinValue, -1L)) { n =>
        StageMeter.of(kind, unit, n, SegmentTotal.Counting).isLeft
      },
      "done is monotone" -> forAll(counts) { n =>
        StageMeter.of(kind, unit, n + 1L, SegmentTotal.Counting).exists { meter =>
          meter.advanceTo(n).isLeft &&
          meter.advanceTo(n + 1L).exists(_.done == n + 1L)
        }
      },
      "counting resolves without changing completed objects" -> forAll(counts) { n =>
        StageMeter.of(kind, unit, n, SegmentTotal.Counting).exists { meter =>
          meter.advanceTo(n, SegmentTotal.Exact(n)).exists { resolved =>
            resolved.done == n && resolved.total == SegmentTotal.Exact(n)
          }
        }
      }
    )
