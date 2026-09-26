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

package eyes4s.compare

import eyes4s.core.*
import eyes4s.kernel.*

enum ScanpathComponent derives CanEqual:
  case VectorShape, Direction, Length, Position, Duration, PositionEmd

enum ScanpathComponentError derives CanEqual:
  case Comparison(error: CompareError)
  case Unavailable(component: ScanpathComponent, reason: String)
  def message: String = this match
    case Comparison(e)     => e.message
    case Unavailable(c, r) => s"Scanpath component $c unavailable: $r"

final class ScanpathComponents private[compare] (
    val values: Vector[(ScanpathComponent, Either[ScanpathComponentError, Similarity])]
)
object ScanpathComparison:
  /** R-baseline cardinality and six named slots. Native MultiMatch's five-component API
    * remains separately available for two-fixation paths. No hidden EMD substitution.
    */
  def baseline[U <: Unit2D](left: Scanpath[U], right: Scanpath[U]): ScanpathComponents =
    val score = for
      _ <- Either.cond(left.n >= 3, (), CompareError.TooShort("left scanpath", left.n, 3))
      _ <- Either.cond(right.n >= 3, (), CompareError.TooShort("right scanpath", right.n, 3))
      s <- MultiMatch[U].compare(left, right)
    yield s
    val selectors = Vector[(ScanpathComponent, MultiMatchScore => Double)](
      ScanpathComponent.VectorShape -> (_.shape),
      ScanpathComponent.Direction   -> (_.direction),
      ScanpathComponent.Length      -> (_.length),
      ScanpathComponent.Position    -> (_.position),
      ScanpathComponent.Duration    -> (_.duration)
    )
    val available = selectors.map { (component, select) =>
      component -> score.left
        .map(ScanpathComponentError.Comparison.apply)
        .flatMap(s =>
          Similarity
            .of(select(s))
            .left
            .map(e =>
              ScanpathComponentError
                .Comparison(CompareError.InvalidScore(component.toString, e))
            )
        )
    }
    new ScanpathComponents(
      available :+ (ScanpathComponent.PositionEmd -> Left(
        ScanpathComponentError.Unavailable(
          ScanpathComponent.PositionEmd,
          "pinned optional transport backend has no native exact-EMD alias; see eyesim-compare decision"
        )
      ))
    )
