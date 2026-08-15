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

import eyes4s.core.OccupancyResult
import eyes4s.kernel.Unit2D

import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** Conservation laws for a trajectory's temporal-support accounting. */
trait TemporalSupportLaws extends Laws:

  def accounting[U <: Unit2D](
      results: Gen[OccupancyResult[U]],
      tolerance: Tolerance
  ): RuleSet =
    new SimpleRuleSet(
      "temporalSupport.accounting",
      "measure mass equals analysable duration" -> forAll(results) { result =>
        Prop(tolerance.approxEquals(result.measure.total, result.analysableTime.toSeconds)) :|
          s"mass=${result.measure.total}, analysable=${result.analysableTime.toSeconds}, policy=${result.policy.render}"
      },
      "accounted times are non-negative" -> forAll(results) { result =>
        Prop(!result.analysableTime.isNegative && !result.censoredTime.isNegative)
      }
    )

end TemporalSupportLaws

object TemporalSupportLaws extends TemporalSupportLaws
