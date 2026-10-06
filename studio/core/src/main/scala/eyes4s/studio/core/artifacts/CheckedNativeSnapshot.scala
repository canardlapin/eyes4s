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

package eyes4s.studio.core.artifacts

import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.{AdmissionLedger, ComparisonMethod, StudyInput, TrialKey}
import eyes4s.studio.core.real.{RealExecution, RealPlan}

/** Native values retained only after the package's complete closure and canonical
  * identities have passed verification. No materialization reparses these bytes.
  */
private[core] final class CheckedNativeSnapshot private[artifacts] (
    val plan: RealPlan.Plan,
    val method: ComparisonMethod,
    val input: StudyInput[TrialKey, Px],
    val ledger: AdmissionLedger[TrialKey],
    val result: RealExecution.Result
)
