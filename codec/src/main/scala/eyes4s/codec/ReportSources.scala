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

package eyes4s.codec

import eyes4s.kernel.Unit2D
import eyes4s.plan.*
import eyes4s.results.*

/** The public way to obtain a bound [[eyes4s.results.ReportSource]]: the
  * binding is computed from the very plan, input, result and ledger the
  * source reads, never supplied by the caller.
  */
object ReportSources:
  /** A source over `result`, which `plan` computed on `input`, bound to the
    * canonical digests of all three and, when `ledger` is given, of the
    * ledger. The ledger must be consistent with the input
    * (`AdmissionLedger.checkAgainst`); the covariates declared by
    * `covariates` are read from its trials table, which it must then carry,
    * each study key joined to its trial through the layout. Without a ledger the
    * source has no covariates and a report that reads any is refused
    * (`ReportError.UnboundCovariates`).
    */
  def study[K, U <: Unit2D, P, S, D](
      plans: StudyCodec[K, U, P, S, D],
      inputs: StudyInputCodec[K, U],
      results: StudyResultCodec[K, U, P, S, D]
  )(
      plan: StudyPlan[K, U, P, S, D],
      input: StudyInput[K, U],
      result: StudyResult[K, U, S, D],
      ledger: Option[AdmissionLedger[K]],
      covariates: CovariateSchema
  ): Either[CodecError, ReportSource[K]] =
    val keys                                                 = input.trials.rows.map(_.key)
    val table: Either[CodecError, Option[CovariateTable[K]]] = ledger match
      case None =>
        Either.cond(
          covariates.covariates.isEmpty,
          None,
          CodecError.Report(ReportError.UnboundCovariates(covariates.names.map(_.value)))
        )
      case Some(l) =>
        l.checkAgainst(input).left.map(CodecError.Admission.apply).flatMap { _ =>
          (l.inventory, covariates.covariates.isEmpty) match
            case (_, true)     => Right(None)
            case (None, false) =>
              Left(
                CodecError.Report(
                  ReportError.UnboundCovariates(covariates.names.map(_.value))
                )
              )
            case (Some(inventory), false) =>
              CovariateTable
                .forKeys(covariates, inventory, plan.layout, keys)
                .left
                .map(CodecError.Covariates.apply)
                .map(Some(_))
        }
    for
      found   <- table
      binding <- ReportCodecs.binding(
        (plans.codec, plan),
        (inputs.input, input),
        (results.codec, result),
        ledger.map(l => (inputs.ledger, l))
      )
      source <- ReportSource
        .study(plan, input, result, found, binding)
        .left
        .map(CodecError.Report.apply)
    yield source
