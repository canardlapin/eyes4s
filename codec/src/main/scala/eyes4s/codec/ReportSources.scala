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
    val table = covariateTable(plan, input, ledger, covariates)
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

  /** The covariates of `input`'s keys from `ledger`'s trials table, when
    * `covariates` declares any; none is needed otherwise.
    */
  private def covariateTable[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      input: StudyInput[K, U],
      ledger: Option[AdmissionLedger[K]],
      covariates: CovariateSchema
  ): Either[CodecError, Option[CovariateTable[K]]] =
    val keys = input.trials.rows.map(_.key)
    ledger match
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

  /** Re-evaluate a stored report over the decoded documents its binding was
    * verified against, and refuse it unless it is exactly what the shipped
    * reduction computes: the first differing cell is named, or else the first
    * differing part. Only window tallies and a linear reduction are
    * recomputed; no pair is scored.
    *
    * No type is assumed shared between the plan and the result: the plan
    * supplies the layout, window tallies and covariates, and the result is
    * reduced through its own method's score schema.
    */
  private[codec] def reevaluate[K, U <: Unit2D](
      plan: LoadedStudy[K, U],
      input: StudyInput[K, U],
      result: LoadedResult[K, U],
      ledger: Option[AdmissionLedger[K]],
      stored: Report[K]
  ): Option[RelationMismatch] =
    // The result is read with its own method's components: the plan's
    // parameters, re-read by the result codec's parameter codec, give the
    // result method's score schema, typed by the result's scores. The plan's
    // and the result's methods must name the same components.
    val planComponents = ScoreSchema.study(plan.plan).map(_.ids)
    val resultSchema   = plan.parametersDocument.flatMap(result.scoreSchema)
    val components     = (planComponents, resultSchema) match
      case (Right(planned), Right(schema)) if planned != schema.ids =>
        Some(RelationMismatch.ReportComponents(planned, schema.ids))
      case _ => None
    lazy val recomputed = for
      schema     <- resultSchema
      covariates <- CovariateSchema
        .of(stored.spec.covariates)
        .left
        .map(e => CodecError.Covariates(e))
      table <- covariateTable(plan.plan, input, ledger, covariates)
      source = ReportSource.of(
        result.result,
        plan.plan.layout,
        plan.plan.windowTallies(input),
        table,
        schema,
        stored.binding
      )
      report <- Report.evaluate(stored.spec, source).left.map(CodecError.Report.apply)
    yield report
    components.orElse(recomputed match
      case Left(error) =>
        Some(
          RelationMismatch.ReportRecomputed("evaluation", "the stored report", error.message)
        )
      case Right(fresh) if fresh == stored => None
      case Right(fresh)                    => Some(difference(stored, fresh)))

  /** The first cell (by position) that differs, else the first other part. */
  private def difference[K](stored: Report[K], fresh: Report[K]): RelationMismatch =
    val size  = math.max(stored.cells.size, fresh.cells.size)
    val cells = (0 until size).iterator
      .map(i => (stored.cells.lift(i), fresh.cells.lift(i)))
      .collectFirst {
        case (a, b) if a != b =>
          val at = a.orElse(b).get
          RelationMismatch.ReportCell(
            at.group.render,
            at.role.toString,
            at.component,
            a.fold("no cell")(_.toString),
            b.fold("no cell")(_.toString)
          )
      }
    cells.getOrElse {
      def part[A](name: String, a: A, b: A)(using CanEqual[A, A]) =
        Option.when(a != b)(RelationMismatch.ReportRecomputed(name, a.toString, b.toString))
      part("groups", stored.groups, fresh.groups)
        .orElse(part("contrasts", stored.contrasts, fresh.contrasts))
        .orElse(part("accounting", stored.accounting, fresh.accounting))
        .orElse(part("findings", stored.findings, fresh.findings))
        .getOrElse(
          RelationMismatch.ReportRecomputed("report", stored.toString, fresh.toString)
        )
    }
