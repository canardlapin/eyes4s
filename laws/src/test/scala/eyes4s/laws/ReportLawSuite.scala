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

import eyes4s.codec.*
import eyes4s.plan.{DefinitionId, StudyKey}
import eyes4s.results.*
import org.scalacheck.Test

/** The published report laws over the shipped reduction, the report codecs'
  * round trips, and mutants of the reduction each law set must refuse.
  */
class ReportLawSuite extends munit.DisciplineSuite:
  import ReportGenerators.*

  private val keys    = StudyCodecs.key(DefinitionId.studyKey)
  private val reports = ReportCodecs.report(keys)

  checkAll(
    "report",
    ReportLaws.reduction(ReportLaws.shipped, genCase, witness, ReportLaws.arithmetic)
  )
  checkAll("report spec", CodecLaws.roundTrip(ReportCodecs.reportSpec, genSpec, _ == _))
  checkAll("study report", CodecLaws.roundTrip(reports, genReport, _ == _))
  checkAll(
    "covariate schema",
    CodecLaws.roundTrip(ReportCodecs.covariates, genCovariateSchema, _ == _)
  )

  // -------------------------------------------------------------------------
  // Mutants
  // -------------------------------------------------------------------------

  /** Mutant checks run from a fixed seed, so every kill is reproducible evidence. */
  private val killParameters =
    Test.Parameters.default.withMinSuccessfulTests(100).withInitialSeed(0x5245504f5254L)

  private def laws(reduce: ReportLaws.Reduce) =
    ReportLaws.reduction(reduce, genCase, witness, ReportLaws.arithmetic).all.properties

  private def killedBy(reduce: ReportLaws.Reduce): Vector[String] =
    laws(reduce).toVector.collect {
      case (name, prop) if (Test.check(killParameters, prop).status match
            case Test.Failed(_, _) | Test.PropException(_, _, _) => true
            case _                                               => false
          ) =>
        name
    }

  /** A reduction whose report the mutation rewrites before it is rebuilt. */
  private def rewritten(
      spec: ReportSpec,
      table: QueryTable[StudyKey]
  )(
      change: Report[StudyKey] => Report[StudyKey]
  ): Either[ReportError[StudyKey], Report[StudyKey]] =
    ReportLaws.shipped(spec, table).map(change)

  private def rebuild(
      r: Report[StudyKey],
      cells: Vector[Cell[StudyKey]] = Vector.empty,
      accounting: Vector[Accounting] = Vector.empty
  ): Report[StudyKey] =
    Report
      .reconstruct(
        r.spec,
        r.binding,
        r.groups,
        if cells.isEmpty then r.cells else cells,
        r.contrasts,
        if accounting.isEmpty then r.accounting else accounting,
        r.findings
      )
      .getOrElse(r)

  test("the shipped reduction passes every law outright") {
    assertEquals(killedBy(ReportLaws.shipped), Vector.empty)
  }

  test("a reduction that pools queries under participant means is refused") {
    val pooled: ReportLaws.Reduce = (spec, table) =>
      ReportLaws.shipped(spec, table).flatMap { r =>
        ReportSpec
          .of(
            spec.id,
            spec.scale,
            spec.selection,
            spec.filter,
            spec.groupBy,
            ReducePolicy.PooledQueries,
            spec.contrast,
            spec.spread
          )
          .toOption
          .flatMap(p => ReportLaws.shipped(p, table).toOption)
          .map(p =>
            rebuild(r, cells = p.cells.map(c => c.copy(perParticipant = c.perParticipant)))
          )
          .toRight(ReportError.UnknownScale(0, 0))
      }
    assert(
      killedBy(pooled).contains("report.participant means give each participant equal weight")
    )
  }

  test("a reduction that reports a missing estimate as zero is refused") {
    val zero: ReportLaws.Reduce = (spec, table) =>
      rewritten(spec, table)(r =>
        rebuild(
          r,
          cells = r.cells.map(c =>
            if c.queries > 0 && !c.estimate.isPresent then c.copy(estimate = Value.Present(0.0))
            else c
          )
        )
      )
    assert(killedBy(zero).contains("report.a missing value is never a zero"))
  }

  test("a reduction that forgets the queries its filter could not decide is refused") {
    val forgetful: ReportLaws.Reduce = (spec, table) =>
      rewritten(spec, table)(r =>
        rebuild(
          r,
          accounting = r.accounting.flatMap(a =>
            Accounting
              .of(
                a.role,
                a.eligible - a.unknownPredicate,
                a.kept,
                a.filteredOut,
                0,
                a.failed,
                a.missingGroupAttribute,
                a.belowMinimum
              )
              .toOption
          )
        )
      )
    assert(killedBy(forgetful).contains("report.every eligible query is accounted for once"))
  }

  test("a report that keeps the stale binding of another result is refused") {
    val r     = ReportLaws.shipped(witness.spec, witness.table).toOption.get
    val other =
      ReportLaws.binding.copy(result = BindingDigest.parse("result", "e" * 64).toOption.get)
    assert(r.checkCurrent(other).isLeft)
    assert(Report.evaluate(witness.spec, source(other), ReportLaws.binding).isLeft)
  }

  /** A source that serves the witness table under `binding`. */
  private def source(binding: ReportBinding): ReportSource[StudyKey] =
    ReportSource.fromQueries(Vector(witness.table), binding)
