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

package eyes4s.results

import eyes4s.plan.*

/** The code table of the results module: report findings (the `report`
  * family, warnings keyed to trials, participants and groups), report and
  * specification refusals, covariate refusals and result-table refusals.
  * Codes are unique across this table and every other catalog, and codes
  * are only ever appended.
  */
object ResultsDiagnosticCatalog:
  import DiagnosticFamily.error

  val report: DiagnosticFamily = DiagnosticFamily.of("report", DiagnosticSeverity.Warning)(
    "EmptyGroup",
    "UnpairedParticipant",
    "MissingCovariate",
    "UnknownPredicate",
    "BelowMinimum",
    "UndefinedWindowShare",
    "CovariateType"
  )
  val reportError: DiagnosticFamily = error("report-error")(
    "UnknownScale",
    "ScaleMismatch",
    "UnknownComponent",
    "UndescribedScores",
    "UnknownCovariate",
    "CovariateMismatch",
    "PlanMismatch",
    "InputMismatch",
    "StaleBinding",
    "MalformedDigest",
    "DuplicateQuery",
    "ComponentCount",
    "BlankParticipant",
    "InvalidQuery",
    "InconsistentAccounting",
    "InconsistentCell",
    "Components",
    "UnboundCovariates",
    // bd-01M43VP5YV6WB4VVQEW2CJTB9V review: host query tables out of order, or
    // declaring other covariates than their covariate table.
    "TableOrder",
    "TableCovariates"
  )
  val reportSpec: DiagnosticFamily = error("report-spec")(
    "BlankId",
    "NegativeScale",
    "InvalidSelection",
    "InvalidMinimum",
    "NonFiniteThreshold",
    "EmptyLevels",
    "UndeclaredLevel",
    "InvalidBin",
    "InvalidBins",
    "DuplicateGrouping",
    "CovariateDeclarations",
    "ContrastTerm",
    "ContrastLevels"
  )
  val covariate: DiagnosticFamily = error("covariate")(
    "BlankName",
    "BlankUnit",
    "InvalidLevels",
    "DuplicateCovariate",
    "DuplicateKey",
    "UnknownAttribute",
    "IncompatibleKind",
    "NoTrialProjection"
  )
  val resultTable: DiagnosticFamily = error("result-table")(
    "Schema",
    "Cell",
    "Width",
    "Context"
  )

  // ---------------------------------------------------------------- appended by UI-G
  val reportNavigation: DiagnosticFamily = error("report-navigation")(
    "NegativeScale",
    "BlankComponent",
    "BlankParticipant",
    "ScaleMismatch",
    "UnknownCell",
    "NotInCell",
    "WrongLevel",
    "NotAMember",
    "QueryCount",
    "UnlistedParticipant"
  )

  /** Every family, in the order documented. */
  val families: Vector[DiagnosticFamily] =
    Vector(report, reportError, reportSpec, covariate, resultTable, reportNavigation)

  /** Every stable code, in catalog order. */
  val codes: Vector[DiagnosticCode] = families.flatMap(_.codes)

/** The results module's [[eyes4s.plan.Diagnose Diagnose]] instances. Import `ResultsDiagnostics.given`
  * for `Diagnostic.of` over its families. A finding or refusal that names a
  * trial has the subject `Locus.Trial`; one that names a participant or a
  * group has `Locus.Participant` or `Locus.Group`.
  */
object ResultsDiagnostics:
  import ResultsDiagnosticCatalog as C

  private given DiagnosticOperand[GroupKey, Nothing] =
    DiagnosticOperand.of(g =>
      Operand.Items(
        g.levels.map((t, l) =>
          Operand.Fields(Vector("term" -> Operand.Name(t), "level" -> Operand.Name(l)))
        )
      )
    )
  private given DiagnosticOperand[Role, Nothing] =
    DiagnosticOperand.of(r => Operand.Token(r.toString))
  private given DiagnosticOperand[WindowMeasure, Nothing] =
    DiagnosticOperand.of(m => Operand.Token(Term.window(m)))

  given report[K]: Diagnose[ReportFinding[K], K] =
    given DiagnosticOperand[K, K] = DiagnosticOperand.key[K]
    Diagnose.derived[ReportFinding[K], K](C.report, findingSubject[K])(_.message)

  given reportError[K]: Diagnose[ReportError[K], K] =
    given DiagnosticOperand[K, K] = DiagnosticOperand.key[K]
    Diagnose.derived[ReportError[K], K](C.reportError, errorSubject[K])(_.message)

  given reportSpec: Diagnose[SpecError, Nothing] =
    Diagnose.derived[SpecError, Nothing](C.reportSpec)(_.message)

  given covariate[K]: Diagnose[CovariateError[K], K] =
    given DiagnosticOperand[K, K] = DiagnosticOperand.key[K]
    Diagnose.derived[CovariateError[K], K](
      C.covariate,
      (e: CovariateError[K]) =>
        e match
          case CovariateError.DuplicateKey(key) => Vector(Locus.Trial(key))
          case _                                => Vector.empty
    )(_.message)

  given resultTable: Diagnose[ResultTableError, Nothing] =
    Diagnose.derived[ResultTableError, Nothing](C.resultTable)(_.message)

  given reportNavigation[K]: Diagnose[ReportNavigationError[K], K] =
    given DiagnosticOperand[K, K] = DiagnosticOperand.key[K]
    import NavigationError.resultRefOperand
    Diagnose.derived[ReportNavigationError[K], K](C.reportNavigation, navigationSubject[K])(
      _.message
    )

  private def navigationSubject[K](error: ReportNavigationError[K]): Vector[Locus[K]] =
    error match
      case ReportNavigationError.UnknownCell(g, _, _)  => Vector(group(g))
      case ReportNavigationError.NotInCell(p, g, _, _) => Vector(group(g), Locus.Participant(p))
      case ReportNavigationError.QueryCount(p, _, _, g, _, _) =>
        Vector(group(g), Locus.Participant(p))
      case ReportNavigationError.UnlistedParticipant(key, _, g, _, _) =>
        Vector(group(g), Locus.Trial(key))
      case ReportNavigationError.NotAMember(key, g, _, _) => Vector(group(g), Locus.Trial(key))
      case ReportNavigationError.ScaleMismatch(ref, _)    => Vector(Locus.Scale(ref))
      case ReportNavigationError.WrongLevel(ref, _)       => ref.loci
      case _                                              => Vector.empty

  private def group(g: GroupKey): Locus[Nothing] = Locus.Group(g.levels)

  private def findingSubject[K](finding: ReportFinding[K]): Vector[Locus[K]] = finding match
    case ReportFinding.EmptyGroup(g, _)                => Vector(group(g))
    case ReportFinding.UnpairedParticipant(p, s, _, _) =>
      Vector(group(s), Locus.Participant(p))
    case ReportFinding.MissingCovariate(key, _)     => Vector(Locus.Trial(key))
    case ReportFinding.UnknownPredicate(key, _)     => Vector(Locus.Trial(key))
    case ReportFinding.BelowMinimum(p, g, _, _, _)  => Vector(group(g), Locus.Participant(p))
    case ReportFinding.UndefinedWindowShare(key, _) => Vector(Locus.Trial(key))
    case ReportFinding.CovariateType(key, _, _, _)  => Vector(Locus.Trial(key))

  private def errorSubject[K](error: ReportError[K]): Vector[Locus[K]] = error match
    case ReportError.DuplicateQuery(key)       => Vector(Locus.Trial(key))
    case ReportError.ComponentCount(key, _, _) => Vector(Locus.Trial(key))
    case ReportError.BlankParticipant(key)     => Vector(Locus.Trial(key))
    case ReportError.InvalidQuery(key, _)      => Vector(Locus.Trial(key))
    case ReportError.UnknownScale(scale, _)    => Vector(Locus.Scale(scale))
    case ReportError.TableOrder(_, scale)      => Vector(Locus.Scale(scale))
    case _                                     => Vector.empty
