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

package eyes4s.studio.core.reports

import cats.syntax.all.*
import io.circe.Codec
import eyes4s.results.*
import eyes4s.studio.core.backend.{
  DroppedCell,
  ReportAbsence,
  ReportCellView,
  ReportContrastView,
  ReportParticipantView,
  ReportQueryRange,
  ReportRole,
  ReportTallyView,
  ReportUndefined,
  ReportUnpairedView,
  ReportView,
  Response,
  RunId,
  TrialKey
}
import eyes4s.studio.core.document.{
  MinimumPerGroup,
  ReportingFilter,
  ReportingSpec,
  ReportingWeight
}
import eyes4s.studio.core.selection.{ReportCount, ReportGroup, ScaleIndex, StudioRef}

/** Why a reporting spec could not be evaluated over a run. Every case names
  * its operands.
  */
enum ReportRefusal derives CanEqual, Codec.AsObject:
  /** eyes4s refused the specification the reporting spec translates to. */
  case Spec(reporting: String, reason: String)

  /** A covariate the spec reads has no declared levels in the run's data. */
  case UndeclaredCovariate(reporting: String, covariate: String, declared: Vector[String])

  /** eyes4s refused to evaluate the specification over the run. */
  case Evaluation(reporting: String, reason: String)

  /** eyes4s refused the run's stored rows as a report source. */
  case Source(reporting: String, reason: String)

  /** A query's stored failure carries `code`, which is not a diagnostic code
    * (`family.name`), so its rows cannot be read.
    */
  case UnreadableFailure(reporting: String, query: TrialKey, code: String)

  def message: String = this match
    case Spec(r, why) => s"The reporting spec $r is not an eyes4s report: $why"
    case UndeclaredCovariate(r, c, declared) =>
      s"The reporting spec $r reads $c, which the run's data does not declare " +
        s"(it declares ${declared.mkString(", ")})."
    case Evaluation(r, why) => s"eyes4s could not evaluate the reporting spec $r: $why"
    case Source(r, why)     =>
      s"The run's stored rows are not an eyes4s report source for the reporting spec $r: $why"
    case UnreadableFailure(r, q, code) =>
      s"Query ${q.participant} ${q.phase.label} ${q.trial} (occurrence ${q.occurrence}) failed " +
        s"with '$code', which is not a diagnostic code, so the reporting spec $r cannot read it."

/** A Studio [[ReportingSpec]] as an eyes4s-results `ReportSpec`, and an
  * eyes4s `Report` as the [[ReportView]] the protocol serves (protocol 1.11,
  * bead bd-01M43VP5YV6WB4VVQEW2CJTB9V). Nothing is computed here: the
  * report is eyes4s's `Report.evaluate` over the run's stored rows.
  */
object ReportEvaluation:

  /** The component every map method's scores carry (`ComparisonMethod.component`). */
  val Component: String = "value"

  private def roleOf(r: Role): ReportRole = r match
    case Role.Matched    => ReportRole.Matched
    case Role.Control    => ReportRole.Control
    case Role.Difference => ReportRole.Difference

  private def categorical(
      reporting: ReportingSpec,
      covariate: String,
      levels: Map[String, Vector[String]]
  ): Either[ReportRefusal, LevelTerm] =
    for
      name   <- CovariateName.of(covariate).leftMap(e => refusal(reporting, e.message))
      values <- levels
        .get(covariate)
        .toRight(
          ReportRefusal
            .UndeclaredCovariate(reporting.id.value, covariate, levels.keys.toVector.sorted)
        )
      declared <- Levels.of(values).leftMap(e => refusal(reporting, e.message))
    yield LevelTerm.Categorical(name, declared)

  private def refusal(reporting: ReportingSpec, why: String) =
    ReportRefusal.Spec(reporting.id.value, why)

  /** The window filter of a spec, if it has one. */
  def windowFilter(reporting: ReportingSpec): Option[Double] =
    reporting.filters.collectFirst { case ReportingFilter.OutsideWindowAtMost(s) => s.value }

  /** Which of a reporting spec's filters an eyes4s specification carries:
    * all of them, or only the outside-window filter (for eyes4s's own count
    * of what that filter alone leaves out and cannot decide).
    */
  enum Filters derives CanEqual:
    case All, WindowOnly

  /** `reporting` at scale `scale` as an eyes4s specification, its categorical
    * covariates declared by `levels` (the run's data), carrying `filters`.
    */
  def spec(
      reporting: ReportingSpec,
      scale: Int,
      levels: Map[String, Vector[String]],
      filters: Filters = Filters.All
  ): Either[ReportRefusal, ReportSpec] =
    val windowOnly                                      = filters == Filters.WindowOnly
    def fail[A](e: SpecError): Either[ReportRefusal, A] = Left(refusal(reporting, e.message))
    for
      id        <- ReportId.of(reporting.id.value).fold(fail, Right(_))
      selection <- ReportSelection
        .of(Role.values.toVector, Vector(Component))
        .fold(fail, Right(_))
      filters <- reporting.filters.traverse {
        case ReportingFilter.Keep(attribute, values) =>
          if windowOnly then Right(None)
          else
            categorical(reporting, attribute.label, levels).map(t =>
              Option(Predicate.In(t, values.values): Predicate)
            )
        case ReportingFilter.OutsideWindowAtMost(share) =>
          Right(
            Some(
              Predicate.Cmp(
                NumericTerm.Window(WindowMeasure.OutsideWindowShare),
                Comparison.LessOrEqual,
                share.value
              ): Predicate
            )
          )
      }
      term <- reporting.groupBy.traverse(c => categorical(reporting, c.label, levels))
      grouping = term.map(Grouping.ByLevel(_))
      contrast =
        for
          t        <- term
          operands <- reporting.contrast
        yield LevelContrast(t, operands.minuend, operands.subtrahend)
      _ <- reporting.minimumPerGroup.traverse_(minimum =>
        Either.cond(
          reporting.weighting != ReportingWeight.PooledQueries,
          (),
          refusal(
            reporting,
            s"Pooled-query weighting cannot apply the requested minimum of ${minimum.queries} queries per participant group."
          )
        )
      )
      reduce <- reporting.weighting match
        case ReportingWeight.PooledQueries    => Right(ReducePolicy.PooledQueries)
        case ReportingWeight.ParticipantMeans =>
          MinimumQueries
            .of(reporting.minimumPerGroup.fold(1)((m: MinimumPerGroup) => m.queries))
            .fold(fail, m => Right(ReducePolicy.ParticipantMeans(m)))
      built <- ReportSpec
        .of(
          id,
          scale,
          selection,
          filter = filters.flatten.reduceOption(Predicate.And(_, _)),
          groupBy = grouping.toVector,
          reduce = reduce,
          contrast = contrast
        )
        .fold(fail, Right(_))
    yield built

  /** Evaluate `reporting` over `source` at `scale` with eyes4s; when it has
    * an outside-window filter, eyes4s also evaluates a specification carrying
    * only that filter, whose own accounting counts the queries the filter
    * leaves out and those it cannot decide (an undefined share). Nothing is
    * subtracted here.
    */
  def evaluateWithNative[K](
      run: RunId,
      reporting: ReportingSpec,
      scale: Int,
      levels: Map[String, Vector[String]],
      source: ReportSource[K]
  ): Either[ReportRefusal, (ReportView, Report[K])] =
    def evaluated(filters: Filters) =
      spec(reporting, scale, levels, filters).flatMap(s =>
        Report
          .evaluate(s, source)
          .leftMap(e => ReportRefusal.Evaluation(reporting.id.value, e.message))
      )
    for
      report <- evaluated(Filters.All)
      window <-
        if windowFilter(reporting).isEmpty then Right(None)
        else evaluated(Filters.WindowOnly).map(Some(_))
      index <- ScaleIndex
        .of(scale)
        .leftMap(e => ReportRefusal.Evaluation(reporting.id.value, e.message))
      served <- view(run, reporting, index, report, window)
    yield (served, report)

  /** The view alone when a consumer does not retain native navigation values. */
  def evaluate[K](
      run: RunId,
      reporting: ReportingSpec,
      scale: Int,
      levels: Map[String, Vector[String]],
      source: ReportSource[K]
  ): Either[ReportRefusal, ReportView] =
    evaluateWithNative(run, reporting, scale, levels, source).map(_._1)

  /** eyes4s's absence as the protocol writes it. */
  def absence(a: Absence): ReportAbsence = a match
    case Absence.NotRecorded           => ReportAbsence.NotRecorded
    case Absence.Unparsed              => ReportAbsence.Unparsed
    case Absence.Failed(code, message) => ReportAbsence.Failed(code.render, message)
    case Absence.Undefined(reason)     =>
      ReportAbsence.Undefined(reason match
        case UndefinedReason.TooFewForSpread(n) => ReportUndefined.TooFewForSpread(n)
        case UndefinedReason.ZeroDuration       => ReportUndefined.ZeroDuration
        case UndefinedReason.NotFinite(op, n)   => ReportUndefined.NotFinite(op, n))
    case Absence.EmptyGroup                      => ReportAbsence.EmptyGroup
    case Absence.Unpaired                        => ReportAbsence.Unpaired
    case Absence.BelowMinimum(queries, required) =>
      ReportAbsence.BelowMinimum(queries, required)

  /** The served view of a report, every value with its ref; `window` is the
    * evaluation of the spec's outside-window filter alone, when it has one.
    * Refused only when eyes4s refuses the report's facts.
    */
  def view[K](
      run: RunId,
      reporting: ReportingSpec,
      scale: ScaleIndex,
      report: Report[K],
      window: Option[Report[K]]
  ): Either[ReportRefusal, ReportView] =
    val id = reporting.id
    // A spec groups by at most one covariate (`ReportingSpec.groupBy` is an
    // Option), so a group key has at most one level: its response.
    def label(g: GroupKey): Option[Response] = g.levels.headOption.map((_, l) => Response(l))
    def group(g: GroupKey): ReportGroup = label(g).fold(ReportGroup.Whole)(ReportGroup.Level(_))
    def tally(role: Role, count: ReportCount, value: Int) =
      ReportTallyView(
        roleOf(role),
        count,
        value,
        StudioRef.ReportTally(run, id, scale, roleOf(role), count)
      )
    def missing(v: Value[Double]): Option[ReportAbsence] = v.absence.map(absence)
    val cells                                            = report.cells.map(c =>
      ReportCellView(
        label(c.group),
        roleOf(c.role),
        c.estimate.toOption,
        missing(c.estimate),
        c.participants,
        c.queries,
        c.failed,
        StudioRef.ReportCell(run, id, scale, group(c.group), roleOf(c.role))
      )
    )
    val participants = report.cells.flatMap(c =>
      c.perParticipant.map(p =>
        ReportParticipantView(
          label(c.group),
          roleOf(c.role),
          p.participant,
          p.queries,
          p.value.toOption,
          missing(p.value),
          StudioRef
            .ReportParticipant(run, id, scale, group(c.group), roleOf(c.role), p.participant)
        )
      )
    )
    val contrasts = report.contrasts.map(k =>
      val (minuend, subtrahend) = (Response(k.minuend), Response(k.subtrahend))
      ReportContrastView(
        roleOf(k.role),
        minuend,
        subtrahend,
        k.estimate.toOption,
        missing(k.estimate),
        k.dispersion.n,
        k.unpaired.map(u =>
          ReportUnpairedView(u.participant, Response(u.present), Response(u.missing))
        ),
        StudioRef.ReportContrast(run, id, scale, roleOf(k.role), minuend, subtrahend)
      )
    )
    // eyes4s's range of queries per participant and group, role by role.
    val ranges = report.spec.selection.roles.traverse(role =>
      ReportFacts
        .of(report, id.value, Some(role))
        .leftMap(e => ReportRefusal.Evaluation(id.value, e.message))
        .map(_.collect {
          case f if f.slot == eyes4s.plan.FactSlot.GroupSizeRange =>
            f.value match
              case eyes4s.plan.FactValue.Range(fewest, most) =>
                Some(
                  ReportQueryRange(
                    roleOf(role),
                    fewest.toInt,
                    most.toInt,
                    StudioRef.ReportQueryRange(run, id, scale, roleOf(role))
                  )
                )
              case _ => None
        }.flatten)
    )
    val dropped = report.findings.collect {
      case ReportFinding.BelowMinimum(p, g, Role.Difference, queries, required) =>
        DroppedCell(
          label(g),
          p,
          queries,
          required,
          StudioRef.ReportParticipant(run, id, scale, group(g), ReportRole.Difference, p)
        )
    }
    val accounting = report.accounting.flatMap(a =>
      Vector(
        tally(a.role, ReportCount.Eligible, a.eligible),
        tally(a.role, ReportCount.Kept, a.kept),
        tally(a.role, ReportCount.FilteredOut, a.filteredOut),
        tally(a.role, ReportCount.UnknownPredicate, a.unknownPredicate),
        tally(a.role, ReportCount.Failed, a.failed),
        tally(a.role, ReportCount.MissingGroupAttribute, a.missingGroupAttribute),
        tally(a.role, ReportCount.BelowMinimum, a.belowMinimum)
      )
    )
    // eyes4s's own accounting of the window filter alone.
    val windowed = window.toVector.flatMap(
      _.accounting.flatMap(a =>
        Vector(
          tally(a.role, ReportCount.OutsideWindowFiltered, a.filteredOut),
          tally(a.role, ReportCount.OutsideWindowUnknown, a.unknownPredicate)
        )
      )
    )
    ranges.map(r =>
      ReportView(
        run,
        id,
        scale.value,
        cells,
        participants,
        contrasts,
        dropped,
        r.flatten,
        accounting ++ windowed
      )
    )
