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

import eyes4s.plan.ResultRef

/** A SHA-256 digest a report is bound by: 64 lowercase hexadecimal digits,
  * the canonical digest (`eyes4s.codec.CanonicalDigest`) of the plan, input,
  * result or covariate source the report was evaluated over.
  */
final case class BindingDigest private (hex: String) derives CanEqual:
  override def toString: String = s"sha256:$hex"

object BindingDigest:
  def parse(field: String, hex: String): Either[ReportError[Nothing], BindingDigest] =
    Either.cond(
      hex.length == 64 && hex.forall(c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')),
      new BindingDigest(hex),
      ReportError.MalformedDigest(field, hex)
    )

/** What a report was evaluated over: the plan revision, the input, the
  * stored result and, when it reads covariates, the covariate source (the
  * admission ledger that carries the trials table), each by its canonical
  * digest. A report whose binding differs from its source's current one is
  * stale and is refused ([[Report.checkCurrent]]).
  */
final case class ReportBinding(
    plan: BindingDigest,
    input: BindingDigest,
    result: BindingDigest,
    covariates: Option[BindingDigest]
) derives CanEqual:
  /** Refuses the first field in which `current` differs from this binding. */
  def check(current: ReportBinding): Either[ReportError[Nothing], Unit] =
    Vector(
      ("plan", Some(plan), Some(current.plan)),
      ("input", Some(input), Some(current.input)),
      ("result", Some(result), Some(current.result)),
      ("covariates", covariates, current.covariates)
    ).collectFirst {
      case (field, bound, now) if bound != now =>
        ReportError.StaleBinding(
          field,
          bound.fold("none")(_.toString),
          now.fold("none")(_.toString)
        )
    }.toLeft(())

/** The spread of the values an estimate averages: their count and sample
  * standard deviation, and the standard error when the report asks for it.
  */
final case class CellSpread(n: Int, sd: Value[Double], sem: Option[Value[Double]])
    derives CanEqual

/** One participant's value in a cell: the mean of its queries there. */
final case class ParticipantValue(participant: String, queries: Int, value: Value[Double])
    derives CanEqual

/** One cell: a group's estimate for one role and component, with the counts
  * that stand behind it and the stored rows it was read from (`members`,
  * resolvable through `ResultInspection`).
  *
  * `queries` counts every query assigned to the group; `participants`
  * counts those whose value contributes (under participant means, those
  * meeting the minimum). A cell with no query has the absence `EmptyGroup`,
  * never an estimate of zero.
  */
final case class Cell[+K](
    group: GroupKey,
    role: Role,
    component: String,
    estimate: Value[Double],
    dispersion: CellSpread,
    participants: Int,
    queries: Int,
    perParticipant: Vector[ParticipantValue],
    members: Vector[ResultRef[K]]
) derives CanEqual

/** A participant present in one level of a contrast and absent from the
  * other.
  */
final case class UnpairedParticipant(participant: String, present: String, missing: String)
    derives CanEqual

/** A within-participant level contrast in one stratum (a level of each other
  * grouping dimension): the mean over paired participants of their
  * minuend-minus-subtrahend differences. `paired` lists each paired
  * participant's difference; its size is the contrast's n.
  */
final case class LevelContrastStat(
    stratum: GroupKey,
    term: String,
    minuend: String,
    subtrahend: String,
    role: Role,
    component: String,
    estimate: Value[Double],
    dispersion: CellSpread,
    paired: Vector[ParticipantValue],
    unpaired: Vector[UnpairedParticipant]
) derives CanEqual

/** Where one role's eligible queries went. The identities
  * `eligible = kept + filteredOut + unknownPredicate + failed` and
  * `missingGroupAttribute <= kept` always hold; the cells of the role
  * together hold `kept - missingGroupAttribute` queries. `belowMinimum`
  * counts participant-and-group pairs left out under the minimum.
  */
final case class Accounting private (
    role: Role,
    eligible: Int,
    kept: Int,
    filteredOut: Int,
    unknownPredicate: Int,
    failed: Int,
    missingGroupAttribute: Int,
    belowMinimum: Int
) derives CanEqual

object Accounting:
  def of(
      role: Role,
      eligible: Int,
      kept: Int,
      filteredOut: Int,
      unknownPredicate: Int,
      failed: Int,
      missingGroupAttribute: Int,
      belowMinimum: Int
  ): Either[ReportError[Nothing], Accounting] =
    val counts = Vector(kept, filteredOut, unknownPredicate, failed, missingGroupAttribute)
    Either.cond(
      counts.forall(_ >= 0) && belowMinimum >= 0 &&
        eligible == kept + filteredOut + unknownPredicate + failed &&
        missingGroupAttribute <= kept,
      new Accounting(
        role,
        eligible,
        kept,
        filteredOut,
        unknownPredicate,
        failed,
        missingGroupAttribute,
        belowMinimum
      ),
      ReportError.InconsistentAccounting(
        role,
        eligible,
        kept,
        filteredOut,
        unknownPredicate,
        failed,
        missingGroupAttribute
      )
    )

/** What a report found while reducing: exclusions and absences keyed to the
  * trials, participants and groups they concern. These are the `report`
  * diagnostics family (`ResultsDiagnostics`).
  */
enum ReportFinding[+K] derives CanEqual:
  /** No query of `role` fell in `group`. */
  case EmptyGroup(group: GroupKey, role: Role)

  /** In the level contrast of `stratum`, the participant has no value at
    * level `missing`, so it contributes no difference.
    */
  case UnpairedParticipant(participant: String, stratum: GroupKey, role: Role, missing: String)

  /** The query passed the filter, but its value of grouping `term` is
    * missing or lies in no declared bin, so it belongs to no group.
    */
  case MissingCovariate(key: K, term: String)

  /** The filter is unknown for the query because `term` has no value; the
    * query is excluded.
    */
  case UnknownPredicate(key: K, term: String)

  /** The participant has `queries` queries of `role` in `group`, fewer
    * than `required`, and is left out of that group only.
    */
  case BelowMinimum(
      participant: String,
      group: GroupKey,
      role: Role,
      queries: Int,
      required: Int
  )

  /** The trial's window `measure` is undefined (its fixations have no
    * duration, or its tally was refused), and the report reads it.
    */
  case UndefinedWindowShare(key: K, measure: WindowMeasure)

  /** The trial's `covariate` cell `raw` is not a value of its declared
    * type `expected`.
    */
  case CovariateType(key: K, covariate: String, raw: String, expected: String)

  def message: String = this match
    case EmptyGroup(group, role) => s"Group ${group.render} has no $role query."
    case ReportFinding.UnpairedParticipant(participant, stratum, role, missing) =>
      s"Participant $participant has no $role value at level '$missing' in ${stratum.render}; " +
        "it is unpaired in the level contrast."
    case MissingCovariate(key, term) =>
      s"Trial $key has no group: its $term is missing or in no declared bin."
    case UnknownPredicate(key, term) =>
      s"Trial $key is excluded: the filter is unknown because its $term is missing."
    case BelowMinimum(participant, group, role, queries, required) =>
      s"Participant $participant has $queries $role queries in ${group.render}, fewer than " +
        s"$required, and is left out of that group."
    case UndefinedWindowShare(key, measure) =>
      s"Trial $key has no defined ${Term.window(measure)}."
    case CovariateType(key, covariate, raw, expected) =>
      s"Trial $key has $covariate='$raw', which is not $expected."

/** Why a report could not be evaluated, checked or rebuilt; every case names
  * its operands.
  */
enum ReportError[+K] derives CanEqual:
  case UnknownScale(scale: Int, scales: Int)
  case ScaleMismatch(spec: Int, table: Int)
  case UnknownComponent(component: String, available: Vector[String])
  case UndescribedScores(requested: Vector[String])
  case UnknownCovariate(covariate: String, declared: Vector[String])
  case CovariateMismatch(covariate: String, report: String, table: String)
  case PlanMismatch(changes: Vector[String])
  case InputMismatch(result: String, input: String)
  case StaleBinding(field: String, bound: String, current: String)
  case MalformedDigest(field: String, value: String)
  case DuplicateQuery(key: K)
  case ComponentCount(key: K, components: Vector[String], found: Int)
  case BlankParticipant(key: K)
  case InvalidQuery(key: K, reason: String)
  case InconsistentAccounting(
      role: Role,
      eligible: Int,
      kept: Int,
      filteredOut: Int,
      unknownPredicate: Int,
      failed: Int,
      missingGroupAttribute: Int
  )
  case InconsistentCell(group: String, role: Role, component: String, reason: String)

  /** The plan's described score components do not name its method's contrast components. */
  case Components(underlying: eyes4s.plan.DescriptorError)

  def message: String = this match
    case UnknownScale(scale, scales) =>
      s"The report reads scale $scale, but the result has $scales scales."
    case ScaleMismatch(spec, table) =>
      s"The report reads scale $spec, but the query table is of scale $table."
    case UnknownComponent(component, available) =>
      s"Score component '$component' is not one of the method's components $available."
    case UndescribedScores(requested) =>
      s"The method describes no score components, so components $requested cannot be read."
    case UnknownCovariate(covariate, declared) =>
      s"The report reads covariate '$covariate', which the covariate table does not declare; " +
        s"declared: $declared."
    case CovariateMismatch(covariate, report, table) =>
      s"The report reads covariate '$covariate' as $report, but the table declares $table."
    case PlanMismatch(changes) =>
      s"The result was not computed by this plan; the descriptions differ in $changes."
    case InputMismatch(result, input) =>
      s"The result refers to input $result, but the supplied input is $input."
    case StaleBinding(field, bound, current) =>
      s"The report is stale: its $field is $bound, but the source's is now $current."
    case MalformedDigest(field, value) =>
      s"The $field binding digest '$value' is not 64 lowercase hexadecimal digits."
    case DuplicateQuery(key) => s"Trial $key is listed as more than one query."
    case ComponentCount(key, components, found) =>
      s"Trial $key has $found score values; the components are $components."
    case BlankParticipant(key)     => s"Trial $key has a blank participant."
    case InvalidQuery(key, reason) => s"Trial $key cannot be a query: $reason."
    case InconsistentAccounting(role, eligible, kept, filtered, unknown, failed, missing) =>
      s"The $role accounting does not add up: eligible=$eligible, kept=$kept, " +
        s"filteredOut=$filtered, unknownPredicate=$unknown, failed=$failed, " +
        s"missingGroupAttribute=$missing."
    case InconsistentCell(group, role, component, reason) =>
      s"The $role cell of $component in $group is inconsistent: $reason."
    case Components(underlying) => underlying.message

/** A report: the cells, level contrasts and accounting a [[ReportSpec]]
  * reduces a scale's stored rows to, with its findings and the binding of
  * what it was evaluated over. Cells are listed by group (in grouping
  * order), then role and component (in selection order).
  */
final class Report[K] private (
    val spec: ReportSpec,
    val binding: ReportBinding,
    val groups: Vector[GroupKey],
    val cells: Vector[Cell[K]],
    val contrasts: Vector[LevelContrastStat],
    val accounting: Vector[Accounting],
    val findings: Vector[ReportFinding[K]]
):
  def cell(group: GroupKey, role: Role, component: String): Option[Cell[K]] =
    cells.find(c => c.group == group && c.role == role && c.component == component)

  def accountingOf(role: Role): Option[Accounting] = accounting.find(_.role == role)

  /** Refuses this report when its source has changed since it was evaluated. */
  def checkCurrent(current: ReportBinding): Either[ReportError[Nothing], Unit] =
    binding.check(current)

  override def equals(other: Any): Boolean = other match
    case that: Report[?] =>
      spec == that.spec && binding == that.binding && groups == that.groups &&
      cells == that.cells && contrasts == that.contrasts && accounting == that.accounting &&
      findings == that.findings
    case _ => false
  override def hashCode: Int = (spec, binding, groups, cells, contrasts, accounting).hashCode

object Report:
  /** Evaluate `spec` over the stored rows of `source`. Pair scores are never
    * rerun: each query's values are read from the stored reductions and
    * contrast rows.
    */
  def evaluate[K](
      spec: ReportSpec,
      source: ReportSource[K]
  ): Either[ReportError[K], Report[K]] =
    source
      .queries(spec.scale)
      .flatMap(table => reduce(spec, table, source.binding)(using source.ordering))

  /** As [[evaluate]], refusing a source whose binding is not `expected`. */
  def evaluate[K](
      spec: ReportSpec,
      source: ReportSource[K],
      expected: ReportBinding
  ): Either[ReportError[K], Report[K]] =
    expected.check(source.binding).flatMap(_ => evaluate(spec, source))

  /** Reduce a query table under `spec`: the pure core of [[evaluate]]. */
  def reduce[K](spec: ReportSpec, table: QueryTable[K], binding: ReportBinding)(using
      ordering: Ordering[K]
  ): Either[ReportError[K], Report[K]] =
    for
      _ <- Either.cond(
        spec.scale == table.scale,
        (),
        ReportError.ScaleMismatch(spec.scale, table.scale)
      )
      indices <- spec.selection.components.foldLeft[Either[ReportError[K], Vector[Int]]](
        Right(Vector.empty)
      ) { (acc, c) =>
        acc.flatMap(found =>
          Option(table.components.indexOf(c))
            .filter(_ >= 0)
            .map(found :+ _)
            .toRight(
              if table.components.isEmpty then
                ReportError.UndescribedScores(spec.selection.components)
              else ReportError.UnknownComponent(c, table.components)
            )
        )
      }
      _ <- spec.covariates
        .collectFirst(Function.unlift { c =>
          table.covariates.get(c.name) match
            case None =>
              Some(
                ReportError.UnknownCovariate(c.name.value, table.covariates.names.map(_.value))
              )
            case Some(kind) if kind != c.kind =>
              Some(ReportError.CovariateMismatch(c.name.value, c.kind.render, kind.render))
            case _ => None
        })
        .toLeft(())
      report <- Reduction(spec, table, indices, binding).run
    yield report

  /** Rebuild a stored report, checking its structure: one cell per group,
    * role and component in order; consistent counts; and accounting whose
    * identities hold and whose kept queries the cells hold.
    */
  def reconstruct[K](
      spec: ReportSpec,
      binding: ReportBinding,
      groups: Vector[GroupKey],
      cells: Vector[Cell[K]],
      contrasts: Vector[LevelContrastStat],
      accounting: Vector[Accounting],
      findings: Vector[ReportFinding[K]]
  ): Either[ReportError[K], Report[K]] =
    val roles      = spec.selection.roles
    val components = spec.selection.components
    val expected   = for
      g <- groups
      r <- roles
      c <- components
    yield (g, r, c)
    def cellError(c: Cell[K], reason: String) =
      ReportError.InconsistentCell(c.group.render, c.role, c.component, reason)
    val shape =
      Option.when(cells.map(c => (c.group, c.role, c.component)) != expected)(
        ReportError.InconsistentCell(
          "all",
          roles.headOption.getOrElse(Role.Difference),
          components.mkString(","),
          "cells do not cover every group, role and component once, in order"
        )
      )
    val counts = cells.collectFirst(Function.unlift { c =>
      val present = c.perParticipant.count(_.value.isPresent)
      val listed  = c.perParticipant.map(_.queries).sum
      if c.members.size != c.queries then Some(cellError(c, "members differ from queries"))
      else if listed != c.queries then Some(cellError(c, "participant queries differ"))
      else if spec.reduce != ReducePolicy.PooledQueries && present != c.participants then
        Some(cellError(c, "contributing participants differ"))
      else if c.queries == 0 && c.estimate != Value.Missing(Absence.EmptyGroup) then
        Some(cellError(c, "an empty group has an estimate"))
      else None
    })
    val books =
      Option
        .when(accounting.map(_.role) != roles)(
          ReportError.InconsistentCell("all", Role.Difference, "", "accounting roles differ")
        )
        .orElse(accounting.collectFirst(Function.unlift { a =>
          val held =
            cells.filter(c => c.role == a.role && components.headOption.contains(c.component))
          Option.when(held.map(_.queries).sum != a.kept - a.missingGroupAttribute)(
            ReportError.InconsistentAccounting(
              a.role,
              a.eligible,
              a.kept,
              a.filteredOut,
              a.unknownPredicate,
              a.failed,
              a.missingGroupAttribute
            )
          )
        }))
    shape
      .orElse(counts)
      .orElse(books)
      .toLeft(new Report(spec, binding, groups, cells, contrasts, accounting, findings))

  // ------------------------------------------------------------------- reduction

  private enum TermValue derives CanEqual:
    case Num(value: Double)
    case Lvl(value: String)
    case Flg(value: Boolean)

  private final case class Reduction[K](
      spec: ReportSpec,
      table: QueryTable[K],
      indices: Vector[Int],
      binding: ReportBinding
  )(using ordering: Ordering[K]):
    private val roles      = spec.selection.roles
    private val components = spec.selection.components
    private val queries    = table.queries.sortBy(_.key)

    private def termValue(q: Query[K], term: Term): Value[TermValue] = term match
      case NumericTerm.Occurrence         => Value.Present(TermValue.Num(q.occurrence.toDouble))
      case NumericTerm.Covariate(name, _) =>
        q.covariate(name).flatMap {
          case CovariateValue.Number(d) => Value.Present(TermValue.Num(d))
          case _                        => Value.Missing(Absence.Unparsed)
        }
      case NumericTerm.Window(measure)               => q.measure(measure).map(TermValue.Num(_))
      case LevelTerm.Layout(LayoutField.Participant) =>
        Value.Present(TermValue.Lvl(q.participant))
      case LevelTerm.Layout(LayoutField.Item)  => Value.Present(TermValue.Lvl(q.item))
      case LevelTerm.Layout(LayoutField.Phase) => Value.Present(TermValue.Lvl(q.phase))
      case LevelTerm.Categorical(name, _)      => level(q, name)
      case LevelTerm.Ordinal(name, _)          => level(q, name)
      case FlagTerm.Binary(name)               =>
        q.covariate(name).flatMap {
          case CovariateValue.Flag(b) => Value.Present(TermValue.Flg(b))
          case _                      => Value.Missing(Absence.Unparsed)
        }

    private def level(q: Query[K], name: CovariateName): Value[TermValue] =
      q.covariate(name).flatMap {
        case CovariateValue.Level(s) => Value.Present(TermValue.Lvl(s))
        case _                       => Value.Missing(Absence.Unparsed)
      }

    private def truth(q: Query[K], p: Predicate): Truth = p match
      case Predicate.Cmp(term, comparison, threshold) =>
        termValue(q, term) match
          case Value.Present(TermValue.Num(x)) =>
            if comparison.holds(x, threshold) then Truth.True else Truth.False
          case _ => Truth.Unknown
      case Predicate.In(term, levels) =>
        termValue(q, term) match
          case Value.Present(TermValue.Lvl(s)) =>
            if levels.contains(s) then Truth.True else Truth.False
          case _ => Truth.Unknown
      case Predicate.AtLeast(term, level) =>
        termValue(q, term) match
          case Value.Present(TermValue.Lvl(s)) =>
            (term.levels.rank(s), term.levels.rank(level)) match
              case (Some(a), Some(b)) => if a >= b then Truth.True else Truth.False
              case _                  => Truth.Unknown
          case _ => Truth.Unknown
      case Predicate.Is(term, value) =>
        termValue(q, term) match
          case Value.Present(TermValue.Flg(b)) => if b == value then Truth.True else Truth.False
          case _                               => Truth.Unknown
      case Predicate.IsMissing(term) =>
        if termValue(q, term).isPresent then Truth.False else Truth.True
      case Predicate.And(l, r) => truth(q, l).and(truth(q, r))
      case Predicate.Or(l, r)  => truth(q, l).or(truth(q, r))
      case Predicate.Not(i)    => truth(q, i).not

    /** The query's level in one grouping, when it has one. */
    private def groupLevel(q: Query[K], g: Grouping): Option[String] = g match
      case Grouping.ByLevel(term) =>
        termValue(q, term).toOption.collect { case TermValue.Lvl(s) => s }
      case Grouping.ByFlag(term) =>
        termValue(q, term).toOption.collect { case TermValue.Flg(b) => b.toString }
      case Grouping.ByBins(term, bins) =>
        termValue(q, term).toOption
          .collect { case TermValue.Num(x) => x }
          .flatMap(bins.locate)
          .map(_.label)

    private def mean(values: Vector[Double]): Value[Double] =
      if values.isEmpty then Value.Missing(Absence.EmptyGroup)
      else
        val m = values.sum / values.size
        if m.isFinite then Value.Present(m)
        else Value.Missing(Absence.Undefined(UndefinedReason.NotFinite("mean", values.size)))

    private def dispersion(values: Vector[Double], estimate: Value[Double]): CellSpread =
      val n  = values.size
      val sd = estimate match
        case Value.Missing(reason)     => Value.Missing(reason)
        case Value.Present(_) if n < 2 =>
          Value.Missing(Absence.Undefined(UndefinedReason.TooFewForSpread(n)))
        case Value.Present(m) =>
          val squares = values.map(v => (v - m) * (v - m)).sum
          val s       = math.sqrt(squares / (n - 1))
          if s.isFinite then Value.Present(s)
          else Value.Missing(Absence.Undefined(UndefinedReason.NotFinite("sd", n)))
      val sem = Option.when(spec.spread == Spread.StandardDeviationAndError)(
        sd.map(s => s / math.sqrt(n.toDouble))
      )
      CellSpread(n, sd, sem)

    def run: Either[ReportError[K], Report[K]] =
      val filter = spec.filter
      val truths = queries.map(q => filter.fold(Truth.True)(truth(q, _)))
      // Group levels are the declared ones, else those observed among every
      // eligible query, sorted; groups are their product in grouping order.
      val dims = spec.groupBy.map(g =>
        g -> g.declared.getOrElse(queries.flatMap(groupLevel(_, g)).distinct.sorted)
      )
      val groups = dims.foldLeft(Vector(GroupKey.all)) { case (acc, (g, levels)) =>
        for
          key   <- acc
          level <- levels
        yield GroupKey(key.levels :+ (g.on.render -> level))
      }
      val assigned: Vector[Option[GroupKey]] = queries.map { q =>
        val levels = spec.groupBy.map(g => groupLevel(q, g).map(g.on.render -> _))
        Option.when(levels.forall(_.isDefined))(GroupKey(levels.flatten))
      }

      // Findings about queries, once per query, in key order.
      val queryFindings = queries.zip(truths).zip(assigned).flatMap { case ((q, t), group) =>
        val covariateTypes = spec.covariates.flatMap(c =>
          q.unparsed.collect {
            case (name, raw) if name == c.name =>
              ReportFinding.CovariateType(q.key, name.value, raw, c.kind.render)
          }
        )
        val windows = spec.terms.collect {
          case NumericTerm.Window(m)
              if (m == WindowMeasure.OutsideWindowShare ||
                m == WindowMeasure.OutsideScreenShare) && !q.measure(m).isPresent =>
            ReportFinding.UndefinedWindowShare(q.key, m)
        }
        val unknown =
          if t != Truth.Unknown then Vector.empty
          else
            filter.toVector
              .flatMap(_.terms)
              .filter(term => !termValue(q, term).isPresent)
              .map(term => ReportFinding.UnknownPredicate(q.key, term.render))
        val ungrouped =
          if t != Truth.True || group.isDefined then Vector.empty
          else
            spec.groupBy
              .find(g => groupLevel(q, g).isEmpty)
              .map(g => ReportFinding.MissingCovariate(q.key, g.on.render))
              .toVector
        covariateTypes ++ windows ++ unknown ++ ungrouped
      }

      val minimum = spec.reduce match
        case ReducePolicy.ParticipantMeans(m) => Some(m.value)
        case ReducePolicy.PooledQueries       => None

      val perRole = roles.map { role =>
        val kept = queries.indices.filter(i =>
          truths(i) == Truth.True && queries(i).outcome(role).isInstanceOf[RoleOutcome.Scored]
        )
        val failed = queries.indices.count(i =>
          truths(i) == Truth.True && !queries(i).outcome(role).isInstanceOf[RoleOutcome.Scored]
        )
        val byGroup: Map[GroupKey, Vector[Query[K]]] =
          kept.flatMap(i => assigned(i).map(_ -> queries(i))).toVector.groupMap(_._1)(_._2)
        val cells = groups.map { g =>
          val members = byGroup.getOrElse(g, Vector.empty)
          val people  = members.groupBy(_.participant).toVector.sortBy(_._1)
          g -> people
        }
        val below = cells.flatMap { case (g, people) =>
          minimum.toVector.flatMap(m =>
            people.collect {
              case (p, qs) if qs.size < m => ReportFinding.BelowMinimum(p, g, role, qs.size, m)
            }
          )
        }
        val empties = cells.collect {
          case (g, people) if people.isEmpty => ReportFinding.EmptyGroup(g, role)
        }
        val accounting = Accounting.of(
          role,
          queries.size,
          kept.size,
          truths.count(_ == Truth.False),
          truths.count(_ == Truth.Unknown),
          failed,
          kept.count(i => assigned(i).isEmpty),
          below.size
        )
        (role, cells, accounting, below ++ empties)
      }

      val built = for
        (role, grouped, _, _) <- perRole
        (g, people)           <- grouped
        (component, index)    <- components.zip(indices)
      yield
        val values: Vector[ParticipantValue] = people.map { (p, qs) =>
          val v = mean(qs.map(q => q.outcome(role).value(index)).flatMap(_.toOption))
          minimum match
            case Some(m) if qs.size < m =>
              ParticipantValue(p, qs.size, Value.Missing(Absence.BelowMinimum(qs.size, m)))
            case _ => ParticipantValue(p, qs.size, v)
        }
        val members  = people.flatMap(_._2).sortBy(_.key)
        val queryN   = members.size
        val averaged = minimum match
          case Some(_) => values.flatMap(_.value.toOption)
          case None    => members.flatMap(q => q.outcome(role).value(index).toOption)
        val estimate =
          if queryN == 0 then Value.Missing(Absence.EmptyGroup)
          else if averaged.isEmpty then
            Value.Missing(
              Absence.BelowMinimum(
                values.map(_.queries).maxOption.getOrElse(0),
                minimum.getOrElse(1)
              )
            )
          else mean(averaged)
        Cell(
          g,
          role,
          component,
          estimate,
          dispersion(averaged, estimate),
          minimum.fold(people.size)(_ => values.count(_.value.isPresent)),
          queryN,
          values,
          members.map(_.ref(role, table.scale))
        )
      // Order cells by group, then role and component in selection order.
      val cells = for
        g    <- groups
        r    <- roles
        c    <- components
        cell <- built.find(x => x.group == g && x.role == r && x.component == c)
      yield cell

      val contrasts = spec.contrast.toVector.flatMap(c => levelContrast(c, cells))

      val contrastFindings = contrasts
        .distinctBy(s => (s.stratum, s.role))
        .flatMap(s =>
          s.unpaired.map(u =>
            ReportFinding.UnpairedParticipant(u.participant, s.stratum, s.role, u.missing)
          )
        )

      perRole
        .map(_._3)
        .foldLeft[Either[ReportError[K], Vector[Accounting]]](Right(Vector.empty))((acc, a) =>
          acc.flatMap(v => a.map(v :+ _))
        )
        .map { accounting =>
          new Report(
            spec,
            binding,
            groups,
            cells,
            contrasts,
            accounting,
            queryFindings ++ perRole.flatMap(_._4) ++ contrastFindings
          )
        }

    private def levelContrast(
        c: LevelContrast,
        cells: Vector[Cell[K]]
    ): Vector[LevelContrastStat] =
      val term   = c.term.render
      val index  = spec.groupBy.indexWhere(_.on == c.term)
      val strata = cells
        .map(_.group)
        .distinct
        .map(g => GroupKey(g.levels.patch(index, Nil, 1)))
        .distinct
      def at(stratum: GroupKey, level: String) =
        GroupKey(stratum.levels.patch(index, Vector(term -> level), 0))
      for
        stratum   <- strata
        role      <- roles
        component <- components
        a         <- cells
          .find(x =>
            x.group == at(stratum, c.minuend) && x.role == role && x.component == component
          )
          .toVector
        b <- cells
          .find(x =>
            x.group == at(stratum, c.subtrahend) && x.role == role && x.component == component
          )
          .toVector
      yield
        def present(cell: Cell[K]) =
          cell.perParticipant.collect { case ParticipantValue(p, n, Value.Present(v)) =>
            p -> (n, v)
          }.toMap
        val pa     = present(a)
        val pb     = present(b)
        val paired = (pa.keySet intersect pb.keySet).toVector.sorted
        // A paired participant's queries are those of both levels.
        val diffs = paired.map(p =>
          ParticipantValue(p, pa(p)._1 + pb(p)._1, Value.Present(pa(p)._2 - pb(p)._2))
        )
        val values   = diffs.flatMap(_.value.toOption)
        val finite   = values.forall(_.isFinite)
        val unpaired =
          (pa.keySet diff pb.keySet).toVector.sorted.map(
            UnpairedParticipant(_, c.minuend, c.subtrahend)
          ) ++ (pb.keySet diff pa.keySet).toVector.sorted.map(
            UnpairedParticipant(_, c.subtrahend, c.minuend)
          )
        val estimate =
          if values.isEmpty then Value.Missing(Absence.Unpaired)
          else if !finite then
            Value.Missing(
              Absence.Undefined(UndefinedReason.NotFinite("difference", values.size))
            )
          else mean(values)
        LevelContrastStat(
          stratum,
          term,
          c.minuend,
          c.subtrahend,
          role,
          component,
          estimate,
          dispersion(if finite then values else Vector.empty, estimate),
          diffs,
          unpaired
        )
