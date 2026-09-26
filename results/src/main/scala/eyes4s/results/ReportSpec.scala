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

/** A report's name, chosen by its author. */
final case class ReportId private (value: String) derives CanEqual

object ReportId:
  def of(value: String): Either[SpecError, ReportId] =
    Either.cond(value.trim.nonEmpty, new ReportId(value), SpecError.BlankId(value))

/** Which stored per-query value a report reduces: the matched reduction, the
  * control reduction, or their contrast (matched minus control).
  */
enum Role derives CanEqual:
  case Matched, Control, Difference

/** The roles and score components a report reads, both non-empty and
  * distinct, in the order the report lists them.
  */
final case class ReportSelection private (roles: Vector[Role], components: Vector[String])
    derives CanEqual

object ReportSelection:
  def of(roles: Vector[Role], components: Vector[String]): Either[SpecError, ReportSelection] =
    val valid =
      roles.nonEmpty && components.nonEmpty && roles.distinct.size == roles.size &&
        components.distinct.size == components.size && components.forall(_.trim.nonEmpty)
    Either.cond(
      valid,
      new ReportSelection(roles, components),
      SpecError.InvalidSelection(roles, components)
    )

/** The fewest queries a participant needs in a group to contribute to it. */
final case class MinimumQueries private (value: Int) derives CanEqual

object MinimumQueries:
  val one: MinimumQueries                               = new MinimumQueries(1)
  def of(value: Int): Either[SpecError, MinimumQueries] =
    Either.cond(value >= 1, new MinimumQueries(value), SpecError.InvalidMinimum(value))

/** How a group's queries become its estimate. */
enum ReducePolicy derives CanEqual:
  /** Each participant's value is the mean of its queries in the group; the
    * estimate is the mean of participant values, each participant weighted
    * equally. A participant with fewer than `minimumQueries` queries in a
    * group is left out of that group only ([[Absence.BelowMinimum]]). Spread
    * is across participants.
    */
  case ParticipantMeans(minimumQueries: MinimumQueries)

  /** The estimate is the mean of every query in the group, pooled across
    * participants, so a participant with more queries weighs more. Named so
    * that it is never chosen by default; spread is across queries.
    */
  case PooledQueries

object ReducePolicy:
  /** Participant means, equal weight, one query minimum. */
  val default: ReducePolicy = ParticipantMeans(MinimumQueries.one)

/** Which dispersion a cell reports beside its `n`. There is no inferential
  * interval: a report states n and spread, and inference is the reader's.
  */
enum Spread derives CanEqual:
  /** The sample standard deviation (n - 1 denominator). The default. */
  case StandardDeviation

  /** The standard deviation and the standard error of the mean, SD / sqrt(n). */
  case StandardDeviationAndError

/** A within-participant contrast between two levels of one grouping
  * dimension (for example `memory`: Remembered minus Forgotten), taken in
  * each combination of the other dimensions' levels. Each participant
  * contributes its own difference; participants in one level only are
  * unpaired and reported.
  */
final case class LevelContrast(term: Term, minuend: String, subtrahend: String) derives CanEqual

/** A pure, serializable description of a report over a study result: which
  * scale, roles and components; which queries (a three-valued row filter);
  * how to group them (by levels, flags, or declared bins of a numeric
  * term); how to reduce them (participant means by default); an optional
  * within-participant level contrast; and the spread to report.
  */
final case class ReportSpec private (
    id: ReportId,
    scale: Int,
    selection: ReportSelection,
    filter: Option[Predicate],
    groupBy: Vector[Grouping],
    reduce: ReducePolicy,
    contrast: Option[LevelContrast],
    spread: Spread
) derives CanEqual:
  /** Every term the report reads: the filter's, then the groupings'. */
  def terms: Vector[Term] = (filter.toVector.flatMap(_.terms) ++ groupBy.map(_.on)).distinct

  /** Every covariate the report reads, as its terms declare it. */
  def covariates: Vector[Covariate] = terms.flatMap(_.covariate).distinct

object ReportSpec:
  /** A checked specification: a non-negative scale; comparison thresholds
    * finite; `In` and `AtLeast` levels declared by their terms; grouping
    * terms distinct; one declaration per covariate across all terms; and a
    * contrast over a grouping dimension between two of its levels.
    */
  def of(
      id: ReportId,
      scale: Int,
      selection: ReportSelection,
      filter: Option[Predicate] = None,
      groupBy: Vector[Grouping] = Vector.empty,
      reduce: ReducePolicy = ReducePolicy.default,
      contrast: Option[LevelContrast] = None,
      spread: Spread = Spread.StandardDeviation
  ): Either[SpecError, ReportSpec] =
    val spec = new ReportSpec(id, scale, selection, filter, groupBy, reduce, contrast, spread)
    val grouped                                    = groupBy.map(_.on)
    def predicate(p: Predicate): Option[SpecError] = p match
      case Predicate.Cmp(term, _, threshold) =>
        Option.when(!threshold.isFinite)(SpecError.NonFiniteThreshold(term.render, threshold))
      case Predicate.In(term, levels) =>
        if levels.isEmpty then Some(SpecError.EmptyLevels(term.render))
        else
          term.declared.flatMap(declared =>
            levels
              .find(!declared.contains(_))
              .map(SpecError.UndeclaredLevel(term.render, _, declared.values))
          )
      case Predicate.AtLeast(term, level) =>
        Option.when(!term.levels.contains(level))(
          SpecError.UndeclaredLevel(term.render, level, term.levels.values)
        )
      case Predicate.Is(_, _) | Predicate.IsMissing(_) => None
      case Predicate.And(l, r)                         => predicate(l).orElse(predicate(r))
      case Predicate.Or(l, r)                          => predicate(l).orElse(predicate(r))
      case Predicate.Not(inner)                        => predicate(inner)
    val declarations = spec.terms.flatMap(_.covariate).distinct
    val conflicting  =
      declarations.groupBy(_.name).toVector.sortBy(_._1).collectFirst {
        case (name, kinds) if kinds.size > 1 =>
          SpecError.CovariateDeclarations(name.value, kinds.map(_.kind.render))
      }
    val contrastError = contrast.flatMap { c =>
      groupBy.find(_.on == c.term) match
        case None => Some(SpecError.ContrastTerm(c.term.render, grouped.map(_.render)))
        case Some(grouping) =>
          val levels = grouping.declared
          Option.when(
            c.minuend == c.subtrahend || c.minuend.trim.isEmpty || c.subtrahend.trim.isEmpty ||
              levels.exists(ls => !ls.contains(c.minuend) || !ls.contains(c.subtrahend))
          )(SpecError.ContrastLevels(c.term.render, c.minuend, c.subtrahend, levels))
    }
    Vector(
      Option.when(scale < 0)(SpecError.NegativeScale(scale)),
      filter.flatMap(predicate),
      Option.when(grouped.distinct.size != grouped.size)(
        SpecError.DuplicateGrouping(grouped.map(_.render))
      ),
      conflicting,
      contrastError
    ).flatten.headOption.toLeft(spec)

/** Why a report specification was refused; every case names its operands. */
enum SpecError derives CanEqual:
  case BlankId(value: String)
  case NegativeScale(scale: Int)
  case InvalidSelection(roles: Vector[Role], components: Vector[String])
  case InvalidMinimum(value: Int)
  case NonFiniteThreshold(term: String, threshold: Double)
  case EmptyLevels(term: String)
  case UndeclaredLevel(term: String, level: String, declared: Vector[String])
  case InvalidBin(label: String, from: Double, until: Double)
  case InvalidBins(labels: Vector[String])
  case DuplicateGrouping(terms: Vector[String])
  case CovariateDeclarations(covariate: String, declared: Vector[String])
  case ContrastTerm(term: String, groupings: Vector[String])
  case ContrastLevels(
      term: String,
      minuend: String,
      subtrahend: String,
      declared: Option[Vector[String]]
  )

  def message: String = this match
    case BlankId(value)         => s"A report id must not be blank, got '$value'."
    case NegativeScale(scale)   => s"A report reads a scale index from 0, got $scale."
    case InvalidSelection(r, c) =>
      s"A report selects non-empty, distinct roles and non-blank components: roles=$r, components=$c."
    case InvalidMinimum(value) =>
      s"A participant needs at least one query in a group, got minimum=$value."
    case NonFiniteThreshold(term, threshold) =>
      s"The comparison on $term needs a finite threshold, got $threshold."
    case EmptyLevels(term)                      => s"The level filter on $term names no level."
    case UndeclaredLevel(term, level, declared) =>
      s"Level '$level' of $term is not declared; declared: $declared."
    case InvalidBin(label, from, until) =>
      s"Bin '$label' needs a non-blank label and finite edges with from < until: [$from, $until]."
    case InvalidBins(labels) =>
      s"Bins $labels must be non-empty, distinctly labelled, ascending and disjoint."
    case DuplicateGrouping(terms) => s"Grouping terms $terms must be distinct."
    case CovariateDeclarations(covariate, declared) =>
      s"Covariate '$covariate' is declared as more than one type: $declared."
    case ContrastTerm(term, groupings) =>
      s"The level contrast on $term needs it among the groupings $groupings."
    case ContrastLevels(term, minuend, subtrahend, declared) =>
      s"The level contrast on $term needs two distinct levels" +
        declared.fold("")(d => s" of $d") + s": minuend='$minuend', subtrahend='$subtrahend'."
