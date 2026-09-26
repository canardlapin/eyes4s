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

/** A trial field the study layout projects from every key. */
enum LayoutField derives CanEqual:
  case Participant, Item, Phase

/** A per-trial quantity of the plan's window tally (see
  * `eyes4s.plan.WindowTally`). Shares are of the trial's total fixation
  * duration and are undefined for a trial whose fixations have none.
  */
enum WindowMeasure derives CanEqual:
  case OutsideWindowShare, OutsideScreenShare, OutsideWindowCount, OutsideScreenCount
  case FixationCount

/** One per-trial quantity a report reads: a layout field, a declared
  * covariate or a window tally. Terms are typed by their values, so a
  * filter or grouping that does not fit its term does not compile: a
  * numeric term is compared and binned, a level term is matched by level, an
  * ordinal term is also compared by rank, and a flag term is true or false.
  */
sealed trait Term derives CanEqual:
  /** A stable name, `participant`, `covariate:confidence` or `window:outside-window-share`. */
  def render: String

  /** The covariate this term reads, with the type it is declared as here. */
  def covariate: Option[Covariate] = this match
    case NumericTerm.Covariate(name, unit) => Some(Covariate(name, CovariateType.Numeric(unit)))
    case LevelTerm.Categorical(name, levels) =>
      Some(Covariate(name, CovariateType.Categorical(levels)))
    case LevelTerm.Ordinal(name, levels) => Some(Covariate(name, CovariateType.Ordinal(levels)))
    case FlagTerm.Binary(name)           => Some(Covariate(name, CovariateType.Binary))
    case _                               => None

object Term:
  private def kebab(name: String): String =
    name.flatMap(c => if c.isUpper then "-" + c.toLower else c.toString).stripPrefix("-")

  private[results] def window(measure: WindowMeasure): String =
    "window:" + kebab(measure.toString)
  private[results] def layout(field: LayoutField): String = kebab(field.toString)

/** A term whose value is a finite real. */
enum NumericTerm extends Term derives CanEqual:
  /** Which presentation of its item the trial is, counted from 1; a layout
    * without occurrences means 1.
    */
  case Occurrence
  case Covariate(name: CovariateName, unit: NumericUnit)
  case Window(measure: WindowMeasure)

  def render: String = this match
    case Occurrence         => "occurrence"
    case Covariate(name, _) => s"covariate:${name.value}"
    case Window(measure)    => Term.window(measure)

/** A term whose value is one level: an observed layout value or a declared level. */
enum LevelTerm extends Term derives CanEqual:
  case Layout(field: LayoutField)
  case Categorical(name: CovariateName, levels: Levels)
  case Ordinal(name: CovariateName, levels: Levels)

  def render: String = this match
    case Layout(field)        => Term.layout(field)
    case Categorical(name, _) => s"covariate:${name.value}"
    case Ordinal(name, _)     => s"covariate:${name.value}"

  /** The declared levels, when the term declares them. */
  def declared: Option[Levels] = this match
    case Layout(_)              => None
    case Categorical(_, levels) => Some(levels)
    case Ordinal(_, levels)     => Some(levels)

/** A term whose value is true or false. */
enum FlagTerm extends Term derives CanEqual:
  case Binary(name: CovariateName)

  def render: String = this match
    case Binary(name) => s"covariate:${name.value}"

/** How a numeric term is compared with a threshold. */
enum Comparison derives CanEqual:
  case Less, LessOrEqual, Greater, GreaterOrEqual

  def holds(value: Double, threshold: Double): Boolean = this match
    case Less           => value < threshold
    case LessOrEqual    => value <= threshold
    case Greater        => value > threshold
    case GreaterOrEqual => value >= threshold

/** Three-valued truth: a predicate over a missing term value is unknown. */
enum Truth derives CanEqual:
  case True, False, Unknown

  def and(that: => Truth): Truth = this match
    case False   => False
    case True    => that
    case Unknown => if that == False then False else Unknown

  def or(that: => Truth): Truth = this match
    case True    => True
    case False   => that
    case Unknown => if that == True then True else Unknown

  def not: Truth = this match
    case True    => False
    case False   => True
    case Unknown => Unknown

/** A row-level condition on terms, evaluated per query with three-valued
  * logic (Kleene): a comparison over a missing value is `Unknown`, and a
  * query whose filter is unknown is excluded and reported, never counted as
  * failing or passing. Only [[Predicate.IsMissing]] is decided on a missing
  * value.
  */
enum Predicate derives CanEqual:
  case Cmp(term: NumericTerm, comparison: Comparison, threshold: Double)

  /** The level is one of `levels`. */
  case In(term: LevelTerm, levels: Vector[String])

  /** The ordinal level ranks at or above `level`. */
  case AtLeast(term: LevelTerm.Ordinal, level: String)
  case Is(term: FlagTerm, value: Boolean)
  case IsMissing(term: Term)
  case And(left: Predicate, right: Predicate)
  case Or(left: Predicate, right: Predicate)
  case Not(inner: Predicate)

  /** Every term the predicate reads, in first-use order. */
  def terms: Vector[Term] = (this match
    case Cmp(term, _, _)  => Vector(term)
    case In(term, _)      => Vector(term)
    case AtLeast(term, _) => Vector(term)
    case Is(term, _)      => Vector(term)
    case IsMissing(term)  => Vector(term)
    case And(left, right) => left.terms ++ right.terms
    case Or(left, right)  => left.terms ++ right.terms
    case Not(inner)       => inner.terms
  ).distinct

  /** A readable, unambiguous rendering, as report tables cite the filter. */
  def render: String = this match
    case Cmp(term, comparison, threshold) =>
      val op = comparison match
        case Comparison.Less           => "<"
        case Comparison.LessOrEqual    => "<="
        case Comparison.Greater        => ">"
        case Comparison.GreaterOrEqual => ">="
      s"${term.render} $op ${DecimalText.render(threshold)}"
    case In(term, levels) =>
      levels.map(TableJson.quote).mkString(s"${term.render} in [", ", ", "]")
    case AtLeast(term, level) => s"${term.render} >= ${TableJson.quote(level)}"
    case Is(term, value)      => s"${term.render} is $value"
    case IsMissing(term)      => s"${term.render} is missing"
    case And(left, right)     => s"(${left.render} and ${right.render})"
    case Or(left, right)      => s"(${left.render} or ${right.render})"
    case Not(inner)           => s"not ${inner.render}"

  def &&(that: Predicate): Predicate = And(this, that)
  def ||(that: Predicate): Predicate = Or(this, that)
  def unary_! : Predicate            = Not(this)

/** Whether a bin's upper edge belongs to it. */
enum UpperEdge derives CanEqual:
  /** `[from, until)`. */
  case Excluded

  /** `[from, until]`: used for the last bin of a closed range. */
  case Included

/** One declared bin of a numeric term. */
final case class Bin private (label: String, from: Double, until: Double, upper: UpperEdge)
    derives CanEqual:
  def contains(value: Double): Boolean =
    from <= value && (value < until || (upper == UpperEdge.Included && value == until))

object Bin:
  def of(label: String, from: Double, until: Double, upper: UpperEdge): Either[SpecError, Bin] =
    Either.cond(
      label.trim.nonEmpty && from.isFinite && until.isFinite && from < until,
      new Bin(label, from, until, upper),
      SpecError.InvalidBin(label, from, until)
    )

/** Declared bins of a numeric term: non-empty, distinct labels, in
  * ascending order and pairwise disjoint. A value no bin contains has no
  * group.
  */
final case class Bins private (bins: Vector[Bin]) derives CanEqual:
  def labels: Vector[String]             = bins.map(_.label)
  def locate(value: Double): Option[Bin] = bins.find(_.contains(value))

object Bins:
  def of(bins: Vector[Bin]): Either[SpecError, Bins] =
    val labels   = bins.map(_.label)
    val disjoint = bins.zip(bins.drop(1)).forall { (a, b) =>
      a.until < b.from || (a.until == b.from && a.upper == UpperEdge.Excluded)
    }
    Either.cond(
      bins.nonEmpty && labels.distinct.size == labels.size && disjoint,
      new Bins(bins),
      SpecError.InvalidBins(labels)
    )

/** One grouping dimension. A numeric term groups only through declared
  * bins; there is no grouping by raw numeric value.
  */
enum Grouping derives CanEqual:
  case ByLevel(term: LevelTerm)
  case ByFlag(term: FlagTerm)
  case ByBins(term: NumericTerm, bins: Bins)

  /** The term this dimension groups on. */
  def on: Term = this match
    case ByLevel(t)   => t
    case ByFlag(t)    => t
    case ByBins(t, _) => t

  /** The declared levels, in order, when the grouping declares them;
    * a layout field's levels are those observed.
    */
  def declared: Option[Vector[String]] = this match
    case ByLevel(t)      => t.declared.map(_.values)
    case ByFlag(_)       => Some(Vector("false", "true"))
    case ByBins(_, bins) => Some(bins.labels)

/** One group: a level of each grouping dimension, in grouping order. The
  * empty group is the whole report.
  */
final case class GroupKey(levels: Vector[(String, String)]) derives CanEqual:
  def render: String =
    if levels.isEmpty then "all" else levels.map((t, l) => s"$t=$l").mkString(", ")

object GroupKey:
  val all: GroupKey = GroupKey(Vector.empty)
