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

import eyes4s.plan.{DiagnosticCode, ResultRef, StudyKey}
import eyes4s.results.*
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

import java.math.{BigDecimal as Exact, MathContext}

/** One report to check: a specification and the query table it reduces. */
final case class ReportCase(spec: ReportSpec, table: QueryTable[StudyKey])

/** Published conformance for report reductions ([[eyes4s.results.Report.reduce]]).
  *
  * The laws take the reduction as a parameter so that a deliberately broken
  * reduction can be shown to fail them. Numeric laws compare within a named
  * [[Tolerance]]: the rational oracle computes every mean and spread exactly
  * over the report's own cell members, so a law passing within
  * [[ReportLaws.arithmetic]] says the double arithmetic is only rounding.
  */
trait ReportLaws extends Laws:
  type Reduce =
    (ReportSpec, QueryTable[StudyKey]) => Either[ReportError[StudyKey], Report[StudyKey]]

  private def report(reduce: Reduce, c: ReportCase): Option[Report[StudyKey]] =
    reduce(c.spec, c.table).toOption

  private def close(tolerance: Tolerance)(a: Value[Double], b: Value[Double]): Boolean =
    (a, b) match
      case (Value.Present(x), Value.Present(y)) => tolerance.approxEquals(x, y)
      case _                                    => a == b

  private def respec(spec: ReportSpec)(
      filter: Option[Predicate] = spec.filter,
      groupBy: Vector[Grouping] = spec.groupBy,
      reduce: ReducePolicy = spec.reduce,
      contrast: Option[LevelContrast] = spec.contrast
  ): Option[ReportSpec] =
    ReportSpec
      .of(spec.id, spec.scale, spec.selection, filter, groupBy, reduce, contrast, spec.spread)
      .toOption

  private def table(c: ReportCase, queries: Vector[Query[StudyKey]]) =
    QueryTable.of(c.table.scale, c.table.components, c.table.covariates, queries).toOption

  /** Every law over generated cases. `witness` is a case in which one
    * participant has more queries than another with a different mean, so
    * pooling the queries would move the estimate.
    */
  def reduction(
      reduce: Reduce,
      cases: Gen[ReportCase],
      witness: ReportCase,
      tolerance: Tolerance
  ): RuleSet =
    val same = close(tolerance)

    def duplicated(c: ReportCase, participant: String): Option[QueryTable[StudyKey]] =
      val copies = c.table.queries
        .filter(_.participant == participant)
        .map(q => q.rekey(q.key.copy(stimulus = q.key.stimulus + "#copy")))
      table(c, c.table.queries ++ copies)

    new SimpleRuleSet(
      "report",
      "participant means give each participant equal weight" ->
        forAll(cases, Gen.choose(0, 7)) { (c, pick) =>
          val participants = c.table.queries.map(_.participant).distinct
          (for
            spec <- respec(c.spec)(reduce = ReducePolicy.default)
            p = participants(pick % participants.size)
            twice <- duplicated(c, p)
            a     <- reduce(spec, c.table).toOption
            b     <- reduce(spec, twice).toOption
          yield a.cells.size == b.cells.size && a.cells.zip(b.cells).forall { (x, y) =>
            same(x.estimate, y.estimate) && same(x.dispersion.sd, y.dispersion.sd) &&
            x.participants == y.participants &&
            x.perParticipant.map(_.participant) == y.perParticipant.map(_.participant) &&
            x.perParticipant.zip(y.perParticipant).forall((u, v) => same(u.value, v.value))
          } && a.contrasts.zip(b.contrasts).forall((x, y) => same(x.estimate, y.estimate)))
            .getOrElse(false)
        },
      "pooled queries weigh participants by their query count (witness)" -> {
        val p = witness.table.queries.head.participant
        Prop(
          (for
            pooled <- respec(witness.spec)(reduce = ReducePolicy.PooledQueries)
            means  <- respec(witness.spec)(reduce = ReducePolicy.default)
            twice  <- duplicated(witness, p)
            a      <- reduce(pooled, witness.table).toOption
            b      <- reduce(pooled, twice).toOption
            c      <- reduce(means, witness.table).toOption
            d      <- reduce(means, twice).toOption
          yield a.cells.zip(b.cells).exists((x, y) => !same(x.estimate, y.estimate)) &&
            c.cells.zip(d.cells).forall((x, y) => same(x.estimate, y.estimate)))
            .getOrElse(false)
        )
      },
      "the report does not depend on query order" ->
        forAll(cases, Gen.long) { (c, seed) =>
          val shuffled = new scala.util.Random(seed).shuffle(c.table.queries)
          (for
            t <- table(c, shuffled)
            a <- report(reduce, c)
            b <- reduce(c.spec, t).toOption
          yield a == b).getOrElse(false)
        },
      "relabelling participants relabels their values and keeps every estimate" ->
        forAll(cases) { c =>
          // A reversing relabelling, so participants also change order.
          val names   = c.table.queries.map(_.participant).distinct.sorted
          val renamed = names.zip(names.reverse.map("z-" + _)).toMap
          (for
            t <- table(c, c.table.queries.map(q => q.relabel(renamed(q.participant))))
            a <- report(reduce, c)
            b <- reduce(c.spec, t).toOption
          yield a.cells.zip(b.cells).forall { (x, y) =>
            same(x.estimate, y.estimate) && same(x.dispersion.sd, y.dispersion.sd) &&
            x.participants == y.participants && x.queries == y.queries &&
            x.perParticipant.forall(u =>
              y.perParticipant.exists(v =>
                v.participant == renamed(u.participant) && v.queries == u.queries &&
                  same(u.value, v.value)
              )
            )
          } && a.accounting == b.accounting).getOrElse(false)
        },
      "filtering to a group gives that group's cell" ->
        forAll(cases) { c =>
          c.spec.groupBy match
            case Vector(g @ Grouping.ByLevel(term)) =>
              report(reduce, c).exists { r =>
                r.groups.forall { group =>
                  val level  = group.levels.head._2
                  val narrow = Predicate.In(term, Vector(level))
                  val filter = c.spec.filter.fold[Predicate](narrow)(_ && narrow)
                  (for
                    spec <- respec(c.spec)(Some(filter), Vector.empty, contrast = None)
                    one  <- reduce(spec, c.table).toOption
                  yield c.spec.selection.roles.forall(role =>
                    c.spec.selection.components.forall { component =>
                      (
                        r.cell(group, role, component),
                        one.cell(GroupKey.all, role, component)
                      ) match
                        case (Some(x), Some(y)) =>
                          same(x.estimate, y.estimate) && x.queries == y.queries &&
                          x.participants == y.participants && x.members == y.members
                        case _ => false
                    }
                  )).getOrElse(false) && g.on == term
                }
              }
            case _ => report(reduce, c).isDefined
        },
      "negating the filter swaps passed and filtered-out queries and keeps the unknown" ->
        forAll(cases) { c =>
          c.spec.filter match
            case None    => report(reduce, c).isDefined
            case Some(p) =>
              (for
                negated <- respec(c.spec)(filter = Some(Predicate.Not(p)))
                a       <- report(reduce, c)
                b       <- reduce(negated, c.table).toOption
              yield a.accounting.zip(b.accounting).forall { (x, y) =>
                x.unknownPredicate == y.unknownPredicate &&
                x.filteredOut == y.kept + y.failed && y.filteredOut == x.kept + x.failed
              }).getOrElse(false)
        },
      "a decided conjunct or disjunct decides the filter" ->
        forAll(cases) { c =>
          // No query's item is this one, so the level test is false for every query.
          val never  = Predicate.In(LevelTerm.Layout(LayoutField.Item), Vector("\u0000never"))
          val filter = c.spec.filter.getOrElse(never)
          (for
            conjunction <- respec(c.spec)(filter = Some(filter && never))
            disjunction <- respec(c.spec)(filter = Some(filter || !never))
            a           <- reduce(conjunction, c.table).toOption
            b           <- reduce(disjunction, c.table).toOption
          yield a.accounting.forall(x => x.filteredOut == x.eligible) &&
            b.accounting.forall(x => x.filteredOut == 0 && x.unknownPredicate == 0))
            .getOrElse(false)
        },
      "every eligible query is accounted for once" ->
        forAll(cases) { c =>
          report(reduce, c).exists { r =>
            val first = c.spec.selection.components.head
            r.accounting.map(_.role) == c.spec.selection.roles && r.accounting.forall { a =>
              val held  = r.cells.filter(x => x.role == a.role && x.component == first)
              val below = r.findings.count {
                case ReportFinding.BelowMinimum(_, _, role, _, _) => role == a.role
                case _                                            => false
              }
              a.eligible == c.table.queries.size &&
              a.eligible == a.kept + a.filteredOut + a.unknownPredicate + a.failed &&
              held.map(_.queries).sum == a.kept - a.missingGroupAttribute &&
              a.belowMinimum == below
            }
          }
        },
      "a level contrast pairs exactly the participants present at both levels" ->
        forAll(cases) { c =>
          report(reduce, c).exists { r =>
            r.contrasts.forall { s =>
              def present(level: String) =
                val group = GroupKey(
                  s.stratum.levels.patch(
                    c.spec.groupBy.indexWhere(_.on.render == s.term),
                    Vector(s.term -> level),
                    0
                  )
                )
                r.cell(group, s.role, s.component)
                  .toVector
                  .flatMap(_.perParticipant.collect {
                    case ParticipantValue(p, _, Value.Present(_)) => p
                  })
                  .toSet
              val a = present(s.minuend)
              val b = present(s.subtrahend)
              s.paired.map(_.participant).toSet == (a intersect b) &&
              s.paired.size == (a intersect b).size &&
              s.unpaired.map(_.participant).toSet == ((a diff b) union (b diff a)) &&
              s.estimate.isPresent == (a intersect b).nonEmpty &&
              s.dispersion.n == s.paired.size
            }
          }
        },
      "a missing value is never a zero" ->
        forAll(cases) { c =>
          report(reduce, c).exists { r =>
            val cellsOk = r.cells.forall { x =>
              val contributing = c.spec.reduce match
                case ReducePolicy.PooledQueries => x.queries
                case _                          => x.perParticipant.count(_.value.isPresent)
              x.estimate.isPresent == (contributing > 0) &&
              (x.queries > 0 || x.estimate == Value.Missing(Absence.EmptyGroup))
            }
            val tableOk = ReportTables.cells(r).toOption.exists { t =>
              val estimate = t.columns.indexWhere(_.name == "estimate")
              val absence  = t.columns.indexWhere(_.name == "estimate_absence")
              t.rows.zip(r.cells).forall { (row, x) =>
                x.estimate match
                  case Value.Present(v) =>
                    row(estimate) == ResultCell.Number(v) && row(absence) == ResultCell.Missing
                  case Value.Missing(a) =>
                    row(estimate) == ResultCell.Missing &&
                    row(absence) == ResultCell.Text(ReportTables.absence(a))
              }
            }
            cellsOk && tableOk
          }
        },
      "estimates agree with an exact rational oracle" ->
        forAll(cases) { c =>
          report(reduce, c).exists(r => ReportLaws.oracle(c, r, tolerance))
        }
    )

object ReportLaws extends ReportLaws:
  /** Means and spreads of at most a few dozen doubles: rounding only. */
  val arithmetic: Tolerance = Tolerance(absolute = 1e-12, relative = 1e-12)

  /** The binding every generated report carries. */
  val binding: ReportBinding =
    def digest(c: Char, field: String) =
      BindingDigest.parse(field, c.toString * 64).toOption.get
    ReportBinding(digest('a', "plan"), digest('b', "input"), digest('c', "result"), None)

  /** The shipped reduction under [[binding]]. */
  val shipped: Reduce = (spec, table) => Report.reduce(spec, table, binding)

  private val context = MathContext.DECIMAL128

  private def exact(value: Double): Exact = new Exact(value)

  private def mean(values: Vector[Exact]): Option[Exact] =
    Option.when(values.nonEmpty)(
      values.foldLeft(Exact.ZERO)(_.add(_)).divide(new Exact(values.size), context)
    )

  private def sd(values: Vector[Exact]): Option[Double] =
    Option.when(values.size >= 2) {
      val m      = mean(values).get
      val square = values.map(v => v.subtract(m).pow(2)).foldLeft(Exact.ZERO)(_.add(_))
      math.sqrt(square.divide(new Exact(values.size - 1), context).doubleValue)
    }

  /** Recompute every cell's participant values, estimate and standard
    * deviation, and every contrast's, exactly from the member queries.
    */
  def oracle(c: ReportCase, r: Report[StudyKey], tolerance: Tolerance): Boolean =
    val byKey   = c.table.queries.map(q => q.key -> q).toMap
    val minimum = c.spec.reduce match
      case ReducePolicy.ParticipantMeans(m) => Some(m.value)
      case ReducePolicy.PooledQueries       => None
    def key(ref: ResultRef[StudyKey]): Option[StudyKey] = ref match
      case ResultRef.Reduction(_, _, k) => Some(k)
      case ResultRef.ContrastRow(_, k)  => Some(k)
      case _                            => None
    def near(v: Value[Double], expected: Option[Exact]): Boolean = (v, expected) match
      case (Value.Present(x), Some(e)) => tolerance.approxEquals(x, e.doubleValue)
      case (Value.Missing(_), None)    => true
      case _                           => false
    def spreadNear(v: Value[Double], expected: Option[Double]): Boolean = (v, expected) match
      case (Value.Present(x), Some(e)) => tolerance.approxEquals(x, e)
      case (Value.Missing(_), None)    => true
      case _                           => false
    val participantValues = r.cells.map { cell =>
      val index   = c.table.components.indexOf(cell.component)
      val queries = cell.members.flatMap(key).flatMap(byKey.get)
      val values  = queries.map(q =>
        q.outcome(cell.role) match
          case RoleOutcome.Scored(v) => Some(exact(v(index)))
          case _                     => None
      )
      val people         = queries.zip(values).groupBy(_._1.participant).toVector.sortBy(_._1)
      val perParticipant = people.map { (p, rows) =>
        val means = mean(rows.flatMap(_._2))
        p -> (if minimum.exists(rows.size < _) then None else means)
      }
      val averaged = minimum match
        case Some(_) => perParticipant.flatMap(_._2)
        case None    => values.flatten
      val ok = values.forall(_.isDefined) &&
        cell.perParticipant.map(_.participant) == perParticipant.map(_._1) &&
        cell.perParticipant.zip(perParticipant).forall((v, e) => near(v.value, e._2)) &&
        near(cell.estimate, mean(averaged)) && spreadNear(cell.dispersion.sd, sd(averaged)) &&
        cell.participants == (minimum match
          case Some(_) => perParticipant.count(_._2.isDefined)
          case None    => perParticipant.size)
      (cell, perParticipant.toMap, ok)
    }
    val contrastsOk = r.contrasts.forall { s =>
      val index             = c.spec.groupBy.indexWhere(_.on.render == s.term)
      def at(level: String) =
        participantValues.collectFirst {
          case (cell, values, _)
              if cell.role == s.role && cell.component == s.component &&
                cell.group == GroupKey(
                  s.stratum.levels.patch(index, Vector(s.term -> level), 0)
                ) =>
            values
        }
      (at(s.minuend), at(s.subtrahend)) match
        case (Some(a), Some(b)) =>
          val differences = s.paired.map(p =>
            (a.get(p.participant).flatten, b.get(p.participant).flatten) match
              case (Some(x), Some(y)) => Some(x.subtract(y))
              case _                  => None
          )
          differences.forall(_.isDefined) &&
          near(s.estimate, mean(differences.flatten)) &&
          spreadNear(s.dispersion.sd, sd(differences.flatten))
        case _ => false
    }
    participantValues.forall(_._3) && contrastsOk && membership(c, r)

  // ------------------------------------------------------------------ independent membership
  //
  // The oracle decides which queries each cell holds on its own: its own
  // three-valued filter, its own comparisons, and its own group assignment
  // with its own bin edges. None of the reduction's term, predicate, bin or
  // comparison code is used, so a cell holding the wrong queries fails here.

  private def numeric(q: Query[StudyKey], t: NumericTerm): Option[Double] = t match
    case NumericTerm.Occurrence         => Some(q.occurrence.toDouble)
    case NumericTerm.Covariate(name, _) =>
      q.covariate(name) match
        case Value.Present(CovariateValue.Number(d)) => Some(d)
        case _                                       => None
    case NumericTerm.Window(m) => q.measure(m).toOption

  private def level(q: Query[StudyKey], t: LevelTerm): Option[String] = t match
    case LevelTerm.Layout(LayoutField.Participant) => Some(q.participant)
    case LevelTerm.Layout(LayoutField.Item)        => Some(q.item)
    case LevelTerm.Layout(LayoutField.Phase)       => Some(q.phase)
    case LevelTerm.Categorical(name, _)            => covariateLevel(q, name)
    case LevelTerm.Ordinal(name, _)                => covariateLevel(q, name)

  private def covariateLevel(q: Query[StudyKey], name: CovariateName): Option[String] =
    q.covariate(name) match
      case Value.Present(CovariateValue.Level(s)) => Some(s)
      case _                                      => None

  private def flag(q: Query[StudyKey], t: FlagTerm): Option[Boolean] = t match
    case FlagTerm.Binary(name) =>
      q.covariate(name) match
        case Value.Present(CovariateValue.Flag(b)) => Some(b)
        case _                                     => None

  private def present(q: Query[StudyKey], t: Term): Boolean = t match
    case n: NumericTerm => numeric(q, n).isDefined
    case l: LevelTerm   => level(q, l).isDefined
    case f: FlagTerm    => flag(q, f).isDefined

  /** Kleene logic: `None` is unknown. */
  private def decide(q: Query[StudyKey], p: Predicate): Option[Boolean] = p match
    case Predicate.Cmp(t, comparison, threshold) =>
      numeric(q, t).map(x =>
        comparison match
          case Comparison.Less           => x < threshold
          case Comparison.LessOrEqual    => !(x > threshold)
          case Comparison.Greater        => threshold < x
          case Comparison.GreaterOrEqual => !(x < threshold)
      )
    case Predicate.In(t, levels)     => level(q, t).map(levels.contains)
    case Predicate.AtLeast(t, least) =>
      level(q, t).map(s => t.levels.values.indexOf(s) >= t.levels.values.indexOf(least))
    case Predicate.Is(t, value) => flag(q, t).map(_ == value)
    case Predicate.IsMissing(t) => Some(!present(q, t))
    case Predicate.And(l, r)    =>
      (decide(q, l), decide(q, r)) match
        case (Some(false), _) | (_, Some(false)) => Some(false)
        case (Some(true), Some(true))            => Some(true)
        case _                                   => None
    case Predicate.Or(l, r) =>
      (decide(q, l), decide(q, r)) match
        case (Some(true), _) | (_, Some(true)) => Some(true)
        case (Some(false), Some(false))        => Some(false)
        case _                                 => None
    case Predicate.Not(inner) => decide(q, inner).map(!_)

  private def levelOf(q: Query[StudyKey], g: Grouping): Option[String] = g match
    case Grouping.ByLevel(t)      => level(q, t)
    case Grouping.ByFlag(t)       => flag(q, t).map(_.toString)
    case Grouping.ByBins(t, bins) =>
      numeric(q, t).flatMap(x =>
        bins.bins
          .find(b =>
            b.from <= x && (x < b.until || (b.upper == UpperEdge.Included && x == b.until))
          )
          .map(_.label)
      )

  /** Each cell holds exactly the queries that pass the filter, have a value
    * for its role and belong to its group; each role's filter counts agree.
    */
  def membership(c: ReportCase, r: Report[StudyKey]): Boolean =
    val decided = c.table.queries.map(q => q -> c.spec.filter.fold(Option(true))(decide(q, _)))
    def groupOf(q: Query[StudyKey]): Option[GroupKey] =
      val levels = c.spec.groupBy.map(g => levelOf(q, g).map(g.on.render -> _))
      Option.when(levels.forall(_.isDefined))(GroupKey(levels.flatten))
    def key(ref: ResultRef[StudyKey]): Option[StudyKey] = ref match
      case ResultRef.Reduction(_, _, k) => Some(k)
      case ResultRef.ContrastRow(_, k)  => Some(k)
      case _                            => None
    val cellsOk = r.cells.forall { cell =>
      val passing = decided.collect {
        case (q, Some(true)) if groupOf(q).contains(cell.group) => q
      }
      val (valued, unvalued) = passing.partition(q =>
        q.outcome(cell.role) match
          case RoleOutcome.Scored(_) => true
          case _                     => false
      )
      cell.members.flatMap(key).sortBy(_.toString) == valued.map(_.key).sortBy(_.toString) &&
      cell.queries == valued.size && cell.failed == unvalued.size
    }
    val booksOk = r.accounting.forall { a =>
      a.filteredOut == decided.count(_._2.contains(false)) &&
      a.unknownPredicate == decided.count(_._2.isEmpty)
    }
    cellsOk && booksOk

/** Generators of report cases: participants with differing numbers of
  * queries, dyadic and decimal scores, covariates that are sometimes missing or unparsed, undefined window
  * shares, failed and unstored rows, and specifications over every filter,
  * grouping, reduction and contrast form.
  */
object ReportGenerators:
  private def sure[E, A](e: Either[E, A]): A =
    e.fold(x => throw new IllegalStateException(s"$x"), identity)

  val memory: CovariateName       = sure(CovariateName.of("memory"))
  val confidence: CovariateName   = sure(CovariateName.of("confidence"))
  val memoryLevels: Levels        = sure(Levels.of(Vector("Remembered", "Forgotten")))
  val rating: NumericUnit         = sure(NumericUnit.of("rating"))
  val memoryTerm: LevelTerm       = LevelTerm.Categorical(memory, memoryLevels)
  val confidenceTerm: NumericTerm = NumericTerm.Covariate(confidence, rating)
  val share: NumericTerm          = NumericTerm.Window(WindowMeasure.OutsideWindowShare)
  val item: LevelTerm             = LevelTerm.Layout(LayoutField.Item)

  val schema: CovariateSchema = sure(
    CovariateSchema.of(
      Vector(
        Covariate(memory, CovariateType.Categorical(memoryLevels)),
        Covariate(confidence, CovariateType.Numeric(rating))
      )
    )
  )

  private val failure = DiagnosticCode("contrast-row", "arithmetic")

  /** Dyadic scores, whose sums are exact, and decimal ones, whose sums depend
    * on their order, so a reduction that sums in input order is caught.
    */
  val genScore: Gen[Double] = Gen.oneOf(
    Gen.choose(-64, 64).map(_ / 64.0),
    Gen.choose(-100, 100).map(_ / 100.0)
  )

  private def outcome(scored: Int): Gen[RoleOutcome] = Gen.frequency(
    scored -> genScore.map(v => RoleOutcome.Scored(Vector(v))),
    2      -> Gen.const(RoleOutcome.Failed(failure, "failed")),
    1      -> Gen.const(RoleOutcome.NotStored)
  )

  private def query(participant: String, item: String): Gen[Query[StudyKey]] = for
    remembered <- Gen.frequency(
      4 -> Gen.const(Value.Present(CovariateValue.Level("Remembered"))),
      4 -> Gen.const(Value.Present(CovariateValue.Level("Forgotten"))),
      1 -> Gen.const(Value.Missing(Absence.NotRecorded))
    )
    rated <- Gen.frequency(
      // Ratings sit on every threshold and bin edge the specifications use.
      8 -> Gen.oneOf(1.0, 2.0, 2.5, 3.0, 4.0).map(r => Value.Present(CovariateValue.Number(r))),
      1 -> Gen.const(Value.Missing(Absence.NotRecorded)),
      1 -> Gen.const(Value.Missing(Absence.Unparsed))
    )
    windowShare <- Gen.frequency(
      8 -> Gen.oneOf(0.0, 0.125, 0.25, 0.5).map(Value.Present(_)),
      1 -> Gen.const(Value.Missing(Absence.Undefined(UndefinedReason.ZeroDuration)))
    )
    matched    <- outcome(12)
    control    <- outcome(12)
    difference <- outcome(12)
  yield sure(
    Query.of(
      StudyKey(participant, item, "recall"),
      participant,
      item,
      "recall",
      1,
      Vector(memory -> remembered, confidence -> rated),
      Option.when(rated == Value.Missing(Absence.Unparsed))(confidence -> "high").toVector,
      Vector(share -> windowShare).collect { case (NumericTerm.Window(m), v) => m -> v },
      matched,
      control,
      difference
    )
  )

  val genTable: Gen[QueryTable[StudyKey]] = for
    people <- Gen.choose(1, 5)
    perPerson = (p: Int) =>
      Gen
        .choose(1, 5)
        .flatMap(n =>
          Gen.sequence[Vector[Query[StudyKey]], Query[StudyKey]](
            (1 to n).toVector.map(i => query(s"p$p", s"i$i"))
          )
        )
    queries <- Gen
      .sequence[Vector[Vector[Query[StudyKey]]], Vector[Query[StudyKey]]](
        (1 to people).toVector.map(perPerson)
      )
      .map(_.flatten)
  yield sure(QueryTable.of(0, Vector("value"), schema, queries))

  val genFilter: Gen[Option[Predicate]] = Gen.oneOf(
    Gen.const(None),
    Gen.const(Some(Predicate.Cmp(confidenceTerm, Comparison.GreaterOrEqual, 2.0))),
    Gen.const(Some(Predicate.Cmp(confidenceTerm, Comparison.Less, 2.5))),
    Gen.const(Some(Predicate.Cmp(confidenceTerm, Comparison.LessOrEqual, 2.0))),
    Gen.const(Some(Predicate.Cmp(confidenceTerm, Comparison.Greater, 2.5))),
    Gen.const(Some(Predicate.In(memoryTerm, Vector("Remembered")))),
    Gen.const(Some(Predicate.Cmp(share, Comparison.LessOrEqual, 0.25))),
    Gen.const(Some(Predicate.IsMissing(confidenceTerm))),
    Gen.const(
      Some(
        Predicate.Or(
          Predicate.Cmp(share, Comparison.Less, 0.5),
          Predicate.Not(Predicate.In(item, Vector("i1")))
        )
      )
    ),
    Gen.const(
      Some(
        Predicate.And(
          Predicate.Cmp(confidenceTerm, Comparison.Greater, 1.0),
          Predicate.Not(Predicate.IsMissing(memoryTerm))
        )
      )
    )
  )

  private val bins: Bins = sure(
    Bins.of(
      Vector(
        sure(Bin.of("low", 1, 2.5, UpperEdge.Excluded)),
        sure(Bin.of("high", 2.5, 4, UpperEdge.Included))
      )
    )
  )

  /** Bins open at the top: a rating of 4 lies in none of them. */
  private val openBins: Bins = sure(
    Bins.of(
      Vector(
        sure(Bin.of("low", 1, 2.5, UpperEdge.Excluded)),
        sure(Bin.of("high", 2.5, 4, UpperEdge.Excluded))
      )
    )
  )

  val genGrouping: Gen[Vector[Grouping]] = Gen.oneOf(
    Vector(Grouping.ByBins(confidenceTerm, openBins)),
    Vector.empty,
    Vector(Grouping.ByLevel(memoryTerm)),
    Vector(Grouping.ByLevel(item)),
    Vector(Grouping.ByBins(confidenceTerm, bins)),
    Vector(Grouping.ByBins(confidenceTerm, bins), Grouping.ByLevel(memoryTerm))
  )

  val genReduce: Gen[ReducePolicy] = Gen.oneOf(
    ReducePolicy.default,
    ReducePolicy.ParticipantMeans(sure(MinimumQueries.of(2))),
    ReducePolicy.PooledQueries
  )

  val genSelection: Gen[ReportSelection] =
    Gen
      .someOf(Role.values.toVector)
      .suchThat(_.nonEmpty)
      .map(roles => sure(ReportSelection.of(roles.toVector.sortBy(_.ordinal), Vector("value"))))

  val genSpec: Gen[ReportSpec] = for
    selection <- genSelection
    filter    <- genFilter
    groupBy   <- genGrouping
    reduce    <- genReduce
    contrast  <- Gen
      .oneOf(true, false)
      .map(on =>
        Option.when(on && groupBy.exists(_.on == memoryTerm))(
          LevelContrast(memoryTerm, "Remembered", "Forgotten")
        )
      )
    spread <- Gen.oneOf(Spread.values.toVector)
    id     <- Gen.oneOf("memory", "window", "items")
  yield sure(
    ReportSpec
      .of(sure(ReportId.of(id)), 0, selection, filter, groupBy, reduce, contrast, spread)
  )

  val genCase: Gen[ReportCase] = for
    spec  <- genSpec
    table <- genTable
  yield ReportCase(spec, table)

  /** Participant p1 has three queries of mean 1, p2 one query of -1. */
  val witness: ReportCase =
    def scored(p: String, item: String, v: Double) = sure(
      Query.of(
        StudyKey(p, item, "recall"),
        p,
        item,
        "recall",
        1,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        RoleOutcome.NotStored,
        RoleOutcome.NotStored,
        RoleOutcome.Scored(Vector(v))
      )
    )
    ReportCase(
      sure(
        ReportSpec.of(
          sure(ReportId.of("witness")),
          0,
          sure(ReportSelection.of(Vector(Role.Difference), Vector("value")))
        )
      ),
      sure(
        QueryTable.of(
          0,
          Vector("value"),
          CovariateSchema.empty,
          Vector(
            scored("p1", "a", 1.0),
            scored("p1", "b", 1.0),
            scored("p1", "c", 1.0),
            scored("p2", "a", -1.0)
          )
        )
      )
    )

  /** Generated covariate declarations of every type. */
  val genCovariateSchema: Gen[CovariateSchema] = for
    n     <- Gen.choose(0, 4)
    kinds <- Gen.listOfN(
      n,
      Gen.oneOf(
        Gen.const(CovariateType.Numeric(rating)),
        Gen.const(CovariateType.Ordinal(sure(Levels.of(Vector("low", "mid", "high"))))),
        Gen.const(CovariateType.Categorical(memoryLevels)),
        Gen.const(CovariateType.Binary)
      )
    )
  yield sure(
    CovariateSchema.of(
      kinds.zipWithIndex.map((k, i) => Covariate(sure(CovariateName.of(s"c$i")), k)).toVector
    )
  )

  /** Generated reports: the shipped reduction of a generated case. */
  val genReport: Gen[Report[StudyKey]] =
    genCase.map(c => sure(ReportLaws.shipped(c.spec, c.table)))
