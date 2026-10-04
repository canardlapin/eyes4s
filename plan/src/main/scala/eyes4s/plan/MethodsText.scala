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

package eyes4s.plan

import eyes4s.core.Weight
import eyes4s.design.FailurePolicy
import eyes4s.kernel.*
import eyes4s.surface.EdgePolicy

/** What a value in a methods sentence stands for, so a host styles it
  * without parsing English.
  */
enum TokenRole derives CanEqual:
  /** The focal (query) phase. */
  case Query

  /** The reference phase. */
  case Reference

  /** The matched-reference rule. */
  case Match

  /** The control rule. */
  case Control

  /** A spatial scale: bandwidths, the grid, the declared units per degree. */
  case Scale

  /** The comparison method. */
  case Metric

  /** A policy: weighting, windows, exclusions, failures. */
  case Policy

/** Which part of a fact a token shows. */
enum FactPart derives CanEqual:
  /** The fact's value: a count, a range, a share, a label or a policy; a
    * controls fact's range; a breakdown's number of cells.
    */
  case Value

  /** The slot's own name: a cause's or failure's code label, a group level. */
  case Name

  /** A control count of a controls fact. */
  case Controls

  /** A query count: of a control count, or of a breakdown cell. */
  case Queries

  /** Why the queries of a control count had fewer controls. */
  case Loss

  /** A breakdown cell's participant or group. */
  case Participant, Group

/** One piece of a methods sentence: fixed English words, a value drawn from a
  * form field with its role, or one number or name of a fact from beyond the
  * plan ([[MethodsFacts]]) with its slot, the part it shows, its one source
  * and its typed value; `shown` is the default English rendering, which a
  * host may localise from `value` without parsing it.
  */
enum Token derives CanEqual:
  case Words(words: String)
  case Value(field: FieldId, role: TokenRole, shown: String)
  case Fact(slot: FactSlot, part: FactPart, source: FactSource, value: FactValue, shown: String)

  def text: String = this match
    case Words(w)            => w
    case Value(_, _, s)      => s
    case Fact(_, _, _, _, s) => s

/** What a methods sentence is about: one recipe field, or the facts of the
  * admission, the design, the contrast or the reporting.
  */
enum ClauseTopic derives CanEqual:
  case Field(field: FieldId)
  case Admission, Design, Contrast, Reporting

/** A sentence about one topic. */
final case class Clause(topic: ClauseTopic, tokens: Vector[Token]) derives CanEqual:
  def text: String = tokens.map(_.text).mkString

  /** The recipe field the sentence is about, if it is about one. */
  def field: Option[FieldId] = topic match
    case ClauseTopic.Field(f) => Some(f)
    case _                    => None

object Clause:
  /** A sentence about a recipe field. */
  def apply(field: FieldId, tokens: Vector[Token]): Clause =
    Clause(ClauseTopic.Field(field), tokens)

/** Clauses in reading order. `text` is the default English rendering; a host
  * localises from the tokens' fields and roles.
  */
final case class Phrase(clauses: Vector[Clause]) derives CanEqual:
  def tokens: Vector[Token] = clauses.flatMap(_.tokens)
  def text: String          = clauses.map(_.text).mkString(" ")

/** The recipe sentence (Studio S7.2) and the methods text (S9.4) of a study
  * plan, built from its typed values. Run counts (eligible queries, records
  * outside the window) are not part of a plan and are left to the caller.
  */
object StudyText:
  import Token.*

  import StudyForm.ids
  private val weight       = ids.weight
  private val failure      = ids.failurePolicy
  private val grid         = ids.grid
  private val window       = ids.window
  private val offWindow    = ids.offWindow
  private val scales       = ids.scales
  private val angularScale = ids.angularScale
  private val initial      = ids.initialFixations
  private val method       = ids.method

  private val phases  = ids.phases
  private val pairing = ids.pairing

  // A value drawn from a part of a composite field names the part's path.
  private def path(field: FieldId, part: String) = FieldId.literal(s"${field.value}.$part")
  private val focal                              = path(phases, "focal")
  private val reference                          = path(phases, "reference")
  private val matchedId                          = path(pairing, "matched")
  private val controlId                          = path(pairing, "controls")
  private val unmatchedId                        = path(pairing, "unmatched")

  private def w(s: String) = Words(s)

  private def length(x: Double, unit: PlanarUnit): String =
    if unit == PlanarUnit.Deg then s"${Display.decimals(x, 2)}°"
    else s"${Display.decimals(x, 2)} ${unit.symbol}"

  private def pair(v: PerAxis, unit: PlanarUnit): String =
    if v.x == v.y then length(v.x, unit)
    else s"${Display.decimals(v.x, 2)} × ${length(v.y, unit)}"

  private def matched(m: MatchedReferences): String = m match
    case MatchedReferences.RequireOne     => "the same item"
    case MatchedReferences.SameOccurrence => "the same item and occurrence"
    case MatchedReferences.MeanOfAll      => "every presentation of the same item, averaged"
    case MatchedReferences.Select(OccurrenceChoice.First) =>
      "the first presentation of the same item"
    case MatchedReferences.Select(OccurrenceChoice.Last) =>
      "the last presentation of the same item"
    case MatchedReferences.Select(OccurrenceChoice.At(n)) =>
      s"presentation ${n.value} of the same item"

  private def controls(c: ControlReferences): String = c match
    case ControlReferences.SameSelection  => "the other items, chosen alike"
    case ControlReferences.AllOccurrences => "every presentation of the other items"

  private def weighting(x: Weight): String = x match
    case Weight.Uniform  => "unweighted (count)"
    case Weight.Duration => "duration-weighted"

  private def edgeText(e: EdgePolicy): String = e match
    case EdgePolicy.Truncate    => "truncated at the grid edge"
    case EdgePolicy.Renormalise => "renormalised at the grid edge"

  private def estimate[V <: Unit2D](
      e: StudyEstimate[V],
      unit: PlanarUnit,
      withEdges: Boolean
  ): String =
    def edges(p: EdgePolicy) = if withEdges then s" (${edgeText(p)})" else ""
    e match
      case StudyEstimate.Binned()             => "binned"
      case StudyEstimate.Gaussian(s, p)       => s"σ ${length(s.value, unit)}${edges(p)}"
      case StudyEstimate.Anisotropic(x, y, p) =>
        s"σ ${Display.decimals(x.value, 2)} × ${length(y.value, unit)}${edges(p)}"

  private def scale[U <: Unit2D](
      s: StudyScale[U],
      unit: PlanarUnit,
      withEdges: Boolean
  ): String =
    s match
      case StudyScale.Native(e)  => estimate(e, unit, withEdges)
      case StudyScale.Angular(e) => estimate(e, PlanarUnit.Deg, withEdges)

  /** The scales; each states its edge policy when the policies differ. */
  private def scaleList[U <: Unit2D](plan: StudyPlan[?, U, ?, ?, ?], unit: PlanarUnit) =
    val mixed = edgePolicies(plan).size > 1
    plan.scales.map(scale(_, unit, mixed)).mkString(", ")

  private def edgePolicies[U <: Unit2D](plan: StudyPlan[?, U, ?, ?, ?]): Vector[EdgePolicy] =
    plan.estimates.collect {
      case StudyEstimate.Gaussian(_, e)       => e
      case StudyEstimate.Anisotropic(_, _, e) => e
    }.distinct

  /** The one edge policy every smoothed scale shares, if they share one. */
  private def edges[U <: Unit2D](plan: StudyPlan[?, U, ?, ?, ?]): Option[EdgePolicy] =
    edgePolicies(plan) match
      case Vector(e) => Some(e)
      case _         => None

  /** The one-sentence recipe summary of Studio S7.2. */
  def sentence[K, U <: Unit2D, P, S, D](plan: StudyPlan[K, U, P, S, D])(using
      u: UnitLabel[U]
  ): Phrase =
    Phrase(
      Vector(
        Clause(
          phases,
          Vector(
            w("Compare "),
            Value(focal, TokenRole.Query, plan.focalPhase),
            w(" fixations with "),
            Value(reference, TokenRole.Reference, plan.referencePhase),
            w(" fixations of "),
            Value(matchedId, TokenRole.Match, matched(plan.pairing.matched)),
            w(", against "),
            Value(controlId, TokenRole.Control, controls(plan.pairing.controls)),
            w(", by "),
            Value(method, TokenRole.Metric, plan.method.name),
            w(" of "),
            Value(weight, TokenRole.Policy, weighting(plan.weight)),
            w(" maps at "),
            Value(scales, TokenRole.Scale, scaleList(plan, u.planar)),
            w(".")
          )
        )
      )
    )

  /** The methods text of Studio S9.4: one clause per recipe field the plan
    * declares, the method-determined clauses on the contrast, and, in reading
    * order, a clause for the admission, design and reporting facts given
    * (CR6d). Every fact given is stated, each in tokens carrying that fact
    * and so its one source; a fact not given is not stated, and with no facts
    * the text states the plan alone.
    */
  def methods[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      facts: MethodsFacts = MethodsFacts.empty
  )(using u: UnitLabel[U]): Phrase =
    val known      = FactText(facts)
    val unit       = u.planar
    val advice     = StudyAdvice.facts(plan)
    val comparison = Clause(
      phases,
      Vector(
        w("Each "),
        Value(focal, TokenRole.Query, plan.focalPhase),
        w(" trial was compared with the "),
        Value(reference, TokenRole.Reference, plan.referencePhase),
        w(
          if plan.pairing.matched == MatchedReferences.MeanOfAll then " trials of "
          else " trial of "
        ),
        Value(matchedId, TokenRole.Match, matched(plan.pairing.matched)),
        w(" and with the "),
        Value(reference, TokenRole.Reference, plan.referencePhase),
        w(" trials of "),
        Value(controlId, TokenRole.Control, controls(plan.pairing.controls)),
        w(", within participant.")
      )
    )
    val unmatched = Clause(
      pairing,
      Vector(
        w("Queries without a matched reference were "),
        Value(
          unmatchedId,
          TokenRole.Policy,
          plan.pairing.unmatched match
            case UnmatchedFocalPolicy.ReportNoMatch => "reported as no match"
            case UnmatchedFocalPolicy.Refuse        => "grounds to refuse the study"
        ),
        w(".")
      )
    )
    def region(b: Bounds[U]) =
      s"[${Display.decimals(b.xMin, 2)}, ${Display.decimals(b.xMax, 2)}) × " +
        s"[${Display.decimals(b.yMin, 2)}, ${Display.decimals(b.yMax, 2)}) ${unit.symbol}"
    val mapped = plan.geometry match
      case StudyGeometry.Windowed(win, _, policy) =>
        Clause(
          window,
          Vector(
            w("Maps covered the analysis window "),
            Value(window, TokenRole.Policy, region(win.region)),
            w(" of the admission frame; fixations on the screen outside it "),
            Value(
              offWindow,
              TokenRole.Policy,
              policy match
                case OffWindowPolicy.Exclude   => "were left out of the map"
                case OffWindowPolicy.FailTrial => "failed their trial"
            ),
            w(".")
          ) ++ known.outsideWindow
        )
      case other =>
        Clause(
          window,
          Vector(
            w("Maps covered the whole admission frame "),
            Value(window, TokenRole.Policy, region(other.admission.bounds)),
            w(".")
          ) ++ known.outsideWindow
        )
    val declared = plan.angularScale.toVector.map(s =>
      Clause(
        angularScale,
        Vector(
          w("Degrees were converted linearly at a declared "),
          Value(
            angularScale,
            TokenRole.Scale,
            s"${Display.decimals(s.unitsPerDegree, 2)} ${unit.symbol}/°"
          ),
          w(", not a calibration.")
        )
      )
    )
    val initialClause = Clause(
      initial,
      plan.initialFixations match
        case InitialFixationPolicy.KeepAll() =>
          Vector(Value(initial, TokenRole.Policy, "Every fixation was kept"), w("."))
        case InitialFixationPolicy.DropFirst() =>
          Vector(
            w("The "),
            Value(initial, TokenRole.Policy, "first fixation"),
            w(" of every trial was left out, in queries and references alike.")
          )
        case InitialFixationPolicy.DropLeadingInClosedDisc(cross, r) =>
          Vector(
            w("Leading fixations within "),
            Value(initial, TokenRole.Policy, s"${Display.decimals(r, 2)}°"),
            w(" of the fixation cross at "),
            Value(
              initial,
              TokenRole.Policy,
              s"(${Display.decimals(cross.x, 2)}, ${Display.decimals(cross.y, 2)}) ${unit.symbol}"
            ),
            w(" were left out of every trial, in queries and references alike.")
          )
    )
    val gridClause = Clause(
      grid,
      Vector(
        w("Maps were estimated on a "),
        Value(grid, TokenRole.Scale, s"${plan.grid.nx} × ${plan.grid.ny}"),
        w(" grid (cell "),
        Value(grid, TokenRole.Scale, pair(advice.cell, unit))
      ) ++ advice.cellDegrees.toVector.flatMap(d =>
        Vector(w(", "), Value(grid, TokenRole.Scale, pair(d, PlanarUnit.Deg)))
      ) ++ Vector(w(")."))
    )
    val scaleClause = Clause(
      scales,
      Vector(
        w("Each map summed "),
        Value(weight, TokenRole.Policy, weighting(plan.weight)),
        w(" fixations at "),
        Value(scales, TokenRole.Scale, scaleList(plan, unit))
      ) ++ edges(plan).toVector.flatMap(e =>
        Vector(
          w(", "),
          Value(scales, TokenRole.Policy, edgeText(e))
        )
      ) ++ Vector(w("; each scale was analysed on its own."))
    )
    val metric = Clause(
      method,
      Vector(
        w("Maps were compared by "),
        Value(method, TokenRole.Metric, plan.method.name),
        w(".")
      )
    )
    // Method-determined words: what M, B and D are, and what D measures. Under
    // MeanOfAll a query has several matched scores and M is their mean.
    val matchedScore = plan.pairing.matched match
      case MatchedReferences.MeanOfAll => "M was the mean of its matched scores"
      case _                           => "M was its matched score"
    val contrast = Clause(
      ClauseTopic.Contrast,
      Vector(
        w(
          s"For each query, $matchedScore and B the mean of its control scores, " +
            "and D = M − B. D measures spatial correspondence, not sequential replay."
        )
      )
    )
    val failures = Clause(
      failure,
      plan.policy match
        case FailurePolicy.RequireAll =>
          Vector(
            Value(failure, TokenRole.Policy, "Every selected pair had to be scored"),
            w(", so a query's contrast needed all its pairs.")
          )
        case FailurePolicy.SuccessfulOnly(m) =>
          Vector(
            w("Reductions used successful scores only, requiring at least "),
            Value(failure, TokenRole.Policy, s"${m.value} successful scores"),
            w(".")
          )
    )
    Phrase(
      known.admission ++ Vector(comparison, unmatched) ++ known.design ++
        Vector(mapped) ++ known.outsideScreen ++ declared ++
        Vector(initialClause, gridClause, scaleClause, metric, contrast, failures) ++
        known.reporting
    )

/** The clauses and tokens a methods text states from its facts. Every number
  * and name is a fact token with its own slot and source; the words around
  * them state no number.
  */
private final class FactText(facts: MethodsFacts):
  import FactText.*
  private type F = eyes4s.plan.Fact
  private def w(s: String) = Token.Words(s)

  private def part(
      f: F,
      p: FactPart,
      value: FactValue,
      source: Option[FactSource] = None
  ): Token = Token.Fact(f.slot, p, source.getOrElse(f.source), value, show(value))

  /** The fact's value as one token: a controls fact shows its range, a
    * breakdown its number of cells.
    */
  private def number(f: F): Token               = part(f, FactPart.Value, valueOf(f))
  private def name(f: F, text: String): Token   = part(f, FactPart.Name, FactValue.Label(text))
  private def at(slot: FactSlot): Option[Token] = facts.get(slot).map(number)

  /** " record" or " records", as the slot's count is one or not. */
  private def noun(slot: FactSlot, singular: String, plural: String): String =
    facts.get(slot).map(valueOf) match
      case Some(FactValue.Count(1)) | Some(FactValue.Range(_, 1)) => singular
      case _                                                      => plural

  /** "a, b and c". */
  private def joined(parts: Vector[Vector[Token]], and: String = " and "): Vector[Token] =
    parts.zipWithIndex.flatMap { (p, i) =>
      val sep =
        if i == 0 then Vector.empty
        else if i == parts.size - 1 then Vector(w(and))
        else Vector(w(", "))
      sep ++ p
    }

  /** The facts of a parameterised slot, largest count first: "overlap 6". */
  private def named(name: PartialFunction[FactSlot, String]): Vector[Vector[Token]] =
    facts.facts
      .filter(f => name.isDefinedAt(f.slot))
      .sortBy(f =>
        (
          f.value match
            case FactValue.Count(n) => -n
            case _                  => 0L
          ,
          f.slot.slotId
        )
      )
      .map(f => Vector(this.name(f, name(f.slot)), w(" "), number(f)))

  /** " (overlap 6, rejected-records 2)". */
  private def listed(items: Vector[Vector[Token]]): Vector[Token] =
    if items.isEmpty then Vector.empty
    else
      Vector(w(" (")) ++ items.zipWithIndex.flatMap((c, i) =>
        (if i == 0 then Vector.empty else Vector(w(", "))) ++ c
      ) :+ w(")")

  /** "Fixations (11,520 records) from dataset r3 were admitted by trial: 937 of
    * 960 inventory trials were admitted, 12 quarantined (overlap 6, …), 5 with
    * no admitted fixations and 6 absent (no fixation records)."
    */
  def admission: Vector[Clause] =
    val opening = (at(FactSlot.FixationRecords), at(FactSlot.DatasetRevision)) match
      case (None, None)       => Vector.empty
      case (records, dataset) =>
        Vector(w("Fixations")) ++
          records.toVector.flatMap(r =>
            Vector(w(" ("), r, w(noun(FactSlot.FixationRecords, " record)", " records)")))
          ) ++
          dataset.toVector.flatMap(d => Vector(w(" from dataset "), d)) ++
          Vector(w(" were admitted by trial"))
    val causes    = listed(named { case FactSlot.QuarantineCause(c) => c.label })
    val admitted  = at(FactSlot.Admitted)
    val inventory = at(FactSlot.InventoryTrials)
    val counts    = Vector(
      admitted.map(a =>
        Vector(a) ++ inventory.toVector.flatMap(i =>
          Vector(
            w(" of "),
            i,
            w(noun(FactSlot.InventoryTrials, " inventory trial", " inventory trials"))
          )
        ) ++ Vector(w(noun(FactSlot.Admitted, " was admitted", " were admitted")))
      ),
      at(FactSlot.Quarantined) match
        case Some(q) => Some(Vector(q, w(" quarantined")) ++ causes)
        case None => Option.when(causes.nonEmpty)(Vector(w("some were quarantined")) ++ causes)
      ,
      at(FactSlot.NoFixations).map(n => Vector(n, w(" with no admitted fixations"))),
      at(FactSlot.Absent).map(a => Vector(a, w(" absent (no fixation records)")))
    ).flatten
    // Without an admitted count, the inventory leads the list: "of 960 inventory trials, …".
    val lead   = if admitted.isDefined then None else inventory
    val trials = noun(FactSlot.InventoryTrials, " inventory trial", " inventory trials")
    val main   = Option.when(opening.nonEmpty || counts.nonEmpty || lead.nonEmpty) {
      val body = (opening.nonEmpty, counts.nonEmpty, lead) match
        case (_, true, Some(i)) =>
          val of = Vector(i, w(s"$trials, ")) ++ joined(counts)
          if opening.isEmpty then w("Of ") +: of else opening ++ (w(": of ") +: of)
        case (false, true, None) => Vector(w("Of the inventory trials, ")) ++ joined(counts)
        case (true, true, None)  => opening ++ Vector(w(": ")) ++ joined(counts)
        case (_, false, Some(i)) =>
          if opening.isEmpty then
            Vector(
              w("The inventory held "),
              i,
              w(noun(FactSlot.InventoryTrials, " trial", " trials"))
            )
          else opening ++ Vector(w(" from "), i, w(trials))
        case (_, false, None) => opening
      Clause(ClauseTopic.Admission, body :+ w("."))
    }
    // The tallied records join the window sentence when it counts records.
    val tallied =
      if facts.get(FactSlot.RecordsOutsideWindow).isDefined then Vector.empty
      else
        at(FactSlot.TalliedRecords).toVector.map(t =>
          Clause(
            ClauseTopic.Admission,
            Vector(
              w("The admitted trials held "),
              t,
              w(noun(FactSlot.TalliedRecords, " fixation record.", " fixation records."))
            )
          )
        )
    main.toVector ++ tallied

  /** " 543 of the 11,311 fixation records of admitted trials, in 409 trials,
    * fell outside it (4.8% of their fixation duration).", after the sentence
    * naming the mapped region.
    */
  def outsideWindow: Vector[Token] =
    val trials   = at(FactSlot.TrialsOutsideWindow).toVector
    val inTrials = noun(FactSlot.TrialsOutsideWindow, " trial", " trials")
    val share    = at(FactSlot.OutsideWindowShare).toVector
    val basis    = facts.get(FactSlot.OutsideWindowShare).map(valueOf).collect {
      case FactValue.Share(_, b) => b.label
    }
    val ofDuration = share.flatMap(s => Vector(w(" ("), s, w(s" of their ${basis.mkString})")))
    at(FactSlot.RecordsOutsideWindow) match
      case Some(r) =>
        val records = at(FactSlot.TalliedRecords) match
          case Some(t) =>
            Vector(
              w(" of the "),
              t,
              w(
                noun(
                  FactSlot.TalliedRecords,
                  " fixation record of admitted trials",
                  " fixation records of admitted trials"
                )
              )
            )
          case None => Vector(w(noun(FactSlot.RecordsOutsideWindow, " record", " records")))
        Vector(w(" "), r) ++ records ++
          trials.flatMap(t => Vector(w(", in "), t, w(s"$inTrials,"))) ++
          Vector(w(" fell outside it")) ++ ofDuration :+ w(".")
      case None if trials.nonEmpty =>
        Vector(w(" Records in ")) ++ trials ++ Vector(w(s"$inTrials fell outside it")) ++
          ofDuration :+ w(".")
      case None =>
        share.flatMap(s =>
          Vector(w(" Fixations outside it took "), s, w(s" of the ${basis.mkString}."))
        )

  /** "120 records, in 40 trials, fell outside the screen; they were admitted
    * but left out of every map."
    */
  def outsideScreen: Vector[Clause] =
    val trials  = noun(FactSlot.TrialsOutsideScreen, " trial", " trials")
    val subject = (at(FactSlot.RecordsOutsideScreen), at(FactSlot.TrialsOutsideScreen)) match
      case (Some(r), Some(t)) =>
        Vector(
          r,
          w(noun(FactSlot.RecordsOutsideScreen, " record, in ", " records, in ")),
          t,
          w(s"$trials,")
        )
      case (Some(r), None) =>
        Vector(r, w(noun(FactSlot.RecordsOutsideScreen, " record", " records")))
      case (None, Some(t)) => Vector(w("Records in "), t, w(trials))
      case (None, None)    => Vector.empty
    val policy = at(FactSlot.OffScreenPolicy)
    if subject.isEmpty then
      policy.toVector.map(p =>
        Clause(ClauseTopic.Admission, Vector(w("Records outside the screen "), p, w(".")))
      )
    else
      Vector(
        Clause(
          ClauseTopic.Admission,
          subject ++ Vector(w(" fell outside the screen")) ++
            policy.toVector.flatMap(p => Vector(w("; they "), p)) :+ w(".")
        )
      )

  /** "Of 480 requested queries, 457 were eligible; 454 contributed, 3 failed
    * (off-window 3), 9 had no admitted matched trial and 14 were not admitted.
    * Of the compared queries, 286 had 19 controls and 171 had 18 (overlap)."
    */
  def design: Vector[Clause] =
    val failures = listed(named { case FactSlot.FailureCause(c) => c.label })
    val left     = Vector(
      at(FactSlot.ContributingQueries).map(c => Vector(c, w(" contributed"))),
      at(FactSlot.FailedQueries) match
        case Some(f) => Some(Vector(f, w(" failed")) ++ failures)
        case None    => Option.when(failures.nonEmpty)(Vector(w("some failed")) ++ failures)
      ,
      at(FactSlot.UnmatchedQueries).map(u => Vector(u, w(" had no admitted matched trial"))),
      at(FactSlot.NotAdmittedQueries).map(n =>
        Vector(
          n,
          w(noun(FactSlot.NotAdmittedQueries, " was not admitted", " were not admitted"))
        )
      )
    ).flatten
    val eligibility =
      (at(FactSlot.RequestedQueries), at(FactSlot.EligibleQueries)) match
        case (None, None) =>
          Option.when(left.nonEmpty)(
            Clause(
              ClauseTopic.Design,
              Vector(w("Of the requested queries, ")) ++ joined(left) :+ w(".")
            )
          )
        case (requested, eligible) =>
          val requestedQueries =
            noun(FactSlot.RequestedQueries, " requested query", " requested queries")
          val head = (requested, eligible) match
            case (Some(r), Some(e)) =>
              Vector(
                w("Of "),
                r,
                w(s"$requestedQueries, "),
                e,
                w(noun(FactSlot.EligibleQueries, " was eligible", " were eligible"))
              )
            case (Some(r), None) =>
              Vector(
                w("The design requested "),
                r,
                w(noun(FactSlot.RequestedQueries, " query", " queries"))
              )
            case (None, Some(e)) =>
              Vector(
                e,
                w(
                  noun(
                    FactSlot.EligibleQueries,
                    " query was eligible",
                    " queries were eligible"
                  )
                )
              )
            case (None, None) => Vector.empty
          Some(
            Clause(
              ClauseTopic.Design,
              head ++ (if left.isEmpty then Vector.empty
                       else Vector(w("; ")) ++ joined(left)) :+
                w(".")
            )
          )
    val controls = facts.get(FactSlot.ControlsPerQuery).map { f =>
      val counts = f.value match
        case FactValue.Controls(cs) => cs.sortBy(-_.controls)
        case _                      => Vector.empty
      val most  = counts.map(_.controls).maxOption.getOrElse(0)
      val items = counts.map { c =>
        Vector(
          part(f, FactPart.Queries, FactValue.Count(c.queries)),
          w(" had "),
          part(f, FactPart.Controls, FactValue.Count(c.controls.toLong)),
          w(if c.controls == 1 then " control" else " controls")
        ) ++ c.loss.toVector.flatMap { l =>
          val lost = if most - c.controls == 1 then " (one lost to " else " (others lost to "
          Vector(w(lost), part(f, FactPart.Loss, FactValue.Label(l.label)), w(")"))
        }
      }
      Clause(
        ClauseTopic.Design,
        Vector(w("Of the compared queries, ")) ++ joined(items) :+ w(".")
      )
    }
    eligibility.toVector ++ controls.toVector

  /** "P05 · Forgotten · 1 query": each part its own token, with the cell's
    * source.
    */
  private def cell(f: F, c: GroupCell): Vector[Token] =
    Vector(
      part(f, FactPart.Participant, FactValue.Label(c.participant), Some(c.source)),
      w(" · "),
      part(f, FactPart.Group, FactValue.Label(c.group), Some(c.source)),
      w(" · "),
      part(f, FactPart.Queries, FactValue.Count(c.queries.toLong), Some(c.source)),
      w(if c.queries == 1 then " query" else " queries")
    )

  private def breakdown(slot: FactSlot, says: String): Option[Clause] =
    facts.get(slot).map { f =>
      val cells = f.value match
        case FactValue.Breakdown(cs) => cs
        case _                       => Vector.empty
      Clause(
        ClauseTopic.Reporting,
        Vector(
          number(f),
          w(
            (if cells.size == 1 then " participant-group cell"
             else " participant-group cells") +
              says
          )
        ) ++ listed(cells.map(cell(f, _))) :+ w(".")
      )
    }

  /** "Results were reported by retrieval response; n = 24 Remembered and 23
    * Forgotten; the paired contrast held 24. Per participant, groups held
    * 2–17 queries; groups with fewer than 3 queries were left out. 1
    * participant-group cell had fewer queries than the minimum (P05 ·
    * Forgotten · 1 query)."
    */
  def reporting: Vector[Clause] =
    val groups = facts.facts.collect {
      case f @ eyes4s.plan.Fact(FactSlot.GroupN(level), _, _) =>
        Vector(number(f), w(" "), name(f, level))
    }
    val head = Vector(
      at(FactSlot.ReportingSpec).map(r => Vector(w("results were reported by "), r)),
      Option.when(groups.nonEmpty)(Vector(w("n = ")) ++ joined(groups)),
      at(FactSlot.PairedN).map(n => Vector(w("the paired contrast held "), n))
    ).flatten
    val main = Option.when(head.nonEmpty)(
      Clause(
        ClauseTopic.Reporting,
        capitalised(head.head ++ head.tail.flatMap(p => w("; ") +: p)) :+ w(".")
      )
    )
    val minimum = at(FactSlot.MinimumQueries).map(m =>
      Vector(
        w("participant-group cells with fewer than "),
        m,
        w(noun(FactSlot.MinimumQueries, " query were left out", " queries were left out"))
      )
    )
    val sizes = at(FactSlot.GroupSizeRange) match
      case Some(r) =>
        Some(
          Vector(
            w("Per participant, groups held "),
            r,
            w(noun(FactSlot.GroupSizeRange, " query", " queries"))
          ) ++ minimum.toVector.flatMap(w("; ") +: _) :+ w(".")
        )
      case None => minimum.map(m => capitalised(m) :+ w("."))
    val below = breakdown(FactSlot.BelowMinimumQueries, " had fewer queries than the minimum")
    val smallest = breakdown(FactSlot.SmallestGroups, " had the fewest queries")
    main.toVector ++ sizes.map(Clause(ClauseTopic.Reporting, _)).toVector ++ below.toVector ++
      smallest.toVector

private object FactText:
  /** The value a fact token shows for the whole fact. */
  def valueOf(f: eyes4s.plan.Fact): FactValue = f.value match
    case FactValue.Controls(cs) =>
      val n = cs.map(_.controls.toLong)
      FactValue.Range(n.minOption.getOrElse(0L), n.maxOption.getOrElse(0L))
    case FactValue.Breakdown(cells) => FactValue.Count(cells.size.toLong)
    case v                          => v

  /** The default English of a value. */
  def show(value: FactValue): String = value match
    case FactValue.Count(n)    => count(n)
    case FactValue.Range(a, b) => range(a, b)
    case FactValue.Share(f, _) => s"${Display.decimals(f * 100, 1)}%"
    case FactValue.Label(text) => text
    case FactValue.OffScreen(OffScreenPolicy.ExcludeRecord) =>
      "were admitted but left out of every map"
    case FactValue.OffScreen(OffScreenPolicy.QuarantineTrial) => "quarantined their trial"
    case FactValue.Controls(_) | FactValue.Breakdown(_)       => ""

  /** The tokens with their first word capitalised, to open a sentence. */
  def capitalised(tokens: Vector[Token]): Vector[Token] = tokens match
    case Token.Words(w) +: rest => Token.Words(w.capitalize) +: rest
    case other                  => other

  /** "11,520": digits grouped in threes. */
  def count(n: Long): String =
    val digits  = math.abs(n).toString
    val grouped = digits.reverse.grouped(3).mkString(",").reverse
    if n < 0 then "−" + grouped else grouped

  /** "18–19", or "19" when the range is one value. */
  def range(a: Long, b: Long): String = if a == b then count(a) else s"${count(a)}–${count(b)}"
