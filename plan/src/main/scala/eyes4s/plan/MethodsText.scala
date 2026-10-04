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

/** One piece of a methods sentence: fixed English words, a value drawn from a
  * form field with its role, or a fact from beyond the plan
  * ([[MethodsFacts]]). A fact token carries the fact itself, so its slot and
  * its one source are the fact's own; a fact may be shown in several tokens
  * (a range and its exceptions).
  */
enum Token derives CanEqual:
  case Words(words: String)
  case Value(field: FieldId, role: TokenRole, shown: String)
  case Fact(fact: eyes4s.plan.Fact, shown: String)

  def text: String = this match
    case Words(w)       => w
    case Value(_, _, s) => s
    case Fact(_, s)     => s

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
    // Method-determined words: what M, B and D are, and what D measures.
    val contrast = Clause(
      ClauseTopic.Contrast,
      Vector(
        w(
          "For each query, M was its matched score and B the mean of its control scores, " +
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
      known.admission.toVector ++ Vector(comparison, unmatched) ++ known.design ++
        Vector(mapped) ++ known.outsideScreen ++ declared ++
        Vector(initialClause, gridClause, scaleClause, metric, contrast, failures) ++
        known.reporting
    )

/** The clauses and tokens a methods text states from its facts. */
private final class FactText(facts: MethodsFacts):
  import Token.*
  private def w(s: String) = Words(s)

  private def shown(fact: eyes4s.plan.Fact): String = fact.value match
    case FactValue.Count(n)         => FactText.count(n)
    case FactValue.Range(a, b)      => FactText.range(a, b)
    case FactValue.Controls(r, _)   => FactText.range(r.min, r.max)
    case FactValue.Breakdown(cells) => FactText.count(cells.size.toLong)
    case FactValue.Label(text)      => text

  private def token(fact: eyes4s.plan.Fact): Token = Fact(fact, shown(fact))
  private def at(slot: FactSlot): Option[Token]    = facts.get(slot).map(token)

  /** `parts` joined into one list: "a, b and c". */
  private def joined(parts: Vector[Vector[Token]]): Vector[Token] =
    parts.zipWithIndex.flatMap { (p, i) =>
      val sep =
        if i == 0 then Vector.empty
        else if i == parts.size - 1 then Vector(w(" and "))
        else Vector(w(", "))
      sep ++ p
    }

  private def causes: Vector[eyes4s.plan.Fact] =
    facts.facts
      .filter(_.slot match
        case FactSlot.QuarantineCause(_) => true
        case _                           => false)
      .sortBy(f =>
        (
          f.value match
            case FactValue.Count(n) => -n
            case _                  => 0L
          ,
          f.slot.toString
        )
      )

  /** "Fixations (11,520 records) from dataset r3 were admitted by trial: 937
    * of 960 inventory trials were admitted, 17 quarantined (overlap 6, …) and
    * 6 absent (no fixation records)."
    */
  def admission: Option[Clause] =
    val opening = (at(FactSlot.FixationRecords), at(FactSlot.DatasetRevision)) match
      case (None, None)       => Vector.empty
      case (records, dataset) =>
        Vector(w("Fixations")) ++
          records.toVector.flatMap(r => Vector(w(" ("), r, w(" records)"))) ++
          dataset.toVector.flatMap(d => Vector(w(" from dataset "), d)) ++
          Vector(w(" were admitted by trial"))
    val quarantinedCauses = causes.map { c =>
      val name = c.slot match
        case FactSlot.QuarantineCause(cause) => cause.label
        case _                               => ""
      Vector(Fact(c, s"$name ${shown(c)}"))
    }
    val causeList = Option
      .when(quarantinedCauses.nonEmpty)(
        Vector(w(" (")) ++ quarantinedCauses.zipWithIndex.flatMap((c, i) =>
          (if i == 0 then Vector.empty else Vector(w(", "))) ++ c
        ) ++ Vector(w(")"))
      )
      .toVector
      .flatten
    val admitted  = at(FactSlot.Admitted)
    val inventory = at(FactSlot.InventoryTrials)
    val counts    = Vector(
      admitted.map(a =>
        Vector(a) ++ inventory.toVector.flatMap(i =>
          Vector(w(" of "), i, w(" inventory trials"))
        ) ++ Vector(w(" were admitted"))
      ),
      at(FactSlot.Quarantined) match
        case Some(q) => Some(Vector(q, w(" quarantined")) ++ causeList)
        case None    =>
          Option.when(causeList.nonEmpty)(Vector(w("some were quarantined")) ++ causeList)
      ,
      at(FactSlot.Absent).map(a => Vector(a, w(" absent (no fixation records)")))
    ).flatten
    // Without an admitted count, the inventory leads the list: "of 960 inventory trials, …".
    val lead = if admitted.isDefined then None else inventory
    Option.when(opening.nonEmpty || counts.nonEmpty || lead.nonEmpty) {
      val body = (opening.nonEmpty, counts.nonEmpty, lead) match
        case (_, true, Some(i)) =>
          val of = Vector(i, w(" inventory trials, ")) ++ joined(counts)
          if opening.isEmpty then w("Of ") +: of else opening ++ (w(": of ") +: of)
        case (false, true, None) => Vector(w("Of the inventory trials, ")) ++ joined(counts)
        case (true, true, None)  => opening ++ Vector(w(": ")) ++ joined(counts)
        case (_, false, Some(i)) =>
          if opening.isEmpty then Vector(w("The inventory held "), i, w(" trials"))
          else opening ++ Vector(w(" from "), i, w(" inventory trials"))
        case (_, false, None) => opening
      Clause(ClauseTopic.Admission, body :+ w("."))
    }

  /** "Of 480 requested queries, 457 were eligible; 9 had no matched reference
    * and 14 were not admitted. Each eligible query had 18–19 controls (171
    * queries had 18: overlap)."
    */
  def design: Vector[Clause] =
    val eligibility =
      val left = Vector(
        at(FactSlot.UnmatchedQueries).map(u => Vector(u, w(" had no matched reference"))),
        at(FactSlot.NotAdmittedQueries).map(n => Vector(n, w(" were not admitted")))
      ).flatten
      (at(FactSlot.RequestedQueries), at(FactSlot.EligibleQueries)) match
        case (None, None) =>
          Option.when(left.nonEmpty)(
            Clause(
              ClauseTopic.Design,
              Vector(w("Of the requested queries, ")) ++ joined(left) :+ w(".")
            )
          )
        case (requested, eligible) =>
          val head = (requested, eligible) match
            case (Some(r), Some(e)) =>
              Vector(w("Of "), r, w(" requested queries, "), e, w(" were eligible"))
            case (Some(r), None) => Vector(w("The design requested "), r, w(" queries"))
            case (None, Some(e)) => Vector(e, w(" queries were eligible"))
            case (None, None)    => Vector.empty
          Some(
            Clause(
              ClauseTopic.Design,
              head ++ (if left.isEmpty then Vector.empty
                       else Vector(w("; ")) ++ joined(left)) :+
                w(".")
            )
          )
    val controls = facts.get(FactSlot.ControlsPerQuery).map { f =>
      val fewer = f.value match
        case FactValue.Controls(_, fs) => fs
        case _                         => Vector.empty
      Clause(
        ClauseTopic.Design,
        Vector(w("Each eligible query had "), token(f), w(" controls")) ++
          Option
            .when(fewer.nonEmpty)(
              Vector(w(" (")) ++ fewer.zipWithIndex.flatMap { (x, i) =>
                (if i == 0 then Vector.empty else Vector(w("; "))) :+
                  Fact(
                    f,
                    s"${FactText.count(x.queries.toLong)} queries had " +
                      s"${FactText.count(x.controls.toLong)}: ${x.cause.label}"
                  )
              } ++ Vector(w(")"))
            )
            .toVector
            .flatten :+ w(".")
      )
    }
    eligibility.toVector ++ controls.toVector

  /** " 543 records in 409 trials fell outside it.", after the sentence
    * naming the mapped region.
    */
  def outsideWindow: Vector[Token] =
    val trials = at(FactSlot.TrialsOutsideWindow).toVector
    at(FactSlot.RecordsOutsideWindow) match
      case Some(r) =>
        Vector(w(" "), r, w(" records")) ++
          trials.flatMap(t => Vector(w(" in "), t, w(" trials"))) ++
          Vector(w(" fell outside it."))
      case None =>
        trials.flatMap(t => Vector(w(" Records in "), t, w(" trials fell outside it.")))

  def outsideScreen: Vector[Clause] =
    at(FactSlot.RecordsOutsideScreen).toVector.map(r =>
      Clause(
        ClauseTopic.Admission,
        Vector(r, w(" records outside the screen were left out of admission."))
      )
    )

  /** "Results were reported by retrieval response; groups held 2–17
    * participants and the paired contrast 24. 3 participant-group cells had
    * fewer queries than the minimum (P05 · Forgotten · 1, …)."
    */
  def reporting: Vector[Clause] =
    val head = Vector(
      at(FactSlot.ReportingSpec).map(r => Vector(w("Results were reported by "), r)),
      at(FactSlot.GroupSizeRange).map(g => Vector(w("groups held "), g, w(" participants"))),
      at(FactSlot.PairedN).map(n => Vector(w("the paired contrast "), n))
    ).flatten
    val main = Option.when(head.nonEmpty)(
      Clause(
        ClauseTopic.Reporting,
        FactText.capitalised(head.head) ++ head.tail.zipWithIndex.flatMap((p, i) =>
          (if i == 0 then Vector(w("; ")) else Vector(w(" and "))) ++ p
        ) :+ w(".")
      )
    )
    val below = facts.get(FactSlot.BelowMinimumQueries).map { f =>
      val cells = f.value match
        case FactValue.Breakdown(cs) => cs
        case _                       => Vector.empty
      Clause(
        ClauseTopic.Reporting,
        Vector(token(f), w(" participant-group cells had fewer queries than the minimum")) ++
          Option
            .when(cells.nonEmpty)(
              Vector(w(" (")) ++ cells.zipWithIndex.flatMap { (c, i) =>
                (if i == 0 then Vector.empty else Vector(w(", "))) :+
                  Fact(f, s"${c.participant} · ${c.group} · ${c.queries}")
              } ++ Vector(w(")"))
            )
            .toVector
            .flatten :+ w(".")
      )
    }
    main.toVector ++ below.toVector

private object FactText:
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
