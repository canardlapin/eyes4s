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

/** One piece of a methods sentence: fixed English words, or a value drawn
  * from a form field, with its role.
  */
enum Token derives CanEqual:
  case Words(words: String)
  case Value(field: FieldId, role: TokenRole, shown: String)

  def text: String = this match
    case Words(w)       => w
    case Value(_, _, s) => s

/** A sentence about one form field. */
final case class Clause(field: FieldId, tokens: Vector[Token]) derives CanEqual:
  def text: String = tokens.map(_.text).mkString

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
    * declares, in reading order.
    */
  def methods[K, U <: Unit2D, P, S, D](plan: StudyPlan[K, U, P, S, D])(using
      u: UnitLabel[U]
  ): Phrase =
    val unit       = u.planar
    val facts      = StudyAdvice.facts(plan)
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
          )
        )
      case other =>
        Clause(
          window,
          Vector(
            w("Maps covered the whole admission frame "),
            Value(window, TokenRole.Policy, region(other.admission.bounds)),
            w(".")
          )
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
        Value(grid, TokenRole.Scale, pair(facts.cell, unit))
      ) ++ facts.cellDegrees.toVector.flatMap(d =>
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
    val failures = Clause(
      failure,
      plan.policy match
        case FailurePolicy.RequireAll =>
          Vector(
            Value(failure, TokenRole.Policy, "Every selected pair had to be scored"),
            w(".")
          )
        case FailurePolicy.SuccessfulOnly(m) =>
          Vector(
            w("Reductions used successful scores only, requiring at least "),
            Value(failure, TokenRole.Policy, s"${m.value} successful scores"),
            w(".")
          )
    )
    Phrase(
      Vector(comparison, unmatched, mapped) ++ declared ++
        Vector(initialClause, gridClause, scaleClause, metric, failures)
    )
