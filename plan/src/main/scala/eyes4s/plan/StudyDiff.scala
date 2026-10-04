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

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.surface.EdgePolicy

/** The declared fields of a study plan a structural diff compares, in the
  * order a review panel lists them. A diff has at most one change per field,
  * so this order is total over a diff.
  */
enum StudyField derives CanEqual:
  case Input, Layout, Method, Phases, Weighting, FailurePolicy, Grid, Window, OffWindow, Scales,
    AngularScale, MatchedReferences, ControlReferences, UnmatchedFocal, InitialFixations

  /** The field's name in a review panel's sentence. */
  def label: String = this match
    case Input             => "input"
    case Layout            => "layout"
    case Method            => "method"
    case Phases            => "phases"
    case Weighting         => "weight"
    case FailurePolicy     => "failure policy"
    case Grid              => "grid"
    case Window            => "window"
    case OffWindow         => "off-window policy"
    case Scales            => "scales"
    case AngularScale      => "units per degree"
    case MatchedReferences => "matched references policy"
    case ControlReferences => "control references"
    case UnmatchedFocal    => "queries without a matched reference"
    case InitialFixations  => "initial fixations"

/** One field of a study plan that differs between two plans, with its typed
  * value before and after. `inverse` exchanges them, so the diff of two plans
  * in the other direction is the inverse of every change. `render` is the
  * default English rendering ("scales +8°", "matched references policy
  * RequireOne → SameOccurrence"); an application localises from the field
  * and the typed values.
  */
sealed trait StudyChange[K, U <: Unit2D, P, S, D]:
  def field: StudyField
  def inverse: StudyChange[K, U, P, S, D]
  def render(using UnitLabel[U]): String

  /** The default rendering of the value the change starts from. */
  def stated(using UnitLabel[U]): String

object StudyChange:
  import StudyRender.*

  given [K, U <: Unit2D, P, S, D]
      : CanEqual[StudyChange[K, U, P, S, D], StudyChange[K, U, P, S, D]] =
    CanEqual.derived

  /** Changes in [[StudyField]] order. */
  given ordering[K, U <: Unit2D, P, S, D]: Ordering[StudyChange[K, U, P, S, D]] =
    Ordering.by(_.field.ordinal)

  /** The study input, compared by the content digest of its artifact reference. */
  final case class Input[K, U <: Unit2D, P, S, D](
      before: ArtifactRef[StudyInput[K, U]],
      after: ArtifactRef[StudyInput[K, U]]
  ) extends StudyChange[K, U, P, S, D]:
    def field: StudyField                  = StudyField.Input
    def stated(using UnitLabel[U]): String = before.digest
    def inverse: Input[K, U, P, S, D]      = Input(after, before)
    def render(using UnitLabel[U]): String = s"input ${before.digest} → ${after.digest}"

  /** Layouts are compared by their definition identity. */
  final case class Layout[K, U <: Unit2D, P, S, D](
      before: StudyLayout[K],
      after: StudyLayout[K]
  ) extends StudyChange[K, U, P, S, D]:
    def field: StudyField                  = StudyField.Layout
    def stated(using UnitLabel[U]): String = definition(before.id)
    def inverse: Layout[K, U, P, S, D]     = Layout(after, before)
    def render(using UnitLabel[U]): String =
      s"layout ${definition(before.id)} → ${definition(after.id)}"

  /** The comparison method and its parameters, compared by the method's
    * identity and the parameters it describes.
    */
  final case class Method[K, U <: Unit2D, P, S, D](
      before: StudyMethod[P, U, S, D],
      beforeParameters: P,
      after: StudyMethod[P, U, S, D],
      afterParameters: P
  ) extends StudyChange[K, U, P, S, D]:
    def field: StudyField                  = StudyField.Method
    def stated(using UnitLabel[U]): String =
      s"${definition(before.id)} ${parameters(before.parameters(beforeParameters))}"
    def inverse: Method[K, U, P, S, D] =
      Method(after, afterParameters, before, beforeParameters)
    def render(using UnitLabel[U]): String =
      if before.id != after.id then s"method ${definition(before.id)} → ${definition(after.id)}"
      else
        s"method parameters ${parameters(before.parameters(beforeParameters))} → " +
          parameters(after.parameters(afterParameters))

  /** The focal and reference phase names; a change to either records both pairs. */
  final case class Phases[K, U <: Unit2D, P, S, D](
      beforeFocal: String,
      beforeReference: String,
      afterFocal: String,
      afterReference: String
  ) extends StudyChange[K, U, P, S, D]:
    def field: StudyField                  = StudyField.Phases
    def stated(using UnitLabel[U]): String = s"$beforeFocal vs $beforeReference"
    def inverse: Phases[K, U, P, S, D]     =
      Phases(afterFocal, afterReference, beforeFocal, beforeReference)
    def render(using UnitLabel[U]): String =
      s"phases $beforeFocal vs $beforeReference → $afterFocal vs $afterReference"

  /** How fixations weight a density, such as by duration. */
  final case class Weighting[K, U <: Unit2D, P, S, D](before: Weight, after: Weight)
      extends StudyChange[K, U, P, S, D]:
    def field: StudyField                  = StudyField.Weighting
    def stated(using UnitLabel[U]): String = before.toString
    def inverse: Weighting[K, U, P, S, D]  = Weighting(after, before)
    def render(using UnitLabel[U]): String = s"weight $before → $after"

  /** The failure policy the plan's reductions apply. */
  final case class Failures[K, U <: Unit2D, P, S, D](
      before: FailurePolicy,
      after: FailurePolicy
  ) extends StudyChange[K, U, P, S, D]:
    def field: StudyField                  = StudyField.FailurePolicy
    def stated(using UnitLabel[U]): String = before.render
    def inverse: Failures[K, U, P, S, D]   = Failures(after, before)
    def render(using UnitLabel[U]): String =
      s"failure policy ${before.render} → ${after.render}"

  /** The grid every density lies on (on the window's frame when windowed). */
  final case class Grid[K, U <: Unit2D, P, S, D](
      before: eyes4s.kernel.Grid[U],
      after: eyes4s.kernel.Grid[U]
  ) extends StudyChange[K, U, P, S, D]:
    def field: StudyField                  = StudyField.Grid
    def stated(using UnitLabel[U]): String = grid(before)
    def inverse: Grid[K, U, P, S, D]       = Grid(after, before)
    def render(using UnitLabel[U]): String = s"grid ${grid(before)} → ${grid(after)}"

  /** The analysis window; `None` is the whole admission frame. */
  final case class Window[K, U <: Unit2D, P, S, D](
      before: Option[Subframe[U]],
      after: Option[Subframe[U]]
  ) extends StudyChange[K, U, P, S, D]:
    def field: StudyField                  = StudyField.Window
    def stated(using UnitLabel[U]): String = before.fold("the whole frame")(window(_))
    def inverse: Window[K, U, P, S, D]     = Window(after, before)
    def render(using UnitLabel[U]): String = (before, after) match
      case (None, Some(w))    => s"window added: ${window(w)}"
      case (Some(w), None)    => s"window removed: ${window(w)}; the whole frame is mapped"
      case (Some(b), Some(a)) => s"window changed: ${window(b)} → ${window(a)}"
      case (None, None)       => "window unchanged"

  /** The off-window policy; `None` for a whole-frame plan, which has none. */
  final case class OffWindow[K, U <: Unit2D, P, S, D](
      before: Option[OffWindowPolicy],
      after: Option[OffWindowPolicy]
  ) extends StudyChange[K, U, P, S, D]:
    def field: StudyField                  = StudyField.OffWindow
    def stated(using UnitLabel[U]): String = before.fold("none")(_.toString)
    def inverse: OffWindow[K, U, P, S, D]  = OffWindow(after, before)
    def render(using UnitLabel[U]): String =
      s"off-window policy ${before.fold("none")(_.toString)} → ${after.fold("none")(_.toString)}"

  /** The declared scales, in order. `added` and `removed` compare them as
    * sets; a change with neither reorders them.
    */
  final case class Scales[K, U <: Unit2D, P, S, D](
      before: Vector[StudyScale[U]],
      after: Vector[StudyScale[U]]
  ) extends StudyChange[K, U, P, S, D]:
    def field: StudyField                  = StudyField.Scales
    def stated(using UnitLabel[U]): String = before.map(scale(_)).mkString(", ")
    def inverse: Scales[K, U, P, S, D]     = Scales(after, before)
    def added: Vector[StudyScale[U]]       = after.filterNot(before.contains)
    def removed: Vector[StudyScale[U]]     = before.filterNot(after.contains)
    def render(using UnitLabel[U]): String =
      val parts = added.map("+" + scale(_)) ++ removed.map("−" + scale(_))
      if parts.isEmpty then
        s"scales reordered: ${before.map(scale(_)).mkString(", ")} → " +
          after.map(scale(_)).mkString(", ")
      else s"scales ${parts.mkString(" ")}"

  /** The plan's linear angular scale, in `U` units per degree; `None` when it declares none. */
  final case class AngularScale[K, U <: Unit2D, P, S, D](
      before: Option[LinearAngularScale[U]],
      after: Option[LinearAngularScale[U]]
  ) extends StudyChange[K, U, P, S, D]:
    def field: StudyField                     = StudyField.AngularScale
    def stated(using u: UnitLabel[U]): String =
      before.fold("none")(v => s"${num(v.unitsPerDegree)} ${u.symbol}/°")
    def inverse: AngularScale[K, U, P, S, D]  = AngularScale(after, before)
    def render(using u: UnitLabel[U]): String =
      def show(s: Option[LinearAngularScale[U]]) =
        s.fold("none")(v => s"${num(v.unitsPerDegree)} ${u.symbol}/°")
      s"units per degree ${show(before)} → ${show(after)}"

  /** The policy that chooses each focal trial's matched references. */
  final case class Matched[K, U <: Unit2D, P, S, D](
      before: MatchedReferences,
      after: MatchedReferences
  ) extends StudyChange[K, U, P, S, D]:
    def field: StudyField                  = StudyField.MatchedReferences
    def stated(using UnitLabel[U]): String = matched(before)
    def inverse: Matched[K, U, P, S, D]    = Matched(after, before)
    def render(using UnitLabel[U]): String =
      s"matched references policy ${matched(before)} → ${matched(after)}"

  /** The policy that chooses each focal trial's control references. */
  final case class Controls[K, U <: Unit2D, P, S, D](
      before: ControlReferences,
      after: ControlReferences
  ) extends StudyChange[K, U, P, S, D]:
    def field: StudyField                  = StudyField.ControlReferences
    def stated(using UnitLabel[U]): String = before.toString
    def inverse: Controls[K, U, P, S, D]   = Controls(after, before)
    def render(using UnitLabel[U]): String = s"control references $before → $after"

  /** What the plan does with a focal trial that has no matched reference. */
  final case class Unmatched[K, U <: Unit2D, P, S, D](
      before: UnmatchedFocalPolicy,
      after: UnmatchedFocalPolicy
  ) extends StudyChange[K, U, P, S, D]:
    def field: StudyField                  = StudyField.UnmatchedFocal
    def stated(using UnitLabel[U]): String = before.toString
    def inverse: Unmatched[K, U, P, S, D]  = Unmatched(after, before)
    def render(using UnitLabel[U]): String =
      s"queries without a matched reference $before → $after"

  /** Which leading fixations of each trial are kept: all, all but the first, or those
    * outside a disc around the fixation cross.
    */
  final case class InitialFixations[K, U <: Unit2D, P, S, D](
      before: InitialFixationPolicy[U],
      after: InitialFixationPolicy[U]
  ) extends StudyChange[K, U, P, S, D]:
    def field: StudyField                        = StudyField.InitialFixations
    def stated(using UnitLabel[U]): String       = before.render
    def inverse: InitialFixations[K, U, P, S, D] = InitialFixations(after, before)
    def render(using UnitLabel[U]): String       =
      s"initial fixations ${before.render} → ${after.render}"

/** Default English renderings of the typed values a change carries. */
private[plan] object StudyRender:
  def num(value: Double): String = Provenance.Param.Num(value).render

  def definition(id: DefinitionId): String = s"${id.name}@${id.version}"

  def parameters(values: Vector[(String, Provenance.Param)]): String =
    if values.isEmpty then "none"
    else values.map((k, v) => s"$k=${v.render}").mkString("(", ", ", ")")

  def grid[U <: Unit2D](g: eyes4s.kernel.Grid[U]): String = s"${g.nx}×${g.ny} (${g.id.name})"

  def window[U <: Unit2D](w: Subframe[U])(using u: UnitLabel[U]): String =
    val r = w.region
    s"${w.frame.id.name} [${num(r.xMin)}, ${num(r.xMax)}) × [${num(r.yMin)}, ${num(r.yMax)}) ${u.symbol}"

  private def edges(e: EdgePolicy): String = e match
    case EdgePolicy.Truncate => ""
    case other               => s" ($other)"

  private def estimate[V <: Unit2D](e: StudyEstimate[V], unit: String): String = e match
    case StudyEstimate.Binned()                => "binned"
    case StudyEstimate.Gaussian(sigma, edge)   => s"${num(sigma.value)}$unit${edges(edge)}"
    case StudyEstimate.Anisotropic(x, y, edge) =>
      s"${num(x.value)}$unit×${num(y.value)}$unit${edges(edge)}"

  def scale[U <: Unit2D](s: StudyScale[U])(using u: UnitLabel[U]): String = s match
    case StudyScale.Native(e)  => estimate(e, s" ${u.symbol}")
    case StudyScale.Angular(e) => estimate(e, "°")

  def matched(m: MatchedReferences): String = m match
    case MatchedReferences.Select(OccurrenceChoice.First) => "Select(first)"
    case MatchedReferences.Select(OccurrenceChoice.Last)  => "Select(last)"
    case MatchedReferences.Select(OccurrenceChoice.At(n)) => s"Select(occurrence ${n.value})"
    case other                                            => other.toString

/** Refusals while applying structural changes to a plan, each naming the
  * field and the values that disagree.
  */
enum StudyRevisionError derives CanEqual:
  /** Two changes name the same field. */
  case DuplicateField(field: StudyField)

  /** A change starts from `stated`, but the plan's value of the field is
    * `current` (both in their default rendering).
    */
  case Stale(field: StudyField, stated: String, current: String)

  /** A window without an off-window policy, or a policy without a window. */
  case IncompleteWindow(window: Option[String], offWindow: Option[OffWindowPolicy])

  /** The revised plan is refused as a plan. */
  case Plan(underlying: PlanError)

  def message: String = this match
    case DuplicateField(field) =>
      s"Two changes name the ${field.label}; a revision changes each field once."
    case Stale(field, stated, current) =>
      s"The change of ${field.label} starts from $stated, but the plan has $current."
    case IncompleteWindow(window, offWindow) =>
      s"A windowed plan needs both a window and an off-window policy; the revision gives " +
        s"window ${window.getOrElse("none")} and off-window policy ${offWindow.fold("none")(_.toString)}."
    case Plan(e) => e.message

object StudyRevisionError:
  given diagnose: Diagnose[StudyRevisionError, Nothing] =
    Diagnose.instance(DiagnosticCatalog.studyRevision)(RevisionDiagnostics.studyRevision)

/** Structural differences between study plans. */
object StudyDiff:
  private def window[U <: Unit2D](g: StudyGeometry[U]): Option[Subframe[U]] = g match
    case StudyGeometry.WholeFrame(_)     => None
    case StudyGeometry.Windowed(w, _, _) => Some(w)
  private def offWindow[U <: Unit2D](g: StudyGeometry[U]): Option[OffWindowPolicy] = g match
    case StudyGeometry.WholeFrame(_)     => None
    case StudyGeometry.Windowed(_, _, p) => Some(p)

  private def sameMethod[P, U <: Unit2D, S, D](
      a: StudyMethod[P, U, S, D],
      ap: P,
      b: StudyMethod[P, U, S, D],
      bp: P
  ): Boolean = a.id == b.id && a.parameters(ap) == b.parameters(bp)

  /** Every declared field that differs from `before` to `after`, in
    * [[StudyField]] order. Layouts and methods are compared by identity (and
    * a method's described parameters); every other field by its typed value.
    * The diff of a plan with itself is empty, and `between(b, a)` is
    * `between(a, b).map(_.inverse)`.
    */
  def between[K, U <: Unit2D, P, S, D](
      before: StudyPlan[K, U, P, S, D],
      after: StudyPlan[K, U, P, S, D]
  ): Vector[StudyChange[K, U, P, S, D]] =
    import StudyChange as C
    Vector[Option[StudyChange[K, U, P, S, D]]](
      Option.when(before.input != after.input)(C.Input(before.input, after.input)),
      Option.when(before.layout.id != after.layout.id)(C.Layout(before.layout, after.layout)),
      Option.unless(
        sameMethod(before.method, before.parameters, after.method, after.parameters)
      )(C.Method(before.method, before.parameters, after.method, after.parameters)),
      Option.when(
        before.focalPhase != after.focalPhase || before.referencePhase != after.referencePhase
      )(
        C.Phases(
          before.focalPhase,
          before.referencePhase,
          after.focalPhase,
          after.referencePhase
        )
      ),
      Option.when(before.weight != after.weight)(C.Weighting(before.weight, after.weight)),
      Option.when(before.policy != after.policy)(C.Failures(before.policy, after.policy)),
      Option.when(before.grid != after.grid)(C.Grid(before.grid, after.grid)),
      Option.when(window(before.geometry) != window(after.geometry))(
        C.Window(window(before.geometry), window(after.geometry))
      ),
      Option.when(offWindow(before.geometry) != offWindow(after.geometry))(
        C.OffWindow(offWindow(before.geometry), offWindow(after.geometry))
      ),
      Option.when(before.scales != after.scales)(C.Scales(before.scales, after.scales)),
      Option.when(before.angularScale != after.angularScale)(
        C.AngularScale(before.angularScale, after.angularScale)
      ),
      Option.when(before.pairing.matched != after.pairing.matched)(
        C.Matched(before.pairing.matched, after.pairing.matched)
      ),
      Option.when(before.pairing.controls != after.pairing.controls)(
        C.Controls(before.pairing.controls, after.pairing.controls)
      ),
      Option.when(before.pairing.unmatched != after.pairing.unmatched)(
        C.Unmatched(before.pairing.unmatched, after.pairing.unmatched)
      ),
      Option.when(before.initialFixations != after.initialFixations)(
        C.InitialFixations(before.initialFixations, after.initialFixations)
      )
    ).flatten

  /** One line a host can show: "scales +8°; window changed: …", or "no
    * changes".
    */
  def render[K, U <: Unit2D, P, S, D](changes: Vector[StudyChange[K, U, P, S, D]])(using
      UnitLabel[U]
  ): String =
    if changes.isEmpty then "no changes" else changes.sorted.map(_.render).mkString("; ")

  /** Apply `changes` to `plan`: every change's `before` must be the plan's
    * current value of its field, each field changes at most once, and the
    * revised plan is checked like any configured plan. Applying
    * `between(a, b)` to `a` gives a plan equal to `b`.
    */
  def revise[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      changes: Vector[StudyChange[K, U, P, S, D]]
  )(using unit: UnitLabel[U]): Either[StudyRevisionError, StudyPlan[K, U, P, S, D]] =
    import StudyChange as C
    import StudyRevisionError.*
    case class Draft(
        input: ArtifactRef[StudyInput[K, U]],
        layout: StudyLayout[K],
        method: StudyMethod[P, U, S, D],
        parameters: P,
        focal: String,
        reference: String,
        weight: Weight,
        policy: FailurePolicy,
        grid: eyes4s.kernel.Grid[U],
        window: Option[Subframe[U]],
        offWindow: Option[OffWindowPolicy],
        scales: Vector[StudyScale[U]],
        angular: Option[LinearAngularScale[U]],
        pairing: StudyPairing,
        initial: InitialFixationPolicy[U]
    )
    val start = Draft(
      plan.input,
      plan.layout,
      plan.method,
      plan.parameters,
      plan.focalPhase,
      plan.referencePhase,
      plan.weight,
      plan.policy,
      plan.grid,
      window(plan.geometry),
      offWindow(plan.geometry),
      plan.scales,
      plan.angularScale,
      plan.pairing,
      plan.initialFixations
    )
    def stale(change: StudyChange[K, U, P, S, D], current: StudyChange[K, U, P, S, D]) =
      Left(Stale(change.field, change.stated, current.stated))
    // Each check compares the change's `before` with the plan's value, shown
    // through a change from the plan's value to the change's `after`.
    def step(
        draft: Draft,
        change: StudyChange[K, U, P, S, D]
    ): Either[StudyRevisionError, Draft] = change match
      case c @ C.Input(b, a) =>
        if b == plan.input then Right(draft.copy(input = a))
        else stale(c, C.Input(plan.input, a))
      case c @ C.Layout(b, a) =>
        if b.id == plan.layout.id then Right(draft.copy(layout = a))
        else stale(c, C.Layout(plan.layout, a))
      case c @ C.Method(b, bp, a, ap) =>
        if sameMethod(b, bp, plan.method, plan.parameters) then
          Right(draft.copy(method = a, parameters = ap))
        else stale(c, C.Method(plan.method, plan.parameters, a, ap))
      case c @ C.Phases(bf, br, af, ar) =>
        if bf == plan.focalPhase && br == plan.referencePhase then
          Right(draft.copy(focal = af, reference = ar))
        else stale(c, C.Phases(plan.focalPhase, plan.referencePhase, af, ar))
      case c @ C.Weighting(b, a) =>
        if b == plan.weight then Right(draft.copy(weight = a))
        else stale(c, C.Weighting(plan.weight, a))
      case c @ C.Failures(b, a) =>
        if b == plan.policy then Right(draft.copy(policy = a))
        else stale(c, C.Failures(plan.policy, a))
      case c @ C.Grid(b, a) =>
        if b == plan.grid then Right(draft.copy(grid = a)) else stale(c, C.Grid(plan.grid, a))
      case c @ C.Window(b, a) =>
        if b == window(plan.geometry) then Right(draft.copy(window = a))
        else stale(c, C.Window(window(plan.geometry), a))
      case c @ C.OffWindow(b, a) =>
        if b == offWindow(plan.geometry) then Right(draft.copy(offWindow = a))
        else stale(c, C.OffWindow(offWindow(plan.geometry), a))
      case c @ C.Scales(b, a) =>
        if b == plan.scales then Right(draft.copy(scales = a))
        else stale(c, C.Scales(plan.scales, a))
      case c @ C.AngularScale(b, a) =>
        if b == plan.angularScale then Right(draft.copy(angular = a))
        else stale(c, C.AngularScale(plan.angularScale, a))
      case c @ C.Matched(b, a) =>
        if b == plan.pairing.matched then
          Right(draft.copy(pairing = draft.pairing.copy(matched = a)))
        else stale(c, C.Matched(plan.pairing.matched, a))
      case c @ C.Controls(b, a) =>
        if b == plan.pairing.controls then
          Right(draft.copy(pairing = draft.pairing.copy(controls = a)))
        else stale(c, C.Controls(plan.pairing.controls, a))
      case c @ C.Unmatched(b, a) =>
        if b == plan.pairing.unmatched then
          Right(draft.copy(pairing = draft.pairing.copy(unmatched = a)))
        else stale(c, C.Unmatched(plan.pairing.unmatched, a))
      case c @ C.InitialFixations(b, a) =>
        if b == plan.initialFixations then Right(draft.copy(initial = a))
        else stale(c, C.InitialFixations(plan.initialFixations, a))

    val fields     = changes.map(_.field)
    val duplicated = fields.diff(fields.distinct).headOption
    for
      _     <- duplicated.map(DuplicateField(_)).toLeft(())
      draft <- changes.foldLeft[Either[StudyRevisionError, Draft]](Right(start))((acc, c) =>
        acc.flatMap(step(_, c))
      )
      geometry <- (draft.window, draft.offWindow) match
        case (None, None)         => Right(StudyGeometry.WholeFrame(draft.grid))
        case (Some(w), Some(off)) =>
          StudyGeometry
            .windowed(w, draft.grid, off)
            .left
            .map(e => Plan(PlanError.Geometry(e)))
        case (w, off) => Left(IncompleteWindow(w.map(_.frame.id.name), off))
      revised <- StudyPlan
        .configure(
          draft.input,
          draft.layout,
          geometry,
          draft.focal,
          draft.reference,
          draft.weight,
          draft.scales,
          draft.angular,
          draft.policy,
          draft.method,
          draft.parameters,
          draft.pairing,
          draft.initial
        )
        .left
        .map(Plan(_))
    yield revised
