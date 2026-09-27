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

import cats.syntax.all.*
import eyes4s.core.Weight
import eyes4s.design.FailurePolicy
import eyes4s.kernel.*
import eyes4s.surface.EdgePolicy

/** The focal (query) and reference phases of a study: non-empty and
  * different.
  */
final case class StudyPhases private (focal: String, reference: String) derives CanEqual

object StudyPhases:
  /** The phases of a configured plan, which `StudyPlan.configure` checked. */
  def of[K, U <: Unit2D, P, S, D](plan: StudyPlan[K, U, P, S, D]): StudyPhases =
    new StudyPhases(plan.focalPhase, plan.referencePhase)

  def of(focal: String, reference: String): Either[PlanError, StudyPhases] =
    Either.cond(
      focal.trim.nonEmpty && reference.trim.nonEmpty && focal != reference,
      new StudyPhases(focal, reference),
      PlanError.InvalidPhases(focal, reference)
    )

/** A grid's columns and rows: positive, with a cell count an `Int` holds, the
  * same rule as `Grid.of`.
  */
final case class GridCells private (columns: Int, rows: Int) derives CanEqual

object GridCells:
  def of(columns: Int, rows: Int): Either[GeometryError, GridCells] =
    if columns <= 0 || rows <= 0 then Left(GeometryError.DegenerateGrid(columns, rows))
    else if columns.toLong * rows.toLong > Int.MaxValue then
      Left(GeometryError.GridCellCountOverflow(columns, rows, columns.toLong * rows.toLong))
    else Right(new GridCells(columns, rows))

  def of[U <: Unit2D](grid: Grid[U]): GridCells = new GridCells(grid.nx, grid.ny)

/** What a study form takes from outside the form: the admission frame the
  * input lies on, the grid's identity and, when the host declares one, the
  * identity of the analysis window's frame. Without it the form refuses a
  * window rather than invent a frame.
  */
final case class StudyFormContext[U <: Unit2D](
    admission: Frame[U],
    window: Option[FrameId],
    grid: GridId
) derives CanEqual

object StudyFormContext:
  /** The context a plan was built in: a windowed plan's window frame, and no
    * window frame for a whole-frame plan.
    */
  def of[K, U <: Unit2D, P, S, D](plan: StudyPlan[K, U, P, S, D]): StudyFormContext[U] =
    plan.geometry match
      case StudyGeometry.Windowed(w, g, _) => StudyFormContext(w.parent, Some(w.frame.id), g.id)
      case other => StudyFormContext(other.admission, None, other.grid.id)

/** The typed values of a study form, each parsed on its own. `plan` runs the
  * whole-recipe checks, keyed by [[StudyField]].
  */
final case class StudyRecipe[U <: Unit2D](
    context: StudyFormContext[U],
    phases: StudyPhases,
    weight: Weight,
    failurePolicy: FailurePolicy,
    grid: GridCells,
    window: Option[Subframe[U]],
    offWindow: Option[OffWindowPolicy],
    angularScale: Option[LinearAngularScale[U]],
    scales: Vector[StudyScale[U]],
    pairing: StudyPairing,
    initialFixations: InitialFixationPolicy[U]
):
  /** The plan these values describe with the given input, layout and method,
    * or the first whole-recipe refusal, keyed by the field it concerns.
    */
  def plan[K, P, S, D](
      input: ArtifactRef[StudyInput[K, U]],
      layout: StudyLayout[K],
      method: StudyMethod[P, U, S, D],
      parameters: P
  )(using UnitLabel[U]): Either[StudyRecipeError, StudyPlan[K, U, P, S, D]] =
    for
      _ <- (window, offWindow) match
        case (Some(_), None) => Left(StudyRecipeError.WindowWithoutPolicy)
        case (None, Some(p)) => Left(StudyRecipeError.PolicyWithoutWindow(p))
        case _               => Right(())
      g <- Grid
        .of(context.grid, window.fold(context.admission)(_.frame), grid.columns, grid.rows)
        .left
        .map(StudyRecipeError.Grid.apply)
      geometry <- (window, offWindow) match
        case (Some(w), Some(p)) =>
          StudyGeometry.windowed(w, g, p).left.map(StudyRecipeError.Grid.apply)
        case _ => Right(StudyGeometry.WholeFrame(g))
      plan <- StudyPlan
        .configure(
          input,
          layout,
          geometry,
          phases.focal,
          phases.reference,
          weight,
          scales,
          angularScale,
          failurePolicy,
          method,
          parameters,
          pairing,
          initialFixations
        )
        .left
        .map(StudyRecipeError.Plan.apply)
    yield plan

/** A whole-recipe refusal of a study form, keyed by the [[StudyField]] it
  * concerns.
  */
enum StudyRecipeError derives CanEqual:
  /** A window needs its off-window policy. */
  case WindowWithoutPolicy

  /** An off-window policy needs a window. */
  case PolicyWithoutWindow(policy: OffWindowPolicy)

  /** The grid or the window refused the geometry. */
  case Grid(underlying: GeometryError)

  /** The plan refused the recipe. */
  case Plan(underlying: PlanError)

  def field: StudyField = this match
    case WindowWithoutPolicy | PolicyWithoutWindow(_) => StudyField.OffWindow
    case Grid(_)                                      => StudyField.Grid
    case Plan(e)                                      => StudyRecipeError.fieldOf(e)

  def message: String = this match
    case WindowWithoutPolicy    => "An analysis window needs a policy for fixations outside it."
    case PolicyWithoutWindow(p) =>
      s"The off-window policy $p needs an analysis window; the study maps the whole frame."
    case Grid(e) => e.message
    case Plan(e) => e.message

object StudyRecipeError:
  /** The study field a plan refusal concerns. */
  def fieldOf(error: PlanError): StudyField =
    import PlanError.*
    error match
      case InvalidDefinition(_, _) | Specification(_) | ChangedPreparedPlan(_, _) |
          ComparisonWork(_) | UnsupportedExecution(_, _) =>
        StudyField.Method
      case InvalidArtifact(_) | MissingArtifact(_) | ArtifactMismatch(_, _) | Schedule(_) |
          StudyWorkBudget(_, _, _, _) =>
        StudyField.Input
      case InvalidPhases(_, _)                 => StudyField.Phases
      case EmptyScales(_) | DuplicateScales(_) => StudyField.Scales
      // configure's only geometry check is the angular scale's frame.
      case Geometry(_) | MissingAngularScale(_) => StudyField.AngularScale
      case InvalidWindowTally(_, _, _, _, _, _) => StudyField.Window
      case BlankKeyField(_)                     => StudyField.Layout
      case InvalidOccurrence(_) | OccurrenceUnavailable(_, _) | MatchItemConflict(_) |
          MatchedCardinality(_, _, _) =>
        StudyField.MatchedReferences
      case UnmatchedFocalRefused(_) => StudyField.UnmatchedFocal
      case InitialFixations(_)      => StudyField.InitialFixations

/** The form of a fixation study's recipe: one field per editable recipe
  * field, each parsed on its own (a raw value to a typed value, or a
  * `FieldError`), without a whole valid recipe. The input, layout and method
  * come from outside the form. Defaults are the library's own factory
  * defaults only (pairing and initial fixations).
  */
final class StudyForm[U <: Unit2D](val context: StudyFormContext[U])(using u: UnitLabel[U]):
  import RawParts.*
  import RecipeViews as V
  private type E = RecipeParameterError
  private def err(e: E): String = e.message

  val phases: StructuredField[E, StudyPhases] =
    StructuredField.of[E, StudyPhases](
      V.phases(
        "phases",
        "Focal and reference phases; exhaustive controls stay within participant"
      )
    )(
      raw =>
        for
          f <- tokenPart("phases", raw, "focal")
          r <- tokenPart("phases", raw, "reference")
          p <- StudyPhases.of(f, r).left.map(RecipeParameterError.Plan.apply)
        yield p,
      p =>
        group("focal" -> RawValue.Choice(p.focal), "reference" -> RawValue.Choice(p.reference)),
      err
    )

  val weight: StructuredField[E, Weight] =
    StructuredField.of[E, Weight](V.weight)(
      raw => choose("weight", raw, Weight.values.toVector),
      choice(_),
      err
    )

  val failurePolicy: StructuredField[E, FailurePolicy] =
    StructuredField.of[E, FailurePolicy](V.failurePolicy)(
      raw =>
        optional(raw).fold(Right(FailurePolicy.RequireAll))(n =>
          intOf("failurePolicy", n).flatMap(
            FailurePolicy.successfulOnly(_).left.map(RecipeParameterError.Reduction.apply)
          )
        ),
      {
        case FailurePolicy.RequireAll        => RawValue.Absent
        case FailurePolicy.SuccessfulOnly(m) => number(m.value)
      },
      err
    )

  val grid: StructuredField[E, GridCells] =
    StructuredField.of[E, GridCells](
      V.view(
        "grid",
        "Grid columns and rows over the mapped region; row-major cells",
        FieldKind.Group(
          Vector(
            V.atLeastOne("columns", "Grid columns", Counted.Cells),
            V.atLeastOne("rows", "Grid rows", Counted.Cells)
          ),
          GroupRule.Independent
        )
      )
    )(
      raw =>
        for
          c <- int("grid", raw, "columns")
          r <- int("grid", raw, "rows")
          g <- geometry(GridCells.of(c, r))
        yield g,
      g => group("columns" -> number(g.columns), "rows" -> number(g.rows)),
      err
    )

  val window: StructuredField[E, Option[Subframe[U]]] =
    StructuredField.of[E, Option[Subframe[U]]](
      V.view(
        "window",
        "Analysis window: a half-open region [xMin, xMax) x [yMin, yMax) of the admission frame",
        FieldKind.Optional(
          V.box("region", "The window's region of the admission frame", u.planar, Vector.empty),
          "Maps cover the whole admission frame"
        )
      )
    )(
      raw =>
        optional(raw).traverse { r =>
          for
            x0 <- real("window", r, "xMin")
            y0 <- real("window", r, "yMin")
            x1 <- real("window", r, "xMax")
            y1 <- real("window", r, "yMax")
            b  <- geometry(Bounds.of[U](x0, y0, x1, y1))
            id <- context.window.toRight(
              RecipeParameterError.UndeclaredWindowFrame(context.admission.id)
            )
            s <- geometry(Subframe.of(context.admission, id, b))
          yield s
        },
      _.fold(RawValue.Absent)(w =>
        group(
          "xMin" -> number(w.region.xMin),
          "yMin" -> number(w.region.yMin),
          "xMax" -> number(w.region.xMax),
          "yMax" -> number(w.region.yMax)
        )
      ),
      err
    )

  val offWindow: StructuredField[E, Option[OffWindowPolicy]] =
    StructuredField.of[E, Option[OffWindowPolicy]](
      V.view(
        "offWindow",
        "Fixations on the screen but outside the window: excluded from the map, or failing the trial",
        FieldKind.Optional(
          V.choice("policy", "The off-window policy", OffWindowPolicy.values.toVector),
          "Only a windowed study has an off-window policy"
        )
      )
    )(
      raw => optional(raw).traverse(choose("offWindow", _, OffWindowPolicy.values.toVector)),
      _.fold(RawValue.Absent)(choice(_)),
      err
    )

  val angularScale: StructuredField[E, Option[LinearAngularScale[U]]] =
    StructuredField.of[E, Option[LinearAngularScale[U]]](
      V.view(
        "angularScale",
        "Declared linear units per degree on the admission frame; not a calibration",
        FieldKind.Optional(
          V.positive(
            "unitsPerDegree",
            "Linear units per degree",
            Quantity.UnitsPerDegree(u.planar)
          ),
          "Scales and radii are in frame units only"
        )
      )
    )(
      raw =>
        optional(raw).traverse(n =>
          realOf("angularScale", n).flatMap(x =>
            geometry(LinearAngularScale.of(context.admission, x))
          )
        ),
      _.fold(RawValue.Absent)(s => number(s.unitsPerDegree)),
      err
    )

  // A frame already in degrees declares its scales natively.
  private val degreeCases: Vector[VariantCase] =
    if u.planar == PlanarUnit.Deg then Vector.empty
    else
      V.estimateCases(PlanarUnit.Deg)
        .map(c => VariantCase(StudyForm.degrees + c.token, s"${c.label}, in degrees", c.parts))

  val scales: StructuredField[E, Vector[StudyScale[U]]] =
    StructuredField.of[E, Vector[StudyScale[U]]](
      V.view(
        "scales",
        "Estimation scales, in frame units or in degrees; each is its own analysis",
        FieldKind.Repeated(
          V.view(
            "scale",
            "One estimation scale",
            FieldKind.Variant(V.estimateCases(u.planar) ++ degreeCases)
          ),
          1,
          None
        )
      )
    )(
      {
        case RawValue.Items(items) => items.traverse(StudyForm.scale[U])
        case _                     => Left(missing("scales", "items"))
      },
      scales => RawValue.Items(scales.map(StudyForm.scaleRaw)),
      err
    )

  val pairing: StructuredField[E, StudyPairing] =
    StructuredField.of[E, StudyPairing](V.pairing)(
      raw =>
        for
          m <- part("pairing", raw, "matched").flatMap(StudyForm.matched)
          c <- part("pairing", raw, "controls")
            .flatMap(choose("pairing", _, ControlReferences.values.toVector))
          f <- part("pairing", raw, "unmatched")
            .flatMap(choose("pairing", _, UnmatchedFocalPolicy.values.toVector))
        yield StudyPairing(m, c, f),
      p =>
        group(
          "matched"   -> StudyForm.matchedRaw(p.matched),
          "controls"  -> choice(p.controls),
          "unmatched" -> choice(p.unmatched)
        ),
      err
    )

  val initialFixations: StructuredField[E, InitialFixationPolicy[U]] =
    StructuredField.of[E, InitialFixationPolicy[U]](V.initialFixations(u.planar))(
      raw =>
        token("initialFixations", raw).flatMap {
          case "keepAll"                 => Right(InitialFixationPolicy.keepAll[U])
          case "dropFirst"               => Right(InitialFixationPolicy.dropFirst[U])
          case "dropLeadingInClosedDisc" =>
            for
              x <- real("initialFixations", raw, "crossX")
              y <- real("initialFixations", raw, "crossY")
              r <- real("initialFixations", raw, "radius")
              p <- InitialFixationPolicy
                .dropLeadingInClosedDisc(Pt[U](x, y), r)
                .left
                .map(RecipeParameterError.InitialFixation.apply)
            yield p
          case t =>
            Left(
              unknown(
                "initialFixations",
                t,
                Vector("keepAll", "dropFirst", "dropLeadingInClosedDisc")
              )
            )
        },
      {
        case InitialFixationPolicy.KeepAll()                         => variant("keepAll")
        case InitialFixationPolicy.DropFirst()                       => variant("dropFirst")
        case InitialFixationPolicy.DropLeadingInClosedDisc(cross, r) =>
          variant(
            "dropLeadingInClosedDisc",
            "crossX" -> number(cross.x),
            "crossY" -> number(cross.y),
            "radius" -> number(r)
          )
      },
      err
    )

  /** Every field with the study field it edits, in [[StudyField]] order. */
  val fields: Vector[(StudyField, FormField[E, ?])] = Vector(
    StudyField.Phases            -> phases,
    StudyField.Weighting         -> weight,
    StudyField.FailurePolicy     -> failurePolicy,
    StudyField.Grid              -> grid,
    StudyField.Window            -> window,
    StudyField.OffWindow         -> offWindow,
    StudyField.Scales            -> scales,
    StudyField.AngularScale      -> angularScale,
    StudyField.MatchedReferences -> pairing,
    StudyField.InitialFixations  -> initialFixations
  )

  /** The host views of the fields, in order. */
  def views: Vector[FieldView] = fields.map(_._2.view)

  /** The study field a form field edits. Pairing also covers the control and
    * unmatched-focal fields.
    */
  def studyField(id: FieldId): Option[StudyField] = fields.collectFirst {
    case (f, field) if field.view.id == id => f
  }

  /** One raw value checked by its own field, without the other fields. */
  def validate(field: FieldId, raw: RawValue): Either[FieldError[E], Unit] =
    fields
      .find(_._2.view.id == field)
      .toRight(FieldError.UnknownField(field))
      .flatMap(_._2.parse(raw).map(_ => ()))

  /** Every field parsed on its own; all refusals are reported together. */
  def parse(values: FormValues): Either[FormParse.Refusals, StudyRecipe[U]] =
    val p       = phases.parse(values.get(phases.view.id))
    val w       = weight.parse(values.get(weight.view.id))
    val f       = failurePolicy.parse(values.get(failurePolicy.view.id))
    val g       = grid.parse(values.get(grid.view.id))
    val wi      = window.parse(values.get(window.view.id))
    val o       = offWindow.parse(values.get(offWindow.view.id))
    val a       = angularScale.parse(values.get(angularScale.view.id))
    val s       = scales.parse(values.get(scales.view.id))
    val pa      = pairing.parse(values.get(pairing.view.id))
    val i       = initialFixations.parse(values.get(initialFixations.view.id))
    val unknown = values.values.keys.toVector.sortBy(_.value).collect {
      case k if !fields.exists(_._2.view.id == k) => FieldError.UnknownField(k)
    }
    FormParse.errors(p, w, f, g, wi, o, a, s, pa, i) match
      case Some(refusals) => Left(refusals.appendVector(unknown))
      case None           =>
        cats.data.NonEmptyVector.fromVector(unknown).toLeft(()).flatMap { _ =>
          (for
            p1  <- p
            w1  <- w
            f1  <- f
            g1  <- g
            wi1 <- wi
            o1  <- o
            a1  <- a
            s1  <- s
            pa1 <- pa
            i1  <- i
          yield StudyRecipe(context, p1, w1, f1, g1, wi1, o1, a1, s1, pa1, i1)).left.map(
            cats.data.NonEmptyVector.one
          )
        }

  /** The raw values of a plan's recipe, as the form holds them. */
  def values[K, P, S, D](plan: StudyPlan[K, U, P, S, D]): FormValues =
    val (w, o) = plan.geometry match
      case StudyGeometry.Windowed(win, _, policy) => (Some(win), Some(policy))
      case _                                      => (None, None)
    FormValues.of(
      phases.view.id           -> phases.raw(StudyPhases.of(plan)),
      weight.view.id           -> weight.raw(plan.weight),
      failurePolicy.view.id    -> failurePolicy.raw(plan.policy),
      grid.view.id             -> grid.raw(GridCells.of(plan.grid)),
      window.view.id           -> window.raw(w),
      offWindow.view.id        -> offWindow.raw(o),
      angularScale.view.id     -> angularScale.raw(plan.angularScale),
      scales.view.id           -> scales.raw(plan.scales),
      pairing.view.id          -> pairing.raw(plan.pairing),
      initialFixations.view.id -> initialFixations.raw(plan.initialFixations)
    )

object StudyForm:
  import RawParts.*

  /** The ids of the study form's fields, and of the method, which the form
    * takes from outside.
    */
  object ids:
    val phases: FieldId           = FieldId.literal("phases")
    val weight: FieldId           = FieldId.literal("weight")
    val failurePolicy: FieldId    = FieldId.literal("failurePolicy")
    val grid: FieldId             = FieldId.literal("grid")
    val window: FieldId           = FieldId.literal("window")
    val offWindow: FieldId        = FieldId.literal("offWindow")
    val scales: FieldId           = FieldId.literal("scales")
    val angularScale: FieldId     = FieldId.literal("angularScale")
    val pairing: FieldId          = FieldId.literal("pairing")
    val initialFixations: FieldId = FieldId.literal("initialFixations")
    val method: FieldId           = FieldId.literal("method")

  /** The form field that edits a study field; `None` for the input, layout
    * and method, which the form takes from outside.
    */
  def formField(field: StudyField): Option[FieldId] = field match
    case StudyField.Input | StudyField.Layout | StudyField.Method => None
    case StudyField.Phases                                        => Some(ids.phases)
    case StudyField.Weighting                                     => Some(ids.weight)
    case StudyField.FailurePolicy                                 => Some(ids.failurePolicy)
    case StudyField.Grid                                          => Some(ids.grid)
    case StudyField.Window                                        => Some(ids.window)
    case StudyField.OffWindow                                     => Some(ids.offWindow)
    case StudyField.Scales                                        => Some(ids.scales)
    case StudyField.AngularScale                                  => Some(ids.angularScale)
    case StudyField.MatchedReferences | StudyField.ControlReferences |
        StudyField.UnmatchedFocal =>
      Some(ids.pairing)
    case StudyField.InitialFixations => Some(ids.initialFixations)

  /** The token prefix of a scale declared in degrees. */
  val degrees: String = "degrees."

  private def estimate[V <: Unit2D](raw: RawValue, name: String): R[StudyEstimate[V]] =
    val edges = (r: RawValue) =>
      part("scale", r, "edges").flatMap(choose("scale", _, EdgePolicy.values.toVector))
    def sigma(id: String) =
      real("scale", raw, id).flatMap(x => geometry(Sigma.of[V](x)))
    name match
      case "binned"   => Right(StudyEstimate.Binned[V]())
      case "gaussian" =>
        for s <- sigma("sigma"); e <- edges(raw) yield StudyEstimate.Gaussian(s, e)
      case "anisotropic" =>
        for
          x <- sigma("sigmaX")
          y <- sigma("sigmaY")
          e <- edges(raw)
        yield StudyEstimate.Anisotropic(x, y, e)
      case other => Left(unknown("scale", other, Vector("binned", "gaussian", "anisotropic")))

  private[plan] def scale[U <: Unit2D](raw: RawValue): R[StudyScale[U]] =
    token("scale", raw).flatMap { t =>
      if t.startsWith(degrees) then
        estimate[Unit2D.Deg](raw, t.stripPrefix(degrees)).map(StudyScale.Angular[U](_))
      else estimate[U](raw, t).map(StudyScale.Native(_))
    }

  private def estimateRaw[V <: Unit2D](prefix: String, e: StudyEstimate[V]): RawValue = e match
    case StudyEstimate.Binned()           => variant(prefix + "binned")
    case StudyEstimate.Gaussian(s, edges) =>
      variant(prefix + "gaussian", "sigma" -> number(s.value), "edges" -> choice(edges))
    case StudyEstimate.Anisotropic(x, y, edges) =>
      variant(
        prefix + "anisotropic",
        "sigmaX" -> number(x.value),
        "sigmaY" -> number(y.value),
        "edges"  -> choice(edges)
      )

  private[plan] def scaleRaw[U <: Unit2D](scale: StudyScale[U]): RawValue = scale match
    case StudyScale.Native(e)  => estimateRaw("", e)
    case StudyScale.Angular(e) => estimateRaw(degrees, e)

  private[plan] def matched(raw: RawValue): R[MatchedReferences] =
    token("pairing", raw).flatMap {
      case "requireOne"     => Right(MatchedReferences.RequireOne)
      case "sameOccurrence" => Right(MatchedReferences.SameOccurrence)
      case "meanOfAll"      => Right(MatchedReferences.MeanOfAll)
      case "select"         =>
        part("pairing", raw, "occurrence").flatMap { o =>
          token("pairing", o).flatMap {
            case "first" => Right(MatchedReferences.Select(OccurrenceChoice.First))
            case "last"  => Right(MatchedReferences.Select(OccurrenceChoice.Last))
            case "at"    =>
              int("pairing", o, "n").flatMap(n =>
                TrialOccurrence
                  .of(n)
                  .map(t => MatchedReferences.Select(OccurrenceChoice.At(t)))
                  .left
                  .map(RecipeParameterError.Plan.apply)
              )
            case t => Left(unknown("pairing", t, Vector("first", "last", "at")))
          }
        }
      case t =>
        Left(
          unknown("pairing", t, Vector("requireOne", "sameOccurrence", "select", "meanOfAll"))
        )
    }

  private[plan] def matchedRaw(m: MatchedReferences): RawValue = m match
    case MatchedReferences.RequireOne     => variant("requireOne")
    case MatchedReferences.SameOccurrence => variant("sameOccurrence")
    case MatchedReferences.MeanOfAll      => variant("meanOfAll")
    case MatchedReferences.Select(c)      =>
      val occurrence = c match
        case OccurrenceChoice.First => variant("first")
        case OccurrenceChoice.Last  => variant("last")
        case OccurrenceChoice.At(n) => variant("at", "n" -> number(n.value))
      variant("select", "occurrence" -> occurrence)
