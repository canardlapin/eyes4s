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

package eyes4s.studio.core.document

import cats.syntax.all.*
import eyes4s.core.Weight
import eyes4s.design.FailurePolicy
import eyes4s.plan.{
  ControlReferences,
  DefinitionId,
  MatchedReferences,
  OccurrenceChoice,
  OffWindowPolicy,
  StudyField,
  UnmatchedFocalPolicy
}
import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, Phase}
import io.circe.{Codec, Decoder, Encoder}

// ---------------------------------------------------------------------------
// Recipe values
// ---------------------------------------------------------------------------

private[document] object Degrees:
  /** `2` not `2.0`, the same on the JVM and Scala.js. */
  def render(value: Double): String =
    if value == math.rint(value) && math.abs(value) < 1e15 then value.toLong.toString
    else value.toString

/** One smoothing scale: the kernel's standard deviation, in degrees. */
final case class Sigma private (degrees: Double) derives CanEqual:
  def render: String = s"σ ${Degrees.render(degrees)}°"

object Sigma:
  def of(degrees: Double): Either[DocumentError, Sigma] =
    Checks.positive("sigma (degrees)", degrees).map(new Sigma(_))

  given Codec[Sigma] = DocumentCodecs.validated(of, _.degrees)

/** The declared scales, in order: at least one, none repeated. */
final case class ScaleSet private (values: Vector[Sigma]) derives CanEqual:
  def render: String = values.map(s => Degrees.render(s.degrees)).mkString(", ") + "°"

  def contains(sigma: Sigma): Boolean = values.contains(sigma)

object ScaleSet:
  def of(values: Vector[Sigma]): Either[DocumentError, ScaleSet] =
    if values.isEmpty then Left(DocumentError.EmptyScales)
    else if values.distinct.size != values.size then
      Left(DocumentError.DuplicateScales(values.map(_.degrees)))
    else Right(new ScaleSet(values))

  given Codec[ScaleSet] = DocumentCodecs.validated(of, _.values)

/** The density grid on the image frame, in cells. */
final case class GridSize private (columns: Int, rows: Int) derives CanEqual:
  def render: String = s"$columns×$rows"

object GridSize:
  def of(columns: Int, rows: Int): Either[DocumentError, GridSize] =
    (Checks.positive("grid columns", columns), Checks.positive("grid rows", rows))
      .mapN(new GridSize(_, _))

  given Encoder.AsObject[GridSize] =
    Encoder.forProduct2("columns", "rows")(g => (g.columns, g.rows))
  given Decoder[GridSize] =
    Decoder.forProduct2("columns", "rows")(of).emap(_.left.map(_.message))

/** The focal (query) phase and the reference phase. */
final case class PhasePair(focal: Phase, reference: Phase) derives CanEqual, Codec.AsObject:
  def render: String = s"${focal.label} vs ${reference.label}"

/** The radius of the disc around the fixation cross, in degrees. */
final case class CrossRadius private (degrees: Double) derives CanEqual

object CrossRadius:
  def of(degrees: Double): Either[DocumentError, CrossRadius] =
    Checks.positive("cross radius (degrees)", degrees).map(new CrossRadius(_))

  given Codec[CrossRadius] = DocumentCodecs.validated(of, _.degrees)

/** eyes4s `InitialFixationPolicy` (UI-F), applied to queries and references
  * alike.
  */
enum InitialFixationChoice derives CanEqual, Codec.AsObject:
  case KeepAll, DropFirst
  case DropLeadingNearCross(radius: CrossRadius)

  def render: String = this match
    case KeepAll                 => "keep all"
    case DropFirst               => "drop first fixation"
    case DropLeadingNearCross(r) =>
      s"drop fixations within ${Degrees.render(r.degrees)}° of the cross before first saccade"

/** An occurrence number, 1 for an item's first presentation. */
final case class Occurrence private (value: Int) derives CanEqual

object Occurrence:
  def of(value: Int): Either[DocumentError, Occurrence] =
    Checks.positive("occurrence", value).map(new Occurrence(_))

  given Codec[Occurrence] = DocumentCodecs.validated(of, _.value)

/** eyes4s `OccurrenceChoice`. */
enum OccurrencePick derives CanEqual, Codec.AsObject:
  case First, Last
  case At(occurrence: Occurrence)

  def render: String = this match
    case First => "first"
    case Last  => "last"
    case At(o) => s"occurrence ${o.value}"

object OccurrencePick:
  def of(choice: OccurrenceChoice): Either[DocumentError, OccurrencePick] = choice match
    case OccurrenceChoice.First => Right(First)
    case OccurrenceChoice.Last  => Right(Last)
    case OccurrenceChoice.At(o) => Occurrence.of(o.value).map(At(_))

/** eyes4s `MatchedReferences` (UI-B). */
enum MatchedChoice derives CanEqual, Codec.AsObject:
  case RequireOne, SameOccurrence
  case Select(pick: OccurrencePick)
  case MeanOfAll

  def render: String = this match
    case RequireOne     => "require one"
    case SameOccurrence => "same occurrence"
    case Select(pick)   => s"select ${pick.render}"
    case MeanOfAll      => "mean of all"

object MatchedChoice:
  def of(core: MatchedReferences): Either[DocumentError, MatchedChoice] = core match
    case MatchedReferences.RequireOne     => Right(RequireOne)
    case MatchedReferences.SameOccurrence => Right(SameOccurrence)
    case MatchedReferences.Select(c)      => OccurrencePick.of(c).map(Select(_))
    case MatchedReferences.MeanOfAll      => Right(MeanOfAll)

/** eyes4s `ControlReferences` (UI-B). `AllOccurrences` is never the default. */
enum ControlChoice derives CanEqual, Codec.AsObject:
  case SameSelection, AllOccurrences

  def render: String = this match
    case SameSelection  => "same selection"
    case AllOccurrences => "all occurrences as controls"

object ControlChoice:
  def of(core: ControlReferences): ControlChoice = core match
    case ControlReferences.SameSelection  => SameSelection
    case ControlReferences.AllOccurrences => AllOccurrences

/** eyes4s `UnmatchedFocalPolicy` (UI-B): "Queries without a matched
  * reference".
  */
enum UnmatchedChoice derives CanEqual, Codec.AsObject:
  case ReportNoMatch, Refuse

  def render: String = this match
    case ReportNoMatch => "report as no match"
    case Refuse        => "refuse the study"

object UnmatchedChoice:
  def of(core: UnmatchedFocalPolicy): UnmatchedChoice = core match
    case UnmatchedFocalPolicy.ReportNoMatch => ReportNoMatch
    case UnmatchedFocalPolicy.Refuse        => Refuse

/** A registered eyes4s definition, by name and version (a layout or a
  * method). Validated by the public `DefinitionId.of`.
  */
final case class DefinitionRef private (name: String, version: Int) derives CanEqual:
  def render: String = s"$name@$version"

object DefinitionRef:
  def of(name: String, version: Int): Either[DocumentError, DefinitionRef] =
    DefinitionId
      .of(name, version)
      .bimap(
        _ => DocumentError.BadDefinition(name, version),
        d => new DefinitionRef(d.name, d.version)
      )

  def fromCore(id: DefinitionId): DefinitionRef = new DefinitionRef(id.name, id.version)

  given Encoder.AsObject[DefinitionRef] =
    Encoder.forProduct2("name", "version")(d => (d.name, d.version))
  given Decoder[DefinitionRef] =
    Decoder.forProduct2("name", "version")(of).emap(_.left.map(_.message))

/** One method parameter as eyes4s describes it (`Provenance.Param` rendered). */
final case class MethodParameter(name: String, value: String) derives CanEqual, Codec.AsObject

/** The comparison method and its described parameters. */
final case class MethodSpec(definition: DefinitionRef, parameters: Vector[MethodParameter])
    derives CanEqual,
      Codec.AsObject:
  def render: String =
    if parameters.isEmpty then definition.render
    else
      definition.render + parameters.map(p => s"${p.name}=${p.value}").mkString("(", ", ", ")")

/** eyes4s `Weight`: how much each fixation counts toward a map. */
enum WeightChoice derives CanEqual, Codec.AsObject:
  case Uniform, Duration

  def render: String = this match
    case Uniform  => "uniform"
    case Duration => "duration"

object WeightChoice:
  def of(core: Weight): WeightChoice = core match
    case Weight.Uniform  => Uniform
    case Weight.Duration => Duration

/** The fewest successful scores a reduction needs. */
final case class MinimumSuccessful private (value: Int) derives CanEqual

object MinimumSuccessful:
  def of(value: Int): Either[DocumentError, MinimumSuccessful] =
    Checks.positive("minimum successful scores", value).map(new MinimumSuccessful(_))

  given Codec[MinimumSuccessful] = DocumentCodecs.validated(of, _.value)

/** eyes4s `FailurePolicy`: how score failures affect a reduction. */
enum FailureChoice derives CanEqual, Codec.AsObject:
  case RequireAll
  case SuccessfulOnly(minimum: MinimumSuccessful)

  def render: String = this match
    case RequireAll        => "require all"
    case SuccessfulOnly(m) => s"successful only (minimum ${m.value})"

object FailureChoice:
  def of(core: FailurePolicy): Either[DocumentError, FailureChoice] = core match
    case FailurePolicy.RequireAll        => Right(RequireAll)
    case FailurePolicy.SuccessfulOnly(m) => MinimumSuccessful.of(m.value).map(SuccessfulOnly(_))

/** The analysis window in admission-frame units (screen px, y down): the
  * half-open box [xMin, xMax) × [yMin, yMax), non-empty and finite.
  */
final case class AnalysisWindow private (xMin: Double, yMin: Double, xMax: Double, yMax: Double)
    derives CanEqual:
  def render: String =
    s"[${Degrees.render(xMin)}, ${Degrees.render(xMax)}) × " +
      s"[${Degrees.render(yMin)}, ${Degrees.render(yMax)}) px"

object AnalysisWindow:
  def of(
      xMin: Double,
      yMin: Double,
      xMax: Double,
      yMax: Double
  ): Either[DocumentError, AnalysisWindow] =
    for
      _ <- Vector("xMin" -> xMin, "yMin" -> yMin, "xMax" -> xMax, "yMax" -> yMax).traverse_ {
        (f, v) => Checks.finite(s"window $f", v)
      }
      _ <- Either.cond(
        xMin < xMax && yMin < yMax,
        (),
        DocumentError.EmptyWindow(xMin, yMin, xMax, yMax)
      )
    yield new AnalysisWindow(xMin, yMin, xMax, yMax)

  given Encoder.AsObject[AnalysisWindow] =
    Encoder.forProduct4("xMin", "yMin", "xMax", "yMax")(w => (w.xMin, w.yMin, w.xMax, w.yMax))
  given Decoder[AnalysisWindow] =
    Decoder.forProduct4("xMin", "yMin", "xMax", "yMax")(of).emap(_.left.map(_.message))

/** eyes4s `OffWindowPolicy`: fixations on the screen but outside the window. */
enum OffWindowChoice derives CanEqual, Codec.AsObject:
  case Exclude, FailTrial

  def render: String = this match
    case Exclude   => "exclude"
    case FailTrial => "fail the trial"

object OffWindowChoice:
  def of(core: OffWindowPolicy): OffWindowChoice = core match
    case OffWindowPolicy.Exclude   => Exclude
    case OffWindowPolicy.FailTrial => FailTrial

private[document] object OptionalRender:
  def apply[A](none: String)(render: A => String)(value: Option[A]): String =
    value.fold(none)(render)

/** The fields of an analysis the studio form edits: a mirror of every eyes4s
  * `StudyPlan` field a structural diff compares (`StudyField`), in its order.
  * The plan itself is bound by digest in [[AnalysisRevisionSpec.plan]].
  *
  *  - `input`: the eyes4s `StudyInput` reference, `None` while unbound;
  *  - `window`: `None` maps the whole admission frame; `offWindow` is `None`
  *    exactly for a whole-frame plan (eyes4s checks the pairing when it
  *    configures the plan);
  *  - `angularScale`: the plan's declared units per degree, `None` for none.
  */
final case class Recipe(
    input: Option[SemanticIdentity],
    layout: DefinitionRef,
    method: MethodSpec,
    phases: PhasePair,
    weighting: WeightChoice,
    failurePolicy: FailureChoice,
    grid: GridSize,
    window: Option[AnalysisWindow],
    offWindow: Option[OffWindowChoice],
    scales: ScaleSet,
    angularScale: Option[DeclaredPixelsPerDegree],
    matched: MatchedChoice,
    controls: ControlChoice,
    unmatched: UnmatchedChoice,
    initialFixations: InitialFixationChoice
) derives CanEqual,
      Codec.AsObject

// ---------------------------------------------------------------------------
// Drafts
// ---------------------------------------------------------------------------

/** The recipe fields a draft changes: every eyes4s `StudyField`, in its order. */
enum RecipeField derives CanEqual, Codec.AsObject:
  case Input, Layout, Method, Phases, Weighting, FailurePolicy, Grid, Window, OffWindow, Scales,
    AngularScale, MatchedReferences, ControlReferences, UnmatchedFocal, InitialFixations

  def core: StudyField = this match
    case Input             => StudyField.Input
    case Layout            => StudyField.Layout
    case Method            => StudyField.Method
    case Phases            => StudyField.Phases
    case Weighting         => StudyField.Weighting
    case FailurePolicy     => StudyField.FailurePolicy
    case Grid              => StudyField.Grid
    case Window            => StudyField.Window
    case OffWindow         => StudyField.OffWindow
    case Scales            => StudyField.Scales
    case AngularScale      => StudyField.AngularScale
    case MatchedReferences => StudyField.MatchedReferences
    case ControlReferences => StudyField.ControlReferences
    case UnmatchedFocal    => StudyField.UnmatchedFocal
    case InitialFixations  => StudyField.InitialFixations

  def label: String = core.label

object RecipeField:
  /** The studio field of an eyes4s `StudyField`: every one has a mirror. */
  def of(field: StudyField): RecipeField = field match
    case StudyField.Input             => Input
    case StudyField.Layout            => Layout
    case StudyField.Method            => Method
    case StudyField.Phases            => Phases
    case StudyField.Weighting         => Weighting
    case StudyField.FailurePolicy     => FailurePolicy
    case StudyField.Grid              => Grid
    case StudyField.Window            => Window
    case StudyField.OffWindow         => OffWindow
    case StudyField.Scales            => Scales
    case StudyField.AngularScale      => AngularScale
    case StudyField.MatchedReferences => MatchedReferences
    case StudyField.ControlReferences => ControlReferences
    case StudyField.UnmatchedFocal    => UnmatchedFocal
    case StudyField.InitialFixations  => InitialFixations

/** One field's typed values and how a recipe holds it. */
private[document] final class FieldPart[A](
    before: A,
    after: A,
    get: Recipe => A,
    set: (Recipe, A) => Recipe,
    show: A => String
)(using CanEqual[A, A]):
  def identity: Boolean                   = before == after
  def mismatch(r: Recipe): Option[String] = Option.when(get(r) != before)(show(get(r)))
  def applyTo(r: Recipe): Recipe          = set(r, after)
  def rendered: (String, String)          = (show(before), show(after))

/** One recipe field a draft changes, with its typed value before and after:
  * the studio mirror of eyes4s `StudyChange` (UI-F), one case per
  * `StudyField`, which S3.7 maps from `StudyPlan.structuralDiff`.
  */
enum RecipeChange derives CanEqual, Codec.AsObject:
  case Input(before: Option[SemanticIdentity], after: Option[SemanticIdentity])
  case Layout(before: DefinitionRef, after: DefinitionRef)
  case Method(before: MethodSpec, after: MethodSpec)
  case Phases(before: PhasePair, after: PhasePair)
  case Weighting(before: WeightChoice, after: WeightChoice)
  case Failures(before: FailureChoice, after: FailureChoice)
  case Grid(before: GridSize, after: GridSize)
  case Window(before: Option[AnalysisWindow], after: Option[AnalysisWindow])
  case OffWindow(before: Option[OffWindowChoice], after: Option[OffWindowChoice])
  case Scales(before: ScaleSet, after: ScaleSet)
  case AngularScale(
      before: Option[DeclaredPixelsPerDegree],
      after: Option[DeclaredPixelsPerDegree]
  )
  case Matched(before: MatchedChoice, after: MatchedChoice)
  case Controls(before: ControlChoice, after: ControlChoice)
  case Unmatched(before: UnmatchedChoice, after: UnmatchedChoice)
  case InitialFixations(before: InitialFixationChoice, after: InitialFixationChoice)

  private def part: FieldPart[?] = this match
    case Input(b, a) =>
      FieldPart(b, a, _.input, (r, v) => r.copy(input = v), OptionalRender("unbound")(_.value))
    case Layout(b, a)    => FieldPart(b, a, _.layout, (r, v) => r.copy(layout = v), _.render)
    case Method(b, a)    => FieldPart(b, a, _.method, (r, v) => r.copy(method = v), _.render)
    case Phases(b, a)    => FieldPart(b, a, _.phases, (r, v) => r.copy(phases = v), _.render)
    case Weighting(b, a) =>
      FieldPart(b, a, _.weighting, (r, v) => r.copy(weighting = v), _.render)
    case Failures(b, a) =>
      FieldPart(b, a, _.failurePolicy, (r, v) => r.copy(failurePolicy = v), _.render)
    case Grid(b, a)   => FieldPart(b, a, _.grid, (r, v) => r.copy(grid = v), _.render)
    case Window(b, a) =>
      FieldPart(
        b,
        a,
        _.window,
        (r, v) => r.copy(window = v),
        OptionalRender[AnalysisWindow]("whole frame")(_.render)
      )
    case OffWindow(b, a) =>
      FieldPart(
        b,
        a,
        _.offWindow,
        (r, v) => r.copy(offWindow = v),
        OptionalRender[OffWindowChoice]("none")(_.render)
      )
    case Scales(b, a)       => FieldPart(b, a, _.scales, (r, v) => r.copy(scales = v), _.render)
    case AngularScale(b, a) =>
      FieldPart(
        b,
        a,
        _.angularScale,
        (r, v) => r.copy(angularScale = v),
        OptionalRender[DeclaredPixelsPerDegree]("none")(p => s"${Degrees.render(p.value)} px/°")
      )
    case Matched(b, a)  => FieldPart(b, a, _.matched, (r, v) => r.copy(matched = v), _.render)
    case Controls(b, a) => FieldPart(b, a, _.controls, (r, v) => r.copy(controls = v), _.render)
    case Unmatched(b, a) =>
      FieldPart(b, a, _.unmatched, (r, v) => r.copy(unmatched = v), _.render)
    case InitialFixations(b, a) =>
      FieldPart(b, a, _.initialFixations, (r, v) => r.copy(initialFixations = v), _.render)

  def field: RecipeField = this match
    case Input(_, _)            => RecipeField.Input
    case Layout(_, _)           => RecipeField.Layout
    case Method(_, _)           => RecipeField.Method
    case Phases(_, _)           => RecipeField.Phases
    case Weighting(_, _)        => RecipeField.Weighting
    case Failures(_, _)         => RecipeField.FailurePolicy
    case Grid(_, _)             => RecipeField.Grid
    case Window(_, _)           => RecipeField.Window
    case OffWindow(_, _)        => RecipeField.OffWindow
    case Scales(_, _)           => RecipeField.Scales
    case AngularScale(_, _)     => RecipeField.AngularScale
    case Matched(_, _)          => RecipeField.MatchedReferences
    case Controls(_, _)         => RecipeField.ControlReferences
    case Unmatched(_, _)        => RecipeField.UnmatchedFocal
    case InitialFixations(_, _) => RecipeField.InitialFixations

  /** The same change in the other direction. */
  def inverse: RecipeChange = this match
    case Input(b, a)            => Input(a, b)
    case Layout(b, a)           => Layout(a, b)
    case Method(b, a)           => Method(a, b)
    case Phases(b, a)           => Phases(a, b)
    case Weighting(b, a)        => Weighting(a, b)
    case Failures(b, a)         => Failures(a, b)
    case Grid(b, a)             => Grid(a, b)
    case Window(b, a)           => Window(a, b)
    case OffWindow(b, a)        => OffWindow(a, b)
    case Scales(b, a)           => Scales(a, b)
    case AngularScale(b, a)     => AngularScale(a, b)
    case Matched(b, a)          => Matched(a, b)
    case Controls(b, a)         => Controls(a, b)
    case Unmatched(b, a)        => Unmatched(a, b)
    case InitialFixations(b, a) => InitialFixations(a, b)

  /** The rendered values before and after. */
  def renderedValues: (String, String) = part.rendered

  def isIdentity: Boolean = part.identity

  /** The value `recipe` holds for this field, rendered, when it is not the
    * change's `before`.
    */
  def mismatch(recipe: Recipe): Option[String] = part.mismatch(recipe)

  /** `recipe` with this field set to `after`. */
  def applyTo(recipe: Recipe): Recipe = part.applyTo(recipe)

  /** The default English rendering: "scales +σ 8°", "grid 64×48 → 32×24". */
  def render: String = this match
    case Scales(b, a) =>
      val added   = a.values.filterNot(b.contains).map("+" + _.render)
      val removed = b.values.filterNot(a.contains).map("−" + _.render)
      val parts   = added ++ removed
      if parts.isEmpty then s"scales reordered: ${b.render} → ${a.render}"
      else s"scales ${parts.mkString(" ")}"
    case other =>
      val (b, a) = other.renderedValues
      s"${other.field.label} $b → $a"

object RecipeChange:
  /** The changes that turn `before` into `after`, in field order. */
  def between(before: Recipe, after: Recipe): Vector[RecipeChange] =
    Vector(
      Input(before.input, after.input),
      Layout(before.layout, after.layout),
      Method(before.method, after.method),
      Phases(before.phases, after.phases),
      Weighting(before.weighting, after.weighting),
      Failures(before.failurePolicy, after.failurePolicy),
      Grid(before.grid, after.grid),
      Window(before.window, after.window),
      OffWindow(before.offWindow, after.offWindow),
      Scales(before.scales, after.scales),
      AngularScale(before.angularScale, after.angularScale),
      Matched(before.matched, after.matched),
      Controls(before.controls, after.controls),
      Unmatched(before.unmatched, after.unmatched),
      InitialFixations(before.initialFixations, after.initialFixations)
    ).filterNot(_.isIdentity)

/** The pending changes against one analysis revision: what Save & run would
  * make revision `id` ("Draft rev 5 · 1 change"). Changes are typed, one per
  * field, none an identity, in field order. `dataset` is the dataset revision
  * Save & run would configure on when it differs from the base's (a rebase
  * onto newly admitted data). A draft changes something: a field, the
  * dataset, or both. The document checks each change's `before` against
  * `base`'s recipe and that the rebase target is admitted ([[Draft.against]],
  * `StudioDocument.of`).
  */
final case class Draft private (
    id: AnalysisRevision,
    base: AnalysisRevision,
    dataset: Option[DatasetRevision],
    changes: Vector[RecipeChange]
) derives CanEqual:
  /** Field changes, plus one for a rebase. */
  def changeCount: Int = changes.size + (if dataset.isDefined then 1 else 0)

  /** The recipe the draft describes, applied to its base's recipe. */
  def recipe(baseRecipe: Recipe): Recipe = changes.foldLeft(baseRecipe)((r, c) => c.applyTo(r))

  def render: String =
    (dataset.map(d => s"data → ${d.label}").toVector ++ changes.map(_.render)).mkString("; ")

object Draft:
  def of(
      id: AnalysisRevision,
      base: AnalysisRevision,
      dataset: Option[DatasetRevision],
      changes: Vector[RecipeChange]
  ): Either[DocumentError, Draft] =
    for
      _ <- Either.cond(
        changes.nonEmpty || dataset.nonEmpty,
        (),
        DocumentError.NoChanges(id, base)
      )
      _ <- changes.groupBy(_.field).toVector.sortBy(_._1.ordinal).traverse_ { (field, cs) =>
        Either.cond(cs.size == 1, (), DocumentError.RepeatedField(id, field))
      }
      _ <- changes.traverse_ { c =>
        Either.cond(
          !c.isIdentity,
          (),
          DocumentError.IdentityChange(id, c.field, c.renderedValues._1)
        )
      }
    yield new Draft(id, base, dataset, changes.sortBy(_.field.ordinal))

  /** A draft whose every change starts from `base`'s recipe, and whose
    * rebase, if any, leaves `base`'s dataset.
    */
  def against(
      id: AnalysisRevision,
      base: AnalysisRevisionSpec,
      dataset: Option[DatasetRevision],
      changes: Vector[RecipeChange]
  ): Either[DocumentError, Draft] =
    of(id, base.id, dataset, changes).flatTap(_.check(base))

  /** The draft that turns `base`'s recipe into `target`, optionally rebased. */
  def between(
      id: AnalysisRevision,
      base: AnalysisRevisionSpec,
      target: Recipe,
      dataset: Option[DatasetRevision] = None
  ): Either[DocumentError, Draft] =
    against(id, base, dataset, RecipeChange.between(base.recipe, target))

  extension (draft: Draft)
    private[document] def check(base: AnalysisRevisionSpec): Either[DocumentError, Unit] =
      for
        _ <- draft.dataset.traverse_ { d =>
          Either.cond(d != base.dataset, (), DocumentError.RebaseToSame(draft.id, d))
        }
        _ <- draft.changes.traverse_ { c =>
          c.mismatch(base.recipe) match
            case None       => Right(())
            case Some(held) =>
              Left(
                DocumentError.DraftBefore(draft.id, base.id, c.field, held, c.renderedValues._1)
              )
        }
      yield ()

  given Encoder.AsObject[Draft] =
    Encoder.forProduct4("id", "base", "dataset", "changes")(d =>
      (d.id, d.base, d.dataset, d.changes)
    )
  given Decoder[Draft] =
    Decoder.forProduct4("id", "base", "dataset", "changes")(of).emap(_.left.map(_.message))

// ---------------------------------------------------------------------------
// Analysis revision
// ---------------------------------------------------------------------------

/** The studio preset an analysis started from. */
enum Preset derives CanEqual, Codec.AsObject:
  /** Encoding → retrieval reinstatement: matched-minus-control similarity. */
  case EncodingRetrieval

  /** Recognition: lures and novel probes have no study trial by design. */
  case Recognition

  case Custom

/** A non-blank name. */
final case class RevisionName private (value: String) derives CanEqual

object RevisionName:
  def of(value: String): Either[DocumentError, RevisionName] =
    Checks.nonBlank("analysis name", value).map(new RevisionName(_))

  given Codec[RevisionName] = DocumentCodecs.validated(of, _.value)

/** Studio's own fields of an analysis; they are part of the document's
  * science (a methods section cites the preset and name), not presentation.
  */
final case class StudioFields(preset: Preset, name: RevisionName, description: String)
    derives CanEqual,
      Codec.AsObject

/** One saved analysis revision ("Analysis · rerun"): the eyes4s `StudyPlan`
  * it configured, bound by CR3 digest, the recipe the form shows, studio's
  * fields and the dataset revision it was configured on.
  *
  * The recipe and the plan are not checked against each other here: nothing
  * in studio-core reads a plan. S3.7 checks that a bound plan's fields are
  * exactly the recipe (`RecipeChange` over `StudyPlan.structuralDiff`) when
  * the real backend configures or reopens it.
  */
final case class AnalysisRevisionSpec(
    id: AnalysisRevision,
    dataset: DatasetRevision,
    plan: CoreBinding[StudyPlanArtifact],
    recipe: Recipe,
    studio: StudioFields
) derives CanEqual,
      Codec.AsObject
