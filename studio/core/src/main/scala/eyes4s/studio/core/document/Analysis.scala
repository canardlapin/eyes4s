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
import eyes4s.plan.{
  ControlReferences,
  MatchedReferences,
  OccurrenceChoice,
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

/** The fields of an analysis the studio form edits: a mirror of the eyes4s
  * `StudyPlan` fields a draft can change ("Analysis · rerun"). The plan
  * itself is bound by digest in [[AnalysisRevisionSpec.plan]].
  */
final case class Recipe(
    phases: PhasePair,
    grid: GridSize,
    scales: ScaleSet,
    matched: MatchedChoice,
    controls: ControlChoice,
    unmatched: UnmatchedChoice,
    initialFixations: InitialFixationChoice
) derives CanEqual,
      Codec.AsObject

// ---------------------------------------------------------------------------
// Drafts
// ---------------------------------------------------------------------------

/** The recipe fields a draft changes: a subset of eyes4s `StudyField`, in its
  * order.
  */
enum RecipeField derives CanEqual, Codec.AsObject:
  case Phases, Grid, Scales, MatchedReferences, ControlReferences, UnmatchedFocal,
    InitialFixations

  def core: StudyField = this match
    case Phases            => StudyField.Phases
    case Grid              => StudyField.Grid
    case Scales            => StudyField.Scales
    case MatchedReferences => StudyField.MatchedReferences
    case ControlReferences => StudyField.ControlReferences
    case UnmatchedFocal    => StudyField.UnmatchedFocal
    case InitialFixations  => StudyField.InitialFixations

  def label: String = core.label

/** One recipe field a draft changes, with its typed value before and after:
  * the studio mirror of eyes4s `StudyChange` (UI-F), which S3.7 maps from
  * `StudyPlan.structuralDiff`.
  */
enum RecipeChange derives CanEqual, Codec.AsObject:
  case Phases(before: PhasePair, after: PhasePair)
  case Grid(before: GridSize, after: GridSize)
  case Scales(before: ScaleSet, after: ScaleSet)
  case Matched(before: MatchedChoice, after: MatchedChoice)
  case Controls(before: ControlChoice, after: ControlChoice)
  case Unmatched(before: UnmatchedChoice, after: UnmatchedChoice)
  case InitialFixations(before: InitialFixationChoice, after: InitialFixationChoice)

  def field: RecipeField = this match
    case Phases(_, _)           => RecipeField.Phases
    case Grid(_, _)             => RecipeField.Grid
    case Scales(_, _)           => RecipeField.Scales
    case Matched(_, _)          => RecipeField.MatchedReferences
    case Controls(_, _)         => RecipeField.ControlReferences
    case Unmatched(_, _)        => RecipeField.UnmatchedFocal
    case InitialFixations(_, _) => RecipeField.InitialFixations

  /** The same change in the other direction. */
  def inverse: RecipeChange = this match
    case Phases(b, a)           => Phases(a, b)
    case Grid(b, a)             => Grid(a, b)
    case Scales(b, a)           => Scales(a, b)
    case Matched(b, a)          => Matched(a, b)
    case Controls(b, a)         => Controls(a, b)
    case Unmatched(b, a)        => Unmatched(a, b)
    case InitialFixations(b, a) => InitialFixations(a, b)

  /** The rendered values before and after. */
  def renderedValues: (String, String) = this match
    case Phases(b, a)           => (b.render, a.render)
    case Grid(b, a)             => (b.render, a.render)
    case Scales(b, a)           => (b.render, a.render)
    case Matched(b, a)          => (b.render, a.render)
    case Controls(b, a)         => (b.render, a.render)
    case Unmatched(b, a)        => (b.render, a.render)
    case InitialFixations(b, a) => (b.render, a.render)

  def isIdentity: Boolean = this match
    case Phases(b, a)           => b == a
    case Grid(b, a)             => b == a
    case Scales(b, a)           => b == a
    case Matched(b, a)          => b == a
    case Controls(b, a)         => b == a
    case Unmatched(b, a)        => b == a
    case InitialFixations(b, a) => b == a

  /** The value `recipe` holds for this field, rendered, when it is not the
    * change's `before`.
    */
  def mismatch(recipe: Recipe): Option[String] = this match
    case Phases(b, _)           => Option.when(recipe.phases != b)(recipe.phases.render)
    case Grid(b, _)             => Option.when(recipe.grid != b)(recipe.grid.render)
    case Scales(b, _)           => Option.when(recipe.scales != b)(recipe.scales.render)
    case Matched(b, _)          => Option.when(recipe.matched != b)(recipe.matched.render)
    case Controls(b, _)         => Option.when(recipe.controls != b)(recipe.controls.render)
    case Unmatched(b, _)        => Option.when(recipe.unmatched != b)(recipe.unmatched.render)
    case InitialFixations(b, _) =>
      Option.when(recipe.initialFixations != b)(recipe.initialFixations.render)

  /** `recipe` with this field set to `after`. */
  def applyTo(recipe: Recipe): Recipe = this match
    case Phases(_, a)           => recipe.copy(phases = a)
    case Grid(_, a)             => recipe.copy(grid = a)
    case Scales(_, a)           => recipe.copy(scales = a)
    case Matched(_, a)          => recipe.copy(matched = a)
    case Controls(_, a)         => recipe.copy(controls = a)
    case Unmatched(_, a)        => recipe.copy(unmatched = a)
    case InitialFixations(_, a) => recipe.copy(initialFixations = a)

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
      Phases(before.phases, after.phases),
      Grid(before.grid, after.grid),
      Scales(before.scales, after.scales),
      Matched(before.matched, after.matched),
      Controls(before.controls, after.controls),
      Unmatched(before.unmatched, after.unmatched),
      InitialFixations(before.initialFixations, after.initialFixations)
    ).filterNot(_.isIdentity)

/** The pending changes against one analysis revision: what Save & run would
  * make revision `id` ("Draft rev 5 · 1 change"). Changes are typed, one per
  * field, none an identity, in field order. The document checks each
  * change's `before` against `base`'s recipe ([[Draft.against]]).
  */
final case class Draft private (
    id: AnalysisRevision,
    base: AnalysisRevision,
    changes: Vector[RecipeChange]
) derives CanEqual:
  def changeCount: Int = changes.size

  /** The recipe the draft describes, applied to its base's recipe. */
  def recipe(baseRecipe: Recipe): Recipe = changes.foldLeft(baseRecipe)((r, c) => c.applyTo(r))

  def render: String = changes.map(_.render).mkString("; ")

object Draft:
  def of(
      id: AnalysisRevision,
      base: AnalysisRevision,
      changes: Vector[RecipeChange]
  ): Either[DocumentError, Draft] =
    for
      _ <- Either.cond(changes.nonEmpty, (), DocumentError.NoChanges(id, base))
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
    yield new Draft(id, base, changes.sortBy(_.field.ordinal))

  /** A draft whose every change starts from `base`'s recipe. */
  def against(
      id: AnalysisRevision,
      base: AnalysisRevisionSpec,
      changes: Vector[RecipeChange]
  ): Either[DocumentError, Draft] =
    of(id, base.id, changes).flatTap(_.check(base))

  /** The draft that turns `base`'s recipe into `target`. */
  def between(
      id: AnalysisRevision,
      base: AnalysisRevisionSpec,
      target: Recipe
  ): Either[DocumentError, Draft] =
    against(id, base, RecipeChange.between(base.recipe, target))

  extension (draft: Draft)
    private[document] def check(base: AnalysisRevisionSpec): Either[DocumentError, Unit] =
      draft.changes.traverse_ { c =>
        c.mismatch(base.recipe) match
          case None       => Right(())
          case Some(held) =>
            Left(
              DocumentError.DraftBefore(draft.id, base.id, c.field, held, c.renderedValues._1)
            )
      }

  given Encoder.AsObject[Draft] =
    Encoder.forProduct3("id", "base", "changes")(d => (d.id, d.base, d.changes))
  given Decoder[Draft] =
    Decoder.forProduct3("id", "base", "changes")(of).emap(_.left.map(_.message))

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
  */
final case class AnalysisRevisionSpec(
    id: AnalysisRevision,
    dataset: DatasetRevision,
    plan: CoreBinding[StudyPlanArtifact],
    recipe: Recipe,
    studio: StudioFields
) derives CanEqual,
      Codec.AsObject
