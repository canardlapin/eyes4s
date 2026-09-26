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
import eyes4s.compare.MeasureScale
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*

/** A typed address of one inspectable result item. References are built from
  * scale indices, designs, trial keys, windows and sample ranges; never from a
  * row position, a display label or a digest. Items of a temporal cell are
  * addressed [[ResultRef.InCell]], so a reference from one cell never
  * resolves in another.
  */
enum ResultRef[+K] derives CanEqual:
  case Estimation(scale: Int, key: K)
  case PairRow(scale: Int, design: StudyDesign, focal: K, reference: K)
  case Reduction(scale: Int, design: StudyDesign, key: K)
  case ContrastRow(scale: Int, key: K)
  case Occupancy(repetition: String, window: String, key: K)
  case Event(from: Int, until: Int)
  case InCell(repetition: String, window: String, ref: ResultRef[K])

  /** The same address as a diagnostic subject path. */
  def loci: Vector[Locus[K]] = this match
    case Estimation(scale, key) => Vector(Locus.Scale(scale), Locus.Trial(key))
    case PairRow(scale, design, focal, reference) =>
      Vector(Locus.Scale(scale), Locus.Design(design), Locus.Pair(focal, reference))
    case Reduction(scale, design, key) =>
      Vector(Locus.Scale(scale), Locus.Design(design), Locus.Trial(key))
    case ContrastRow(scale, key)            => Vector(Locus.Scale(scale), Locus.Trial(key))
    case Occupancy(repetition, window, key) =>
      Vector(Locus.Repetition(repetition), Locus.Window(window), Locus.Trial(key))
    case Event(from, until)              => Vector(Locus.Samples(from, until))
    case InCell(repetition, window, ref) =>
      Vector(Locus.Repetition(repetition), Locus.Window(window)) ++ ref.loci

/** Refusals while building or navigating a result inspection. */
enum InspectionError[+K] derives CanEqual:
  case UnknownScale(index: Int, scales: Int)
  case UnknownCell(repetition: String, window: String)
  case UnknownReference(reference: ResultRef[K])
  case DuplicateReference(reference: ResultRef[K])
  case InvalidPageSize(requested: Int, maximum: Int)
  case Sources(refusal: LedgerRefusal[K])
  case InputMismatch(result: ArtifactRef[?], sources: ArtifactRef[?])
  case Components(underlying: DescriptorError)
  case PlanMismatch(changes: Vector[PlanChange])
  case ReductionMembership(
      reference: ResultRef[K],
      selected: Int,
      members: Int,
      contributing: Int,
      contributors: Int
  )
  case Orientation(scale: Int, design: StudyDesign, found: ReductionOrientation)
  case NoContrast(scale: Int)

  def message: String = this match
    case UnknownScale(index, scales) => s"Result has $scales scales; there is no scale $index."
    case UnknownCell(repetition, window) =>
      s"Temporal result has no cell for repetition '$repetition' and window '$window'."
    case UnknownReference(reference)   => s"No result item has reference $reference."
    case DuplicateReference(reference) =>
      s"Reference $reference addresses more than one result item; it cannot identify one."
    case InvalidPageSize(requested, maximum) =>
      s"Page size must be between 1 and $maximum, got $requested."
    case Sources(refusal)               => refusal.error.message
    case InputMismatch(result, sources) =>
      s"Result was computed on input ${result.digest}; the source index describes input ${sources.digest}."
    case Components(e)         => e.message
    case PlanMismatch(changes) =>
      s"Result was computed by another plan; fields ${changes.map(_.field)} differ."
    case ReductionMembership(reference, selected, members, contributing, contributors) =>
      s"Reduction $reference stores selected=$selected and contributing=$contributing, " +
        s"but its pair rows give $members members and $contributors contributors."
    case Orientation(scale, design, found) =>
      s"Scale $scale $design reduction is $found; study results reduce by the focal key."
    case NoContrast(scale) => s"Scale $scale has no contrast rows; its contrast was refused."

/** A bounded page length. */
final case class PageSize private (value: Int) derives CanEqual
object PageSize:
  val maximum: Int                                               = 4096
  def of(value: Int): Either[InspectionError[Nothing], PageSize] =
    Either.cond(
      value >= 1 && value <= maximum,
      new PageSize(value),
      InspectionError.InvalidPageSize(value, maximum)
    )

/** One page: at most the requested number of entries, and the reference the
  * next page starts at, if any.
  */
final case class Page[K, A](entries: Vector[A], next: Option[ResultRef[K]])

/** An immutable, keyed view in the result's own order. Entries are reached by
  * typed reference, and pages start at a reference rather than a row number.
  */
final class Listing[K, A] private (
    entries: Vector[A],
    positions: Map[ResultRef[K], Int],
    reference: A => ResultRef[K]
):
  def size: Int                            = entries.size
  def isEmpty: Boolean                     = entries.isEmpty
  def contains(ref: ResultRef[K]): Boolean = positions.contains(ref)
  def get(ref: ResultRef[K]): Option[A]    = positions.get(ref).map(entries)

  private[plan] def inOrder: Vector[A] = entries

  /** The first page. */
  def first(size: PageSize): Page[K, A] = at(0, size)

  /** The page that starts at `from`. */
  def page(from: ResultRef[K], size: PageSize): Either[InspectionError[K], Page[K, A]] =
    positions.get(from).toRight(InspectionError.UnknownReference(from)).map(at(_, size))

  private def at(start: Int, size: PageSize): Page[K, A] =
    val end = math.min(entries.size, start + size.value)
    Page(entries.slice(start, end), entries.lift(end).map(reference))

object Listing:
  /** Refuses a reference that addresses more than one entry. */
  private[plan] def of[K, A](entries: Vector[A])(
      reference: A => ResultRef[K]
  ): Either[InspectionError[K], Listing[K, A]] =
    entries.zipWithIndex
      .foldLeft[Either[InspectionError[K], Map[ResultRef[K], Int]]](Right(Map.empty)) {
        case (Right(seen), (entry, index)) =>
          val ref = reference(entry)
          if seen.contains(ref) then Left(InspectionError.DuplicateReference(ref))
          else Right(seen.updated(ref, index))
        case (failed, _) => failed
      }
      .map(new Listing(entries, _, reference))

/** One named score or difference component, read from the typed value by the
  * method's descriptor. No component is computed here. Values compare by bit
  * pattern, as [[ExactDouble]] does.
  */
final case class ComponentValue(
    id: String,
    units: ParameterUnits,
    range: MeasureScale,
    direction: ScoreDirection,
    value: Double
) derives CanEqual:
  override def equals(other: Any): Boolean = other match
    case that: ComponentValue =>
      id == that.id && units == that.units && range == that.range &&
      direction == that.direction && ExactDouble(value) == ExactDouble(that.value)
    case _ => false
  override def hashCode: Int = (id, units, range, direction, ExactDouble(value)).hashCode

/** A typed score (or difference) with its named components. Empty components
  * mean the method is undescribed, never that a component is zero.
  */
final case class ScoreView[A](value: A, components: Vector[ComponentValue])

/** The named components of a method's scores and differences. An inspection
  * refuses a schema whose component ids differ from those the result's
  * evaluation specification stores.
  */
final class ScoreSchema[S, D] private (val components: Vector[ScoreComponent[S, D]]):
  def ids: Vector[String]           = components.map(_.id)
  def score(value: S): ScoreView[S] = ScoreView(
    value,
    components.map(c => ComponentValue(c.id, c.units, c.range, c.direction, c.score(value)))
  )
  def difference(value: D): ScoreView[D] = ScoreView(
    value,
    components.map(c =>
      ComponentValue(c.id, c.units, c.range, c.direction, c.difference(value))
    )
  )

object ScoreSchema:
  def of[S, D](components: Vector[ScoreComponent[S, D]]): ScoreSchema[S, D] =
    new ScoreSchema(components)
  def undescribed[S, D]: ScoreSchema[S, D] = new ScoreSchema(Vector.empty)

  /** The plan method's described components, which must name the method's
    * contrast components in order; an undescribed method has none.
    */
  def study[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D]
  ): Either[DescriptorError, ScoreSchema[S, D]] =
    plan.method.descriptor match
      case None             => Right(undescribed)
      case Some(descriptor) =>
        descriptor.components(plan.parameters).flatMap { components =>
          val ids = components.map(_.id)
          Either.cond(
            ids == plan.method.difference.components,
            of(components),
            DescriptorError.ComponentMismatch(ids, plan.method.difference.components)
          )
        }

/** An estimated density on its grid. `cells` returns a fresh copy of the cell
  * masses in grid order, so nothing a consumer does to it reaches the result.
  */
final class DensityView[U <: Unit2D] private[plan] (mass: Mass[U]):
  def frame: FrameId         = mass.grid.frame.id
  def frameSpec: FrameSpec   = mass.grid.frame.spec
  def grid: GridId           = mass.grid.id
  def nx: Int                = mass.grid.nx
  def ny: Int                = mass.grid.ny
  def provenance: Provenance = mass.provenance
  def cells: IArray[Double]  = IArray.from(mass.values)

  override def equals(other: Any): Boolean = other match
    case that: DensityView[?] =>
      frame == that.frame && frameSpec == that.frameSpec && grid == that.grid &&
      nx == that.nx && ny == that.ny && provenance == that.provenance &&
      cells.map(ExactDouble(_)).sameElements(that.cells.map(ExactDouble(_)))
    case _ => false
  override def hashCode: Int    = (frame, grid, nx, ny, provenance).hashCode
  override def toString: String = s"DensityView($grid, ${nx}x$ny)"

enum EstimationOutcome[+K, U <: Unit2D]:
  case Estimated(density: DensityView[U])
  case Failed(diagnostic: Diagnostic[K])

/** One trial's estimation at one scale. More than one outcome means the input
  * repeats this full key; pairing excluded every occurrence.
  */
final case class EstimationEntry[K, U <: Unit2D](
    ref: ResultRef[K],
    key: K,
    outcomes: Vector[EstimationOutcome[K, U]]
):
  def ambiguous: Boolean = outcomes.size > 1

/** One evaluated directed pair; both keys survive success or failure. */
final case class PairEntry[K, S](
    ref: ResultRef[K],
    design: StudyDesign,
    focal: K,
    reference: K,
    outcome: Either[Diagnostic[K], ScoreView[S]]
)

/** Why one pair does or does not contribute to its focal key's reduction. */
enum Membership[+K] derives CanEqual:
  case Contributing

  /** The pair's own evaluation failed, so it contributes nothing. */
  case FailedPair(diagnostic: Diagnostic[K])

  /** The pair succeeded, but its focal key's reduction failed as a whole. */
  case Withheld(reduction: Diagnostic[K])

/** A selected pair of a reduced key: the pair reference, the contributing
  * observation's key, and its membership.
  */
final case class Member[K](pair: ResultRef[K], reference: K, status: Membership[K])

/** One focal key's reduction. The aggregate target (`key`) stays distinct from
  * the contributing observations (`members`). The denominators are the stored
  * ones; the members are the stored pair rows regrouped by focal key, and an
  * inspection is refused unless they agree with those denominators.
  */
final case class ReductionEntry[K, S](
    ref: ResultRef[K],
    design: StudyDesign,
    key: K,
    outcome: Either[Diagnostic[K], ScoreView[S]],
    selected: Int,
    successful: Int,
    failed: Int,
    contributing: Int,
    members: Vector[Member[K]]
):
  /** Reference trials whose pairs contribute, in pair order. */
  def contributors: Vector[K] = members.collect {
    case Member(_, reference, Membership.Contributing) => reference
  }

  /** Selected pairs that do not contribute, each with its reason. */
  def exclusions: Vector[Member[K]] = members.filter(_.status != Membership.Contributing)

/** One contrast row: references to the two reductions it subtracts (absent
  * when an operand is missing) and the difference or its failure.
  */
final case class ContrastEntry[K, D](
    ref: ResultRef[K],
    key: K,
    matched: Option[ResultRef[K]],
    control: Option[ResultRef[K]],
    outcome: Either[Diagnostic[K], ScoreView[D]]
)

enum ScaleContrast[K, D]:
  /** The scale's contrast was refused as a whole. */
  case Failed(diagnostic: Diagnostic[K])
  case Rows(rows: Listing[K, ContrastEntry[K, D]])

/** One scale of a study result. */
final class ScaleInspection[K, U <: Unit2D, S, D] private[plan] (
    val index: Int,
    val estimate: StudyEstimate[U],
    val estimation: Listing[K, EstimationEntry[K, U]],
    val excludedPhases: Vector[K],
    matchedPairs: Listing[K, PairEntry[K, S]],
    controlPairs: Listing[K, PairEntry[K, S]],
    matchedReductions: Listing[K, ReductionEntry[K, S]],
    controlReductions: Listing[K, ReductionEntry[K, S]],
    matchedSource: DirectedPairwiseAnalysis[K, K, StudyFailure[K], S],
    controlSource: DirectedPairwiseAnalysis[K, K, StudyFailure[K], S],
    matchedReduced: Analysis[K, S],
    controlReduced: Analysis[K, S],
    val contrast: ScaleContrast[K, D]
):
  def pairs(design: StudyDesign): Listing[K, PairEntry[K, S]] = design match
    case StudyDesign.Matched => matchedPairs
    case StudyDesign.Control => controlPairs
  def reductions(design: StudyDesign): Listing[K, ReductionEntry[K, S]] = design match
    case StudyDesign.Matched => matchedReductions
    case StudyDesign.Control => controlReductions

  /** Pairing accounting as the result stores it: eligible and selected counts,
    * unmatched keys and duplicate-key ambiguities.
    */
  def pairing(design: StudyDesign): PairingReport[K, K] = design match
    case StudyDesign.Matched => matchedSource.diagnostics
    case StudyDesign.Control => controlSource.diagnostics

  /** Reduction accounting as the result stores it. */
  def report(design: StudyDesign): ReductionReport[K] = design match
    case StudyDesign.Matched => matchedReduced.diagnostics
    case StudyDesign.Control => controlReduced.diagnostics

/** A navigable, renderer-neutral view of one completed study result and the
  * sources of its input. Nothing is recomputed: every value is read from the
  * result, and every failure is a located, source-linked diagnostic. `cell`
  * names the temporal cell this study belongs to, if any; its references are
  * then [[ResultRef.InCell]].
  */
final class StudyInspection[K, U <: Unit2D, S, D] private[plan] (
    val input: ArtifactRef[StudyInput[K, U]],
    val description: Vector[(String, Vector[Provenance.Param])],
    val sources: StudySources[K],
    val scales: Vector[ScaleInspection[K, U, S, D]],
    val cell: Option[(String, String)],
    val windowTallies: Vector[(K, Either[GeometryError, WindowTally])] = Vector.empty,
    val initialFixationTallies: Vector[(K, Either[GeometryError, InitialFixationTally])] =
      Vector.empty
):
  /** One trial's initial fixations the plan's policy dropped, when the
    * inspection was opened with its plan and input.
    */
  def initialFixationTally(key: K): Option[InitialFixationTally] =
    initialFixationTallies.collectFirst { case (`key`, Right(tally)) => tally }

  /** One trial's fixations outside the analysis window and the screen, when
    * the inspection was opened with its plan and input.
    */
  def windowTally(key: K): Option[WindowTally] = windowTallies.collectFirst {
    case (`key`, Right(tally)) => tally
  }

  def scale(index: Int): Either[InspectionError[K], ScaleInspection[K, U, S, D]] =
    scales.lift(index).toRight(InspectionError.UnknownScale(index, scales.size))

  def estimation(ref: ResultRef[K]): Either[InspectionError[K], EstimationEntry[K, U]] =
    local(ref) match
      case Some(ResultRef.Estimation(index, _)) =>
        scale(index).flatMap(found(ref, _.estimation))
      case _ => Left(InspectionError.UnknownReference(ref))

  def pair(ref: ResultRef[K]): Either[InspectionError[K], PairEntry[K, S]] = local(ref) match
    case Some(ResultRef.PairRow(index, design, _, _)) =>
      scale(index).flatMap(found(ref, _.pairs(design)))
    case _ => Left(InspectionError.UnknownReference(ref))

  def reduction(ref: ResultRef[K]): Either[InspectionError[K], ReductionEntry[K, S]] =
    local(ref) match
      case Some(ResultRef.Reduction(index, design, _)) =>
        scale(index).flatMap(found(ref, _.reductions(design)))
      case _ => Left(InspectionError.UnknownReference(ref))

  def contrastRow(ref: ResultRef[K]): Either[InspectionError[K], ContrastEntry[K, D]] =
    local(ref) match
      case Some(ResultRef.ContrastRow(index, _)) =>
        scale(index).flatMap(_.contrast match
          case ScaleContrast.Rows(rows) =>
            rows.get(ref).toRight(InspectionError.UnknownReference(ref))
          case ScaleContrast.Failed(_) => Left(InspectionError.NoContrast(index)))
      case _ => Left(InspectionError.UnknownReference(ref))

  /** The source evidence of one trial. */
  def trial(key: K): Either[MissingSource[K], TrialSources[K]] = sources.trial(key)

  /** Every failure the result records, per scale: estimation failures, then
    * for the matched and then the control design its pair failures followed
    * by its reduction failures, then the contrast's. Each is located and linked.
    */
  lazy val failures: Vector[Diagnostic[K]] = scales.flatMap { scale =>
    val estimation = scale.estimation.inOrder.flatMap(_.outcomes.collect {
      case EstimationOutcome.Failed(d) => d
    })
    val designs = StudyDesign.values.toVector.flatMap { design =>
      scale.pairs(design).inOrder.flatMap(_.outcome.left.toOption) ++
        scale.reductions(design).inOrder.flatMap(_.outcome.left.toOption)
    }
    val contrast = scale.contrast match
      case ScaleContrast.Failed(d)  => Vector(d)
      case ScaleContrast.Rows(rows) => rows.inOrder.flatMap(_.outcome.left.toOption)
    estimation ++ designs ++ contrast
  }

  /** The cell-relative address, when `ref` belongs to this study. */
  private def local(ref: ResultRef[K]): Option[ResultRef[K]] = (cell, ref) match
    case (Some((repetition, window)), ResultRef.InCell(r, w, inner))
        if r == repetition && w == window =>
      Some(inner)
    case (None, ResultRef.InCell(_, _, _)) => None
    case (None, other)                     => Some(other)
    case _                                 => None

  private def found[A](
      ref: ResultRef[K],
      listing: ScaleInspection[K, U, S, D] => Listing[K, A]
  )(scale: ScaleInspection[K, U, S, D]): Either[InspectionError[K], A] =
    listing(scale).get(ref).toRight(InspectionError.UnknownReference(ref))

/** Window occupancy of one trial as the temporal result stores it. Fixation
  * indices address the trial's scanpath, so each links to its source.
  */
final case class OccupancyView(
    interval: Interval,
    boundary: FixationBoundary,
    observedMicros: Long,
    missingMicros: Long,
    fixations: Vector[FixationWindowTime]
) derives CanEqual

final case class OccupancyEntry[K](
    ref: ResultRef[K],
    key: K,
    outcome: Either[Diagnostic[K], OccupancyView]
)

/** One repetition-by-window cell: occupancy per trial and the cell's study,
  * whose references are addressed to this cell.
  */
final class TemporalCellInspection[K, U <: Unit2D, S, D] private[plan] (
    val repetition: String,
    val window: String,
    val occupancy: Listing[K, OccupancyEntry[K]],
    val study: StudyInspection[K, U, S, D]
)

/** A temporal result addressed by repetition and window names. */
final class TemporalInspection[K, U <: Unit2D, S, D] private[plan] (
    val description: Vector[(String, Vector[Provenance.Param])],
    cells: Vector[TemporalCellInspection[K, U, S, D]]
):
  /** Repetition and window names of every cell, in plan order. */
  def cellNames: Vector[(String, String)] = cells.map(c => c.repetition -> c.window)

  def cell(
      repetition: String,
      window: String
  ): Either[InspectionError[K], TemporalCellInspection[K, U, S, D]] =
    cells
      .find(c => c.repetition == repetition && c.window == window)
      .toRight(InspectionError.UnknownCell(repetition, window))

  /** The cell a reference addresses: its occupancy or its study's items. */
  def cellOf(
      ref: ResultRef[K]
  ): Either[InspectionError[K], TemporalCellInspection[K, U, S, D]] = ref match
    case ResultRef.InCell(repetition, window, _)    => cell(repetition, window)
    case ResultRef.Occupancy(repetition, window, _) => cell(repetition, window)
    case other => Left(InspectionError.UnknownReference(other))

/** The kind of a detected event. */
enum EventKind derives CanEqual:
  case Fixation, Saccade, Blink, Pursuit

/** One detected event, addressed by its source sample range. */
final case class EventEntry(
    ref: ResultRef[Nothing],
    kind: EventKind,
    span: Interval,
    source: SourceLink[Nothing]
) derives CanEqual

/** Detected events of a recording analysis. Sample ranges address the input
  * recording: preprocessing keeps its sample count and order.
  */
final class RecordingInspection private[plan] (
    val description: Vector[(String, Vector[Provenance.Param])],
    val events: Listing[Nothing, EventEntry]
)

object ResultInspection:
  /** Inspect a study result over the sources of the input it was computed on. */
  def study[K, U <: Unit2D, S, D](
      result: StudyResult[K, U, S, D],
      sources: StudySources[K],
      schema: ScoreSchema[S, D]
  ): Either[InspectionError[K], StudyInspection[K, U, S, D]] =
    inspectStudy(result, sources, schema, None)

  /** Inspect a study result with the plan's described components and the
    * input's ledger, when it has one. The result must have been computed by
    * this plan.
    */
  def study[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      result: StudyResult[K, U, S, D],
      input: StudyInput[K, U],
      ledger: Option[AdmissionLedger[K]]
  ): Either[InspectionError[K], StudyInspection[K, U, S, D]] =
    given KeyDigest[K] = plan.layout.digest
    for
      _ <- Either.cond(
        result.description == plan.description,
        (),
        InspectionError.PlanMismatch(PlanChange.between(plan.description, result.description))
      )
      sources <- ledger.fold(Right(StudySources.unledgered(input)))(l =>
        StudySources.of(input, l).left.map(InspectionError.Sources.apply)
      )
      schema <- ScoreSchema.study(plan).left.map(InspectionError.Components.apply)
      found  <- study(result, sources, schema)
    yield new StudyInspection(
      found.input,
      found.description,
      found.sources,
      found.scales,
      found.cell,
      plan.windowTallies(input),
      plan.initialFixationTallies(input)
    )

  /** Inspect every cell of a temporal result; cell studies share the base
    * input's sources, and every item and diagnostic is addressed to its cell.
    */
  def temporal[K, U <: Unit2D, P, S, D](
      result: TemporalStudyResult[K, U, P, S, D],
      sources: StudySources[K],
      schema: ScoreSchema[S, D]
  ): Either[InspectionError[K], TemporalInspection[K, U, S, D]] =
    result.cells
      .traverse { cell =>
        val rep    = cell.repetition.name
        val window = cell.window.name
        for
          occupancy <- Listing.of(cell.occupancy.map { case (key, outcome) =>
            val ref = ResultRef.Occupancy(rep, window, key)
            OccupancyEntry(
              ref,
              key,
              outcome
                .map(o =>
                  OccupancyView(
                    o.interval,
                    o.boundary,
                    o.observedMicros,
                    o.missingMicros,
                    o.fixationTimes
                  )
                )
                .left
                .map(e => located(ref.loci, Diagnostics.temporal(e), sources))
            )
          })(_.ref)
          inspected <- inspectStudy(cell.result, sources, schema, Some(rep -> window))
        yield new TemporalCellInspection(rep, window, occupancy, inspected)
      }
      .map(new TemporalInspection(result.description, _))

  /** Inspect the detected events of a recording analysis run by this plan.
    * Each event links to the samples of the plan's input recording.
    */
  def recording[P](
      plan: RecordingPlan[P],
      analysis: RecordingAnalysis[P]
  ): Either[InspectionError[Nothing], RecordingInspection] =
    val series = analysis.detection.eventSeries
    for
      _ <- Either.cond(
        analysis.description == plan.description,
        (),
        InspectionError.PlanMismatch(
          PlanChange.between(plan.description, analysis.description)
        )
      )
      events <- Listing.of(series.events.zip(series.support).map { case (event, range) =>
        val kind = event match
          case _: Event.Fixation[?] => EventKind.Fixation
          case _: Event.Saccade[?]  => EventKind.Saccade
          case _: Event.Blink[?]    => EventKind.Blink
          case _: Event.Pursuit[?]  => EventKind.Pursuit
        EventEntry(
          ResultRef.Event(range.from, range.until),
          kind,
          event.span,
          SourceLink.Samples(plan.source, plan.input, range.from, range.until)
        )
      })(_.ref)
    yield new RecordingInspection(analysis.description, events)

  /** A diagnostic placed under a result address and linked to its sources.
    * Loci the diagnostic already names are not repeated.
    */
  private def located[K](
      context: Vector[Locus[K]],
      diagnostic: Diagnostic[K],
      sources: StudySources[K]
  ): Diagnostic[K] =
    sources.link(
      diagnostic.copy(subject = context ++ diagnostic.subject.filterNot(context.contains))
    )

  private def inspectStudy[K, U <: Unit2D, S, D](
      result: StudyResult[K, U, S, D],
      sources: StudySources[K],
      schema: ScoreSchema[S, D],
      cell: Option[(String, String)]
  ): Either[InspectionError[K], StudyInspection[K, U, S, D]] =
    for
      _ <- Either.cond(
        sources.input.digest == result.input.digest,
        (),
        InspectionError.InputMismatch(result.input, sources.input)
      )
      scales <- result.scales.zipWithIndex.traverse { case (scale, index) =>
        inspectScale(scale, index, sources, schema, cell)
      }
    yield new StudyInspection(result.input, result.description, sources, scales, cell)

  private def inspectScale[K, U <: Unit2D, S, D](
      scale: StudyScaleResult[K, U, S, D],
      index: Int,
      sources: StudySources[K],
      schema: ScoreSchema[S, D],
      cell: Option[(String, String)]
  ): Either[InspectionError[K], ScaleInspection[K, U, S, D]] =
    def address(ref: ResultRef[K]): ResultRef[K] =
      cell.fold(ref)((repetition, window) => ResultRef.InCell(repetition, window, ref))
    def place(ref: ResultRef[K], diagnostic: Diagnostic[K]) =
      located(address(ref).loci, diagnostic, sources)
    val outcomes   = scale.estimation.groupMap(_._1)(_._2)
    val estimation = scale.estimation.map(_._1).distinct.map { key =>
      val ref = ResultRef.Estimation(index, key)
      EstimationEntry(
        address(ref),
        key,
        outcomes(key).map(
          _.fold(
            f => EstimationOutcome.Failed(place(ref, Diagnostics.failure(f))),
            mass => EstimationOutcome.Estimated(new DensityView(mass))
          )
        )
      )
    }
    def checked(design: StudyDesign): Either[InspectionError[K], Unit] =
      val orientation = scale.analyses.reduced(design).diagnostics.orientation
      val stored      =
        scale.analyses.source(design).evaluation.specification.fold(Vector.empty)(_.components)
      if orientation != ReductionOrientation.ByLeft then
        Left(InspectionError.Orientation(index, design, orientation))
      else if schema.components.nonEmpty && schema.ids != stored then
        Left(InspectionError.Components(DescriptorError.ComponentMismatch(schema.ids, stored)))
      else Right(())
    def pairs(design: StudyDesign): Vector[PairEntry[K, S]] =
      scale.analyses.source(design).rows.map { row =>
        val ref = ResultRef.PairRow(index, design, row.left, row.right)
        PairEntry(
          address(ref),
          design,
          row.left,
          row.right,
          row.result.left.map(f => place(ref, Diagnostics.failure(f))).map(schema.score)
        )
      }
    def reductions(
        design: StudyDesign,
        pairs: Vector[PairEntry[K, S]]
    ): Either[InspectionError[K], Vector[ReductionEntry[K, S]]] =
      val byFocal = pairs.groupBy(_.focal)
      scale.analyses.reduced(design).entries.traverse { row =>
        val ref     = ResultRef.Reduction(index, design, row.key)
        val outcome =
          row.result.left.map(e => place(ref, Diagnostics.reduction(e))).map(schema.score)
        val members = byFocal.getOrElse(row.key, Vector.empty).map { pair =>
          val status = (pair.outcome, outcome) match
            case (Left(failure), _)    => Membership.FailedPair(failure)
            case (Right(_), Left(why)) => Membership.Withheld(why)
            case (Right(_), Right(_))  => Membership.Contributing
          Member(pair.ref, pair.reference, status)
        }
        val entry = ReductionEntry(
          address(ref),
          design,
          row.key,
          outcome,
          row.selected,
          row.successful,
          row.failed,
          row.contributing,
          members
        )
        Either.cond(
          members.size == row.selected && entry.contributors.size == row.contributing,
          entry,
          InspectionError.ReductionMembership(
            entry.ref,
            row.selected,
            members.size,
            row.contributing,
            entry.contributors.size
          )
        )
      }
    val contrast: Either[InspectionError[K], ScaleContrast[K, D]] = scale.contrast match
      case Left(error) =>
        Right(
          ScaleContrast.Failed(
            located(
              cell.toVector.flatMap((r, w) => Vector(Locus.Repetition(r), Locus.Window(w))) :+
                Locus.Scale(index),
              Diagnostics.contrast(error),
              sources
            )
          )
        )
      case Right(value) =>
        Listing
          .of(value.rows.map { row =>
            val ref = ResultRef.ContrastRow(index, row.key)
            ContrastEntry(
              address(ref),
              row.key,
              row.matched
                .map(r => address(ResultRef.Reduction(index, StudyDesign.Matched, r.key))),
              row.control
                .map(r => address(ResultRef.Reduction(index, StudyDesign.Control, r.key))),
              row.difference.left
                .map(e => place(ref, Diagnostics.contrastRow(e)))
                .map(schema.difference)
            )
          })(_.ref)
          .map(ScaleContrast.Rows(_))
    val matchedPairs = pairs(StudyDesign.Matched)
    val controlPairs = pairs(StudyDesign.Control)
    for
      _                 <- checked(StudyDesign.Matched)
      _                 <- checked(StudyDesign.Control)
      estimationListing <- Listing.of(estimation)(_.ref)
      matchedListing    <- Listing.of(matchedPairs)(_.ref)
      controlListing    <- Listing.of(controlPairs)(_.ref)
      matchedRows       <- reductions(StudyDesign.Matched, matchedPairs)
      controlRows       <- reductions(StudyDesign.Control, controlPairs)
      matchedReduced    <- Listing.of(matchedRows)(_.ref)
      controlReduced    <- Listing.of(controlRows)(_.ref)
      contrastView      <- contrast
    yield new ScaleInspection(
      index,
      scale.estimate,
      estimationListing,
      scale.excludedPhases,
      matchedListing,
      controlListing,
      matchedReduced,
      controlReduced,
      scale.analyses.matchedSource,
      scale.analyses.controlSource,
      scale.analyses.matched,
      scale.analyses.control,
      contrastView
    )
