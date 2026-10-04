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

package eyes4s.studio.app.nav

import cats.Monad
import cats.data.EitherT
import cats.syntax.all.*
import eyes4s.studio.core.backend.{PageRequest, PairDesign, Response, RunId}
import eyes4s.studio.core.document.ReportingId
import eyes4s.studio.core.navigation.{
  ChainLevel,
  MissingSource,
  NavigationError,
  ReportRef,
  StudyNavigator
}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

/** The run and scale a summary shows; the summary and group places name only
  * their reporting spec and group.
  */
final case class SummaryScope(run: RunId, scale: ScaleIndex) derives CanEqual

/** Where a provenance chain ends: a source record, or the typed reason there
  * is none (DESIGN_SPEC section 1: "result → comparison → gaze → fixation →
  * source record").
  */
enum ChainEnd derives CanEqual:
  case Record(ref: StudioRef)
  case Missing(reason: MissingSource)

/** The provenance trail model (ticket S3.4) over S1.0's [[Place]] trails.
  *
  * A trail is the path of places from a root to where the user is:
  * `Summary · by retrieval response › Remembered › P17 › ret_07 · beach-042 ›
  * pair ret_07 × enc_03 › map enc_03 › fixation 6 › fixations.csv record
  * 7,214`. It carries the active reporting spec and group as its summary and
  * group places, so drilling down keeps them.
  *
  * Up is [[parent]]: each place's container, from the place and the trail it
  * is shown in (which group a query was reached through is a fact of the
  * trail, not of the query). Down is [[children]], one step of the
  * [[StudyNavigator]] per level. [[explain]] lands on any place, keeping
  * whatever prefix of the current trail already leads to it.
  */
object Provenance:

  /** A place's level in the chain, if it is a chain step. */
  def level(place: Place): Option[ChainLevel] = place match
    case Place.Summary(_)  => Some(ChainLevel.Summary)
    case Place.Group(_, _) => Some(ChainLevel.Group)
    case Place.At(ref)     => StudyNavigator.level(ref)
    case _                 => None

  /** The reporting spec a trail carries, from its summary or group. */
  def reporting(trail: Vector[Place]): Option[ReportingId] =
    trail.reverseIterator.collectFirst {
      case Place.Summary(r)  => r
      case Place.Group(r, _) => r
    }

  /** The group a trail carries, if it went through one. */
  def group(trail: Vector[Place]): Option[Response] =
    trail.reverseIterator.collectFirst { case Place.Group(_, g) => g }

  /** The trail's studio refs, in order: its path without the report places. */
  def refs(trail: Vector[Place]): Vector[StudioRef] = trail.collect { case Place.At(r) => r }

  /** The place `place` sits under, in the context of `trail`:
    *
    *  - a group under its summary; a participant mean under its group (or the
    *    summary, when the spec does not group);
    *  - a query contrast under the trail's crumb for its participant; else
    *    under the participant's mean in the trail's spec and group; else,
    *    with no summary on the trail, under the participant;
    *  - a pair under its query contrast; a map under the trail's pair that
    *    holds it, else under its trial; a fixation under the trail's map of
    *    its trial, else the trail's pair that holds its trial (the boards'
    *    "pair › fixation 6"), else its trial; a record under its fixation;
    *  - a trial under its participant; a reduction under its contrast.
    *
    * A root (summary, participant, figure panel, or a place outside the
    * chain) has none.
    */
  def parent(place: Place, trail: Vector[Place]): Option[Place] = place match
    case Place.Group(reporting, _) => Some(Place.Summary(reporting))
    case Place.At(ref)             =>
      ref match
        case StudioRef.GroupCell(_, reporting, _, _) => Some(Place.Summary(reporting))
        case StudioRef.ParticipantSummary(_, reporting, _, group, _) =>
          Some(group.fold(Place.Summary(reporting))(Place.Group(reporting, _)))
        case StudioRef.QueryContrast(run, scale, query) =>
          val p = query.participant
          trail.reverseIterator
            .collectFirst {
              case at @ Place.At(StudioRef.ParticipantSummary(r, _, s, _, q))
                  if r == run && s == scale && q == p =>
                at
              case at @ Place.At(StudioRef.Participant(q)) if q == p => at
            }
            .orElse(
              Some(
                reporting(trail).fold(Place.At(StudioRef.Participant(p)))(r =>
                  Place.At(StudioRef.ParticipantSummary(run, r, scale, group(trail), p))
                )
              )
            )
        case StudioRef.Pair(run, scale, _, focal, _) =>
          Some(Place.At(StudioRef.QueryContrast(run, scale, focal)))
        case StudioRef.TrialMap(run, scale, key) =>
          trail.reverseIterator
            .collectFirst {
              case p @ Place.At(StudioRef.Pair(r, s, _, focal, reference))
                  if r == run && s == scale && (focal == key || reference == key) =>
                p
            }
            .orElse(Some(Place.At(StudioRef.Trial(key))))
        case StudioRef.Result(_, _) =>
          // A reduction: under its query's contrast (its structural parent).
          ref.parent.map(Place.At(_))
        case StudioRef.Fixation(trial, _) =>
          trail.reverseIterator
            .collectFirst {
              case p @ Place.At(StudioRef.TrialMap(_, _, key)) if key == trial => p
              case p @ Place.At(StudioRef.Pair(_, _, _, focal, reference))
                  if focal == trial || reference == trial =>
                p
            }
            .orElse(Some(Place.At(StudioRef.Trial(trial))))
        case StudioRef.SourceRecord(_, _, _, _) | StudioRef.Trial(_) =>
          ref.parent.map(Place.At(_))
        // A cause under the quarantined count that holds it.
        case StudioRef.InventoryCount(_, _) => ref.parent.map(Place.At(_))
        // A participant's phase lies under the participant.
        case StudioRef.TrialGroup(_, _) => ref.parent.map(Place.At(_))
        case StudioRef.Participant(_) | StudioRef.FigurePanel(_, _) |
            StudioRef.WindowTally(_, _) =>
          None
    case _ => None

  /** The trail that lands on `target` from `trail`: the longest prefix of
    * `trail` that `target` descends from (by [[parent]] in that context),
    * followed by the path down to `target`. A target already on the trail
    * truncates it there; one unrelated to it starts a new trail at its root.
    */
  def explain(trail: Vector[Place], target: Place): Vector[Place] =
    @annotation.tailrec
    def climb(at: Place, below: List[Place], steps: Int): Vector[Place] =
      val onTrail = trail.lastIndexOf(at)
      if onTrail >= 0 then trail.take(onTrail + 1) ++ below
      else
        parent(at, trail) match
          case Some(up) if steps < MaximumDepth => climb(up, at :: below, steps + 1)
          case _                                => (at :: below).toVector
    climb(target, Nil, 0)

  /** No chain is deeper than this; a guard on [[explain]]'s climb. */
  val MaximumDepth: Int = 32

  /** One step down from `place`: the `page` of its children, as places, in
    * order. The summary and group places take their run and scale from
    * `scope`. A query contrast's children are its matched pairs, then its
    * control pairs (each design paged by `page`), and a backend that holds no
    * control pairs for it (`NoPairs`) gives the matched ones alone; every
    * other refusal is surfaced. A record has none.
    */
  def children[F[_]: Monad](
      navigator: StudyNavigator[F],
      place: Place,
      scope: SummaryScope,
      page: PageRequest
  ): F[Either[NavigationError, Vector[Place]]] =
    def at(refs: Vector[StudioRef])        = refs.map(Place.At(_))
    def participants(cell: ReportRef.Cell) =
      EitherT(navigator.participants(cell, page)).map(ps =>
        ps.entries.map(p => Place.At(p.summary))
      )
    place match
      case Place.Summary(reporting) =>
        EitherT(navigator.cells(scope.run, reporting, scope.scale, page)).flatMap { cells =>
          cells.entries.flatTraverse { cell =>
            cell.group match
              case Some(g) =>
                EitherT.rightT[F, NavigationError](Vector(Place.Group(reporting, g)))
              case None => participants(cell)
          }
        }.value
      case Place.Group(reporting, g) =>
        participants(ReportRef.Cell(scope.run, reporting, scope.scale, Some(g))).value
      case Place.At(ref) =>
        (ReportRef.of(ref), StudyNavigator.level(ref)) match
          case (Some(cell: ReportRef.Cell), _)     => participants(cell).value
          case (Some(p: ReportRef.Participant), _) =>
            navigator.queries(p, page).map(_.map(q => at(q.entries)))
          case (None, Some(ChainLevel.Query)) =>
            (for
              matched <- EitherT(navigator.pairs(ref, PairDesign.Matched, page))
              control <- EitherT(navigator.pairs(ref, PairDesign.Control, page).map {
                case Left(NavigationError.NoPairs(_, _)) => Right(Vector.empty)
                case other                               => other.map(_.entries)
              })
            yield at(matched.entries ++ control)).value
          case (None, Some(ChainLevel.Pair)) => navigator.maps(ref).map(_.map(m => at(m.both)))
          case (None, Some(ChainLevel.Map))  =>
            navigator.fixations(ref, page).map(_.map(f => at(f.entries)))
          case (None, Some(ChainLevel.Fixation)) =>
            navigator.record(ref).map(_.map(r => Vector(Place.At(r))))
          case _ => Monad[F].pure(Right(Vector.empty))
      case _ => Monad[F].pure(Right(Vector.empty))

  /** Where a fixation's chain ends: its record, or the typed reason it has
    * none. Any other refusal stays an error.
    */
  def end[F[_]: Monad](
      navigator: StudyNavigator[F],
      fixation: StudioRef
  ): F[Either[NavigationError, ChainEnd]] =
    navigator.record(fixation).map {
      case Right(record)                           => Right(ChainEnd.Record(record))
      case Left(NavigationError.Source(_, reason)) => Right(ChainEnd.Missing(reason))
      case Left(other)                             => Left(other)
    }
