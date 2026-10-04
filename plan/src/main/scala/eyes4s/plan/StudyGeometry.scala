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
import eyes4s.kernel.*

/** What a study does with fixations whose centre lies on the admission frame
  * but outside its analysis window. Either way they are counted and reported
  * per trial; a trial with no fixation inside the window always fails with
  * [[StudyFailure.OffWindow]], because its map would be empty.
  */
enum OffWindowPolicy derives CanEqual:
  /** Leave outside fixations out of the trial's map; the trial still runs. */
  case Exclude

  /** Fail the whole trial when any fixation lies outside the window. */
  case FailTrial

/** How one trial's fixations fall against the admission frame (the screen)
  * and the plan's analysis window, by count and by duration. Only a
  * fixation's centre decides. The three classes partition the trial:
  *
  *  - `outsideScreen`: the centre lies outside the admission frame, which an
  *    admission under [[OffScreenPolicy.ExcludeRecord]] admits and reports;
  *  - `outsideWindow`: on the admission frame but outside the half-open
  *    window (always zero for a whole-frame plan);
  *  - `inside`: the rest, which the trial's map is built from.
  *
  * Tallies are derived from the input and the plan by [[WindowTally.screen]]
  * and [[WindowTally.window]]; duration shares are of the trial's total
  * fixation duration.
  */
final case class WindowTally private (
    outsideScreen: Int,
    outsideWindow: Int,
    total: Int,
    outsideScreenDuration: Span,
    outsideWindowDuration: Span,
    totalDuration: Span
) derives CanEqual:
  /** Fixations left out of the map, for either reason. */
  def outside: Int          = outsideScreen + outsideWindow
  def outsideDuration: Span = outsideScreenDuration + outsideWindowDuration
  def inside: Int           = total - outside
  def insideDuration: Span  = totalDuration - outsideDuration
  def allOutside: Boolean   = inside == 0
  def anyOutside: Boolean   = outside > 0

  private def share(part: Span): Option[Double] =
    if totalDuration.toMicros <= 0L then None
    else Some(part.toMicros.toDouble / totalDuration.toMicros.toDouble)

  /** The outside-window share of total fixation duration; absent when the
    * trial's fixations have no duration at all.
    */
  def outsideWindowShare: Option[Double] = share(outsideWindowDuration)

  /** The outside-screen share of total fixation duration. */
  def outsideScreenShare: Option[Double] = share(outsideScreenDuration)

  def render: String =
    s"outside window $outsideWindow of $total (${outsideWindowDuration.render} of " +
      s"${totalDuration.render}); outside screen $outsideScreen of $total " +
      s"(${outsideScreenDuration.render})"

object WindowTally:
  /** Rebuild a stored tally; the counts and durations must partition the trial. */
  def of(
      outsideScreen: Int,
      outsideWindow: Int,
      total: Int,
      outsideScreenDuration: Span,
      outsideWindowDuration: Span,
      totalDuration: Span
  ): Either[PlanError, WindowTally] =
    val counts =
      outsideScreen >= 0 && outsideWindow >= 0 &&
        BigInt(outsideScreen) + outsideWindow <= total
    val durations =
      !outsideScreenDuration.isNegative && !outsideWindowDuration.isNegative &&
        BigInt(outsideScreenDuration.toMicros) + outsideWindowDuration.toMicros <=
        totalDuration.toMicros
    if counts && durations then
      Right(
        new WindowTally(
          outsideScreen,
          outsideWindow,
          total,
          outsideScreenDuration,
          outsideWindowDuration,
          totalDuration
        )
      )
    else
      Left(
        PlanError.InvalidWindowTally(
          outsideScreen,
          outsideWindow,
          total,
          outsideScreenDuration.toMicros,
          outsideWindowDuration.toMicros,
          totalDuration.toMicros
        )
      )

  /** The tally of a trial with no fixation left to count. */
  private[plan] val none: WindowTally =
    new WindowTally(0, 0, 0, Span.zero, Span.zero, Span.zero)

  /** Count a scanpath's fixations against its admission frame alone. */
  def screen[U <: Unit2D](
      frame: Frame[U],
      path: Scanpath[U]
  ): Either[GeometryError, WindowTally] =
    Agreement.frames(frame, path.frame).map(_ => tally(path, p => frame.contains(p), _ => true))

  /** Count a scanpath's fixations against a window of its admission frame. */
  def window[U <: Unit2D](
      window: Subframe[U],
      path: Scanpath[U]
  ): Either[GeometryError, WindowTally] =
    Agreement
      .frames(window.parent, path.frame)
      .map(_ => tally(path, p => window.parent.contains(p), p => window.locate(p).isInside))

  private def tally[U <: Unit2D](
      path: Scanpath[U],
      onScreen: Pt[U] => Boolean,
      inWindow: Pt[U] => Boolean
  ): WindowTally =
    var screen         = 0
    var window         = 0
    var screenDuration = Span.zero
    var windowDuration = Span.zero
    var totalDuration  = Span.zero
    var i              = 0
    while i < path.n do
      val fixation = path.fixations(i)
      totalDuration = totalDuration + fixation.duration
      if !onScreen(fixation.centre) then
        screen += 1
        screenDuration = screenDuration + fixation.duration
      else if !inWindow(fixation.centre) then
        window += 1
        windowDuration = windowDuration + fixation.duration
      i += 1
    new WindowTally(screen, window, path.n, screenDuration, windowDuration, totalDuration)

/** Totals over a study's per-trial tallies: fixation records outside the
  * window and outside the screen, all tallied fixations, the trials with any
  * of each, the trials tallied, and the trials not tallied because they are
  * in another frame. `sourceRecords`, when the admission ledger is supplied,
  * is the importer's count of source records, so an application can say
  * "543 of 11,520 records".
  */
final case class WindowSummary(
    outsideWindow: Int,
    outsideScreen: Int,
    total: Int,
    trialsOutsideWindow: Int,
    trialsOutsideScreen: Int,
    trials: Int,
    untallied: Int,
    sourceRecords: Option[Int]
) derives CanEqual

object WindowSummary:
  def of[K](tallies: Vector[(K, Either[GeometryError, WindowTally])]): WindowSummary =
    val counted = tallies.collect { case (_, Right(t)) => t }
    WindowSummary(
      counted.map(_.outsideWindow).sum,
      counted.map(_.outsideScreen).sum,
      counted.map(_.total).sum,
      counted.count(_.outsideWindow > 0),
      counted.count(_.outsideScreen > 0),
      counted.size,
      tallies.size - counted.size,
      None
    )

  /** The summary beside the count of source records the ledger admitted or rejected. */
  def of[K](
      tallies: Vector[(K, Either[GeometryError, WindowTally])],
      ledger: AdmissionLedger[K]
  ): WindowSummary =
    of(tallies).copy(sourceRecords = Some(ledger.records.size))

/** Where a study's maps live. `WholeFrame` maps the whole admission frame on
  * its grid, the version-1 meaning. `Windowed` maps a half-open window of the
  * admission frame, on a grid over the window's own frame, and applies an
  * explicit policy to fixations outside it. Either way a fixation outside
  * the admission frame is left out of the map and reported.
  */
sealed trait StudyGeometry[U <: Unit2D] derives CanEqual:
  /** The frame every input trial must be in. */
  def admission: Frame[U]

  /** The grid every density lies on. */
  def grid: Grid[U]

object StudyGeometry:
  /** Maps the whole admission frame: the grid's own frame is the admission frame. */
  final case class WholeFrame[U <: Unit2D](grid: Grid[U]) extends StudyGeometry[U]:
    def admission: Frame[U] = grid.frame

  /** Maps a window of the admission frame on a grid over the window's frame, with an explicit
    * policy for fixations off the window. Built only by `windowed`, which checks through
    * `Agreement.frames` that the grid lies on the window's frame.
    */
  final case class Windowed[U <: Unit2D] private[plan] (
      window: Subframe[U],
      grid: Grid[U],
      offWindow: OffWindowPolicy
  ) extends StudyGeometry[U]:
    def admission: Frame[U] = window.parent

  /** A window with a grid over the window's own frame. */
  def windowed[U <: Unit2D](
      window: Subframe[U],
      grid: Grid[U],
      offWindow: OffWindowPolicy
  ): Either[GeometryError, StudyGeometry[U]] =
    Agreement.frames(window.frame, grid.frame).map(_ => Windowed(window, grid, offWindow))

/** A study's estimation scale, declared in the unit it was chosen in. An
  * `Angular` scale's bandwidths are degrees and are resolved to the plan's
  * frame units through the plan's one [[LinearAngularScale]].
  */
enum StudyScale[U <: Unit2D] derives CanEqual:
  case Native(estimate: StudyEstimate[U])
  case Angular(estimate: StudyEstimate[Unit2D.Deg])

  /** The estimate in frame units; an angular scale needs the plan's scale. */
  def resolve(
      scale: Option[LinearAngularScale[U]],
      index: Int
  ): Either[PlanError, StudyEstimate[U]] =
    this match
      case Native(estimate)  => Right(estimate)
      case Angular(estimate) =>
        scale.toRight(PlanError.MissingAngularScale(index)).flatMap { s =>
          val resolved = estimate match
            case StudyEstimate.Binned()               => Right(StudyEstimate.Binned[U]())
            case StudyEstimate.Gaussian(sigma, edges) =>
              s.sigma(sigma).map(StudyEstimate.Gaussian(_, edges))
            case StudyEstimate.Anisotropic(x, y, edges) =>
              for
                sx <- s.sigma(x)
                sy <- s.sigma(y)
              yield StudyEstimate.Anisotropic(sx, sy, edges)
          resolved.left.map(PlanError.Geometry.apply)
        }

  /** Description parameters for an angular scale: the degrees as declared. */
  def angularParameters: Vector[(String, Provenance.Param)] = this match
    case Native(_)         => Vector.empty
    case Angular(estimate) => estimate.parameters

/** Per-trial screen and window handling of the prepared input. */
private[plan] object StudyWindowing:
  def tally[U <: Unit2D](
      geometry: StudyGeometry[U],
      path: Scanpath[U]
  ): Either[GeometryError, WindowTally] = geometry match
    case StudyGeometry.WholeFrame(grid)       => WindowTally.screen(grid.frame, path)
    case StudyGeometry.Windowed(window, _, _) => WindowTally.window(window, path)

  def tallies[K, U <: Unit2D](
      geometry: StudyGeometry[U],
      trials: Vector[(K, Scanpath[U])]
  ): Vector[(K, Either[GeometryError, WindowTally])] =
    trials.map { case (key, path) => key -> tally(geometry, path) }

  /** The trial fails when nothing would remain in its map, or when the
    * window policy fails trials with fixations outside the window.
    */
  def check[K, U <: Unit2D](
      geometry: StudyGeometry[U],
      key: K,
      path: Scanpath[U]
  ): Either[StudyFailure[K], Unit] =
    val failTrial = geometry match
      case StudyGeometry.Windowed(_, _, OffWindowPolicy.FailTrial) => true
      case _                                                       => false
    tally(geometry, path) match
      case Left(e)                                      => Left(StudyFailure.Frame(key, e))
      case Right(t) if t.allOutside                     => Left(StudyFailure.OffWindow(key, t))
      case Right(t) if failTrial && t.outsideWindow > 0 => Left(StudyFailure.OffWindow(key, t))
      case Right(_)                                     => Right(())

  /** Restrict an occupancy on the admission frame to the map's frame, in the
    * map's coordinates. Positions outside are left out; the trial's tally
    * counted them. A whole-frame occupancy entirely on its frame is returned
    * unchanged.
    */
  def restrict[K, U <: Unit2D](
      geometry: StudyGeometry[U],
      key: K,
      occupancy: PointMeasure[U]
  ): Either[StudyFailure[K], PointMeasure[U]] =
    def keep(target: Frame[U], local: Pt[U] => Option[Pt[U]]) =
      Agreement
        .frames(geometry.admission, occupancy.frame)
        .left
        .map(StudyFailure.Frame(key, _))
        .flatMap { _ =>
          val kept = occupancy.positions.indices.flatMap { i =>
            local(occupancy.positions(i)).map(_ -> occupancy.weights(i))
          }
          PointMeasure
            .of(target, IArray.from(kept.map(_._1)), IArray.from(kept.map(_._2)))
            .left
            .map(StudyFailure.Occupancy(key, _))
        }
    geometry match
      case StudyGeometry.WholeFrame(grid) =>
        if occupancy.positions.forall(grid.frame.contains) &&
          Agreement.frames(grid.frame, occupancy.frame).isRight
        then Right(occupancy)
        else keep(grid.frame, p => Option.when(grid.frame.contains(p))(p))
      case StudyGeometry.Windowed(window, _, _) =>
        keep(
          window.frame,
          p =>
            if !window.parent.contains(p) then None
            else
              window.locate(p) match
                case HalfOpenPlacement.Inside(local) => Some(local)
                case HalfOpenPlacement.Outside(_)    => None
        )

  /** Description fields of a geometry; none for the whole frame. */
  def description[U <: Unit2D](geometry: StudyGeometry[U])(using
      unit: UnitLabel[U]
  ): Vector[(String, Vector[Provenance.Param])] =
    import Provenance.Param.*
    geometry match
      case StudyGeometry.WholeFrame(_)               => Vector.empty
      case StudyGeometry.Windowed(window, _, policy) =>
        val parent = window.parent
        Vector(
          "admission" -> Vector(
            Text(parent.id.name),
            Text(unit.symbol),
            Num(parent.bounds.xMin),
            Num(parent.bounds.yMin),
            Num(parent.bounds.xMax),
            Num(parent.bounds.yMax),
            Text(parent.yAxis.toString)
          ),
          "window" -> Vector(
            Text(window.frame.id.name),
            Num(window.region.xMin),
            Num(window.region.yMin),
            Num(window.region.xMax),
            Num(window.region.yMax)
          ),
          "offWindow" -> Vector(Text(policy.toString))
        )
