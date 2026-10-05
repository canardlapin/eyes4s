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

package eyes4s.studio.core.fixture

import cats.syntax.all.*
import eyes4s.kernel.*
import eyes4s.plan.MapPlacement
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{Geometry, Recipe, Source, SourceRole}
import eyes4s.studio.core.geometry.{DisplayFrames, RecordPositions}
import eyes4s.studio.core.selection.{FixationIndex, RecordNumber, StudioRef}

/** The fake's source records (protocol 1.7, S6.4): fixtures/studio-golden's
  * fixations.csv, embedded verbatim ([[GoldenFixationsCsv]]), one row per
  * data record in file order, numbered from 1.
  *
  * A record's cells are read as the file states them. Its image position is
  * the kernel's image `Subframe` entered from the screen frame, and its
  * degrees the kernel's `LinearAngularScale` on that image (the dataset's
  * declared pixels per degree, from the image's centre, y up); the fake
  * writes no geometry arithmetic. A record in an admitted trial's scanpath
  * ([[GoldenFixations]]) names its fixation and the placement
  * [[FakeTrialViews]] gives it under the revision's study.
  */
object FakeSourceRecords:

  /** The file's lines: the header, then one per data record. */
  private lazy val lines: Vector[String] =
    GoldenFixationsCsv.text.stripSuffix("\n").split("\n", -1).toVector

  /** The number of data records. */
  def total: Int = lines.size - 1

  /** Record `n`'s verbatim line (from 1). */
  def line(n: Int): Option[String] = Option.when(n >= 1 && n <= total)(lines(n))

  private lazy val columns: Map[String, Int] =
    lines.headOption.toVector.flatMap(_.split(",", -1).toVector).zipWithIndex.toMap

  /** Each admitted fixation's trial and scanpath position, by record. */
  private lazy val fixationOf: Either[String, Map[Int, (TrialKey, FixationIndex)]] =
    GoldenFixations.byTrial.flatMap(
      _.toVector
        .flatTraverse((trial, fs) =>
          fs.zipWithIndex.traverse((f, i) =>
            FixationIndex.of(i + 1).bimap(_.message, ix => f.record -> (trial, ix))
          )
        )
        .map(_.toMap)
    )

  private def refused(r: AnalysisRevision, e: SourceRecordsError): BackendError =
    BackendError.SourceRecordsRefused(r, e)

  /** The study's frames ([[RecordPositions.frames]]). */
  private[fixture] def frames(
      dataset: DatasetRevision,
      recipe: Recipe,
      geometry: Geometry
  ): Either[String, (DisplayFrames, ScaleSource)] =
    RecordPositions.frames(dataset, recipe, geometry)

  /** A record's image position and degrees ([[RecordPositions.position]]). */
  private[fixture] def position(
      frames: DisplayFrames,
      record: Int,
      x: Double,
      y: Double
  ): Either[SourceRecordsError, Option[(ImagePosition, PlanePoint)]] =
    RecordPositions.position(frames, record, x, y)

  /** Records `from` to `from + count - 1` of the revision's fixation file. */
  def page(
      moment: StoryMoment,
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      from: Int,
      count: Int
  ): Either[BackendError, SourceRecordPage] =
    val unavailable = BackendError.Unavailable(DiagnosticLocus.Revision(revision))
    for
      (recipe, geometry) <- FakeTrialViews.study(moment, revision)
      doc                <- StorySeed.document(moment).leftMap(_ => unavailable)
      source             <- doc
        .dataset(dataset)
        .flatMap(_.sources.fixations)
        .toRight(unavailable)
      page <- serve(revision, dataset, recipe, geometry, source, from, count)
    yield page

  /** The page under a stated study: `recipe` (its window, off-window policy
    * and angular scale) on `geometry`, of the fixation file `source`.
    */
  private[fixture] def serve(
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      recipe: Recipe,
      geometry: Geometry,
      source: Source,
      from: Int,
      count: Int
  ): Either[BackendError, SourceRecordPage] =
    def study(step: String)(reason: String) =
      refused(revision, SourceRecordsError.Study(step, reason))
    for
      _ <- Either.cond(
        from >= 1 && count >= 1 && count <= SourceRecordPage.Limit,
        (),
        refused(revision, SourceRecordsError.RangeInvalid(from, count, SourceRecordPage.Limit))
      )
      _ <- Either.cond(
        from <= total,
        (),
        refused(revision, SourceRecordsError.PastEnd(from, total))
      )
      (display, scaleSource) <- frames(dataset, recipe, geometry).leftMap(
        study("display frames")
      )
      screen = display.screen
      window <- recipe.window
        .fold(Right(screen.bounds))(w => Bounds.of[Unit2D.Px](w.xMin, w.yMin, w.xMax, w.yMax))
        .flatMap(Subframe.of(screen, FrameId("window"), _))
        .leftMap(e => study("window")(e.message))
      fixations <- fixationOf.leftMap(study("fixation source"))
      byTrial   <- GoldenFixations.byTrial.leftMap(study("fixation source"))
      policy  = FakeTrialViews.policy(recipe)
      numbers = (from until math.min(total + 1, from + count)).toVector
      // A failing trial's in-window fixations are TrialFailed, as in its trial
      // view: each trial on the page is placed once, as a whole.
      trialPlaced <- numbers
        .flatMap(n => fixations.get(n).map(_._1))
        .distinct
        .traverse(trial =>
          byTrial
            .get(trial)
            .toVector
            .flatTraverse(fs =>
              FakeTrialViews.trialPlacements(
                fs.map(g =>
                  FakeTrialViews.placement(screen, window, policy, g.x, g.y) -> g.durationMs
                )
              )
            )
            .map(trial -> _)
        )
        .map(_.toMap)
        .leftMap(study("window tally"))
      failed = (trial: TrialKey, index: FixationIndex) =>
        trialPlaced
          .get(trial)
          .flatMap(_.lift(index.value - 1))
          .collect { case f @ MapPlacement.TrialFailed(_) => f }
      rows <- numbers.traverse { n =>
        val cells                = lines(n).split(",", -1).toVector
        def cell(name: String)   = columns.get(name).flatMap(cells.lift).map(_.trim)
        def int(name: String)    = cell(name).flatMap(_.toIntOption)
        def double(name: String) = cell(name).flatMap(_.toDoubleOption).filter(_.isFinite)
        val placed               = fixations.get(n)
        for
          trial <- (cell("participant"), cell("phase"), cell("trial"), int("occurrence"))
            .mapN((p, ph, t, o) => TrialKey(p, Phase(ph), t, o))
            .toRight(study("record")(s"record $n names no trial"))
          record   <- RecordNumber.of(n).leftMap(e => study("record")(e.message))
          screenAt <- (double("x"), double("y")).tupled.traverse((x, y) =>
            PlanePoint.of(n, "screen", x, y).leftMap(refused(revision, _))
          )
          placedAt <- screenAt
            .flatTraverse(p => position(display, n, p.x, p.y))
            .leftMap(refused(revision, _))
          row <- SourceRecordRow
            .of(
              StudioRef.SourceRecord(trial, placed.map(_._2), SourceRole.Fixations, record),
              int("ordinal"),
              double("onset_ms"),
              double("duration_ms"),
              int("sample_count"),
              screenAt,
              placedAt.map(_._1),
              placedAt.map(_._2),
              placed.flatMap((owner, index) =>
                screenAt.map(c =>
                  FakeTrialViews.placement(screen, window, policy, c.x, c.y) match
                    case MapPlacement.InWindow =>
                      failed(owner, index).getOrElse(MapPlacement.InWindow)
                    case other => other
                )
              ),
              lines(n)
            )
            .leftMap(refused(revision, _))
        yield row
      }
      page <- SourceRecordPage
        .of(
          revision,
          dataset,
          source,
          display.pixelsPerDegree,
          scaleSource,
          total,
          from,
          count,
          rows
        )
        .leftMap(refused(revision, _))
    yield page
