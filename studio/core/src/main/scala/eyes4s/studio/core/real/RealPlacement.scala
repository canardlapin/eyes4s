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

package eyes4s.studio.core.real

import cats.syntax.all.*
import eyes4s.io.{FixationPositions, TrialColumns}
import eyes4s.kernel.{Grid, LinearAngularScale}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{ColumnRole, DatasetRevisionSpec}
import eyes4s.studio.core.geometry.DisplayFrames
import eyes4s.studio.core.importing.SniffedSource

/** A protocol projection of the library's complete position preview. The
  * backend owns source lookup and digest verification; this adapter parses,
  * corrects and places no position itself.
  */
object RealPlacement:
  val Columns: Int = 64

  def place(
      spec: DatasetRevisionSpec,
      bytes: IArray[Byte]
  ): Either[BackendError, PlacementPreview] =
    def refused(message: String) = BackendError.PlacementRefused(spec.id, message)
    def column(role: ColumnRole) =
      RealAdmission.column(spec, role).leftMap(e => refused(e.message))
    for
      text <- SniffedSource
        .decodeUtf8(spec.sources.fixations.fold("fixations")(_.path.value), bytes)
        .leftMap(e => refused(e.message))
      frames      <- DisplayFrames.of(spec.id, spec.geometry).leftMap(e => refused(e.message))
      participant <- column(ColumnRole.Participant)
      phase       <- column(ColumnRole.Phase)
      trial       <- column(ColumnRole.Trial)
      x           <- column(ColumnRole.X)
      y           <- column(ColumnRole.Y)
      columns     <- TrialColumns
        .of(participant, phase, trial, spec.mapping.column(ColumnRole.Occurrence).map(_.value))
        .leftMap(e => refused(e.message))
      policy <- RealAdmission
        .correctionPolicy(spec, key => RealAdmission.trialIdentity(spec, key))
        .leftMap(e => refused(e.message))
      native <- FixationPositions
        .read(text.toString, columns, x, y, frames.screen, policy)
        .leftMap(e => refused(e.message))
      angular <- LinearAngularScale
        .of(frames.screen, spec.geometry.pixelsPerDegree.value)
        .leftMap(e => refused(e.message))
      placed <- native
        .place(frames.image, Some(angular), DisplayFrames.DegreesId)
        .leftMap(e => refused(e.message))
      rows = math.max(1, math.round(Columns * frames.screen.height / frames.screen.width).toInt)
      grid    <- Grid.over(frames.screen, Columns, rows).leftMap(e => refused(e.message))
      measure <- placed.measure.leftMap(e => refused(e.message))
      counts  <- measure.binned(grid).leftMap(e => refused(e.message))
      records = placed.positions.map { p =>
        val source = p.source
        PlacedRecord(
          source.record.value,
          key(source.trial),
          source.raw.x,
          source.raw.y,
          source.rule,
          source.corrected.x,
          source.corrected.y,
          p.image.x,
          p.image.y,
          if p.outsideFrame then RecordPlacement.OutsideScreen
          else if p.outsideWindow then RecordPlacement.OutsideWindow
          else RecordPlacement.Inside,
          p.degrees.map(d => (d.x, d.y))
        )
      }
      tallies <- placed.tallies
        .traverse(t =>
          TrialPlacement.of(key(t.trial), t.records, t.outsideWindow, t.outsideFrame)
        )
        .leftMap(e => refused(e.message))
      density <- PlacementDensityGrid
        .of(grid.nx, grid.ny, counts.toVector, placed.positions.size)
        .leftMap(e => refused(e.message))
      preview <- PlacementPreview
        .of(
          spec.id,
          records,
          placed.unplaced.map(p => UnplacedSourceRecord(p.record.value, p.error.message)),
          tallies,
          density
        )
        .leftMap(e => refused(e.message))
    yield preview

  private def key(identity: eyes4s.plan.TrialIdentity): TrialKey =
    TrialKey(
      identity.participant,
      Phase(identity.phase),
      identity.trial,
      identity.occurrence.value
    )
