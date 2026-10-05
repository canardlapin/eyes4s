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

package eyes4s.studio.core.geometry

import cats.syntax.all.*
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.DatasetRevisionSpec

/** Converts held fixation-source bytes into the placement preview the backend
  * protocol serves. The caller owns source lookup and digest verification;
  * this adapter owns no fixture state and does not invent geometry totals.
  */
object PlacementPreviewAdapter:

  def place(
      spec: DatasetRevisionSpec,
      source: IArray[Byte]
  ): Either[BackendError, PlacementPreview] =
    def refused(reason: String) = BackendError.PlacementRefused(spec.id, reason)
    for
      positions <- SourcePositions.read(spec, source).left.map(p => refused(p.message))
      ledger    <- CorrectionLedger.of(spec).left.map(p => refused(p.message))
      placed    <- ledger.placeAll(positions.positions).left.map(p => refused(p.message))
      density   <- PlacementDensity.of(ledger.frames, placed).left.map(p => refused(p.message))
      records = placed.map { p =>
        val image = p.placement match
          case Placement.Inside(local) => local
          case _                       => ledger.frames.image.enter(p.corrected).getOrElse(p.corrected)
        PlacedRecord(
          p.source.record,
          p.source.trial,
          p.source.x,
          p.source.y,
          p.rule,
          p.corrected.x,
          p.corrected.y,
          image.x,
          image.y,
          p.placement match
            case Placement.Inside(_)     => RecordPlacement.Inside
            case Placement.OutsideWindow => RecordPlacement.OutsideWindow
            case Placement.OutsideScreen => RecordPlacement.OutsideScreen
          ,
          ledger.frames.toDegrees(image).map(d => (d.x, d.y))
        )
      }
      byTrial = records.groupBy(_.trial)
      preview <- (
        for
          tallies <- positions.trials.traverse { trial =>
            val records = byTrial.getOrElse(trial, Vector.empty)
            TrialPlacement.of(
              trial,
              records.size,
              records.count(_.placement == RecordPlacement.OutsideWindow),
              records.count(_.placement == RecordPlacement.OutsideScreen)
            )
          }
          grid <- PlacementDensityGrid.of(
            density.columns,
            density.rows,
            density.counts.toVector,
            density.placed
          )
          preview <- PlacementPreview.of(
            spec.id,
            records,
            positions.unplaced.map(u => UnplacedSourceRecord(u.record, u.reason)),
            tallies,
            grid
          )
        yield preview
      ).left.map(e => refused(e.message))
    yield preview
