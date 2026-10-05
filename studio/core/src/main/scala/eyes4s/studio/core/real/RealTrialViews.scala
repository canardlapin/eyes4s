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
import eyes4s.kernel.Unit2D
import eyes4s.plan.{CoordinateProvenance, ScanpathPosition, TrialKey as CoreKey}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}

/** A revision's trial views (S3.7 slice 7, protocol 1.6): each admitted
  * fixation as eyes4s's `CoordinateProvenance` gives it, over the revision's
  * plan, admitted input and admission ledger. Studio converts units only:
  * microseconds to milliseconds, scanpath positions from 0 to positions
  * from 1.
  */
final class RealTrialViews private (
    work: RealPrepared,
    provenance: CoordinateProvenance[CoreKey, Unit2D.Px],
    keys: Map[TrialKey, CoreKey]
):
  private def refused(trial: TrialKey, step: String, reason: String) =
    BackendError.TrialViewRefused(TrialViewError.Study(trial, step, reason))

  /** The trial's scanpath, every fixation placed by the revision's study.
    * A trial of the inventory without an admitted scanpath is `Unavailable`.
    */
  def fixations(trial: TrialKey): Either[BackendError, TrialFixations] =
    for
      key       <- inventory(trial)
      positions <- provenance.positions(key).leftMap(e => refused(trial, "scanpath", e.message))
      fixations <- positions.traverse(fixation(trial, key, _))
      done      <- TrialFixations
        .of(work.revision, work.dataset, trial, fixations)
        .leftMap(BackendError.TrialViewRefused(_))
    yield done

  private def inventory(trial: TrialKey): Either[BackendError, CoreKey] =
    keys.get(trial).toRight {
      if work.admitted.ledger.exists(_.trial == trial) then
        BackendError.Unavailable(DiagnosticLocus.Trial(trial))
      else BackendError.UnknownTrial(work.dataset, trial)
    }

  private def fixation(
      trial: TrialKey,
      key: CoreKey,
      position: ScanpathPosition
  ): Either[BackendError, AdmittedFixation] =
    for
      p <- provenance
        .fixation(key, position)
        .leftMap(e => refused(trial, "fixation", e.message))
      record <- p.record.leftMap(m =>
        refused(trial, "source record", s"fixation ${position.value + 1}: ${m.toString}")
      )
      index <- FixationIndex
        .of(position.value + 1)
        .leftMap(e => refused(trial, "fixation", e.message))
      centre = p.trail.admitted.position
      made <- AdmittedFixation
        .of(
          StudioRef.Fixation(trial, index),
          record.value,
          centre.x,
          centre.y,
          p.span.onset.toMicros / 1000.0,
          p.span.duration.toMicros / 1000.0,
          p.trail.placement
        )
        .leftMap(BackendError.TrialViewRefused(_))
    yield made

object RealTrialViews:
  def of(work: RealPrepared): Either[BackendError, RealTrialViews] =
    CoordinateProvenance
      .of(work.plan, work.admitted.input, Some(work.admitted.evidence))
      .leftMap(e =>
        BackendError.Unavailable(
          DiagnosticLocus.Artifact(s"${work.revision.label}: ${e.message}")
        )
      )
      .map(p =>
        new RealTrialViews(
          work,
          p,
          work.admitted.input.trials.rows.map(r => RealResults.key(r.key) -> r.key).toMap
        )
      )
