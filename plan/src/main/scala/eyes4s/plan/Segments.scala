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

import eyes4s.kernel.Unit2D

/** The counted spans of the three shipped families and the totals each can
  * state, in the pure plan vocabulary so the published execution laws check
  * the shipped claims themselves. `eyes4s-fs2` re-exports the names and its
  * family wrappers delegate here.
  */

/** The counted span of a study run: a [[StudyStage]] with the trial index
  * erased, so every trial's estimation counts toward one `Estimating(scale)`
  * total.
  */
enum StudySegment derives CanEqual:
  case Estimating(scale: Int)
  case Comparing(scale: Int, design: StudyDesign)
  case Reducing(scale: Int, design: StudyDesign)
  case Contrasting(scale: Int)

  def scale: Int

object StudySegment:
  def of(stage: StudyStage): StudySegment = stage match
    case StudyStage.Estimating(scale, _)     => Estimating(scale)
    case StudyStage.Comparing(scale, design) => Comparing(scale, design)
    case StudyStage.Reducing(scale, design)  => Reducing(scale, design)
    case StudyStage.Contrasting(scale)       => Contrasting(scale)

  /** The total stated as a segment begins: the pre-run total below, refined
    * by what the cursor about to start it knows. A reduction's units are a
    * function of its realised scores, so the cursor states them exactly.
    */
  def total[K, U <: Unit2D, S, D](
      work: PreparedStudy[K, U, ?, S, D],
      segment: StudySegment,
      cursor: StudyCursor[K, U, S, D]
  ): SegmentTotal = segment match
    case Reducing(scale, _) if work.estimates.isDefinedAt(scale) =>
      cursor.reductionUnits.fold(SegmentTotal.Unknown)(SegmentTotal.Exact.apply)
    case other => total(work, other)

  /** Totals the prepared study can state before the segment runs. Estimation
    * is one unit per trial. A comparison segment pages the schedule, which
    * visits each candidate pair at most once: for each focal key, only the
    * reference keys in its block of the design's first declared equality, or
    * one unit when that block is empty (so one per focal key when there are
    * no reference trials). It then charges one unit per reference key for the
    * unmatched-reference report, one unit to begin or wholly evaluate each
    * selected pair, and for a bounded method at most one unit per grid cell
    * inside it; the bound counts every candidate pair and every focal and
    * reference key. Reduction charges per realized score, which preparation
    * does not enumerate, so it is `Unknown` here and exact once the segment
    * begins. A contrast visits at most every focal key.
    *
    * A segment whose scale is not one of the plan's estimates names no work
    * of this study, so its total is `Unknown`; `work`'s own cursor never
    * reports one.
    */
  def total[K, U <: Unit2D, S, D](
      work: PreparedStudy[K, U, ?, S, D],
      segment: StudySegment
  ): SegmentTotal = segment match
    case other if !work.estimates.isDefinedAt(other.scale) => SegmentTotal.Unknown
    case Estimating(_)        => SegmentTotal.Exact(work.input.trials.rows.size.toLong)
    case Comparing(_, design) =>
      val candidates = BigInt(design match
        case StudyDesign.Matched => work.matched.candidatePairCount
        case StudyDesign.Control => work.controls.candidatePairCount)
      val perPair = work.capability match
        case ExecutionCapability.BoundedComparison         => BigInt(2) + work.grid.size
        case ExecutionCapability.SynchronousWholeOperation => BigInt(2)
      val bound =
        candidates * perPair + work.focalIndices.size + work.referenceIndices.size
      if bound.isValidLong then SegmentTotal.AtMost(bound.toLong) else SegmentTotal.Unknown
    case Reducing(_, _) => SegmentTotal.Unknown
    case Contrasting(_) => SegmentTotal.AtMost(work.focalIndices.size.toLong)

/** The counted span of a recording run: a [[RecordingStage]] with the chunk
  * offset erased, so every chunk of one machine counts toward one total.
  */
enum RecordingSegment derives CanEqual:
  case Synchronizing, Warping, Interpolating, Detecting, Assigning
  case Assembling(phase: eyes4s.core.AssemblyPhase)

object RecordingSegment:
  def of(stage: RecordingStage): RecordingSegment = stage match
    case RecordingStage.Synchronizing     => Synchronizing
    case RecordingStage.Warping           => Warping
    case RecordingStage.Interpolating(_)  => Interpolating
    case RecordingStage.Detecting(_)      => Detecting
    case RecordingStage.Assembling(phase) => Assembling(phase)
    case RecordingStage.Assigning         => Assigning

  /** Totals a recording plan can state before a segment runs: the whole
    * steps are one unit each, and each chunked machine feeds exactly every
    * sample, with flush charged to the last feeding chunk. Assembly phases
    * count operations and have unknown totals until their data-dependent work ends.
    */
  def total(samples: Int, segment: RecordingSegment): SegmentTotal = segment match
    case Synchronizing => SegmentTotal.Exact(1L)
    case Warping       => SegmentTotal.Exact(1L)
    case Interpolating => SegmentTotal.Exact(samples.toLong)
    case Detecting     => SegmentTotal.Exact(samples.toLong)
    case Assigning     => SegmentTotal.Exact(1L)
    case Assembling(_) => SegmentTotal.Unknown

/** The counted span of a temporal run: one `Preparing` segment per cell, and
  * the cell's study segments stamped with the cell.
  */
enum TemporalSegment derives CanEqual:
  case Preparing(repetition: Int, window: Int)
  case Studying(repetition: Int, window: Int, segment: StudySegment)

object TemporalSegment:
  def of(stage: TemporalStage): TemporalSegment = stage match
    case TemporalStage.Preparing(repetition, window, _)    => Preparing(repetition, window)
    case TemporalStage.Studying(repetition, window, stage) =>
      Studying(repetition, window, StudySegment.of(stage))

  /** The total stated as a segment begins: the pre-run total below, with a
    * cell's reduction refined to the exact units its cursor knows, as
    * [[StudySegment.total]] does for a study.
    */
  def total[K, U <: Unit2D, P, S, D](
      work: PreparedTemporalStudy[K, U, P, S, D],
      segment: TemporalSegment,
      cursor: TemporalCursor[K, U, P, S, D]
  ): SegmentTotal = segment match
    case Studying(_, _, StudySegment.Reducing(_, _)) =>
      cursor.reductionUnits.fold(SegmentTotal.Unknown)(SegmentTotal.Exact.apply)
    case other => total(work, other)

  /** Totals the prepared temporal study can state before a segment runs: a
    * cell's preparation is exactly one unit per trial, and its study segments
    * are the repetition's prepared study's totals, see [[StudySegment.total]].
    *
    * Total over every segment value: a segment whose repetition or window is
    * not a cell of `work` names no work of this study, so nothing is claimed
    * for it and its total is `Unknown`. Every segment `work`'s own cursor
    * reports is a cell of `work`, so this case never arises from a run.
    */
  def total[K, U <: Unit2D, P, S, D](
      work: PreparedTemporalStudy[K, U, P, S, D],
      segment: TemporalSegment
  ): SegmentTotal =
    def cell(repetition: Int, window: Int): Option[PreparedRepetition[K, U, P, S, D]] =
      work.repetitions.lift(repetition).filter(_ => work.windows.isDefinedAt(window))
    segment match
      case Preparing(repetition, window) =>
        cell(repetition, window).fold(SegmentTotal.Unknown)(_ =>
          SegmentTotal.Exact(work.trials.toLong)
        )
      case Studying(repetition, window, inner) =>
        cell(repetition, window).fold(SegmentTotal.Unknown)(r =>
          StudySegment.total(r.prepared, inner)
        )
