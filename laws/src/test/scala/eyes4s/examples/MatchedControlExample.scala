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

package eyes4s.examples

import cats.syntax.all.*
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

/** Executable public-API example; this package has no access to design internals.
  * Existing fixation summaries are admitted without inventing raw recordings.
  * Includes the matched-minus-control result; see docs/CONTRAST_CONTRACT.md.
  */
object MatchedControlExample:
  import MatchedControlFixtures.FixationRow

  final case class StudyKey(participant: String, image: String, phase: String) derives CanEqual:
    def id: String = s"$participant/$image/$phase"

  given Ordering[StudyKey] = Ordering.by(key => (key.participant, key.image, key.phase))

  given KeyDigest[StudyKey] = KeyDigest.derived[StudyKey]

  enum AdmissionError derives CanEqual:
    case Setup(stage: String, reason: String)
    case Trial(key: StudyKey, stage: String, reason: String)

    def message: String = this match
      case Setup(stage, reason)      => s"Study $stage: $reason"
      case Trial(key, stage, reason) => s"Trial ${key.id} $stage: $reason"

  final case class Study(
      grid: Grid[Px],
      scanpaths: Trials[StudyKey, Unit, Scanpath[Px]],
      sources: Trials[StudyKey, Unit, Mass[Px]],
      references: Trials[StudyKey, Unit, Mass[Px]],
      evaluation: EvaluationInfo
  )

  private val participant = Projection.named[StudyKey, String]("participant")(_.participant)
  private val image       = Projection.named[StudyKey, String]("image")(_.image)

  val matchedDesign: PairDesign.BetweenDirected[StudyKey, StudyKey] =
    Pairing
      .between[StudyKey, StudyKey]
      .sameOn(participant, participant)
      .sameOn(image, image)
      .all

  val controlDesign: PairDesign.BetweenDirected[StudyKey, StudyKey] =
    Pairing
      .between[StudyKey, StudyKey]
      .sameOn(participant, participant)
      .differentOn(image, image)
      .all

  def prepare(
      rows: Vector[FixationRow],
      frameName: String = "matched-control-display"
  ): Either[AdmissionError, Study] =
    for
      frame <- Frame
        .screen(frameName, 2, 2)
        .left
        .map(e => AdmissionError.Setup("frame", e.message))
      grid   <- Grid.over(frame, 2, 2).left.map(e => AdmissionError.Setup("grid", e.message))
      trials <- rows
        .groupBy(row => StudyKey(row.participant, row.image, row.phase))
        .toVector
        .sortBy(_._1.id)
        .traverse { case (key, fixes) =>
          for
            scanpath  <- admit(key, frame, fixes.sortBy(_.ordinal))
            occupancy <- scanpath
              .occupancy(Weight.Duration)
              .left
              .map(e => AdmissionError.Trial(key, "occupancy", e.message))
            cells <- occupancy
              .binned(grid)
              .left
              .map(e => AdmissionError.Trial(key, "binning", e.message))
            intensity <- Surface
              .intensity(grid, cells, occupancy.provenance)
              .left
              .map(e => AdmissionError.Trial(key, "intensity", e.message))
            mass <- intensity.normalised.left
              .map(e => AdmissionError.Trial(key, "normalization", e.message))
          yield (Trial(key, (), scanpath), Trial(key, (), mass))
        }
      masses = Trials(trials.map(_._2))
      specification <- EvaluationSpec
        .of(
          "eyes4s.distribution.cosine",
          "1",
          Vector(
            "weight"        -> Provenance.Param.Text("duration"),
            "estimator"     -> Provenance.Param.Text("binned-occupancy"),
            "normalization" -> Provenance.Param.Text("unit-mass")
          ),
          Vector("value"),
          EvaluationGeometry.onGrid(grid),
          EvaluationTime.OrderFree
        )
        .left
        .map(e => AdmissionError.Setup("evaluation", e.message))
    yield Study(
      grid,
      Trials(trials.map(_._1)),
      masses.filterKey(_.phase == "recall"),
      masses.filterKey(_.phase == "encode"),
      EvaluationInfo.comparison(Distribution.cosine[Px], specification)
    )

  private def admit(
      key: StudyKey,
      frame: Frame[Px],
      rows: Vector[FixationRow]
  ): Either[AdmissionError, Scanpath[Px]] =
    val clock = ClockId(s"trial:${key.id}")
    for
      fixations <- rows.traverse { row =>
        for
          span <- Interval
            .of(
              clock,
              Instant.micros(row.onsetMicros),
              Instant.micros(row.onsetMicros + row.durationMicros)
            )
            .left
            .map(e => AdmissionError.Trial(key, s"fixation ${row.ordinal} interval", e.message))
          event <- Event.Fixation
            .withoutDispersion(
              span,
              Pt[Px](row.x, row.y),
              row.sampleCount
            )
            .left
            .map(e => AdmissionError.Trial(key, s"fixation ${row.ordinal}", e.message))
        yield event
      }
      scanpath <- Scanpath
        .of(frame, clock, IArray.from(fixations))
        .left
        .map(e => AdmissionError.Trial(key, "scanpath", e.message))
    yield scanpath

  def evaluate(
      study: Study,
      design: PairDesign.BetweenDirected[StudyKey, StudyKey]
  ): DirectedPairwiseAnalysis[StudyKey, StudyKey, CompareError, Similarity] =
    evaluatePairs(
      pair(study.sources, study.references, design),
      ContentHash.ofString(MatchedControlFixtures.inputSha256),
      study.evaluation
    )(Distribution.cosine[Px].compare)

  /** A complete existing-fixation study result, retaining both evaluation branches. */
  def analyse(
      study: Study
  ): Either[ContrastError[StudyKey], Contrast[StudyKey, Similarity, SignedDifference]] =
    contrast(
      evaluate(study, matchedDesign).meanByLeft(FailurePolicy.RequireAll),
      evaluate(study, controlDesign).meanByLeft(FailurePolicy.RequireAll)
    )
