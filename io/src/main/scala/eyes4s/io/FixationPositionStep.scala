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

package eyes4s.io

import eyes4s.kernel.{Frame, Pt, Unit2D}
import eyes4s.plan.{AdmissionPolicy, OffScreenPolicy}

/** The position interpretation shared by fixation admission and position previews. */
private[io] final case class InterpretedFixationPosition[U <: Unit2D](
    raw: Pt[U],
    corrected: Pt[U],
    rule: Either[(Int, Int), Option[Int]],
    insideFrame: Boolean
):
  def checkAdmission(frame: Frame[U], policy: OffScreenPolicy): Either[FixationRowError, Unit] =
    Either.cond(
      rule.isLeft || insideFrame || policy == OffScreenPolicy.ExcludeRecord,
      (),
      FixationRowError.Position(corrected.x, corrected.y, frame.id)
    )

private[io] object FixationPositionStep:
  def read[K, U <: Unit2D](
      fields: Map[String, String],
      xColumn: String,
      yColumn: String,
      key: K,
      frame: Frame[U],
      policy: AdmissionPolicy[K],
      participant: K => String
  ): Either[FixationRowError, InterpretedFixationPosition[U]] =
    for
      x <- FixationCsv.finite(fields, xColumn)
      y <- FixationCsv.finite(fields, yColumn)
      raw      = Pt[U](x, y)
      resolved = policy.correctionFor(key, participant)
      corrected <- resolved match
        case Right(Some((_, correction))) =>
          correction.correct(frame, raw).toRight(FixationRowError.Position(x, y, frame.id))
        // Admission retains a conflict for whole-trial quarantine after the
        // remaining record fields have been parsed. A preview never exposes
        // this unchanged point as a successfully corrected position.
        case _ => Right(raw)
    yield InterpretedFixationPosition(
      raw,
      corrected,
      resolved.map(_.map(_._1)),
      frame.contains(corrected)
    )
